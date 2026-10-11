make_history_fixture <- function(path = tempfile(fileext = ".h5"), years = c("2023", "2024"), overwrite = FALSE) {
  dn <- list(year = years, area.name = "A")
  pa_write_poparray_cube(array(seq_along(years), c(length(years), 1), dn), path,
    overwrite = overwrite, dim_semantics = default_dim_semantics(names(dn), "year", "area.name"))
  path
}

test_that("cube history records metadata edits and restoration without changing values", {
  old_options <- options(tarr.pop.metadata_role = "admin")
  on.exit(options(old_options), add = TRUE)
  path <- make_history_fixture()
  before <- rhdf5::h5read(path, "cube/population")
  id <- cube_version(path)$cube_id
  expect_identical(cube_version(path)$schema_version, "1.2.0")
  expect_identical(cube_history(path)$revision, 1L)
  metadata <- cube_metadata(path)
  metadata$source$note <- "Vintage 2025; estimates for earlier years revised"
  update_cube_metadata(path, metadata, "Updated source caveats")
  expect_identical(cube_version(path)$cube_id, id)
  expect_identical(cube_history(path)$revision, 1:2)
  expect_identical(cube_history(path)$reason[[2]], "Updated source caveats")
  expect_identical(cube_metadata_at(path, 2L)$metadata$source$note, metadata$source$note)
  restore_cube_metadata(path, 1L, "Correct note")
  expect_identical(cube_history(path)$revision, 1:3)
  expect_identical(source_meta(path)$note, "")
  expect_identical(rhdf5::h5read(path, "cube/population"), before)
  expect_error(cube_metadata_at(path, 4L), "unavailable")
})

test_that("failed metadata updates preserve current state and committed history", {
  old_options <- options(tarr.pop.metadata_role = "admin")
  on.exit(options(old_options), add = TRUE)
  path <- make_history_fixture()
  before <- cube_metadata(path)
  metadata <- before
  metadata$roles$time <- "absent"
  expect_error(update_cube_metadata(path, metadata, "Invalid role"))
  expect_identical(cube_history(path)$revision, 1L)
  metadata <- before
  metadata$source$note <- "New note"
  original_move <- pa_h5_move
  local_mocked_bindings(pa_h5_move = function(path, from, to) {
    if (from == "cube/metadata_pending") stop("Injected commit failure")
    original_move(path, from, to)
  }, .package = "tarr.pop")
  expect_error(update_cube_metadata(path, metadata, "Failed commit"), "Injected")
  expect_identical(cube_metadata(path), before)
  expect_identical(cube_history(path)$revision, 1L)
  expect_identical(cube_metadata_at(path, 1L)$metadata$source$note, "")
})

test_that("legacy cubes acquire a baseline and migration revision on first edit", {
  old_options <- options(tarr.pop.metadata_role = "admin")
  on.exit(options(old_options), add = TRUE)
  path <- make_history_fixture()
  rhdf5::h5delete(path, "cube/metadata_history")
  rhdf5::h5delete(path, "cube/metadata/cube_id")
  rhdf5::h5delete(path, "cube/metadata/metadata_revision")
  pa_h5_write_dataset(path, "cube/metadata/schema_version", "1.1.0")
  reset_poparray_cache()
  expect_identical(cube_version(path)$metadata_revision, 0L)
  expect_equal(nrow(cube_history(path)), 0)
  source_meta(path) <- list(note = "Legacy update")
  expect_identical(cube_history(path)$revision, 1:2)
  expect_identical(cube_history(path)$schema_version, c("1.1.0", "1.2.0"))
  expect_identical(cube_metadata_at(path, 1L)$metadata$source$note, "")
})

test_that("rebuilds carry metadata history forward without keeping old numeric arrays", {
  old_options <- options(tarr.pop.metadata_role = "admin")
  on.exit(options(old_options), add = TRUE)
  path <- make_history_fixture()
  id <- cube_version(path)$cube_id
  ingest_population(reader = function(...) data.frame(
    year = c("2023", "2024", "2025"), area.name = "A", population = c(11, 12, 13)),
    dims = c("year", "area.name"),
    dim_semantics = default_dim_semantics(c("year", "area.name"), "year", "area.name"),
    filepath = path, series_id = "history", overwrite = TRUE,
    revision_reason = "2025 vintage; all prior estimates revised")
  expect_identical(cube_version(path)$cube_id, id)
  expect_identical(cube_history(path)$time_through, c("2024", "2025"))
  expect_identical(tail(cube_history(path)$reason, 1L), "2025 vintage; all prior estimates revised")
  expect_equal(as.numeric(rhdf5::h5read(path, "cube/population")), c(11, 12, 13))
  expect_identical(cube_metadata_at(path, 1L)$dimnames$year, c("2023", "2024"))
  expect_error(restore_cube_metadata(path, 1L, "Old coverage"), "dimension labels differ")
  expect_equal(dim(rhdf5::h5read(path, "cube/population")), c(3, 1))
  expect_false(pa_h5_group_exists(path, "cube/population_history"))
})

test_that("metadata replacement recovery and access controls are explicit", {
  path <- make_history_fixture()
  old_options <- options(tarr.pop.metadata_role = "reader")
  on.exit(options(old_options), add = TRUE)
  expect_error(update_cube_metadata(path, cube_metadata(path), "Blocked"), "Insufficient role")
  expect_error(recover_cube_metadata(path), "Insufficient role")
  options(tarr.pop.metadata_role = "admin")
  pa_h5_move(path, "cube/metadata", "cube/metadata_previous")
  recover_cube_metadata(path)
  expect_identical(cube_version(path)$metadata_revision, 1L)
  expect_false(pa_h5_group_exists(path, "cube/metadata_previous"))
  pa_h5_write_dataset(path, "cube/metadata/schema_version", "9.0.0")
  expect_error(update_cube_metadata(path, cube_metadata(path), "Future"), "unsupported")
})
