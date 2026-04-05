make_metadata_admin_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  fp <- tempfile("metadata_admin_", fileext = ".h5")
  tarr.pop:::pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = tarr.pop:::default_dim_semantics(names(dn), "year", "area.name")
  )
  fp
}

test_that("dim_semantics accessor reads canonical metadata from cube path", {
  fp <- make_metadata_admin_fixture()
  dsem <- dim_semantics(fp)

  expect_named(dsem, c("year", "area.name", "sex"))
  expect_true(all(vapply(dsem, function(x) S7::S7_inherits(x, DimSemantics), logical(1))))
})

test_that("dim_semantics replacement requires elevated metadata role", {
  fp <- make_metadata_admin_fixture()
  dsem <- dim_semantics(fp)

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "reader")

  expect_error(
    `dim_semantics<-`(fp, dsem),
    "Insufficient role"
  )
})

test_that("dim_semantics replacement round-trips updates under admin role", {
  fp <- make_metadata_admin_fixture()
  dsem <- dim_semantics(fp)
  dsem$sex <- tarr.pop:::pa_update_dim_semantics(
    dsem$sex,
    partition_type = "set",
    overlap_levels = "Female",
    validated = TRUE
  )

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(`dim_semantics<-`(fp, dsem))

  out <- dim_semantics(fp)
  expect_identical(out$sex@partition_type, "set")
  expect_identical(out$sex@overlap_levels, "Female")
  expect_true(isTRUE(out$sex@validated))
})

test_that("dim_semantics replacement writes metadata for cube path", {
  fp <- make_metadata_admin_fixture()
  dsem <- dim_semantics(fp)
  dsem$sex <- tarr.pop:::pa_update_dim_semantics(
    dsem$sex,
    notes = "setter"
  )

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(`dim_semantics<-`(fp, dsem))
  expect_identical(dim_semantics(fp)$sex@notes, "setter")
})

test_that("dim_semantics can be updated through read-modify-write workflow", {
  fp <- make_metadata_admin_fixture()

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  dsem <- dim_semantics(fp)
  dsem$sex <- tarr.pop:::pa_update_dim_semantics(
    dsem$sex,
    notes = "edited"
  )
  expect_silent(`dim_semantics<-`(fp, dsem))

  out <- dim_semantics(fp)
  expect_identical(out$sex@notes, "edited")
})

test_that("roles accessor can read and edit under admin role", {
  fp <- make_metadata_admin_fixture()

  cube_roles <- roles(fp)
  expect_identical(cube_roles$time, "year")
  expect_identical(cube_roles$area, "area.name")

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(`roles<-`(fp, list(time = "year", area = "sex")))
  out <- roles(fp)
  expect_identical(out$time, "year")
  expect_identical(out$area, "sex")
})

test_that("source_meta accessor can read and edit under admin role", {
  fp <- make_metadata_admin_fixture()

  src <- source_meta(fp)
  expect_true(is.list(src))
  expect_true(all(c("note", "source", "updated", "population_type") %in% names(src)))

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(`source_meta<-`(fp, list(note = "n", source = "s", updated = "2026-04-02", population_type = "Test")))
  out <- source_meta(fp)
  expect_identical(out$note, "n")
  expect_identical(out$source, "s")

  src2 <- source_meta(fp)
  src2$note <- "edited"
  expect_silent(`source_meta<-`(fp, src2))
  out2 <- source_meta(fp)
  expect_identical(out2$note, "edited")
})

test_that("data_col accessor can read and edit under admin role", {
  fp <- make_metadata_admin_fixture()

  expect_identical(data_col(fp), "population")

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(`data_col<-`(fp, "count"))
  expect_identical(data_col(fp), "count")

  expect_silent(`data_col<-`(fp, paste0(data_col(fp), "_edited")))
  expect_identical(data_col(fp), "count_edited")
})

test_that("cube_metadata accessor returns canonical fields", {
  fp <- make_metadata_admin_fixture()
  meta <- cube_metadata(fp)

  expect_true(all(c("roles", "source", "data_col", "dim_semantics") %in% names(meta)))
  expect_identical(meta$roles$time, "year")
  expect_identical(meta$data_col, "population")
  expect_true(is.list(meta$source))
  expect_true(is.list(meta$dim_semantics))
})

test_that("bundled metadata write updates fields transactionally", {
  fp <- make_metadata_admin_fixture()
  meta <- cube_metadata(fp)
  meta$data_col <- "count"
  meta$source$note <- "bundle"
  meta$roles <- list(time = "year", area = "sex")
  meta$dim_semantics$sex <- tarr.pop:::pa_update_dim_semantics(
    meta$dim_semantics$sex,
    partition_type = "partition",
    overlap_levels = character()
  )

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(`cube_metadata<-`(fp, meta))

  out <- cube_metadata(fp)
  expect_identical(out$data_col, "count")
  expect_identical(out$source$note, "bundle")
  expect_identical(out$roles$area, "sex")
})

test_that("cube_metadata replacement validates cross-field consistency", {
  fp <- make_metadata_admin_fixture()

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  meta_bad <- cube_metadata(fp)
  meta_bad$roles <- list(time = "year", area = "sex")
  expect_error(
    `cube_metadata<-`(fp, meta_bad),
    "partition_type"
  )

  meta_ok <- cube_metadata(fp)
  meta_ok$roles <- list(time = "year", area = "sex")
  meta_ok$dim_semantics$sex <- tarr.pop:::pa_update_dim_semantics(
    meta_ok$dim_semantics$sex,
    partition_type = "partition",
    overlap_levels = character()
  )
  expect_silent(`cube_metadata<-`(fp, meta_ok))
})
