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

test_that("read_cube_dim_semantics reads canonical metadata", {
  fp <- make_metadata_admin_fixture()
  dsem <- read_cube_dim_semantics(fp)

  expect_named(dsem, c("year", "area.name", "sex"))
  expect_true(all(vapply(dsem, function(x) S7::S7_inherits(x, DimSemantics), logical(1))))
})

test_that("write_cube_dim_semantics requires elevated metadata role", {
  fp <- make_metadata_admin_fixture()
  dsem <- read_cube_dim_semantics(fp)

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "reader")

  expect_error(
    write_cube_dim_semantics(fp, dsem),
    "Insufficient role"
  )
})

test_that("write_cube_dim_semantics round-trips updates under admin role", {
  fp <- make_metadata_admin_fixture()
  dsem <- read_cube_dim_semantics(fp)
  dsem$sex <- tarr.pop:::pa_update_dim_semantics(
    dsem$sex,
    partition_type = "set",
    overlap_levels = "Female",
    validated = TRUE
  )

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(write_cube_dim_semantics(fp, dsem))

  out <- read_cube_dim_semantics(fp)
  expect_identical(out$sex@partition_type, "set")
  expect_identical(out$sex@overlap_levels, "Female")
  expect_true(isTRUE(out$sex@validated))
})

test_that("edit_cube_dim_semantics updates metadata functionally", {
  fp <- make_metadata_admin_fixture()

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(
    edit_cube_dim_semantics(fp, function(dsem) {
      dsem$sex <- tarr.pop:::pa_update_dim_semantics(
        dsem$sex,
        notes = "edited"
      )
      dsem
    })
  )

  out <- read_cube_dim_semantics(fp)
  expect_identical(out$sex@notes, "edited")
})

test_that("roles metadata can be read and edited under admin role", {
  fp <- make_metadata_admin_fixture()

  roles <- read_cube_roles(fp)
  expect_identical(roles$time, "year")
  expect_identical(roles$area, "area.name")

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(write_cube_roles(fp, list(time = "year", area = "sex")))
  out <- read_cube_roles(fp)
  expect_identical(out$time, "year")
  expect_identical(out$area, "sex")
})

test_that("source metadata can be read and edited under admin role", {
  fp <- make_metadata_admin_fixture()

  src <- read_cube_source(fp)
  expect_true(is.list(src))
  expect_true(all(c("note", "source", "updated", "population_type") %in% names(src)))

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(write_cube_source(fp, list(note = "n", source = "s", updated = "2026-04-02", population_type = "Test")))
  out <- read_cube_source(fp)
  expect_identical(out$note, "n")
  expect_identical(out$source, "s")

  expect_silent(edit_cube_source(fp, function(x) { x$note <- "edited"; x }))
  out2 <- read_cube_source(fp)
  expect_identical(out2$note, "edited")
})

test_that("data_col metadata can be read and edited under admin role", {
  fp <- make_metadata_admin_fixture()

  expect_identical(read_cube_data_col(fp), "population")

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_silent(write_cube_data_col(fp, "count"))
  expect_identical(read_cube_data_col(fp), "count")

  expect_silent(edit_cube_data_col(fp, function(x) paste0(x, "_edited")))
  expect_identical(read_cube_data_col(fp), "count_edited")
})

test_that("bundled metadata reader returns canonical fields", {
  fp <- make_metadata_admin_fixture()
  meta <- read_cube_metadata_admin(fp)

  expect_true(all(c("roles", "source", "data_col", "dim_semantics") %in% names(meta)))
  expect_identical(meta$roles$time, "year")
  expect_identical(meta$data_col, "population")
  expect_true(is.list(meta$source))
  expect_true(is.list(meta$dim_semantics))
})

test_that("bundled metadata write updates fields transactionally", {
  fp <- make_metadata_admin_fixture()
  meta <- read_cube_metadata_admin(fp)
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

  expect_silent(write_cube_metadata_admin(fp, meta))

  out <- read_cube_metadata_admin(fp)
  expect_identical(out$data_col, "count")
  expect_identical(out$source$note, "bundle")
  expect_identical(out$roles$area, "sex")
})

test_that("bundled metadata edit validates cross-field consistency", {
  fp <- make_metadata_admin_fixture()

  old <- getOption("tarr.pop.metadata_role")
  on.exit(options(tarr.pop.metadata_role = old), add = TRUE)
  options(tarr.pop.metadata_role = "admin")

  expect_error(
    edit_cube_metadata_admin(fp, function(meta) {
      meta$roles <- list(time = "year", area = "sex")
      meta
    }),
    "partition_type"
  )

  expect_silent(
    edit_cube_metadata_admin(fp, function(meta) {
      meta$roles <- list(time = "year", area = "sex")
      meta$dim_semantics$sex <- tarr.pop:::pa_update_dim_semantics(
        meta$dim_semantics$sex,
        partition_type = "partition",
        overlap_levels = character()
      )
      meta
    })
  )
})
