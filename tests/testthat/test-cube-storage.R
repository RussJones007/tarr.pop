local_cube_option <- function(value) {
  old <- getOption("tarr.pop.cube_path")
  options(tarr.pop.cube_path = value)
  withr::defer(options(tarr.pop.cube_path = old), envir = parent.frame())
}

test_that("cube_path reads and writes YAML-backed configuration", {
  cfg_dir <- tempfile("cube-config-")
  data_dir <- tempfile("cube-data-")
  target <- file.path(tempdir(), "configured-cubes")

  local_cube_option(NULL)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() cfg_dir,
    tarr_pop_data_dir = function() data_dir,
    resolve_extdata_dir = function(strict = TRUE) NULL,
    .package = "tarr.pop"
  )

  expect_false(file.exists(tarr.pop:::tarr_pop_config_file()))

  out <- set_cube_path(target)
  expect_equal(out, normalizePath(target, winslash = "/", mustWork = FALSE))
  expect_true(file.exists(tarr.pop:::tarr_pop_config_file()))
  expect_equal(cube_path(), normalizePath(target, winslash = "/", mustWork = FALSE))

  local_cube_option(NULL)
  expect_equal(cube_path(), normalizePath(target, winslash = "/", mustWork = FALSE))
})

test_that("init_cubes creates expected storage subdirectories", {
  root <- tempfile("cube-root-")
  local_cube_option(NULL)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() tempfile("cube-config-"),
    .package = "tarr.pop"
  )

  out <- init_cubes(root)

  expect_equal(out, normalizePath(root, winslash = "/", mustWork = FALSE))
  expect_true(dir.exists(file.path(root, "base")))
  expect_true(dir.exists(file.path(root, "derived", "projections")))
  expect_true(dir.exists(file.path(root, "derived", "filtered")))
  expect_true(dir.exists(file.path(root, "derived", "custom")))
})

test_that("tarr_series_registry scans cube storage recursively", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  derived_dir <- file.path(root, "derived", "custom")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

  src1 <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  src2 <- system.file("extdata", "seer_estimates_county_1y.h5", package = "tarr.pop")
  expect_true(nzchar(src1))
  expect_true(nzchar(src2))

  dst1 <- file.path(base_dir, basename(src1))
  dst2 <- file.path(derived_dir, basename(src2))
  expect_true(file.copy(src1, dst1))
  expect_true(file.copy(src2, dst2))

  local_cube_option(root)

  reg <- tarr.pop:::tarr_series_registry()

  expect_true(all(c(dst1, dst2) %in% reg$filepath))
  expect_true(all(file.exists(reg$filepath)))
})

test_that("save_poparray defaults to base cube storage when filepath is omitted", {
  sid <- tarr.pop:::tarr_series_registry()$series_id[[1L]]
  x <- open_poparray(sid)

  root <- tempfile("cube-root-")
  local_cube_option(root)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() tempfile("cube-config-"),
    .package = "tarr.pop"
  )

  out <- save_poparray(
    x = x,
    series_id = "default_storage_series",
    overwrite = TRUE
  )

  expect_equal(
    out$filepath,
    normalizePath(
      file.path(root, "base", "default_storage_series.h5"),
      winslash = "/",
      mustWork = FALSE
    )
  )
  expect_true(file.exists(out$filepath))
})

test_that("build_cube_if_missing skips existing cubes", {
  root <- tempfile("cube-root-")
  calls <- 0L

  local_cube_option(root)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() tempfile("cube-config-"),
    .package = "tarr.pop"
  )

  path1 <- build_cube_if_missing("example_cube", function(path, filepath) {
    calls <<- calls + 1L
    file.create(filepath)
  })
  path2 <- build_cube_if_missing("example_cube", function(path, filepath) {
    calls <<- calls + 1L
    file.create(filepath)
  })

  expect_equal(calls, 1L)
  expect_equal(path1, path2)
  expect_true(file.exists(path1))
})
