local_cube_option <- function(value) {
  old <- getOption("tarr.pop.cube_path")
  options(tarr.pop.cube_path = value)
  withr::defer(options(tarr.pop.cube_path = old), envir = parent.frame())
}

local_cube_setup_env <- function(value = "true") {
  old <- Sys.getenv("TARR_POP_SKIP_CUBE_SETUP", unset = NA_character_)
  if (is.na(value)) {
    Sys.unsetenv("TARR_POP_SKIP_CUBE_SETUP")
  } else {
    Sys.setenv(TARR_POP_SKIP_CUBE_SETUP = value)
  }
  withr::defer({
    if (is.na(old)) {
      Sys.unsetenv("TARR_POP_SKIP_CUBE_SETUP")
    } else {
      Sys.setenv(TARR_POP_SKIP_CUBE_SETUP = old)
    }
  }, envir = parent.frame())
}

test_that("cube_path reads and writes YAML-backed configuration", {
  cfg_dir <- tempfile("cube-config-")
  data_dir <- tempfile("cube-data-")
  target <- file.path(tempdir(), "configured-cubes")

  local_cube_option(NULL)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() cfg_dir,
    tarr_pop_data_dir = function() data_dir,
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

test_that("cube_path errors in non-interactive sessions when cube folder is unknown", {
  cfg_dir <- tempfile("cube-config-")
  data_dir <- tempfile("cube-data-")

  local_cube_option(NULL)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() cfg_dir,
    tarr_pop_data_dir = function() data_dir,
    prompt_for_cube_path = function(default = tarr.pop:::tarr_pop_default_cube_path()) NULL,
    .package = "tarr.pop"
  )

  expect_error(cube_path(), "set_cube_path.*init_cubes")
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
  expect_true(dir.exists(file.path(root, "cache")))
  expect_true(dir.exists(file.path(root, "derived", "projections")))
  expect_true(dir.exists(file.path(root, "derived", "filtered")))
  expect_true(dir.exists(file.path(root, "derived", "custom")))
})

test_that("init_cubes seeds base cubes from extdata and initializes registry cache", {
  root <- tempfile("cube-root-")
  local_cube_option(root)
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() tempfile("cube-config-"),
    .package = "tarr.pop"
  )

  out <- init_cubes(root)
  base_dir <- file.path(out, "base")
  cache_file <- file.path(out, "cache", "cube_registry.rds")

  expect_true(cube_files_present(base_dir))
  expect_true(file.exists(cache_file))

  reg <- readRDS(cache_file)
  expect_true(is.data.frame(reg))
  expect_true(nrow(reg) > 0L)
})

test_that("tarr_series_registry scans base recursively and ignores derived cubes", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  nested_base_dir <- file.path(base_dir, "nested")
  derived_dir <- file.path(root, "derived", "custom")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(nested_base_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

  src1 <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  src2 <- system.file("extdata", "seer_estimates_county_1y.h5", package = "tarr.pop")
  expect_true(nzchar(src1))
  expect_true(nzchar(src2))

  dst1 <- file.path(base_dir, basename(src1))
  dst2 <- file.path(nested_base_dir, basename(src2))
  dst3 <- file.path(derived_dir, basename(src2))
  expect_true(file.copy(src1, dst1))
  expect_true(file.copy(src2, dst2))
  expect_true(file.copy(src2, dst3))

  local_cube_option(root)

  reg <- tarr.pop:::tarr_series_registry()

  expect_true(all(c(dst1, dst2) %in% reg$filepath))
  expect_false(dst3 %in% reg$filepath)
  expect_true(all(file.exists(reg$filepath)))
})

test_that("tarr_series_registry reuses cache until file inventory changes", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  src <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  dst <- file.path(base_dir, basename(src))
  expect_true(file.copy(src, dst))

  cache_file <- file.path(root, "cache", "cube_registry.rds")
  calls <- 0L
  orig <- tarr.pop:::read_series_row

  local_cube_option(root)
  testthat::local_mocked_bindings(
    read_series_row = function(path, info = NULL) {
      calls <<- calls + 1L
      orig(path, info = info)
    },
    .package = "tarr.pop"
  )

  reg1 <- tarr.pop:::tarr_series_registry()
  reg2 <- tarr.pop:::tarr_series_registry()

  expect_true(file.exists(cache_file))
  expect_equal(calls, 1L)
  expect_equal(reg1$series_id, reg2$series_id)
})

test_that("tarr_series_registry memoisation avoids repeated cache reads", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  src <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  dst <- file.path(base_dir, basename(src))
  expect_true(file.copy(src, dst))

  cache_reads <- 0L
  orig <- tarr.pop:::read_cube_registry_cache

  local_cube_option(root)
  tarr.pop:::reset_poparray_cache()
  testthat::local_mocked_bindings(
    read_cube_registry_cache = function(cache_file) {
      cache_reads <<- cache_reads + 1L
      orig(cache_file)
    },
    .package = "tarr.pop"
  )

  tarr.pop:::tarr_series_registry()
  tarr.pop:::tarr_series_registry()

  expect_equal(cache_reads, 1L)
})

test_that("cube registry inventory is memoised until caches are reset", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  first <- file.path(base_dir, "first.h5")
  second <- file.path(base_dir, "second.h5")
  expect_true(file.create(first))

  tarr.pop:::reset_poparray_cache()
  inventory1 <- tarr.pop:::.cube_registry_inventory_memoised(root)
  expect_true(file.create(second))
  inventory2 <- tarr.pop:::.cube_registry_inventory_memoised(root)

  expect_equal(inventory2, inventory1)

  tarr.pop:::reset_poparray_cache()
  inventory3 <- tarr.pop:::.cube_registry_inventory_memoised(root)
  expect_equal(nrow(inventory3), 2L)
  expect_true(normalizePath(second, winslash = "/", mustWork = TRUE) %in% inventory3$filepath)
})

test_that("tarr_series_registry refreshes after a cube timestamp changes and cache is reset", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  src <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  dst <- file.path(base_dir, basename(src))
  expect_true(file.copy(src, dst))

  calls <- 0L
  orig <- tarr.pop:::read_series_row

  local_cube_option(root)
  testthat::local_mocked_bindings(
    read_series_row = function(path, info = NULL) {
      calls <<- calls + 1L
      orig(path, info = info)
    },
    .package = "tarr.pop"
  )

  tarr.pop:::tarr_series_registry()
  Sys.sleep(1)
  Sys.setFileTime(dst, Sys.time() + 2)
  tarr.pop:::reset_poparray_cache()
  tarr.pop:::tarr_series_registry()

  expect_equal(calls, 2L)
})

test_that("reset_poparray_cache clears metadata and registry memoisation", {
  root <- tempfile("cube-root-")
  base_dir <- file.path(root, "base")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  src <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  dst <- file.path(base_dir, basename(src))
  expect_true(file.copy(src, dst))

  path <- system.file("extdata", "seer_estimates_county_1y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  metadata_reads <- 0L
  registry_reads <- 0L
  orig_meta <- tarr.pop:::get_cube_metadata
  orig_reg <- tarr.pop:::read_cube_registry_cache

  local_cube_option(root)
  tarr.pop:::reset_poparray_cache()
  testthat::local_mocked_bindings(
    get_cube_metadata = function(path) {
      metadata_reads <<- metadata_reads + 1L
      orig_meta(path)
    },
    read_cube_registry_cache = function(cache_file) {
      registry_reads <<- registry_reads + 1L
      orig_reg(cache_file)
    },
    .package = "tarr.pop"
  )

  tarr.pop:::get_cube_metadata_cached(path)
  tarr.pop:::get_cube_metadata_cached(path)
  tarr.pop:::tarr_series_registry()
  tarr.pop:::tarr_series_registry()

  expect_equal(metadata_reads, 1L)
  expect_equal(registry_reads, 1L)

  tarr.pop:::reset_poparray_cache()
  tarr.pop:::get_cube_metadata_cached(path)
  tarr.pop:::tarr_series_registry()

  expect_equal(metadata_reads, 2L)
  expect_equal(registry_reads, 2L)
})

test_that("startup setup is non-fatal when cube folder is unknown", {
  cfg_dir <- tempfile("cube-config-")

  local_cube_option(NULL)
  local_cube_setup_env("false")
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() cfg_dir,
    prompt_for_cube_path = function(default = tarr.pop:::tarr_pop_default_cube_path()) NULL,
    .package = "tarr.pop"
  )

  expect_false(tarr.pop:::tarr_pop_startup_setup(interactive_session = FALSE))
})

test_that("startup setup prompts interactively and initializes cubes", {
  cfg_dir <- tempfile("cube-config-")
  root <- tempfile("startup-root-")

  local_cube_option(NULL)
  local_cube_setup_env("false")
  testthat::local_mocked_bindings(
    tarr_pop_config_dir = function() cfg_dir,
    prompt_for_cube_path = function(default = tarr.pop:::tarr_pop_default_cube_path()) root,
    .package = "tarr.pop"
  )

  expect_true(tarr.pop:::tarr_pop_startup_setup(interactive_session = TRUE))
  expect_true(dir.exists(file.path(root, "base")))
  expect_true(file.exists(file.path(root, "cache", "cube_registry.rds")))
})

test_that("save_poparray defaults to base cube storage when filepath is omitted", {
  reset_test_cube_root()
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
