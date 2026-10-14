write_open_test_cube <- function(root, series_id, dimnames_list = NULL) {
  base_dir <- file.path(root, "base")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  dn <- dimnames_list %||% list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  fp <- file.path(base_dir, paste0(series_id, ".h5"))
  pa_write_poparray_cube(
    x = array(seq_len(prod(unname(lengths(dn)))), dim = unname(lengths(dn)), dimnames = dn),
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = default_dim_semantics(names(dn), "year", "area.name"),
    series_id = series_id,
    geo = "county",
    extendable_year = "TRUE",
    source = list(
      note = series_id,
      population_type = "estimate",
      source = "unit-test",
      updated = "2026-08-20"
    )
  )
  normalizePath(fp, winslash = "/", mustWork = TRUE)
}

open_test_dims_2d <- function() {
  list(
    year = c("2020", "2021"),
    area.name = c("A", "B")
  )
}

open_test_dims_6d <- function() {
  list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male"),
    age.char = c("0-4", "5-9"),
    race = c("White", "Black"),
    ethnicity = c("Hispanic", "Non-Hispanic")
  )
}

test_that("dataset and group existence checks are pure inventory checks", {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    seq_len(prod(unname(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  dsem <- default_dim_semantics(names(dn), "year", "area.name")
  fp <- tempfile("existence_scan_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )

  info <- tarr.pop:::h5_inventory(fp)
  h5ls_calls <- 0L
  original_h5ls <- rhdf5::h5ls
  testthat::local_mocked_bindings(
    h5ls = function(...) {
      h5ls_calls <<- h5ls_calls + 1L
      original_h5ls(...)
    },
    .package = "rhdf5"
  )

  expect_true(tarr.pop:::h5_dataset_exists(info, "cube/metadata/dim_order"))
  expect_true(tarr.pop:::h5_dataset_exists(info, "/cube/metadata/dim_order"))
  expect_true(tarr.pop:::h5_dataset_exists(info, "cube/metadata/roles/time"))
  expect_true(tarr.pop:::h5_dataset_exists(info, "/cube/metadata/dim_semantics/sex/domain"))
  expect_false(tarr.pop:::h5_dataset_exists(info, "cube/metadata/not_a_dataset"))
  expect_false(tarr.pop:::h5_dataset_exists(info, "cube/metadata/dimnames"))

  expect_true(tarr.pop:::h5_group_exists(info, "cube/metadata/dimnames"))
  expect_true(tarr.pop:::h5_group_exists(info, "/cube/metadata/dim_semantics/sex"))
  expect_false(tarr.pop:::h5_group_exists(info, "cube/metadata/dim_order"))

  expect_equal(h5ls_calls, 0L)
})

test_that("save_poparray writes canonical metadata fields without registry duplication", {
  reset_test_cube_root()
  sid <- tarr.pop:::tarr_series_registry()$series_id[[1L]]
  x <- open_poparray(sid)

  tmp <- tempfile(fileext = ".h5")
  save_poparray(
    x = x,
    filepath = tmp,
    overwrite = TRUE,
    series_id = "unit_test_series",
    geo = "county",
    extendable_year = "TRUE"
  )

  info <- rhdf5::h5ls(tmp)
  expect_true(any(info$group == "/cube/metadata" & info$name == "series_id"))
  expect_true(any(info$group == "/cube/metadata" & info$name == "geo"))
  expect_true(any(info$group == "/cube/metadata" & info$name == "extendable_year"))
  expect_false(any(info$group == "/cube/metadata" & info$name == "registry"))
  expect_false(any(grepl("^/cube/metadata/registry", info$group)))
})

test_that("open_poparray preserves stored data_col when not overridden", {
  reset_test_cube_root()
  sid <- tarr.pop:::tarr_series_registry()$series_id[[1L]]
  x <- open_poparray(sid, data_col = "custom_population")

  tmp <- tempfile(fileext = ".h5")
  save_poparray(
    x = x,
    filepath = tmp,
    overwrite = TRUE,
    series_id = "custom_data_col_series"
  )

  testthat::local_mocked_bindings(
    tarr_series_registry = function(root = tarr.pop:::resolve_cube_dir()) {
      data.frame(
        series_id = "custom_data_col_series",
        filename = basename(tmp),
        filepath = tmp,
        stringsAsFactors = FALSE
      )
    },
    .package = "tarr.pop"
  )
  withr::local_options(list(tarr.pop.cube_path = tempdir()))

  out <- open_poparray("custom_data_col_series")
  expect_equal(data_col(out), "custom_population")
})

test_that("open_poparray looks up a known series from persisted registry", {
  root <- tempfile("open-registry-known-")
  withr::local_options(list(tarr.pop.cube_path = root))
  write_open_test_cube(root, "known_series")
  rebuild_poparray_registry(root)

  out <- open_poparray("known_series")

  expect_s4_class(out, "poparray")
  expect_equal(data_col(out), "population")
})

test_that("open_poparray errors for unknown series_id", {
  root <- tempfile("open-registry-unknown-")
  withr::local_options(list(tarr.pop.cube_path = root))
  write_open_test_cube(root, "known_series")
  rebuild_poparray_registry(root)

  expect_error(open_poparray("missing_series"), "Unknown .*series_id")
})

test_that("tarr_series_registry errors on duplicate series_id", {
  root <- tempfile("open-registry-duplicate-")
  dir.create(file.path(root, "cache"), recursive = TRUE, showWarnings = FALSE)
  withr::local_options(list(tarr.pop.cube_path = root))
  reg <- data.frame(
    series_id = c("dupe", "dupe"),
    filepath = c("/tmp/first.h5", "/tmp/second.h5"),
    filename = c("first.h5", "second.h5"),
    stringsAsFactors = FALSE
  )
  saveRDS(reg, file.path(root, "cache", "cube_registry.rds"))
  reset_poparray_cache()

  expect_error(tarr.pop:::tarr_series_registry(), "Duplicate .*series_id")
})

test_that("open_poparray errors when registered HDF5 file is missing", {
  root <- tempfile("open-registry-missing-file-")
  dir.create(file.path(root, "cache"), recursive = TRUE, showWarnings = FALSE)
  withr::local_options(list(tarr.pop.cube_path = root))
  reg <- data.frame(
    series_id = "missing_file_series",
    filepath = file.path(root, "base", "missing_file_series.h5"),
    filename = "missing_file_series.h5",
    stringsAsFactors = FALSE
  )
  saveRDS(reg, file.path(root, "cache", "cube_registry.rds"))
  reset_poparray_cache()

  expect_error(open_poparray("missing_file_series"), "HDF5 file registered")
})

test_that("open_poparray enumerates selected cube metadata hierarchy once", {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    seq_len(prod(unname(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  fp <- tempfile("open_inventory_once_", fileext = ".h5")
  withr::local_options(list(tarr.pop.cube_path = tempdir()))
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = default_dim_semantics(names(dn), "year", "area.name"),
    series_id = "open_inventory_once"
  )

  inventory_reads <- 0L
  original_inventory <- tarr.pop:::h5_inventory
  reset_poparray_cache()
  testthat::local_mocked_bindings(
    tarr_series_registry = function(root = tarr.pop:::resolve_cube_dir()) {
      data.frame(
        series_id = "open_inventory_once",
        filename = basename(fp),
        filepath = fp,
        stringsAsFactors = FALSE
      )
    },
    h5_inventory = function(path) {
      inventory_reads <<- inventory_reads + 1L
      original_inventory(path)
    },
    .package = "tarr.pop"
  )

  out <- open_poparray("open_inventory_once")

  expect_s4_class(out, "poparray")
  expect_lte(inventory_reads, 1L)
})

test_that("open_poparray does not interrogate unrelated HDF5 cubes", {
  root <- tempfile("open-registry-single-touch-")
  withr::local_options(list(tarr.pop.cube_path = root))
  selected <- write_open_test_cube(root, "selected_series")
  unrelated <- write_open_test_cube(root, "unrelated_series")
  rebuild_poparray_registry(root)
  reset_poparray_cache()

  touched <- character()
  original_inventory <- tarr.pop:::h5_inventory
  testthat::local_mocked_bindings(
    h5_inventory = function(path) {
      touched <<- c(touched, normalizePath(path, winslash = "/", mustWork = TRUE))
      original_inventory(path)
    },
    .package = "tarr.pop"
  )

  out <- open_poparray("selected_series")

  expect_s4_class(out, "poparray")
  expect_true(selected %in% touched)
  expect_false(unrelated %in% touched)
})

test_that("open_poparray hierarchy scans do not scale with dimensionality", {
  root <- tempfile("open-registry-dim-scan-")
  withr::local_options(list(tarr.pop.cube_path = root))
  two_d <- write_open_test_cube(root, "two_dimensional", open_test_dims_2d())
  six_d <- write_open_test_cube(root, "six_dimensional", open_test_dims_6d())
  rebuild_poparray_registry(root)

  original_inventory <- tarr.pop:::h5_inventory
  count_scans <- function(series_id) {
    touched <- character()
    reset_poparray_cache()
    testthat::local_mocked_bindings(
      h5_inventory = function(path) {
        touched <<- c(touched, normalizePath(path, winslash = "/", mustWork = TRUE))
        original_inventory(path)
      },
      .package = "tarr.pop"
    )
    out <- open_poparray(series_id)
    list(out = out, touched = touched)
  }

  two <- count_scans("two_dimensional")
  six <- count_scans("six_dimensional")

  expect_s4_class(two$out, "poparray")
  expect_s4_class(six$out, "poparray")
  expect_lte(sum(two$touched == two_d), 1L)
  expect_lte(sum(six$touched == six_d), 1L)
  expect_equal(sum(two$touched == six_d), 0L)
  expect_equal(sum(six$touched == two_d), 0L)
})

test_that("DimSemantics reconstruction and validObject do not rescan HDF5 hierarchy", {
  root <- tempfile("open-registry-validity-scan-")
  withr::local_options(list(tarr.pop.cube_path = root))
  selected <- write_open_test_cube(root, "six_dimensional", open_test_dims_6d())
  rebuild_poparray_registry(root)
  reset_poparray_cache()

  touched <- character()
  original_inventory <- tarr.pop:::h5_inventory
  testthat::local_mocked_bindings(
    h5_inventory = function(path) {
      touched <<- c(touched, normalizePath(path, winslash = "/", mustWork = TRUE))
      original_inventory(path)
    },
    .package = "tarr.pop"
  )

  out <- open_poparray("six_dimensional")
  scans_after_open <- length(touched)

  expect_named(dim_semantics(out), names(open_test_dims_6d()))
  expect_lte(sum(touched == selected), 1L)
  expect_true(methods::validObject(out))
  expect_equal(length(touched), scans_after_open)
})

test_that("open_poparray keeps population data HDF5 DelayedArray-backed", {
  root <- tempfile("open-registry-delayed-")
  withr::local_options(list(tarr.pop.cube_path = root))
  write_open_test_cube(root, "delayed_series")
  rebuild_poparray_registry(root)

  out <- open_poparray("delayed_series")

  expect_s4_class(out, "poparray")
  expect_true(methods::is(out, "DelayedArray"))
  expect_true(tarr.pop:::is_hdf5_backed_delayed(out))
})

test_that("checkout opens a local copy and validates its arguments", {
  root <- tempfile("checkout-registry-")
  withr::local_options(list(tarr.pop.cube_path = root))
  write_open_test_cube(root, "checkout_series")
  rebuild_poparray_registry(root)
  original <- open_poparray("checkout_series")
  source <- DelayedArray::seed(original)@filepath
  destination <- tempfile("checkout-local-")
  copied <- open_poparray("checkout_series", checkout = TRUE, checkout_dir = destination)
  expect_identical(normalizePath(DelayedArray::seed(copied)@filepath, winslash = "/"),
                   normalizePath(file.path(destination, basename(source)), winslash = "/"))
  expect_identical(attr(copied, "backing_file"), source)
  expect_equal(as.array(copied), as.array(original))
  expect_identical(dim_semantics(copied), dim_semantics(original))
  expect_null(attr(original, "backing_file"))
  default_copy <- open_poparray("checkout_series", checkout = TRUE)
  expect_identical(normalizePath(dirname(DelayedArray::seed(default_copy)@filepath), winslash = "/"), normalizePath(tempdir(), winslash = "/"))
  expect_error(open_poparray("checkout_series", checkout = NA), "checkout")
  expect_error(open_poparray("checkout_series", checkout = TRUE, checkout_dir = NA_character_), "checkout_dir")
})

test_that("validate_hdf5_metadata_shape is disabled by default", {
  reset_test_cube_root()
  sid <- tarr.pop:::tarr_series_registry()$series_id[[1L]]
  obj <- open_poparray(sid)

  old <- getOption("tarr.pop.audit_hdf5_metadata")
  on.exit(options(tarr.pop.audit_hdf5_metadata = old), add = TRUE)
  options(tarr.pop.audit_hdf5_metadata = FALSE)

  expect_true(isTRUE(tarr.pop:::validate_hdf5_metadata_shape(obj)))
})

test_that("direct filepath opening bypasses registry and preserves cube metadata", {
  root <- tempfile("open-direct-")
  withr::defer(unlink(root, recursive = TRUE))
  fp <- write_open_test_cube(root, "direct_series")
  withr::local_options(list(tarr.pop.cube_path = root))
  rebuild_poparray_registry(root)
  registered <- open_poparray("direct_series")
  testthat::local_mocked_bindings(
    resolve_cube_dir = function(...) stop("Registry must not be consulted"),
    tarr_series_registry = function(...) stop("Registry must not be consulted"),
    .package = "tarr.pop"
  )

  direct <- open_poparray(filepath = fp)
  expect_s4_class(direct, "poparray")
  expect_true(tarr.pop:::is_hdf5_backed_delayed(direct))
  expect_identical(dimnames(direct), dimnames(registered))
  expect_identical(cube_metadata(direct), cube_metadata(registered))
  expect_equal(as.array(direct), as.array(registered))
  expect_identical(data_col(open_poparray(filepath = fp, data_col = "count")), "count")
  expect_identical(cube_metadata(open_tarr_pop(filepath = fp)), cube_metadata(direct))

  destination <- tempfile("open-direct-checkout-")
  withr::defer(unlink(destination, recursive = TRUE))
  copied <- open_poparray(filepath = fp, checkout = TRUE, checkout_dir = destination)
  expect_identical(attr(copied, "backing_file"), fp)
  expect_identical(normalizePath(DelayedArray::seed(copied)@filepath, winslash = "/"),
                   normalizePath(file.path(destination, basename(fp)), winslash = "/"))
  expect_identical(cube_metadata(copied), cube_metadata(direct))
  expect_equal(as.array(copied), as.array(direct))
})

test_that("open_poparray requires one valid series identifier or filepath", {
  expect_error(open_poparray(), "exactly one")
  expect_error(open_poparray("series", filepath = "cube.h5"), "exactly one")
  expect_error(open_poparray(filepath = ""), "filepath")
  expect_error(open_poparray(filepath = NA_character_), "filepath")
  expect_error(open_poparray(filepath = c("a", "b")), "filepath")
  expect_error(open_poparray(filepath = tempfile(fileext = ".h5")), "exist")
  expect_error(open_poparray(filepath = tempdir()), "directory")
})
