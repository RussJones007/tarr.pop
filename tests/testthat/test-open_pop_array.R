test_that("read_series_row includes discovered filepath and series_id", {
  path <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  row <- tarr.pop:::read_series_row(path)
  expect_true("filepath" %in% names(row))
  expect_true("series_id" %in% names(row))
  expect_true(nzchar(row$series_id[[1L]]))
  expect_equal(normalizePath(row$filepath[[1L]], winslash = "/", mustWork = TRUE), normalizePath(path, winslash = "/", mustWork = TRUE))
})

test_that("read_series_row can reuse precomputed h5ls info", {
  path <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  info <- tarr.pop:::h5_inventory(path)
  row <- tarr.pop:::read_series_row(path, info = info)

  expect_true("filepath" %in% names(row))
  expect_true("series_id" %in% names(row))
})

test_that("h5_has_cube_schema accepts precomputed h5ls info", {
  path <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  info <- tarr.pop:::h5_inventory(path)
  expect_identical(
    tarr.pop:::h5_has_cube_schema(path, info = info),
    tarr.pop:::h5_has_cube_schema(path)
  )
})

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

test_that("open_poparray uses discovered filepath from canonical scan row", {
  path <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  testthat::local_mocked_bindings(
    tarr_series_registry = function() {
      data.frame(
        series_id = "mock_series",
        filename = "stale_name.h5",
        filepath = path,
        stringsAsFactors = FALSE
      )
    },
    .package = "tarr.pop"
  )

  obj <- open_poparray("mock_series")
  expect_s4_class(obj, "poparray")
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
    tarr_series_registry = function() {
      data.frame(
        series_id = "custom_data_col_series",
        filename = basename(tmp),
        filepath = tmp,
        stringsAsFactors = FALSE
      )
    },
    .package = "tarr.pop"
  )

  out <- open_poparray("custom_data_col_series")
  expect_equal(data_col(out), "custom_population")
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
    tarr_series_registry = function() {
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

test_that("cached cube metadata includes dim_semantics for reuse", {
  fp <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(fp))

  meta <- tarr.pop:::get_cube_metadata_cached(fp)

  expect_true(is.list(meta$dim_semantics))
  expect_identical(names(meta$dim_semantics), meta$dim_order)

  dsem <- tarr.pop:::read_dim_semantics_from_cube(
    fp,
    dim_order = meta$dim_order,
    time_dim = meta$roles$time,
    area_dim = meta$roles$area,
    meta = meta
  )

  expect_identical(dsem, meta$dim_semantics)
})

test_that("population catalog entries resolve to live series ids", {
  reset_test_cube_root()
  reg_ids <- tarr.pop:::tarr_series_registry()$series_id
  pop_ids <- unlist(population, use.names = TRUE)

  expect_true("texas.demographic.center" %in% names(population))
  expect_false("texas.demogrpahic.center" %in% names(population))
  expect_true(all(pop_ids %in% reg_ids))
})

test_that("population catalog entries can be opened lazily", {
  reset_test_cube_root()
  pop_ids <- unlist(population, use.names = TRUE)
  objs <- lapply(pop_ids, open_poparray)
  expect_true(all(vapply(objs, methods::is, logical(1), class2 = "poparray")))
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
