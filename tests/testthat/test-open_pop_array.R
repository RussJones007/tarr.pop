test_that("read_series_row includes discovered filepath and series_id", {
  path <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  row <- tarr.pop:::read_series_row(path)
  expect_true("filepath" %in% names(row))
  expect_true("series_id" %in% names(row))
  expect_true(nzchar(row$series_id[[1L]]))
  expect_equal(normalizePath(row$filepath[[1L]], winslash = "/", mustWork = TRUE), normalizePath(path, winslash = "/", mustWork = TRUE))
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

test_that("population catalog entries resolve to live series ids", {
  reg_ids <- tarr.pop:::tarr_series_registry()$series_id
  pop_ids <- unlist(population, use.names = TRUE)

  expect_true("texas.demographic.center" %in% names(population))
  expect_false("texas.demogrpahic.center" %in% names(population))
  expect_true(all(pop_ids %in% reg_ids))
})

test_that("population catalog entries can be opened lazily", {
  pop_ids <- unlist(population, use.names = TRUE)
  objs <- lapply(pop_ids, open_poparray)
  expect_true(all(vapply(objs, methods::is, logical(1), class2 = "poparray")))
})
