test_that("read_registry_row includes discovered filepath", {
  path <- system.file("extdata", "census_estimates_county_5y.h5", package = "tarr.pop")
  expect_true(nzchar(path))

  row <- tarr.pop:::read_registry_row(path)
  expect_true("filepath" %in% names(row))
  expect_equal(normalizePath(row$filepath[[1L]], winslash = "/", mustWork = TRUE), normalizePath(path, winslash = "/", mustWork = TRUE))
})

test_that("open_poparray prefers discovered filepath over registry filename metadata", {
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

  expect_warning(
    obj <- open_poparray("mock_series"),
    "Registry filename metadata"
  )
  expect_s4_class(obj, "poparray")
})
