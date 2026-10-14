test_that("label access and removal work with the bundled example cube", {
  # Use an explicit package file so this test never depends on the local registry.
  path <- system.file("extdata", "tdc_example.h5", package = "tarr.pop")
  expect_true(file.exists(path))
  arr <- open_poparray(filepath = path)

  expected_ages <- c("< 1", as.character(1:85), "85 +",
                     as.character(86:94), "95 +")
  expect_identical(ages(arr), expected_ages)
  expect_identical(ages(arr, remove = "< 1"), expected_ages[-1])
  expect_identical(ages(arr, remove = stringr::regex("^1")),
                   setdiff(expected_ages, as.character(c(1, 10:19))))

  expect_identical(sexes(arr), c("female", "male"))
  expect_identical(sexes(arr, remove = "female"), "male")
  expect_identical(areas(arr), c("Dallas", "Denton", "Tarrant"))
  expect_identical(areas(arr, remove = stringr::regex("^D")), "Tarrant")

  # This cube stores combined race/ethnicity categories, not a separate race axis.
  expect_identical(dim_labels(arr, "race.eth"),
                   c("asian", "black", "hispanic", "other", "white"))
  expect_identical(dim_labels(arr, "race.eth", remove = stringr::regex("^h")),
                   c("asian", "black", "other", "white"))
  expect_error(races(arr), "absent")
  expect_null(races(arr, missing = "null"))

  expect_identical(years(arr), as.character(2015:2018))
  expect_identical(years(arr, remove = as.character(2015:2016)),
                   c("2017", "2018"))
  expect_identical(times(arr), years(arr))
})
