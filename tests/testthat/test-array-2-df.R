array_2_df_fixture <- function() {
  array(c(100, 95, 102, 98), dim = c(2, 2),
        dimnames = list(year = c("2020", "2021"), sex = c("Female", "Male")))
}

test_that("array_2_df uses the stored response name", {
  arr <- array_2_df_fixture()
  attr(arr, "data_col") <- "population"
  df <- array_2_df(arr)
  expect_true("population" %in% names(df))
  expect_false("value" %in% names(df))
  expect_equal(df$population, as.vector(arr))
})

test_that("array_2_df falls back for absent or unusable stored names", {
  arr <- array_2_df_fixture()
  for (stored in list(NULL, character(0), NA_character_, "", c("a", "b"), 1)) {
    attr(arr, "data_col") <- stored
    df <- array_2_df(arr)
    expect_named(df, c("year", "sex", "value"))
    expect_equal(df$value, as.vector(arr))
  }
})

test_that("explicit arbitrary response names override stored metadata", {
  arr <- array_2_df_fixture()
  attr(arr, "data_col") <- "population"
  for (name in c("cases", "my_population")) {
    df <- array_2_df(arr, data_col = name)
    expect_named(df, c("year", "sex", name))
    expect_equal(df[[name]], as.vector(arr))
  }
})

test_that("array_2_df rejects invalid explicit response names", {
  arr <- array_2_df_fixture()
  for (name in list(character(0), NA_character_, "", c("a", "b"), 1)) {
    expect_error(array_2_df(arr, data_col = name), "data_col")
  }
})

test_that("array conversion preserves semantic coordinate and value mapping", {
  df <- data.frame(year = c("2021", "2020", "2021", "2020"),
                   sex = c("Male", "Female", "Female", "Male"),
                   population = c(0, 100, 102, 95))
  result <- array_2_df(df_2_array(df, data_col = "population"))
  key <- function(x) paste(x$year, x$sex)
  expect_setequal(key(result), key(df))
  expect_equal(result$population[match(key(df), key(result))], df$population)
})

test_that("the documented array_2_df example runs", {
  arr <- array(
    c(100, 95, 102, 98),
    dim = c(2, 2),
    dimnames = list(year = c("2020", "2021"), sex = c("Female", "Male"))
  )
  attr(arr, "data_col") <- "population"
  df <- array_2_df(arr)
  head(df)
  expect_equal(df$population, c(100, 95, 102, 98))
})
