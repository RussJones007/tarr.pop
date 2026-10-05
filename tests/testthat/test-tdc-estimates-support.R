source(test_path("../../data-raw/tdc_estimates_support.r"))

tdc_has_support <- function(support, year, age.char, race.eth) {
  any(
    support$year == year &
      as.character(support$area.name) == "A" &
      as.character(support$sex) == "female" &
      as.character(support$age.char) == age.char &
      as.character(support$race.eth) == race.eth
  )
}

test_that("TDC support encodes race and age schema eras", {
  support <- tdc_estimate_support_table(
    years = c(2016L, 2017L),
    counties = "A"
  )

  expect_true(tdc_has_support(support, 2016L, "85 +", "white"))
  expect_false(tdc_has_support(support, 2016L, "95 +", "white"))
  expect_true(tdc_has_support(support, 2017L, "95 +", "white"))
  expect_false(tdc_has_support(support, 2017L, "85 +", "white"))
  expect_true(tdc_has_support(support, 2016L, "85 +", "asian"))
  expect_true(tdc_has_support(support, 2017L, "95 +", "asian"))
})

test_that("TDC support rectangularizes unsupported era coordinates as NA without changing sums", {
  support <- tdc_estimate_support_table(
    years = c(2016L, 2017L),
    counties = "A"
  )

  observed <- data.frame(
    year = c(2016L, 2016L, 2017L, 2017L),
    area.name = factor("A", levels = "A"),
    sex = factor("female", levels = c("female", "male")),
    age.char = factor(
      c("84", "85 +", "85", "95 +"),
      levels = levels(support$age.char),
      ordered = TRUE
    ),
    race.eth = factor(
      c("white", "white", "asian", "white"),
      levels = levels(support$race.eth)
    ),
    population = c(10, 20, 5, 12)
  )

  completed <- tarr.pop:::apply_completion_policy(
    observed,
    dims = c("year", "area.name", "sex", "age.char", "race.eth"),
    policy = "na",
    data_col = "population",
    support = support
  )
  tarr.pop:::validate_population_df(
    completed,
    dims = c("year", "area.name", "sex", "age.char", "race.eth"),
    allow_na = TRUE,
    data_col = "population"
  )

  rect <- tarr.pop:::rectangularize_population_df(
    completed,
    dims = c("year", "area.name", "sex", "age.char", "race.eth"),
    data_col = "population"
  )
  arr <- df_2_array(as.data.frame(rect), data_col = "population")

  expect_true(is.na(arr["2016", "A", "female", "95 +", "white"]))
  expect_true(is.na(arr["2017", "A", "female", "85 +", "white"]))
  expect_equal(arr["2016", "A", "female", "85 +", "white"], 20)
  expect_equal(arr["2017", "A", "female", "95 +", "white"], 12)

  expect_equal(sum(arr, na.rm = TRUE), sum(observed$population, na.rm = TRUE))
  expect_equal(
    sum(arr["2016", , , , ], na.rm = TRUE),
    sum(observed$population[observed$year == 2016L], na.rm = TRUE)
  )
  expect_equal(
    sum(arr["2017", , , , ], na.rm = TRUE),
    sum(observed$population[observed$year == 2017L], na.rm = TRUE)
  )
})
