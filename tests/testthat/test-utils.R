# Utility function tetsing



test_that("Array access and assignment functions", {
  # test using the census population array
  arr <- census
  age <- ages(arr)
  expect_type(age, "character")
  expect_length(age, 104)
  age <- ages(arr, remove = c("< 1", "All"))
  expect_length(age, 102)
  # test regex
  age <- ages(arr, remove = regex("(1|All)"))
  expect_length(age, 80)

  sex <- sexes(arr)
  expect_type(sex, "character")
  expect_length(sex, 3)
  expect_in(sex, c("All", "Female", "Male"))
  sex <- sexes(arr, remove = "All")
  expect_length(sex, 2)

  # Using census estimates array here
  race <- races(census.estimates)
  expect_type(race, "character")
  expect_length(race, 12)

  # check that regex works
  race <- races(census.estimates, remove =regex("combination$"))
  expect_in(race, c("All", "Asian", "Black", "American Indian and Alaska Native",
                    "Hawaiian or Pacific Islander", "Two or more", "White")
  )

  yr <- years(census.estimates, remove = as.character(2010:2019))
  expect_true(all(as.numeric(yr) > 2019))

  # check assignment operators
  arr <- census
  ages(arr) <- seq(1, 50, by = 2) |> as.character()
  expect_setequal(ages(arr), seq(1, 50, by = 2) |> as.character())

  sexes(arr) <- "Female"
  sex <- sexes(arr)
  expect_equal(sex, "Female")

  counties <- areas(arr)
  expect_in(c("Tarrant", "Harris", "Tom Green"), counties)
  areas(arr) <- "Tarrant"
  expect_equal(areas(arr), "Tarrant")

})
