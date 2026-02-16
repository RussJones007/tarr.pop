# Utility function testing



test_that("Array access and assignment functions", {
  # test using the census population array
  arr <- open_poparray(series_id = population$census.bureau$estimates)
  age <- ages(arr)
  expect_type(age, "character")
  expect_length(age, 18)
  age <- ages(arr, remove = c("< 1"))
  expect_length(age, 18)
  # test regex
  age <- ages(arr, remove = regex("(1|All)"))
  expect_length(age, 16)

  sex <- sexes(arr)
  expect_type(sex, "character")
  expect_length(sex, 2)
  expect_in(sex, c("Female", "Male"))

  # Using census estimates array here
  race <- races(arr)
  expect_type(race, "character")
  expect_length(race, 11)

  # check that regex works
  race <- races(arr, remove =regex("combination$"))
  expect_in(race, c("All", "Asian", "Black", "American Indian and Alaska Native",
                    "Hawaiian or Pacific Islander", "Two or more", "White")
  )

  yr <- years(arr, remove = as.character(2010:2019))
  expect_true(all(as.numeric(yr) > 2019))

  # check assignment operators
  
  # ages(arr) <- seq(1, 50, by = 2) |> as.character()
  # expect_setequal(ages(arr), seq(1, 50, by = 2) |> as.character())
  # 
  # sexes(arr) <- "Female"
  # sex <- sexes(arr)
  # expect_equal(sex, "Female")
  # 
  # counties <- areas(arr)
  # expect_in(c("Tarrant", "Harris", "Tom Green"), counties)
  # areas(arr) <- "Tarrant"
  # expect_equal(areas(arr), "Tarrant")

})
