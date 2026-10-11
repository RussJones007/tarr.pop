test_that("age_overlaps selects whole categories with inclusive bounds", {
  ages <- c("0-4", "5-9", "10-14", "37", "85+")
  expect_identical(age_overlaps(ages, 5, 9), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(age_overlaps(ages, 8, 12), c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(age_overlaps(ages, 37, 37), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(age_overlaps(ages, 85), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(age_overlaps(ages, upper = 4), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(age_overlaps(ages), rep(TRUE, 5))
  expect_identical(age_overlaps(character()), logical())
  expect_identical(age_overlaps(c("All", "all"), 100, 110), c(TRUE, TRUE))
  names(ages) <- letters[1:5]
  expect_identical(names(age_overlaps(ages)), names(ages))
  expect_identical(unname(age_overlaps(factor(ages), 5, 9)),
                   c(FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that("age_overlaps rejects ambiguous labels and invalid bounds", {
  expect_error(age_overlaps(NA_character_), "non-missing")
  expect_error(age_overlaps("5 to 9"), "labels")
  expect_error(age_overlaps("5-9+"), "labels")
  expect_error(age_overlaps("9-5"), "intervals")
  expect_error(age_overlaps(1:5), "character vector or factor")
  expect_error(age_overlaps("5-9", 10, 5), "at least")
  expect_error(age_overlaps("5-9", 5.5, 9), "whole-year")
  expect_error(age_overlaps("5-9", -1))
  expect_error(age_overlaps("5-9", upper = NA_real_))
  expect_error(age_overlaps("5-9", lower = Inf))
  expect_error(age_overlaps("5-9", upper = c(5, 9)))
})

test_that("age_overlaps indices agree with filter and remain lazy", {
  dn <- list(year = "2020", area.name = "A", age.char = c("0-4", "5-9", "10-14", "85+"))
  x <- as.poparray(array(1:4, c(1, 1, 4), dn), filepath = tempfile(fileext = ".h5"))
  out <- x[age.char = age_overlaps(dimnames(x)$age.char, 8, 12)]
  filtered <- dplyr::filter(x, age.char %between% c(8, 12))
  expect_identical(dimnames(out), dimnames(filtered))
  expect_identical(as.numeric(out), as.numeric(filtered))
  expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
  expect_identical(time_role(out), time_role(x))
  expect_identical(area_role(out), area_role(x))
  expect_true(methods::validObject(out))
  excluded <- x[age.char = !age_overlaps(dimnames(x)$age.char, 8, 12)]
  expect_identical(dimnames(excluded)$age.char, c("0-4", "85+"))
})
