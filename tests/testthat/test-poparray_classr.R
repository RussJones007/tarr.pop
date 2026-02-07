# Test file for the poparry class


test_that("poparray requires time and area", {
  x <- array(1, dim = c(2, 2))
  dimnames(x) <- list(foo = 1:2, bar = 1:2)
  
  expect_error(new_poparray(x))
})

test_that("poparray accepts arbitrary extra dimensions", {
  x <- array(1, dim = c(2, 3, 4))
  dimnames(x) <- list(
    year = 2020:2021,
    area.name = c("A", "B", "C"),
    income = c("low", "mid", "high", "very_high")
  )
  
  pa <- new_poparray(x)
  expect_s3_class(pa, "poparray")
})


