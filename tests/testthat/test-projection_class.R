# Need to setup test projection called "proj" for testing



test_that("stat dimension exists", {
  expect_true("stat" %in% names(dimnames(proj$data)))
})

test_that("stat levels are correct", {
  expect_equal(
    dimnames(proj$data)$stat,
    c("projection", "std_error")
  )
})

test_that("projection extraction is lazy", {
  expect_s4_class(projection(proj), "DelayedArray")
})

test_that("confint returns DelayedArrays", {
  ci <- confint(proj)
  expect_s4_class(ci$lower, "DelayedArray")
})
