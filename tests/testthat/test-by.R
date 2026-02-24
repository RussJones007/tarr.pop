make_by_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  as.poparray(arr, filepath = tempfile("by_fixture_", fileext = ".h5"))
}

test_that("by.poparray applies function by dimension labels", {
  pa <- make_by_fixture()

  out <- by(pa, INDICES = "year", FUN = function(z) sum(z, allow_overlap = TRUE), simplify = TRUE)

  expect_type(out, "double")
  expect_equal(length(out), 2)
  expect_equal(names(out), c("2020", "2021"))
})

test_that("by.poparray supports role aliases", {
  pa <- make_by_fixture()

  out <- by(pa, INDICES = "area", FUN = function(z) length(z), simplify = FALSE)

  expect_type(out, "list")
  expect_equal(names(out), c("A", "B"))
  expect_equal(unname(unlist(out)), c(4, 4))
})

test_that("by.poparray validates FUN", {
  pa <- make_by_fixture()

  expect_error(by(pa, INDICES = "year", FUN = 123), "must be a function")
})

test_that("by.poparray supports integer INDICES", {
  pa <- make_by_fixture()

  out <- by(pa, INDICES = 1, FUN = function(z) sum(z, allow_overlap = TRUE), simplify = TRUE)

  expect_type(out, "double")
  expect_equal(names(out), c("2020", "2021"))
})

test_that("by.poparray passes drop through split slices", {
  pa <- make_by_fixture()

  out <- by(
    pa,
    INDICES = "area",
    FUN = function(z) is(z, "DelayedArray"),
    simplify = FALSE,
    drop = TRUE
  )

  expect_type(out, "list")
  expect_equal(names(out), c("A", "B"))
  expect_true(all(unlist(out)))
})
