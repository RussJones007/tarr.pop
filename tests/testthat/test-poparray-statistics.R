make_stat_cube <- function(values = c(1, 2, 3, 4), age = c("0-4", "5-9")) {
  dn <- list(year = c("2020", "2021"), area.name = "A", age.char = age)
  as.poparray(array(values, dim = unname(lengths(dn)), dimnames = dn),
    filepath = tempfile(fileext = ".h5"))
}

test_that("scalar summaries match base R and preserve their source", {
  x <- make_stat_cube()
  vals <- as.numeric(x)
  original <- dimnames(x)
  expect_equal(mean(x), mean(vals))
  expect_equal(var(x), var(vals))
  expect_equal(sd(x), sd(vals))
  expect_equal(min(x), min(vals))
  expect_equal(max(x), max(vals))
  expect_equal(range(x), range(vals))
  expect_equal(range(x, 0, 10), c(0, 10))
  expect_equal(min(x, -1), -1)
  expect_equal(max(x, 10), 10)
  expect_equal(range(x, x), range(vals))
  expect_equal(dimnames(x), original)
  expect_true(is_hdf5_backed_delayed(x))
  expect_error(mean(x, trim = 0.1), "trim")
  expect_error(var(x, y = vals), "Covariance")
})

test_that("missing and infinite values follow numeric reduction conventions", {
  purrr::walk(list(c(1, NA, 3, 4), c(1, NaN, 3, 4), rep(NA_real_, 4),
    c(1, Inf, 3, 4), c(-Inf, Inf, 3, 4)), function(vals) {
    x <- make_stat_cube(vals)
    purrr::walk(c(FALSE, TRUE), function(remove) {
      expect_equal(mean(x, na.rm = remove), mean(vals, na.rm = remove))
      expect_equal(var(x, na.rm = remove), var(vals, na.rm = remove))
      expect_equal(sd(x, na.rm = remove), sd(vals, na.rm = remove))
      expect_equal(suppressWarnings(min(x, na.rm = remove)), suppressWarnings(min(vals, na.rm = remove)))
      expect_equal(suppressWarnings(max(x, na.rm = remove)), suppressWarnings(max(vals, na.rm = remove)))
      expect_equal(suppressWarnings(range(x, na.rm = remove)), suppressWarnings(range(vals, na.rm = remove)))
    })
    expect_equal(suppressWarnings(range(x, finite = TRUE)), suppressWarnings(range(vals, finite = TRUE)))
  })
  x <- make_stat_cube(c(1, NA, 3, 4))
  expect_error(var(x, use = "all.obs"), "missing observations")
  expect_equal(var(x, use = "complete.obs"), var(c(1, 3, 4)))
  expect_equal(var(x, na.rm = TRUE, use = "everything"), NA_real_)
  expect_error(var(x, use = "invalid"), "Invalid")
})

test_that("multiple blocks use stable variance without full cube coercion", {
  x <- make_stat_cube(1e12 + c(1, 2, 3, 4))
  old <- DelayedArray::getAutoBlockSize()
  on.exit(DelayedArray::setAutoBlockSize(old))
  suppressMessages(DelayedArray::setAutoBlockSize(8))
  expect_equal(var(x), var(1e12 + c(1, 2, 3, 4)), tolerance = 1e-12)
  expect_equal(sd(x), sd(1e12 + c(1, 2, 3, 4)), tolerance = 1e-12)
  expect_equal(mean(x), 1e12 + 2.5)
})

test_that("applicability excludes structural values and keeps observed missingness", {
  x <- make_stat_cube(c(2, 4, 999, NA))
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "year", schemas = list(
      list(from = "2020", through = "2020", levels = "0-4"),
      list(from = "2021", through = "2021", levels = c("0-4", "5-9")))))
  dim_semantics(x) <- sem
  old <- DelayedArray::getAutoBlockSize()
  on.exit(DelayedArray::setAutoBlockSize(old))
  suppressMessages(DelayedArray::setAutoBlockSize(8))
  expect_equal(mean(x), NA_real_)
  expect_equal(mean(x, na.rm = TRUE), 3)
  expect_equal(var(x, na.rm = TRUE), 2)
  expect_equal(sd(x, na.rm = TRUE), sqrt(2))
  expect_equal(range(x, na.rm = TRUE), c(2, 4))
  expect_equal(min(x, na.rm = TRUE), 2)
  expect_equal(max(x, na.rm = TRUE), 4)
})

test_that("moments guard overlap while extrema remain valid", {
  x <- make_stat_cube(age = c("0-4", "0-9"))
  expect_error(mean(x), "Unsafe cell summary")
  expect_error(var(x), "Unsafe cell summary")
  expect_error(sd(x), "Unsafe cell summary")
  expect_equal(mean(x, allow_overlap = TRUE), 2.5)
  expect_warning(mean(x, strict = FALSE), "Unsafe cell summary")
  expect_equal(range(x), c(1, 4))
  expect_equal(min(x), 1)
  expect_equal(max(x), 4)
  filtered <- x[, , "0-4", drop = FALSE]
  expect_equal(mean(filtered), 1.5)
  expect_equal(var(filtered), 0.5)
})

test_that("intersecting applicability rules are applied within bounded blocks", {
  x <- make_stat_cube(c(1, 2, 500, 600))
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "year", schemas = list(
      list(from = "2020", through = "2020", levels = "0-4"),
      list(from = "2021", through = "2021", levels = c("0-4", "5-9")))))
  sem$area.name <- pa_update_dim_semantics(sem$area.name, applicability = list(
    by = "age.char", schemas = list(
      list(from = "0-4", through = "0-4", levels = "A"),
      list(from = "5-9", through = "5-9", levels = character()))))
  dim_semantics(x) <- sem
  old <- DelayedArray::getAutoBlockSize()
  on.exit(suppressMessages(DelayedArray::setAutoBlockSize(old)))
  suppressMessages(DelayedArray::setAutoBlockSize(8))
  sizes <- integer()
  original <- pa_stat_block_values
  testthat::local_mocked_bindings(pa_stat_block_values = function(block, viewport, plans) {
    sizes <<- c(sizes, length(block))
    original(block, viewport, plans)
  }, .package = "tarr.pop")
  expect_equal(mean(x), 1.5)
  expect_equal(var(x), 0.5)
  expect_equal(range(x), c(1, 2))
  expect_gt(length(sizes), 1)
  expect_true(all(sizes < prod(dim(x))))
})

test_that("singleton and empty inputs have defined numeric results", {
  x <- make_stat_cube()["2020", , "0-4", drop = FALSE]
  expect_equal(mean(x), 1)
  expect_equal(var(x), NA_real_)
  expect_equal(sd(x), NA_real_)
  expect_equal(range(x), c(1, 1))
  empty <- x[FALSE, , , drop = FALSE]
  expect_equal(mean(empty), NaN)
  expect_equal(var(empty), NA_real_)
  expect_warning(expect_equal(min(empty), Inf), "no non-missing")
  expect_warning(expect_equal(max(empty), -Inf), "no non-missing")
  expect_equal(suppressWarnings(range(empty)), c(Inf, -Inf))
})
