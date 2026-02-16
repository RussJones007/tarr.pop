ns_fun <- function(name) getFromNamespace(name, "tarr.pop")

make_projection_fixture <- function() {
  dn <- list(
    year = as.character(2030:2032),
    area.name = c("A", "B"),
    sex = c("Female", "Male"),
    stat = c("projection", "std_error")
  )
  arr <- array(
    as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  
  ns_fun("new_poparray_projection")(
    handle = DelayedArray::DelayedArray(arr),
    level = 0.95,
    method = "ETS",
    source = list(
      note = "Projection from example source",
      source = "example://source",
      updated = "2026-02-16",
      projected_from = list(
        note = "Base source",
        source = "example://base",
        updated = "2026-02-15"
      )
    ),
    base_years = as.character(2025:2029),
    dimroles = list(time = "year", area = "area.name", strata = "sex"),
    data_col = "population",
    created = as.POSIXct("2026-02-16 00:00:00", tz = "UTC")
  )
}

test_that("constructor and validator support one-cube handle design", {
  pr <- make_projection_fixture()
  
  expect_s3_class(pr, "poparray_projection")
  expect_s4_class(pr$handle, "DelayedArray")
  expect_true("stat" %in% names(dimnames(pr$handle)))
  expect_equal(dimnames(pr$handle)$stat, c("projection", "std_error"))
  expect_true(ns_fun("validate_poparray_projection")(pr))
})

test_that("projection() and std_error() are position-agnostic and lazy", {
  pr <- make_projection_fixture()
  
  proj <- ns_fun("projection")(pr)
  se <- ns_fun("std_error")(pr)
  
  expect_s4_class(proj, "DelayedArray")
  expect_s4_class(se, "DelayedArray")
  
  expect_equal(dim(proj), c(3, 2, 2, 1))
  expect_equal(dim(se), c(3, 2, 2, 1))
  expect_equal(dimnames(proj)$stat, "projection")
  expect_equal(dimnames(se)$stat, "std_error")
})

test_that("confint returns delayed lower and upper arrays", {
  pr <- make_projection_fixture()
  ci <- stats::confint(pr)
  
  expect_named(ci, c("lower", "upper"))
  expect_s4_class(ci$lower, "DelayedArray")
  expect_s4_class(ci$upper, "DelayedArray")
  expect_equal(dim(ci$lower), dim(ns_fun("projection")(pr)))
  expect_equal(dim(ci$upper), dim(ns_fun("projection")(pr)))
})

test_that("subsetting keeps projection class when stat remains", {
  pr <- make_projection_fixture()
  
  y <- pr[year = "2030", drop = FALSE]
  expect_s3_class(y, "poparray_projection")
  expect_true("stat" %in% names(dimnames(y$handle)))
  expect_equal(dimnames(y$handle)$stat, c("projection", "std_error"))
})

test_that("subsetting can return poparray when stat is removed", {
  pr <- make_projection_fixture()
  
  y <- pr[stat = "projection", drop = TRUE]
  expect_s3_class(y, "poparray")
  expect_false("stat" %in% names(dimnames(y)))
})

test_that("as.poparray preserves stat and role metadata", {
  pr <- make_projection_fixture()
  pa <- tarr.pop::as.poparray(pr)
  
  expect_s3_class(pa, "poparray")
  expect_true("stat" %in% names(dimnames(pa)))
  expect_equal(dimnames(pa)$stat, c("projection", "std_error"))
  expect_equal(attr(pa, "dimroles")$time, "year")
  expect_equal(attr(pa, "dimroles")$area, "area.name")
})

test_that("tabular conversions include projection/std_error and keep attributes", {
  pr <- make_projection_fixture()
  
  df <- base::as.data.frame(pr)
  expect_true(all(c("projection", "std_error") %in% names(df)))
  expect_true(is.list(attr(df, "source", exact = TRUE)))
  expect_equal(attr(df, "method", exact = TRUE), "ETS")
  expect_equal(attr(df, "level", exact = TRUE), 0.95)
  
  tb <- tibble::as_tibble(pr)
  expect_s3_class(tb, "tbl_df")
  expect_true(all(c("projection", "std_error") %in% names(tb)))
  expect_true(is.list(attr(tb, "source", exact = TRUE)))
  expect_equal(attr(tb, "method", exact = TRUE), "ETS")
  expect_equal(attr(tb, "level", exact = TRUE), 0.95)
})

test_that("tabular conversions optionally include confidence limits", {
  pr <- make_projection_fixture()
  
  df0 <- base::as.data.frame(pr, include_confidence = FALSE)
  expect_false(any(c("lower", "upper") %in% names(df0)))
  
  df1 <- base::as.data.frame(pr, include_confidence = TRUE)
  expect_true(all(c("lower", "upper") %in% names(df1)))
  expect_true(all(df1$lower <= df1$projection))
  expect_true(all(df1$upper >= df1$projection))
  
  tb1 <- tibble::as_tibble(pr, include_confidence = TRUE)
  expect_true(all(c("lower", "upper") %in% names(tb1)))
})
