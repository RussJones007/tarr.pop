make_select_fixture <- function() {
  dn <- list(year = c("2020", "2021"), area.name = c("A", "B"),
             sex = c("female", "male"), age.char = c("0-4", "5-9"))
  x <- as.poparray(array(as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)), dimnames = dn), filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(x)
  sem$sex <- new_dim_semantics("sex", "sex", "nominal",
    partition_type = "partition", validated = TRUE)
  dim_semantics(x) <- sem
  x
}

test_that("select aggregates omitted dimensions and retains omitted roles", {
  x <- make_select_fixture()
  out <- dplyr::select(x, year, county = area.name)
  expected <- apply(as.array(x), c(1, 2), sum)
  expect_named(dimnames(out), c("year", "county"))
  expect_equal(as.numeric(out), as.numeric(expected))
  expect_named(dim_semantics(out), c("year", "county"))
  expect_identical(area_role(out), "county")
  expect_identical(get_source(out), get_source(x))
  expect_true(is_hdf5_backed_delayed(out))
  expect_true(methods::validObject(out))

  out <- dplyr::select(x, sex)
  expect_named(dimnames(out), c("sex", "year", "area.name"))
  expect_equal(dim(out), c(2L, 1L, 1L))
  expect_equal(as.numeric(out), as.numeric(apply(as.array(x), 3, sum)))
  empty <- dplyr::select(x)
  expect_named(dimnames(empty), c("year", "area.name"))
  expect_equal(dim(empty), c(1L, 1L))
  expect_equal(as.numeric(empty), sum(as.numeric(x)))
  expect_equal(dim(x), c(2L, 2L, 2L, 2L))
})

test_that("select supports name helpers and lazy ordering and renaming", {
  x <- make_select_fixture()
  testthat::local_mocked_bindings(
    collapse_all = function(...) stop("Unexpected collapse"), .package = "tarr.pop")
  expect_identical(dplyr::select(x, dplyr::everything()), x)
  out <- dplyr::select(x, age = age.char, dplyr::starts_with("area"),
                       dplyr::all_of(c("sex", "year")))
  expect_named(dimnames(out), c("age", "area.name", "sex", "year"))
  expect_equal(as.numeric(out), as.numeric(aperm(as.array(x), c(4, 2, 3, 1))))
  expect_identical(dim_semantics(out)$age@dim_name, "age")
  expect_true(is_hdf5_backed_delayed(out))
  expect_true(methods::validObject(out))
})

test_that("select validates selectors and final names before reducing", {
  x <- make_select_fixture()
  testthat::local_mocked_bindings(
    collapse_all = function(...) stop("Unexpected collapse"), .package = "tarr.pop")
  expect_error(dplyr::select(x, missing), "missing")
  expect_error(dplyr::select(x, dplyr::all_of("missing")), "missing")
  expect_error(dplyr::select(x, dplyr::where(is.numeric)), "predicate|where|supported")
  expect_error(dplyr::select(x, a = sex, b = sex), "more than once")
  expect_error(dplyr::select(x, year = sex), "unique")
})

test_that("select retains collapse safety and original collapse order", {
  x <- make_select_fixture()
  sem <- dim_semantics(x)
  sem$sex <- pa_update_dim_semantics(sem$sex, partition_type = "set",
                                    overlap_levels = "male")
  dim_semantics(x) <- sem
  expect_error(dplyr::select(x, -sex), "Unsafe collapse")
  x <- make_select_fixture()
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char,
    applicability = list(by = "sex", schemas = list(list(
      from = NULL, through = NULL, levels = c("0-4", "5-9")))))
  dim_semantics(x) <- sem
  expect_error(dplyr::select(x, -sex), "controls another dimension")

  calls <- character()
  original <- collapse_all
  testthat::local_mocked_bindings(collapse_all = function(x, dim, ...) {
    calls <<- c(calls, dim)
    original(x, dim, ...)
  }, .package = "tarr.pop")
  x <- make_select_fixture()
  dplyr::select(x, area.name, year)
  expect_identical(calls, c("sex", "age.char"))
  calls <- character()
  dplyr::select(x, year, area.name)
  expect_identical(calls, c("sex", "age.char"))
})

test_that("select renames applicability controllers and grouping consistently", {
  x <- make_select_fixture()
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char,
    applicability = list(by = "year", schemas = list(list(
      from = NULL, through = NULL, levels = c("0-4", "5-9")))))
  dim_semantics(x) <- sem
  x <- dplyr::group_by(x, sex, year)
  out <- dplyr::select(x, period = year, dplyr::everything())
  expect_identical(time_role(out), "period")
  expect_identical(pa_dim_applicability(dim_semantics(out)$age.char)$by, "period")
  expect_identical(dplyr::group_vars(out), c("sex", "period"))
  out <- dplyr::select(x, -sex)
  expect_identical(dplyr::group_vars(out), "year")
  expect_true(methods::validObject(out))
  expect_identical(dplyr::select(x, dplyr::any_of("missing"), dplyr::everything()), x)
})
