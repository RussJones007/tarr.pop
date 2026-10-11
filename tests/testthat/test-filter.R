make_filter_fixture <- function(year_labels = as.character(2010:2019)) {
  dn <- list(
    year = year_labels,
    area.name = "A"
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  as.poparray(arr, filepath = tempfile("filter_fixture_", fileext = ".h5"))
}

make_filter_time_role_fixture <- function(time_labels = as.character(2010:2019)) {
  dn <- list(
    time = time_labels,
    area.name = "A"
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  fp <- tempfile("filter_time_fixture_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "time",
    area_dim = "area.name",
    dim_semantics = default_dim_semantics(names(dn), "time", "area.name")
  )
  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  new_poparray(
    dx,
    dimnames_list = dn,
    time_dim = "time",
    area_dim = "area.name",
    dim_semantics = default_dim_semantics(names(dn), "time", "area.name")
  )
}

make_filter_age_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = "A",
    age.char = c("0-4", "5-9", "10-14", "15-19")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  as.poparray(arr, filepath = tempfile("filter_age_fixture_", fileext = ".h5"))
}

test_that("nested boolean predicates keep all clauses", {
  pa <- make_filter_fixture()

  res <- dplyr::filter(pa, year >= 2012 & year <= 2018 & year >= 2015)

  expect_s4_class(res, "poparray")
  expect_equal(dimnames(res)$year, as.character(2015:2018))
})

test_that("parenthesized predicates are accepted", {
  pa <- make_filter_fixture()

  res <- dplyr::filter(pa, (year >= 2012 & year <= 2015))

  expect_s4_class(res, "poparray")
  expect_equal(dimnames(res)$year, as.character(2012:2015))
})

test_that("ordered filtering follows time role, not hard-coded year name", {
  pa <- make_filter_time_role_fixture()

  res <- dplyr::filter(pa, time %between% c(2013, 2015))

  expect_s4_class(res, "poparray")
  expect_equal(dimnames(res)$time, as.character(2013:2015))
})

test_that("age.char supports ordered range semantics when present", {
  pa <- make_filter_age_fixture()

  res <- dplyr::filter(pa, age.char %between% c(5, 10))

  expect_s4_class(res, "poparray")
  expect_equal(dimnames(res)$age.char, c("5-9", "10-14"))
})

test_that("exclusion predicates compose with existing filters", {
  pop <- make_filter_age_fixture()
  out <- dplyr::filter(pop, age.char != "5-9" & age.char != "10-14", year != 2021)
  expect_identical(dimnames(out)$age.char, c("0-4", "15-19"))
  expect_identical(dimnames(out)$year, "2020")
})

test_that("exclusion preserves strict label validation", {
  pop <- make_filter_age_fixture()
  expect_error(dplyr::filter(pop, age.char != "85+"), "Unknown label")
  expect_error(dplyr::filter(pop, area.name != "Missing"), "Unknown label")
  expect_warning(
    out <- dplyr::filter(pop, age.char != "85+", .strict = FALSE),
    "Unknown label"
  )
  expect_identical(dimnames(out), dimnames(pop))
})

test_that("unary negation complements complete dimension predicates", {
  x <- make_filter_age_fixture()
  out <- dplyr::filter(x, !(age.char %in% c("0-4", "5-9")), !(year == 2021))
  expect_identical(dimnames(out)$age.char, c("10-14", "15-19"))
  expect_identical(dimnames(out)$year, "2020")
  expect_identical(dimnames(dplyr::filter(x, !(!(year == 2020))))$year, "2020")
  interval <- dplyr::filter(x, !(age.char %between% c(5, 10)))
  expect_identical(dimnames(interval)$age.char, c("0-4", "15-19"))
  combined <- dplyr::filter(x, !(age.char == "0-4" | age.char == "15-19"))
  expect_identical(dimnames(combined)$age.char, c("5-9", "10-14"))
  expect_error(dplyr::filter(x, !(year == 2020 | area.name == "A")), "exactly one dimension")
  expect_error(dplyr::filter(x, !(age.char %in% "missing")), "Unknown label")
})

test_that("text predicates support patterns, qualification, and caller variables", {
  x <- make_filter_age_fixture()
  prefix <- "1"
  pattern <- stringr::fixed("-9")
  expect_identical(dimnames(dplyr::filter(x, startsWith(age.char, prefix)))$age.char,
    c("10-14", "15-19"))
  expect_identical(dimnames(dplyr::filter(x, base::endsWith(age.char, "9")))$age.char,
    c("5-9", "15-19"))
  expect_identical(dimnames(dplyr::filter(x, stringr::str_detect(age.char, pattern)))$age.char, "5-9")
  expect_identical(dimnames(dplyr::filter(x, str_detect(age.char, "^1")))$age.char,
    c("10-14", "15-19"))
  expect_identical(dimnames(dplyr::filter(x, !startsWith(age.char, "1")))$age.char,
    c("0-4", "5-9"))
  expect_identical(dimnames(dplyr::filter(x, str_detect(age.char, "^1", negate = TRUE)))$age.char,
    c("0-4", "5-9"))
  expect_identical(dimnames(dplyr::filter(x, startsWith(prefix = "1", x = age.char)))$age.char,
    c("10-14", "15-19"))
  expect_identical(dimnames(dplyr::filter(x, str_detect(pattern = "2021", string = year)))$year, "2021")
  expect_identical(dimnames(dplyr::filter(x, str_detect(area.name, stringr::regex("a", ignore_case = TRUE))))$area.name, "A")
})

test_that("text predicates compose and retain delayed metadata", {
  x <- make_filter_age_fixture()
  before <- dimnames(x)
  testthat::local_mocked_bindings(tp_age_bounds = function(...) stop("Must not parse intervals"),
    .package = "tarr.pop")
  out <- dplyr::filter(x, startsWith(age.char, "1") & !endsWith(age.char, "9"),
    str_detect(year, "2020"))
  expect_identical(dimnames(out)$age.char, "10-14")
  expect_identical(dimnames(out)$year, "2020")
  expect_true(is_hdf5_backed_delayed(out))
  expect_equal(time_role(out), time_role(x))
  expect_equal(area_role(out), area_role(x))
  expect_equal(get_source(out), get_source(x))
  expect_named(dim_semantics(out), names(before))
  expect_identical(dimnames(x), before)
  expect_equal(dim(dplyr::filter(x, startsWith(area.name, "missing")))[2], 0L)
})

test_that("unsupported or malformed text predicates fail clearly", {
  x <- make_filter_age_fixture()
  expect_error(dplyr::filter(x, startsWith(unknown, "A")), "Unknown dimension")
  expect_error(dplyr::filter(x, startsWith(paste(age.char), "1")), "bare dimension")
  expect_error(dplyr::filter(x, startsWith(age.char)), "require a pattern")
  expect_error(dplyr::filter(x, str_detect(age.char, NA_character_)), "pattern")
  expect_error(dplyr::filter(x, endsWith(age.char, c("4", "9"))), "pattern")
  expect_error(dplyr::filter(x, str_detect(age.char, "1", negate = NA)), "negate")
  expect_error(dplyr::filter(x, startsWith(age.char, "1") | endsWith(year, "1")), "exactly one dimension")
  expect_error(dplyr::filter(x, year == 2020 && year == 2021), "Unsupported operator")
  expect_error(dplyr::filter(x, year == 2020 || year == 2021), "Unsupported operator")
  expect_error(dplyr::filter(x, is.na(year)), "Unsupported operator")
  expect_error(dplyr::filter(x, stringr::str_starts(age.char, "1")), "Unsupported operator")
})

test_that("text filtering trims applicability without altering source semantics", {
  x <- make_filter_age_fixture()
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "year", schemas = list(
      list(from = "2020", through = "2020", levels = c("0-4", "5-9")),
      list(from = "2021", through = "2021", levels = c("10-14", "15-19")))))
  dim_semantics(x) <- sem
  out <- dplyr::filter(x, endsWith(year, "1"), str_detect(age.char, "^1"))
  app <- pa_dim_applicability(dim_semantics(out)$age.char)
  expect_equal(app$by, "year")
  expect_length(app$schemas, 1)
  expect_identical(app$schemas[[1]]$levels, c("10-14", "15-19"))
  expect_true(methods::validObject(out))
  expect_equal(as.numeric(out), c(6, 8))
  expect_equal(dim_semantics(x), sem)
})
