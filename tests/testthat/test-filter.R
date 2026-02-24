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
