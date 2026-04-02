# poparray class tests (S4 refactor)

make_poparray_fixture <- function(time_dim = "year", area_dim = "area.name") {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  names(dn)[names(dn) == "year"] <- time_dim
  names(dn)[names(dn) == "area.name"] <- area_dim

  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  src <- list(
    note = "Fixture",
    source = "test://fixture",
    updated = "2026-02-22",
    population_type = "Test"
  )

  fp <- tempfile("pa_fixture_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = time_dim,
    area_dim = area_dim,
    dim_semantics = default_dim_semantics(names(dn), time_dim, area_dim),
    source = src,
    data_col = "population"
  )

  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  new_poparray(
    x = dx,
    dimnames_list = dn,
    data_col = "population",
    source = src,
    time_dim = time_dim,
    area_dim = area_dim,
    dim_semantics = default_dim_semantics(names(dn), time_dim, area_dim)
  )
}

test_that("poparray is S4, DelayedArray-backed, and role-aware", {
  pa <- make_poparray_fixture()

  expect_s4_class(pa, "poparray")
  expect_s4_class(pa, "DelayedArray")
  expect_equal(time_role(pa), "year")
  expect_equal(area_role(pa), "area.name")
  expect_equal(data_col(pa), "population")
  expect_equal(names(dimnames(pa)), c("year", "area.name", "sex"))

  src <- get_source(pa)
  expect_type(src, "list")
  expect_equal(src$source, "test://fixture")
  expect_equal(src$population_type, "Test")
})

test_that("new_poparray enforces DelayedArray input and HDF5 backing", {
  arr <- array(
    c(1, 2, 3, 4),
    dim = c(2, 2),
    dimnames = list(year = c("2020", "2021"), area.name = c("A", "B"))
  )
  dsem <- default_dim_semantics(names(dimnames(arr)), "year", "area.name")

  expect_error(new_poparray(arr, dim_semantics = dsem), "DelayedArray")
  expect_error(
    new_poparray(DelayedArray::DelayedArray(arr), dim_semantics = dsem),
    "HDF5Array seed"
  )
})

test_that("subsetting keeps poparray when drop = FALSE and unwraps when drop = TRUE", {
  pa <- make_poparray_fixture()

  default_keep <- pa["2020", , ]
  expect_s4_class(default_keep, "poparray")
  expect_equal(dimnames(default_keep)$year, "2020")

  keep <- pa["2020", , , drop = FALSE]
  expect_s4_class(keep, "poparray")
  expect_equal(time_role(keep), "year")
  expect_equal(area_role(keep), "area.name")
  expect_equal(dimnames(keep)$year, "2020")

  dropped <- pa["2020", , , drop = TRUE]
  expect_false(is(dropped, "poparray"))
})

test_that("subsetting supports named indices and keeps poparray when roles remain", {
  pa <- make_poparray_fixture()

  named_keep <- pa[sex = "Female", drop = TRUE]
  expect_s4_class(named_keep, "poparray")
  expect_equal(time_role(named_keep), "year")
  expect_equal(area_role(named_keep), "area.name")
  expect_equal(names(dimnames(named_keep)), c("year", "area.name"))

  named_time <- pa[year = "2020", , drop = FALSE]
  expect_s4_class(named_time, "poparray")
  expect_equal(dimnames(named_time)$year, "2020")
})

test_that("sd(poparray) uses S4 dispatch and matches realized result", {
  pa <- make_poparray_fixture()

  expect_equal(sd(pa), stats::sd(as.array(pa)))
})

test_that("validation enforces ordered time labels", {
  arr <- array(
    c(1, 2, 3, 4),
    dim = c(2, 2),
    dimnames = list(year = c("2021", "2020"), area.name = c("A", "B"))
  )
  pa <- as.poparray(arr, filepath = tempfile("pa_unsorted_", fileext = ".h5"))

  expect_error(validate_poparray(pa), "must be ordered")
})

test_that("role accessors work with non-default role names", {
  pa <- make_poparray_fixture(time_dim = "time", area_dim = "county")

  expect_equal(time_role(pa), "time")
  expect_equal(area_role(pa), "county")
  expect_true(all(c("time", "county", "sex") %in% names(dimnames(pa))))
})
