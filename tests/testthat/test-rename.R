make_rename_fixture <- function() {
  dn <- list(year = c("2020", "2021"), area.name = "A",
             age.char = c("0-4", "5-9", "10-14", "15-19"))
  as.poparray(array(seq_len(8), c(2, 1, 4), dn),
              filepath = tempfile(fileext = ".h5"))
}

test_that("rename preserves lazy values, provenance and dimension roles", {
  x <- make_rename_fixture()
  source_meta(x) <- list(note = "Rename test")
  dn <- dimnames(x)
  out <- dplyr::rename(x, age = age.char, county = area.name, time = year)
  expect_named(dimnames(out), c("time", "county", "age"))
  expect_identical(unname(dimnames(out)), unname(dn))
  expect_identical(time_role(out), "time")
  expect_identical(area_role(out), "county")
  expect_identical(out@strata_roles, "age")
  expect_identical(get_source(out), get_source(x))
  expect_identical(data_col(out), data_col(x))
  expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
  expect_true(is_hdf5_backed_delayed(out))
  expect_true(methods::validObject(out))
  expect_equal(as.numeric(out), as.numeric(x))
  expect_identical(dimnames(x), dn)
  expect_identical(dplyr::rename(x), x)
})

test_that("rename updates semantic and applicability references together", {
  x <- make_rename_fixture()
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "year", schemas = list(
      list(from = "2020", through = "2020", levels = c("0-4", "5-9")),
      list(from = "2021", through = "2021", levels = c("10-14", "15-19")))))
  dim_semantics(x) <- sem
  out <- dplyr::rename(x, age = age.char, time = year)
  expect_named(dim_semantics(out), names(dimnames(out)))
  expect_identical(dim_semantics(out)$age@dim_name, "age")
  expect_identical(dim_semantics(out)$age@domain, sem$age.char@domain)
  expect_identical(pa_dim_applicability(dim_semantics(out)$age)$by, "time")
  expect_equal(range(out), range(x))
  selected <- dplyr::filter(out, time == 2021, age >= 10)
  expect_identical(dimnames(selected)$age, c("10-14", "15-19"))
  expect_true(methods::validObject(selected))
  expect_identical(dim_semantics(x), sem)
})

test_that("rename retains interval filters and supports programmatic names", {
  x <- make_rename_fixture()
  mapping <- c(age = "age.char", county = "area.name")
  out <- dplyr::rename(x, dplyr::all_of(mapping))
  expect_identical(dimnames(dplyr::filter(out, age >= 5 & age < 15))$age,
                   c("5-9", "10-14"))
  expect_identical(dimnames(dplyr::filter(out, !(age >= 5)))$age, "0-4")
  # Simultaneous swaps retain each dimension's original role and semantics.
  swapped <- dplyr::rename(x, year = area.name, area.name = year)
  expect_identical(time_role(swapped), "area.name")
  expect_identical(area_role(swapped), "year")
  expect_identical(dimnames(dplyr::filter(swapped, area.name >= 2021))$area.name, "2021")
})

test_that("rename rejects invalid mappings", {
  x <- make_rename_fixture()
  expect_error(dplyr::rename(x, age = missing_dimension))
  expect_error(dplyr::rename(x, year = age.char))
  expect_error(dplyr::rename(x, a = year, a = area.name))
  expect_error(dplyr::rename(x, !!!stats::setNames(list(rlang::sym("year")), " ")),
               "non-empty")
})

test_that("rename retains an existing delayed subset without reading values", {
  x <- make_rename_fixture() |> dplyr::filter(year == 2021, age.char >= 5)
  expected <- as.numeric(x)
  read_hdf5 <- get("h5mread", asNamespace("HDF5Array"))
  out <- local({
    testthat::local_mocked_bindings(
      h5mread = function(..., starts = NULL) {
        # DelayedArray validates seeds using zero-length extraction.
        empty_axis <- vapply(starts, function(i) !is.null(i) && length(i) == 0L,
                             logical(1))
        if (!any(empty_axis)) {
          stop("Population values must not be read")
        }
        read_hdf5(..., starts = starts)
      },
      .package = "HDF5Array")
    dplyr::rename(x, age = age.char, time = year)
  })
  expect_identical(dim(out), dim(x))
  expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
  expect_identical(as.numeric(out), expected)
})
