measure_fixture <- function() {
  dn <- list(year = c("2020", "2021"), area.name = c("76002", "75001", "76001"))
  make <- function(values) as.poparray(array(values, c(2, 3), dn), filepath = tempfile(fileext = ".h5"))
  compat <- list(source = "ACS", product = "acs5", period = "five-year ending at year coordinate",
    geography = "ZCTA", vintage = "2020", universe = "all people")
  poparray_measures(list(population = make(c(10,20,30,40,50,60)), population_moe = make(c(3,3,6,6,4,4))),
    list(population = list(statistic = "count", unit = "people", method = "sum", compatibility = compat),
      population_moe = list(statistic = "moe", unit = "people", method = "acs_moe", compatibility = compat,
        estimate = "population", uncertainty_kind = "margin_of_error", confidence_level = .9,
        derivation = "ACS published", status = "published", annotations = "source annotation")))
}

test_that("linked measures collapse and round trip in shared storage", {
  x <- measure_fixture()
  groups <- setNames(substr(dimnames(x@measures[[1]])$area.name, 1, 3), dimnames(x@measures[[1]])$area.name)
  out <- collapse_dim(x, "area.name", groups)
  expect_equal(as.numeric(out@measures$population), c(60,80,30,40))
  expect_equal(as.numeric(out@measures$population_moe), c(5,5,6,6))
  expect_equal(dimnames(out@measures[[1]])$area.name, c("760", "750"))
  expect_equal(out@metadata$population_moe$status, "derived")
  path <- tempfile(fileext = ".h5")
  save_measures(out, path)
  reopened <- open_measures(path)
  expect_identical(reopened@metadata, out@metadata)
  expect_equal(as.array(reopened@measures$population_moe), as.array(out@measures$population_moe))
  selected <- select_measures(reopened, indices = list(area.name = c("750", "760")))
  expect_equal(dimnames(selected@measures[[2]])$area.name, c("750", "760"))
  expect_s4_class(selected@measures[[2]], "DelayedArray")
  expect_error(save_measures(out, path), "exists")
})

test_that("strict missingness and unsafe reductions are guarded", {
  x <- measure_fixture()
  a <- x@measures$population_moe
  a[1,1] <- NA_real_
  x@measures$population_moe <- pa_measure_wrap(a, x@measures$population_moe)
  out <- collapse_dim(x, "area.name", list(prefix = c("76002", "76001"), other = "75001"))
  expect_true(is.na(as.array(out@measures$population)[1,1]))
  expect_true(is.na(as.array(out@measures$population_moe)[1,1]))
  expect_equal(out@metadata$population_moe$annotations, "source annotation")
  expect_error(collapse_dim(x, "year", list(total = c("2020", "2021"))), "reference periods")
  expect_error(select_measures(x, "population_moe"), "companion")
  expect_length(select_measures(x, "population")@measures, 1)
  x@metadata$population$method <- "median"
  expect_error(collapse_dim(x, "area.name", list(total = c("76002", "76001"))), "Unsupported")
})

test_that("alignment and compatibility are explicit", {
  x <- measure_fixture()
  bad <- x@measures
  bad[[2]] <- bad[[2]][,c(2,1,3),drop = FALSE]
  expect_error(poparray_measures(bad, x@metadata), "align")
  for (field in c("period", "universe", "vintage", "product")) {
    meta <- x@metadata
    meta[[2]]$compatibility[[field]] <- "different"
    expect_error(poparray_measures(x@measures, meta), "compatibility")
  }
  meta <- x@metadata; meta[[2]]$confidence_level <- 90
  expect_error(poparray_measures(x@measures, meta))
})

test_that("controlled MOEs invalidate the paired estimate and legacy cubes stay explicit", {
  x <- measure_fixture()
  a <- x@measures$population_moe
  a[1,1] <- -666666666
  x@measures$population_moe <- pa_measure_wrap(a, x@measures$population_moe)
  out <- collapse_dim(x, "area.name", list(prefix = c("76002", "76001"), other = "75001"))
  expect_true(is.na(as.array(out@measures$population)[1,1]))
  expect_true(is.na(as.array(out@measures$population_moe)[1,1]))
  path <- tempfile(fileext = ".h5")
  save_poparray(x@measures$population, path)
  expect_s4_class(open_poparray(filepath = path), "poparray")
  expect_error(open_measures(path), "No measure contract")
})


test_that("integer MOEs retain fractional aggregate uncertainty", {
  x <- measure_fixture()
  template <- x@measures$population_moe
  a <- array(1L, dim(template), dimnames(template))
  x@measures$population_moe <- pa_measure_wrap(HDF5Array::writeHDF5Array(a), template)
  out <- collapse_dim(x, "area.name", list(prefix = c("76002", "76001"), other = "75001"))
  expect_equal(as.numeric(out@measures$population_moe), c(sqrt(2),sqrt(2),1,1))
})
