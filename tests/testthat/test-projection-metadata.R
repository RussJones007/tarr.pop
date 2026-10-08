make_projection_metadata_fixture <- function(overlap = FALSE, applicability = FALSE) {
  dn <- list(sex = c("Female", "Male"), year = as.character(2018:2022),
             race = c("R1", "R2"), area.name = "A")
  sem <- list(
    sex = new_dim_semantics("sex", "sex", "nominal", "partition", TRUE,
                            notes = "Explicit mutually exclusive sex groups"),
    year = new_dim_semantics("year", "time", "interval", "partition", TRUE),
    race = new_dim_semantics("race", "race", "nominal",
                             if (overlap) "set" else "partition", TRUE,
                             overlap_levels = if (overlap) "R2" else character(),
                             notes = c("Original race contract", "Do not infer from labels")),
    area.name = new_dim_semantics("area.name", "geography", "ordinal", "partition", TRUE)
  )
  if (applicability) sem$race <- pa_update_dim_semantics(sem$race,
    applicability = list(by = "year", schemas = list(
      list(from = NULL, through = NULL, levels = dn$race))))
  arr <- array(as.numeric(seq_len(prod(lengths(dn)))) + 0.2,
               dim = unname(lengths(dn)), dimnames = dn)
  h <- HDF5Array::writeHDF5Array(arr, filepath = tempfile(fileext = ".h5"), name = "data")
  dimnames(h) <- dn
  new_poparray(h, dim_semantics = sem)
}

test_that("projection and coercion preserve explicit semantics and allow valid aggregation", {
  pa <- make_projection_metadata_fixture(applicability = TRUE)
  original <- serialize(pa, NULL)
  pr <- project(pa, h = 2, method = "CAGR", guard = FALSE)
  expect_identical(dim_semantics(pr)[names(dimnames(pa))], dim_semantics(pa))
  expect_identical(names(dim_semantics(pr)), names(dimnames(pr)))
  expect_identical(pr@strata_roles, c("sex", "race"))
  expect_identical(dim_semantics(pr)$stat@partition_type, "set")
  expect_identical(dim_semantics(pr)$stat@overlap_levels, c("projection", "std_error"))
  converted <- as.poparray(pr)
  expect_identical(dim_semantics(converted), dim_semantics(pa))
  expect_identical(dimnames(converted), dimnames(pr)[names(dimnames(pa))])
  expect_identical(time_role(converted), time_role(pa))
  expect_identical(area_role(converted), area_role(pa))
  expect_identical(serialize(pa, NULL), original)
  expect_true(validObject(pr))
  expect_true(validObject(converted))
  collapsed_sex <- collapse_dim(converted, "sex", list(Total = dimnames(converted)$sex))
  collapsed_race <- collapse_dim(collapsed_sex, "race", list(Total = dimnames(converted)$race))
  expect_equal(as.numeric(collapsed_race), as.numeric(apply(as.array(converted), c(2, 4), sum)))
  expect_true(is_hdf5_backed_delayed(collapsed_race))
})

test_that("projection semantic metadata survives HDF5 reopening without payload reads", {
  pa <- make_projection_metadata_fixture(applicability = TRUE)
  pr <- project(pa, h = 2, method = "CAGR", guard = FALSE)
  path <- DelayedArray::seed(pr)@filepath
  testthat::local_mocked_bindings(
    extract_array = function(...) stop("unexpected payload read"), .package = "HDF5Array")
  reopened <- read_poparray_projection(path)
  expect_identical(dim_semantics(reopened), dim_semantics(pr))
  expect_identical(dimnames(reopened), dimnames(pr))
  expect_identical(reopened@strata_roles, pr@strata_roles)
  expect_equal(reopened@level, pr@level)
  expect_equal(reopened@method, pr@method)
  expect_identical(reopened@base_years, pr@base_years)
  expect_identical(dim_semantics(as.poparray(reopened)), dim_semantics(pa))
  expect_s4_class(DelayedArray::seed(reopened), "HDF5ArraySeed")
  # The handle constructor also restores semantics and labels from disk.
  h <- HDF5Array::HDF5Array(path, "data")
  restored <- new_poparray_projection(h, pr@level, pr@method, pr@source, pr@base_years)
  expect_identical(dim_semantics(restored), dim_semantics(pr))
})

test_that("overlap guards remain active after projection and coercion", {
  pa <- make_projection_metadata_fixture(overlap = TRUE)
  pr <- project(pa, h = 2, method = "CAGR", guard = FALSE)
  converted <- as.poparray(pr)
  expect_identical(dim_semantics(converted), dim_semantics(pa))
  expect_error(collapse_dim(converted, "race", list(Total = c("R1", "R2"))),
               "Unsafe collapse blocked")
  expect_error(sum(converted), "Unsafe reduction blocked")
  reopened <- as.poparray(read_poparray_projection(DelayedArray::seed(pr)@filepath))
  expect_error(collapse_dim(reopened, "race", list(Total = c("R1", "R2"))),
               "Unsafe collapse blocked")
})

test_that("projection subsetting retains semantics and trims applicability lazily", {
  pr <- project(make_projection_metadata_fixture(applicability = TRUE),
                 h = 2, method = "CAGR", guard = FALSE)
  testthat::local_mocked_bindings(
    extract_array = function(...) stop("unexpected payload read"), .package = "HDF5Array")
  selected <- pr[race = "R1", year = "2024", drop = FALSE]
  expect_identical(dim_semantics(selected)$race@applicability$schemas[[1]]$levels, "R1")
  expect_identical(dim_semantics(selected)$sex, dim_semantics(pr)$sex)
  converted <- as.poparray(selected)
  expect_equal(dim(converted), c(2L, 1L, 1L, 1L))
  expect_true(validObject(converted))
})

test_that("projection applicability uses existing open schemas and rejects uncovered futures", {
  pa <- make_projection_metadata_fixture(applicability = TRUE)
  sem <- dim_semantics(pa)
  sem$race <- pa_update_dim_semantics(sem$race, applicability = list(by = "year", schemas = list(
    list(from = NULL, through = "2019", levels = "R1"),
    list(from = "2020", through = NULL, levels = c("R1", "R2")))))
  dim_semantics(pa) <- sem
  pr <- project(pa, h = 2, method = "CAGR", guard = FALSE)
  expect_identical(dim_semantics(pr)$race@applicability, list(by = "year", schemas = list(
    list(from = NULL, through = NULL, levels = c("R1", "R2")))))
  sem$race <- pa_update_dim_semantics(sem$race, applicability = list(by = "year", schemas = list(
    list(from = NULL, through = "2022", levels = c("R1", "R2")))))
  dim_semantics(pa) <- sem
  expect_error(project(pa, h = 2, method = "CAGR", guard = FALSE), "uncovered")
})

test_that("constructors reject missing or misaligned semantics instead of inferring", {
  pa <- make_projection_metadata_fixture()
  pr <- project(pa, h = 2, method = "CAGR", guard = FALSE)
  h <- projection(pr)
  h <- DelayedArray::aperm(h, seq_len(length(dim(h)) - 1L))
  manual <- poparray_projection(h, h * 0.1, 0.95, "CAGR", pr@source, pr@base_years,
                                 dim_semantics = dim_semantics(pa))
  expect_identical(dim_semantics(as.poparray(manual)), dim_semantics(pa))
  expect_equal(as.numeric(as.poparray(manual)), as.numeric(h))
  expect_true(is_hdf5_backed_delayed(manual))
  expect_error(poparray_projection(h, h * 0.1, 0.95, "CAGR", pr@source, pr@base_years),
               "dim_semantics.*required")
  sem <- dim_semantics(pr)
  expect_error(new_poparray_projection(projection(pr), 0.95, "CAGR", pr@source, pr@base_years,
                                        dim_semantics = sem[c(2, 1, 3, 4, 5)]), "same order")
  testthat::local_mocked_bindings(
    extract_array = function(...) stop("unexpected payload read"), .package = "HDF5Array")
  expect_true(validObject(as.poparray(manual)))
})

test_that("projection validity rejects unsafe statistic contracts and dangling references", {
  pr <- project(make_projection_metadata_fixture(), h = 2, method = "CAGR", guard = FALSE)
  invalid <- pr
  invalid@dim_semantics$stat <- new_dim_semantics("stat", "statistic", "nominal", "partition", TRUE)
  expect_match(validObject(invalid, test = TRUE), "non-additive")
  invalid <- pr
  invalid@dim_semantics <- invalid@dim_semantics[-1L]
  expect_match(validObject(invalid, test = TRUE), "same order")
  invalid <- pr
  invalid@dim_semantics$stat <- NULL
  expect_match(validObject(invalid, test = TRUE), "same order")
  invalid <- pr
  invalid@dim_semantics$race <- pa_update_dim_semantics(invalid@dim_semantics$race,
    applicability = list(by = "stat", schemas = list(
      list(from = NULL, through = NULL, levels = dimnames(pr)$race))))
  expect_error(as.poparray(invalid), "controlling dimension.*dropped")
})
