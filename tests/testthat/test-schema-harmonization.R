make_harmonization_fixture <- function(missing = FALSE) {
  dn <- list(year = as.character(2000:2003), area.name = c("A", "B"),
    sex = c("female", "male"),
    age.char = c("70-74", "75-79", "80-84", "85+", "85-89", "90-94", "95+"))
  values <- array(NA_real_, unname(lengths(dn)), dn)
  values[1:2,,,1:4] <- rep(c(10, 20, 30, 160), each = 8)
  values[3:4,,,c(1:3,5:7)] <- rep(c(10, 20, 30, 100, 40, 20), each = 8)
  if (missing) values[3,1,1,6] <- NA_real_
  filepath <- tempfile(fileext = ".h5")
  pop <- as.poparray(values, filepath = filepath)
  sem <- dim_semantics(pop)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "year", schemas = list(
      list(from = NULL, through = "2001", levels = dn$age.char[1:4]),
      list(from = "2002", through = NULL, levels = dn$age.char[c(1:3,5:7)]))))
  dim_semantics(pop) <- sem
  list(pop = pop, values = values, filepath = filepath)
}

test_that("age harmonization selects contributors before propagating missingness", {
  fx <- make_harmonization_fixture()
  out <- suppressWarnings(group_ages(fx$pop, c("80-84", "85+")))
  expect_equal(as.array(out)[,,,1], array(30, c(4,2,2)), ignore_attr = TRUE)
  expect_equal(as.array(out)[,,,2], array(160, c(4,2,2)), ignore_attr = TRUE)
  expect_identical(dim_semantics(out)$age.char@applicability, list(by = "year",
    schemas = list(list(from = NULL, through = NULL, levels = c("80-84", "85+")))))
  expect_silent(validate_poparray(out))
  older <- group_ages(fx$pop, "70+")
  expect_equal(as.array(older), array(220, c(4,2,2,1)), ignore_attr = TRUE)
  expect_equal(rhdf5::h5read(fx$filepath, "cube/population"), fx$values, ignore_attr = TRUE)
  missing <- suppressWarnings(group_ages(make_harmonization_fixture(TRUE)$pop, "85+"))
  expect_true(is.na(as.array(missing)[3,1,1,1]))
  expect_equal(as.array(missing)[4,1,1,1], 160)
})

test_that("non-derivable age targets are NA with one informative warning", {
  fx <- make_harmonization_fixture()
  expect_warning(out <- group_ages(fx$pop, "90+"), "deriv")
  expect_true(all(is.na(as.array(out)[1:2,,,1])))
  expect_equal(as.array(out)[3:4,,,1], array(60, c(2,2,2)), ignore_attr = TRUE)
  app <- dim_semantics(out)$age.char@applicability
  expect_identical(app$schemas[[1]]$levels, character())
  expect_identical(app$schemas[[2]]$levels, "90+")
  expect_silent(validate_poparray(out))
  early <- fx$pop[year = c("2000", "2001")]
  expect_warning(only_na <- group_ages(early, "90+"), "deriv")
  expect_true(all(is.na(as.array(only_na))))
})

test_that("interval gaps are not guessed and exact source categories carry through", {
  fx <- make_harmonization_fixture()
  gap <- fx$pop[age.char = c("85+", "85-89", "95+")]
  expect_warning(out <- group_ages(gap, "85+"), "deriv")
  expect_equal(as.array(out)[1,1,1,1], 160)
  expect_true(all(is.na(as.array(out)[3:4,,,1])))
  exact <- suppressWarnings(group_ages(fx$pop, "80-84"))
  expect_true(all(as.array(exact) == 30))
})

test_that("generic union collapse excludes structural cells and preserves genuine NA", {
  fx <- make_harmonization_fixture()
  groups <- list(older = dimnames(fx$pop)$age.char)
  out <- collapse_dim(fx$pop, "age.char", groups)
  expect_true(all(as.array(out) == 220))
  expect_identical(dim_semantics(out)$age.char@applicability, list(by = "year",
    schemas = list(list(from = NULL, through = NULL, levels = "older"))))
  missing <- collapse_dim(make_harmonization_fixture(TRUE)$pop, "age.char", groups)
  expect_true(is.na(as.array(missing)[3,1,1,1]))
  expect_equal(as.array(missing)[4,1,1,1], 220)
})

test_that("current subsets compose with age grouping", {
  fx <- make_harmonization_fixture()
  for (pop in list(dplyr::filter(fx$pop, year <= 2001),
                   dplyr::filter(fx$pop, year >= 2002),
                   fx$pop[year = c("2001", "2002")])) {
    out <- group_ages(pop, "70+")
    expect_true(all(as.array(out) == 220))
    expect_silent(validate_poparray(out))
    expect_identical(dimnames(out)$year, dimnames(pop)$year)
  }
})

test_that("nominal schema grouping never decomposes a broader source category", {
  dn <- list(year = as.character(2000:2003), area.name = c("A", "B"),
    race = c("Other including Asian", "Other", "Asian"))
  values <- array(NA_real_, c(4,2,3), dn)
  values[1:2,,1] <- 120
  values[3:4,,2] <- 100
  values[3:4,,3] <- 20
  pop <- as.poparray(values, filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(pop)
  sem$race <- pa_update_dim_semantics(sem$race, partition_type = "partition", validated = TRUE,
    applicability = list(by = "year", schemas = list(
      list(from = NULL, through = "2001", levels = dn$race[1]),
      list(from = "2002", through = NULL, levels = dn$race[2:3]))))
  dim_semantics(pop) <- sem
  total <- collapse_dim(pop, "race", list(combined = dn$race))
  expect_true(all(as.array(total) == 120))
  expect_warning(asian <- withCallingHandlers(
    collapse_dim(pop, "race", list(Asian = "Asian")),
    warning = function(w) if (grepl("dropping", conditionMessage(w))) invokeRestart("muffleWarning")
  ), "deriv")
  expect_true(all(is.na(as.array(asian)[1:2,,1])))
  expect_true(all(as.array(asian)[3:4,,1] == 20))
})

test_that("derived HDF5 results preserve metadata through save and open", {
  fx <- make_harmonization_fixture()
  out <- group_ages(fx$pop, "70+")
  expect_true(is_hdf5_backed_delayed(out))
  expect_identical(time_role(out), time_role(fx$pop))
  expect_identical(area_role(out), area_role(fx$pop))
  expect_identical(data_col(out), data_col(fx$pop))
  filepath <- tempfile(fileext = ".h5")
  save_poparray(out, filepath = filepath, series_id = "harmonization_test")
  testthat::local_mocked_bindings(tarr_series_registry = function(root = resolve_cube_dir()) {
    data.frame(series_id = "harmonization_test", filename = basename(filepath), filepath = filepath)
  }, .package = "tarr.pop")
  withr::local_options(list(tarr.pop.cube_path = tempdir()))
  reopened <- open_poparray("harmonization_test")
  expect_equal(as.array(reopened), as.array(out))
  expect_identical(dimnames(reopened), dimnames(out))
  expect_identical(dim_semantics(reopened), dim_semantics(out))
  expect_identical(source_meta(reopened), source_meta(out))
})

test_that("schema dispatch realizes only bounded numeric blocks", {
  fx <- make_harmonization_fixture()
  withr::local_options(list(poparray.collapse_block_bytes = 7 * 8 * 2))
  tracker <- new.env(parent = emptyenv()); tracker$cells <- numeric()
  invisible(suppressMessages(trace("extract_array", signature = "HDF5ArraySeed", exit = substitute({
    assign("cells", c(tracker$cells, length(returnValue())), envir = tracker)
  }, list(tracker = tracker)), where = asNamespace("HDF5Array"), print = FALSE)))
  withr::defer(invisible(suppressMessages(untrace("extract_array", signature = "HDF5ArraySeed", where = asNamespace("HDF5Array")))))
  out <- group_ages(fx$pop, "70+")
  reads <- tracker$cells[tracker$cells > 0]
  expect_gt(length(reads), 1)
  expect_lte(max(reads), 14)
  expect_lt(max(reads), length(fx$values))
  expect_true(is_hdf5_backed_delayed(out))
})

test_that("schema-specific overlap still obeys strict and explicit override contracts", {
  fx <- make_harmonization_fixture()
  sem <- dim_semantics(fx$pop)
  app <- sem$age.char@applicability
  app$schemas[[2]]$levels <- c(app$schemas[[2]]$levels, "85+")
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = app)
  dim_semantics(fx$pop) <- sem
  groups <- list(older = dimnames(fx$pop)$age.char)
  expect_error(collapse_dim(fx$pop, "age.char", groups), "Unsafe collapse")
  expect_warning(collapse_dim(fx$pop, "age.char", groups, strict = FALSE), "Unsafe collapse")
  expect_silent(collapse_dim(fx$pop, "age.char", groups, allow_overlap = TRUE))
  expect_error(group_ages(fx$pop, "70+"), "Unsafe collapse")
  # Several overlapping intervals with no direct target must remain guarded.
  pop <- fx$pop[age.char = c("85+", "85-89", "90-94", "95+")]
  expect_warning(out <- group_ages(pop, "80+"), "deriv")
  expect_true(all(is.na(as.array(out))))
})

test_that("zero is a real applicable value", {
  fx <- make_harmonization_fixture()
  values <- fx$values
  values[3,,,5:7] <- 0
  pop <- as.poparray(values, filepath = tempfile(fileext = ".h5"))
  dim_semantics(pop) <- dim_semantics(fx$pop)
  out <- group_ages(pop, "70+")
  expect_true(all(as.array(out)[3,,,1] == 60))
})

test_that("controller dispatch follows named dimensions and current label order", {
  fx <- make_harmonization_fixture()
  pop <- fx$pop
  sem <- dim_semantics(pop)
  app <- sem$age.char@applicability
  # Controller is sex, after year/area in the block's row coordinates.
  app$by <- "sex"
  app$schemas[[1]]$through <- "female"
  app$schemas[[2]]$from <- "male"
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = app)
  values <- fx$values
  values[,,1,1:4] <- rep(c(10,20,30,160), each = 8)
  values[,,1,5:7] <- NA
  values[,,2,4] <- NA
  values[,,2,c(1:3,5:7)] <- rep(c(10,20,30,100,40,20), each = 8)
  pop <- as.poparray(values, filepath = tempfile(fileext = ".h5"))
  dim_semantics(pop) <- sem
  out <- group_ages(pop[sex = c("male", "female")], "70+")
  expect_true(all(as.array(out) == 220))
  expect_identical(dimnames(out)$sex, c("male", "female"))
})

test_that("legacy grouping without applicability keeps its previous values and guards", {
  fx <- make_harmonization_fixture()
  sem <- dim_semantics(fx$pop)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = NULL)
  dim_semantics(fx$pop) <- sem
  expect_error(collapse_dim(fx$pop, "age.char", list(older = dimnames(fx$pop)$age.char)), "Unsafe collapse")
  out <- collapse_dim(fx$pop, "age.char", list(older = dimnames(fx$pop)$age.char), allow_overlap = TRUE)
  expect_true(all(is.na(as.array(out))))
})

test_that("requested overlapping age outputs cannot bypass partition safety", {
  fx <- make_harmonization_fixture()
  expect_error(group_ages(fx$pop, c("85+", "90+")), "Requested age groups must not overlap")
  expect_error(group_ages(fx$pop, c("85+", "90+"), allow_overlap = TRUE), "Requested age groups must not overlap")
})

test_that("removed source levels cannot supply a requested age target", {
  fx <- make_harmonization_fixture()
  empty <- fx$pop[age.char = character()]
  expect_warning(out <- group_ages(empty, "70+"), "deriv")
  expect_true(all(is.na(as.array(out))))
  expect_silent(validate_poparray(out))
})

test_that("partial derived applicability survives HDF5 persistence", {
  fx <- make_harmonization_fixture()
  warnings <- character()
  out <- withCallingHandlers(group_ages(fx$pop, "90+"), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 1L)
  expect_match(warnings, "90.*2000.*2001")
  filepath <- tempfile(fileext = ".h5")
  save_poparray(out, filepath = filepath, series_id = "partial_harmonization")
  testthat::local_mocked_bindings(tarr_series_registry = function(root = resolve_cube_dir()) {
    data.frame(series_id = "partial_harmonization", filename = basename(filepath), filepath = filepath)
  }, .package = "tarr.pop")
  withr::local_options(list(tarr.pop.cube_path = tempdir()))
  reopened <- open_poparray("partial_harmonization")
  expect_equal(as.array(reopened), as.array(out))
  expect_identical(dim_semantics(reopened), dim_semantics(out))
  expect_identical(source_meta(reopened), source_meta(out))
  expect_silent(validate_poparray(reopened))
})

test_that("uniform harmonized results retain exact derivability in chained grouping", {
  fx <- make_harmonization_fixture()
  harmonized <- group_ages(fx$pop, "85+")
  expect_length(dim_semantics(harmonized)$age.char@applicability$schemas, 1L)
  expect_warning(narrower <- group_ages(harmonized, "90+"), "deriv")
  expect_true(all(is.na(as.array(narrower))))
  expect_silent(validate_poparray(narrower))
  expect_silent(identity <- group_ages(harmonized, "85+"))
  expect_true(all(as.array(identity) == 160))
})
