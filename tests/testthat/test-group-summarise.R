make_group_verb_fixture <- function(applicability = FALSE, integer_values = FALSE,
                                    overlap = FALSE) {
  dn <- list(year = as.character(2020:2022), area.name = c("A", "B"),
             age.char = c("0-4", "5-9"), sex = c("female", "male"))
  if (overlap) dn$age.char <- c("0-9", "5-14")
  values <- seq_len(prod(lengths(dn)))
  if (!integer_values) values <- as.numeric(values)
  arr <- array(values, unname(lengths(dn)), dn)
  if (applicability) {
    arr[1, , 2, ] <- 999
    arr[2, , 1, ] <- 999
    arr[3, , , ] <- 999
    arr[2, 1, 2, 1] <- NA
  }
  x <- as.poparray(arr, filepath = tempfile(fileext = ".h5"))
  source_meta(x) <- list(note = "Grouping fixture")
  sem <- dim_semantics(x)
  sem$sex <- pa_update_dim_semantics(sem$sex, partition_type = "partition")
  if (applicability) {
    sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
      by = "year", schemas = list(
        list(from = "2020", through = "2020", levels = "0-4"),
        list(from = "2021", through = "2021", levels = "5-9"),
        list(from = "2022", through = "2022", levels = character()))))
  }
  dim_semantics(x) <- sem
  x
}

without_group_value_reads <- function(code) {
  read_hdf5 <- get("h5mread", asNamespace("HDF5Array"))
  testthat::local_mocked_bindings(h5mread = function(..., starts = NULL) {
    empty_axis <- vapply(starts, function(i) !is.null(i) && length(i) == 0L,
                         logical(1))
    if (!any(empty_axis)) stop("Population values must not be read")
    read_hdf5(..., starts = starts)
  }, .package = "HDF5Array")
  force(code)
}

test_that("group_by and ungroup only change valid grouping metadata", {
  x <- make_group_verb_fixture()
  grouped <- without_group_value_reads(dplyr::group_by(x, area.name, year))
  expect_s4_class(grouped, "poparray")
  expect_identical(dplyr::group_vars(grouped), c("area.name", "year"))
  expect_identical(dplyr::groups(grouped), rlang::syms(c("area.name", "year")))
  expect_identical(dimnames(grouped), dimnames(x))
  expect_identical(dim_semantics(grouped), dim_semantics(x))
  expect_identical(roles(grouped), roles(x))
  expect_identical(source_meta(grouped), source_meta(x))
  expect_identical(DelayedArray::seed(grouped), DelayedArray::seed(x))
  expect_identical(dplyr::group_vars(x), character())
  expect_identical(dplyr::group_vars(dplyr::group_by(grouped, sex)), "sex")
  added <- dplyr::group_by(grouped, dplyr::all_of(c("year", "sex")), .add = TRUE)
  expect_identical(dplyr::group_vars(added), c("area.name", "year", "sex"))
  expect_identical(dplyr::group_vars(dplyr::ungroup(added, year)), c("area.name", "sex"))
  expect_identical(without_group_value_reads(dplyr::ungroup(grouped)), x)
  expect_identical(dplyr::group_by(grouped), x)
  expect_identical(dplyr::group_by(grouped, .add = TRUE), grouped)
  expect_error(dplyr::group_by(x, unknown))
  expect_error(dplyr::group_by(x, period = year))
  expect_error(dplyr::group_by(x, year >= 2021))
  expect_error(dplyr::group_by(x, year, .drop = FALSE), "drop")
  expect_error(dplyr::group_by(x, year, .add = NA))
  bad <- grouped
  bad@group_vars <- c("missing", "missing")
  expect_error(validate_poparray(bad), "group_vars")
})

test_that("grouping follows lazy axis operations and remains an analysis setting", {
  x <- make_group_verb_fixture()
  out <- without_group_value_reads(x |>
    dplyr::group_by(year, area.name, age.char) |>
    dplyr::rename_with(toupper) |>
    dplyr::filter(YEAR >= 2021) |>
    dplyr::slice_head(n = 1, .dimension = "AREA.NAME") |>
    dplyr::relocate(AGE.CHAR) |>
    dplyr::slice_tail(n = 1, .dimension = "YEAR"))
  expect_identical(dplyr::group_vars(out), c("YEAR", "AREA.NAME", "AGE.CHAR"))
  expect_identical(times(out), "2022")
  expect_identical(areas(out), "A")
  grouped <- dplyr::group_by(x, year, area.name, age.char)
  reduced <- collapse_dim(grouped, "age.char", list(Total = c("0-4", "5-9")), drop = TRUE)
  expect_identical(dplyr::group_vars(reduced), c("year", "area.name"))
  selected <- grouped[age.char = "0-4", drop = TRUE]
  expect_identical(dplyr::group_vars(selected), c("year", "area.name"))
  renamed <- collapse_dim(grouped, "age.char", list(Young = "0-4", Older = "5-9"), name = "age")
  expect_identical(dplyr::group_vars(renamed), c("year", "area.name", "age"))
  path <- tempfile(fileext = ".h5")
  withr::defer(unlink(path))
  save_poparray(out, filepath = path, series_id = "grouping-view")
  reopened <- open_poparray(filepath = path)
  expect_identical(dplyr::group_vars(reopened), character())
  expect_identical(dimnames(reopened), dimnames(out))
  expect_equal(as.array(reopened), as.array(out))
})

test_that("summarise sums ungrouped axes and controls output grouping", {
  x <- make_group_verb_fixture()
  reference <- apply(as.array(x), c(1, 2), sum)
  grouped <- dplyr::group_by(x, area.name, year)
  out <- dplyr::summarise(grouped, total = sum(population), .groups = "drop")
  expect_s4_class(out, "poparray")
  expect_true(is_hdf5_backed_delayed(out))
  expect_named(dimnames(out), c("year", "area.name"))
  expect_equal(as.array(out), reference)
  expect_identical(data_col(out), "total")
  expect_identical(dplyr::group_vars(out), character())
  expect_identical(roles(out), list(time = "year", area = "area.name", strata = character()))
  expect_match(source_meta(out)$note, "Grouping fixture")
  expect_match(source_meta(out)$note, "summarise: total")
  expect_identical(dplyr::group_vars(dplyr::summarise(grouped, total = sum(population))), "area.name")
  kept <- dplyr::summarise(grouped, total = sum(population), .groups = "keep")
  expect_identical(dplyr::group_vars(kept), c("area.name", "year"))
  expect_equal(as.array(dplyr::summarise(kept, total = sum(total), .groups = "keep")), reference)
  per_call <- dplyr::summarize(x, total = base::sum(.data$population),
                              .by = c(year, area.name))
  expect_equal(as.array(per_call), reference)
  expect_identical(dplyr::group_vars(per_call), character())
  value <- data_col(x)
  expect_equal(as.array(dplyr::summarise(x, total = sum(.data[[value]]),
                                         .by = c(year, area.name))), reference)
  expect_identical(data_col(x), "population")
  expect_identical(dplyr::group_vars(x), character())
  expect_identical(dim(x), c(3L, 2L, 2L, 2L))
})

test_that("summarise validates the entire plan before population reads", {
  x <- make_group_verb_fixture()
  grouped <- dplyr::group_by(x, year, area.name)
  without_group_value_reads({
    expect_error(dplyr::summarise(x, total = sum(population)), "retain required")
    expect_error(dplyr::summarise(grouped), "exactly one named")
    expect_error(dplyr::summarise(grouped, sum(population)), "exactly one named")
    expect_error(dplyr::summarise(grouped, a = sum(population), b = sum(population)), "exactly one")
    expect_error(dplyr::summarise(grouped, a = mean(population)), "Only a population sum")
    expect_error(dplyr::summarise(grouped, a = sum(population * 2)), "current population")
    expect_error(dplyr::summarise(grouped, a = sum(unknown)), "current population")
    expect_error(dplyr::summarise(grouped, year = sum(population)), "collide")
    expect_error(dplyr::summarise(grouped, a = sum(population, 1)), "one population")
    expect_error(dplyr::summarise(grouped, a = sum(population, na.rm = NA)), "na.rm")
    expect_error(dplyr::summarise(grouped, a = sum(population), .groups = "rowwise"))
    expect_error(dplyr::summarise(grouped, a = sum(population), .by = c(year, area.name)), "cannot be combined")
    expect_error(dplyr::summarise(x, a = sum(population), .by = c(year, area.name), .groups = "drop"), "cannot be combined|supply both")
  })
  # Put an unsafe axis last to ensure an earlier safe axis is not reduced first.
  overlap <- dplyr::relocate(make_group_verb_fixture(overlap = TRUE),
                             age.char, .after = sex)
  without_group_value_reads(expect_error(dplyr::summarise(overlap,
    total = sum(population), .by = c(year, area.name)), "Unsafe collapse"))
  app <- make_group_verb_fixture(applicability = TRUE)
  sem <- dim_semantics(app)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "sex", schemas = list(list(from = NULL, through = NULL, levels = c("0-4", "5-9")))))
  dim_semantics(app) <- sem
  without_group_value_reads(expect_error(dplyr::summarise(app,
    total = sum(population), .by = c(year, area.name)), "controller.*sex"))
})

test_that("summarise distinguishes structural exclusions and applicable NA", {
  x <- make_group_verb_fixture(applicability = TRUE)
  raw <- as.array(x)
  expected <- raw[, , 1, 1] + raw[, , 1, 2]
  expected[2, ] <- raw[2, , 2, 1] + raw[2, , 2, 2]
  expected[3, ] <- NA_real_
  expect_warning(out <- dplyr::summarise(x, total = sum(population),
    .by = c(year, area.name)), "Non-derivable")
  expect_equal(as.array(out), expected)
  expected[2, 1] <- raw[2, 1, 2, 2]
  for (bytes in c(32, 128)) {
    withr::local_options(poparray.collapse_block_bytes = bytes)
    expect_warning(out <- dplyr::summarise(x, total = sum(population, na.rm = TRUE),
      .by = c(year, area.name)), "Non-derivable")
    expect_equal(as.array(out), expected)
  }
  # Retained category schemas also mask inapplicable coordinates with stored 999.
  per_age <- dplyr::summarise(x, total = sum(population, na.rm = TRUE),
    .by = c(year, area.name, age.char))
  expect_true(all(is.na(as.array(per_age)[1, , 2])))
  expect_true(all(is.na(as.array(per_age)[2, , 1])))
  expect_true(all(is.na(as.array(per_age)[3, , ])))
  all_axes <- dplyr::summarise(x, total = sum(population, na.rm = TRUE),
    .by = dplyr::everything())
  expect_identical(as.array(all_axes)[2, 1, 2, 1], 0)
  expect_true(all(is.na(as.array(all_axes)[3, , , ])))
  expect_identical(dplyr::group_vars(all_axes), character())
})

test_that("summary results support renamed roles, empty groups and large sums", {
  x <- make_group_verb_fixture() |>
    dplyr::rename(time = year, county = area.name) |>
    dplyr::filter(time >= 2021)
  out <- dplyr::summarise(x, total = sum(population), .by = c(time, county))
  expect_identical(time_role(out), "time")
  expect_identical(area_role(out), "county")
  expect_identical(times(out), c("2021", "2022"))
  empty <- dplyr::slice(x, integer(), .dimension = "time")
  result <- without_group_value_reads(dplyr::summarise(empty, total = sum(population),
    .by = c(time, county)))
  expect_identical(dim(result), c(0L, 2L))
  expect_true(methods::validObject(result))
  no_age <- dplyr::slice(x, integer(), .dimension = "age.char")
  expect_error(dplyr::summarise(no_age, total = sum(population),
    .by = c(time, county)), "empty reduction")
  dn <- list(year = "2020", area.name = "A", age.char = c("0-4", "5-9"))
  large <- as.poparray(array(rep(2000000000L, 2), c(1, 1, 2), dn),
                       filepath = tempfile(fileext = ".h5"))
  total <- dplyr::summarise(large, total = sum(population), .by = c(year, area.name))
  expect_identical(as.numeric(total), 4000000000)
  path <- tempfile(fileext = ".h5")
  withr::defer(unlink(path))
  save_poparray(out, filepath = path, series_id = "summary-result")
  reopened <- open_poparray(filepath = path)
  expect_identical(data_col(reopened), "total")
  expect_identical(dimnames(reopened), dimnames(out))
  expect_identical(dim_semantics(reopened), dim_semantics(out))
  expect_equal(as.array(reopened), as.array(out))
})
