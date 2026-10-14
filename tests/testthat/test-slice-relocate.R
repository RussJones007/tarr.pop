make_axis_verb_fixture <- function(applicability = FALSE) {
  dn <- list(year = c("2015", "2016", "2017", "2018"),
             area.name = c("Dallas", "Tarrant", "Denton"),
             age.char = c("0-4", "5-9", "10-14"), sex = c("female", "male"))
  x <- as.poparray(array(seq_len(prod(lengths(dn))), unname(lengths(dn)), dn),
                    filepath = tempfile(fileext = ".h5"))
  source_meta(x) <- list(note = "Axis verb test")
  sem <- dim_semantics(x)
  sem$sex <- pa_update_dim_semantics(sem$sex, partition_type = "partition")
  dim_semantics(x) <- sem
  if (applicability) {
    sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
      by = "year", schemas = list(
        list(from = "2015", through = "2016", levels = c("0-4", "5-9")),
        list(from = "2017", through = "2018", levels = c("5-9", "10-14")))))
    dim_semantics(x) <- sem
  }
  x
}

without_axis_value_reads <- function(code) {
  read_hdf5 <- get("h5mread", asNamespace("HDF5Array"))
  testthat::local_mocked_bindings(
    h5mread = function(..., starts = NULL) {
      empty_axis <- vapply(starts, function(i) !is.null(i) && length(i) == 0L,
                           logical(1))
      if (!any(empty_axis)) stop("Population values must not be read")
      read_hdf5(..., starts = starts)
    }, .package = "HDF5Array")
  force(code)
}

test_that("head and tail match dplyr size rules on dimension coordinates", {
  x <- make_axis_verb_fixture()
  proxy <- data.frame(position = seq_along(times(x)))
  sizes <- list(list(), list(n = 0), list(n = 1), list(n = 99), list(n = -1),
                list(n = -99), list(prop = .6), list(prop = -.4),
                list(prop = 0), list(prop = 2), list(prop = -2))
  for (verb in list(dplyr::slice_head, dplyr::slice_tail)) {
    for (size in sizes) {
      out <- without_axis_value_reads(do.call(verb, c(
        list(x, .dimension = "year"), size)))
      indices <- do.call(verb, c(list(proxy), size))$position
      expect_identical(times(out), times(x)[indices])
      expect_identical(dim(out), c(length(indices), 3L, 3L, 2L))
      expect_identical(roles(out), roles(x))
      expect_identical(source_meta(out), source_meta(x))
      expect_identical(data_col(out), data_col(x))
      expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
      expect_true(methods::validObject(out))
      expect_equal(as.array(out), as.array(x)[indices, , , , drop = FALSE])
    }
  }
})

test_that("head and tail compose with renamed, filtered and empty dimensions", {
  x <- make_axis_verb_fixture(applicability = TRUE)
  renamed <- dplyr::rename(x, time = year) |>
    dplyr::filter(time >= 2016) |>
    dplyr::relocate(age.char)
  out <- without_axis_value_reads(dplyr::slice_tail(renamed, n = 2,
                                                   .dimension = "time"))
  expect_identical(times(out), c("2017", "2018"))
  expect_identical(dim_semantics(out),
                   dim_semantics(dplyr::slice(renamed, 2:3, .dimension = "time")))
  for (verb in list(dplyr::slice_head, dplyr::slice_tail)) {
    empty <- dplyr::slice(x, integer(), .dimension = "year")
    expect_identical(times(verb(empty, .dimension = "year")), character())
    expect_identical(areas(verb(x, n = 1, .dimension = "area.name")),
                     if (identical(verb, dplyr::slice_head)) "Dallas" else "Denton")
    expect_error(verb(x), "dimension")
    expect_error(verb(x, .dimension = "unknown"), "Unknown dimension")
    expect_error(verb(x, .dimension = c("year", "sex")))
    expect_error(verb(x, .dimension = "year", by = sex), "Grouped")
    expect_error(verb(x, .dimension = "year", .by = sex))
    expect_error(verb(x, 2, .dimension = "year"))
    expect_error(verb(x, n = 1, prop = .5, .dimension = "year"))
    for (size in list(NA_real_, NaN, "2", c(1, 2))) {
      expect_error(verb(x, n = size, .dimension = "year"))
      expect_error(verb(x, prop = size, .dimension = "year"))
    }
  }
  expect_identical(times(x), as.character(2015:2018))
})

test_that("rename_with preserves delayed data, roles and applicability", {
  x <- make_axis_verb_fixture(applicability = TRUE) |>
    dplyr::filter(year >= 2016)
  out <- without_axis_value_reads(dplyr::rename_with(x, toupper))
  expect_named(dimnames(out), toupper(names(dimnames(x))))
  expect_identical(unname(dimnames(out)), unname(dimnames(x)))
  expect_identical(time_role(out), "YEAR")
  expect_identical(area_role(out), "AREA.NAME")
  expect_identical(dim_semantics(out)$AGE.CHAR@domain,
                   dim_semantics(x)$age.char@domain)
  expect_identical(pa_dim_applicability(dim_semantics(out)$AGE.CHAR)$by, "YEAR")
  expect_identical(source_meta(out), source_meta(x))
  expect_identical(data_col(out), data_col(x))
  expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
  expect_true(methods::validObject(out))
  expect_equal(unname(as.array(out)), unname(as.array(x)))
  expect_identical(ages(dplyr::filter(out, AGE.CHAR >= 10)), "10-14")
  selected <- dplyr::rename_with(x, ~ gsub(".", "_", .x, fixed = TRUE),
                                dplyr::ends_with(".char"))
  expect_named(dimnames(selected), c("year", "area.name", "age_char", "sex"))
  suffixed <- dplyr::rename_with(x, paste0, dplyr::all_of("year"), "_time")
  expect_identical(time_role(suffixed), "year_time")
  expect_identical(dplyr::rename_with(x, "toupper"), out)
  expect_identical(dplyr::rename_with(x, identity), x)
  expect_identical(dplyr::rename_with(x, toupper, dplyr::any_of("absent")), x)
  swapped <- dplyr::rename_with(x, ~ rev(.x), c(year, area.name))
  expect_identical(time_role(swapped), "area.name")
  expect_identical(area_role(swapped), "year")
  path <- tempfile(fileext = ".h5")
  withr::defer(unlink(path))
  saved <- dplyr::slice_tail(out, n = 2, .dimension = "YEAR")
  save_poparray(saved, filepath = path, series_id = "edge-rename")
  reopened <- open_poparray(filepath = path)
  expect_identical(dimnames(reopened), dimnames(saved))
  expect_identical(dim_semantics(reopened), dim_semantics(saved))
  expect_identical(roles(reopened), roles(saved))
  expect_equal(as.array(reopened), as.array(saved))
})

test_that("rename_with rejects invalid outputs and selectors", {
  x <- make_axis_verb_fixture()
  expect_error(dplyr::rename_with(x, toupper, unknown))
  expect_error(dplyr::rename_with(x, toupper, c(time = year)))
  for (fn in list(function(x) 1, function(x) character(),
                 function(x) rep(NA_character_, length(x)),
                 function(x) rep(" ", length(x)),
                 function(x) rep("duplicate", length(x)))) {
    expect_error(dplyr::rename_with(x, fn))
  }
  expect_error(dplyr::rename_with(x, function(x) "year", sex))
  expect_error(dplyr::rename_with(x, function(x) "prefix", dplyr::any_of("absent")),
               "length 0")
})

test_that("slice selects current named positions lazily and preserves metadata", {
  x <- make_axis_verb_fixture()
  expected <- as.array(x)[c(1, 3), , , , drop = FALSE]
  out <- without_axis_value_reads(dplyr::slice(x, 1, 3, .dimension = "year"))
  expect_s4_class(out, "poparray")
  expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
  expect_true(is_hdf5_backed_delayed(out))
  expect_identical(source_meta(out), source_meta(x))
  expect_identical(data_col(out), data_col(x))
  expect_identical(roles(out), roles(x))
  expect_equal(as.array(out), expected)
  expect_identical(times(x), as.character(2015:2018))
  positions <- c(3L, 1L)
  county <- dplyr::slice(x, positions, .dimension = "area.name")
  expect_identical(areas(county), c("Denton", "Dallas"))
  expect_equal(as.array(county), as.array(x)[, positions, , , drop = FALSE])
  filtered <- dplyr::filter(x, year >= 2016)
  expect_identical(times(dplyr::slice(filtered, 1, .dimension = "year")), "2016")
})

test_that("slice exclusions, zeros, empty selections and singleton dimensions work", {
  x <- make_axis_verb_fixture()
  expect_identical(times(dplyr::slice(x, c(-1, -99, 0), .dimension = "year")),
                   c("2016", "2017", "2018"))
  expect_identical(times(dplyr::slice(x, c(0, 2, 99), .dimension = "year")), "2016")
  singleton <- dplyr::slice(x, 1, .dimension = "area.name")
  expect_identical(names(dimnames(singleton)), names(dimnames(x)))
  expect_identical(dim(singleton), c(4L, 1L, 3L, 2L))
  for (positions in list(integer(), 0, 99, -seq_len(4))) {
    empty <- dplyr::slice(x, positions, .dimension = "year")
    expect_s4_class(empty, "poparray")
    expect_identical(times(empty), character())
    expect_true(methods::validObject(empty))
    expect_identical(time_role(empty), "year")
  }
  expect_identical(dplyr::slice(x, .dimension = "year"), x)
})

test_that("slice rejects ambiguous or unsafe positions and grouping", {
  x <- make_axis_verb_fixture()
  expect_error(dplyr::slice(x, 1), "dimension")
  expect_error(dplyr::slice(x, 1, .dimension = "unknown"), "Unknown dimension")
  expect_error(dplyr::slice(x, 1, .dimension = c("year", "sex")))
  expect_error(dplyr::slice(x, year = 1, .dimension = "year"), "unnamed")
  for (positions in list(NA_integer_, NaN, Inf, 1.5, "1", TRUE, list(1))) {
    expect_error(dplyr::slice(x, positions, .dimension = "year"))
  }
  expect_error(dplyr::slice(x, c(1, -2), .dimension = "year"), "same sign|positive")
  expect_error(dplyr::slice(x, c(1, 1), .dimension = "area.name"), "Duplicated")
  expect_error(dplyr::slice(x, c(2, 1), .dimension = "year"), "increasing")
  expect_error(dplyr::slice(x, 1, .dimension = "year", .by = sex), "Grouped")
  expect_error(dplyr::slice(x, 1, .dimension = "year", .preserve = TRUE), "Grouped")
})

test_that("slice trims applicability for controller and target selections", {
  x <- make_axis_verb_fixture(applicability = TRUE)
  out <- without_axis_value_reads(dplyr::slice(x, 3:4, .dimension = "year"))
  expect_identical(pa_dim_applicability(dim_semantics(out)$age.char)$schemas,
                   list(list(from = NULL, through = NULL,
                             levels = c("5-9", "10-14"))))
  target <- dplyr::slice(out, 3, .dimension = "age.char")
  expect_identical(pa_dim_applicability(dim_semantics(target)$age.char)$schemas[[1]]$levels,
                   "10-14")
  expect_equal(sum(target), sum(as.array(target)))
  empty <- dplyr::slice(x, integer(), .dimension = "year")
  expect_true(methods::validObject(empty))
  expect_length(pa_dim_applicability(dim_semantics(empty)$age.char)$schemas, 0)
})

test_that("relocate applies a lazy permutation with named semantic integrity", {
  x <- make_axis_verb_fixture(applicability = TRUE)
  expected <- aperm(as.array(x), c(3, 1, 2, 4))
  out <- without_axis_value_reads(dplyr::relocate(x, age.char, .before = year))
  expect_identical(names(dimnames(out)), c("age.char", "year", "area.name", "sex"))
  expect_identical(dimnames(out), dimnames(x)[c(3, 1, 2, 4)])
  expect_identical(dim_semantics(out), dim_semantics(x)[c(3, 1, 2, 4)])
  expect_identical(roles(out), roles(x))
  expect_identical(source_meta(out), source_meta(x))
  expect_identical(data_col(out), data_col(x))
  expect_identical(DelayedArray::seed(out), DelayedArray::seed(x))
  expect_true(is_hdf5_backed_delayed(out))
  expect_true(methods::validObject(out))
  expect_equal(as.array(out), expected)
  expect_equal(sum(out), sum(x))
  expect_identical(names(dimnames(x)), c("year", "area.name", "age.char", "sex"))
})

test_that("relocate supports selectors and destinations without renaming", {
  x <- make_axis_verb_fixture()
  cases <- list(
    rlang::expr(dplyr::relocate(x, sex, age.char)),
    rlang::expr(dplyr::relocate(x, year, .after = area.name)),
    rlang::expr(dplyr::relocate(x, area.name, .after = dplyr::last_col())),
    rlang::expr(dplyr::relocate(x, dplyr::all_of(c("sex", "year")), .before = age.char)),
    rlang::expr(dplyr::relocate(x, year, .after = c(year, area.name))),
    rlang::expr(dplyr::relocate(x, age.char, .before = dplyr::any_of("absent"))),
    rlang::expr(dplyr::relocate(x, year, .after = dplyr::any_of("absent")))
  )
  proxy <- as.data.frame(stats::setNames(as.list(seq_along(dim(x))), names(dimnames(x))))
  for (expression in cases) {
    actual <- rlang::eval_tidy(expression)
    reference <- rlang::eval_tidy(expression, data = list(x = proxy))
    expect_identical(names(dimnames(actual)), names(reference))
  }
  expect_identical(dplyr::relocate(x), x)
  expect_identical(dplyr::relocate(x, year), x)
  expect_error(dplyr::relocate(x, missing_dimension))
  expect_error(dplyr::relocate(x, year, .before = missing_dimension))
  expect_error(dplyr::relocate(x, year, .before = sex, .after = sex), "only one")
  expect_error(dplyr::relocate(x, time = year), "rename|renam")
})

test_that("slice and relocate compose after renaming and persist correctly", {
  x <- make_axis_verb_fixture(applicability = TRUE)
  out <- x |>
    dplyr::rename(time = year, county = area.name, age = age.char) |>
    dplyr::filter(time >= 2016) |>
    dplyr::relocate(age, .before = time) |>
    dplyr::slice(2:3, .dimension = "time") |>
    dplyr::slice(-1, .dimension = "county")
  expect_identical(times(out), c("2017", "2018"))
  expect_identical(areas(out), c("Tarrant", "Denton"))
  expect_identical(pa_dim_applicability(dim_semantics(out)$age)$by, "time")
  expect_identical(ages(dplyr::filter(out, age >= 10)), "10-14")
  path <- tempfile(fileext = ".h5")
  withr::defer(unlink(path))
  save_poparray(out, filepath = path, series_id = "axis-verbs")
  reopened <- open_poparray(filepath = path)
  expect_identical(dimnames(reopened), dimnames(out))
  expect_identical(dim_semantics(reopened), dim_semantics(out))
  expect_identical(roles(reopened), roles(out))
  expect_equal(as.array(reopened), as.array(out))
})
