make_dim_semantics_fixture <- function(include_strata = TRUE, overlap_strata = TRUE) {
  dn <- if (isTRUE(include_strata)) {
    list(
      year = c("2020", "2021"),
      area.name = c("A", "B"),
      sex = c("Female", "Male")
    )
  } else {
    list(
      year = c("2020", "2021"),
      area.name = c("A", "B")
    )
  }

  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  dsem <- default_dim_semantics(names(dn), "year", "area.name")
  if ("sex" %in% names(dsem) && isTRUE(overlap_strata)) {
    dsem$sex <- pa_update_dim_semantics(
      dsem$sex,
      overlap_levels = "Female",
      validated = TRUE
    )
  }

  fp <- tempfile("dim_semantics_fixture_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem,
    data_col = "population"
  )

  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  list(dx = dx, dn = dn, dsem = dsem)
}

test_that("constructor fails when dim_semantics is missing", {
  fx <- make_dim_semantics_fixture(include_strata = FALSE)
  expect_error(
    new_poparray(
      x = fx$dx,
      dimnames_list = fx$dn,
      time_dim = "year",
      area_dim = "area.name"
    ),
    "dim_semantics"
  )
})

test_that("constructor fails when dim_semantics names mismatch", {
  fx <- make_dim_semantics_fixture(include_strata = FALSE)
  bad <- fx$dsem
  names(bad) <- c("year", "county")
  expect_error(
    new_poparray(
      x = fx$dx,
      dimnames_list = fx$dn,
      time_dim = "year",
      area_dim = "area.name",
      dim_semantics = bad
    ),
    "must exactly match"
  )
})

test_that("safe partition cube allows sum", {
  fx <- make_dim_semantics_fixture(include_strata = FALSE)
  pa <- new_poparray(
    x = fx$dx,
    dimnames_list = fx$dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = fx$dsem
  )
  expect_equal(sum(pa), sum(as.array(pa)))
})

test_that("overlapping dimension errors under strict mode", {
  fx <- make_dim_semantics_fixture(include_strata = TRUE, overlap_strata = TRUE)
  pa <- new_poparray(
    x = fx$dx,
    dimnames_list = fx$dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = fx$dsem
  )
  expect_error(sum(pa), "Unsafe reduction blocked")
})

test_that("strict FALSE warns and allow_overlap TRUE proceeds", {
  fx <- make_dim_semantics_fixture(include_strata = TRUE, overlap_strata = TRUE)
  pa <- new_poparray(
    x = fx$dx,
    dimnames_list = fx$dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = fx$dsem
  )

  expect_warning(sum(pa, strict = FALSE), "Unsafe reduction blocked")
  expect_silent(sum(pa, allow_overlap = TRUE))
})

test_that("subsetting to one level removes overlap risk for selected set dims", {
  fx <- make_dim_semantics_fixture(include_strata = TRUE, overlap_strata = TRUE)
  pa <- new_poparray(
    x = fx$dx,
    dimnames_list = fx$dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = fx$dsem
  )

  sliced <- pa[, , "Female", drop = FALSE]
  dsem <- dim_semantics(sliced)
  expect_true(S7::S7_inherits(dsem$sex, DimSemantics))
  expect_identical(dsem$sex@overlap_levels, "Female")
  expect_silent(sum(sliced))
})

test_that("sum allows multiple active set levels after named overlaps are removed", {
  dn <- list(
    year = "2020",
    area.name = "A",
    race.eth = c("All races", "Asian", "Black", "White")
  )
  arr <- array(
    seq_len(prod(unname(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  dsem <- default_dim_semantics(names(dn), "year", "area.name")
  dsem$race.eth <- pa_update_dim_semantics(
    dsem$race.eth,
    partition_type = "set",
    overlap_levels = "All races",
    validated = TRUE
  )
  fp <- tempfile("active_set_levels_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )
  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  pa <- new_poparray(
    x = dx,
    dimnames_list = dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )

  expect_error(sum(pa), "Unsafe reduction blocked")

  filtered <- pa[, , c("Asian", "Black", "White"), drop = FALSE]
  expect_identical(dim_semantics(filtered)$race.eth@overlap_levels, "All races")
  expect_silent(sum(filtered))
  expect_equal(sum(filtered), sum(as.array(filtered)))
})

test_that("dropped dimensions remove semantic entry", {
  fx <- make_dim_semantics_fixture(include_strata = TRUE, overlap_strata = TRUE)
  after_dn <- fx$dn[c("year", "area.name")]
  out <- tarr.pop:::subset_dim_semantics(
    dim_semantics = fx$dsem,
    before_dimnames = fx$dn,
    after_dimnames = after_dn
  )
  expect_false("sex" %in% names(out))
})

test_that("HDF5 round-trip preserves dim_semantics", {
  fx <- make_dim_semantics_fixture(include_strata = TRUE, overlap_strata = TRUE)
  pa <- new_poparray(
    x = fx$dx,
    dimnames_list = fx$dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = fx$dsem
  )

  tmp <- tempfile("dim_semantics_roundtrip_", fileext = ".h5")
  save_poparray(pa, filepath = tmp, overwrite = TRUE, series_id = "dim_semantics_roundtrip")

  testthat::local_mocked_bindings(
    tarr_series_registry = function() {
      data.frame(
        series_id = "dim_semantics_roundtrip",
        filename = basename(tmp),
        filepath = tmp,
        stringsAsFactors = FALSE
      )
    },
    .package = "tarr.pop"
  )

  out <- open_poparray("dim_semantics_roundtrip")
  expect_equal(dim_semantics(out), dim_semantics(pa))
})

test_that("dim semantics metadata is stable across cached and uncached opens", {
  fx <- make_dim_semantics_fixture(include_strata = TRUE, overlap_strata = TRUE)
  pa <- new_poparray(
    x = fx$dx,
    dimnames_list = fx$dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = fx$dsem
  )

  tmp <- tempfile("dim_semantics_cache_", fileext = ".h5")
  save_poparray(pa, filepath = tmp, overwrite = TRUE, series_id = "dim_semantics_cache")

  testthat::local_mocked_bindings(
    tarr_series_registry = function() {
      data.frame(
        series_id = "dim_semantics_cache",
        filename = basename(tmp),
        filepath = tmp,
        stringsAsFactors = FALSE
      )
    },
    .package = "tarr.pop"
  )

  out_cached <- open_poparray("dim_semantics_cache")
  reset_poparray_cache()
  out_uncached <- open_poparray("dim_semantics_cache")

  expect_equal(dim_semantics(out_cached), dim_semantics(out_uncached))
})

test_that("interval dimensions derive overlap risk from labels", {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    age.char = c("0-9", "5-14")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  dsem <- default_dim_semantics(names(dn), "year", "area.name")
  dsem$age.char <- pa_update_dim_semantics(
    dsem$age.char,
    domain = "age",
    scale_type = "interval",
    partition_type = "set",
    validated = TRUE
  )

  fp <- tempfile("dim_semantics_interval_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )

  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  pa <- new_poparray(
    x = dx,
    dimnames_list = dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )

  expect_error(sum(pa), "Unsafe reduction blocked")
  expect_silent(sum(pa[, , "0-9", drop = FALSE]))
})

test_that("sum allows multiple non-overlapping intervals after filtering", {
  dn <- list(
    year = "2020",
    area.name = "A",
    age.char = c("0-9", "5-14", "15-19", "20-24")
  )
  arr <- array(
    seq_len(prod(unname(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  dsem <- default_dim_semantics(names(dn), "year", "area.name")
  dsem$age.char <- pa_update_dim_semantics(
    dsem$age.char,
    partition_type = "set",
    validated = TRUE
  )
  fp <- tempfile("active_interval_levels_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )
  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  pa <- new_poparray(
    x = dx,
    dimnames_list = dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )

  expect_error(sum(pa), "Unsafe reduction blocked")

  filtered <- pa[, , c("15-19", "20-24"), drop = FALSE]
  expect_silent(sum(filtered))
  expect_equal(sum(filtered), sum(as.array(filtered)))
})

test_that("ensure_dim_semantics coerces legacy entries", {
  dnames <- c("year", "area.name", "sex")
  legacy <- list(
    year = list(class = "partition", validated = TRUE),
    area.name = list(class = "partition", validated = TRUE),
    sex = list(class = "set", validated = TRUE)
  )

  out <- tarr.pop:::ensure_dim_semantics(
    dim_semantics = legacy,
    dim_names = dnames,
    time_dim = "year",
    area_dim = "area.name"
  )

  expect_true(all(vapply(out, function(x) S7::S7_inherits(x, DimSemantics), logical(1))))
  expect_identical(out$year@partition_type, "partition")
  expect_identical(out$sex@partition_type, "set")
})

test_that("default semantics treat age.char as interval age domain", {
  dsem <- default_dim_semantics(c("year", "area.name", "age.char"), "year", "area.name")
  expect_identical(dsem$age.char@domain, "age")
  expect_identical(dsem$age.char@scale_type, "interval")
})

test_that("live cubes expose interval semantics for age.char", {
  reset_test_cube_root()
  ids <- tarr.pop:::tarr_series_registry()$series_id
  with_age <- ids[vapply(ids, function(id) {
    x <- open_poparray(id)
    "age.char" %in% names(dim_semantics(x))
  }, logical(1))]

  expect_true(length(with_age) > 0L)

  for (id in with_age) {
    x <- open_poparray(id)
    expect_identical(dim_semantics(x)$age.char@scale_type, "interval", info = id)
  }
})
