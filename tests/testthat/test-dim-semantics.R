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
    dsem$sex <- list(
      class = "set",
      exclusive = FALSE,
      overlapping = TRUE,
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

test_that("subsetting to one level marks dimension exclusive", {
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
  expect_true(dsem$sex$exclusive)
  expect_false(dsem$sex$overlapping)
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
