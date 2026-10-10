make_collapse_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    age.char = c("0-4", "5-9")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  as.poparray(arr, filepath = tempfile("collapse_fixture_", fileext = ".h5"))
}

make_collapse_time_role_fixture <- function() {
  dn <- list(
    time = c("2020", "2021"),
    area.name = "A",
    age.char = c("0-4", "5-9")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  fp <- tempfile("collapse_time_fixture_", fileext = ".h5")
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

test_that("collapse_dim generic works with positional args", {
  pa <- make_collapse_fixture()
  groups <- c("0-4" = "0-9", "5-9" = "0-9")

  out <- collapse_dim(pa, "age.char", groups)

  expect_s4_class(out, "poparray")
  expect_equal(dimnames(out)$age.char, "0-9")
})

test_that("collapse_all sequentially collapses named and indexed dimensions", {
  pa <- make_collapse_time_role_fixture()
  out <- collapse_all(pa, c("age.char", "area.name"), label = "Total")
  indexed <- collapse_all(pa, c(3, 2), label = "Total")
  expect_equal(dim(out), c(2L, 1L, 1L))
  expect_equal(as.numeric(out), c(4, 6))
  expect_equal(as.array(indexed), as.array(out))
  expect_equal(dimnames(out)$age.char, "Total")
  expect_equal(dimnames(out)$area.name, "Total")
  expect_equal(time_role(out), "time")
  expect_equal(area_role(out), "area.name")
  expect_named(dim_semantics(out), names(dimnames(pa)))
  expect_equal(get_source(out), get_source(pa))
  expect_true(is_hdf5_backed_delayed(out))
  expect_equal(dimnames(pa)$age.char, c("0-4", "5-9"))
  expect_equal(as.numeric(collapse_all(pa, "age.char")), c(4, 6))
})

test_that("collapse_all validates every dimension before reducing", {
  pa <- make_collapse_fixture()
  testthat::local_mocked_bindings(
    collapse_dim = function(...) stop("Reduction must not run"),
    .package = "tarr.pop"
  )
  expect_error(collapse_all(pa, c("age.char", "missing")), "unknown dim")
  expect_error(collapse_all(pa, c(3, 4)), "dim")
  expect_error(collapse_all(pa, c(1, 1.5)), "dim")
  expect_error(collapse_all(pa, c(0, 3)), "dim")
  expect_error(collapse_all(pa, c(1, NA)), "dim")
  expect_error(collapse_all(pa, c("year", NA_character_)), "dim")
  expect_error(collapse_all(pa, character()), "dim")
  expect_error(collapse_all(pa, integer()), "dim")
  expect_error(collapse_all(pa, TRUE), "character or numeric")
  expect_error(collapse_all(pa, c("age.char", "age.char")), "duplicate")
  expect_error(collapse_all(pa, c(3, 3)), "duplicate")
})

test_that("collapse_dim preserves non-default time role metadata", {
  pa <- make_collapse_time_role_fixture()
  groups <- c("0-4" = "0-9", "5-9" = "0-9")

  out <- collapse_dim(pa, "age.char", groups)

  expect_s4_class(out, "poparray")
  expect_equal(time_role(out), "time")
  expect_equal(area_role(out), "area.name")
  expect_true("time" %in% names(dimnames(out)))
})

test_that("renaming collapsed role dimension updates roles", {
  pa <- make_collapse_fixture()
  groups <- c("2020" = "p0", "2021" = "p1")

  out <- collapse_dim(pa, "year", groups, name = "period")

  expect_s4_class(out, "poparray")
  expect_true("period" %in% names(dimnames(out)))
  expect_false("year" %in% names(dimnames(out)))
  expect_equal(time_role(out), "period")
  expect_equal(area_role(out), "area.name")
})

test_that("keep_empty retains declared unused factor levels", {
  arr <- array(
    c(5, 7),
    dim = c(1, 1, 2),
    dimnames = list(
      year = "2020",
      area.name = "A",
      age.char = c("0-4", "5-9")
    )
  )
  pa <- as.poparray(arr, filepath = tempfile("collapse_empty_", fileext = ".h5"))

  groups <- factor(c("A", "A"), levels = c("A", "B"))

  out <- collapse_dim(pa, "age.char", groups, keep_empty = TRUE)
  arr_out <- as.array(out)

  expect_equal(dimnames(out)$age.char, c("A", "B"))
  expect_equal(as.numeric(arr_out[1, 1, 1]), 12)
  expect_equal(as.numeric(arr_out[1, 1, 2]), 0)
})

test_that("collapse_dim blocks unsafe grouped reductions by default", {
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
    partition_type = "set",
    validated = TRUE
  )

  fp <- tempfile("collapse_unsafe_", fileext = ".h5")
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
    dx,
    dimnames_list = dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = dsem
  )

  expect_error(
    collapse_dim(pa, "age.char", c("0-9" = "child", "5-14" = "child")),
    "Unsafe collapse blocked"
  )
  expect_warning(
    collapse_dim(pa, "age.char", c("0-9" = "child", "5-14" = "child"), strict = FALSE),
    "Unsafe collapse blocked"
  )
  expect_silent(
    collapse_dim(pa, "age.char", c("0-9" = "child", "5-14" = "child"), allow_overlap = TRUE)
  )
  expect_error(collapse_all(pa, c("area.name", "age.char")), "Unsafe collapse blocked")
  expect_warning(collapse_all(pa, c("area.name", "age.char"), strict = FALSE), "Unsafe collapse blocked")
  expect_silent(collapse_all(pa, c(2L, 3L), allow_overlap = TRUE))
})

test_that("collapse_dim stays HDF5-backed without writeHDF5Array persistence", {
  pa <- make_collapse_fixture()
  groups <- c("0-4" = "0-9", "5-9" = "0-9")

  out <- collapse_dim(pa, "age.char", groups)

  expect_s4_class(out, "poparray")
  expect_true(tarr.pop:::is_hdf5_backed_delayed(out))
  expect_equal(dimnames(out)$age.char, "0-9")
})
