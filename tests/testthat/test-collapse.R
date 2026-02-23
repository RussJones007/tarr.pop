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
    area_dim = "area.name"
  )
  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  new_poparray(dx, dimnames_list = dn, time_dim = "time", area_dim = "area.name")
}

test_that("collapse_dim generic works with positional args", {
  pa <- make_collapse_fixture()
  groups <- c("0-4" = "0-9", "5-9" = "0-9")

  out <- collapse_dim(pa, "age.char", groups)

  expect_s4_class(out, "poparray")
  expect_equal(dimnames(out)$age.char, "0-9")
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
