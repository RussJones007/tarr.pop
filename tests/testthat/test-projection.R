ns_fun <- function(name) getFromNamespace(name, "tarr.pop")

make_poparray_fixture <- function() {
  dn <- list(
    year = as.character(2018:2022),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  tarr.pop::as.poparray(
    arr,
    filepath = tempfile("projection_pop_fixture_", fileext = ".h5"),
    data_col = "population"
  )
}

test_that("writer creates single handle with named stat dimension", {
  out_dim <- c(year = 5, area.name = 2, stat = 2)
  out_dn <- list(
    year = as.character(1:5),
    area.name = c("x", "y"),
    stat = c("projection", "std_error")
  )
  
  out_dir <- tempfile("proj_writer_")
  dir.create(out_dir, recursive = TRUE)
  w <- ns_fun("tp_projection_hdf5_writer")(out_dim, out_dn, year_k = 1, dir = out_dir)
  h <- w$as_handles()$handle
  
  expect_s4_class(h, "DelayedArray")
  expect_true("stat" %in% names(dimnames(h)))
  expect_equal(dimnames(h)$stat, c("projection", "std_error"))
})

test_that("write_year_slice writes into selected stat slice", {
  out_dim <- c(year = 3, area.name = 1, stat = 2)
  out_dn <- list(
    year = c("1", "2", "3"),
    area.name = "x",
    stat = c("projection", "std_error")
  )
  
  out_dir <- tempfile("proj_writer_")
  dir.create(out_dir, recursive = TRUE)
  w <- ns_fun("tp_projection_hdf5_writer")(out_dim, out_dn, year_k = 1, dir = out_dir)
  h <- w$as_handles()$handle
  
  w$write_year_slice("data", fixed_k_list = list("2" = 1), stat = "projection", values = c(10, 11, 12))
  w$write_year_slice("data", fixed_k_list = list("2" = 1), stat = "std_error", values = c(1, 2, 3))
  
  arr <- as.array(h)
  expect_equal(as.numeric(arr[, 1, 1]), c(10, 11, 12))
  expect_equal(as.numeric(arr[, 1, 2]), c(1, 2, 3))
})

test_that("future-year label helper validates and extends numeric years", {
  expect_equal(ns_fun("make_future_year_labels")(c("2020", "2021"), h = 3), c("2022", "2023", "2024"))
  expect_error(ns_fun("make_future_year_labels")(c("Y1", "Y2"), h = 2), "coercible to integer")
})

test_that("infer_projection_method applies threshold rules", {
  expect_equal(ns_fun("infer_projection_method")(5), "ETS")
  expect_equal(ns_fun("infer_projection_method")(8), "CAGR")
  expect_equal(ns_fun("infer_projection_method")(11), "ARIMA")
  expect_error(ns_fun("infer_projection_method")(4), "At least 5")
})

test_that("infer_projection_method_from_tp uses poparray time dimension", {
  pa <- make_poparray_fixture()
  expect_equal(ns_fun("infer_projection_method_from_tp")(pa), "ETS")
})

test_that("project returns poparray_projection with one-cube handle and source metadata", {
  pa <- make_poparray_fixture()

  pr <- tarr.pop::project(pa, h = 2, method = "CAGR", level = 0.95, guard = FALSE)

  expect_s4_class(pr, "poparray_projection")
  expect_s4_class(pr, "DelayedArray")
  expect_true("stat" %in% names(dimnames(pr)))
  expect_equal(dimnames(pr)$stat, c("projection", "std_error"))

  src <- pr@source
  expect_true(is.list(src))
  expect_true(all(c("note", "source", "updated", "projection_method", "projection_level") %in% names(src)))
})

test_that("project returns an HDF5-backed delayed projection cube", {
  pa <- make_poparray_fixture()

  pr <- tarr.pop::project(pa, h = 2, method = "CAGR", level = 0.95, guard = FALSE)
  handle <- ns_fun("pp_handle")(pr)
  seed <- DelayedArray::seed(handle)

  expect_s4_class(handle, "DelayedArray")
  expect_s4_class(seed, "HDF5ArraySeed")
  expect_true(file.exists(seed@filepath))
  expect_equal(dimnames(pr)$stat, c("projection", "std_error"))
})

test_that("project preserves time and area roles and projected year labels", {
  pa <- make_poparray_fixture()

  pr <- tarr.pop::project(pa, h = 3, method = "CAGR", level = 0.95, guard = FALSE)
  roles <- ns_fun("pp_roles")(pr)

  expect_equal(roles$time, "year")
  expect_equal(roles$area, "area.name")
  expect_equal(dimnames(pr)$year, c("2023", "2024", "2025"))
  expect_equal(dimnames(pr)$area.name, dimnames(pa)$area.name)
  expect_equal(dimnames(pr)$sex, dimnames(pa)$sex)
})

test_that("project output remains delayed until explicitly realized", {
  pa <- make_poparray_fixture()

  pr <- tarr.pop::project(pa, h = 2, method = "CAGR", level = 0.95, guard = FALSE)
  proj <- ns_fun("projection")(pr)
  handle <- ns_fun("pp_handle")(pr)
  seed <- DelayedArray::seed(handle)

  expect_s4_class(proj, "DelayedArray")
  expect_s4_class(seed, "HDF5ArraySeed")

  one_cell <- as.numeric(proj[, 1, 1, 1])
  expect_length(one_cell, 2)
  expect_true(all(is.finite(one_cell)))
})

test_that("project enforces numeric-like time labels for horizon generation", {
  dn <- list(
    year = paste0("Y", 2018:2022),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  pa_bad <- tarr.pop::as.poparray(arr, filepath = tempfile("projection_bad_years_", fileext = ".h5"))

  expect_error(
    tarr.pop::project(pa_bad, h = 2, method = "CAGR", level = 0.95, guard = FALSE),
    "coercible to integer"
  )
})
