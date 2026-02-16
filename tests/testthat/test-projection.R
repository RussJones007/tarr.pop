ns_fun <- function(name) getFromNamespace(name, "tarr.pop")

make_poparray_fixture <- function() {
  dn <- list(
    year = as.character(2018:2022),
    area.name = c("A", "B"),
    sex = c("Female", "Male")
  )
  arr <- array(
    as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  
  pa <- ns_fun("new_poparray")(
    x = DelayedArray::DelayedArray(arr),
    dimnames_list = dn,
    data_col = "population",
    source = list(
      note = "Fixture source",
      source = "fixture://source",
      updated = "2026-02-16"
    )
  )
  pa
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
  
  expect_s3_class(pr, "poparray_projection")
  expect_s4_class(pr$handle, "DelayedArray")
  expect_true("stat" %in% names(dimnames(pr$handle)))
  expect_equal(dimnames(pr$handle)$stat, c("projection", "std_error"))
  
  src <- attr(pr, "source", exact = TRUE)
  expect_true(is.list(src))
  expect_true(all(c("note", "source", "updated", "projected_from") %in% names(src)))
  expect_true(is.list(src$projected_from))
})

test_that("project enforces named dimensions defensively", {
  pa <- make_poparray_fixture()
  pa_bad <- pa
  names(pa_bad$dimn) <- NULL
  
  expect_error(
    ns_fun("project_cube")(pa_bad, h = 2, level = 0.95, method = "CAGR", guard = FALSE),
    "no valid dimn dimnames metadata"
  )
})
