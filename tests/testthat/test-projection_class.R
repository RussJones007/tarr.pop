ns_fun <- function(name) getFromNamespace(name, "tarr.pop")

make_projection_fixture <- function() {
  dn <- list(
    year = as.character(2030:2032),
    area.name = c("A", "B"),
    sex = c("Female", "Male"),
    stat = c("projection", "std_error")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  fp <- tempfile("projection_fixture_", fileext = ".h5")
  HDF5Array::writeHDF5Array(arr, filepath = fp, name = "proj")
  h <- HDF5Array::HDF5Array(filepath = fp, name = "proj")
  dimnames(h) <- dn

  ns_fun("new_poparray_projection")(
    handle = h,
    level = 0.95,
    method = "ETS",
    source = list(
      note = "Projection from example source",
      source = "example://source",
      updated = "2026-02-16",
      projected_from = list(
        note = "Base source",
        source = "example://base",
        updated = "2026-02-15"
      )
    ),
    base_years = as.character(2025:2029),
    dimroles = list(time = "year", area = "area.name", strata = "sex"),
    data_col = "population",
    dim_semantics = ns_fun("default_dim_semantics")(setdiff(names(dn), "stat"), "year", "area.name"),
    created = as.POSIXct("2026-02-16 00:00:00", tz = "UTC")
  )
}

test_that("constructor and validator support one-cube handle design", {
  pr <- make_projection_fixture()

  expect_s4_class(pr, "poparray_projection")
  expect_s4_class(pr, "DelayedArray")
  expect_true("stat" %in% names(dimnames(pr)))
  expect_equal(dimnames(pr)$stat, c("projection", "std_error"))
  expect_true(ns_fun("validate_poparray_projection")(pr))
})

test_that("projection() and std_error() are position-agnostic and lazy", {
  pr <- make_projection_fixture()
  
  proj <- ns_fun("projection")(pr)
  se <- ns_fun("std_error")(pr)
  
  expect_s4_class(proj, "DelayedArray")
  expect_s4_class(se, "DelayedArray")
  
  expect_equal(dim(proj), c(3, 2, 2, 1))
  expect_equal(dim(se), c(3, 2, 2, 1))
  expect_equal(dimnames(proj)$stat, "projection")
  expect_equal(dimnames(se)$stat, "std_error")
})

test_that("confint returns delayed lower and upper arrays", {
  pr <- make_projection_fixture()
  ci <- stats::confint(pr)
  
  expect_named(ci, c("lower", "upper"))
  expect_s4_class(ci$lower, "DelayedArray")
  expect_s4_class(ci$upper, "DelayedArray")
  expect_equal(dim(ci$lower), dim(ns_fun("projection")(pr)))
  expect_equal(dim(ci$upper), dim(ns_fun("projection")(pr)))
})

test_that("subsetting keeps projection class when stat remains", {
  pr <- make_projection_fixture()

  y <- pr[year = "2030", drop = FALSE]
  expect_s4_class(y, "poparray_projection")
  expect_true("stat" %in% names(dimnames(y)))
  expect_equal(dimnames(y)$stat, c("projection", "std_error"))
})

test_that("subsetting can return poparray when stat is removed", {
  pr <- make_projection_fixture()

  y <- pr[stat = "projection", drop = TRUE]
  expect_s4_class(y, "poparray")
  expect_false("stat" %in% names(dimnames(y)))
})

test_that("as.poparray selects estimates and removes only stat lazily", {
  pr <- make_projection_fixture()
  original <- serialize(pr, NULL)
  pa <- tarr.pop::as.poparray(pr)

  expect_s4_class(pa, "poparray")
  expect_false(is(pa, "poparray_projection"))
  expect_false("stat" %in% names(dimnames(pa)))
  expect_identical(dimnames(pa), dimnames(pr)[names(dimnames(pr)) != "stat"])
  expect_false("stat" %in% methods::slot(pa, "strata_roles"))
  expect_identical(names(dim_semantics(pa)), names(dimnames(pa)))
  expect_true(ns_fun("pa_validate_applicability")(dim_semantics(pa), dimnames(pa)))
  expect_true(all(vapply(dim_semantics(pa), function(sem) {
    app <- ns_fun("pa_dim_applicability")(sem)
    is.null(app) || app$by %in% names(dimnames(pa))
  }, logical(1))))
  expect_true(validObject(pa))
  expect_equal(pa@source, pr@source)
  expect_equal(data_col(pa), pr@data_col)
  expect_equal(attr(pa, "projection_method"), pr@method)
  expect_equal(attr(pa, "projection_level"), pr@level)
  expect_equal(attr(pa, "projection_base_years"), pr@base_years)
  expect_identical(DelayedArray::seed(pa), DelayedArray::seed(pr))
  expect_equal(as.numeric(pa), as.numeric(ns_fun("projection")(pr)))
  expect_equal(time_role(pa), "year")
  expect_equal(area_role(pa), "area.name")
  expect_identical(serialize(pr, NULL), original)
})

test_that("tabular conversions include projection/std_error and keep attributes", {
  pr <- make_projection_fixture()
  
  df <- base::as.data.frame(pr)
  expect_true(all(c("projection", "std_error") %in% names(df)))
  expect_true(is.list(attr(df, "source", exact = TRUE)))
  expect_equal(attr(df, "method", exact = TRUE), "ETS")
  expect_equal(attr(df, "level", exact = TRUE), 0.95)
  
  tb <- tibble::as_tibble(pr)
  expect_s3_class(tb, "tbl_df")
  expect_true(all(c("projection", "std_error") %in% names(tb)))
  expect_true(is.list(attr(tb, "source", exact = TRUE)))
  expect_equal(attr(tb, "method", exact = TRUE), "ETS")
  expect_equal(attr(tb, "level", exact = TRUE), 0.95)
})

test_that("tabular conversions optionally include confidence limits", {
  pr <- make_projection_fixture()
  
  df0 <- base::as.data.frame(pr, include_confidence = FALSE)
  expect_false(any(c("lower", "upper") %in% names(df0)))
  
  df1 <- base::as.data.frame(pr, include_confidence = TRUE)
  expect_true(all(c("lower", "upper") %in% names(df1)))
  expect_true(all(df1$lower <= df1$projection))
  expect_true(all(df1$upper >= df1$projection))
  
  tb1 <- tibble::as_tibble(pr, include_confidence = TRUE)
  expect_true(all(c("lower", "upper") %in% names(tb1)))
})

test_that("projection tabular coercion warns before large realization", {
  pr <- make_projection_fixture()

  expect_warning(
    base::as.data.frame(pr, bytes_threshold = 1),
    "EAGER"
  )
})


test_that("coercion preserves singleton dimensions with stat in the middle", {
  pr <- make_projection_fixture()
  h <- ns_fun("pp_handle")(pr)[, "A", "Female", , drop = FALSE]
  h <- DelayedArray::aperm(h, c(3, 4, 1, 2))
  pr <- ns_fun("new_poparray_projection")(
    h, level = pr@level, method = pr@method, source = pr@source,
    base_years = pr@base_years,
    dimroles = list(time = "year", area = "area.name", strata = "sex"),
    dim_semantics = subset_dim_semantics(dim_semantics(pr), dimnames(pr), dimnames(h))
  )
  # Fail if coercion attempts to read any HDF5 payload.
  testthat::local_mocked_bindings(
    extract_array = function(...) stop("unexpected realization"),
    .package = "HDF5Array"
  )
  pa <- as.poparray(pr)
  expect_identical(dimnames(pa), dimnames(pr)[c("sex", "year", "area.name")])
  expect_equal(dim(pa), c(1L, 3L, 1L))
  expect_s4_class(DelayedArray::seed(pa), "HDF5ArraySeed")
  expect_true(validObject(pa))
})

test_that("coercion preserves singleton time and rejects uncertainty-only input", {
  pr <- make_projection_fixture()[year = "2030", drop = FALSE]
  pa <- as.poparray(pr)
  expect_identical(dimnames(pa), dimnames(pr)[c("year", "area.name", "sex")])
  expect_equal(dim(pa), c(1L, 2L, 2L))
  expect_equal(as.numeric(pa), as.numeric(ns_fun("projection")(pr)))
  expect_error(as.poparray(pr[stat = "std_error", drop = FALSE]), "projection.*level")
})
