make_demographic_fixture <- function(projection = FALSE) {
  dn <- list(year = c("2030", "2031"), area.name = c("A", "B"),
    race = c("White", "Black", "Total"))
  if (projection) dn$stat <- c("projection", "std_error")
  backend <- HDF5Array::writeHDF5Array(array(as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)), dimnames = dn), filepath = tempfile(fileext = ".h5"))
  dimnames(backend) <- dn
  sem <- default_dim_semantics(setdiff(names(dn), "stat"), "year", "area.name")
  sem$race <- pa_update_dim_semantics(sem$race, overlap_levels = "Total",
    applicability = list(by = "year", schemas = list(
      list(from = NULL, through = NULL, levels = dn$race))))
  if (projection) return(new_poparray_projection_s4(backend, 0.95, "ETS",
    list(note = "Fixture", source = "example", updated = "2026-10-31"),
    c("2028", "2029"), "year", "area.name", dim_semantics = sem))
  new_poparray(backend, dim_semantics = sem)
}

test_that("demographic_array is virtual and concrete classes are siblings", {
  expect_true(methods::isVirtualClass("demographic_array"))
  expect_error(methods::new("demographic_array"), "virtual")
  for (projected in c(FALSE, TRUE)) {
    x <- make_demographic_fixture(projected)
    expect_s4_class(x, "demographic_array")
    expect_s4_class(x, "DelayedArray")
    expect_true(methods::validObject(x))
    expect_identical(is(x, "poparray"), !projected)
    expect_identical(is(x, "poparray_projection"), projected)
    bad <- x
    bad@area_role <- bad@time_role
    expect_match(paste(methods::validObject(bad, test = TRUE), collapse = " "), "different")
    bad <- x
    bad@dim_semantics <- rev(bad@dim_semantics)
    expect_match(paste(methods::validObject(bad, test = TRUE), collapse = " "), "same order")
  }
})

test_that("shared accessors and lazy verbs preserve concrete classes and metadata", {
  for (projected in c(FALSE, TRUE)) {
    x <- make_demographic_fixture(projected)
    original <- list(dimnames = dimnames(x), semantics = dim_semantics(x),
      source = x@source, seed = DelayedArray::seed(x))
    source_path <- DelayedArray::seed(x)@filepath
    original_file <- tools::md5sum(source_path)
    # No structural operation below may extract an HDF5 payload.
    testthat::local_mocked_bindings(extract_array = function(...) stop("Unexpected payload read"),
      .package = "HDF5Array")
    expect_identical(time_role(x), "year")
    expect_identical(area_role(x), "area.name")
    expect_identical(get_source(x), x@source)
    expect_identical(data_col(x), x@data_col)
    out <- dplyr::filter(x, race != "Total", year == 2031)
    out <- dplyr::rename(out, period = year, ethnicity = race)
    out <- dplyr::relocate(out, ethnicity, .before = period)
    expect_identical(class(out), class(x))
    expect_identical(time_role(out), "period")
    expect_identical(names(dimnames(out))[1:2], c("ethnicity", "period"))
    expect_identical(dim_semantics(out)$ethnicity@dim_name, "ethnicity")
    expect_identical(dim_semantics(out)$ethnicity@overlap_levels, character())
    expect_identical(dim_semantics(out)$ethnicity@partition_type, "partition")
    app <- dim_semantics(out)$ethnicity@applicability
    expect_identical(app$by, "period")
    expect_identical(app$schemas[[1]]$levels, c("White", "Black"))
    expect_true(validObject(out))
    expect_true(is_hdf5_backed_delayed(out))
    expect_identical(dimnames(x), original$dimnames)
    expect_identical(dim_semantics(x), original$semantics)
    expect_identical(x@source, original$source)
    expect_identical(DelayedArray::seed(x), original$seed)
    expect_identical(tools::md5sum(source_path), original_file)
    sliced <- dplyr::slice_head(out, n = 1, .dimension = "ethnicity")
    expect_identical(class(sliced), class(x))
    expect_identical(dimnames(sliced)$ethnicity, "White")
    named <- dplyr::rename_with(x, toupper, .cols = c(year, race))
    expect_identical(time_role(named), "YEAR")
    expect_identical(class(named), class(x))
    data_col(out) <- "count"
    expect_identical(data_col(out), "count")
    sem <- dim_semantics(out)
    sem$ethnicity <- pa_update_dim_semantics(sem$ethnicity, notes = "Reviewed")
    dim_semantics(out) <- sem
    expect_identical(dim_semantics(out)$ethnicity@notes, "Reviewed")
    if (projected) {
      for (field in c("level", "method", "base_years", "created", "source")) {
        expect_identical(methods::slot(out, field), methods::slot(x, field))
      }
    }
  }
})

test_that("special axes and numerical boundaries remain projection-specific", {
  x <- make_demographic_fixture(TRUE)
  out <- dplyr::select(x, race, county = area.name, stat, period = year)
  expect_s4_class(out, "poparray_projection")
  expect_identical(area_role(out), "county")
  expect_identical(out@strata_roles, "race")
  expect_error(dplyr::select(x, -race), "subclass-specific reduction")
  expect_error(dplyr::select(x, -stat), "subclass-specific reduction")
  expect_error(dplyr::rename(x, measure = stat), "statistical axes")
  errors <- x[stat = "std_error", drop = FALSE]
  expect_s4_class(errors, "poparray_projection")
  expect_identical(dim_semantics(errors)$stat@overlap_levels, "std_error")
  expect_identical(dim_semantics(errors)$stat@partition_type, "set")
  dropped <- x[stat = "std_error", drop = TRUE]
  expect_s4_class(dropped, "DelayedArray")
  expect_false(is(dropped, "demographic_array"))
  counts <- x[stat = "projection", drop = TRUE]
  expect_s4_class(counts, "poparray")
  expect_identical(attr(counts, "projection_method"), x@method)
  expect_equal(as.numeric(counts), as.numeric(projection(x)))
  expect_equal(as.numeric(as.poparray(x)), as.numeric(projection(x)))
  for (fn in list(sum, mean, min, max, range, var, sd, summary)) {
    expect_error(fn(x), "Extract a statistic")
  }
  expect_error(save_poparray(x, filepath = tempfile(fileext = ".h5")), "poparray")
  sem <- dim_semantics(x)
  sem$stat <- pa_update_dim_semantics(sem$stat, overlap_levels = character(), partition_type = "partition")
  expect_error(`dim_semantics<-`(x, sem), "non-additive")
  expect_identical(dim_semantics(x)$stat@partition_type, "set")
})

test_that("projection structural verbs do not extract HDF5 payloads", {
  x <- make_demographic_fixture(TRUE)
  tracker <- new.env(parent = emptyenv())
  tracker$cells <- numeric()
  invisible(suppressMessages(trace("extract_array", signature = "HDF5ArraySeed",
    exit = substitute({
      assign("cells", c(tracker$cells, length(returnValue())), envir = tracker)
    }, list(tracker = tracker)), where = asNamespace("HDF5Array"), print = FALSE)))
  withr::defer(invisible(suppressMessages(untrace("extract_array",
    signature = "HDF5ArraySeed", where = asNamespace("HDF5Array")))))
  out <- dplyr::filter(x, race != "Total", year == 2031)
  out <- dplyr::rename(out, period = year)
  out <- dplyr::relocate(out, race)
  out <- dplyr::select(out, race, stat, area.name, period)
  out <- dplyr::slice_tail(out, n = 1, .dimension = "area.name")
  expect_true(validObject(out))
  expect_identical(out@base_years, x@base_years)
  expect_equal(sum(tracker$cells), 0)
  # Confirm the instrumentation observes a real read after the lazy operations.
  expect_gt(length(as.numeric(std_error(out))), 0)
  expect_gt(sum(tracker$cells), 0)
})

test_that("canonical metadata access shares structure without losing projection provenance", {
  for (projected in c(FALSE, TRUE)) {
    x <- make_demographic_fixture(projected)
    expect_identical(roles(x)$strata, "race")
    expect_identical(cube_metadata(x)$dim_semantics, dim_semantics(x))
    expect_identical(source_meta(x)$note, if (projected) "Fixture" else "")
    roles(x) <- roles(x)
    expect_true(validObject(x))
    if (projected) x@source$fit_details <- list(parameter = "retained")
    source_meta(x) <- list(note = "Edited source", source = "example", updated = "2026-11-01")
    expect_identical(source_meta(x)$note, "Edited source")
    metadata <- cube_metadata(x)
    metadata$data_col <- "estimate"
    cube_metadata(x) <- metadata
    expect_identical(data_col(x), "estimate")
    expect_true(validObject(x))
    if (projected) {
      expect_identical(get_source(x)$fit_details, list(parameter = "retained"))
      expect_identical(x@method, "ETS")
      expect_error(`roles<-`(x, list(time = "stat", area = "area.name")), "dimension names")
    }
  }
})


test_that("shared structure does not impose count storage on in-memory projections", {
  template <- make_demographic_fixture(TRUE)
  backend <- DelayedArray::DelayedArray(as.array(template))
  x <- new_poparray_projection_s4(backend, template@level, template@method,
    template@source, template@base_years, "year", "area.name",
    dim_semantics = dim_semantics(template))
  expect_true(validObject(x))
  expect_false(is_hdf5_backed_delayed(x))
  selected <- dplyr::filter(x, race != "Total")
  expect_s4_class(selected, "poparray_projection")
  expect_false(is_hdf5_backed_delayed(selected))
  dropped <- selected[stat = "projection", drop = TRUE]
  expect_s4_class(dropped, "DelayedArray")
  expect_false(is(dropped, "poparray"))
  expect_error(new_poparray(backend, dim_semantics = dim_semantics(template)), "HDF5Array seed")
})


test_that("shared entry points retain count-specific ordered-time validation", {
  x <- make_demographic_fixture()
  reordered <- x[year = c("2031", "2030")]
  expect_true(validObject(reordered))
  expect_error(time_role(reordered), "ordered")
  expect_error(reordered[year = "2030"], "ordered")
  expect_error(dplyr::rename(reordered, period = year), "ordered")
})
