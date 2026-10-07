make_applicability_fixture <- function(by = "year", declare = TRUE) {
  controlling <- if (by == "year") as.character(2000:2003) else c("winter", "spring", "summer", "autumn")
  dn <- list(year = if (by == "year") controlling else as.character(2000:2001), area.name = c("A", "B"))
  if (by != "year") dn[[by]] <- controlling
  dn$age.char <- c("80-84", "85+", "85-89", "90-94", "95+")
  arr <- array(NA_real_, dim = unname(lengths(dn)), dimnames = dn)
  # Numeric values are deliberately not masked by Phase 2 reductions.
  arr[] <- 1
  filepath <- tempfile("applicability_", fileext = ".h5")
  pop <- as.poparray(arr, filepath = filepath)
  sem <- dim_semantics(pop)
  if (by != "year") sem[[by]] <- pa_update_dim_semantics(
    sem[[by]], scale_type = "ordinal", partition_type = "partition", validated = TRUE
  )
  applicability <- list(by = by, schemas = list(
    list(from = NULL, through = controlling[[2L]], levels = c("80-84", "85+")),
    list(from = controlling[[3L]], through = NULL, levels = c("80-84", "85-89", "90-94", "95+"))
  ))
  if (declare) sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = applicability)
  dim_semantics(pop) <- sem
  list(pop = pop, filepath = filepath, arr = arr, applicability = applicability)
}

replace_test_applicability <- function(pop, applicability) {
  sem <- dim_semantics(pop)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = applicability)
  `dim_semantics<-`(pop, sem)
}

test_that("applicability is optional and structurally validated by S7", {
  sem <- new_dim_semantics("age.char", "age", "interval")
  expect_null(sem@applicability)
  expect_null(DimSemantics(dim_name = "age.char", domain = "age", scale_type = "interval", partition_type = "set", validated = FALSE)@applicability)
  app <- list(by = "year", schemas = list(list(from = NULL, through = NULL, levels = "85+")))
  expect_identical(pa_update_dim_semantics(sem, applicability = app)@applicability, app)
  bad <- list(
    list(by = "", schemas = list()),
    list(by = c("year", "area.name"), schemas = list()),
    list(by = "year", schemas = "bad"),
    list(by = "year", schemas = list(list(levels = c("85+", "85+")))),
    list(by = "year", schemas = list(list(levels = NA_character_))),
    list(by = "year", schemas = list(list(levels = 85))),
    list(by = "year", schemas = list(list(from = c("a", "b"), levels = "85+"))),
    list(by = "year", schemas = list(list(levels = "85+", expression = "TRUE")))
  )
  for (value in bad) expect_error(pa_update_dim_semantics(sem, applicability = value), "applicability")
})

test_that("context validity rejects unknown dimensions, levels, and boundaries", {
  fx <- make_applicability_fixture()
  bad <- fx$applicability
  bad$by <- "missing"
  expect_error(replace_test_applicability(fx$pop, bad), "existing dimension")
  bad$by <- "age.char"
  expect_error(replace_test_applicability(fx$pop, bad), "another existing")
  bad <- fx$applicability
  bad$schemas[[1]]$levels <- "unknown"
  expect_error(replace_test_applicability(fx$pop, bad), "unknown target levels")
  bad <- fx$applicability
  bad$schemas[[1]]$through <- "1999"
  expect_error(replace_test_applicability(fx$pop, bad), "unknown boundary")
  bad <- fx$applicability
  bad$schemas[[2]]$from <- "2003"
  bad$schemas[[2]]$through <- "2002"
  expect_error(replace_test_applicability(fx$pop, bad), "reversed")
})

test_that("current controlling labels must have exactly one schema", {
  fx <- make_applicability_fixture()
  bad <- fx$applicability
  bad$schemas[[2]]$from <- "2001"
  expect_error(replace_test_applicability(fx$pop, bad), "ranges.*overlap")
  bad <- fx$applicability
  bad$schemas[[2]]$from <- "2003"
  expect_error(replace_test_applicability(fx$pop, bad), "uncovered")
  bad$schemas <- list()
  expect_error(replace_test_applicability(fx$pop, bad), "uncovered")

  # Exercise both construction and S4 validity, not just the replacement path.
  sem <- dim_semantics(fx$pop)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = bad)
  expect_error(new_poparray(methods::as(fx$pop, "DelayedArray"), dimnames_list = dimnames(fx$pop), dim_semantics = sem), "uncovered")
  invalid <- fx$pop
  invalid@dim_semantics <- sem
  expect_match(methods::validObject(invalid, test = TRUE), "uncovered")
  expect_error(validate_poparray(invalid), "uncovered")
  expect_error(sum(invalid), "uncovered")
})

test_that("mutually exclusive interval schemas clear union overlap only", {
  fx <- make_applicability_fixture()
  expect_true(pa_has_interval_overlap(dimnames(fx$pop)$age.char))
  expect_silent(validate_poparray(fx$pop))
  expect_false(pa_dim_has_overlap_risk(dim_semantics(fx$pop)$age.char, dimnames(fx$pop)$age.char, dimnames(fx$pop)))
  expect_silent(expect_equal(sum(fx$pop, strict = TRUE), sum(fx$arr)))
  legacy <- replace_test_applicability(fx$pop, NULL)
  expect_error(sum(legacy, strict = TRUE), "Unsafe reduction blocked")

  app <- fx$applicability
  app$schemas[[1]]$levels <- c("80-84", "85+", "85-89")
  unsafe <- replace_test_applicability(fx$pop, app)
  expect_silent(validate_poparray(unsafe))
  expect_error(sum(unsafe, strict = TRUE), "Unsafe reduction blocked")
  expect_warning(sum(unsafe, strict = FALSE), "Unsafe reduction blocked")
  expect_silent(sum(unsafe, allow_overlap = TRUE))
  expect_error(pa_check_collapse_semantics(unsafe, "age.char", dimnames(unsafe)$age.char, "group", rep(1L, 5L), TRUE, FALSE), "Unsafe collapse blocked")
  expect_silent(pa_check_collapse_semantics(fx$pop, "age.char", dimnames(fx$pop)$age.char, "group", rep(1L, 5L), TRUE, FALSE))
})

test_that("applicability changes safety but never masks population or genuine NA", {
  fx <- make_applicability_fixture()
  values <- fx$arr
  values[1, 1, 1] <- NA_real_
  pop <- as.poparray(values, filepath = tempfile(fileext = ".h5"))
  pop <- replace_test_applicability(pop, fx$applicability)
  expect_silent(expect_true(is.na(sum(pop))))
  expect_silent(expect_equal(sum(pop, na.rm = TRUE), sum(values, na.rm = TRUE)))
})

test_that("controlling-dimension filtering trims and validates schemas", {
  fx <- make_applicability_fixture()
  early <- dplyr::filter(fx$pop, year <= 2001)
  late <- dplyr::filter(fx$pop, year >= 2002)
  both <- dplyr::filter(fx$pop, year %in% c(2001, 2002))
  for (pop in list(early, late, both)) {
    expect_silent(validate_poparray(pop))
    expect_silent(sum(pop, strict = TRUE))
  }
  expect_length(dim_semantics(early)$age.char@applicability$schemas, 1L)
  expect_identical(dim_semantics(early)$age.char@applicability$schemas[[1]]$levels, c("80-84", "85+"))
  expect_length(dim_semantics(late)$age.char@applicability$schemas, 1L)
  expect_identical(dim_semantics(late)$age.char@applicability$schemas[[1]]$levels, c("80-84", "85-89", "90-94", "95+"))
  expect_length(dim_semantics(both)$age.char@applicability$schemas, 2L)
  expect_identical(dim_semantics(both)$age.char@applicability$schemas[[1]]$through, "2001")
  expect_identical(dim_semantics(both)$age.char@applicability$schemas[[2]]$from, "2002")
})

test_that("target filtering intersects schema levels and preserves empty schemas", {
  fx <- make_applicability_fixture()
  pop <- dplyr::filter(fx$pop, age.char != "85+")
  expect_identical(dimnames(pop)$age.char, c("80-84", "85-89", "90-94", "95+"))
  expect_identical(dim_semantics(pop)$age.char@applicability$schemas[[1]]$levels, "80-84")
  expect_silent(validate_poparray(pop))
  expect_silent(sum(pop, strict = TRUE))
  empty_early <- fx$pop[age.char = c("85-89", "90-94")]
  expect_identical(dim_semantics(empty_early)$age.char@applicability$schemas[[1]]$levels, character())
  expect_silent(validate_poparray(empty_early))
  expect_silent(sum(empty_early, strict = TRUE))
})

test_that("unrelated filtering preserves applicability exactly", {
  fx <- make_applicability_fixture()
  pop <- dplyr::filter(fx$pop, area.name == "A")
  expect_identical(dim_semantics(pop)$age.char@applicability, fx$applicability)
  expect_silent(validate_poparray(pop))
})

test_that("boundaries use label positions, including nonnumeric and reordered labels", {
  fx <- make_applicability_fixture(by = "period")
  expect_silent(validate_poparray(fx$pop))
  expect_silent(sum(fx$pop))
  pop <- fx$pop[period = c("spring", "summer")]
  expect_length(dim_semantics(pop)$age.char@applicability$schemas, 2L)
  expect_silent(validate_poparray(pop))
  # Alternating contexts cannot be represented as one range per original schema.
  interleaved <- fx$pop[period = c("winter", "summer", "spring", "autumn")]
  expect_length(dim_semantics(interleaved)$age.char@applicability$schemas, 4L)
  expect_silent(validate_poparray(interleaved))
  expect_silent(sum(interleaved))
  selected <- dplyr::filter(fx$pop, period == "summer")
  expect_length(dim_semantics(selected)$age.char@applicability$schemas, 1L)
  expect_silent(validate_poparray(selected))
})

test_that("dropping an applicability controller returns the delayed backend", {
  fx <- make_applicability_fixture(by = "period")
  pop <- fx$pop[period = "winter", drop = TRUE]
  expect_s4_class(pop, "DelayedArray")
  expect_false(is.poparray(pop))
  expect_true(is_hdf5_backed_delayed(pop))
  # Both role dimensions remain, so this exercises the controller-specific fallback.
  expect_identical(dim(pop), c(2L, 2L, 5L))
})

test_that("nominal schemas retain conservative safety and intrinsic overlap descriptors", {
  dn <- list(year = as.character(2000:2003), area.name = "A", category = c("Combined", "One", "Two"))
  pop <- as.poparray(array(as.numeric(1:12), dim = c(4L, 1L, 3L), dimnames = dn), filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(pop)
  sem$category <- pa_update_dim_semantics(sem$category, overlap_levels = "Combined", applicability = list(by = "year", schemas = list(
    list(from = NULL, through = "2001", levels = "Combined"),
    list(from = "2002", through = NULL, levels = c("One", "Two"))
  )))
  dim_semantics(pop) <- sem
  expect_silent(sum(pop))
  expect_identical(dim_semantics(pop)$category@overlap_levels, "Combined")
  sem$category <- pa_update_dim_semantics(sem$category, overlap_levels = character())
  dim_semantics(pop) <- sem
  expect_error(sum(pop), "Unsafe reduction blocked")
})

test_that("HDF5 save and registered open preserve applicability", {
  fx <- make_applicability_fixture()
  filepath <- tempfile(fileext = ".h5")
  save_poparray(fx$pop, filepath = filepath, series_id = "applicability_test")
  testthat::local_mocked_bindings(tarr_series_registry = function(root = resolve_cube_dir()) {
    data.frame(series_id = "applicability_test", filename = basename(filepath), filepath = filepath)
  }, .package = "tarr.pop")
  withr::local_options(list(tarr.pop.cube_path = tempdir()))
  reopened <- open_poparray("applicability_test")
  expect_identical(dim_semantics(reopened)$age.char@applicability, fx$applicability)
  expect_equal(dimnames(reopened), dimnames(fx$pop))
  expect_silent(validate_poparray(reopened))
  expect_silent(sum(reopened))
  expect_equal(as.array(reopened), fx$arr)
  expect_length(dim_semantics(dplyr::filter(reopened, year <= 2001))$age.char@applicability$schemas, 1L)
})

test_that("legacy HDF5 without applicability retains previous behavior", {
  fx <- make_applicability_fixture(declare = FALSE)
  info <- rhdf5::h5ls(fx$filepath)
  expect_false(any(info$name == "applicability"))
  expect_null(dim_semantics(fx$filepath)$age.char@applicability)
  testthat::local_mocked_bindings(tarr_series_registry = function(root = resolve_cube_dir()) {
    data.frame(series_id = "legacy_test", filename = basename(fx$filepath), filepath = fx$filepath)
  }, .package = "tarr.pop")
  withr::local_options(list(tarr.pop.cube_path = tempdir()))
  pop <- open_poparray("legacy_test")
  expect_null(dim_semantics(pop)$age.char@applicability)
  expect_error(sum(pop), "Unsafe reduction blocked")
  expect_silent(sum(dplyr::filter(pop, age.char != "85+")))
})

test_that("metadata replacement validates before writing and leaves values unchanged", {
  fx <- make_applicability_fixture()
  withr::local_options(list(tarr.pop.metadata_role = "admin"))
  sem <- dim_semantics(fx$pop)
  expect_silent(`dim_semantics<-`(fx$filepath, sem))
  expect_identical(dim_semantics(fx$filepath)$age.char@applicability, fx$applicability)
  bad <- sem
  app <- fx$applicability
  app$schemas[[2]]$from <- "missing"
  bad$age.char <- pa_update_dim_semantics(bad$age.char, applicability = app)
  expect_error(`dim_semantics<-`(fx$filepath, bad), "unknown boundary")
  expect_identical(dim_semantics(fx$filepath)$age.char@applicability, fx$applicability)
  expect_equal(rhdf5::h5read(fx$filepath, "cube/population"), unname(fx$arr), ignore_attr = TRUE)
  bundle <- cube_metadata(fx$filepath)
  bundle$dim_semantics$age.char <- pa_update_dim_semantics(bundle$dim_semantics$age.char, applicability = NULL)
  expect_silent(`cube_metadata<-`(fx$filepath, bundle))
  expect_null(dim_semantics(fx$filepath)$age.char@applicability)
})

test_that("metadata and applicability operations never extract or rewrite population", {
  fx <- make_applicability_fixture()
  tracker <- new.env(parent = emptyenv()); tracker$cells <- numeric()
  invisible(suppressMessages(trace("extract_array", signature = "HDF5ArraySeed", exit = substitute({
    assign("cells", c(tracker$cells, length(returnValue())), envir = tracker)
  }, list(tracker = tracker)), where = asNamespace("HDF5Array"), print = FALSE)))
  withr::defer(invisible(suppressMessages(untrace("extract_array", signature = "HDF5ArraySeed", where = asNamespace("HDF5Array")))))
  testthat::local_mocked_bindings(pa_write_poparray_cube = function(...) stop("Unexpected population write"), .package = "tarr.pop")
  withr::local_options(list(tarr.pop.metadata_role = "admin"))
  expect_identical(dim_semantics(fx$pop)$age.char@applicability, fx$applicability)
  expect_silent(validate_poparray(fx$pop))
  expect_false(pa_dim_has_overlap_risk(dim_semantics(fx$pop)$age.char, dimnames(fx$pop)$age.char, dimnames(fx$pop)))
  filtered <- dplyr::filter(fx$pop, year <= 2001, age.char != "85+")
  sliced <- fx$pop[year = "2002", age.char = c("85-89", "90-94")]
  expect_silent(validate_poparray(filtered))
  expect_silent(validate_poparray(sliced))
  expect_true(is_hdf5_backed_delayed(filtered))
  expect_true(is_hdf5_backed_delayed(sliced))
  expect_silent(`dim_semantics<-`(fx$filepath, dim_semantics(fx$pop)))
  expect_identical(dim_semantics(fx$filepath)$age.char@applicability, fx$applicability)
  bundle <- cube_metadata(fx$filepath)
  expect_silent(`cube_metadata<-`(fx$filepath, bundle))
  expect_error(collapse_dim(fx$pop, "year", list("period" = dimnames(fx$pop)$year)), "applicability controller")
  expect_equal(sum(tracker$cells), 0)
})

test_that("declared applicability partition schemas enforce interval validity", {
  fx <- make_applicability_fixture()
  sem <- dim_semantics(fx$pop)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, partition_type = "partition")
  expect_silent(pop <- `dim_semantics<-`(fx$pop, sem))
  expect_silent(sum(pop))
  app <- fx$applicability
  app$schemas[[1]]$levels <- c("85+", "85-89")
  expect_error(replace_test_applicability(pop, app), "partition schema")
})

test_that("empty subsets remain valid without losing declared applicability", {
  fx <- make_applicability_fixture()
  no_periods <- fx$pop[year = character()]
  expect_identical(dim_semantics(no_periods)$age.char@applicability$schemas, list())
  expect_silent(validate_poparray(no_periods))
  expect_silent(expect_equal(sum(no_periods), 0))
  no_levels <- fx$pop[age.char = character()]
  expect_true(all(lengths(lapply(dim_semantics(no_levels)$age.char@applicability$schemas, `[[`, "levels")) == 0L))
  expect_silent(validate_poparray(no_levels))
  expect_silent(expect_equal(sum(no_levels), 0))
})

test_that("identity collapse trims applicability and updates renamed controllers", {
  fx <- make_applicability_fixture()
  labels <- dimnames(fx$pop)$year
  renamed <- collapse_dim(fx$pop, "year", stats::setNames(labels, labels), name = "time")
  expect_identical(dim_semantics(renamed)$age.char@applicability$by, "time")
  expect_silent(validate_poparray(renamed))
  expect_silent(sum(renamed))
  levels <- dimnames(fx$pop)$age.char
  expect_warning(identity <- collapse_dim(fx$pop, "age.char", stats::setNames(levels, levels)), "deriv")
  expect_identical(dim_semantics(identity)$age.char@applicability, fx$applicability)
  expect_silent(validate_poparray(identity))
})

test_that("subsetting discards unsafe schemas outside the current context", {
  fx <- make_applicability_fixture()
  app <- fx$applicability
  app$schemas[[1]]$levels <- c("85+", "85-89")
  unsafe <- replace_test_applicability(fx$pop, app)
  expect_error(sum(unsafe), "Unsafe reduction blocked")
  expect_silent(sum(dplyr::filter(unsafe, year >= 2002)))
  expect_error(sum(dplyr::filter(unsafe, year <= 2001)), "Unsafe reduction blocked")
  expect_silent(sum(dplyr::filter(unsafe, age.char != "85+")))
})
