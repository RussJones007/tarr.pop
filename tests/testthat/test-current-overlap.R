make_current_overlap_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    age.char = c("80-84", "85+", "85-89", "90-94", "95+")
  )
  arr <- array(
    as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)), dimnames = dn
  )
  pop <- as.poparray(arr, filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(pop)
  sem$age.char <- pa_update_dim_semantics(
    sem$age.char, overlap_levels = "85+", validated = TRUE
  )
  dim_semantics(pop) <- sem
  list(pop = pop, arr = arr)
}

test_that("current overlapping ages retain strict and override behavior", {
  fx <- make_current_overlap_fixture()
  expect_error(sum(fx$pop, strict = TRUE), "Unsafe reduction blocked")
  expect_warning(
    expect_equal(sum(fx$pop, strict = FALSE), sum(fx$arr)),
    "Unsafe reduction blocked"
  )
  expect_silent(expect_equal(sum(fx$pop, allow_overlap = TRUE), sum(fx$arr)))
})

test_that("filtering the broad age category clears current overlap", {
  fx <- make_current_overlap_fixture()
  pop <- dplyr::filter(fx$pop, age.char != "85+")
  expect_identical(dimnames(pop)$age.char, c("80-84", "85-89", "90-94", "95+"))
  expect_equal(dim_semantics(pop), dim_semantics(fx$pop))
  expect_silent(validate_poparray(pop))
  expect_silent(expect_equal(sum(pop, strict = TRUE), sum(fx$arr[, , -2, drop = FALSE])))
})

test_that("keeping the broad age category without details clears overlap", {
  fx <- make_current_overlap_fixture()
  pop <- dplyr::filter(fx$pop, age.char %in% c("80-84", "85+"))
  expect_identical(dimnames(pop)$age.char, c("80-84", "85+"))
  expect_identical(dim_semantics(pop)$age.char@overlap_levels, "85+")
  expect_silent(expect_equal(sum(pop, strict = TRUE), sum(fx$arr[, , 1:2, drop = FALSE])))
})

test_that("partial and unrelated filtering keep genuine overlap unsafe", {
  fx <- make_current_overlap_fixture()
  partial <- dplyr::filter(fx$pop, age.char != "95+")
  expect_error(sum(partial, strict = TRUE), "Unsafe reduction blocked")
  unrelated <- dplyr::filter(fx$pop, year != 2021, area.name != "B")
  expect_identical(dimnames(unrelated)$age.char, dimnames(fx$pop)$age.char)
  expect_error(sum(unrelated, strict = TRUE), "Unsafe reduction blocked")
})

test_that("base subsetting checks current age intervals and preserves metadata", {
  fx <- make_current_overlap_fixture()
  detailed <- fx$pop[age.char = c("80-84", "85-89", "90-94", "95+")]
  broad <- fx$pop[, , c("80-84", "85+"), drop = FALSE]
  partial <- fx$pop[age.char = c("80-84", "85+", "85-89")]
  expect_silent(expect_equal(sum(detailed, strict = TRUE), sum(fx$arr[, , -2, drop = FALSE])))
  expect_silent(expect_equal(sum(broad, strict = TRUE), sum(fx$arr[, , 1:2, drop = FALSE])))
  expect_error(sum(partial, strict = TRUE), "Unsafe reduction blocked")
  expect_equal(dim_semantics(detailed), dim_semantics(fx$pop))
  expect_equal(roles(detailed), roles(fx$pop))
  expect_equal(source_meta(detailed), source_meta(fx$pop))
  expect_identical(data_col(detailed), data_col(fx$pop))
  expect_silent(validate_poparray(detailed))
})

test_that("nominal overlap-causing levels are evaluated against current labels", {
  dn <- list(year = "2020", area.name = "A", race.eth = c("All races", "Asian", "Black", "White"))
  pop <- as.poparray(
    array(as.numeric(1:4), dim = c(1L, 1L, 4L), dimnames = dn),
    filepath = tempfile(fileext = ".h5")
  )
  sem <- dim_semantics(pop)
  sem$race.eth <- pa_update_dim_semantics(sem$race.eth, overlap_levels = "All races", validated = TRUE)
  dim_semantics(pop) <- sem
  expect_error(sum(pop), "Unsafe reduction blocked")
  detailed <- dplyr::filter(pop, race.eth != "All races")
  expect_identical(dim_semantics(detailed)$race.eth@overlap_levels, "All races")
  expect_silent(expect_equal(sum(detailed, strict = TRUE), 9))
  expect_silent(expect_equal(sum(dplyr::filter(pop, race.eth == "All races")), 1))
  expect_error(sum(dplyr::filter(pop, race.eth != "White")), "Unsafe reduction blocked")
})

test_that("collapse and age grouping honor current intervals after filtering", {
  fx <- make_current_overlap_fixture()
  expect_error(group_ages(fx$pop, c("80-84", "85+")), "Unsafe collapse blocked")
  pop <- dplyr::filter(fx$pop, age.char != "85+")
  grouped <- group_ages(pop, c("80-84", "85+"))
  collapsed <- collapse_dim(pop, "age.char", list("80-84" = "80-84", "85+" = c("85-89", "90-94", "95+")))
  expect_silent(expect_equal(sum(grouped, strict = TRUE), sum(pop)))
  expect_equal(as.array(grouped), as.array(collapsed))
  expect_error(
    collapse_dim(fx$pop, "age.char", list("80-84" = "80-84", "85+" = c("85+", "85-89", "90-94", "95+"))),
    "Unsafe collapse blocked"
  )
})

test_that("subsetting and overlap guards do not extract HDF5 population data", {
  fx <- make_current_overlap_fixture()
  tracker <- new.env(parent = emptyenv())
  tracker$cells <- numeric()
  # Instrument the public seed extraction method, rather than delayed-tree internals.
  # Validity may request empty arrays; count values returned, not method calls.
  invisible(suppressMessages(trace(
    "extract_array", signature = "HDF5ArraySeed",
    exit = substitute({
      assign("cells", c(tracker$cells, length(returnValue())), envir = tracker)
    }, list(tracker = tracker)),
    where = asNamespace("HDF5Array"), print = FALSE
  )))
  withr::defer(invisible(suppressMessages(untrace(
    "extract_array", signature = "HDF5ArraySeed", where = asNamespace("HDF5Array")
  ))))

  filtered <- dplyr::filter(fx$pop, age.char != "85+")
  sliced <- fx$pop[age.char = c("80-84", "85+")]
  expect_s4_class(filtered, "DelayedArray")
  expect_true(is_hdf5_backed_delayed(filtered))
  expect_true(is_hdf5_backed_delayed(sliced))
  expect_false(pa_dim_has_overlap_risk(dim_semantics(filtered)$age.char, dimnames(filtered)$age.char))
  expect_false(pa_dim_has_overlap_risk(dim_semantics(sliced)$age.char, dimnames(sliced)$age.char))
  expect_error(sum(fx$pop, strict = TRUE), "Unsafe reduction blocked")
  expect_error(group_ages(fx$pop, c("80-84", "85+")), "Unsafe collapse blocked")
  expect_equal(sum(tracker$cells), 0)

  # A successful reduction must read values; confirm the instrumentation sees it.
  expect_equal(sum(filtered), sum(fx$arr[, , -2, drop = FALSE]))
  expect_gt(sum(tracker$cells), 0)
})
