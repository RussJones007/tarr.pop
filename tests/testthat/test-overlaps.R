make_overlap_report_fixture <- function(age_labels = c("0-4", "5-9")) {
  dn <- list(year = c("2020", "2021"), area.name = "A",
    race = c("White", "Black", "White in combination"), age.char = age_labels)
  x <- as.poparray(array(1, dim = unname(lengths(dn)), dimnames = dn),
    filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(x)
  sem$race <- pa_update_dim_semantics(sem$race,
    overlap_levels = "White in combination", validated = TRUE)
  dim_semantics(x) <- sem
  x
}

test_that("overlaps reports original declarations and current status after filtering", {
  x <- make_overlap_report_fixture()
  before <- dim_semantics(x)
  result <- overlaps(x)
  expect_named(result, names(dimnames(x)))
  expect_equal(result$race, list(original_levels = "White in combination",
    current_levels = "White in combination", has_overlap = TRUE))
  expect_equal(result$area.name, list(original_levels = character(),
    current_levels = character(), has_overlap = FALSE))
  clean <- drop_overlap_levels(x, "race")
  expect_equal(overlaps(clean)$race, list(original_levels = "White in combination",
    current_levels = character(), has_overlap = FALSE))
  single <- x[, , "White in combination", , drop = FALSE]
  expect_equal(overlaps(single)$race$current_levels, "White in combination")
  expect_false(overlaps(single)$race$has_overlap)
  expect_equal(dim_semantics(x), before)
  expect_true(is_hdf5_backed_delayed(x))
  expect_error(overlaps(list()))
})

test_that("overlaps derives interval risk and distinguishes unknown declarations", {
  x <- make_overlap_report_fixture(c("0-9", "5-14", "20-24"))
  expect_equal(overlaps(x)$age.char$original_levels, character())
  expect_true(overlaps(x)$age.char$has_overlap)
  expect_false(overlaps(x[, , , "20-24", drop = FALSE])$age.char$has_overlap)
  sem <- dim_semantics(x)
  sem$race <- pa_update_dim_semantics(sem$race, overlap_levels = character())
  dim_semantics(x) <- sem
  expect_equal(overlaps(x)$race$current_levels, character())
  expect_true(overlaps(x)$race$has_overlap)
})

test_that("overlaps checks simultaneous applicability instead of union-label presence", {
  x <- make_overlap_report_fixture(c("80-84", "85+", "85-89", "90+"))
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, overlap_levels = "85+",
    applicability = list(by = "year", schemas = list(
      list(from = "2020", through = "2020", levels = c("80-84", "85+")),
      list(from = "2021", through = "2021", levels = c("80-84", "85-89", "90+")))))
  dim_semantics(x) <- sem
  expect_equal(overlaps(x)$age.char$current_levels, "85+")
  expect_false(overlaps(x)$age.char$has_overlap)
  # Race remains unsafe independently of the resolved age dimension.
  expect_true(overlaps(x)$race$has_overlap)
  expect_error(sum(x), "Unsafe reduction")
  expect_silent(sum(drop_overlap_levels(x, "race")))
})
