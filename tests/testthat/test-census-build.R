load_census_build_helpers <- function() {
  path <- testthat::test_path("..", "..", "data-raw", "census_data_2.r")
  testthat::skip_if_not(file.exists(path), "Census build script is excluded from built packages")
  env <- new.env(parent = asNamespace("tarr.pop"))
  # Evaluate function definitions only: no downloads or production cube writes.
  for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, env)
  }
  env
}

test_that("estimate YEAR decoding follows the final layouts, including July base years", {
  h <- load_census_build_helpers()
  expect_equal(h$census_estimate_years(1:12, "intercensal"), c(NA, 2010:2019, NA))
  expect_equal(h$census_estimate_years(1:7, "current"), c(NA, 2020:2025))
  expect_error(h$census_estimate_years(13, "intercensal"), "Unexpected")
})

test_that("estimates retain race combination categories and separate ethnicity", {
  h <- load_census_build_helpers()
  races <- h$census_estimate_races()
  df <- data.frame(year = 2020L, area.name = "Tarrant", agegrp = "1")
  for (prefix in c("nh", "h")) for (race in names(races)) for (sex in c("male", "female")) {
    df[[paste0(prefix, race, "_", sex)]] <- "12"
  }
  long <- h$transform_census_estimates(df)
  expect_equal(nrow(long), 44L)
  expect_setequal(as.character(long$ethnicity), c("Hispanic", "Non-Hispanic"))
  expect_setequal(as.character(long$race), unname(races))
  expect_false(any(vapply(long, function(x) any(as.character(x) == "All"), logical(1))))
  support <- h$census_support_table(long, "Tarrant")
  sem <- h$census_dimension_semantics(support, "July 1")
  expect_true(all(vapply(sem, function(entry) isTRUE(entry@validated), logical(1))))
  expect_true(pa_labels_have_overlap_risk(sem$race, levels(long$race)))
  expect_false(pa_labels_have_overlap_risk(sem$race, unname(races[1:6])))
  expect_equal(sem$ethnicity@partition_type, "partition")
  expect_error(h$canonical_census_table(rbind(long, long)), "Duplicate")
  official <- h$census_estimate_support(2020L)
  expect_equal(nrow(official), 254L * 2L * 18L * 11L * 2L)
  expect_false(anyNA(official))
})

test_that("decennial decoding subtracts ethnicity and rejects incomplete pairs", {
  h <- load_census_build_helpers()
  raw <- data.frame(year = 2020L, GEOID = "48439", value = c(20, 15, 22, 18),
    label = rep(c(" !!Total:!!Male:!!Under 1 year", " !!Total:!!Female:!!Under 1 year"), each = 2),
    concept = rep(c("SEX BY SINGLE-YEAR AGE (WHITE ALONE)",
      "SEX BY SINGLE-YEAR AGE (WHITE ALONE, NOT HISPANIC OR LATINO)"), 2))
  df <- h$transform_census_decennial(raw)
  expect_equal(sort(df$population), c(4, 5, 15, 18))
  expect_equal(levels(df$age.char), "< 1")
  raw$label <- gsub(":", "", raw$label)
  expect_equal(h$transform_census_decennial(raw)$population, df$population)
  expect_error(h$transform_census_decennial(raw[-2, ]), "Incomplete")
})

test_that("ACS support retains unavailable ZCTAs and records period limitations", {
  h <- load_census_build_helpers()
  raw <- data.frame(year = 2024L, GEOID = "76001", estimate = 20, moe = -555555555)
  expect_true(is.na(h$transform_census_zcta(raw, "moe")$moe))
  support <- h$census_zcta_support(2023:2024, c("76001", "76005"))
  expect_equal(nrow(support), 4L)
  sem <- h$census_zcta_semantics()
  expect_true(all(vapply(sem, function(entry) isTRUE(entry@validated), logical(1))))
  expect_equal(sem$year@partition_type, "partition")
  expect_match(sem$year@notes, "periods overlap")
  expect_false(pa_labels_have_overlap_risk(sem$zip.code, c("76001", "76005")))
})

test_that("census ingestion round trips roles, semantics, and source NA", {
  h <- load_census_build_helpers()
  root <- tempfile("census-build-"); dir.create(root)
  withr::local_options(list(tarr.pop.cube_path = root))
  on.exit(unlink(root, recursive = TRUE))
  support <- h$census_zcta_support(2024L, c("76001", "76005"))
  df <- data.frame(year = 2024L, zip.code = "76001", estimate = 20)
  path <- h$ingest_census_table(df, support, h$census_zcta_semantics(), "fixture", root,
    list(note = "fixture", population_type = "Estimate", source = "fixture"),
    data_col = "estimate", area_dim = "zip.code", completion_policy = "na")
  rebuild_poparray_registry(root)
  pop <- open_poparray("fixture")
  expect_s4_class(pop, "poparray")
  expect_equal(pop@time_role, "year")
  expect_equal(pop@area_role, "zip.code")
  expect_equal(pop@data_col, "estimate")
  expect_equal(dim_semantics(pop), h$census_zcta_semantics())
  expect_true(all(vapply(dim_semantics(pop), function(entry) isTRUE(entry@validated), logical(1))))
  expect_equal(dim_semantics(pop)$year@partition_type, "partition")
  expect_equal(as.numeric(pop), c(20, NA))
})


test_that("annual appends preserve validated source semantics", {
  h <- load_census_build_helpers()
  root <- tempfile("census-append-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  semantics <- h$census_zcta_semantics()
  df <- data.frame(year = 2023L, zip.code = "76001", estimate = 20)
  path <- h$ingest_census_table(df, h$census_zcta_support(2023L, "76001"),
    semantics, "append-fixture", root,
    list(note = "fixture", population_type = "Estimate", source = "fixture"),
    data_col = "estimate", area_dim = "zip.code")
  next_df <- data.frame(year = 2024L, zip.code = "76001", estimate = 21)
  output <- file.path(root, "appended.h5")
  add_population_data(cube = path, reader = function(...) next_df,
    output_filepath = output, completion_policy = "error", data_col = "estimate")
  expect_equal(dim_semantics(output), semantics)
  expect_true(all(vapply(dim_semantics(output), function(entry) isTRUE(entry@validated), logical(1))))
})
