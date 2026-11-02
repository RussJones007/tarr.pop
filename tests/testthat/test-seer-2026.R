load_seer_2026_helpers <- function() {
  script <- test_path("../../data-raw/seer_data_2026.r")
  if (!file.exists(script)) skip("SEER builder is available in the source repository only")
  env <- new.env(parent = asNamespace("tarr.pop"))
  sys.source(script, envir = env)
  env
}

seer_2026_fixture <- function(age_type, years = 2023:2024) {
  ages <- if (age_type == "single") 0:90 else 0:19
  keys <- expand.grid(age = ages, sex = 1:2, origin = 0:1, race = 1:4,
    county = c(1L, 439L), year = years)
  keys <- keys[order(keys$year), ]
  # Single ages each contribute 1; grouped file is their exact aggregation.
  pop <- if (age_type == "single") rep(1L, nrow(keys)) else
    c(1L, 4L, rep(5L, 17L), 1L)[keys$age + 1L]
  sprintf("%04dTX48%03d  %d%d%d%02d%08d", keys$year, keys$county,
    keys$race, keys$origin, keys$sex, keys$age, pop)
}

write_seer_2026_fixture <- function(lines) {
  file <- tempfile(fileext = ".gz")
  con <- gzfile(file, "wt")
  writeLines(lines, con)
  close(con)
  file
}

test_that("SEER age and applicability contracts describe this release, without Texas", {
  env <- load_seer_2026_helpers()
  dn <- env$seer_2026_dimnames("grouped")
  expect_length(dn$area.name, 254L)
  expect_false("Texas" %in% dn$area.name)
  expect_length(dn$age.char, 20L)
  expect_identical(tail(dn$age.char, 2), c("85-89", "90+"))
  expect_identical(tail(unname(env$seer_2026_ages("single")), 1), "90+")
  sem <- env$seer_2026_semantics(dn)
  expect_silent(validate_dim_semantics(sem, names(dn), "year", "area.name", dn))
  expect_true(all(vapply(sem, function(x) x@validated && x@partition_type == "partition", logical(1))))
  expect_identical(sem$age.char@applicability$schemas[[1]]$from, "1990")
  expect_identical(sem$age.char@applicability$schemas[[1]]$through, "2024")
  expect_identical(sem$age.char@applicability$schemas[[1]]$levels, dn$age.char)
  expect_false(pa_dim_has_overlap_risk(sem$age.char, dn$age.char, dn))
})

test_that("SEER parser maps explicit codes and rejects malformed data", {
  env <- load_seer_2026_helpers()
  counties <- c(Anderson = "48001", Tarrant = "48439")
  row <- "2024TX48439  4129000000042"
  x <- env$parse_seer_2026(row, "single", counties)
  expect_equal(x$population, 42L)
  expect_equal(x$county, 2L)
  expect_equal(x$sex, 1L) # Female, independent of which sexes appear first.
  expect_equal(x$race, 4L)
  expect_equal(x$ethnicity, 2L)
  expect_equal(x$age, 91L)
  expect_error(env$parse_seer_2026(row, "grouped", counties), "age code")
  expect_error(env$parse_seer_2026(paste0(row, "9"), "single", counties), "Malformed")
  expect_error(env$parse_seer_2026(sub("48439", "48000", row), "single", counties), "Unknown county")
  expect_error(env$parse_seer_2026(sub("  412", "  492", row), "single", counties), "unsupported")
})

test_that("SEER streaming checks coverage, duplicate keys, and valid zero completion", {
  env <- load_seer_2026_helpers()
  counties <- c(Anderson = "48001", Tarrant = "48439")
  lines <- seer_2026_fixture("grouped")
  # One omitted valid cell should be zero, while all category labels still apply.
  file <- write_seer_2026_fixture(lines[-1L])
  result <- list()
  env$read_seer_2026(file, "grouped", years = 2023:2024, counties = counties,
    chunk_lines = 137L, consume = function(x, i) result[[i]] <<- x)
  expect_length(result, 2L)
  expect_equal(result[[1]]["Anderson", "Male", "0", "White", "Non-Hispanic"], 0L)
  expect_false(anyNA(result[[1]]))
  expect_equal(sum(result[[1]]), sum(result[[2]]) - 1L)
  duplicate <- write_seer_2026_fixture(c(lines[1L], lines))
  expect_error(env$read_seer_2026(duplicate, "grouped", function(...) NULL,
    years = 2023:2024, counties = counties), "Duplicate")
  missing_year <- write_seer_2026_fixture(lines[substr(lines, 1, 4) == "2024"])
  expect_error(env$read_seer_2026(missing_year, "grouped", function(...) NULL,
    years = 2023:2024, counties = counties), "Missing")
  missing_county <- write_seer_2026_fixture(lines[substr(lines, 9, 11) == "001"])
  expect_error(env$read_seer_2026(missing_county, "grouped", function(...) NULL,
    years = 2023:2024, counties = counties), "Missing county")
  missing_end <- write_seer_2026_fixture(lines[substr(lines, 1, 4) == "2023"])
  expect_error(env$read_seer_2026(missing_end, "grouped", function(...) NULL,
    years = 2023:2024, counties = counties), "Incomplete")
  unsorted <- write_seer_2026_fixture(rev(lines))
  expect_error(env$read_seer_2026(unsorted, "grouped", function(...) NULL,
    years = 2023:2024, counties = counties), "unsorted")
})

test_that("SEER candidates round-trip current schema and reconcile both age resolutions", {
  env <- load_seer_2026_helpers()
  counties <- c(Anderson = "48001", Tarrant = "48439")
  paths <- stats::setNames(vapply(c("single", "grouped"), function(type) {
    input <- write_seer_2026_fixture(seer_2026_fixture(type))
    output <- tempfile(fileext = ".h5")
    env$write_seer_2026_candidate(input, output, type, 2023:2024, counties)
  }, character(1)), c("single", "grouped"))
  expect_true(env$validate_seer_2026_pair(paths[1], paths[2]))
  x <- open_poparray(filepath = paths[2])
  expect_s4_class(x, "poparray")
  expect_s4_class(x, "DelayedArray")
  expect_identical(cube_version(paths[2])$schema_version, "1.2.0")
  expect_identical(cube_version(paths[2])$metadata_revision, 1L)
  expect_identical(time_role(x), "year")
  expect_identical(area_role(x), "area.name")
  expect_identical(dim_semantics(x)$age.char@applicability$schemas[[1]]$levels, dimnames(x)$age.char)
  expect_match(get_source(x)$note, "MD5")
  # Installing a validated candidate must preserve existing identity and history.
  installed <- tempfile(fileext = ".h5")
  save_poparray(x, filepath = installed, revision_reason = "Initial fixture")
  original <- cube_version(installed)
  save_poparray(x, filepath = installed, overwrite = TRUE,
    revision_reason = "SEER February 2026 rebuild")
  expect_identical(cube_version(installed)$cube_id, original$cube_id)
  expect_identical(cube_version(installed)$metadata_revision, 2L)
  expect_identical(tail(cube_history(installed)$reason, 1), "SEER February 2026 rebuild")
  # Guard must catch a numerical discrepancy, not merely different marginal labels.
  rhdf5::h5write(99L, paths[2], "cube/population", index = rep(list(1L), 6L))
  expect_error(env$validate_seer_2026_pair(paths[1], paths[2]), "disagree")
})
