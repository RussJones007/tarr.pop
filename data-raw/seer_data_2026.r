# -------------------------------------------------------------------------------------->
# Script: seer_data_2026.r
# Description:
#   Rebuilds county-only SEER cubes from the February 2026 Texas expanded-race files.
#   Reads compressed records in chunks and constructs one EAGER annual array at a time;
#   output and subsequent copying remain HDF5-backed. Sourcing defines helpers only.
#
#   build_seer_2026() stages both age resolutions, checks every grouped-age cell against
#   single ages, then writes canonical cubes with current schema, semantics, and history.
#   Texas totals are calculated at runtime. Sparse unreported valid cells are zeros;
#   malformed records, duplicate keys, missing years/counties, or pair discrepancies fail.
#   See seer_data_2026.md for usage, provenance, applicability and replacement details.
# -------------------------------------------------------------------------------------->
# Author: Russ Jones (original seer_def.r); adapted with Codex
# Created: November 2, 2026
# -------------------------------------------------------------------------------------->

#' Define February 2026 SEER age codes as canonical intervals
#' @param age_type Single-year or grouped-age input.
#' @return Named character vector indexed by zero-based source age codes.
seer_2026_ages <- function(age_type = c("single", "grouped")) {
  age_type <- match.arg(age_type)
  labels <- if (age_type == "single") c(as.character(0:89), "90+") else
    c("0", "1-4", paste0(seq(5, 85, 5), "-", seq(9, 89, 5)), "90+")
  labels <- as.character(agecraft::as.age_group(labels))
  stats::setNames(labels, as.character(seq_along(labels) - 1L))
}

#' Resolve the county-only Texas reference
#' @return Named vector of five-character FIPS identifiers, in reference order.
seer_2026_counties <- function() {
  ref <- tarr.pop::county_fips
  stats::setNames(as.character(ref[names(ref) != "Texas"]), names(ref)[names(ref) != "Texas"])
}

#' Declare the complete, fixed-schema support of this release
#' @param age_type Single-year or grouped-age input.
#' @param years Expected consecutive years; restricted subsets support validation runs.
#' @param counties Named county FIPS vector; default is all 254 Texas counties.
#' @return Named list of dimension labels in canonical storage order.
seer_2026_dimnames <- function(age_type, years = 1990:2024, counties = seer_2026_counties()) {
  checkmate::assert_integerish(years, lower = 1990, upper = 2024, min.len = 1, any.missing = FALSE)
  if (any(diff(years) != 1)) cli::cli_abort("Years must be consecutive and increasing.")
  checkmate::assert_character(counties, min.len = 1, any.missing = FALSE, unique = TRUE)
  if (is.null(names(counties)) || anyNA(names(counties)) || any(!nzchar(names(counties))) ||
      anyDuplicated(names(counties)) || any(names(counties) %in% c("Texas", "All")) ||
      any(!grepl("^48[0-9]{3}$", counties)) || any(counties == "48000")) {
    cli::cli_abort("counties must be named Texas county FIPS identifiers, without Texas totals.")
  }
  list(year = as.character(years), area.name = names(counties), sex = c("Female", "Male"),
    age.char = unname(seer_2026_ages(age_type)),
    race = c("White", "Black", "American Indian/Alaskan Native", "Asian or Pacific Islander"),
    ethnicity = c("Non-Hispanic", "Hispanic"))
}

#' Build validated dimensional contracts from the release dictionary
#' @param dn Canonical dimension labels.
#' @return Named DimSemantics list, with explicit fixed applicability by year.
#' @details The 2026 release applies 20 grouped ages (or 91 single ages) to ALL
#'   years. Do not attach the old 19-group schema to historical years. Race and
#'   ethnicity are separate, mutually exclusive partitions within their axes.
seer_2026_semantics <- function(dn) {
  result <- lapply(names(dn), function(d) {
    app <- if (d == "year") NULL else list(by = "year", schemas = list(list(
      from = dn$year[1L], through = tail(dn$year, 1L), levels = dn[[d]])))
    tarr.pop:::new_dim_semantics(d, domain = switch(d, year = "time", area.name = "area",
      age.char = "age", d), scale_type = if (d %in% c("year", "age.char")) "interval" else "nominal",
      partition_type = "partition", validated = TRUE, applicability = app,
      notes = switch(d,
        year = "July 1 estimates; February 2026 SEER release, Vintage 2024; rebuilds historical estimates.",
        area.name = "Texas counties only; state totals are calculated at runtime.",
        age.char = "Canonical half-open age intervals; terminal 90+ is open-ended. Fixed across 1990-2024.",
        race = "Four mutually exclusive bridged race groups; not Census race-alone-or-in-combination categories.",
        ethnicity = "Hispanic origin applies independently of bridged race in every source year.",
        "Source-defined mutually exclusive categories."))
  })
  stats::setNames(result, names(dn))
}

#' Parse and validate fixed-width SEER records
#' @param lines Character records, excluding line terminators.
#' @param age_type Single-year or grouped-age input.
#' @param counties Named county FIPS reference.
#' @return Data frame of numeric source keys and population counts.
#' @details Uses 26-byte records, including the eight-byte population field at
#'   columns 19-26. Codes and geography are checked before sparse completion.
parse_seer_2026 <- function(lines, age_type, counties = seer_2026_counties()) {
  if (!length(lines) || anyNA(lines) || any(nchar(lines, type = "bytes") != 26L) ||
      any(!grepl("^[0-9]{4}TX48[0-9]{3}  [1-4][01][12][0-9]{10}$", lines))) {
    cli::cli_abort("Malformed 26-byte Texas SEER records or unsupported source codes.")
  }
  result <- data.frame(year = as.integer(substr(lines, 1, 4)),
    county = match(substr(lines, 7, 11), unname(counties)),
    sex = match(as.integer(substr(lines, 16, 16)), c(2L, 1L)),
    age = as.integer(substr(lines, 17, 18)) + 1L,
    race = as.integer(substr(lines, 14, 14)),
    ethnicity = as.integer(substr(lines, 15, 15)) + 1L,
    population = as.integer(substr(lines, 19, 26)))
  if (anyNA(result) || any(result$year < 1990L | result$year > 2024L) ||
      any(result$age > length(seer_2026_ages(age_type)))) {
    cli::cli_abort("Unknown county, year, or age code in SEER source.")
  }
  result
}

#' Convert one source year to its explicitly declared support
#' @param x Parsed records for exactly one year.
#' @param dn Full cube dimension labels.
#' @return Annual five-dimensional integer array (county, sex, age, race, origin).
#' @details EAGER annual buffer only. All release categories apply throughout;
#'   omitted records within this support represent zero counts, not structural NA.
seer_2026_year_array <- function(x, dn) {
  if (length(unique(x$year)) != 1L) cli::cli_abort("Expected exactly one source year.")
  if (!setequal(x$county, seq_along(dn$area.name))) cli::cli_abort("Missing county coverage in source year.")
  if (!setequal(x$age, seq_along(dn$age.char)) || !setequal(x$sex, 1:2) ||
      !setequal(x$race, 1:4) || !setequal(x$ethnicity, 1:2)) {
    cli::cli_abort("Missing demographic-axis coverage in source year.")
  }
  extents <- lengths(dn[-1L])
  strides <- c(1, head(cumprod(extents), -1L))
  keys <- x[c("county", "sex", "age", "race", "ethnicity")]
  index <- 1 + Reduce(`+`, Map(function(key, stride) (key - 1) * stride, keys, strides))
  if (anyDuplicated(index)) cli::cli_abort("Duplicate county demographic keys in SEER source year.")
  result <- array(0L, dim = extents, dimnames = dn[-1L])
  result[index] <- x$population
  result
}

#' Stream compressed source records to an annual consumer
#' @param file Gzip source path.
#' @param age_type Single-year or grouped-age input.
#' @param consume Function taking an annual array and year index.
#' @param years Exact expected year coverage.
#' @param counties Named county FIPS reference.
#' @param chunk_lines Maximum records read per chunk.
#' @return Invisibly, the dimension labels. Fails on missing or unsorted years.
read_seer_2026 <- function(file, age_type, consume, years = 1990:2024,
    counties = seer_2026_counties(), chunk_lines = 100000L) {
  checkmate::assert_file_exists(file)
  checkmate::assert_function(consume)
  checkmate::assert_int(chunk_lines, lower = 1L)
  dn <- seer_2026_dimnames(age_type, years, counties)
  con <- gzfile(file, "rt")
  on.exit(close(con))
  pending <- character()
  current <- NULL
  seen <- integer()
  flush <- function() {
    index <- match(current, years)
    if (is.na(index) || index != length(seen) + 1L) cli::cli_abort("Missing, unexpected, or unsorted SEER source years.")
    x <- parse_seer_2026(pending, age_type, counties)
    values <- seer_2026_year_array(x, dn)
    consume(values, index)
    seen <<- c(seen, current)
  }
  repeat {
    lines <- readLines(con, n = chunk_lines, warn = FALSE)
    if (!length(lines)) break
    chunk_years <- suppressWarnings(as.integer(substr(lines, 1, 4)))
    if (anyNA(chunk_years) || any(diff(chunk_years) < 0) ||
        (!is.null(current) && chunk_years[1L] < current)) cli::cli_abort("Invalid or unsorted SEER source years.")
    for (year in unique(chunk_years)) {
      if (!is.null(current) && year != current) {
        flush()
        pending <- character()
      }
      current <- year
      pending <- c(pending, lines[chunk_years == year])
    }
  }
  if (length(pending)) flush()
  if (!identical(as.integer(seen), as.integer(years))) cli::cli_abort("Incomplete SEER source year coverage.")
  invisible(dn)
}

#' Build one candidate cube through the canonical versioned writer
#' @param file Compressed source file.
#' @param filepath Candidate cube output path.
#' @param age_type Single-year or grouped-age input.
#' @param years Expected source years.
#' @param counties Named county reference.
#' @return Normalized output path. Existing candidate files are never overwritten.
write_seer_2026_candidate <- function(file, filepath, age_type, years = 1990:2024,
    counties = seer_2026_counties()) {
  if (file.exists(filepath)) cli::cli_abort("Candidate already exists: {.file {filepath}}.")
  dn <- seer_2026_dimnames(age_type, years, counties)
  scratch <- tempfile("seer_annual_", fileext = ".h5")
  on.exit(unlink(scratch))
  rhdf5::h5createFile(scratch)
  rhdf5::h5createDataset(scratch, "population", dims = lengths(dn), storage.mode = "integer",
    chunk = c(1L, min(32L, length(counties)), 2L, length(dn$age.char), 4L, 2L), level = 6L)
  read_seer_2026(file, age_type, years = years, counties = counties,
    consume = function(values, index) rhdf5::h5write(array(values, dim = c(1L, dim(values))),
      scratch, "population", index = c(list(index), rep(list(NULL), 5L))))
  series <- paste0("seer_estimates_county_", if (age_type == "single") "1y" else "5y")
  note <- paste("SEER February 2026 release; expanded bridged races by Hispanic origin; Vintage 2024.",
    "Full replacement includes revised 2010-2019 intercensal and 2020-2024 estimates.",
    "County-only support; omitted valid source records completed with zero; all categories apply in every year.",
    "Source:", basename(file), "MD5:", unname(tools::md5sum(file)),
    "Dictionary: https://seer.cancer.gov/popdata/popdic.html")
  tarr.pop:::pa_write_poparray_cube(HDF5Array::HDF5Array(scratch, "population"),
    filepath = filepath, dimnames_list = dn, time_dim = "year", area_dim = "area.name",
    dim_semantics = seer_2026_semantics(dn), series_id = series, geo = "county",
    extendable_year = TRUE, source = list(note = note,
      source = "National Cancer Institute; SEER Program; https://seer.cancer.gov/popdata/download.html",
      population_type = if (age_type == "single") "single_age" else "grouped_age",
      updated = as.character(Sys.Date())), revision_reason = "Rebuilt from February 2026 SEER release")
  normalizePath(filepath)
}

#' Check all grouped-age cells against the independent single-age file
#' @param single Single-age canonical candidate path.
#' @param grouped Grouped-age canonical candidate path.
#' @return Invisibly TRUE; mismatch fails before destination cubes are replaced.
validate_seer_2026_pair <- function(single, grouped) {
  s <- tarr.pop::open_poparray(filepath = single)
  g <- tarr.pop::open_poparray(filepath = grouped)
  sdn <- dimnames(s)
  gdn <- dimnames(g)
  if (!identical(sdn[-4L], gdn[-4L])) cli::cli_abort("SEER candidate dimensions do not align.")
  contributors <- lapply(gdn$age.char, function(age) tarr.pop:::pa_age_contributors(sdn$age.char, age))
  for (year in seq_along(sdn$year)) {
    index <- c(list(year), rep(list(NULL), 5L))
    a <- rhdf5::h5read(single, "cube/population", index = index)
    b <- rhdf5::h5read(grouped, "cube/population", index = index)
    dim(a) <- lengths(sdn[-1L])
    dim(b) <- lengths(gdn[-1L])
    for (age in seq_along(contributors)) {
      positions <- match(contributors[[age]], sdn$age.char)
      if (!length(positions) || anyNA(positions)) cli::cli_abort("Cannot reconcile SEER age intervals.")
      totals <- apply(a[, , positions, , , drop = FALSE], c(1, 2, 4, 5), sum)
      if (any(totals != b[, , age, , ])) cli::cli_abort("SEER age files disagree in {sdn$year[year]}, age {gdn$age.char[age]}.")
    }
  }
  invisible(TRUE)
}

#' Recreate both SEER cubes from the downloaded February 2026 release
#' @param cube_root Cube storage root containing base/; explicit to avoid accidental builds.
#' @param input_dir Directory containing the two downloaded Texas gzip files.
#' @param overwrite Permit replacement of existing SEER cubes (default FALSE).
#' @return Named paths of the two written cubes; does not rebuild the registry.
#' @details All candidates are validated before installation. Canonical writer
#'   preserves existing cube IDs/history, adopting legacy metadata as a baseline.
#'   Each replacement is staged, but the two files are not a joint transaction.
build_seer_2026 <- function(cube_root,
    input_dir = "~/R/Projects/Population/Estimates/SEER", overwrite = FALSE) {
  checkmate::assert_string(cube_root, min.chars = 1L)
  checkmate::assert_flag(overwrite)
  input_dir <- path.expand(input_dir)
  output_dir <- file.path(path.expand(cube_root), "base")
  files <- c(single = "tx.1990_2024.singleages.through89.90plus.adjusted.txt.gz",
    grouped = "tx.1990_2024.20ages.adjusted.txt.gz")
  sources <- file.path(input_dir, files)
  invisible(lapply(sources, checkmate::assert_file_exists))
  paths <- stats::setNames(file.path(output_dir,
    c("seer_estimates_county_1y.h5", "seer_estimates_county_5y.h5")), names(files))
  if (!overwrite && any(file.exists(paths))) cli::cli_abort("SEER cubes already exist; use overwrite = TRUE for a full rebuild.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("seer_2026_", tmpdir = output_dir)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE))
  candidates <- stats::setNames(file.path(stage, basename(paths)), names(files))
  for (i in seq_along(files)) {
    message("Staging SEER ", names(files)[i], " ages")
    write_seer_2026_candidate(sources[i], candidates[i], names(files)[i])
  }
  validate_seer_2026_pair(candidates[["single"]], candidates[["grouped"]])
  for (i in seq_along(paths)) {
    x <- tarr.pop::open_poparray(filepath = candidates[i])
    tarr.pop::save_poparray(x, filepath = paths[i], overwrite = overwrite,
      series_id = sub("\\.h5$", "", basename(paths[i])), geo = "county", extendable_year = TRUE,
      revision_reason = paste("SEER February 2026 full rebuild: 1990-2024; revised historical estimates;",
        "removed Texas aggregate; corrected open-ended ages; fixed-schema applicability"))
  }
  invisible(paths)
}
