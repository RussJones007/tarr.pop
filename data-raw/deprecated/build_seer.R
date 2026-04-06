# -------------------------------------------------------------------------------------->
# Script: build_seer.R
# Description:
#   Build SEER population cubes into inst/extdata using slab-by-year writing.
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 4, 2026
# Revised:
# -------------------------------------------------------------------------------------->

#  Setup  ----------------------------

need_ns <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for build scripts. Install it and retry.", call. = FALSE)
  }
  invisible(TRUE)
}

need_ns("readr")
need_ns("purrr")

# Source build infrastructure (paths are what you listed as already written)
stopifnot(file.exists("data-raw/series_spec.R"))
stopifnot(file.exists("data-raw/labels_build.R"))
stopifnot(file.exists("data-raw/io_h5.R"))
stopifnot(file.exists("data-raw/validate.R"))

# Your uploaded spec file is named series_spec.r; repo likely uses series_specs.R.
if (file.exists("data-raw/series_specs.R")) {
  source("data-raw/series_specs.R")
} else {
  source("data-raw/series_spec.R")
}
source("data-raw/labels_build.R")
source("data-raw/io_h5.R")
source("data-raw/validate.R")

# Load county fips mapping (labels_build.R expects this too)
if (!file.exists("data/county_fips.rda")) {
  stop("Expected 'data/county_fips.rda' in package root.", call. = FALSE)
}
load("data/county_fips.rda")  # defines county_fips

#  SEER parsing helpers  ----------------------------

seer_fwf_cols <- function() {
  # Fixed-width columns as used historically in your pipeline
  readr::fwf_cols(
    year        = c(1, 4),
    state       = c(5, 6),
    state_fips  = c(7, 8),
    county_fips = c(9, 11),
    race        = c(14, 14),
    ethnicity   = c(15, 15),
    sex         = c(16, 16),
    age         = c(17, 18),
    population  = c(19, 27)
  )
}

seer_fips_to_county <- function(county_levels) {
  # county_fips: named integer vector; names are county labels in canonical set
  want <- county_fips[match(county_levels, names(county_fips))]
  setNames(names(want), as.character(unname(want)))
}

seer_recode <- function(df, spec, labels) {
  # df from read_fwf
  # labels: list of character vectors keyed by dim name (year, area.name, sex, age.char, race, ethnicity)
  
  df$year <- as.character(df$year)
  
  # geo -> area.name
  df$state_fips  <- as.integer(df$state_fips)
  df$county_fips <- as.integer(df$county_fips)
  fips <- df$state_fips * 1000L + df$county_fips
  
  fips_to_name <- seer_fips_to_county(labels$`area.name`)
  df$area.name <- unname(fips_to_name[as.character(fips)])
  df <- df[!is.na(df$area.name), , drop = FALSE]
  
  df$population <- as.double(df$population)
  
  # sex: file 1=Male, 2=Female; canonical labels are c("Female","Male")
  df$sex <- ifelse(as.integer(df$sex) == 2L, "Female", "Male")
  
  # ethnicity: file 0=Non-Hispanic, 1=Hispanic
  df$ethnicity <- ifelse(as.integer(df$ethnicity) == 1L, "Hispanic", "Non-Hispanic")
  
  # race: file 1..4 maps to your race_levels_seer order
  race_map <- c(
    "White",
    "Black",
    "American Indian/Alaskan Native",
    "Asian or Pacific Islander"
  )
  df$race <- race_map[as.integer(df$race)]
  
  # age: use spec$type_key to decide mapping
  age_i <- as.integer(df$age)
  if (identical(spec$type_key, "grouped_age")) {
    # grouped files use 0..(n-1) indexing into ages_seer_5y labels
    idx <- age_i + 1L
    idx[idx < 1L | idx > length(labels$age.char)] <- NA_integer_
    df$age.char <- labels$age.char[idx]
  } else {
    # single-age files: literal age values (0..90) as character
    df$age.char <- as.character(age_i)
  }
  
  # Keep only needed vars, then cast to factors with canonical levels
  df <- df[, c("year", "area.name", "sex", "age.char", "race", "ethnicity", "population"), drop = FALSE]
  
  df$year      <- factor(df$year,      levels = labels$year, ordered = TRUE)
  df$area.name <- factor(df$area.name, levels = labels$`area.name`)
  df$sex       <- factor(df$sex,       levels = labels$sex)
  df$age.char  <- ordered(df$age.char, levels = labels$age.char)
  df$race      <- factor(df$race,      levels = labels$race)
  df$ethnicity <- factor(df$ethnicity, levels = labels$ethnicity)
  
  df
}

seer_year_slab <- function(df) {
  # slab dim order must match spec$dims excluding leading 'year':
  # area.name, sex, age.char, race, ethnicity
  xtabs(
    population ~ area.name + sex + age.char + race + ethnicity,
    data = df,
    drop.unused.levels = FALSE
  ) |> 
    as.array()
}

seer_stream_by_year <- function(path, years_keep, on_year, chunk_n = 200000L) {
  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, open = "rt") else file(path, open = "rt")
  on.exit(close(con), add = TRUE)
  
  current_year <- NULL
  buf <- character()
  
  repeat {
    lines <- readLines(con, n = chunk_n, warn = FALSE)
    if (!length(lines)) break
    
    yrs <- substr(lines, 1L, 4L)
    
    # starts/ends of runs where year is constant
    change <- c(TRUE, yrs[-1L] != yrs[-length(yrs)])
    idx_start <- which(change)
    idx_end <- c(idx_start[-1L] - 1L, length(lines))
    
    for (k in seq_along(idx_start)) {
      y <- yrs[idx_start[k]]
      seg <- lines[idx_start[k]:idx_end[k]]
      
      if (is.null(current_year)) current_year <- y
      
      if (!identical(y, current_year)) {
        if (current_year %in% years_keep && length(buf)) on_year(current_year, buf)
        current_year <- y
        buf <- character()
      }
      
      # only accumulate if you actually plan to process this year
      if (current_year %in% years_keep) {
        buf <- c(buf, seg)
      }
    }
  }
  
  if (!is.null(current_year) && current_year %in% years_keep && length(buf)) {
    on_year(current_year, buf)
  }
  
  invisible(TRUE)
}

#  Input paths  ----------------------------

seer_input_paths <- function(paths = NULL) {
  # Prefer user's old convention: paths$population
  if (!is.null(paths) && !is.null(paths$population)) {
    pop_root <- paths$population
    return(list(
      stdpop  = file.path(pop_root, "Standard Populations/stdpop.19ages.txt"),
      seer_1y = file.path(pop_root, "Estimates/SEER/tx.1990_2023.singleages.through89.90plus.adjusted.txt.gz"),
      seer_5y = file.path(pop_root, "Estimates/SEER/tx.1990_2023.20ages.adjusted.txt.gz")
    ))
  }
  
  # Fallback: repo-local convention (handy for CI if you stage raw inputs)
  list(
    stdpop  = file.path("data-raw/raw/seer", "stdpop.19ages.txt"),
    seer_1y = file.path("data-raw/raw/seer", "tx.1990_2023.singleages.through89.90plus.adjusted.txt.gz"),
    seer_5y = file.path("data-raw/raw/seer", "tx.1990_2023.20ages.adjusted.txt.gz")
  )
}

seer_input_for_series <- function(series_id, seer_paths) {
  switch(
    series_id,
    seer_estimates_county_1y = seer_paths$seer_1y,
    seer_estimates_county_5y = seer_paths$seer_5y,
    stop("No SEER input mapping for series_id: ", series_id, call. = FALSE)
  )
}

#  Build one SEER series  ----------------------------

build_seer_series <- function(series_id, paths = NULL) {
  
  specs <- tarr_series_specs()
  spec <- specs[[series_id]]
  if (is.null(spec)) stop("Unknown series_id: ", series_id, call. = FALSE)
  
  # Resolve labels + dim lengths from validator (contract source of truth)
  lbl <- validate_labels_for_spec(spec, envir = parent.frame())
  labels <- lbl$labels
  dim_lengths <- lbl$dim_lengths
  
  # Resolve raw input path using old convention
  seer_paths <- seer_input_paths(paths = paths)
  input_path <- seer_input_for_series(series_id, seer_paths)
  
  if (!file.exists(input_path)) {
    stop(
      "SEER input file not found for ", series_id, ":\n  ", input_path, "\n\n",
      "If you're using the old convention, ensure `paths$population` points to your population root folder.",
      call. = FALSE
    )
  }
  
  # HDF5 target
  h5_path <- tarr_h5_path(spec$filename, root = "inst/extdata")
  
  # Rebuild from scratch (safe, deterministic)
  h5_rebuild_cube(
    path = h5_path,
    spec = spec,
    dim_lengths = dim_lengths,
    overwrite = TRUE,
    attributes = list(
      built_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      input_path = normalizePath(input_path, winslash = "/", mustWork = TRUE)
    )
  )
  
  cols <- seer_fwf_cols()
  years_keep <- labels$year
  
  on_year <- function(year_chr, lines) {
    message("SEER ", series_id, ": year ", year_chr, " (", length(lines), " lines)")
    
    df <- readr::read_fwf(
      file = I(lines),
      col_positions = cols,
      col_types = readr::cols(
        year        = readr::col_integer(),
        state       = readr::col_character(),
        state_fips  = readr::col_integer(),
        county_fips = readr::col_integer(),
        race        = readr::col_integer(),
        ethnicity   = readr::col_integer(),
        sex         = readr::col_integer(),
        age         = readr::col_integer(),
        population  = readr::col_double()
      )
    )
    
    df <- seer_recode(df, spec = spec, labels = labels)
    
    slab <- seer_year_slab(df)
    
    # Map year label -> index i in canonical year labels
    i <- match(year_chr, years_keep)
    if (is.na(i)) stop("Year '", year_chr, "' not found in label set for ", series_id, call. = FALSE)
    
    # Write slab
    h5_write_slab(
      path = h5_path,
      spec = spec,
      dim_lengths = dim_lengths,
      i = i,
      slab = slab
    )
  }
  
  seer_stream_by_year(input_path, years_keep = years_keep, on_year = on_year)
  
  # Structural validation against spec+labels+attrs
  validate_h5_for_spec(spec, root = "inst/extdata", envir = parent.frame(), check_attributes = TRUE)
  
  message("SEER ", series_id, ": done -> ", h5_path)
  invisible(h5_path)
}

#  Build SEER (public entry point for build_all.R)  ----------------------------

build_seer <- function(paths = NULL,
                       series_ids = c("seer_estimates_county_1y", "seer_estimates_county_5y")) {
  
  purrr::walk(series_ids, build_seer_series, paths = paths)
  invisible(TRUE)
}
