# -------------------------------------------------------------------------------------->
# Script: build_census.R
# Description:
#   Build Census population cubes into inst/extdata using slab-by-year writing.
#   Series covered:
#     - census_decennial_county_1y (from Census/Census_2000_thru_2020.parquet)
#     - census_estimates_county_5y (from Estimates/Census cc-est*.csv)
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

need_ns("purrr")

# For data wrangling in estimates pipeline (mirrors census_data.r)
need_ns("readr")
need_ns("dplyr")
need_ns("tidyr")
need_ns("stringr")
need_ns("janitor")

# For parquet read (decennial + optional)
need_ns("arrow")

# Source build infrastructure
stopifnot(file.exists("data-raw/series_spec.R"))
stopifnot(file.exists("data-raw/labels_build.R"))
stopifnot(file.exists("data-raw/io_h5.R"))
stopifnot(file.exists("data-raw/validate.R"))

source("data-raw/series_spec.R")
source("data-raw/labels_build.R")
source("data-raw/io_h5.R")
source("data-raw/validate.R")

#  Helpers: load label objects saved in /data  ----------------------------

load_data_object <- function(name, envir = .GlobalEnv) {
  fn <- file.path("data", paste0(name, ".rda"))
  if (file.exists(fn)) {
    load(fn, envir = envir)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

ensure_labels_loaded_for_spec <- function(spec, envir = .GlobalEnv) {
  # spec$label_keys are object names like "years_census", "ages_census_5y", etc.
  keys <- unname(unlist(spec$label_keys))
  for (nm in unique(keys)) {
    if (!exists(nm, envir = envir, inherits = TRUE)) {
      ok <- load_data_object(nm, envir = envir)
      if (!ok) {
        # allow validate_labels_for_spec() to throw a precise error
        next
      }
    }
  }
  invisible(TRUE)
}

#  Resolve input paths (paths$population convention)  ----------------------------

census_input_paths <- function(paths = NULL) {
  if (!is.null(paths) && !is.null(paths$population)) {
    pop_root <- paths$population
    return(list(
      decennial_parquet = file.path(pop_root, "Census/Census_2000_thru_2020.parquet"),
      est_2010s_csv     = file.path(pop_root, "Estimates/Census/cc-est2020-alldata-48.csv"),
      est_2020s_csv     = file.path(pop_root, "Estimates/Census/cc-est2024-alldata-48.csv"),
      zcta_parquet = file.path(pop_root, "ZCTA/Tarrant ZCTA.parquet")#,
      #zcta_csv     = file.path(pop_root, "Estimates/Census/zcta_5y_tx.csv")
      
    ))
  }
  
  # Fallback for dev/CI if you stage raw files in-repo
  list(
    decennial_parquet = file.path("data-raw/raw/census", "Census_2000_thru_2020.parquet"),
    est_2010s_csv     = file.path("data-raw/raw/census", "cc-est2020-alldata-48.csv"),
    est_2020s_csv     = file.path("data-raw/raw/census", "cc-est2024-alldata-48.csv"),
    zcta_parquet = file.path("data-raw/raw/census", "zcta_5y_tx.parquet"),
    zcta_csv     = file.path("data-raw/raw/census", "zcta_5y_tx.csv")
    
  )
}

census_input_for_series <- function(series_id, census_paths) {
  switch(
    series_id,
    census_decennial_county_1y = census_paths$decennial_parquet,
    census_estimates_county_5y = NA_character_,  # estimates uses both csvs
    stop("No Census input mapping for series_id: ", series_id, call. = FALSE)
  )
}

#  Common: cast to canonical levels and build year slab  ----------------------------

cast_to_canonical <- function(df, labels) {
  # df must have: year, area.name, sex, age.char, race, ethnicity, population
  
  df$year      <- factor(as.character(df$year), levels = labels$year, ordered = TRUE)
  df$area.name <- factor(as.character(df$area.name), levels = labels$`area.name`)
  df$sex       <- factor(as.character(df$sex), levels = labels$sex)
  df$age.char  <- ordered(as.character(df$age.char), levels = labels$age.char)
  df$race      <- factor(as.character(df$race), levels = labels$race)
  df$ethnicity <- factor(as.character(df$ethnicity), levels = labels$ethnicity)
  df$population <- as.double(df$population)
  
  df
}

year_slab_5d <- function(df_one_year) {
  # IMPORTANT: slab dim order must match spec$dims excluding leading 'year':
  # area.name, sex, age.char, race, ethnicity
  xtabs(
    population ~ area.name + sex + age.char + race + ethnicity,
    data = df_one_year,
    drop.unused.levels = FALSE
  )
}

#  Decennial build (from parquet produced by census_data.r)  ----------------------------

read_decennial_parquet <- function(path) {
  if (!file.exists(path)) stop("Decennial parquet not found: ", path, call. = FALSE)
  arrow::read_parquet(path) |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      year      = as.integer(.data$year),
      area.name = as.character(.data$area.name),
      sex       = as.character(.data$sex),
      age.char  = as.character(.data$age.char),
      race      = as.character(.data$race),
      ethnicity = as.character(.data$ethnicity),
      population = as.double(.data$population)
    ) |> 
    dplyr::filter(sex      != "All",
                  age.char != "All",
                  race     != "All",
                  ethnicity != "All")
}

#  Estimates build (from Census Bureau all-data CSVs; mirrors census_data.r)  ----------------------------

read_estimate_file <- function(fn) {
  readr::read_csv(fn, col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}

build_estimates_long <- function(est_2010s_fn, est_2020s_fn) {
  if (!file.exists(est_2010s_fn)) stop("Estimates CSV not found: ", est_2010s_fn, call. = FALSE)
  if (!file.exists(est_2020s_fn)) stop("Estimates CSV not found: ", est_2020s_fn, call. = FALSE)
  
  # agegrp encoding and race encoding from census_data.r
  agegrp_vec <- c("All" = 0, "0-4" = 1, "5-9" = 2, "10-14" = 3, "15-19" = 4, "20-24" = 5,
                  "25-29" = 6, "30-34" = 7, "35-39" = 8, "40-44" = 9, "45-49" = 10, "50-54" = 11,
                  "55-59" = 12, "60-64" = 13, "65-69" = 14, "70-74" = 15, "75-79" = 16, "80-84" = 17,
                  "85 +" = 18)
  
  race_vec <- c(
    "allraces" = "All",
    "aa"  = "Asian",
    "aac" = "Asian or in combination",
    "ba"  = "Black",
    "bac" = "Black or in combination",
    "ia"  = "American Indian and Alaska Native",
    "iac" = "American Indian and Alaska Native or in combination",
    "na"  = "Hawaiian or Pacific Islander",
    "nac" = "Hawaiian or Pacific Islander or in combination",
    "tom" = "Two or more",
    "wa"  = "White",
    "wac" = "White or in combination"
  )
  
  est_2010s <- read_estimate_file(est_2010s_fn) |>
    dplyr::filter(!(year %in% c("1", "2", "13"))) # drop census/base rows 
  
  est_2020s <- read_estimate_file(est_2020s_fn) |>
    dplyr::filter(year != "1") |>
    dplyr::mutate(year = as.character(as.integer(year) + 11L)) # shift to align
  
  # browser()
  # check that the filter line below removes the "All" lines, looks like ot removes the 0 age
  est <- dplyr::bind_rows(est_2010s, est_2020s) |>
    dplyr::filter(agegrp != "0" ) |>  
    dplyr::rename(name = ctyname) |>
    dplyr::mutate(
      year = as.integer(year) + 2007L,  # 3->2010 ... (matches your name vec approach without hard-coding)
      # agegrp is integer codes; map to labels using agegrp_vec names
      age.char = factor(agegrp,
                        levels = as.character(agegrp_vec),
                        labels = names(agegrp_vec),
                        ordered = TRUE),
      area.name = stringr::str_remove(name, " County") |> as.character(),
      # fips in file: county is 3-digit; prepend "48"
      fips = paste0("48", county)
    ) |>
    # rename tot_* to allraces_*
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "^tot_", "allraces_"),
      .cols = dplyr::starts_with("tot_")
    ) |>
    # Gather race-by-sex columns; keep just county/year/age group axes
    tidyr::pivot_longer(
      cols = dplyr::matches("^(allraces|nh|h).+_(male|female)$"),
      names_to = "race_sex",
      values_to = "population"
    ) |>
    dplyr::mutate( dplyr::across(tidyr::matches("_pop$|_female$|_male$|population"), as.integer)) 
  
  est <- est |> 
    tidyr::separate(race_sex, into = c("race_code", "sex"), sep = "_") |>
    dplyr::filter(stringr::str_detect(race_code, "^(nh|h)")) |>   # added to remove "All"
    # # Compute sex  followin commented out as it does nothing
    # tidyr::pivot_wider(names_from = sex, values_from = population) |>
     #tidyr::pivot_longer(cols = c("male", "female"), names_to = "sex", values_to = "population") |>
    dplyr::mutate(
      sex = stringr::str_to_title(sex),
      ethnicity = dplyr::case_when(
        stringr::str_detect(race_code, "^nh") ~ "Non-Hispanic",
        stringr::str_detect(race_code, "^h")  ~ "Hispanic",
        TRUE ~ "All"
      ),
      race_code = dplyr::case_when(
        stringr::str_detect(race_code, "^(nh|h)$") ~ "allraces",
        stringr::str_detect(race_code, "^(nh|h)")  ~ stringr::str_remove(race_code, "^(nh|h)"),
        TRUE ~ race_code
      ),
      race = unname(race_vec[race_code])
    ) |>
    dplyr::filter(race_code != "allraces", ethnicity != "All" ) |>  # added to remove "All"
    dplyr::transmute(
      year = year,
      area.name = area.name,
      sex = sex,
      age.char = as.character(age.char),
      race = race,
      ethnicity = ethnicity,
      population = as.double(population)
    )
  
  est
}

#  ZCTA estimates build (2D: year x zcta)  ----------------------------

read_zcta_estimates <- function(zcta_parquet, zcta_csv) {
  if (file.exists(zcta_parquet)) {
    df <- arrow::read_parquet(zcta_parquet) |> dplyr::as_tibble()
  } else if (file.exists(zcta_csv)) {
    df <- readr::read_csv(zcta_csv, col_types = readr::cols(.default = readr::col_character())) |>
      dplyr::as_tibble()
  } else {
    stop("ZCTA input not found.", call. = FALSE)
  }
  
  df |>
    dplyr::transmute(
      `end.year` = as.integer(.data$`end.year`),
      `zip.code` = as.character(.data$`zip.code`),
      estimate   = as.double(.data$estimate),
      moe        = as.double(.data$moe)
    )
}

cast_zcta_to_canonical <- function(df, labels) {
  df$`end.year` <- factor(as.character(df$`end.year`), levels = labels$`end.year`, ordered = TRUE)
  df$`zip.code` <- factor(as.character(df$`zip.code`), levels = labels$`zip.code`)
  df
}

zcta_year_slab_1d <- function(df_one_year, zip_levels, value_col = c("estimate", "moe")) {
  value_col <- match.arg(value_col)
  
  x <- tapply(df_one_year[[value_col]], df_one_year$`zip.code`, sum, na.rm = TRUE)
  
  out <- array(0, dim = length(zip_levels))
  names(out) <- zip_levels
  out[names(x)] <- as.double(x)
  out
}


#  Build one Census series  ----------------------------

build_census_series <- function(series_id, paths = NULL) {
  
  specs <- tarr_series_specs()
  spec  <- specs[[series_id]]
  if (is.null(spec)) stop("Unknown series_id: ", series_id, call. = FALSE)
  
  # Ensure label objects saved in /data are loaded into env before validation
  ensure_labels_loaded_for_spec(spec, envir = .GlobalEnv)
  
  # Resolve labels + dim lengths (contract)
  
  lbl <- validate_labels_for_spec(spec, envir = parent.frame())
  labels <- lbl$labels
  dim_lengths <- lbl$dim_lengths
  
  # Resolve inputs
  cpaths <- census_input_paths(paths = paths)
  
  #  ZCTA series (2D: end.year × zip.code)  ----------------------------
  if (identical(series_id, "census_zcta_estimates")) {
    
    df <- read_zcta_estimates(cpaths$zcta_parquet, cpaths$zcta_csv)
    df <- cast_zcta_to_canonical(df, labels = labels)
    
    h5_path <- tarr_h5_path(spec$filename, root = "inst/extdata")
    
    h5_rebuild_cube(
      path = h5_path,
      spec = spec,
      dim_lengths = dim_lengths,
      overwrite = TRUE,
      attributes = list(
        built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
        input_source = if (file.exists(cpaths$zcta_parquet)) "parquet" else "csv",
        value_written = "estimate"
      )
    )
    
    years_keep <- labels$`end.year`
    zip_levels <- labels$`zip.code`
    by_year <- split(df, df$`end.year`)
    
    for (y in years_keep) {
      y_chr <- as.character(y)
      one <- by_year[[y_chr]]
      if (is.null(one)) {
        stop("No records found for end.year ", y_chr, " in series ", series_id, call. = FALSE)
      }
      
      slab <- zcta_year_slab_1d(one, zip_levels = zip_levels, value_col = "estimate")
      
      i <- match(y_chr, years_keep)
      if (is.na(i)) stop("end.year '", y_chr, "' not found in label set for ", series_id, call. = FALSE)
      
      h5_write_slab(
        path = h5_path,
        spec = spec,
        dim_lengths = dim_lengths,
        i = i,
        slab = slab
      )
    }
    
    validate_h5_for_spec(spec, root = "inst/extdata", envir = parent.frame(), check_attributes = TRUE)
    message("Census ", series_id, ": done -> ", h5_path)
    return(invisible(h5_path))
  }
  
  #  County series (6D: year × area.name × sex × age.char × race × ethnicity)  ----------------------------
  
  if (identical(series_id, "census_decennial_county_1y")) {
    browser()
    input_path <- census_input_for_series(series_id, cpaths)
    df <- read_decennial_parquet(input_path)
    
    
  } else if (identical(series_id, "census_estimates_county_5y")) {
    df <- build_estimates_long(cpaths$est_2010s_csv, cpaths$est_2020s_csv)
    
  } else {
    stop("build_census_series(): unsupported series_id: ", series_id, call. = FALSE)
  }
  
  #browser()
  # Cast to canonical factor levels for xtabs completeness & ordering
  df <- cast_to_canonical(df, labels = labels)
  df <- df[! is.na(df$area.name),]
  
  # guard against NA
  bad <- anyNA(df$year) ||
    anyNA(df$area.name) ||
    anyNA(df$sex)       ||
    anyNA(df$age.char)  ||
    anyNA(df$race)      ||
    anyNA(df$ethnicity)
  
  if(bad) {
    stop("Non-canonical labels detected after casting. Likely all or unexpected values.", call. = FALSE)
  }
    
  
  # HDF5 target
  h5_path <- tarr_h5_path(spec$filename, root = "inst/extdata")
  
  # Rebuild from scratch (deterministic)
  h5_rebuild_cube(
    path = h5_path,
    spec = spec,
    dim_lengths = dim_lengths,
    overwrite = TRUE,
    attributes = list(
      built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      input_source = if (identical(series_id, "census_decennial_county_1y")) "parquet" else "csv"
    )
  )
  
  # Write slab-by-year
  years_keep <- labels$year
  by_year <- split(df, df$year)
  
  for (y in years_keep) {
    y_chr <- as.character(y)
    one <- by_year[[y_chr]]
    if (is.null(one)) {
      stop("No records found for year ", y_chr, " in series ", series_id, call. = FALSE)
    }
    
    slab <- year_slab_5d(one)
    
    i <- match(y_chr, years_keep)
    if (is.na(i)) stop("Year '", y_chr, "' not found in label set for ", series_id, call. = FALSE)
    
    h5_write_slab(
      path = h5_path,
      spec = spec,
      dim_lengths = dim_lengths,
      i = i,
      slab = slab
    )
  }
  
  # Structural validation
  validate_h5_for_spec(spec, root = "inst/extdata", envir = parent.frame(), check_attributes = TRUE)
  
  message("Census ", series_id, ": done -> ", h5_path)
  invisible(h5_path)
}


#  Build Census (entry point for build_all.R)  ----------------------------

build_census <- function(paths = NULL,
                         series_ids = c(
                         "census_decennial_county_1y"#,
                         #"census_estimates_county_5y"#,
                          # "census_zcta_estimates"
                         )) {
  purrr::walk(series_ids, build_census_series, paths = paths)
  invisible(TRUE)
}
