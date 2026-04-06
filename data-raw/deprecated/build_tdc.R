# -------------------------------------------------------------------------------------->
# Script: build_tdc.R
# Description:
#   Build Texas Demographic Center (TDC) population cubes into inst/extdata using
#   slab-by-year writing.
#
#   Series covered (see data-raw/series_spec*.r):
#     - tdc_estimates_county   (5D: year × area.name × sex × age.char × race.eth)
#     - tdc_projections_county (5D: year × area.name × sex × age.char × race.eth)
#
# Constraints:
#   - Leave TDC as race.eth (combined race/eth dimension)
#   - Use slab-by-year writing via io_h5.R
#
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
need_ns("arrow")
need_ns("dplyr")

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

tdc_input_paths <- function(paths = NULL) {
  if (!is.null(paths) && !is.null(paths$population)) {
    pop_root <- paths$population
    return(list(
      # From pop_def.r outputs:
      estimates_parquet   = file.path(pop_root, "Estimates/Texas Demographic Center/asre/Estimates.parquet"),
      projections_parquet = file.path(pop_root, "Projections/Population_Proj_2010-2050.parquet")
    ))
  }
  
  # Fallback for dev/CI if you stage raw files in-repo
  list(
    estimates_parquet   = file.path("data-raw/raw/tdc", "Estimates.parquet"),
    projections_parquet = file.path("data-raw/raw/tdc", "Population_Proj_2010-2050.parquet")
  )
}

tdc_input_for_series <- function(series_id, tdc_paths) {
  switch(
    series_id,
    tdc_estimates_county   = tdc_paths$estimates_parquet,
    tdc_projections_county = tdc_paths$projections_parquet,
    stop("No TDC input mapping for series_id: ", series_id, call. = FALSE)
  )
}

#  Read + canonicalize  ----------------------------

read_tdc_parquet <- function(path) {
  if (!file.exists(path)) stop("TDC parquet not found: ", path, call. = FALSE)
  
  arrow::read_parquet(path) |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      year      = as.integer(.data$year),
      area.name = as.character(.data$area.name),
      sex       = as.character(.data$sex),
      age.char  = as.character(.data$age.char),
      `race.eth`= as.character(.data$`race.eth`),
      population = as.double(.data$population)
    )
}

cast_tdc_to_canonical <- function(df, labels) {
  # IMPORTANT: order/levels must match spec dims and label objects
  df$year      <- factor(as.character(df$year), levels = labels$year, ordered = TRUE)
  df$area.name <- factor(as.character(df$area.name), levels = labels$`area.name`)
  df$sex       <- factor(as.character(df$sex), levels = labels$sex)
  df$age.char  <- ordered(as.character(df$age.char), levels = labels$age.char)
  df$`race.eth`<- factor(as.character(df$`race.eth`), levels = labels$`race.eth`)
  df$population <- as.double(df$population)
  df
}

# slab for 5D cube (excluding leading year): area.name × sex × age.char × race.eth
year_slab_4d_tdc <- function(df_one_year) {
  xtabs(
    population ~ area.name + sex + age.char + `race.eth`,
    data = df_one_year,
    drop.unused.levels = FALSE
  )
}

#  Build one TDC series  ----------------------------

build_tdc_series <- function(series_id, paths = NULL) {
  
  specs <- tarr_series_specs()
  spec  <- specs[[series_id]]
  if (is.null(spec)) stop("Unknown series_id: ", series_id, call. = FALSE)
  
  # Labels contract
  ensure_labels_loaded_for_spec(spec, envir = .GlobalEnv)
  lbl <- validate_labels_for_spec(spec, envir = parent.frame())
  labels <- lbl$labels
  dim_lengths <- lbl$dim_lengths
  
  # Inputs
  tpaths <- tdc_input_paths(paths = paths)
  input_path <- tdc_input_for_series(series_id, tpaths)
  
  # Read + cast
  df <- read_tdc_parquet(input_path)
  df <- cast_tdc_to_canonical(df, labels = labels)
  
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
      input_source = "parquet"
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
    
    slab <- year_slab_4d_tdc(one)
    
    i <- match(y_chr, years_keep)
    if (is.na(i)) stop("Year '", y_chr, "' not found in label set for ", series_id, call. = FALSE)
    
    storage.mode(slab) <- "integer"
    
    # slab MUST be a 4D array (area.name × sex × age.char × race.eth)
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
  
  message("TDC ", series_id, ": done -> ", h5_path)
  invisible(h5_path)
}

#  Build TDC (entry point for build_all.R)  ----------------------------

build_tdc <- function(paths = NULL,
                      series_ids = c(
                        "tdc_estimates_county",
                        "tdc_projections_county"
                      )) {
  purrr::walk(series_ids, build_tdc_series, paths = paths)
  invisible(TRUE)
}
