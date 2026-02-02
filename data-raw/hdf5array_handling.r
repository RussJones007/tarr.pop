# ------------------------------------------------------------------------------------------------------------------->
# hdf5array_handdling.r
#
# Function to handle conversion from a a normal R array to a hdf5Array and saved to disk
# This script assumes you have the following .rda files and objects:
#   data/census.rda              -> census
#   data/census.estimates.rda    -> census.estimates
#   data/census.zcta.estimates.rda -> census.zcta.estimates
#   data/tdc.estimates.rda       -> tdc.estimates
#   data/tdc.projections.rda     -> tdc.projections
#   data/seer_single_age.rda     -> seer_single_age
#   data/seer_grouped_age.rda    -> seer_grouped_age
#
# ------------------------------------------------------------------------------------------------------------------->
# By Russ Jones, ChatGPT skeleton
# Created: November 18, 2025
# ------------------------------------------------------------------------------------------------------------------->
library(HDF5Array)
library("rhdf5")


# Function to handle conversion

#' Write a population cube (R array) to an HDF5 file in inst/extdata
#'
#' This is a build-time utility: run it from the package source tree to convert
#' existing R arrays into HDF5 files suitable for HDF5Array-backed tarr_pop
#' objects.
#'
#' @param x         Numeric array (your population cube). Dimnames are preserved.
#' @param file_stub Base name (without extension) for the .h5 file, e.g. "tdc_1y".
#' @param pkg_dir   Path to the package root (defaults to current working dir).
#' @param dataset   Name of the dataset inside the HDF5 file (default "/pop").
#' @param chunk_margins Optional vector of chunk sizes per dimension. If NULL,
#'   the full dim(x) is used (one chunk = whole array). Typically you want
#'   smaller chunks, e.g. c(20, 2, 5, 2, 16, 3) for a 6D cube.
#' @param compression_level Gzip compression level (0–9). 6 is a good default.
#@param overwrite Logical; if TRUE, overwrite existing file.
#' @param attrs     Optional named list of attributes to store on the dataset
#'   (e.g. list(source = "TDC", age_res = "1y")).
#'
#' @return Invisibly, the full path to the written .h5 file.
#'
write_pop_cube_h5 <- function(x,
                              file_stub,
                              pkg_dir = ".",
                              dataset = "/pop",
                              chunk_margins = NULL,
                              compression_level = 6L,
                              overwrite = FALSE,
                              attrs = NULL) {
  stopifnot(is.array(x), is.numeric(x))
  stopifnot(length(file_stub) == 1L, nzchar(file_stub))

  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("Package 'rhdf5' is required for write_pop_cube_h5(). Please install it.")
  }

  # convert from tarr_pop to an integer array
  if( inherits(x, what = "tarr_pop")) x <- array(as.integer(x), dim = dim(x))

  # a) Ensure inst/extdata exists ------------------------------------------------
  extdata_dir <- file.path(pkg_dir, "inst", "extdata")
  if (!dir.exists(extdata_dir)) {
    dir.create(extdata_dir, recursive = TRUE)
  }

  h5_path <- file.path(extdata_dir, paste0(file_stub, ".h5"))

  if (file.exists(h5_path)) {
    if (!isTRUE(overwrite)) {
      stop("File already exists: ", h5_path,
           " (set overwrite = TRUE to replace it).")
    } else {
      message("Overwriting existing file: ", h5_path)
      unlink(h5_path)
    }
  }

  # b) Choose chunk dimensions ---------------------------------------------------
  d <- dim(x)

  if (is.null(chunk_margins)) {
    if (length(d) == 6L) {
      # dim order: year, county, age, sex, race, ethnicity
      target <- c(
        year      = 5,
        county    = 32,
        age       = 20,
        sex       = 2,
        race      = 3,
        ethnicity = 2
      )
      if (!is.null(names(d))) {
        target <- target[names(d)]
      }
      chunkdim <- pmin(d, target)
    } else {
      # generic fallback
      chunkdim <- pmin(d, 16L)
    }
  } else {
    stopifnot(length(chunk_margins) == length(d))
    chunkdim <- pmin(d, as.integer(chunk_margins))
  }

  chunkdim <- as.integer(chunkdim)

  message("Writing HDF5 file: ", h5_path)
  message("  dims:      ", paste(d, collapse = " x "))
  message("  chunkdim:  ", paste(chunkdim, collapse = " x "))
  message("  compress:  level ", compression_level)

  # c) Create file & dataset -----------------------------------------------------
  rhdf5::h5createFile(h5_path)

  storage_mode <- if (is.integer(x)) "integer" else "double"

  rhdf5::h5createDataset(
    file         = h5_path,
    dataset      = dataset,
    dims         = d,
    storage.mode = storage_mode,
    chunk        = chunkdim,
    level        = as.integer(compression_level)
  )

  # d) Write the entire array in one go -----------------------------------------
  # This is a straight C-level write, no DelayedArray/recursion involved.
  rhdf5::h5write(x, file = h5_path, name = dataset)

  # e) Optional: write attributes -----------------------------------------------
  if (!is.null(attrs)) {
    for (nm in names(attrs)) {
      rhdf5::h5writeAttribute(
        attr     = attrs[[nm]],
        h5obj    = h5_path,
        h5loc    = "/pop",
        name     = nm
      )
    }
  }

  # f) Close open HDF5 handles just to be safe ----------------------------------
  rhdf5::H5close()

  invisible(h5_path)
}

# Helper for common chunking of 6D cubes (year, area.nme, sex, age, race, ethnicity)
chunk6d <- function(x) {
  if (length(dim(x)) == 6L) {
    # heuristic: in year, area, age, sex,race, ethnicity
    return(c(5, 32, 16, 2, 3, 2))
  }
  # fallback
  rep(16L, length(dim(x)))
}


# 6) Census decennial: census.rda -> census_decennial_county_1y.h5 ----
load("data/census.rda")            # loads object 'census'

write_pop_cube_h5(
  x            = census,
  file_stub    = "census_decennial_county_1y",
  chunk_margins = chunk6d(census),
  attrs        = list(
    source     = "US Census Bureau",
    series_id  = "census_decennial_county_1y",
    product    = "decennial",
    geography  = "county",
    age_scheme = "1y_with_tail_groups",
    years      = "2000, 2010, 2020",
    note       = "Decennial census counts; single-year ages 0–99 plus 100–104, 105–109, 110+, and All."
  ),
  compression_level = 6,
  overwrite    = TRUE
)

rm(census)


# 7) Census estimates (county, 5-year): census.estimates.rda ----
#    -> census_estimates_county_5y.h5

load("data/census.estimates.rda")  # loads 'census.estimates'

write_pop_cube_h5(
  x            = census.estimates,
  file_stub    = "census_estimates_county_5y",
  chunk_margins = chunk6d(census.estimates),
  attrs        = list(
    source     = "US Census Bureau",
    series_id  = "census_estimates_county_5y",
    product    = "estimates",
    geography  = "county",
    age_scheme = "5y_0-4_to_80-84_85+_All",
    years      = "2001–2023",
    note       = "Annual postcensal estimates; 5-year age groups 0–4 to 80–84, 85+, and All."
  ),
  overwrite    = TRUE
)

rm("census.estimates")
# 8) Census ZCTA estimates: census.zcta.estimates.rda ----
#    -> census_estimates_zcta_5y.h5  (adjust age_scheme if needed)
load("data/census.zcta.estimates.rda")  # loads 'census.zcta.estimates'

write_pop_cube_h5(
  x            = census.zcta.estimates,
  file_stub    = "census_estimates_zcta_5y",
  # if different dim structure, chunk6d() still gives a reasonable fallback
  chunk_margins = rep(16L, length(dim(census.zcta.estimates))),
  attrs        = list(
    source     = "US Census Bureau",
    series_id  = "census_estimates_zcta_5y",
    product    = "estimates",
    geography  = "zcta",
    age_scheme = "5y_groups",   # refine if needed
    years      = "2001–2023",
    note       = "Annual postcensal estimates at ZCTA level; 5-year age groups."
  ),
  overwrite    = TRUE
)

rm(census.zcta.estimates)

# 9) TDC estimates: tdc.estimates.rda ----
#    -> tdc_estimates_county_mixed.h5
load("data/tdc.estimates.rda")     # loads 'tdc.estimates'

write_pop_cube_h5(
  x            = tdc.estimates,
  file_stub    = "tdc_estimates_county_mixed",
  chunk_margins = chunk6d(tdc.estimates),
  attrs        = list(
    source     = "Texas Demographic Center",
    series_id  = "tdc_estimates_county_mixed",
    product    = "estimates",
    geography  = "county",
    age_scheme = "mixed_1y_5y_special",
    years      = "2001–2023",
    note       = paste(
      "Single ages 0–95 plus 85+, 95+, and All; also 5-year groups 0–4 to 80–84,",
      "and special spans 15–44 and 15–49."
    )
  ),
  overwrite    = TRUE
)

rm(tdc.estimates)

# 10) TDC projections: tdc.projections.rda ----
#    -> tdc_projections_county_1y.h5
load("data/tdc.projections.rda")   # loads 'tdc.projections'

write_pop_cube_h5(
  x            = tdc.projections,
  file_stub    = "tdc_projections_county_1y",
  chunk_margins = chunk6d(tdc.projections),
  attrs        = list(
    source     = "Texas Demographic Center",
    series_id  = "tdc_projections_county_1y",
    product    = "projections",
    geography  = "county",
    age_scheme = "1y_with_95+_All",
    years      = "2010–2050",
    note       = "Population projections; single-year ages 0–94, 95+, and All."
  ),
  overwrite    = TRUE
)

rm(tdc.projections)

# 11) SEER single-age: seer_single_age.rda ----
#    -> seer_estimates_county_1y.h5
load("data/seer_single_age.rda")   # loads 'seer_single_age'

write_pop_cube_h5(
  x            = seer_single_age,
  file_stub    = "seer_estimates_county_1y",
  chunk_margins = chunk6d(seer_single_age),
  attrs        = list(
    source     = "National Cancer Institute; SEER Program",
    series_id  = "seer_estimates_county_1y",
    product    = "estimates",
    geography  = "county",
    age_scheme = "1y_0-89_90+",
    years      = "2000–2024",
    note       = "SEER population estimates; single ages 0–89 and 90+."
  ),
  overwrite    = TRUE
)

rm(seer_single_age)

# 7) SEER grouped-age: seer_grouped_age.rda ----
#    -> seer_estimates_county_5y.h5
load("data/seer_grouped_age.rda")  # loads 'seer_grouped_age'

write_pop_cube_h5(
  x            = seer_grouped_age,
  file_stub    = "seer_estimates_county_5y",
  chunk_margins = chunk6d(seer_grouped_age),
  attrs        = list(
    source     = "National Cancer Institute; SEER Program",
    series_id  = "seer_estimates_county_5y",
    product    = "estimates",
    geography  = "county",
    age_scheme = "5y_0_1-4_..._85+",
    years      = "2000–2024",
    note       = "SEER population estimates; age groups 0, 1–4, 5–9, …, 80–84, 85+."
  ),
  overwrite    = TRUE
)

rm(seer_grouped_age)


message("All HDF5 files written to inst/extdata/")

