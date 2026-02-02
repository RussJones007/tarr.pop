# ------------------------------------------------------------------------------------------------------------------->
# Script: build_all.r
# Description:
#   Calls the other scrits in dara-raw/ to ceate the population HDF5Arrays and write them to disk
#
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 3, 2026


#  Resolve paths  ----------------------------

resolve_paths <- function() {
  # 1) already in the session?
  if (exists("paths", inherits = TRUE)) {
    return(get("paths", inherits = TRUE))
  }
  
  # 2) saved in package data/ as paths.rda?
  if (file.exists("data/paths.rda")) {
    e <- new.env(parent = emptyenv())
    load("data/paths.rda", envir = e)
    if (exists("paths", envir = e, inherits = FALSE)) return(e$paths)
  }
  
  # 3) optional: derive from tarr, if available
  if (requireNamespace("tarr", quietly = TRUE) &&
      exists("paths_defined", asNamespace("tarr"), inherits = FALSE)) {
    return(tarr::paths_defined())
  }
  
  # 4) no paths available
  NULL
}

maybe_paths <- resolve_paths()

#  Load specs + labels  ----------------------------
source("data-raw/series_spec.R")
source("data-raw/labels_registry.R")
source("data-raw/labels_build.R")

labels_init(build_labels())

#  Load I/O + validation  ----------------------------
source("data-raw/io_h5.R")
source("data-raw/validate.R")

#  Load builders  ----------------------------
source("data-raw/build_seer.R")
source("data-raw/build_census.R")
source("data-raw/build_tdc.R")


#  Build all cubes  ----------------------------
build_seer(paths   = maybe_paths)
undebug(h5_write_slab)
undebug(build_census_series)
build_census(paths = maybe_paths)
build_tdc(paths    = maybe_paths)


# population list variable
population <- list(
  "texas.demogrpahic.center" = list(
    "estimates"   = "tdc_estimates_county_mixed",
    "projections" = "tdc_projections_county_1y"
  ),
  "census.bureau" = list(
    "census"    = "census_decennial_county_1y",
    "estimates" = "census_estimates_county_5y",
    "zcta"      = "census_estimates_zcta_5y"
  ),
  "seer"  = list(
    "single_age" = "seer_estimates_county_1y",
    "grouped_age"= "seer_estimates_county_5y"
  )
)

usethis::use_data(population, overwrite = TRUE)


#  Validate  ----------------------------
#validate_all_h5()
rm(list = ls(all.names = TRUE))
