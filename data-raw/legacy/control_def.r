# =============================================================================>
# control_def.r
# Calls package definition scripts:
# - paths_def.r to set up usual paths used in epi analysis
# - pop_def.r for Texas Demographic Center population data
# - census_data.r fo processing census bureau population data
# - seer_def.r to process National Cancer Institute population data
#
# Created Feb 1, 2025
# R Jones
# =============================================================================>
library(magrittr)
library(tidyverse)
library(readxl)
library(tidycensus)
library(janitor)
library(nanoparquet)
library(ivs)
library(janitor)
library(data.table)
library(tarr)

rm(list = ls())

# need paths defines globally for some functions to work.
# source( "R/Paths.r")
devtools::load_all()

paths <- tarr::paths_defined()

# convenience function for sorting values for levels when making a factor
sort_values <- compose(sort, unique)

# check files are in location expected relative to the package home directory
fls <- c(
  "data-raw/pop_def.r",
  "data-raw/census_data.r",
  "data-raw/seer_def.r"
)

if (all(file.exists(fls))) {
  # call each script
  purrr::walk(fls, ~ {
    source(file = .x)
    print(paste("Finished", .x))
  })
} else {
  stop(paste("one of the defintion files does not exists in the location
             expected.Current folder is", getwd()))
}

rm(fls)

# population
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
rm(list = ls())
