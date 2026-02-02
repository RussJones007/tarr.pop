# ------------------------------------------------------------------------------------------------------------------->
# Script: open_tarr_pop.r
#
# Assumptions:
#   * HDF5 files live in inst/extdata/
#   * Main dataset inside each file is "/pop"
#   * dim order in all cubes is: year, county, age.char, sex, race, ethnicity
#   * Canonical label vectors live in the package as small R objects and are lazily loaded
#
# Description:
#  Used when opening a tarr_pop to give the appropriate dimension names and labels.
#  1. The tarr_series_registry is a data frame of cubes and associated file names.
#  2. The Label getters section is where each dimension name is further defined with labels
#  3. The Build dimnames handles building the names and labels for a population series
#  4. Validation of names against open cubes ensures the number of dimensions and label match
#  5. open_tarr_pop() is the function opens a cube and sets up the dimension names
#
#   Many of the functions were written with ChatGPT R Wizard assistance which saved time in coding.
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones with AI assistance
# Created: December 27, 2025
# ------------------------------------------------------------------------------------------------------------------->

# 1. Series registry ----------------------------------------------------

# Map "series_id" to filename and label-set keys.
tarr_series_registry <- function() {
  data.frame(
    series_id = c(
      "census_decennial_county_1y",
      "census_estimates_county_5y",
      "census_estimates_zcta_5y",
      "tdc_estimates_county_mixed",
      "tdc_projections_county_1y",
      "seer_estimates_county_1y",
      "seer_estimates_county_5y"
    ),
    filename = c(
      "census_decennial_county_1y.h5",
      "census_estimates_county_5y.h5",
      "census_estimates_zcta_5y.h5",
      "tdc_estimates_county_mixed.h5",
      "tdc_projections_county_1y.h5",
      "seer_estimates_county_1y.h5",
      "seer_estimates_county_5y.h5"
    ),
    geo = c("county", "county", "zcta", "county", "county", "county", "county"),
    age_scheme = c("census_1y", "census_5y", "census_5y", "tdc_mixed", "tdc_1y", "seer_1y", "seer_5y"),
    year_scheme = c("census_decennial", "census_est_2001_2023", "census_est_2001_2023",
                    "tdc_est_2001_2023", "tdc_proj_2010_2050", "seer_2000_2024", "seer_2000_2024"),
    stringsAsFactors = FALSE
  )
}

# 2. Label getters (package-level) ----------------------------

# Years: keep these as small package objects or compute deterministically.
years_for <- function(year_scheme) {
  switch(
    year_scheme,
    census_decennial      = c("2000", "2010", "2020"),
    census_est_2001_2023  = as.character(2010:2024),
    tdc_est_2001_2023     = as.character(2001:2023),
    tdc_proj_2010_2050    = as.character(2010:2050),
    seer_2000_2024        = as.character(2000:2024),
    stop("Unknown year_scheme: ", year_scheme)
  )
}

# Geography labels:
county_levels <- function() {
  # Example: use county names stored as names(county_fips)
  # Replace with your actual canonical vector.
  if (exists("county_fips", envir = parent.frame(), inherits = TRUE)) {
    return(names(get("county_fips", envir = parent.frame(), inherits = TRUE)))
  }
  stop("county_fips not found; implement county_levels() to return your canonical county labels.")
}

zcta_levels <- function() {
  # Replace with your package object if you have one:
  if (exists("zcta_levels", envir = parent.frame(), inherits = TRUE)) {
    return(get("zcta_levels", envir = parent.frame(), inherits = TRUE))
  }
  stop("Implement zcta_levels() to return your canonical ZCTA labels.")
}

# Age label sets: implement these using your existing age label objects.
age_levels_for <- function(age_scheme) {
  switch(
    age_scheme,
    census_1y  = get("ages_census_1y",  envir = parent.frame(), inherits = TRUE),
    census_5y  = get("ages_census_5y",  envir = parent.frame(), inherits = TRUE),
    tdc_1y     = get("ages_tdc_1y",     envir = parent.frame(), inherits = TRUE),
    tdc_mixed  = get("ages_tdc_mixed",  envir = parent.frame(), inherits = TRUE),
    seer_1y    = get("ages_seer_1y",    envir = parent.frame(), inherits = TRUE),
    seer_5y    = get("ages_seer_5y",    envir = parent.frame(), inherits = TRUE),
    stop("Unknown age_scheme: ", age_scheme)
  )
}

# Sex/race/ethnicity label sets can vary by series (esp. race/eth in your docs).
sex_levels_for <- function(series_id) {
  # Most datasets allow Female/Male/All; SEER has Female/Male only.
  if (grepl("^seer_", series_id)) return(c("Female", "Male"))
  c("Female", "Male")
}

race_levels_for <- function(series_id) {
  if (grepl("^seer_", series_id)) {
    # bridged categories per your docs
    return(c("American Indian/Alaskan Native", "Asian or Pacific Islander", "Black", "White"))
  }
  if (grepl("^tdc_", series_id)) {
    return(c("Asian", "Black", "Other", "White", "All")) # include "All" if your cube includes it
  }
  if (grepl("^census_estimates_", series_id)) {
    # include the "or in combination" set if that’s how your cube is labeled
    return(get("race_levels_census_estimates", envir = parent.frame(), inherits = TRUE))
  }
  # census decennial
  get("race_levels_census", envir = parent.frame(), inherits = TRUE)
}

ethnicity_levels_for <- function(series_id) {
  if (grepl("^seer_", series_id)) return(c("Hispanic", "Non-Hispanic"))
  c("Hispanic", "Non-Hispanic")
}

# Source metadata (whatever your existing get_source()/print method expects)
source_for <- function(series_id) {
  if (startsWith(series_id, "census_")) {
    return(list(source = "US Census Bureau", note = series_id))
  }
  if (startsWith(series_id, "tdc_")) {
    return(list(source = "Texas Demographic Center", note = series_id))
  }
  if (startsWith(series_id, "seer_")) {
    return(list(source = "National Cancer Institute; SEER Program", note = series_id))
  }
  list(source = "unknown", note = series_id)
}

# 3. Build dimnames for a given series ----------------------------------

labels_for_series <- function(series_id) {
  reg <- tarr_series_registry()
  row <- reg[reg$series_id == series_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown series_id: ", series_id)

  year <- years_for(row$year_scheme)
  geo  <- if (row$geo == "county") county_levels() else zcta_levels()
  age  <- age_levels_for(row$age_scheme)

  sex       <- sex_levels_for(series_id)
  race      <- race_levels_for(series_id)
  ethnicity <- ethnicity_levels_for(series_id)

  # IMPORTANT: dim order year, county/zcta, sex, age.char, race, ethnicity
  list(
    year      = year,
    area.name = geo,
    sex       = sex,
    age.char  = age,
    race      = race,
    ethnicity = ethnicity
  )
}

# 4. Validation helpers --------------------------------------------------

validate_labels_against_cube <- function(h5_handle, dimn, series_id) {
  d <- dim(h5_handle)

  if (length(d) != 6L) {
    stop("Expected 6D cube for series '", series_id, "'. Got: ", length(d), "D.")
  }

  # Match expected order: year, area.name, sex, age.char, race, ethnicity
  expected <- c("year", "area.name", "sex", "age.char", "race", "ethnicity")
  if (!all(expected %in% names(dimn))) {
    stop("dimn must contain names: ", paste(expected, collapse = ", "))
  }

  lens <- vapply(dimn[expected], length, integer(1))
  if (!all(lens == d)) {
    msg <- paste0(
      "Dimension length mismatch for series '", series_id, "':\n",
      "  cube dim():      ", paste(d, collapse = " x "), "\n",
      "  label lengths:   ", paste(lens, collapse = " x "), "\n",
      "  order:           ", paste(expected, collapse = ", ")
    )
    stop(msg)
  }

  invisible(TRUE)
}

# 5. Opener: HDF5 -> HDF5Array -> tarr_pop ------------------------------

#' Open population series
#'
#' Using the name of a population series that exists on disk,  open it and retunr as a tarr_pop object.
#'
#' @param series_id name of the population series.  See the population list variable for easier selection in the IDE.
#' @param dataset path to the data in the HDF5Array file
#' @param data_col name of the column with the population figures when array is transoformed into a data frame.
#'
#' @returns the selected population series as a tarr_pop object
#' @export
#' @examples
#' #TBD
open_tarr_pop <- function(series_id,
                          dataset = "/pop",
                          data_col = "population") {
  reg <- tarr_series_registry()
  row <- reg[reg$series_id == series_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown series_id: ", series_id)

  path <- system.file("extdata", row$filename, package = utils::packageName())
  if (!nzchar(path)) {
    stop("HDF5 file not found for series '", series_id, "'. Expected in inst/extdata/: ", row$filename)
  }

  h5 <- HDF5Array::HDF5Array(filepath = path, name = dataset)

  dimn <- labels_for_series(series_id)
  validate_labels_against_cube(h5, dimn, series_id)

  # Attach dimnames onto the handle (cheap; does not read the whole dataset)
  dimnames(h5) <- dimn

  new_tarr_pop(
    x             = h5,
    dimnames_list = dimn,
    data_col      = data_col,
    source        = source_for(series_id)
  )
}
