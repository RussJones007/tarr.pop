# series_spec.r
# Description:
#   What each dhf5Array series will have, define the names and labels for each dimension. 
#   Define the “contract” for every on-disk population cube:
#   - which HDF5 file + dataset holds it
#   - the dimension order (must match what open_tarr_pop() expects)
#   - which label-set keys should be used for each dimension
#   - chunking / compression intent
#   - whether the year dimension is extendable (for appending new years)
#
# Design rules:
#   * NO I/O in this file (no reading, no writing, no API calls).
#   * Specs should be declarative and stable — changes should be deliberate and versioned.
#   * Dim order for all 6D cubes is:
#       year, area.name, sex, age.char, race, ethnicity
#
# Used by:
#   - data-raw/build_all.R (orchestrator)
#   - data-raw/io_h5.R     (HDF5 writers)
#   - data-raw/validate.R  (QA checks)
#
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: Janury 3, 2025
# -----------------------------------------------------------------------------------------

# data-raw/series_specs.R
#
# Declarative contracts for each HDF5 cube.
# IMPORTANT: label_keys values must match objects saved in /data by labels_build.R

tarr_series_specs <- function() {
  dims_6d <- c("year", "area.name", "sex", "age.char", "race", "ethnicity")
  
  specs <- list(
    
    # ---------------- Census decennial (county, 1y ages) ----------------
    census_decennial_county_1y = list(
      schema = "cube_6d",
      series_id   = "census_decennial_county_1y",
      source_key  = "census",
      type_key    = "decennial",
      filename    = "census_decennial_county_1y.h5",
      dataset     = "/pop",
      geo         = "county",
      
      dims       = dims_6d,
      label_keys = list(
        year      = "years_census",
        area.name = "county_levels",
        sex       = "sex_levels_census",
        age.char  = "ages_census_1y",
        race      = "race_levels_census",
        ethnicity = "ethnicity_levels_census"
      ),
      
      storage_mode    = "integer",
      extendable_year = FALSE,
      chunk = c(year=1, area.name=64, sex=3, age.char=16, race=8, ethnicity=3),
      gzip_level = 6
    ),
    
    # ---------------- Census estimates (county, 5y ages) ----------------
    census_estimates_county_5y = list(
      schema = "cube_6d",
      series_id   = "census_estimates_county_5y",
      source_key  = "census",
      type_key    = "estimates",
      filename    = "census_estimates_county_5y.h5",
      dataset     = "/pop",
      geo         = "county",
      
      dims       = dims_6d,
      label_keys = list(
        year      = "years_census_estimates",
        area.name = "county_levels",
        sex       = "sex_levels_census",
        age.char  = "ages_census_5y",
        race      = "race_levels_census_estimates",
        ethnicity = "ethnicity_levels_census_estimates"
      ),
      
      storage_mode    = "integer",
      extendable_year = TRUE,
      chunk = c(year=1, area.name=64, sex=3, age.char=8, race=12, ethnicity=3),
      gzip_level = 6
    ),
    
    # ---------------- Census ZCTA estimates (simple cube) ----------------
    # NOTE: your dim_name_definitions uses end.year and zip.code; that’s a different shape
    # than your canonical 6D cube. Best practice is to represent this as its own dim contract.
    census_zcta_estimates = list(
      schema = "zcta_2d",
      series_id   = "census_zcta_estimates",
      source_key  = "census",
      type_key    = "zcta",
      filename    = "census_zcta_estimates.h5",
      dataset     = "/pop",
      geo         = "zcta",
      
      # This spec reflects the labels you actually have: end.year × zip.code
      dims       = c("end.year", "zip.code"),
      label_keys = list(
        `end.year` = "zcta_end_year_levels",
        `zip.code` = "zcta_levels"
      ),
      
      storage_mode    = "integer",
      extendable_year = TRUE, # extendable on end.year
      chunk = c(`end.year`=1, `zip.code`=256),
      gzip_level = 6
    ),
    
    # ---------------- SEER single-age (county, 1y ages) ----------------
    seer_estimates_county_1y = list(
      schema = "cube_6d",
      series_id   = "seer_estimates_county_1y",
      source_key  = "seer",
      type_key    = "single_age",
      filename    = "seer_estimates_county_1y.h5",
      dataset     = "/pop",
      geo         = "county",
      
      dims       = dims_6d,
      label_keys = list(
        year      = "years_seer",
        area.name = "county_levels",
        sex       = "sex_levels_seer",
        age.char  = "ages_seer_1y",
        race      = "race_levels_seer",
        ethnicity = "ethnicity_levels_seer"
      ),
      
      storage_mode    = "integer",
      extendable_year = TRUE,
      chunk = c(year=1, area.name=64, sex=2, age.char=16, race=4, ethnicity=2),
      gzip_level = 6
    ),
    
    # ---------------- SEER grouped-age (county, 5y ages-ish) ----------------
    seer_estimates_county_5y = list(
      schema      = "cube_6d",
      series_id   = "seer_estimates_county_5y",
      source_key  = "seer",
      type_key    = "grouped_age",
      filename    = "seer_estimates_county_5y.h5",
      dataset     = "/pop",
      geo         = "county",
      
      dims       = dims_6d,
      label_keys = list(
        year      = "years_seer",
        area.name = "county_levels",
        sex       = "sex_levels_seer",
        age.char  = "ages_seer_5y",
        race      = "race_levels_seer",
        ethnicity = "ethnicity_levels_seer"
      ),
      
      storage_mode    = "integer",
      extendable_year = TRUE,
      chunk = c(year=1, area.name=64, sex=2, age.char=8, race=4, ethnicity=2),
      gzip_level = 6
    ),
    
    # ---------------- TDC estimates (county) ----------------
    # IMPORTANT: TDC uses race.eth as a single dim in your labels script.
    # That means this is NOT the canonical 6D cube unless you split it during ETL.
    tdc_estimates_county = list(
      schema      = "tdc_raceeth_5d",
      series_id   = "tdc_estimates_county",
      source_key  = "tdc",
      type_key    = "estimates",
      filename    = "tdc_estimates_county.h5",
      dataset     = "/pop",
      geo         = "county",
      
      dims       = c("year", "area.name", "sex", "age.char", "race.eth"),
      label_keys = list(
        year      = "years_tdc_estimates",
        area.name = "county_levels",
        sex       = "sex_levels_tdc",
        age.char  = "ages_tdc_mixed",
        `race.eth`= "race_eth_levels_tdc_estimates"
      ),
      
      storage_mode    = "integer",
      extendable_year = TRUE,
      chunk = c(year=1, area.name=64, sex=3, age.char=16, `race.eth`=6),
      gzip_level = 6
    ),
    
    # ---------------- TDC projections (county) ----------------
    tdc_projections_county = list(
      schema      = "tdc_raceeth_5d",
      series_id   = "tdc_projections_county",
      source_key  = "tdc",
      type_key    = "projections",
      filename    = "tdc_projections_county.h5",
      dataset     = "/pop",
      geo         = "county",
      
      dims       = c("year", "area.name", "sex", "age.char", "race.eth"),
      label_keys = list(
        year       = "years_tdc_projections",
        area.name  = "county_levels",
        sex        = "sex_levels_tdc",
        age.char   = "ages_tdc_1y",
        `race.eth` = "race_eth_levels_tdc_projections"
      ),
      
      storage_mode    = "integer",
      extendable_year = FALSE,
      chunk = c(year=1, area.name=64, sex=3, age.char=16, `race.eth`=6),
      gzip_level = 6
    )
  )
  
  # Basic integrity checks
  ids <- vapply(specs, `[[`, character(1), "series_id")
  stopifnot(!anyDuplicated(ids))
  
  specs
}

tarr_population_list_from_specs <- function(specs = tarr_series_specs()) {
  by_source <- split(specs, vapply(specs, `[[`, character(1), "source_key"))
  lapply(by_source, function(src_specs) {
    setNames(
      vapply(src_specs, `[[`, character(1), "filename"),
      vapply(src_specs, `[[`, character(1), "type_key")
    )
  })
}

tarr_series_registry_from_specs <- function(specs = tarr_series_specs()) {
  data.frame(
    series_id       = vapply(specs, `[[`, character(1), "series_id"),
    filename        = vapply(specs, `[[`, character(1), "filename"),
    dataset         = vapply(specs, `[[`, character(1), "dataset"),
    geo             = vapply(specs, `[[`, character(1), "geo"),
    source_key      = vapply(specs, `[[`, character(1), "source_key"),
    type_key        = vapply(specs, `[[`, character(1), "type_key"),
    dim_order       = vapply(specs, function(x) paste(x$dims, collapse=","), character(1)),
    extendable_year = vapply(specs, `[[`, logical(1), "extendable_year"),
    stringsAsFactors = FALSE
  )
}
