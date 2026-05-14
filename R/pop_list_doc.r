# ------------------------------------------------------------------------------------------------------------------->
# Script:  pop_list_doc.r
# Description:
#   Describes the population arrays
#
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: Feb 7, 2025
# Revised: June 5, 2025 - updated to the population list of functions for getting the population tables.
# Revised: August 5, 2025 - updated using census estimates through 2024.  Cleaned language for clarity
# Further revisions updates are found in git and pushed to github.
# ------------------------------------------------------------------------------------------------------------------->
#
# Population table/array documentation ====
#' Population Tables/Arrays
#'
#' `population` is a list containing a named catalog of the population cubes currently available. 
#' Entries are grouped by source agency and table type. Each entry stores a `series_id` string is basically the cube
#' file name minus the extension. The series name is passed to the [open_poparray()] function and a [poparray] object 
#' is returned.
#'
#' A `poparray` extends `DelayedArray` (typically backed by an HDF5 file) and stores explicit dimension labels, and
#' roles as part of the  dimension  metadata. See [DimSemantics] for how each dimension is described. The meta data is
#' used to enable safe  filtering and aggregation before realization, reducing memory use and helping guard against
#' unsafe aggregation when non-exclusive categories are present.
#'
#' The available cubes are sourced from :
#' * [U.S. Census Bureau](https://www.census.gov/)
#' * [Texas Demographic Center](https://demographics.texas.gov/)
#' * [National Cancer Institute SEER Program](https://seer.cancer.gov/data-software/uspopulations.html)
#'
#' Most cubes use are at the county-level wit demographic dimensions liske  Age, Sex, Race, and  Ethnicity (**ASRE**).
#' The `census_zcta_estimates` cube is different: it contains ZCTA-level totals focused on Tarrant County- geography
#' rather than ASRE county slices.
#'
#' Common workflow:
#' * open a cube: [open_poparray()]
#' * subset by labels: [`[`] or [dplyr::filter()]
#' * collapse/group dimensions: [collapse_dim()], [split()], [by()]
#' * coerce to tabular output: [as.data.frame()] or [tibble::as_tibble()] (**EAGER**)
#'
#' Dimension labels and categories differ by source. The package standardizes
#' names where possible, but label/category sets still vary, especially for age and
#' race/ethnicity dimensions.
#'
#' Notes:
#' * census-estimate vintages are revised over time; historical year values may change across releases.
#' * use accessors such as [years()], [areas()], [ages()], [races()],
#' [ethnicities()] to inspect currently available labels in a loaded cube.
#' * use [dim_semantics()] to inspect whether a dimension behaves like a
#' partition, set, or interval domain before aggregating.
#' 
#' @format A named list with source-grouped entries:
#' * `population$texas.demographic.center$estimates` = `"tdc_estimates_county"`
#' * `population$texas.demographic.center$projections` = `"tdc_projections_county"`
#' * `population$census.bureau$census` = `"census_decennial_county_1y"`
#' * `population$census.bureau$estimates` = `"census_estimates_county_5y"`
#' * `population$census.bureau$zcta` = `"census_zcta_estimates"`
#' * `population$seer$single_age` = `"seer_estimates_county_1y"`
#' * `population$seer$grouped_age` = `"seer_estimates_county_5y"`
#'
#' Cube summary by current registry:
#'
#' * `census_decennial_county_1y`
#'   Source: [U.S. Census Bureau decennial census](https://www.census.gov/programs-surveys/decennial-census.html).
#'   Dimensions: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`.
#'   The `year` dimension is a 3-level ordered interval-like time axis
#'   (`2000`, `2010`, `2020`). `area.name` is a 255-level county partition for
#'   Texas. `sex` is a 2-level partition (`Female`, `Male`). `age.char` is a
#'   single-year age axis summarized over many levels, so it should be treated
#'   as an ordered interval domain. `race` is a 7-level set-like dimension with
#'   categories such as `White`, `Black`, and `Two Or More`; combinations can
#'   make aggregation assumptions important. `ethnicity` is a 2-level partition
#'   (`Hispanic`, `Non-Hispanic`).
#'
#' * `census_estimates_county_5y`
#'   Source: [U.S. Census Bureau county characteristics datasets](https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-detail.html).
#'   Dimensions: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`.
#'   `year` has 15 levels (`2010` through `2024`) and is an ordered interval-like
#'   time axis. `area.name` is the 255-county Texas partition. `sex` is a
#'   2-level partition. `age.char` is a partition of 18 grouped age intervals
#'   (`0-4` through `85 +`). `race` is an 11-level set-like dimension that
#'   includes bridged and "or in combination" categories. `ethnicity` is a
#'   2-level partition (`Hispanic`, `Non-Hispanic`).
#'
#' * `census_zcta_estimates`
#'   Source: [U.S. Census Bureau American Community Survey](https://www.census.gov/programs-surveys/acs).
#'   Dimensions: `end.year`, `zip.code`.
#'   `end.year` has 13 levels (`2011` through `2023`) and is an ordered
#'   interval-like time axis representing estimate vintages. `zip.code` is a
#'   91-level ZCTA partition. This cube contains totals rather than ASRE strata.
#'
#' * `seer_estimates_county_1y`
#'   Source: [SEER U.S. population data](https://seer.cancer.gov/data-software/uspopulations.html).
#'   Dimensions: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`.
#'   `year` has 34 levels (`1990` through `2023`) and is an ordered interval-like
#'   time axis. `area.name` is the 255-county Texas partition. `sex` is a
#'   2-level partition (`Female`, `Male`). `age.char` is a single-year ordered
#'   interval domain (ages `0` through `90`). `race` is a 4-level partition
#'   (`White`, `Black`, `American Indian/Alaskan Native`,
#'   `Asian or Pacific Islander`). `ethnicity` is a 2-level partition
#'   (`Non-Hispanic`, `Hispanic`).
#'
#' * `seer_estimates_county_5y`
#'   Source: [SEER U.S. population data](https://seer.cancer.gov/data-software/uspopulations.html).
#'   Dimensions: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`.
#'   This cube shares the same year, county, sex, race, and ethnicity semantics
#'   as `seer_estimates_county_1y`. The difference is `age.char`, which is a
#'   19-level partition of grouped age intervals (`0`, `1-4`, `5-9`, ..., `80-84`,
#'   `85`).
#'
#' * `tdc_estimates_county`
#'   Source: [Texas Demographic Center population estimates](https://demographics.texas.gov/Resources/TPEPP/Estimates/).
#'   Dimensions: `year`, `area.name`, `sex`, `age.char`, `race.eth`.
#'   `year` has 11 levels (`2011` through `2022`, with no `2020` level in the
#'   current registry) and is an ordered interval-like time axis. `area.name` is
#'   the 255-county Texas partition. `sex` is a 2-level partition. `age.char`
#'   is an ordered age domain that mixes single ages with top-coded intervals
#'   such as `85 +` and `95 +`, so it should be treated as an interval domain.
#'   `race.eth` is a 5-level partition (`asian`, `black`, `hispanic`, `other`,
#'   `white`) combining race and ethnicity into one dimension.
#'
#' * `tdc_projections_county`
#'   Source: [Texas Demographic Center population projections](https://demographics.texas.gov/Resources/TPEPP/Projections/).
#'   Dimensions: `year`, `area.name`, `sex`, `age.char`, `race.eth`.
#'   `year` has 41 levels (`2010` through `2050`) and is an ordered interval-like
#'   time axis. `area.name` is the 255-county Texas partition. `sex` is a
#'   2-level partition. `age.char` is a single-year ordered interval domain with
#'   an infant `< 1` label and a top-coded `95 +` label. `race.eth` is the same
#'   5-level partition used by the TDC estimates cube.
"population"
