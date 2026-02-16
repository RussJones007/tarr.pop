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
# ------------------------------------------------------------------------------------------------------------------->
#
# Population table/array documentation ====
#' Population Tables/Arrays
#'
#' The `population` object is a named catalog of population cubes grouped by
#' source agency and table type. Each entry points to a delayed,
#' disk-backed cube that can be opened as a [poparray] with [open_poparray()]
#' (or the legacy alias [open_tarr_pop()]).
#'
#' A `poparray` wraps a `DelayedArray`/`HDF5Array` backend and stores explicit
#' dimension labels and role metadata (`time`, `area`, optional `strata`).
#' This allows filtering and aggregation before realization, reducing memory use.
#'
#' Available series include decennial census counts, annual estimates, and
#' projections from:
#' * [U.S. Census Bureau](https://www.census.gov/)
#' * [Texas Demographic Center](https://demographics.texas.gov/)
#' * National Cancer Institute SEER program
#'
#' The `zcta` table differs from county ASRE cubes and is focused on ZCTA-level
#' totals for Tarrant County.
#'
#' Common workflow:
#' * open a cube: [open_poparray()]
#' * subset by labels: [`[`], [dplyr::filter()]
#' * collapse/group dimensions: [collapse_dim()], [split()], [by()]
#' * coerce to tabular output: [as.data.frame()], [tibble::as_tibble()]
#'
#' Dimension labels and categories differ by source. The package standardizes
#' names where possible, but category sets may still vary (especially
#' race/ethnicity and age group structure).
#'
#' Notes:
#' * census-estimate vintages are revised over time; historical year values may
#'   change across releases.
#' * use accessors such as [years()], [areas()], [ages()], [races()],
#'   [ethnicities()] to inspect currently available labels in a loaded cube.
#'
#' @format
#' Typical county-level ASRE cubes include dimensions:
#' * `year` (time role)
#' * `area.name` (area role)
#' * optional strata such as `sex`, `age.char`, `race`, `ethnicity`
#'
#' Some tables may use a reduced schema (for example, `zcta` without ASRE
#' stratification). Use [dimnames()], [names()], and role helpers
#' [time_role()] / [area_role()] on the loaded `poparray` to inspect the exact
#' structure.
"population"
