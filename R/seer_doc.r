# ------------------------------------------------------------------------------------------------------------------->
# seer_doc.r
# Description:
#   Describes the SEER  population arrays in seer.
#
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: March 1, 2025
# Revised:
# -----------------------------------------------------------------------------------------

#' @name seer
#' 
#' @title SEER NCI Population
#' @description
#' Two SEER population estimate cubes are available in the package registry:
#' `seer_estimates_county_1y` and `seer_estimates_county_5y`. Both are sourced
#' from the
#' [National Cancer Institute Surveillance, Epidemiology, and End Results (SEER) population data](https://seer.cancer.gov/data-software/uspopulations.html).
#'
#' These cubes contain county-level Texas estimates by year, sex, age, race,
#' and ethnicity. Compared with Census Bureau estimates, the SEER cubes use the
#' SEER bridged race/ethnicity structure designed for epidemiologic rate work.
#'
#' The two registered cubes differ only in the age dimension:
#' * `seer_estimates_county_1y` uses single-year ages.
#' * `seer_estimates_county_5y` uses grouped age intervals.
#'
#' @format
#' The dimension names are `year`, `area.name`, `sex`, `age.char`, `race`, and
#' `ethnicity`.
#'
#' `dim_semantics` interpretation:
#' * `year` is an ordered interval-like time dimension with 34 levels
#'   (`1990` through `2023`).
#' * `area.name` is a 255-level Texas county partition.
#' * `sex` is a 2-level partition: `Female`, `Male`.
#' * `age.char` is an ordered interval domain:
#'   * `seer_estimates_county_1y` uses single ages `0` through `90`.
#'   * `seer_estimates_county_5y` uses grouped intervals `0`, `1-4`, `5-9`,
#'     `10-14`, ..., `80-84`, `85`.
#' * `race` is a 4-level partition:
#'   `White`, `Black`, `American Indian/Alaskan Native`,
#'   `Asian or Pacific Islander`.
#' * `ethnicity` is a 2-level partition: `Non-Hispanic`, `Hispanic`.
#'
#' Use [open_poparray()] to load one of the registered SEER cubes, then subset
#' lazily with [`[`] or [dplyr::filter()]. Tabular coercion with
#' [as.data.frame()] or [tibble::as_tibble()] is **EAGER** and should be used
#' after subsetting.
#'
#' @source Surveillance, Epidemiology, and End Results (SEER) Program
#'   Populations (1969-2023), National Cancer Institute, DCCPS, Surveillance
#'   Research Program.
#' @references [SEER U.S. population data](https://seer.cancer.gov/data-software/uspopulations.html)
NULL

