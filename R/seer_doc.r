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
#'  Two data arrays of population estimates plus an array of standard population for use in adjustment. Sourced from the
#' [National Cancer Institute; Surveillance, Epidemiology, and End Results program](https://seer.cancer.gov/data-software/uspopulations.html).
#' The arrays contain intercensal estimates of age/age group, sex, race, and ethnicity (ASRE) population figures by
#' year,and state/county. These estimates differ from the Census Bureau estimates primarily by using bridged race and
#' ethnicity categories matching those from 1990. County level population estimates are produced in collaboration
#' with the U.S. Census Bureau and the National Center for Health Statistics, with some years contracted out to private
#' firms. The two population estimate
#' arrays are:
#' * "seer_grouped_age" uses 20 age groups
#' * "seer_single_age" uses single year age groups
#' * "population_std" is actually a list of tables with standard populations from different decades that may be used
#'    for adjustment.
#'
#' @format
#' The dimension names are: year, area.name, sex, age.char, race, and ethnicity.
#' Index selection from each dimension returns a subset array. An easier means of selecting the desired data is
#' to use the [dplyr::filter()] function as it simplifies making a query, performs some error checks, and then
#' returns the data. The group_ages() function can be used to collapse ages into custom groupings.
#'
#' The dimensions and labels of each array are expressed as character strings.
#' The dimensions and values are:
#'
#'  * year:  from
#'  * area.name: All of the counties in Texas.
#'  * age.char:
#'       * seer_single_age has single ages 0 through 89 and 90+
#'       * seer_grouped_age age groups 0, 1-4, 5-9, 10-14 ...80-84, and 85+
#'  * sex:  Female ,  Male
#'  * race: bridged categories that are American Indian/Alaskan Native, Asian or Pacific Islander, Black, and White.
#'  * ethnicity has the values: Hispanic, or Non-Hispanic.
#'  * population is the integer value of the table/array.
#'
#'  @source Surveillance, Epidemiology, and End Results (SEER) Program Populations (1969-2023)
#'  National Cancer Institute, DCCPS, Surveillance Research Program, released March 2025
#'  @reference [NCI SEER data](www.seer.cancer.gov/popdata),
NULL

