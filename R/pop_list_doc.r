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
#' The 'population' variable is a named list of population series available grouped by agency, then type. Each
#' population series is a large multidimensional array.  The names in the 'population' variable can be used in the
#' open_tarr_pop() function to retrieve an object of "tarr_pop" to get the population data as a tarr_pop object. The
#' tarr_pop class is a wrapper around HDF5Array that enables efficient operations in filtering and summarizing of data
#' before actually retrieving the data from disk - saving memory.
#'
#' Population series come from different sources and can have different dimension values. Population figures include
#' data for the decennial censuses, estimates, and projections sourced from the [U.S. Census
#' Bureau](https://www.census.gov/) and [Texas Demographic Center](https://demographics.texas.gov/). These tables
#' contain age, sex, race, and ethnicity (ASRE) of the population for Texas and all counties. The zcta table is the
#' exception and only provides the total population by zip code for Tarrant County. Available tables and their sources
#' include:
#'  *  census is the population on April 1st for the years 2000, 2010, and 2020 decennial censuses.
#' [U.S. Census Bureau](https://www.census.gov/) Summary Tape File #1 were used for 2000 and 2010.  The Demographic and
#' Housing Characteristics table was used for 2020.
#'  *  census.estimates has the estimates for July 1st for the years 2010 through 2023. These were sourced from the
#'  [Census Bureau Population and Housing Unit Estimates program](https://www.census.gov/programs-surveys/popest.html).
#'  New estimates are typically published in late June of each year.
#'  * seer has the July 1st estimates by year as published by the National Cancer Institute; Surveillance, Epidemiology,
#'    and End Results program. The seer help topic covers these data sets.
#'  *  texas.estimates are the July 1st estimates for years 2010 through 2022. The years 2010 through 2022 were sourced from the
#'  [Texas Demographic Center Estimates program](https://demographics.texas.gov/Data/TPEPP/Estimates/).
#'  *  texas.projections  are the the projected populations counts for 2010 through 2050 using the 1.0 migration scenario
#'  and produced by the [Texas Demographic Center Projections program](https://demographics.texas.gov/Data/TPEPP/Projections/)
#'  2018 release. The center has delayed Age, Sex, Race, Ethinicty (ASRE) projections to further study the effects of
#'  the 2020 demographic and housing characteristics file delay as well as the implementation of
#'  [differential privacy](https://demographics.texas.gov/Resources/TDC/Publications/20210526.2481/20210526_EvaluatingTheImpactDifferentialPrivacy.pdf?v=20211216)
#'  by the Census Bureau.  NOTE:  TO BE ADDED years 2001 through 2009 are derived from saved files previously downloaded from the
#'  [Texas Department of State Health Services Center for Health Statistics](https://www.dshs.texas.gov/center-health-statistics).
#'
#'  * zcta are the zip code tabulation area 5-year period population estimates of years 2011 through 2022 for Tarrant
#'  County only.  Sourced from the  [Census Bureau American Community Survey](https://www.census.gov/programs-surveys/acs/data.html).
#'  It contains population by zip.code and margin of error as an attribute.
#'
#'  Functions are provided to retrieve the subset of population needed [dplyr::filter()]. The summarise() function is
#'  available to group/ collapse dimensions into larger groups which is 2 orders of magnitude faster as a an array
#'  compared to a data frame. Any of the tarr_pop objects  or subsets returned from indexing [`[`], [dplyr::filter()], or
#'  summarise()  can be converted to a data frame or tibble using the generic [as.data.frame()] or
#'  [tibble::as_tibble()] functions respectively.
#'
#' The dimension names and associated values for age, sex, race, and ethnicity (ASRE) differ depending on the data
#' source. The population series have been standardized to use consistent dimension names and, where possible, aligned
#' categorical values. The primary differences are in the treatment of age and race/ethnicity:
#' * _Age_: Census Bureau census data and TDC estimates, projections are available in single year ages where as the
#' Census Bureau estimates are  in 5-year age groups. See the format section for details.
#' * _Race/Ethnicity_: Census Bureau data includes a greater number of race categories, each cross-tabulated by
#' ethnicity (Hispanic/Non-Hispanic). In contrast, TDC data includes fewer race categories and treats ethnicity as a
#' racial classification. For example, categories like "Black-Hispanic" or "White-Hispanic" are not available in TDC
#' data.
#'
#'  Carefully consider how to use the census.estimates when selecting race when using "...in combination" categories as
#'  they are not mutually exclusive.
#'
#' The Census Bureau  **updates** population estimates each year for the years after the most recent decennial census.
#' Therefore the estimates for the 2021 and 2022 released in 2023 were slightly different when released in 2024. For
#' the census.estimates table, the years 2010 through 2019 were sourced from the data file released in mid-2020.
#' The census bureau is expected to release in late 2024 or early 2025 final estimates for the inter-censal years between
#' 2010 and 2020 that will become the final estimates for those years. Estimates for years 2020 through 2024 are from
#' the most recent estimates data file. As a result the estimates for 2020 through the most recent year will be
#' updated/changed each year compared to the previous vintage file release.
#'
#' @format
#' The dimension names for all the tables except for zcta are: year, area.name, sex, age.char, race, and ethnicity.
#' Index selection from each dimension returns the filtered table.
#' The census, census.estimates, texas.estimates and texas.projections tables include the following dimensions
#'    and available values:
#'  * year is an integer that with values:
#'    - census 2000, 2010, and 2020.
#'    - census.estimates 2001 through 2023.
#'    - texas.estimates 2001 through 2023.
#'    - texas.projections 2010 through 2050.
#'  * fips is a factor of the state and county fips codes.  These codes have been moved out of the tables as of February
#'    2025.  However, they are available in the package variable 'county_fips'.
#'  * area.name a factor with the names of 'Texas' and all counties in title case.
#'  * sex is a factor with the values:  Female ,  Male , or  All
#'  * age.char is an ordered factor of ages and/or age groups:
#'       * census has single years 0 through 99, plus 100-104, 105-109, 110+, and  All  age groups.
#'       * census.estimates has five year age groups beginning 0-4 through 80-84, 85+, and All age groups.
#'       * texas.estimates has single years 0 through 95 plus 85 +, 95 +, and  All  age groups.  It also has 5 year
#'         age groups starting 0-4, through 80-84, and special spans of 15-44, and 15-49.
#'       * texas.projections has single years 0 through 94, 95 + and  All  age groups
#'  * race is a factor:
#'       * census has American Indian And Alaska Native, Asian, Black, Hawaiian Or Pacific Islander, Other,
#'         Two Or More, and White
#'       * census.estimates has the same race categories as census with addition of "or in combination" for all the single
#'         categories.  For example, "Asian or in combination"
#'       * texas.estimates and texas.projections has Asian, Black, Other, and White.  __NOTE__  that texas.estimates
#'         race variable has had 'Asian' since 2017, that category was included in "Other" before 2017.
#'  * ethnicity is a factor with the values: All, Hispanic, or Non-Hispanic.  __NOTE that the "Non-Hispanic" values
#'    exist only for race equal to 'All' for texas.estimates and texas.projections tables.__
#'  * population is the numeric value of the population.
"population"
