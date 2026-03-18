#' tarr.pop-package.R
#'
#' The package provides a disk-based approach for storing and working with large
#' population arrays. It includes population figures for Texas and its counties,
#' with data sets covering age, sex, race, and ethnicity (ASRE). Estimated
#' population by Zip Code Tabulation Area (ZCTA) is also available. Refer to the
#' [population] help topic for the available data sets and their formats.
#'
#' @section Package functions:
#' * county_population() for convenient access to the county asre data sets for years of interest.
#' * retrieve_zip_code_population() to select different years of zip codes and associated population estimates for Tarrant
#' County.
#'
#' @author Russ Jones <RussJones007@gmail.com>
#' @references Population figure sources are:
#' * Decennial censuses and Estimates [U.S. Census Bureau](https://www.census.gov/)
#' * [Texas Demographic Center Estimates program](https://demographics.texas.gov/Data/TPEPP/Estimates/)
#' * [Texas Demographic Center Projections program](https://demographics.texas.gov/Data/TPEPP/Projections/)
#' @keywords package
#' @name tarr.pop
"_PACKAGE"
