#' tarr.pop-package.R
#'
#' Contains the population figures for Texas, and all counties. The population data sets contain age, sex, race, and
#' ethnicity (asre) data. The estimated population by Zip Code Tabulation Areas (zcta) are also available.  Refer to the
#' population help topic for what is contained and data format for the data sets.
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
