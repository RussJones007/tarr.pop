# pop_doc.r
# 
# File that documents the population list and data frames


# ---- Population data frames documentation ----

#' List of population data frames
#'
#'  A named list of data frames with population figures.  The list contains population data for the decennial censuses,
#'  estimates, and projections sourced from the [U.S. Census Bureau](https://www.census.gov/) and [Texas Demographic
#'  Center](https://demographics.texas.gov/). With one exception, these data frames contain age, sex, race, and
#'  ethnicity (ASRE) of the population for Texas, Tarrant County, plus selected counties of interest including those in
#'  the DFW metroplex, and other large urban counties in Texas. The zcta data frame is the exception and only provides
#'  the total population by zip code. Available data frames and their sources include:
#'  *  census is the population on April 1st for the years 2000, 2010, and 2020 decennial censuses. 
#'  [U.S. Census Bureau](https://www.census.gov/) Summary Tape File #1 were used for 2000 and 2010.  The Demographic 
#'  and Housing Characteristics table was used for 2020. 
#'  *  census.estimates has the estimates for July 1st for the years 2010 through 2023. These were sourced from the
#'  [Census Bureau Population and Housing Unit Estimates program](https://www.census.gov/programs-surveys/popest.html).
#'  *  texas.estimates are the July 1st estimates for years 2001 through 2022. The
#'  years 2001 through 2009 are derived from saved files previously downloaded from the 
#'  [Texas Department of State Health Services Center for Health Statistics](https://www.dshs.texas.gov/center-health-statistics). 
#'  The years 2010 through 2022 were sourced from the 
#'  [Texas Demographic Center Estimates program](https://demographics.texas.gov/Data/TPEPP/Estimates/).
#'  *  texas.projections  are the the projected populations counts for 2010 through 2050 using the 1.0 migration scenario
#'  and produced by the [Texas Demographic Center Projections program](https://demographics.texas.gov/Data/TPEPP/Projections/) 
#'  2018 release. The center has delayed Age, Sex, Race, Ethinicty (ASRE) projections to further study the effects of
#'  the 2020 demographic and housing characteristics file delay as well as the implementation of
#'  [differential privacy](https://demographics.texas.gov/Resources/TDC/Publications/20210526.2481/20210526_EvaluatingTheImpactDifferentialPrivacy.pdf?v=20211216)
#'  by the Census Bureau. 
#'  * zcta are the zip code tabulation area 5-year period population estimates of years 2011 through 2022 for Tarrant 
#'  County only.  Sourced from the  [Census Bureau American Community Survey](https://www.census.gov/programs-surveys/acs/data.html). 
#'  It contains population by zip.code  and margin of error.
#'  
#'  The name "population" conflicts with another data frame in the tidyr package.  The last package loaded via the 
#'  [library()] function usually results in the data frame that will be available. Each data frame can be 
#'  explicitly referenced by using the package name e.g., tarr::population.   Another option is to use the exclude argument 
#'  in the library() function like so `library(tidyr, exclude = "population")`.
#'  
#' The age, sex, race and ethnicity (ASRE) field names with their associated values can differ by source. These tables
#' have been standardized to use the same field names and where possible the same cateogrical values. However, there are
#' differences in race where the census has a greater number of race categories available compared to those from the
#' Texas Demographic Center (TDC).  Ethnicity categories also differ as the TDC has  population numbers for  All  and
#' Hispanic , but not stratified by race, e.g., Black-Hispanic, White-Hispanic etc. are  not available.
#' 
#'  Carefully consider how to use the census.estimates when selecting race when using "...in combination" categories as
#'  they are not mutually exclusive.
#'  
#'  The age.char variable  also differs depending on source.  For example the census.estimates table has five year age
#'  groups, whereas the other tables with ASRE have single years available.
#'  
#' The Census Bureau  **updates** the estimates each year for the years after the most recent decennial census.
#' Therefore the estimates for the 2021 and 2022 released in 2023 will be slightly different when released in 2024. For
#' the census.estimates data frame, the years 2010 through 2019 were sourced from the data file released in mid-2020.  
#' The census bureau is expected to release in late 2024 or early 2025 final estimates for the inter-censal years between 
#' 2010 and 2020 that will become the final estimates for those years. Estimates for years 2020 through 2023 are from
#' the most recent estimates data file. As a result the estimates for 2020 through the most recent year will be
#' updated/changed each year compared to the previous vintage file release.
#'    
#' @format The census, census.estimates, texas.estimates and texas.projections data frames include the following fields
#'    and available values :
#'  * year is an integer that with values:
#'    - census 2000, 2010, and 2020.
#'    - census.estimates 2001 through 2023. 
#'    - texas.estimates 2001 through 2023.
#'    - texas.projections 2010 through 2050.
#'  * fips is a factor of the state and county fips codes
#'  * area.name a factor with the names of the following areas: Texas, Bexar, Collin, Dallas, Denton, Ellis, El Paso, 
#'    Fort Bend, Harris, Hidalgo, Johnson, Kaufman, Lubbock, Nueces, Parker, Rockwall, Tarrant, Travis, Williamson, 
#'    and Wise.
#'  * sex is a factor with the values:  Female ,  Male , or  All 
#'  * age.char is an ordered factor of ages and age groups:
#'       * census has single years 0 through 99, plus 100-104, 105-109, 110+, and  All  age groups.
#'       * census.estimates has five year age groups beginning 0-4 through 80-84, 85+, and  All  age groups.
#'       * texas.estimates has single years 0 through 95 plus 85 +, 95 +, and  All  age groups.  It also has 5 year 
#'       age groups starting 0-4, through 80-84, and special spans of 15-44, and 15-49.  
#'       * texas.projections has single years 0 through 94, 95 + and  All  age groups
#'  * age.iv are right open intervals representing the ages and age groups. See [ivs::ivs] for more interval information.
#'  * race is a factor:
#'       * census has American Indian And Alaska Native, Asian, Black, Hawaiian Or Pacific Islander, Other, 
#'         Two Or More, and White
#'       * census.estimates has the same race categories as census with addition of "or in combination" for all the single 
#'       categories.  For example, "Asian or in combination"
#'       * texas.estimates and texas.projections has Asian, Black, Other, and White.  NOTE that texas.estimates race variable
#'       has had Asian since 2017, that category was included in "Other" before 2017.. 
#'  * ethnicity is a factor with the values: All, Hispanic, or Non-Hispanic.  __NOTE that the "Non-Hispanic" values  
#'  exist only for race equal to 'All' for texas.estimates and texas.projections tables.__
#'  * population is the numeric value of the population.  
"population"
