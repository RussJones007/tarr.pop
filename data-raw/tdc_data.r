
# -------------------------------------------------------------------------------------->
# Script: tdc_data.r
# Description:
#   This script creates the population cubes for data from the Texas Demographic Center (TDC).  
#   Estimates from the TDC are the more stable form of population figures, but are usually not
#   available for the most recent year.
#   Projections are created that can cover several decades, but the figures are based on assumptions
#   that may not hold through the whole time period.
#   data.table is used to save memory and increase processing speed
#   
#   This can be used as an example of how to read , process and save population figures to a poparray.
#
# Steps:
# For each type of population product, estimates and projections
# 1. Read the csv files,convert variables to appropriate type, change names or values.# 
# 2. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: `May 4 2026
# Revised:
# -------------------------------------------------------------------------------------->

process_age_char <- compose(
  ~ str_replace(., "5\\+", "5 \\+"),
  ~ str_trim(., side = "both"),
  ~ str_remove_all(.x, regex("Ages", ignore_case = TRUE)),
  ~ str_remove_all(.x, regex(" (ye?a??rs?|Ages)", ignore_case = TRUE))
)



# 1. Read the csv files,convert variables to appropriate type, change names or values. --------------------------

#' Reads population estimates files downloaded from the Texas Demographic Center.  All variable transformations
#' are done in this function - one place.
#' # changes some of the value labels, for example TDC has chnaged "anglo" to "white".  So "white" s used for
#' all records.  TDC combine race and ethnicity.   Function removes "nh-" from non-hispanic race as that is already 
#' known.  Column names use the older R convention f spaces in the name if needed, e.g., age.char, area.name, etc
#' @param .pattern a regex pattern to match the files to read.
#' @param .counties a character vector of county names to include in the estimates.
#' @returns A list of data frames, each containing population estimates for a specific year.
read_estimate <- function(.pattern, .counties){
  # get the files to read
  pth   <- file.path(tarr::paths$population,"Estimates/Texas Demographic Center/asre")
  files <- list.files(path = pth, pattern = .pattern, full.names = TRUE, )
  
  columns <- cols(County = col_factor(),
                  FIPS   = col_factor(),
                  Age    = col_factor(ordered = TRUE),
                  .default = col_integer())
  
  read_and_format <- compose(
    \(df) select(df, -file_name),
    \(df) rename_with(df,
                      .fn = \(col) str_replace(col, "anglo", "white") |>
                        str_remove("^nh_"), .cols = everything()),
    \(df) mutate(.data = df,
                 year   = basename(file_name) |>
                   str_extract("^20[1-2][0-9]") |>
                   as.numeric()
    ),
    clean_names,
    \(f) read_csv(file = f, col_types = columns, id = "file_name", progress = FALSE)
  )
  
  ests <- map(files, read_and_format, .progress = "Reading Texas Demographic Center estimate files" )
  return(ests)
}

csvs <- read_estimate(.pattern = "20[1-2][0-9]_ASRE_Estimate_alldata\\.csv", .counties = names(county_fips))

# bind the list of read csvs and convert to a data.table
tdc.estimates <- bind_rows(csvs) |>
  relocate(year, .after = asian_female) |>
  setDT()

rm(csvs, read_estimate)

# change the column names
columns <- names(tdc.estimates) |>
  str_replace(pattern = "_", replacement = ".") |>
  str_replace(pattern = "^total$", replacement = "All.All") |>
  str_replace(pattern = "^total", replacement = "All") |>
  str_replace(pattern = "\\.total$", replacement = ".All")

setnames(x = tdc.estimates, columns)

tdc.estimates <- melt(data = tdc.estimates,
                      id.vars         = c("year", "fips", "county", "age"),
                      variable.name   = "race.sex",
                      variable.factor = TRUE,
                      value.name      = "population",
                      verbose         = FALSE)

# reorders age groups of factor levels function
ord_ages <- compose(
  ~c(.x, "All"),
  as.character,
  sort,
  as.age_group,
  ~.x[.x != "All"],
  levels
)

tdc.estimates[
  , c("race.eth", "sex") := tstrsplit(race.sex, split = ".", fixed = TRUE, fill = NA)][
    , fips := fct_relabel(fips, ~ paste0("48", .x))][
      #, type := factor("Estimate")][
      , county := fct_relabel(county, ~str_to_title(gsub(pattern = " COUNTY", "", .x , ignore.case = T))) |>
        fct_recode("Texas" = "State Of Texas" )][
          , age := fct_relabel(age,
                               .fun = ~process_age_char(.x) |> rage::as.age_group() |> as.character()) ][
                                 , age := ordered(age, levels = ord_ages(age))][
                                   , c("race.eth", "sex") := tstrsplit(race.sex, split = ".", fixed = TRUE, fill = NA)][
                                     , sex := fct_na_value_to_level(sex, level = "All")][
                                       , race.eth := factor(race.eth)][
                                         , "race.sex" := NULL][
                                           , fips := NULL] |>
  setnames(old = c("age", "county"), new = c("age.char", "area.name"))

# Remove the 'All" labels from each column
cols <- names(tdc.estimates)[map_lgl(tdc.estimates, ~ is.factor(.x) | is.character(.x) )]
tdc_estimates_no_all <- tdc.estimates[
  tdc.estimates[
    ,
    !Reduce(`|`, lapply(.SD, \(x) x == "All")),
    .SDcols = cols
  ]
][
  , (cols) := lapply(.SD, droplevels), .SDcols = cols
]

rm(ord_ages, tdc.estimates,cols, columns)

setcolorder(
  tdc_estimates_no_all,
  c("year", "area.name", "sex", "age.char", "race.eth", "population")
)

tdc.est.array <- df_2_array(tdc_estimates_no_all, data_col = "population")

# check_all <-  compose( \(df) map_lgl(df, \(x) any(x == "All")) )
# check_all(tdc_estimates_no_all)
# check_all(df)
# rm(check_all)

rm(tdc_estimates_no_all)

semantics <- list(
  year   = new_dim_semantics(dim_name = "year", 
                           domain         = "time", 
                           partition_type = "partition", 
                           scale_type     = "ordinal", 
                           validated      = TRUE,
                           notes          = "The estimate if for July 1 of the year in this dimension"
                           ),
  area.name   = new_dim_semantics(dim_name = "area.name",
                         domain           = "area",
                         partition_type   ="partition",
                         scale_type       = "nominal",
                         validated        = TRUE,
                         #overlap_levels   = "Texas",
                         notes            = "County names in title case"),
  sex     = new_dim_semantics(dim_name = "sex", domain = "sex", 
                              partition_type = "partition", 
                              scale_type = "nominal", validated = TRUE),
  age.char = new_dim_semantics(dim_name = "age.char",
                           domain         = "age interval",
                           partition_type = "set", 
                           scale_type     = "ordinal",
                           validated      = TRUE,
                           overlap_levels = "85 +",
                           notes          = "From < 1 through 95 +, treated as an age interval."
                           ),
  race.eth = new_dim_semantics("race.eth",
                               domain = "Race and ethniciy", 
                               partition_type = "partition",
                               scale_type = "nominal",
                               TRUE,
                               notes = "Race and hispnic ethiniciy are combined. There is no addtional information \n
                               to identify a hispanic black or hispanic white.  Te hispnic category is treated as a `race`.")
)

setdiff(tdc.est.array |> dimnames() |> names(), names(semantics))

cube_root <- init_cubes()
tdc_estimates_file <- file.path(cube_root, "base", "tdc_estimates_county.h5")

pa_write_poparray_cube(
  x = tdc.est.array,
  filepath = tdc_estimates_file,
  dimnames_list = dimnames(tdc.est.array),
  overwrite = TRUE,
  time_dim = "year",
  area_dim = "area.name",
  dim_semantics = semantics,
  source = list(source = "Texas Demographic Center, Estimates program"),
  data_col = "population",
  series_id = "tdc_estimates_county"
)

tdc_estimates_h5 <- HDF5Array::HDF5Array(
  filepath = tdc_estimates_file,
  name = "cube/population"
)
dimnames(tdc_estimates_h5) <- dimnames(tdc.est.array)

est_poparray <- new_poparray(
  x = tdc_estimates_h5,
  dimnames_list = dimnames(tdc.est.array),
  dim_semantics = semantics,
  area_dim = "area.name",
  time_dim = "year",
  source = list(source = "Texas Demographic Center, Estimates program")
)

