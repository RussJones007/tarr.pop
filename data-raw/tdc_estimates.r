# -------------------------------------------------------------------------------------->
# Script: tdc_estimates.r
# Description: An example script for building a population cube. Build the Texas Demographic Center county estimates cube
# through the package-native ingestion pipeline. This script expects the package to be loaded from
# data-raw/control_def.r so that package functions and source-data paths are available.
# 
#   Reads the Texas Data Center annual Estimates csv files.  These files usually become available in early November 
#   the year after the estimate date.  For example estimates for July 1, 2024 were released in November 2025.
#   Each file contains age, sex, race, ethnicity or "asre".  The state data center does not split race and ethnicity.
#   Instead Hispanic is treated as a separate "race" and is mutually exclusive to the  categories. 
#   From 2011 through 2016, the Asian category was included in "other".  Since 2017, Asian is now its own category.
#   Ages are represented as single years with two capped categories at 85 + and 95 +.  The 85 + group overlaps 
#   the 86,87,88,89,90,91,92,93,94 and 95 + categories.  So one set of the overlap should be filtered out before 
#   removed if attempting to aggregate population figures.
#   
#  Note: The TDC Estimates are stable and generally do not change with updates, though TDC did revise 2021-2024 estimates
#  to align with significant Census Bureau Updates. The link to the download tool for estimates is
#  https://www.demographics.texas.gov/Estimates/Download and select the "Age, Sex, and Race/Ethnicity" categories.
 
# -------------------------------------------------------------------------------------->
# Created May 14, 2026 - R. Jones
# Revised October 2026 - R. Jones

source(file.path("data-raw", "tdc_estimates_support.r"))

# 1. Define functions used inside other functions ---------------------------
## Modifies Age column names, only used in the transform function below
process_age_char <- function(x) {
  x |>
    stringr::str_remove_all(stringr::regex("Ages", ignore_case = TRUE)) |>
    stringr::str_remove_all(stringr::regex(" (ye?a??rs?|Ages)", ignore_case = TRUE)) |>
    stringr::str_trim(side = "both") |>
    stringr::str_replace("5\\+", "5 +")
}

## Used to sort and define age levels in the transform function
ordered_age_levels <- function(x) {
  age_levels <- levels(x)
  age_levels <- age_levels[age_levels != "All"]
  c(sort(as.character(rage::as.age_group(age_levels))), "All")
}



# 2. Define reading and transformation functions -------------------------
## CSV reader function called for each file used in the master reader function below ----
read_est_csv <- function(file){
  
  header <- readr::read_csv(file = file, n_max = 0) |> names()
  area_name <- if("Area Name" %in% header) "Area Name" else "County"
  code_name   <- if("FIPS" %in% header) "FIPS" else "Area Code"
  
  col_types <- readr::cols(
    !!area_name  := readr::col_factor(),
    !!code_name  := readr::col_factor(),
    Age           = readr::col_factor(ordered = TRUE),
    .default      = readr::col_character()
  )

  file_year <- basename(file) |>
    stringr::str_extract("^20[1-2][0-9]") |>
    as.integer()
  
  readr::read_csv(file = file, col_types = col_types, id = "file_name",
                  progress = TRUE, show_col_types = FALSE)|>
    janitor::clean_names() |>
    # if year is not present use the year in the file name.  
    dplyr::select(-file_name) |>
    dplyr::rename_with( \(col) {
      col |>
        stringr::str_replace("anglo", "white") |>
        stringr::str_remove("^nh_") |> 
        stringr::str_remove("_population$")}) |> 
    dplyr::rename(county = dplyr::all_of(make_clean_names(area_name )),
                  fips   = dplyr::all_of(make_clean_names(code_name))) |> 
    # ensure the year column is present
    (\(df)  if(! "year"  %in% names(df)){
      dplyr::mutate(df, year = file_year) 
      } else {
      dplyr::mutate(df, year = as.integer(year))
      })() |> 
    # remove any columns with "total"  involved
    select(- contains("total"))
  
}

## Reader function that iterates over each csv file ----
read_tdc_estimates_raw <- function(
    pattern = "^20[1-2][0-9]_ASRE_Estimate_alldata\\.csv",
    input_dir = file.path(tarr::paths$population, "Estimates", "Texas Demographic Center", "asre"),
    ...
) {
  files <- list.files(path = input_dir, pattern = pattern, full.names = TRUE)

  if (!length(files)) {
    cli::cli_abort("No TDC estimate files matched {.val {pattern}} in {.file {input_dir}}.")
  }

  dfs <- purrr::map(files, read_est_csv) 
  df <- dplyr::bind_rows(dfs, ) |>
    mutate( across(where(is.character), ~ str_remove(.x, ",") |> as.integer())) |> 
    data.table::setDT()
return(df)
}

transform_tdc_estimates <- function(df, counties = NULL, include_texas_total = FALSE) {
  stopifnot(data.table::is.data.table(df))

  wide_names <- names(df) |>
    stringr::str_replace_all("_", ".") |>
    stringr::str_replace("^total$", "All.All") |>
    stringr::str_replace("^total", "All") |>
    stringr::str_replace("\\.total$", ".All")

  data.table::setnames(df, wide_names)
  
  if(any(names(df) %in% "fips")) df[, fips := NULL]
  
  long <- data.table::melt(
    data = df,
    id.vars = c("year", "county", "age"),
    variable.name = "race.sex",
    variable.factor = TRUE,
    value.name = "population",
    verbose = FALSE
  )

  long[ , c("race.eth", "sex") := data.table::tstrsplit(race.sex, split = ".", fixed = TRUE, fill = NA)
  ][    , county   := forcats::fct_relabel(county, \(x) county_names(gsub(" COUNTY", "", x, ignore.case = TRUE)))
  ][    , county   := forcats::fct_recode(county, "Texas" = "State Of Texas")
  ][    , age      := forcats::fct_relabel(age,\(x) process_age_char(x) |> rage::as.age_group() |> as.character())
  ][    , age      := ordered(age, levels = ordered_age_levels(age))
  ][    , sex      := forcats::fct_na_value_to_level(sex, level = "All")
  ][    , race.eth := factor(race.eth)
  ][    , c("race.sex") := NULL]

  data.table::setnames(long, old = c("age", "county"), new = c("age.char", "area.name"))

  if (!is.null(counties)) long <- long[area.name %chin% counties]

  if (!isTRUE(include_texas_total)) long <- long[area.name != "Texas"]
  
  # remove "all" from any row.
  dim_cols <- c("area.name", "sex", "age.char", "race.eth")
  keep <- long[
    ,
    !Reduce(`|`, lapply(.SD, \(x) as.character(x) == "All")),
    .SDcols = dim_cols
  ]
  long <- long[keep]
  
  # sort on year and area
  setorder(long, year, area.name)

  factor_cols <- names(long)[vapply(long, is.factor, logical(1))]
  if (length(factor_cols)) {
    long[, (factor_cols) := lapply(.SD, droplevels), .SDcols = factor_cols]
  }

  data.table::setcolorder( long, c("year", "area.name", "sex", "age.char", "race.eth", "population"))

  long
}

tdc_estimate_semantics <- function() {
  list(
    year = tarr.pop:::new_dim_semantics(
      dim_name = "year",
      domain = "time",
      partition_type = "partition",
      scale_type = "interval",
      validated = TRUE,
      notes = "July 1 estimate year."
    ),
    area.name = tarr.pop:::new_dim_semantics(
      dim_name = "area.name",
      domain = "area",
      partition_type = "partition",
      scale_type = "nominal",
      validated = TRUE,
      notes = "County-only geography. Texas state total is excluded from this cube."
    ),
    sex = tarr.pop:::new_dim_semantics(
      dim_name = "sex",
      domain = "sex",
      partition_type = "partition",
      scale_type = "nominal",
      validated = TRUE
    ),
    age.char = tarr.pop:::new_dim_semantics(
      dim_name = "age.char",
      domain = "age interval",
      partition_type = "set",
      scale_type = "interval",
      validated = TRUE,
      overlap_levels = "85 +",
      notes = "Age groups are interval-valued and may include an open upper bound."
    ),
    race.eth = tarr.pop:::new_dim_semantics(
      dim_name = "race.eth",
      domain = "race and ethnicity",
      partition_type = "partition",
      scale_type = "nominal",
      validated = TRUE,
      notes = paste(
        "TDC combines race and Hispanic ethnicity into one dimension.",
        "The Hispanic category is treated as a level in this partition."
      )
    )
  )
}

default_counties <- NULL
if (exists("county_fips", inherits = TRUE)) default_counties <- setdiff(names(county_fips), "Texas")

dims <- c("year", "area.name", "sex", "age.char", "race.eth")

# Support table ----
## Support is the complete valid source support, not only missing Asian rows.
## TDC changed schema in 2017:
## - 2011-2016: Asian was subsumed under other; ages end at "85 +".
## - 2017 onward: Asian is directly reported; ages end at "95 +".
tdc_estimate_input_dir <- file.path(
  tarr::paths$population,
  "Estimates",
  "Texas Demographic Center",
  "asre"
)
tdc_estimate_pattern <- "^20[1-2][0-9]_ASRE_Estimate_alldata\\.csv"
tdc_estimate_years <- tdc_estimate_years_from_files(
  input_dir = tdc_estimate_input_dir,
  pattern = tdc_estimate_pattern
)

support_table <- tdc_estimate_support_table(
  years = tdc_estimate_years,
  counties = default_counties
)


cube_root <- tarr.pop::init_cubes()
tdc_estimates_file <- file.path(cube_root, "base", "tdc_estimates_county.h5")

# 3. Ingest -------------------------------------------------------------------------------------------------------
# undebug(read_tdc_estimates_raw)
# undebug(read_est_csv)
# undebug(transform_tdc_estimates)
undebug(ingest_population)
undebug(build_poparray_from_df)
debug(df_2_array)
#undebug(validate_population_df)
#debug(df_2_array)

tarr.pop::ingest_population(
  reader = read_tdc_estimates_raw,
  transformer = transform_tdc_estimates,
  dims = dims,
  dim_semantics = tdc_estimate_semantics(),
  filepath = tdc_estimates_file,
  series_id = "tdc_estimates_county",
  completion_policy = "na",
  drop_all = TRUE,
  source_meta = list(
    note = "Texas Demographic Center county estimates",
    population_type = "Estimate",
    source = "Texas Demographic Center, Estimates program"
  ),
  time_dim = "year",
  area_dim = "area.name",
  support = support_table,
  overwrite = TRUE,
  data_col = "population",
  counties = default_counties,
  include_texas_total = FALSE
)


rm(pa)
