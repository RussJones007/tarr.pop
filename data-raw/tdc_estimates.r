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
#  
#  Steps in creating the TDC cube:
#  Calls the support function script then
#  1.  Define the read CSV files function read_tdc_estimates_raw() that calls the reader function read_est_csv()
#  2.  Define the transformation function transform_tdc_estimates() that takes all the read csv files, formats,
#      removes totals, optionally removes Texas rows, creates and returns a long data.table
#  3. The tdc_estimate_semantics() function is used to define the dimension semantics
#  4.  Create the "support" table
#  5. Ingest by calling ingest_population()
# -------------------------------------------------------------------------------------->
# Created May 14, 2026 - R. Jones
# Revised October 2026 - R. Jones

source(file.path("data-raw", "tdc_estimates_support.r"))

# 1. Define reading and transformation functions -------------------------
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


# 2.  Define the transformation function transform_tdc_estimates() ---------------------------------------
## Transform the read csv files into a canaonocal data frame, removing total, and state of Texas rows.
## The function returns a long data.table
transform_tdc_estimates <- function(df, counties = NULL, include_texas_total = FALSE) {
  stopifnot(data.table::is.data.table(df))

  # Resolve source geography by identifier before filtering or discarding FIPS.
  # The county reference includes the canonical state entry: 48000 -> Texas.
  if (!"fips" %in% names(df)) {
    cli::cli_abort("TDC estimates require FIPS/Area Code to resolve source geography.")
  }
  reference <- tarr.pop::county_fips
  reference_codes <- as.character(reference)
  reference_names <- names(reference)
  if (anyNA(reference_codes) || anyNA(reference_names) ||
      anyDuplicated(reference_codes) || anyDuplicated(reference_names)) {
    cli::cli_abort("The county FIPS reference must map identifiers and names one-to-one.")
  }
  
  source_codes <- trimws(as.character(df$fips))
  # Newer files use the two-digit state FIPS rather than county code 000.
  source_codes[source_codes %in% "48"] <- as.character(reference["Texas"])
  short_codes <- grepl("^[0-9]{1,3}$", source_codes)
  source_codes[short_codes] <- paste0(
    "48", sprintf("%03d", as.integer(source_codes[short_codes]))
  )
  
  geography_index <- match(source_codes, reference_codes)
  if (anyNA(geography_index)) {
    unresolved <- unique(paste0(
      as.character(df$county[is.na(geography_index)]),
      " (FIPS/Area Code ", as.character(df$fips[is.na(geography_index)]), ")"
    ))
    cli::cli_abort(
      "Unresolved TDC geography identifiers: {.val {head(unresolved, 5L)}}."
    )
  }
  
  canonical_names <- reference_names[geography_index]
  df[, county := factor(canonical_names, levels = unique(canonical_names))]

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
  ][    , age      := forcats::fct_relabel(age,\(x) process_age_char(x) |> rage::as.age_group() |> as.character())
  ][    , age      := ordered(age, levels = ordered_age_levels(age))
  ][    , sex      := forcats::fct_na_value_to_level(sex, level = "All")
  ][    , race.eth := factor(race.eth)
  ][    , c("race.sex") := NULL]

  data.table::setnames(long, old = c("age", "county"), new = c("age.char", "area.name"))

  if (!is.null(counties)) {
    long <- long[area.name %chin% counties | (isTRUE(include_texas_total) & area.name == "Texas")]
  }

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

# 3. The tdc_estimate_semantics() function is used to define the dimension semantics ------------------------
tdc_estimate_semantics <- function(support = NULL) {
  semantics <- list(
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
  if (!is.null(support)) {
    # Repository source rules above and tdc_estimate_support_table establish
    # 2017 as the transition. Resolve boundaries from represented year labels.
    years <- sort(unique(as.integer(support$year)))
    age_labels <- unique(as.character(support$age.char))
    race_labels <- unique(as.character(support$race.eth))
    schemas <- function(early_levels, late_levels) {
      eras <- list(years[years < 2017L], years[years >= 2017L])
      levels <- list(early_levels, late_levels)
      selected <- which(lengths(eras) > 0L)
      lapply(selected, function(i) list(
        from = if (min(eras[[i]]) == min(years)) NULL else as.character(min(eras[[i]])),
        through = if (max(eras[[i]]) == max(years)) NULL else as.character(max(eras[[i]])),
        levels = levels[[i]]
      ))
    }
    early_age <- as.character(rage::as.age_group(c("< 1", as.character(1:84), "85 +")))
    late_age <- as.character(rage::as.age_group(c("< 1", as.character(1:94), "95 +")))
    semantics$age.char <- tarr.pop:::pa_update_dim_semantics(semantics$age.char,
      applicability = list(by = "year", schemas = schemas(
        intersect(age_labels, early_age), intersect(age_labels, late_age))))
    semantics$race.eth <- tarr.pop:::pa_update_dim_semantics(semantics$race.eth,
      applicability = list(by = "year", schemas = schemas(
        setdiff(race_labels, "asian"), race_labels)),
      notes = c(semantics$race.eth@notes,
        "Before 2017 Asian is included in Other; no numerical decomposition is implied."))
  }
  semantics
}

default_counties <- NULL
if (exists("county_fips", inherits = TRUE)) default_counties <- setdiff(names(county_fips), "Texas")

dims <- c("year", "area.name", "sex", "age.char", "race.eth")

# 4.  Create the "support" table ----------------------------------------------------------------------------------
## Support is the complete valid source support, not only missing Asian rows.
## TDC changed schema in 2017:
## - 2011-2016: Asian was included under other; ages end at "85 +".
## - 2017 onward: Asian is directly reported; ages end at "95 +".
tdc_estimate_input_dir <- file.path(tarr::paths$population, "Estimates", "Texas Demographic Center", "asre")
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

# 5. Ingest by calling ingest_population() -------------------------------------------------------------------------
tarr.pop::ingest_population(
  reader = read_tdc_estimates_raw,
  transformer = transform_tdc_estimates,
  dims = dims,
  dim_semantics = tdc_estimate_semantics(support_table),
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
