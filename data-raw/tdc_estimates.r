# -------------------------------------------------------------------------------------->
# Script: tdc_estimates.r
# Description: An example scrit for building a population cube. Build the Texas Demographic Center county estimates cube
# through the package-native ingestion pipeline. This script expects the package to be loaded from
# data-raw/control_def.r so that package functions and source-data paths are available.
# -------------------------------------------------------------------------------------->
# Created May 14, 20026

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

## CSV reader function used in the master reader function below
read_est_csv <- function(file){
  readr::read_csv(file = file,
                  col_types = col_types,
                  id = "file_name",
                  progress = FALSE
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(year = basename(file_name) |>
                    stringr::str_extract("^20[1-2][0-9]") |>
                    as.integer()
    ) |>
    dplyr::select(-file_name) |>
    dplyr::rename_with( \(col) {
      col |>
        stringr::str_replace("anglo", "white") |>
        stringr::str_remove("^nh_")
    }
    )
}


# 2. Define reading and transformation functions -------------------------

read_tdc_estimates_raw <- function(
    pattern = "20[1-2][0-9]_ASRE_Estimate_alldata\\.csv",
    input_dir = file.path(tarr::paths$population, "Estimates", "Texas Demographic Center", "asre")
) {
  files <- list.files(path = input_dir, pattern = pattern, full.names = TRUE)

  if (!length(files)) {
    cli::cli_abort("No TDC estimate files matched {.val {pattern}} in {.file {input_dir}}.")
  }

  col_types <- readr::cols(
    County   = readr::col_factor(),
    FIPS     = readr::col_factor(),
    Age      = readr::col_factor(ordered = TRUE),
    .default = readr::col_integer()
  )

  purrr::map(files, read_est_csv) |>
    dplyr::bind_rows() |>
    data.table::setDT()
}

transform_tdc_estimates <- function(df, counties = NULL, include_texas_total = FALSE) {
  stopifnot(data.table::is.data.table(df))

  wide_names <- names(df) |>
    stringr::str_replace_all("_", ".") |>
    stringr::str_replace("^total$", "All.All") |>
    stringr::str_replace("^total", "All") |>
    stringr::str_replace("\\.total$", ".All")

  data.table::setnames(df, wide_names)

  long <- data.table::melt(
    data = df,
    id.vars = c("year", "fips", "county", "age"),
    variable.name = "race.sex",
    variable.factor = TRUE,
    value.name = "population",
    verbose = FALSE
  )

  long[ , c("race.eth", "sex") := data.table::tstrsplit(race.sex, split = ".", fixed = TRUE, fill = NA)
  ][    , county   := forcats::fct_relabel(county, \(x) stringr::str_to_title(gsub(" COUNTY", "", x, ignore.case = TRUE)))
  ][    , county   := forcats::fct_recode(county, "Texas" = "State Of Texas")
  ][    , age      := forcats::fct_relabel(age,\(x) process_age_char(x) |> rage::as.age_group() |> as.character())
  ][    , age      := ordered(age, levels = ordered_age_levels(age))
  ][    , sex      := forcats::fct_na_value_to_level(sex, level = "All")
  ][    , race.eth := factor(race.eth)
  ][    , c("race.sex", "fips") := NULL]

  data.table::setnames(long, old = c("age", "county"), new = c("age.char", "area.name"))

  if (!is.null(counties)) long <- long[area.name %chin% counties]

  if (!isTRUE(include_texas_total)) long <- long[area.name != "Texas"]

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
cube_root <- tarr.pop::init_cubes()
tdc_estimates_file <- file.path(cube_root, "base", "tdc_estimates_county.h5")

tarr.pop::ingest_population(
  reader = read_tdc_estimates_raw,
  transformer = transform_tdc_estimates,
  dims = dims,
  dim_semantics = tdc_estimate_semantics(),
  filepath = tdc_estimates_file,
  series_id = "tdc_estimates_county",
  completion_policy = "error",
  drop_all = TRUE,
  source_meta = list(
    note = "Texas Demographic Center county estimates",
    population_type = "Estimate",
    source = "Texas Demographic Center, Estimates program"
  ),
  time_dim = "year",
  area_dim = "area.name",
  overwrite = TRUE,
  data_col = "population",
  counties = default_counties,
  include_texas_total = FALSE
)

est_poparray <- tarr.pop::open_poparray("tdc_estimates_county")
