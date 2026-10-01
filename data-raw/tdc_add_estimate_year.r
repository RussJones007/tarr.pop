# --------------------------------------------------------------------------------------
# Script: tdc_add_estimate_year.r
# Description: Add one annual Texas Demographic Center estimate file to the existing
# county estimates cube. Run the package/path setup from data-raw/control_def.r first.
# --------------------------------------------------------------------------------------

if (
  !exists("paths", inherits = TRUE) ||
    !exists("county_names", inherits = TRUE) ||
    !exists("county_fips", inherits = TRUE)
) {
  cli::cli_abort(
    "Run the setup section of {.file data-raw/control_def.r} before this script."
  )
}

# Change this value when a new annual estimate file is downloaded.
update_year <- 2024L

input_dir <- file.path(paths$population,"Estimates","Texas Demographic Center", "asre")
update_pattern <- sprintf("^%d_ASRE_Estimate_alldata\\.csv$", update_year)


# 1. Source specific helper functions -----------------------------------------------------------------------------

## Function for age labels processing ---- 
process_tdc_update_age <- function(x) {
  x |>
    stringr::str_remove_all(stringr::regex("Ages", ignore_case = TRUE)) |>
    stringr::str_remove_all(stringr::regex(" (ye?a??rs?|Ages)", ignore_case = TRUE)) |>
    stringr::str_trim(side = "both") |>
    stringr::str_replace("5\\+", "5 +")
}

## Create an ordered factor from the ages labels ---
ordered_tdc_update_ages <- function(x) {
  age_levels <- setdiff(levels(x), "All")
  c(sort(as.character(rage::as.age_group(age_levels))), "All")
}

# 2. Functions to read the raw CSV file and transform the data. -----------------------------------------------------------------------

## The read function for tdc csv file ----
### Reads the csv, formts names to snake case, adds the year of the estimate

read_tdc_update_csv <- function(file) {
  
  # handle differing column names in later file compared to files from 2023 and earlier.
  header <- readr::read_csv(file = file, n_max = 0) |> names()
  county_name <- if("Area Name" %in% header) "Area Name" else "County"
  fips_name   <- if("FIPS" %in% header) "FIPS" else "Area Code"
  
  col_types <- readr::cols(
    !!county_name := readr::col_factor(),
    !! fips_name  := readr::col_factor(),
    Age            = readr::col_factor(ordered = TRUE),
    .default       = readr::col_character()
  )
  
  df <- readr::read_csv(file      = file,
                        col_types = col_types,
                        id        = "file_name",
                        progress  = TRUE ) |>
    janitor::clean_names() |>
    # NOTE:  the 2024 file already has "year" as a field, files from previous years do not.
    dplyr::mutate( 
      year = basename(file_name) |> stringr::str_extract("^20[1-2][0-9]") |> as.integer(),
      # The population data will often be formatted with a comma, remove them to convert to integer
      across( where(is.character), ~ str_remove(.x, ",") |> as.integer())
    ) |>
    dplyr::select(-file_name) |>
    dplyr::rename_with(\(col) {
      col |>
        #  Change "anglo" to "white" in older files.
        stringr::str_replace("anglo", "white") |>
        stringr::str_remove("^nh_")
    })
  
  #Check that all population figures are present, no NA and throw an error if NA is present
  pop_cols <- grep("population$", names(df), value = TRUE)
  na_present <- map_lgl(df[pop_cols], \(col) any(is.na(col)) )
  if(any(na_present)){
    msg <- paste0("Columns ", pop_cols[na_present], " has one or more NA values", collapse = ", ")
    stop(msg)
  }
  
  # rename the area name/county and area code/fips fields to a common field name
  dt <- dplyr::rename(df, 
                      county = dplyr::all_of(make_clean_names(county_name )),
                      fips   = dplyr::all_of(make_clean_names(fips_name)) 
  ) |> 
    data.table::setDT()
  
  return(dt)
}


## Reader function for the CSV ----
read_tdc_estimate_year <- function(...) {
  files <- list.files(
    path = input_dir,
    pattern = update_pattern,
    full.names = TRUE
  )

  if (length(files) != 1L) {
    cli::cli_abort(c(
      "Expected exactly one TDC estimate file for {update_year}.",
      "i" = "Matched {length(files)} files in {.file {input_dir}}."
    ))
  }

  dt <- read_tdc_update_csv(file = files[[1]])
  return(dt)
}

## Data transformation function ----
transform_tdc_estimate_year <- function(
    df,
    counties = NULL,
    include_texas_total = FALSE
) {
  stopifnot(data.table::is.data.table(df))

  wide_names <- names(df) |>
    stringr::str_replace_all("_", ".") |>
    stringr::str_replace("^total$", "All.All") |>
    stringr::str_replace("^total", "All") |>
    stringr::str_replace("\\.total$", ".All")

  data.table::setnames(df, wide_names)
  if ("fips" %in% names(df)) {
    df[, fips := NULL]
  }

  # Remove the "All" columns
  df[, grep("^All", names(df)) := NULL]
  # Remove the race alone columns
  df[, names(df)[(str_count(names(df), "\\.") == 1)] := NULL]
  
  cols <- grep("\\.population$", names(df), value = TRUE)
  setnames(df, cols, sub("\\.population$", "", cols) )
  
  
  long <- data.table::melt(
    data = df,
    id.vars = c("year", "county", "age"),
    variable.name = "race.sex",
    variable.factor = TRUE,
    value.name = "population",
    verbose = FALSE
  )
  
  long[, c("race.eth", "sex") := data.table::tstrsplit(
      race.sex,
      split = ".",
      fixed = TRUE,
      fill = NA
    )][ , county := forcats::fct_relabel(
            county,
            \(x) county_names(gsub(" COUNTY", "", x, ignore.case = TRUE))
    )
    ][, county := forcats::fct_recode(county, "Texas" = "State Of Texas")
    ][, age := forcats::fct_relabel(
          age,
          \(x) process_tdc_update_age(x) |> rage::as.age_group() |> as.character())
    ][, age := ordered(age, levels = ordered_tdc_update_ages(age))
  ][, sex := forcats::fct_na_value_to_level(sex, level = "All")
  ][, race.eth := factor(race.eth)
  ][, race.sex := NULL  ]

  data.table::setnames(long, c("age", "county"), c("age.char", "area.name"))

  if (!is.null(counties)) {
    long <- long[area.name %chin% counties]
  }
  if (!isTRUE(include_texas_total)) {
    long <- long[area.name != "Texas"]
  }

  #long <- long[!is.na(population)] # This step is dangerous and can remove catgories unexpectantly

  dim_cols <- c("area.name", "sex", "age.char", "race.eth")
  keep <- long[ , !Reduce(`|`, lapply(.SD, \(x) as.character(x) == "All")),
    .SDcols = dim_cols]
  
  long <- long[keep]

  factor_cols <- names(long)[vapply(long, is.factor, logical(1))]
  if (length(factor_cols)) {
    long[, (factor_cols) := lapply(.SD, droplevels), .SDcols = factor_cols]
  }

  data.table::setorder(long, year, area.name)
  data.table::setcolorder(
    long,
    c("year", "area.name", "sex", "age.char", "race.eth", "population")
  )

  long
}

dims <- c("year", "area.name", "sex", "age.char", "race.eth")
default_counties <- setdiff(names(county_fips), "Texas")

cube_root <- tarr.pop::init_cubes()
tdc_estimates_file <- file.path(cube_root, "base", "tdc_estimates_county.h5")

if (!file.exists(tdc_estimates_file)) {
  cli::cli_abort(c(
    "The existing TDC estimates cube was not found.",
    "i" = "Expected {.file {tdc_estimates_file}}.",
    "i" = "Create it with {.file data-raw/tdc_estimates.r} first."
  ))
}

#undebug(read_tdc_update_csv)
#undebug(read_tdc_estimate_year)
debug(transform_tdc_estimate_year)
debug(add_population_data)
tarr.pop::add_population_data(
  cube = tdc_estimates_file,
  reader = read_tdc_estimate_year,
  transformer = transform_tdc_estimate_year,
  dims = dims,
  add_dim = "year",
  completion_policy = "error",
  drop_all = TRUE,
  source_meta = list(
    note = sprintf(
      "Texas Demographic Center county estimates, updated through %d",
      update_year
    ),
    population_type = "Estimate",
    source = "Texas Demographic Center, Estimates program"
  ),
  data_col = "population",
  counties = default_counties,
  include_texas_total = FALSE
)

updated <- tarr.pop::open_poparray("tdc_estimates_county")
if (!as.character(update_year) %in% tarr.pop::years(updated)) {
  cli::cli_abort("The updated cube does not contain year {update_year}.")
}
