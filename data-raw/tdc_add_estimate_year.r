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

input_dir <- file.path(
  paths$population,
  "Estimates",
  "Texas Demographic Center",
  "asre"
)
update_pattern <- sprintf("^%d_ASRE_Estimate_alldata\\.csv$", update_year)

process_tdc_update_age <- function(x) {
  x |>
    stringr::str_remove_all(stringr::regex("Ages", ignore_case = TRUE)) |>
    stringr::str_remove_all(stringr::regex(" (ye?a??rs?|Ages)", ignore_case = TRUE)) |>
    stringr::str_trim(side = "both") |>
    stringr::str_replace("5\\+", "5 +")
}

ordered_tdc_update_ages <- function(x) {
  age_levels <- setdiff(levels(x), "All")
  c(sort(as.character(rage::as.age_group(age_levels))), "All")
}

read_tdc_update_csv <- function(file, col_types) {
  readr::read_csv(
    file = file,
    col_types = col_types,
    id = "file_name",
    progress = TRUE
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      year = basename(file_name) |>
        stringr::str_extract("^20[1-2][0-9]") |>
        as.integer()
    ) |>
    dplyr::select(-file_name) |>
    dplyr::rename_with(\(col) {
      col |>
        stringr::str_replace("anglo", "white") |>
        stringr::str_remove("^nh_")
    })
}

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

  col_types <- readr::cols(
    County = readr::col_factor(),
    FIPS = readr::col_factor(),
    Age = readr::col_factor(ordered = TRUE),
    .default = readr::col_integer()
  )

  read_tdc_update_csv(files[[1L]], col_types = col_types) |>
    data.table::setDT()
}

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

  long <- data.table::melt(
    data = df,
    id.vars = c("year", "county", "age"),
    variable.name = "race.sex",
    variable.factor = TRUE,
    value.name = "population",
    verbose = FALSE
  )

  long[
    ,
    c("race.eth", "sex") := data.table::tstrsplit(
      race.sex,
      split = ".",
      fixed = TRUE,
      fill = NA
    )
  ][
    ,
    county := forcats::fct_relabel(
      county,
      \(x) county_names(gsub(" COUNTY", "", x, ignore.case = TRUE))
    )
  ][
    ,
    county := forcats::fct_recode(county, "Texas" = "State Of Texas")
  ][
    ,
    age := forcats::fct_relabel(
      age,
      \(x) process_tdc_update_age(x) |> rage::as.age_group() |> as.character()
    )
  ][
    ,
    age := ordered(age, levels = ordered_tdc_update_ages(age))
  ][
    ,
    sex := forcats::fct_na_value_to_level(sex, level = "All")
  ][
    ,
    race.eth := factor(race.eth)
  ][
    ,
    race.sex := NULL
  ]

  data.table::setnames(long, c("age", "county"), c("age.char", "area.name"))

  if (!is.null(counties)) {
    long <- long[area.name %chin% counties]
  }
  if (!isTRUE(include_texas_total)) {
    long <- long[area.name != "Texas"]
  }

  long <- long[!is.na(population)]

  dim_cols <- c("area.name", "sex", "age.char", "race.eth")
  keep <- long[
    ,
    !Reduce(`|`, lapply(.SD, \(x) as.character(x) == "All")),
    .SDcols = dim_cols
  ]
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
