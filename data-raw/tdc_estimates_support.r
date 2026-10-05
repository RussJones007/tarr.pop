# Support helpers for Texas Demographic Center county estimates.


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
  
##------------------------------------------------------------------------->


# 2. General support functions ------------------------------------------------------------------------------------

  
tdc_estimate_years_from_files <- function(
    input_dir,
    pattern = "^20[1-2][0-9]_ASRE_Estimate_alldata\\.csv"
) {
  files <- list.files(path = input_dir, pattern = pattern, full.names = FALSE)
  years <- basename(files) |>
    stringr::str_extract("^20[1-2][0-9]") |>
    as.integer()
  years <- sort(unique(stats::na.omit(years)))

  if (!length(years)) {
    cli::cli_abort("No TDC estimate years were found in {.file {input_dir}}.")
  }

  years
}

tdc_ordered_age_factor <- function(labels, levels = labels) {
  ordered(
    as.character(rage::as.age_group(labels)),
    levels = as.character(sort(rage::as.age_group(levels)))
  )
}

tdc_estimate_support_table <- function(
    years,
    counties,
    transition_year = 2017L,
    sex_levels = c("female", "male"),
    race_eth_levels = c("asian", "black", "hispanic", "other", "white")
) {
  checkmate::assert_integerish(years, min.len = 1L, any.missing = FALSE)
  checkmate::assert_character(counties, min.len = 1L, any.missing = FALSE)
  checkmate::assert_integerish(transition_year, len = 1L, any.missing = FALSE)
  checkmate::assert_character(sex_levels, min.len = 1L, any.missing = FALSE)
  checkmate::assert_character(race_eth_levels, min.len = 1L, any.missing = FALSE)

  years <- sort(unique(as.integer(years)))
  transition_year <- as.integer(transition_year)
  county_levels <- sort(unique(as.character(counties)))
  sex_levels <- sort(unique(sex_levels))
  race_eth_levels <- sort(unique(race_eth_levels))

  early_age_labels <- c("< 1", as.character(1:84), "85 +")
  later_age_labels <- c("< 1", as.character(1:94), "95 +")
  all_age_labels <- unique(c(early_age_labels, later_age_labels))
  age_levels <- levels(tdc_ordered_age_factor(all_age_labels))

  expand_era <- function(era_years, age_labels) {
    if (!length(era_years)) {
      return(NULL)
    }

    tidyr::expand_grid(
      year = era_years,
      area.name = factor(county_levels, levels = county_levels),
      sex = factor(sex_levels, levels = sex_levels),
      age.char = factor(
        as.character(rage::as.age_group(age_labels)),
        levels = age_levels,
        ordered = TRUE
      ),
      race.eth = factor(race_eth_levels, levels = race_eth_levels)
    )
  }

  dplyr::bind_rows(
    expand_era(years[years < transition_year], early_age_labels),
    expand_era(years[years >= transition_year], later_age_labels)
  )
}
