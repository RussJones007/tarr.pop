# ------------------------------------------------------------------------------------------------------------------->
# Script: seer_def.r
# Description:
#   Reads and processes two lists of population arrays, US Standard populations and NCI bridged population estimates
#
#  US Standard populations
# - Standard millions for 1940 and 2000 with 19 age groups
# -  Standard population 2000 with 19 age groups
# -  Standard population 2000 with single ages to 99
# - Reads, processes and saves the National Cancer Institute SEER race bridged population estimates at the
#   county level
#
# Steps:
# Setup work
# 1. Prepare fips and area.name to use
# 2. Create a list of file reading definitions including the file path and name, column width definitions,
#    and formatting functions to read the fixed width files into data frames
# 3. Read each SEER file
# 4. Format as an array  for storage
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: February 20, 2025
# Revised June 6, 2025
# ------------------------------------------------------------------------------------------------------------------->

# 1. Prepare fips and area.name to use ---------------------------------------------------------------------------
# # fips and area.name, the county_fips variable has the fips code and name for each county
area.name <- set_names(names(county_fips), county_fips)

# 2. Create formatting functions to read data frames ---------------------------------------------------
## age group functions to use for reading in the different data files.
age_19_groups <- function(){
  grps <- c(0, 1, seq(from = 5, to = 85, by = 5))
  age_groups_19_nms <- tarr::as.age_group(grps)
  age_groups_19_nms <- age_groups_19_nms|>
    as.character()
  age_groups_19 <- set_names(seq(0, 18,1), age_groups_19_nms)
  age_groups_19
}

age_single_years <- function(){
  yrs <- 0:90
  age_nms <- tarr::as.age_group(yrs) |>
    as.character()
  age_yrs <- set_names(yrs, age_nms)
  age_yrs
}
## Reading format functions ----
### Each function contains the logic to read the different file formats, and each return a data frame.
### Standard Population format function
standard_pop_format <- function(df, ages = age_19_groups()){
  # std pop names
  std_nms <- c(paste(seq(from = 1940, to= 2000, by = 10), "U.S. Std Million"), "2000 U.S. Std Population")

  # standard millions and standard population for the US
  stds <- c(seq(from = 141, to = 201, by =10), 203) |>
    as.integer() |>
    set_names(std_nms)

  std_pop <- df |>
    filter(standard  %in% stds) |>
    mutate(standard = factor(standard, labels = names(stds)),
           across( c(age, population), as.integer),
           age      = factor(age, levels = ages, labels = names(ages)))

  std_pop
}

## SEER population Estimate formatting function, for files from 1990 and later that have 4 races
## 2 variations of this function based on age groups are defined below this function
pop_est_format <- function(df, ages_groups, area = area.name)  {
  assert_that(df %has_name% "state_fips")
  fed_id = names(area) |> as.integer()

  df <- df |>
    mutate(across( c(state_fips, county_fips, population), as.integer))

    # create state level data from all counties to add to df
    state_df <- df |>
      group_by(year,state, state_fips, race, ethnicity, sex, age) |>
      summarise( population = sum(population),
                 county_fips = 0,
                 .groups = "drop_last") |>
      ungroup()

    df <- bind_rows(state_df, df)

    df <- df |>
      mutate(fips = ((state_fips * 1000) + county_fips) ) |>
      filter(fips %in% fed_id) |>     # select only desired  counties
      mutate(area.name = area[as.character(fips)] |> unname()) |>
      #df <- df |>
      select(-c(state, state_fips, county_fips, fips)) |>
      select(year, area.name, sex, age, race, ethnicity, population)


  race_vec   <- c("White", "Black", "American Indian/Alaskan Native", "Asian or Pacific Islander")
  ethnicity_vec <- c("Non-Hispanic", "Hispanic")  # add 1 when referencing ethnicity field to match the coding
  sex_vec    <- c("Male", "Female")

  # create factors
  df <- df |>
    mutate(age    = ordered(as.integer(age), levels = ages_groups, labels = names(ages_groups)),
           year   = ordered(year),
           race   = factor(race, levels = as.character(1:4),  labels = race_vec),
           sex    = factor(sex, labels = sex_vec),
           ethnicity = ethnicity_vec[ethnicity+1] |> factor(labels = ethnicity_vec),
           area.name =  factor(area.name))
  df
}

pop_19_group_format <- partial(pop_est_format, ages_groups = age_19_groups())
pop_year_format <- partial(pop_est_format, ages_groups = age_single_years())

### List of files, column widths, and formatting functions -------
file_defs <- list(
  standard_populations = list(file = file.path(paths$population, "Standard Populations/stdpop.19ages.txt"),
                              columns = readr::fwf_cols(standard = c(1, 3), age = c(4, 6) , population = c(7, 14)),
                              format_func = standard_pop_format),
  county_one_year = list(file = file.path(paths$population, "Estimates/SEER/tx.1990_2023.singleages.through89.90plus.adjusted.txt.gz"),
                         columns = readr::fwf_cols(year = c(1,4), state = c(5,6), state_fips = c(7,8),
                                                   county_fips = c(9,11), race = c(14,14), ethnicity = c(15,15),
                                                   sex = c(16,16), age = c(17, 18), population = c(19, 27)),
                         format_func = pop_year_format),
  county_20_age_group  = list(file = file.path(paths$population, "Estimates/SEER/tx.1990_2023.20ages.adjusted.txt.gz"),
                        columns = readr::fwf_cols(year = c(1,4), state = c(5,6), state_fips = c(7,8),
                                                  county_fips = c(9,11), race = c(14,14), ethnicity = c(15,15),
                                                  sex = c(16,16), age = c(17, 18), population = c(19, 27)),
                        format_func = pop_19_group_format)
)

## Function to read files
read_seer <- function(def, area = area.name, fed_id = fips){
  assert_that(file.exists(def[["file"]]))
  ret <- readr::read_fwf(file = def$file,
                         col_positions = def$columns,
                         col_types = rep("i", by = length(def$columns)),
                         progress = TRUE) |>
    def$format_func()
  ret
}

# 3. Read the SEER files -----
seer_df_list <- map(file_defs, read_seer)

population_std_df <- split(x = seer_df_list$standard_populations,
                              f = seer_df_list$standard_populations$standard,
                              drop = TRUE) |>
  map(.f = \(df) select(df, - standard))
seer_df_list$standard_populations <- NULL

rm(pop_19_group_format, pop_year_format, pop_est_format, read_seer, standard_pop_format, file_defs,
   age_19_groups, age_single_years)

# 4. Format as an array  for storage ---------------
# split off the standard populations
seer <- map(seer_df_list, ~ rename(.x, age.char = age) |> as.tarr_pop())
seer_single_age  <- seer$county_one_year
seer_grouped_age <- seer$county_20_age_group

population_std <- map(population_std_df, as.tarr_pop)

rm(population_std_df, seer_df_list, area.name)

usethis::use_data(seer_single_age, seer_grouped_age, internal = FALSE, overwrite = TRUE)
usethis::use_data(population_std, internal = FALSE, overwrite = TRUE)
rm(population_std, list = ls(pattern = "seer"))
print("SEER population data read and saved as tarr_pop")
