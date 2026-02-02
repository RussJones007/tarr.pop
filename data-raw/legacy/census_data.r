# census_data.r
#
#  Query the Census Bureau APIs for data
# use after pop_def.r
# get decennial data and add to the pop data frame
## Local functions to gather census bureau data ------------------------------------------------

# Query and save decennial data, variable components to the block level
# Decennial census Age Sex Race by Geography  function
#' get_census_age_race
#' Retrieve the age and race data from the decennial census (2000-2020).  Population and housing data for the 2020
#' census released May 25 2023.
#' @details The census has additional races as compared to the Texas Demographic Center estimates and projections files.
#'   Where the two overlap, the Texas race names are used. For sex variables, value may be male, female, or all.
#'
#' @param dec_year is the decennial year, currently limited to 1980 through 2020
#' @param state the abbreviation of the state (defaults to 'Tx')
#' @param county the county in the state to retrieve (defaults to 'Tarrant')
#' @param geography is state, county, tract, etc.  (defaults to 'county')
#' @return a data frame in long format of the requested population data with
#'   variable labels
get_census_age_race <- function(dec_year = 2010,
                                state = "Tx",
                                county = "Tarrant",
                                geography = "county",
                                dataset = "sf1") {
  # check validity of the passed arguments by finding in
  st <- toupper(state)
  data(fips_codes)
  if (!all(st %in% fips_codes$state %>% unique())) {
    mssg <- state[!st %in% (fips_codes$state %>% unique())]
    stop(paste(
      "state(s)",
      paste0("'", mssg, "'", collapse = ","),
      "is not valid"
    ))
  }

  fips <- tidycensus::fips_codes %>%
    filter(state %in% st) %>%
    mutate(county = str_to_lower(county))


  county_full <- paste(county, "County") %>% str_to_lower()
  if (!all(county_full %in% fips$county) & geography == "county") {
    mssg <- county[!county_full %in% fips$county]
    stop(paste(
      "County(s)",
      paste0("'", mssg, "'", collapse = ","),
      "are not valid"
    ))
  }

  county_fips <- fips %>%
    filter(county  %in% county_full) %>%
    pull(county_code) %>%
    unique()

  st_fips <- fips %>%
    filter(st  %in%  state) %>%
    pull(state_code) %>%
    unique()


  stopifnot(dec_year %in% seq(1990, 2020, by = 10))

  # convenience functions
  # function that formats labels to use underscores between words. Used for census tables
  format_label    <- compose(
    ~ str_remove(., ":$"),
    ~ str_remove(., "total_"),
    ~ str_replace_all(., c(
      "^_{1,2}" = "",
      ":_" = "_",
      ";"  = "",
      ":$" = ""
    )),
    str_to_lower,
    ~ str_replace_all(.x, "(!!| )", "_")
  )

  # Function to extract the race ethnicity as well the 18 and over clause from the concept field in the vars data frame
  # Used for census tables
  extract_concept <- compose(
    format_label,
    ~ replace_na(., ""),
    ~ str_extract(.x, "(18 YEARS AND OVER|(?<=\\().*(?=\\)))")
  )

  # Get the variable names and concepts for the data set and year. Unique tables
  # are found in the first 4 character for blocks, e.g., 'P012'and the first 6
  # characters for census tracts, e.g., 'PCT012'.
  vars <- load_variables(year = dec_year,
                         dataset = dataset,
                         cache = T)

  # Variables are based on the table accessed.  Always use census tract variables
  age_sex_vars <- vars %>%
    filter(str_detect(string = name, pattern = "^PCT0?12")) %>%
    pull(name)

  pop <- get_decennial(
    year = dec_year,
    sumfile = dataset,
    state = st_fips,
    county = county_fips,
    geography = c(geography),
    variables = age_sex_vars
  ) %>%
    janitor::clean_names() %>%
    # add the variable name
    left_join(.,
              vars %>% select(name, label, concept),
              by  = c("variable" = "name")) %>%
    mutate(label = paste(format_label(label), extract_concept(concept), sep = "_") %>%
             str_remove(., "_$")) %>%
    select(-concept)

  return(pop)
}

#' Calculate hispanic ethnicity numbers function
#' Used to extract the hispanic, not hispanic and "All" components from census year data.
#' Use filtered data sets that are single race and do not include "All" or "Hispanic"
#' @param df a data frame with race, race.eth, and population columns
#' @return a data frame with ethnicity column added
extract_ethnicity <- function(df) {
  no_hisp_field <- df$race.eth %>%
    fct_drop() |>
    as.character() |>
    unique() %>%
    `[`(str_detect(., "Not Hispanic"))

  race_field <- df$race |> unique()
  stopifnot(length(race_field) == 1)
  ret <- df |>
    pivot_wider(names_from = race.eth, values_from = population) |>
    rename(
      All            = !!race_field,
      `Non-Hispanic` = !!no_hisp_field) |>
    mutate(Hispanic  = All - `Non-Hispanic`)

  pivot_longer(ret,
               cols = c(All, `Non-Hispanic`, Hispanic),
               names_to = "ethnicity",
               values_to = "population")

}

# extract components of the label field like sex, age group, and race ethnicity
extract_sex <- partial(str_extract, pattern = "^(total|male|female)")

age_string <-
  "([:digit:]{1,3}_(to|and)_[:digit:]{1,3}(_| )?(year)?s?|(under)?_[:digit:]{1,3}_years?(_and_over)?)"
extract_age_grp <- compose(~ str_remove_all(., "_(year)?s?"),
                           ~ str_replace(., "_(to|and)_", "-"),
                           ~ str_replace(., "(male_)?under_", "< "),
                           ~ str_replace(., pattern = "_years_and_over",  replacement = " +"),
                           ~ replace_na(., replace = "all ages"),
                           ~ str_extract(.x, pattern = age_string))

rac_eth_string <- "(hispanic|not_hispanic|two_or_more|american|asian|black|native|some|white)"
extract_race <- compose(~replace_na(., replace = "All"),
                        str_to_title,
                        ~str_replace_all(., "_", " "),
                        ~str_replace(., "american_indian_and_alaska_native", "American Indian and Alaska Native"),
                        ~str_replace(., "some_other_race", "Other"),
                        ~str_replace(., "native_hawaiian_and_other_pacific_islander", "Hawaiian or Pacific islander"),
                        ~str_replace(., "population_of_two_or_more_races", "Two or more races"),
                        ~str_remove(., ","),
                        ~str_remove(., "(some_|_or_latino|_or_african_american)"),
                        ~str_remove(., "_(race_)?(alone|races|or_latino)"),
                        partial(str_extract, pattern = str_c(rac_eth_string, ".+", sep = ""))
)

# composed sequence to separate the components of the label field for sex, age or age group
extract_label <- . %>%
  mutate(sex       = extract_sex(label),
         sex       = case_when(
           .default = str_to_title(sex),
           str_detect(sex, "total") ~ "All"
         ),
         age.group = extract_age_grp(label),
         race.eth  =extract_race(label)
         )

# get County level decennial population for the selected counties from pop.def
print("Retrieving decennial population for 2000 through 2020")
years <- c(2000, 2010, 2020)

#cnts_to_get <- counties %>% .[!str_detect(., "Texas")]
cnts_to_get <- county_fips |> names() |> (\(x) x[!str_detect(x, "Texas")])()
cnts_to_get[cnts_to_get == "De Witt"] <- "dewitt" # change to lower case for county_fips


#undebug(get_census_age_race)
#get_census_age_race(2000, county = 'DeWitt')

counties_pop <- map(years,
                    ~ get_census_age_race(dec_year = .x,
                                          dataset = ifelse(.x != 2020, "sf1", "dhc"),
                                          geography = "county",
                                          state = "Tx",
                                          county = cnts_to_get) %>%
                      mutate(year = .x) %>%
                      select(year, everything())
                    ) %>%
  set_names(years) |>
  bind_rows()

texas_pop <- map(years, ~ get_census_age_race(dec_year = .x,
                                              dataset = ifelse(.x != 2020, "sf1", "dhc"),
                                              geography = "state",
                                              state = "Tx",
                                              county = "") %>%
                   mutate(year = .x) %>%
                   select(year, everything())
)  |>
  set_names(years) |>
  bind_rows()

# set the geoid to 5 digits
texas_pop <- texas_pop |>
  mutate(geoid = (as.integer(geoid) * 1000) |> as.character())

census <- bind_rows(counties_pop, texas_pop) |>
  extract_label() |>  # extract the components of the label field
  dplyr::select(-label, -variable) |>
  select(year, geoid, area.name = name, sex, race.eth,
         age.char = age.group, population = value) %>%
  arrange(area.name, sex, race.eth, age.char) %>%
  mutate(area.name = str_remove(area.name, " County, Texas"),
         year = as.integer(year),
         fips = geoid,
         across( where(is.character), factor),
         #age.iv   = tarr::as.age_group(age.char),
         #age.char  = tarr::age_group_to_char(age.iv)
  ) |>
  select( year, fips, area.name, sex, age.char,
          #age.iv,
          race.eth, population)

levels(census$age.char)[levels(census$age.char) == "all ages"] <- "All"

## --- Calculate totals for "All" sex as the census data only has numbers for Female and Male  ----
#  A functional sequence that uses all the mutually exclusive ages and age groups to get totals by sex
#  Then add the total_sex records to the census data frame
total_sex <- census |>
  ungroup() |>
  filter(age.char != "All") |>
  summarise(population = sum(population), .by = c(year, fips, area.name, age.char, race.eth)) |>
  mutate(sex = "All") |>
  select(names(census))

# add total_sex to census
census <- bind_rows(census, total_sex) |>
  arrange(area.name, race.eth, sex)

rm(total_sex)

## Split race.eth into race and ethnicity ----
# For the census table race is given which includes hispanic, and race without hispanic. Change race.eth to race and
# ethnicity hispanic total for a race = race - race non-hispanic.
# Example, 'Asian Hispanic' = 'Asian' - 'Asian Not Hispanic'
# Create a race as a category and split the data frame.  Then use extract_ethnicity() to get the hispanic component of
# the data frame. Do not use the "All" or "Race" in extract_ethnicity

# census$race.eth |> sort_values()

census_list <- census |>
  filter(! race.eth  %in% c("All", "Hispanic")) |>
  mutate(race = str_remove(race.eth, "Not Hispanic( Or Latino)?") |> str_trim(side = "both")) |>
  split(f = ~race) |>
  map(extract_ethnicity)

#undebug(extract_ethnicity)

# process race = "All" and "Hispanic"separately, not using extract_ethnicity
census_list$all <- census |>
  filter(race.eth %in% c("All", "Hispanic")) |>
  pivot_wider(names_from = race.eth, values_from = population) |>
  mutate(
    race = "All",
    `Non-Hispanic` = All - Hispanic) |>
  pivot_longer(cols = c(All, `Non-Hispanic`, Hispanic),
               names_to = "ethnicity",
               values_to = "population")


census <- bind_rows(census_list) |>
  mutate(
    across( where(is.character) , ~factor(.x, levels = sort_values(.x)))#,
    #age.char = reorder(age.char, iv_end(age.iv)) |> ordered()
  )

paths$population
"//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Population"
fn <- file.path(paths$population, "Census/Census_2000_thru_2020.parquet")
write_parquet(x = census, file = fn)
rm(fn)

census <- as.tarr_pop(census)
census <- set_source_url(
  census,
  nm = "Decennial Census; 2000, 2010, 2020",
  pop_type = "Census",
  url = "U.S. Census Bureau Summary Tape File 1 for 2000 and 2010, \nDemographic and Housing Characteristics for 2020")

usethis::use_data(census, internal = FALSE, compress = "xz", overwrite = TRUE)

rm(age_string, rac_eth_string, extract_ethnicity,
   get_census_age_race, counties_pop, fips_codes, years)
rm(list = ls(pattern = "extract_+"), census_list, texas_pop, cnts_to_get, census)

# Census Estimates --------------------------------------------------------
# The tidycensus package was to be used to get estimates, but fails after 2019.
# Therefore data as csv files have been downloaded from  the Census Bureau.
# The original files were downloaded from "https://www.census.gov/programs-surveys/popest/data/tables.html"
# Files used include
#  - "cc-est2010-alldata-48.csv",, for census 2010, base 2010 and years 2011- 2020.  This file has the latest estimates for the 2010s
#  - "cc-est2022-alldata-48.csv", for years 2020 through 2022, includes latest census update to 2022 released June 2023.
#  - "cc-est2023-alldata-48.csv" for years 2020 through 2023, includes latest census update to 2023 released June 27, 2024.
#  The census bureau updates the 2020s estimates each year resulting in new estimates for previous years at each new
#  publication. This may/may not change when the decennial census characteristics are published.

# Commented out tidycensus use, saved for possible future use.
# yrs <- 2015:2019
#  Tidycensus is unable to download vintage estimates after 2019
# get_tarr_estimates <- partial(get_estimates, state = "TX", county = "TARRANT",
#                               variables = NULL,
#                              geography = "county", product = "characteristics",
#                              breakdown = c("AGEGROUP", "RACE", "SEX", "HISP"),
#                              breakdown_labels = TRUE)
#
# census.estimates <- map(yrs, ~get_tarr_estimates(.x) %>%
#                    mutate(year = .x)) %>%
#   bind_rows() %>%
#   janitor::clean_names()

## Vector used for encoding census integer agegrp to a factor
agegrp_vec <- c("All"   =  0,  "0-4"   =  1,
                "5-9"   =  2,  "10-14" =  3,
                "15-19" =  4,  "20-24" =  5,
                "25-29" =  6,  "30-34" =  7,
                "35-39" =  8,  "40-44" =  9,
                "45-49" =  10, "50-54" =  11,
                "55-59" =  12, "60-64" =  13,
                "65-69" =  14, "70-74" =  15,
                "75-79" =  16, "80-84" =  17,
                "85 +"     =  18
)

## Vector for translating census variable race  to a factor
race_vec <- c("allraces" = "All",
              "All"      = "All",
              "aa"       = "Asian",
              "aac"      = "Asian or in combination",
              "ba"       = "Black",
              "bac"      = "Black or in combination",
              "ia"       = "American Indian and Alaska Native",
              "iac"      = "American Indian and Alaska Native or in combination",
              "na"       = "Hawaiian or Pacific Islander",
              "nac"      = "Hawaiian or Pacific Islander or in combination",
              "tom"      = "Two or more",
              "wa"       = "White",
              "wac"      = "White or in combination")
race_vec <- set_names(x = names(race_vec), nm = race_vec) # reverse the names and values as it was typed incorrectly

## Read census estimate files --------------------
# The first file is the latest estimates for the 2010s update. The second file has the latest estimates for the 2020s
est_fns <- list(
  est_2010_2019 = "cc-est2020-alldata-48.csv",
  est_2020s     = "cc-est2024-alldata-48.csv"
)

# Function to read an all estimates county data file, clean the names and set to appropriate types
# The one argument needed is the file name
read_estimate_file <- compose(
  ~ mutate(.data = ., across(year:last_col(), as.numeric),
           geoid = paste0(state, county)),
  #~ filter(.data = ., str_detect(ctyname, "Tarrant")),
    janitor::clean_names,
  ~ read_csv(file = ., col_types = cols(.default = col_character())),
  ~ file.path(paths$population, "Estimates/Census", .x)
)

# read the files into a list
est_list <- map(est_fns, read_estimate_file)

# The 2010 census and base data rows are removed as is the 2020 base estimate leaving just the
# estimates for each year.
est_list$est_2010_2019 <- est_list$est_2010_2019 |>
  filter(!year %in%  c(1, 2, 13))

# Remove the 2020 base data row
est_list$est_2020s <- est_list$est_2020s |>
  filter(!year %in%  c(1))

# The year column in the 2020s dataframe is increased by 11 so that it can be combined
# with the previous decade of estimates
est_list$est_2020s$year <- est_list$est_2020s$year + 11

# Vectors for renaming the year variable in each estimate file read The 2010-2019 data frame has 10 entries in the year
# column an estimate for years 2010 through 2019. The 2020s estimate data frame estimates for each year in the file
# which is 2020 to the latest year represented.
entries_in_2020s <- est_list$est_2020s$year |> unique() |> length()
years_in_2020s   <- 2020:(2020 + entries_in_2020s - 1)

years_name_vec <- c(#"1" = "2010 Census",
                    #"2" = "2010 Base",
                    set_names(x = 2010:2019, nm = 3:12),
                    #"13" = "2020 Base",
                    set_names(x = years_in_2020s, 13:(13 + entries_in_2020s - 1))
)

# create total estimates for Texas for each year
est_list$texas <- bind_rows(est_list[1:2]) |>
  group_by(sumlev, state, stname, year, agegrp) |>
  summarise(across(tot_pop:hnac_female, sum), .groups = "drop_last") |>
  mutate(
    county  = "000",
    ctyname = "Texas",
    geoid   = "48000"
  )

#est_list$est_2010_2019 |> View()
# combine the list of data frames and process to final data frame
census.estimates <- bind_rows(est_list) |>
  rename(name = ctyname) |>
  mutate( year      = years_name_vec[as.character(year)] |> as.integer(),
          agegroup  = factor(x = agegrp, levels = agegrp_vec, labels = names(agegrp_vec)) |>  ordered(),
          age.iv    = as.character(agegroup) |> tarr::as.age_group(),
          agegroup  = fct_reorder(.f = agegroup, .x = iv_end(age.iv)) |> ordered(),
          area.name = str_remove(name, " County") |> factor(),
          county    = paste0("48", county)) |>
#  filter(area.name %in% counties) |>
  rename_with(.fn   = ~str_replace(., "tot_", "allraces_"),
              .cols = starts_with("tot_")) |>
  select(where(is.character), where(is.factor),everything())  |>
  select(-age.iv) |>
  pivot_longer(cols = allraces_pop:last_col(), names_to = "race_sex", values_to = "estimate") %>%
  separate(col      = race_sex, into = c("race", "sex"), sep = "_") |>
  pivot_wider(names_from = sex, values_from = estimate ) |>
  select(-pop) |>
  mutate( All = male + female) |>
  pivot_longer(cols = c(male, female, All), names_to = "sex", values_to = "estimate") |>
  mutate(
    #type            = "Census estimate",
    ethnicity       = case_when(
      str_detect(race, "^nh") ~ "Non-Hispanic",
      str_detect(race, "^h")  ~ "Hispanic",
      TRUE ~ "All"),
    race      = case_when(
      #str_detect(race, "allraces")   ~ "all races",
      str_detect(race, "^(nh|h)$")   ~ "All",
      str_detect(race, "^(nh|h)")    ~ str_remove(race, "^(nh|h)"),
      TRUE ~ race
    ),
    race = factor(race, levels = race_vec, labels = names(race_vec)),
    sex  = sex %>% stringr::str_to_title(),
    #type = "Census Estimate",
    across( c(county, sex, ethnicity), ~factor(.x, levels = sort_values(.x)) )
    ) %>%
  select(fips = county, year, area.name, sex, age.char = agegroup, #age.iv,
         race, ethnicity, population = estimate)

years <- range(census.estimates$year)
census.estimates <- census.estimates |>
  as.tarr_pop() |>
  set_source_url(
    pop_type = "Estimates",
    nm = str_glue("Census Bureau estimates from {years[1]} through {years[2]}"),
    url = "https://www.census.gov/programs-surveys/popest/data/tables.html"
  )

usethis::use_data(census.estimates, internal = FALSE, compress = "xz", overwrite = TRUE)

rm(read_estimate_file, entries_in_2020s, years_name_vec, est_list, years,
   years_in_2020s, est_fns, agegrp_vec, race_vec)
rm(census.estimates)

# Zip Code Estimate from the ACS -----------------------------------------
# Retrieve data through 2023
# Get the zip codes of interest for Tarrant County

load(paste0(paths$spatial,"/Tarrant/Zip Code Tabluation Areas/Tarrant Zip Codes.rdata"))

tarrant.zip.codes <-  zips.tarrant@data %>%
  pull(ZCTA5CE10)

# zip codes used by Michelle M. supplied to Coulter G.
zips_michelle <- c(75019, 75022, 75028, 75038, 75050, 75051, 75052, 75054, 75061,
                   75062, 75063, 75067, 75104, 75261, 76001, 76002, 76005, 76006,
                   76008, 76009, 76010, 76011, 76012, 76013, 76014, 76015, 76016,
                   76017, 76018, 76019, 76020, 76021, 76022, 76023, 76028, 76034,
                   76036, 76039, 76040, 76044, 76051, 76052, 76053, 76054, 76060,
                   76063, 76065, 76071, 76084, 76092, 76102, 76103, 76104, 76105,
                   76106, 76107, 76108, 76109, 76110, 76111, 76112, 76114, 76115,
                   76116, 76117, 76118, 76119, 76120, 76122, 76123, 76126, 76127,
                   76129, 76131, 76132, 76133, 76134, 76135, 76137, 76140, 76148,
                   76155, 76164, 76177, 76179, 76180, 76182, 76244, 76247, 76248,
                   76262) %>%
  as.character()


additional_zips <- c(75061, 75062, 75067, 75261, 76005, 76009, 76019, 76023,
                     76044, 76084, 76122, 76247) %>% as.character()

tarrant.zip.codes <- c(tarrant.zip.codes, additional_zips, zips_michelle) %>% unique()
rm(zips_michelle, additional_zips, zips.tarrant)

yrs <- 2023:2011
print("Downloading ZCTA population estimates for several years from the Census Bureau. This can take a minute")
print(paste("Pulling", length(tarrant.zip.codes),"ZCTA areas for years", paste(yrs, collapse = ", ")))

# function to pull the data, caching is switched on so this should be fast after
# the first run. returns the estimates for each zip code for Tarrant County.
get_tarr <- function(yr, zips){
  ret <- get_acs(geography = "zcta", table = "B01003", year = yr,
                 cache_table = TRUE, survey = "acs5")   %>%
    mutate(GEOID =
             str_sub(GEOID, str_length(GEOID)-4, str_length(GEOID))
    ) %>%
    filter(GEOID %in% zips) %>%
    mutate(Year = yr)
}

# Pull the data for each year and place into a list
years.zip.codes <- purrr::map(yrs, ~ get_tarr(.x, tarrant.zip.codes)) |>
  set_names(yrs)
tarrant.zip.codes.estimates <- bind_rows(years.zip.codes) %>%
  rename(zip.code = GEOID) %>%
  select(end.year = Year, zip.code, estimate, moe)


fn <- file.path(paths$population, "ZCTA/Tarrant ZCTA.parquet")
write_parquet(x = tarrant.zip.codes.estimates, file = fn)
rm(fn)
dput(names(tarrant.zip.codes.estimates))
yrs <- range(tarrant.zip.codes.estimates$end.year)
margin_error <- tarrant.zip.codes.estimates$moe

census.zcta.estimates <-tarrant.zip.codes.estimates |>
  select(-moe) |>
  as.tarr_pop(data_col = "estimate") |>
  set_source_url(
  #obj     = tarrant.zip.codes.estimates,
  pop_type = "Estimates",
  url      = "https://data.census.gov/all?q=Population+by+age+in+ZCTA",
  nm       = str_glue("ACS ZCTA population 5 year period estimates {yrs[1]} - {yrs[2]}")
  ) |>
  set_attr(which = "moe", value = margin_error)

usethis::use_data(census.zcta.estimates, internal = FALSE, overwrite = TRUE)
rm(tarrant.zip.codes,get_tarr, years.zip.codes, sort_values, tarrant.zip.codes.estimates, margin_error,
   list = ls(pattern = "census."), yrs)


print("END of census bureau population construction script")
