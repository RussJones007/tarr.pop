# ======================================================================================>
# pop_def.R
# This script processes data originated from the Texas Demographic Center. Uses the population projection 2010-2050 file
# from the Texas Demographic Center and the year specific estimate csv files to construct one data frame. This script is
# part of the tarr package. It saves a copy of the 'pop' data frame for  use by the package
#
# Population estimates from the Census Bureau for Tarrant Zip codes are also available from 2011-2019.
#
# Russ Jones
#  Revised May, 2025
#    - All counties in Texas are now in the tarr_pop data arrays.
#    - Updated the processing the Texas Data Center projections.  Whereas previously it was about 5 minutes to
#    process, it has been reduced to about 45 seconds. This is mostly due to using data.table and working with factors
#    rather than values in each vector.
#    - The projections for Texas counties that includes race and ethnicities is still the 2018 released projections.
#      The 2022 release did not include race/ethnicity.  The state total release did include these categories, but
#      counties has not been released.
#  Revised February 1, 2025
#    - Adapted for stand alone package
#    - The census data is processed via the census_data.r script
#  Revised August 11, 2023
#    - Removed the "type" field from the tables.
#    - All population tables are part of the population list and documented.
#    - The census table was updated to calculate and include total population by sex, previously only the "All"
#      category had population.
#    - Five population tables available - census, census.estimates, texas.estimates, texas.projections, and zcta
#
#  Revised July 2023
#   - Migration scenario columns removed as the SDC did not include them in the 2018 vintage projections.
#   - Later years do include migration scenario but it may be described as a number or character.
#     When downloading new years, use the 1.0 or "mid" scenario.
#   - Added two functions for encoding and decoding ages and age groups from character to iv (intervals) and vice versa.
#
#  Revised June 2023
#    - Encode age groups as ivs intervals.
#    - Interval for an age group are encoded as left closed and right open intervals.  For example, [10, 15)  means all
#      numbers from 10 up to but not including 15.
#    - Functions age_group_encode and age_group_decode were removed
#    - Added functions tarr::as.age_group and iv_to_age_group that convert age groups or ivs intervals to each other.
#
# Revised March 2023.  Age group encoding is dropped as it added too much complexity.
#
# Revised June 4, 2022.  Estimates for 2011 through 2019 are processed by a separate script and saved in individual
# folders in "Population/Estimates/Texas Demographic Center/ (county, asre, place)" This simplifies this script to read
# the final estimate files from those folders.
#
# Revised January 8, 2022 - an encoding scheme was implemented for the age.num field to handle age groups.
#     - A single age is shown as that number e.g., "2" in the age.car field is 22 in the age.num field
#     - 9000 is reseved for all ages.
#     - Age groups are coded for numbers above 9000.  The encoding is # '9lluu'. Meaning 9, the (ll)ower limit number,
#       and the (uu)pper digit number.  The lower and upper limits will always consist of two digits. For example, an
#       age group of 5-15 is shown as 90515. Another example for ages 0 through 1 is 90001.
#
# Two functions are used to encode and decode age groups:
#     - age_group_encode(age.grp)
#     - age_group_decode(age.grp)
#
#
# Revised January 1, 2022 - Added decennial census figures from 2000 and 2010 The general 2020 census data was not yet
# available - just PL1 information with race-ethnicity data.
#
# Revised April 2021 - due to changes in the 2019 projections file from the state demographic center, county.name was
# changed to area.name. Race ethnicity changed from the old term "Anglo" to the "NH-White".  Asian has been split from
# "Other" Migration scenario for projections were dropped. One scenario, that for 2010-2015 census estimates was used
# for the scenario. See "Methodology 2018 Projections of the Population of Texas and Counties in Texas by Age,.pdf" in
# the projections folder.
#
# Revised November 2019
#   - added 2018 population estimates from the Texas Demographic center.
#   updated the coding for importing estimate csv files to be more robust.
#
# Revised June 20, 2019
#
#   - added ZCTA population counts and estimates for 2010 through 2017.  To prevent loading of a large data set, Tarrant
#   zip code information is stored separately. Revised May 27, 2019 - Loaded the population estimates for 2000-2009
#
# Revised 5/31/2018
#    - made the table in long format, added a "Type" field for "Estimate" versus "Projection"
#
# Revised 8/14/18
#     - made this script part of the tarr package and placed in the data-raw folder.  It is  used to update the pop
#     dataframe variable.  The Data  frame is saved in two places: one in the population folder found in paths, and the
#     other in the R folder of the tarr package as sysdata.rda
#
# Revised 8/15/18
#   - When this scrpt is finished, it removes all variables created except 'pop'. The script is meant to be called by
#   control_def.r which saves pop and other data frames to sysdata.rda
#
# Note: The Texas State Data Center Projections are made every two years and changes do occur with updates. Estimates
# are stable and generally do not change with updates. Mid-year estimates are released for two years before the current
# year. For 2001-2009 estimates this script is dependent on the year all county, age, sex, race estimate csv files
# downloaded from the Texas Demographic Center and named as "YYYY_ASRE_Estimate_alldata.csv" in a folder of "Estimates"
# under the population folder.  The link to download estimate files is https://demographics.texas.gov/Estimates/ and
# select the "Age, Sex, and Race/Ethnicity" files. Population projections are found in a downloaded file from the SDC
# and named as "State Data Center Tarrant_County 2010-2050 Single year age.csv" Projections may be downloaded from
# http://txsdc.utsa.edu/Data/TPEPP/Projections/. Estimates for 2001-2009 were downloaded from the DSHS Center For Health
# Statistics site. These files were originally sourced form the Texas Demographic Center, but used aggregated age
# groups.
# ======================================================================================>
# --- Counties to include in the saved population data
# counties <-  c("Tarrant", "Dallas","Harris", "Travis", "Bexar", "El Paso",
#                "Rockwall", "Williamson", "Nueces", "Lubbock", "Johnson", "Parker",
#                "Wise", "Denton", "Ellis", "Collin","Kaufman", "Fort Bend",
#                "Hidalgo", "Texas")
#
# load("data-raw/population_df.rda")
# pop_df <- population_df$census |>
#   filter(area.name  %in% counties) |>
#   select(area.name, fips) |>
#   unique() |>
#   mutate( across( everything(), as.character))
#
# county_fips <- purrr::set_names(x = pop_df$fips, nm = pop_df$area.name)
# rm(pop_df, population_df)

# ==== Projections processing =======
print("Constructing population projection tables")
fn <- file.path(paths$population, "projections", "2018_vintage_2010-2050_projections.csv")
#file.exists(fn)

# col.types <- cols(
#     # migration_scenario_num  = col_double(),
#     # migration_scenario_char = col_character(),
#     year            = col_integer(),
#     FIPS            = col_character(),
#     area_name       = col_character(),
#     age_in_yrs_num  = col_double(),
#     age_in_yrs_char = col_character(),
#     .default        = col_double()
#     )

col.types <- cols(
  # migration_scenario_num  = col_double(),
  # migration_scenario = col_character(),
  year            = col_integer(),
  FIPS            = col_factor(),
  area_name       = col_factor(),
  age_in_yrs_num  = col_integer(),
  age_in_yrs_char = col_factor(),,
  .default        = col_integer()
)

process_age_char <- compose(
  ~ str_replace(., "5\\+", "5 \\+"),
  ~ str_trim(., side = "both"),
  ~ str_remove_all(.x, regex("Ages", ignore_case = TRUE)),
  ~ str_remove_all(.x, regex(" (ye?a??rs?|Ages)", ignore_case = TRUE))
)

read_csv(file = fn, n_max = 2) |> View()

print("Reading population projections")
projections <- read_csv(file=fn, col_types = col.types) #, col_select = c(-migration_scenario_num, -migration_scenario_char))

projections <- projections %>%
  # clean column names
  rename_with(~ gsub("_",".",.x) %>% tolower(), everything()) %>%
  rename(age.num   = age.in.yrs.num,
         age.char  = age.in.yrs.char
  ) %>%
  mutate(type      = factor("Projection"),
         area.name = fct_relabel(area.name, ~str_remove(.x, " County")),
         area.name = fct_recode(.f = area.name, Texas = "State of Texas"),
         fips      = fct_relabel(fips, ~ paste0("48", str_pad(.x, width = 3, pad = "0", side = "left")))
  )
county_fips <- projections$fips |> unique() |> set_names( unique(projections$area.name))

# local function to process each df
process_df <- function(df) {

  # pivot columns to longer format
  df <- df |>
    setDT() |>
    melt(id.vars         = c("year", "fips", "area.name", "age.num", "age.char", "type"),
         variable.name   = "race.ethnicity",
         variable.factor = TRUE,
         value.name      = "population",
         verbose         = FALSE)

  #new_df <- as.data.frame(new_df)

  race_eth_levels <- levels(df$race.ethnicity)
  race_eth_levels[grepl("(white|black|hispanic|asian|other)\\.total",race_eth_levels)] <- race_eth_levels[grepl("(white|black|hispanic|asian|other)\\.total",race_eth_levels)] |>
    gsub(pattern = "\\.total", replacement = "", x = _)

  race_eth_levels <- str_replace_all(string = race_eth_levels, pattern = "^nh\\.", replacement = "")

  levels(df$race.ethnicity) <- race_eth_levels
  rm(race_eth_levels)

  df[, c("race.eth", "sex") := tstrsplit(race.ethnicity, split = ".", fixed = TRUE, fill = NA)][
    , sex := fct_na_value_to_level(sex, level = "All")][
    , race.eth := factor(race.eth)] [
    , c("race.ethnicity", "age.num") := NULL]

  levels(df$race.eth) <- levels(df$race.eth) |> str_replace(pattern = "total", "All")

  return(df)
}

# split the projections by county and process each one to save memory and working time.  Row bind the list to finish
dfs <- split(x = projections, f = projections$area.name) |>
  map(process_df, .progress = "Processing State Data Center Projections") # process each data frame in the dfs

dfs_bound <- rbindlist(dfs)
levels(dfs_bound$age.char) <- process_age_char(levels(dfs_bound$age.char))

rm(dfs, projections, fn,col.types, process_df)
gc(full = TRUE)

#save(file = paste0(paths$population,"/Projections/Population_Proj_2010-2050.rdata"),projections)
arrow::write_parquet(sink = file.path(paths$population, "/Projections/Population_Proj_2010-2050.parquet"),
                       x = dfs_bound)

years <- range(dfs_bound$year)
dfs_bound[ , (c("fips", "type")) :=NULL]
setcolorder(dfs_bound, c("year", "area.name", "sex", "age.char", "race.eth", "population"))

cat("Creating tarr_pop object, expect to wait a few seconds...\n")
tdc.projections <- dfs_bound |>
  as.tarr_pop() |>
  set_source_url(
    pop_type = "Projections",
    url = "https://demographics.texas.gov/Projections/2018/#pageContent",
    nm = str_glue("Texas Demographic Center population projections for {years[1]} through {years[2]}.")
  )

rm(dfs_bound, years)
cat("Saving tdc.projections table to the data folder.\nThis takes a minute...\n")
usethis::use_data(tdc.projections, internal = FALSE, overwrite = TRUE, compress = "xz")
usethis::use_data(county_fips, internal = FALSE, overwrite = TRUE, compress = "xz")
rm(tdc.projections)
gc(full = TRUE)

# ==== Estimates processing ====
print("Reading population estimate files since 2010")


#' Reads population estimates files downloaded from the Texas Demographic Center.
#' @param .pattern a regex pattern to match the files to read.
#' @param .counties a character vector of county names to include in the estimates.
#' @returns A list of data frames, each containing population estimates for a specific year.
read_estimate <- function(.pattern, .counties){
  # get the files to read
  pth   <- file.path(paths$population,"Estimates/Texas Demographic Center/asre")
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

tdc.estimates <- bind_rows(csvs) |>
  relocate(year, .after = asian_female) |>
  setDT()

rm(csvs)

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
                       .fun = ~process_age_char(.x) |> tarr::as.age_group() |> as.character()) ][
  , age := ordered(age, levels = ord_ages(age))][
    , c("race.eth", "sex") := tstrsplit(race.sex, split = ".", fixed = TRUE, fill = NA)][
, sex := fct_na_value_to_level(sex, level = "All")][
, race.eth := factor(race.eth)][
, "race.sex" := NULL][
, fips := NULL] |>
  setnames(old = c("age", "county"), new = c("age.char", "area.name"))

# save the estimates file to a parquet file for future reading
asre <- file.path(paths$population, "Estimates/Texas Demographic Center/asre/Estimates.parquet")
write_parquet(x = tdc.estimates, file = asre)
rm(asre)

rm(columns, read_estimate, ord_ages)

years <- tdc.estimates$year |> range()
setcolorder(tdc.estimates, c("year", "area.name", "sex", "age.char", "race.eth", "population"))

tdc.estimates <- tdc.estimates |>
  as.data.frame() |>
  as.tarr_pop() |>
  set_source_url(
    pop_type = "Estimates",
    url = "https://demographics.texas.gov/Estimates/2023/",
    nm = str_glue("Population estimates from the Texas Demographic Center for {years[1]} through {years[2]}")
    )

usethis::use_data(tdc.estimates, internal = FALSE, overwrite = TRUE, compress = "xz")
rm(tdc.estimates, years, process_age_char)
gc(full = TRUE)

# ==== Process 2001-2009 county estimates data ====
# These files contain age groups and are best saved a seprate array.
#
# estimates were saved from the DSHS Center for Health Statistics as excel files
# Age groups are only available for:
# - single year 0-21,
# - five year age groups up to 85+
# - and total number shown as "all"
# print("Processing 2001-2009 population estimate files")
# fns <- list.files(pattern = "Dtl200[1-9]{1}.xls", path = paste0(paths$population,"/Estimates"))
# fns <- paste(paste0(paths$population,"/Estimates"), fns, sep = "/")
# sheets <- map(fns, function(x) read_excel(path = x,
#                                           range = "A1:S11731",
#                                         col_names = TRUE,
#                                         col_types = "text",
#                                         #guess_max = 100,
#                                         sheet=1,
#                                         progress = FALSE),
#               .progress = TRUE)
#
# # make sure all the names in the sheets are the same and in the same order as the first sheet
# nms <- names(sheets[[1]])
# sheets[3:6] <- map(3:6, function(x) sheets[[x]] %>%
#                 select(CONAME, YEAR, AGE, CNTY, everything()) %>%
#                 setNames(.,nm = nms))
#
# sheets[7:9] <- map(7:9, function(x) sheets[[x]] %>%
#                      setNames(.,nms))
# rm(nms)
#
# sheets[[3]] |> names()
# tdc.estimates |> names()
#
# # fix the column names to match projection and recent estimate data frame
# est01_09 <- bind_rows(sheets) %>%
#   mutate(across( c(Year, Total:OtherFemale), as.integer)) |>
#   rename_with(~str_replace(.x,"Anglo", "White"), .cols = contains("Anglo")) %>%
#   rename_with(~str_remove(.x, "Tot") %>% paste0(.,".Total"),
#               .cols = matches(match = "Tot[A-Z]", ignore.case = F)) |>
#   select(-Fips)
#
# est_nms <- names(est01_09) %>%
#   str_replace_all(., c("Male$" = ".Male", "Female$" = ".Female", "AgeGroup"="Age.Group")) %>%
#   str_to_lower(.)
#
# est_nms
# est_nms[est_nms %in%  c("area" )] <- c("area.name")
# est01_09 <- set_names(est01_09,est_nms)
# #names(est01_09)
#
# format_age_group <- compose(
#   ~ifelse(nchar(.) == 2,
#           as.character(suppressWarnings(as.integer(.))),
#           .),
#   ~recode(., "05-09" = "5-9", "00-04" = "0-4"),
#   ~str_trim(.,side = "both"),
#   ~str_remove_all(.x,"'"))
#
#
# # add rename age.groups to age.char
# est01_09 <- est01_09 %>%
#   mutate(age.group  = format_age_group(age.group),
#          area.name  = str_to_title(area.name)#,
#          #fips       = ifelse(str_detect(area.name, "Texas"), "000", convert_2_fips(fips)
#   ) %>%
#   filter(#area.name %in% counties,
#          !age.group %in% c("18+","21+","65+")) %>%
#   mutate(
#     age.iv          = tarr::as.age_group(age.group),
#     type            = "Estimate",
#     migration.scenario.num  = NA,
#     migration.scenario.char = NA) %>%
#   rename(age.char   = age.group) %>%
#   pivot_longer(cols = total:other.female, names_to = "sex_race", values_to = "population") %>%
#   mutate(sex        = str_extract(sex_race, "(male|female|total)") %>%
#                       str_replace("total", "All"),
#          race.eth   =  str_extract(sex_race, "(black|hispanic|other|white)") |> str_to_title(),
#          age.char   = ifelse(age.char == "ALL", "All", age.char)
#          ) %>%
#   replace_na(list(race.eth = "All"))
#
# est01_09 <- est01_09[,names(tdc.estimates)]
# est01_09$age.char |> sort_values()
#
#
# # Final pop data frame and last adjustments to fields
# pop <- bind_rows(tdc.estimates,est01_09) |>
#   mutate(sex  = str_to_title(sex),
#          year = as.integer(year),
#          #area.name = factor(x = area.name, levels = counties),
#          ethnicity = case_match(.default = "All",
#                                 race.eth,
#                                 "Hispanic" ~ "Hispanic"
#          ),
#          race = case_match(.default = race.eth,
#                            race.eth,
#                            "Hispanic"  ~ "All"),
#          across( c(ethnicity, race), ~factor(.x, levels = sort_values(.x))),
#           age.char  = factor(age.char),
#          across( c(area.name, sex, type), factor)
#          )
# #glimpse(pop)
#
#
# pop <- pop[, c("year", "area.name", "type", "sex", "age.char",
#                 "race", "ethnicity", "population")]
#
#
# map(pop[, c("sex", "race", "ethnicity")], table)
#
# glimpse(pop)
# range(est01_09$year)
# pop <- set_source_url(
#   obj = pop,
#   nm  = "Population estimates from the Texas Demographic Center and DSHS Center for Health Statistics",
#   url = "https://demographics.texas.gov/")
#


#rm(fns, sheets, est_nms, convert_2_fips, format_age_group, pop_1, est01_09)
