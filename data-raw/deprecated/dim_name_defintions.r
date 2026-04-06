# Objects for array  dimensions definitions.  Saved as small objects and lazily loaded when need
#


# data-raw/make_label_objects.R
# Assumes you have `tmp` in memory (your big list of dimnames/labels),
# OR you can assign tmp <- <paste-your-list>.

# tmp is used to hold the diemnsion labels excpet area.name
tmp <-
  list(census.estimates = list(
    year = c("2010", "2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020",
             "2021", "2022", "2023", "2024"),
    sex = c("All", "Female", "Male"),
    age.char = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                 "65-69", "70-74", "75-79", "80-84", "All", "85 +"),
    race = c("All",
             "Asian", "Asian or in combination", "Black", "Black or in combination",
             "American Indian and Alaska Native", "American Indian and Alaska Native or in combination",
             "Hawaiian or Pacific Islander", "Hawaiian or Pacific Islander or in combination",
             "Two or more", "White", "White or in combination"),
    ethnicity = c("All",
                  "Hispanic", "Non-Hispanic")),
    census = list(
      year = c("2000", "2010", "2020"),
      sex = c("All", "Female", "Male"),
      age.char = c("< 1", "1", "10", "100-104", "105-109", "11",
                   "110 +", "12", "13", "14", "15", "16", "17", "18", "19", "2",
                   "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "3",
                   "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "4",
                   "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "5",
                   "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "6",
                   "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "7",
                   "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "8",
                   "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "9",
                   "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "All"),
      race = c("All", "American Indian And Alaska Native", "Asian",
               "Black", "Hawaiian Or Pacific Islander", "Other", "Two Or More","White"),
      ethnicity = c("All", "Hispanic", "Non-Hispanic")),
    census.zcta.estimates = list(
      end.year = c("2011", "2012",
                   "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020",
                   "2021", "2022", "2023"),
      zip.code = c("75019", "75022", "75028",
                   "75038", "75050", "75051", "75052", "75054", "75061", "75062",
                   "75063", "75067", "75104", "75261", "76001", "76002", "76005",
                   "76006", "76008", "76009", "76010", "76011", "76012", "76013",
                   "76014", "76015", "76016", "76017", "76018", "76020", "76021",
                   "76022", "76023", "76028", "76034", "76035", "76036", "76039",
                   "76040", "76044", "76051", "76052", "76053", "76054", "76058",
                   "76060", "76063", "76065", "76071", "76084", "76092", "76102",
                   "76103", "76104", "76105", "76106", "76107", "76108", "76109",
                   "76110", "76111", "76112", "76114", "76115", "76116", "76117",
                   "76118", "76119", "76120", "76123", "76126", "76127", "76129",
                   "76131", "76132", "76133", "76134", "76135", "76137", "76140",
                   "76148", "76155", "76164", "76177", "76179", "76180", "76182",
                   "76244", "76247", "76248", "76262")),
    seer_grouped_age = list(
      year = c("1990", "1991", "1992", "1993", "1994", "1995",
               "1996", "1997", "1998", "1999", "2000", "2001", "2002",
               "2003", "2004", "2005", "2006", "2007", "2008", "2009",
               "2010", "2011", "2012", "2013", "2014", "2015", "2016",
               "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
       sex = c("Male", "Female"),
       age.char = c("0","1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                    "65-69", "70-74", "75-79", "80-84", "85", NA),
       race = c("White", "Black", "American Indian/Alaskan Native", "Asian or Pacific Islander"),
       ethnicity = c("Non-Hispanic", "Hispanic")),
       seer_single_age = list(
         year = c("1990", "1991", "1992", "1993", "1994", "1995",
                  "1996", "1997", "1998", "1999", "2000", "2001", "2002",
                  "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                  "2010", "2011", "2012", "2013", "2014", "2015", "2016",
                  "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
         sex = c("Male", "Female"),
         age.char = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                      "11","12", "13", "14", "15", "16", "17", "18", "19", "20","21", "22",
                      "23", "24", "25", "26", "27", "28", "29",
                      "30", "31", "32", "33", "34", "35", "36", "37", "38",
                      "39", "40", "41", "42", "43", "44", "45", "46", "47",
                      "48", "49", "50", "51", "52", "53", "54", "55", "56",
                      "57", "58", "59", "60", "61", "62", "63", "64", "65",
                      "66", "67", "68", "69", "70", "71", "72", "73", "74",
                      "75", "76", "77", "78", "79", "80", "81", "82", "83",
                      "84", "85", "86", "87", "88", "89", "90"),
         race = c("White","Black", "American Indian/Alaskan Native", "Asian or Pacific Islander"),
         ethnicity = c("Non-Hispanic", "Hispanic")),
    tdc.estimates = list(
      year = c("2011", "2012", "2013", "2014", "2015", "2016",
               "2017", "2018", "2019", "2021", "2022"),
      sex = c("All", "female", "male"),
      age.char = c("0", "1", "2", "3", "4", "5", "6", "7",
                   "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
                   "18", "19", "20", "21", "22", "23", "24", "25", "26",
                   "27", "28", "29", "30", "31", "32", "33", "34", "35",
                   "36", "37", "38", "39", "40", "41", "42", "43", "44",
                   "45", "46", "47", "48", "49", "50", "51", "52", "53",
                   "54", "55", "56", "57", "58", "59", "60", "61", "62",
                   "63", "64", "65", "66", "67", "68", "69", "70", "71",
                   "72", "73", "74", "75", "76", "77", "78", "79", "80",
                   "81", "82", "83", "84", "85 +", "85", "86", "87", "88",
                   "89", "90", "91", "92", "93", "94", "95 +", "All"),
      race.eth = c("All",
                   "asian", "black", "hispanic", "other", "white")),
    tdc.projections = list(
      year = c("2010", "2011", "2012", "2013", "2014", "2015",
               "2016", "2017", "2018", "2019", "2020", "2021", "2022",
               "2023", "2024", "2025", "2026", "2027", "2028", "2029",
               "2030", "2031", "2032", "2033", "2034", "2035", "2036",
               "2037", "2038", "2039", "2040", "2041", "2042", "2043",
               "2044", "2045", "2046", "2047", "2048", "2049", "2050"),
      sex = c("female", "male", "All"),
      age.char = c("All", "< 1", "1", "2", "3", "4", "5", "6",
                   "7", "8", "9", "10", "11", "12", "13", "14", "15", "16",
                   "17", "18", "19", "20", "21", "22", "23", "24", "25",
                   "26", "27", "28", "29", "30", "31", "32", "33", "34",
                   "35", "36", "37", "38", "39", "40", "41", "42", "43",
                   "44", "45", "46", "47", "48", "49", "50", "51", "52",
                   "53", "54", "55", "56", "57", "58", "59", "60", "61",
                   "62", "63", "64", "65", "66", "67", "68", "69", "70",
                   "71", "72", "73", "74", "75", "76", "77", "78", "79",
                   "80", "81", "82", "83", "84", "85", "86", "87", "88",
                   "89", "90", "91", "92", "93", "94", "95 +"),
      race.eth = c("asian","black", "hispanic", "other", "All", "white")))


#-----------------------------
# helpers
#-----------------------------
trim_chr <- function(x) {
  x <- as.character(x)
  x <- gsub("^\\s+|\\s+$", "", x)
  x
}

drop_na_chr <- function(x) {
  x <- x[!is.na(x)]
  x
}

title_sex <- function(x) {
  # Standardize sex labels where they appear lowercase
  x <- trim_chr(x)
  x <- ifelse(tolower(x) == "female", "Female",
              ifelse(tolower(x) == "male", "Male", x))
  x
}

#-----------------------------
# pull a few shared vectors
#-----------------------------

# Canonical county names:
# Load county_fips vector with names of counties and values being FIPS codes. NOTE there is a subtle bug to be avoided
# here.  The TDC files list Texas as the first entry in area.name  The census and seer cubes list Texas in alphabetical
# order.The county names vector is TDC friendly.  The names must be sorted for the census and seer arrays .
load("data/county_fips.rda")
county_names <- trim_chr(names(county_fips))
#county_names <- trim_chr(tmp$census.estimates$area.name)


# ZCTA keys
zcta_end_year <- trim_chr(tmp$census.zcta.estimates$end.year)
zcta_zip_code <- trim_chr(tmp$census.zcta.estimates$zip.code)

#-----------------------------
# Years by dataset
#-----------------------------
years_census_decennial <- trim_chr(tmp$census$year)
years_census_estimates <- trim_chr(tmp$census.estimates$year)

years_seer <- trim_chr(tmp$seer_single_age$year)           # same as grouped (in your paste)
years_tdc_estimates <- trim_chr(tmp$tdc.estimates$year)
years_tdc_projections <- trim_chr(tmp$tdc.projections$year)

#-----------------------------
# Sex label sets
#-----------------------------
sex_census <- trim_chr(tmp$census$sex)                     # c("All","Female","Male")
sex_census_estimates <- trim_chr(tmp$census.estimates$sex) # same as above

sex_seer <- title_sex(tmp$seer_single_age$sex)             # c("Male","Female") as given
# if you prefer consistent ordering:
sex_seer <- c("Female", "Male")

sex_tdc_est <- title_sex(tmp$tdc.estimates$sex)            # had lowercase
sex_tdc_proj <- title_sex(tmp$tdc.projections$sex)          # had lowercase

#-----------------------------
# Age label sets
#-----------------------------
ages_census_5y <- trim_chr(tmp$census.estimates$age.char)

ages_census_1y <- trim_chr(tmp$census$age.char)

ages_seer_5y <- drop_na_chr(trim_chr(tmp$seer_grouped_age$age.char))
# Your seer_grouped_age includes "85" and NA; docs say 85+.
# Keep exactly what you stored, minus NA:
ages_seer_5y <- ages_seer_5y

ages_seer_1y <- drop_na_chr(trim_chr(tmp$seer_single_age$age.char))

ages_tdc_mixed <- trim_chr(tmp$tdc.estimates$age.char)      # mixed single + grouped + All
ages_tdc_1y <- trim_chr(tmp$tdc.projections$age.char)       # single-year-ish + All + "< 1" + "95 +"

#-----------------------------
# Race / ethnicity
#-----------------------------
# Census decennial races:
race_levels_census <- trim_chr(tmp$census$race)

# Census estimates races (includes "or in combination"):
race_levels_census_estimates <- trim_chr(tmp$census.estimates$race)

# SEER races/ethnicity:
race_levels_seer <- trim_chr(tmp$seer_single_age$race)
ethnicity_levels_seer <- trim_chr(tmp$seer_single_age$ethnicity)

# Census ethnicity:
ethnicity_levels_census <- trim_chr(tmp$census$ethnicity)
ethnicity_levels_census_estimates <- trim_chr(tmp$census.estimates$ethnicity)

# TDC: you stored combined "race.eth" rather than separate dims
race_eth_levels_tdc_estimates <- trim_chr(tmp$tdc.estimates$race.eth)
race_eth_levels_tdc_projections <- trim_chr(tmp$tdc.projections$race.eth)

rm(tmp)

#-----------------------------
# Package objects you can save
#-----------------------------

# Geography label sets
county_levels <- county_names
zcta_levels <- zcta_zip_code
zcta_end_year_levels <- zcta_end_year

# Year label sets (by series)
years_census <- years_census_decennial

# Sex label sets (by series family)
sex_levels_census <- sex_census
sex_levels_seer <- sex_seer
sex_levels_tdc <- unique(c(sex_tdc_est, sex_tdc_proj))


#-----------------------------
# (Optional) quick checks
#-----------------------------
stopifnot(length(county_levels) > 200)
stopifnot(all(nzchar(county_levels)))
stopifnot(all(nzchar(ages_census_5y)))
stopifnot(!any(is.na(ages_seer_5y)))

#-----------------------------
# Save to package (run in package root)
#-----------------------------
# install.packages("usethis")
 usethis::use_data(
  county_levels,
  zcta_levels,
  zcta_end_year_levels,
  years_census,
  years_census_estimates,
  years_seer,
  years_tdc_estimates,
  years_tdc_projections,
  sex_levels_census,
  sex_levels_seer,
  sex_levels_tdc,
  ethnicity_levels_census,
  ethnicity_levels_census_estimates,
  ethnicity_levels_seer,
  ages_census_1y,
  ages_census_5y,
  ages_seer_1y,
  ages_seer_5y,
  ages_tdc_1y,
  ages_tdc_mixed,
  race_levels_census,
  race_levels_census_estimates,
  race_levels_seer,
  race_eth_levels_tdc_estimates,
  race_eth_levels_tdc_projections,
  overwrite = TRUE
)
