# ------------------------------------------------------------------------------------------------------------------->
# Script: MonRep_2025.r
# Description:
#  Creates heat map charts for cases reported in a month by condition for year to date compared to previous years.  This
#  script is a clean up and revision of "MonRep_2023.r". Rates are calculated and compared to previous years. Note that 
#  years 2020 and 2021 are generally excluded as report data for most diseases was minimal during the COVID-19 pandemic. 
#  The charts created from this script are saved to the Monthly Report, Year, and Month folder.
#  
# Steps:
# Download the latest EpiTrax export from "Monthly Report version 2", ensuring the beginning and ending dates
# are one day before and after the month of interest. Name the downloaded csv file as starting with "EpiTrax_*.csv" and
# place it in the epidemiology division shared drive at Data\Communicable Disease\Current Year\.
# Change the params argument in step #2 below for the report month and year, as well as the name of the EpiTrax_*.csv
# file you downloaded and saved. 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  February 2025
# ------------------------------------------------------------------------------------------------------------------->

# 1.  Setup needed packages and fonts -----------------------------------------------------------------------------
pkgs <- c("tidyverse", "tarr", "scales", "extrafont") |> 
  sapply(function(f) suppressMessages(suppressWarnings(library(package = f, character.only = TRUE))))
rm(pkgs)

## Uncomment the code below if this is the first time running the script on your computer. ---
# This loads many of the Windows fonts and makes them available for use.

# load windows fonts available - need to run only once on a machine
# loadfonts(device = "win")
# font_import()
# fonts()
# windowsFonts()

# 2. Set the params for the month and data file -------------------------------------------------------------------
params <- list(
  year  = 2024,
  month = 8,
  aud   = c("InLabels","ExLabels"),
  audience = "InLabels",      # 
  epitrax_file = "EpiTrax_Epi Monthly Report_ET_02.18.25.csv"
)

# 3. Read/Combine NBS and EpiTrax data ----------------------------------------------------------------------------
## Read and transform EpiTrax data to NBS format ----
epitrax_recs <- epitrax_import(.file = file.path(paths$data, params$epitrax_file), .return_missing_county = FALSE) |> 
  distinct(patient_event_id, .keep_all = TRUE) |> 
  tarr:::epitrax_2_nbs_format() 

# uncomment and run the following line  line if you have checked all the records with missing county should be in Tarrant
# epitrax_recs$County |> table()
# epitrax_recs <- epitrax_recs |> mutate(County = ifelse(County == "NA County", "Tarrant County", County))


## NBS records data combined with EpiTrax ----
nbs_recs <- baseData(.chooseNew = FALSE) 
dis <- bind_rows(Epitrax = epitrax_recs, NBS = nbs_recs, .id = "dataset") |> 
  mutate(County = str_replace_all(County, "County", "") |> str_trim()) 

## NBS records for previous year, EpiTrax for current year ----
NBS_lgl    <- (dis$dataset == "NBS") & (dis$Report.Date <= lubridate::mdy("12/31/2023"))
EpiTrx_lgl <- (dis$dataset == "Epitrax") & (dis$Report.Date > lubridate::mdy("12/31/2023"))
dis <- dis[ NBS_lgl | EpiTrx_lgl, ]

rm(NBS_lgl, EpiTrx_lgl, epitrax_recs, nbs_recs)

# 4. Use last six years of data, create variables to use ------------------------------------------------------------
## New variables,last six years, de-dupe ----
cases <- dis |>
  mutate(across(c(contains("Person"), Case.Status, Condition), as.character)) |> 
  mutate(Report.Month = month(Report.Date), 
         Year         = year(Report.Date),
         Event.Month  = month(Event.Date), 
         Event.Year   = year(Event.Date),
         County       = str_replace(County, "County County", "County")) |>  
  filter(Case.Status %in% c("Probable","Confirmed"),
         str_detect(County, "Tarrant"),
         !is.na(Condition),
         Year %in% params$year:(params$year-6),
         Report.Month <= params$month) |>  
  rename(
    Sex=Current.Sex.Code
  ) |> 
  distinct(Person.Name, Condition, Birth.Time, Event.Year, .keep_all = TRUE)


# 5. Change and simplify condition names --------------------------------------------------------------------------
#The first entry on the left is the simplified name and the second entry on the right is the original NBS name.
subs <- c(
  "Haemophilus influenzae, invasive"      =  "Haemophilus influenzae, invasive disease" ,
  "Hepatitis A"                           =  "Hepatitis A, acute",
  "Carbapenem-resistant Enterobacteriaceae" = "Carbapenem-resistant Enterobacteriaceae \\(CRE\\)", 
  #"Candida Auris"                        =  "Candida Auris",
  #"Candida Auris"                        =  "Candida auris, clinical",
  "Candida auris, colonization"           =  "Candida auris, colonization/screening",
  "Hemolytic Uremic Syndrome (HUS)"       =  "Hemolytic uremic",                                 
  #"Hemolytic Uremic Syndrome (HUS)"     =  "Hemolytic Uremic Syndrome, postdiarrheal",
  "Mpox"                                  =  "Monkeypox",                                 
  "Neisseria meningitidis"                =  "Neisseria meningitidis, invasive \\(Mening\\. disease\\)",                  
  "STEC/Escherichia coli"                 =  "Shiga toxin-producing Escherichia coli \\(STEC\\)",                       
  "Strep. pneumoniae, invasive"           =  "Streptococcus pneumoniae, invasive disease \\(IPD\\)",                    
  "Strep. Group A, invasive"              =  "Streptococcus, invasive Group A",                                     
  "Strep. Group B, invasive"              =  "Streptococcus, invasive Group B",                                     
  "Zika disease, non-congenital"          =  "Zika virus disease, non-congenital",                                  
  "Zika infection, non-congenital"        =  "Zika virus infection, non-congenital",                                
  "Zika disease, congenital"              =  "Zika virus disease, congenital",                                      
  "Zika infection, congenital"            =  "Zika virus infection, congenital",                                    
  "Chikungunya"                           =  "Chikungunya virus disease",                                           
  "Salmonellosis"                         =  "Salmonella, non-Paratyphi/non-Typhi",                                 
  "Haemophilus influenzae, invasive*"     =  "Haemophilus influenzae, invasive",
  "Varicella (Chickenpox)"                =  "Chickenpox",
  "West Nile non-neuroinvasive"           = "West Nile Fever",
  "West Nile non-neuroinvasive"           = "West Nile Virus, Non-neuroinvasive",
  "West Nile non-neuroinvasive"           = "West Nile Virus Non-neuroinvasive disease",
  "West Nile neuroinvasive"               = "West Nile Virus, Neuroinvasive",
  "West Nile neuroinvasive"               = "West Nile Encephalitis",
  "West Nile neuroinvasive"               = "West Nile Virus neuroinvasive disease",
  "West Nile neuroinvasive"               = "Encephalitis, West Nile",
  # added Vibrio changes July 2, 2024, Simplified Feb 18, 2025
  "Vibriosis, non-cholera species"       = "Vibrio vulnificus infection" ,
  "Vibriosis, non-cholera species"       = "Vibrio parahaemolyticus" ,
  "Vibriosis, non-cholera species"       = "Vibriosis, other or unspecified",
  "Vibriosis, non-cholera species"       = "non-cholera Vibrio species"
) 


## Change conditon names ----
conds <- cases$Condition |> unique() |> sort()
sub_lgl <- map_lgl(subs, \(x) str_detect(conds, regex(x, ignore_case = TRUE)) |> any())

## Function to change each condition to those in subs
to_subs <- function(.x, .y){
  rec_lgl <- str_detect(cases$Condition, regex(.x, ignore_case = TRUE))
  cases$Condition[rec_lgl] <<- .y
}
walk2(subs[sub_lgl], names(subs[sub_lgl]) , to_subs)
rm(subs, to_subs, conds, sub_lgl)

## Remove conditons we do not use in the report ----
cases <- cases |> 
  filter(! Condition %in% c("Rabies, animal","Foodborne Illness, NOS", "2019-nCoV"))

# 6. Cases from year month to selected month  ---------------------------------------------------------------------
ev.dt <- 1:params$month

## Year to date cases ----
ytd <- cases |>
  filter(Report.Month %in% ev.dt) |> 
  group_by(Condition,Year) |>
  summarise(Count=n(), .groups = "drop_last") |>  
  ungroup() |> 
  spread(Year,Count, fill = 0) |> 
  gather(key = Year,value=Count,-Condition) |> 
  mutate(Year = as.integer(Year))

## Show missing data for years a condition was not reportable ----
# report_year list has the condition name and year it began to be reportable.
report_year <- list(
  "Acute Flaccid Myelitis (AFM)"                  = 2016,
  "Carbapenem-resistant Enterobacteriaceae (CRE)" = 2014,
  "Multidrug-resistant Acinetobacter (MDR-A)"     = 2014,
  "Zika"                                          = 2016,
  "Chikungunya"                                   = 2014,
  "Candida auris, colonization"                   = 2021,
  "Candida auris, clinical"                       = 2021,
  "Candida auris"                                 = 2021,
  "COVID-19"                                      = 2020, 
  "Mpox"                                          = 2022)

iwalk(report_year, \(x, nm) ytd$Count[str_detect(ytd$Condition,nm) & ytd$Year < x] <- NA_real_)
ytd$Count[str_detect(ytd$Condition, "^Candida") & ytd$Year < 2021] <- NA_real_
rm(report_year)

# 7.  Create data frame for heat map ------------------------------------------------------------------------------
## Calculate Rates for each Condition and Year ----
### Tarrant Population for for four earlier years as this and last year are not available 
years <- ytd$Year |> unique() |> sort() 
years <- years[years < 2024]

pop <- county_population(pop = population$census.estimates, year = years, county = "Tarrant") |> 
  as_tibble() |> 
  select(year, area.name, population) 
rm(years)

# Local function to add years and simple population estimates to the population data frame. Supply the population
# dataframe and number of years to add and number of previous years to use for estimating the increase
# This function will need to be improved and added to the tarr.population package
add_pop_years <- function(pop_df = pop, n = 2, previous = 3, func = mean){
  add_pop <- pop_df |> 
    mutate(diff = population - lag(x = population, 1)) |> 
    arrange(year) |> 
    slice_tail(n = previous) |> 
    pull(diff) |> 
    func()
  
  for(i in seq_len(n)){
    new_row <- pop_df |> 
      slice_tail(n = 1) |> 
      mutate(year = year + 1,
             population = population + add_pop)
    pop_df <- rbind(pop_df, new_row)
  }
  pop_df
}

### Forecasted population for last two years using simple method
pop <- add_pop_years()

##  Safely calculate rates, returning NA if cases is below the valid number. Rate to 2 digits
calc_rate <- function(cases, pop, basis = 10^5, valid = 20){
  ret <- double(length(cases))
  ret <- round(cases / pop * basis, digits = 1)
  ret[cases < valid] <- NA_real_
  return(ret)
}


ytd <- left_join(ytd, pop |> select( -area.name), by = c("Year" = "year")) |> 
  mutate(rate = calc_rate(Count, population)) |> 
         #rate = ifelse(Condition %nin% suppress_cond, calc_rate(Count, population), NA)) |> 
  select( -population)

rm(pop, add_pop_years, calc_rate)


# 8. Create the heat data frame and scale the rates ---------------------------------------------------------------
## Scale the rate variables ----
scale_this <- function(x) {
  if(!is.numeric(x)) {
    return(x)
  }
  
  rescale(x)
}


# All condtions that have been reported in params$year
year_conditions <- cases |> 
  filter(Year == params$year) |> 
  pull(Condition) |> 
  unique()

heat <- ytd |> 
  #mutate(rate = ifelse(Year %in% c(2020, 2021), NA, rate)) |> 
  group_by(Condition) |> 
  mutate(Scale = ifelse(!is.na(rate), scale_this(rate), NA)) |> 
  ungroup() |> 
  as.data.frame() |>    # prevents warning messages
  # add labels to the data frame
  mutate(count_rate = ifelse(is.na(rate), 
                             as.character(Count), 
                             str_glue("{Count} ({format(rate, nsmall = 1)})")),
         count_rate  = ifelse(Year == 2020 & is.na(rate), "-", count_rate)
  ) |> 
  mutate(ExLabels = ifelse(is.na(Count),"NR",
                         ifelse(Count != 0 & Count < 5,"<5", count_rate)),
         InLabels = ifelse(is.na(Count),"NR", count_rate)) |> 
  select(- c(Count, rate, count_rate))

# Conditions and counts for report month
mon2 <- cases |>
  filter(Year == params$year, Report.Month %in% ev.dt) |>
  group_by(Condition, Report.Month) |>
  summarise(Count = n(), .groups = "drop_last") |>
  filter(Report.Month == params$month) |> 
  #spread(Report.Month, Cases,fill=0) |> 
  mutate(Year = month.abb[params$month],
         count_rate = as.character(Count), 
         Scale = NA, 
         ExLabels = ifelse(Count > 0 & Count < 5,"<5", count_rate),
         InLabels = count_rate) |> 
  _[, names(heat)]  

# Add conditions that are missing form the report month
missing_conditions <- setdiff(year_conditions, mon2$Condition)
new_rows <- data.frame(matrix(NA, nrow = length(missing_conditions), ncol = ncol(mon2))) |> 
  set_names( names(mon2) ) |> 
  mutate(Condition = missing_conditions,
         Year = mon2$Year[1], 
         ExLabels = "0",
         InLabels = "0")

mon2 <- rbind(mon2, new_rows) |> 
  arrange(Condition)

# Keep only those conditions that are present in the report year
heat <- rbind(heat,mon2) |> 
  filter(Condition %in%  year_conditions)

rm(ytd, ev.dt, missing_conditions)
year_conditions |> sort()


ttl <- ifelse(params$month == 1, # One month only
              paste0("   Cumulative Total (",month.abb[1],")"),
              paste0("   Cumulative Total (",month.abb[min(cases$Report.Month,na.rm=TRUE)],
                     "-",month.abb[params$month],")"))

selected_colors <- c(
  "COURTHOUSE GREEN"   = "#5D7E82",
  "JUNIPER GREEN"      = "#23513C",
  "VERDIGRIS"          = "#A6E2BE",
  "MIDNIGHT"           = "#284E5F",
  "SKY BLUE"           = "#2292D0",
  "COPPER"             = "#803B21",
  "PECAN"              = "#4C3113",
  "TEXAS GRANITE"      = "#DCB59E",
  "GRAPEFRUIT"         = "#CA6A53", 
  "PRAIRIE SKY"        = "#F2EFE8",
  "BLACK"              = "#000000",
  "WHITE"              = "#FFFFFF",
  "GOLD STAR"          = "#DEBC73",
  "LONE STAR BLUE"     = "#1D2856"
)

fill_colors <- list(
  low = selected_colors["COURTHOUSE GREEN"],
  mid = selected_colors["PRAIRIE SKY"] ,
  high = selected_colors["GRAPEFRUIT"]
)


makeHeat <- function(df,col){
  h <- ggplot(df,aes(x=Year,y=Condition)) +
    # draw the current month column
    geom_tile(data= df[df$Year==month.abb[params$month],],aes(x=Year, y=Condition),color="White", fill="White") +
    # draw the previous years columns
    geom_tile(data =df[df$Year!=month.abb[params$month],], aes(fill=Scale),color = "Grey20") +
    scale_fill_gradient2(name = "Rates",
                         low = fill_colors$low, 
                         mid= fill_colors$mid,
                         high= fill_colors$high, 
                         midpoint=.5, na.value="White",
                         breaks = c(0,.5,1),labels = c("Min","Median", "Max")
                         )+
    scale_x_discrete(expand=c(0,0), limits = rev(unique(df$Year)),position="top") +
    scale_y_discrete(limits = rev(unique(df$Condition))) +
    #scale_y_reverse()+
    labs(x=ttl,y="") +
    theme(axis.ticks=element_blank(),panel.grid = element_blank(),
          axis.text=element_text(colour = "Black"),
          legend.position = "right",
          text = element_text(size = 15, colour = "Black", family = "Arial"))+
    geom_text(label = df[[col]],color="grey10",size=4) +
    coord_cartesian()
  #coord_equal(ratio = .65)
  return(h)
}

# create the actual tables and save as png files
pls <- map(params$aud, ~ makeHeat(heat, .x)) |> 
  set_names(params$aud)

#pl_path <- file.path(yrPath, month.name[params$month])
pls[1]

# 9. Create folders for report/charts -----------------------------------------------------------------------------
## Create the year if it does not exist ----
yrPath <- paste(paths$monthReport,params$year ,sep="/")  # ensure the year is set up
if(file.exists(yrPath)==FALSE) {
  dir.create(yrPath)
}

## create the month folder for the report ----
out_folder <- paste(paths$monthReport, params$year, month.name[params$month],sep="/")  # create the month folder if needed
if(file.exists(out_folder) == FALSE){
  dir.create(out_folder)
}


iwalk(pls, ~{
  fn <- file.path(out_folder, paste0("Monthly_report_", .y, ".png"))
  ggsave(filename = fn, plot = .x, device = "png", width = 9, height = 9.5, units = "in")
})

rm(yrPath)