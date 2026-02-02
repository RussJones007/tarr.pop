# =============================================================================================>
# PopRetreive.r
# County population retrieval functions.
# including Tarrant Zip code population estimates
#
# The population data is from the Texas Demographic Center and prepared using a script in the
# Population/Projections folder
# Created 6/1/2018
# R Jones
#
# Revised 6/3/18 - added ability to use custom break points to age.cat
# Revised 7/30/2018 - added to the tarr package
# Revised 8/6/2018 - retrieve_county_population Revised 8/10-12/18.  Added
# capability to use "typical" race ethnicity in the retrieve_county_population
# function
# Revised 10/23/18 - Plot_Age_Group_Year() added to package
# Revised 4/26/2019 - Revised plot_age_group() to show the total number of cases
# in a year and to include in that total the number suppressed.  Therefore the
# number shown in the graph may not match the total.
# =============================================================================================>

# use_test()
# ====Function retrieve_county_population ===========
#' Retrieve County Population
#'
#' Returns a data frame with the requested county(s) population. One or more years may be requested.
#' This function was originally written to handle population data frames.  The package has been re-factored to use
#' tables and arrays.  This function has been updated to use either a data frame or a tarr_pop object.
#' The .age.groups argument is now ignored.
#' For an updated and more efficient method of retrieving population date, see the [county_population()] functions.
#'
#' The type of population count returned is dependent on the .df argument.  This argument is just a way to reference
#' the arrays available from the [population] list.
#'
#' [Population](population) counts are available for Texas and specific counties including:
#' - Tarrant (default):
#' - Texas, the whole state
#' - counties surrounding Tarrant,
#' - counties that comprise the metroplex,
#' - and other urban counties in Texas.
#'
#' @param .pop_df a data frame available in the [population] list, with the exception of the 'zcta' data frame.
#' @param .year an integer vector of the year(s) of data to return. When multiple years are specified in a an integer
#'   vector, each year will be returned.
#' @param .race a character vector (default is "All" ) selects the race(s) to select.  Allowed values are dependent
#'   on the data frame used passed to the .df argument.  Refer to the [population] help topic for race categories that
#'   may be used for each data frame.
#' @param .ethnicity a character vector with the following values "Hispanic", or "Non-Hispanic", "All" (default).
#' @param .sex a character vector  of "Female", "Male", "All" (default)
#' @param .county a character vector the county name(s) to return. The default is "Tarrant"
#' @param .age are the ages. "ALL" is the default. A single year age or vectoi of ages may be used. The vecotr can be
#' generated manually or using seq or the ':' operator, e.g., 10:25
 #' @param .age.groups A vector of age cut points can also be passed for a custom age grouping such as using the
#'   [rage::age.groups] list.
#'
#' @return dataframe with the requested population \itemize{
#' \item year: the year(s) as an integer
#' \item fips:  the county FIPS code as a character string
#' \item area.name: name of the Texas county in title case. (e.g., El Paso)
#' \item race:  the race for each record, defaults to "All"
#' \item sex: can be "Female", "Male", or "All" which is the total of male and female.
#' \item age: character string of the age or age group, e.g., "10-13".
#' \item population: is the population for the group.
#' }
#' @seealso the \code{\link{age.groups}} list for predefined age groups.
#' @examples
#' # Retrieve census population estimates for Hispanic Black, and White Females in 2017 for Tarrant County
#'  tarrant <- retrieve_county_population( .pop_df = population$census.estimates,
#'                                              .year = 2017,
#'                                              .sex  = "Female",
#'                                              .race = c("Black", "White"),
#'                                              .ethnicity = "Hispanic")
#'  tarrant |> head()
#'  unique(tarrant$race)
#'
#'
#'  # Get the census population for Dallas and Denton counties in 2000, 2010 and 2020
#'  neighbors <- retrieve_county_population( .pop_df = population$census,
#'                                          .year = c(2000, 2010, 2020),
#'                                          .county = c("Dallas", "Denton"))
#'  neighbors |> head(15)
#'  unique(neighbors$year)
#'
#' @export
retrieve_county_population <- function(.pop_df,
                                       .year,
                                       .race      = "All",
                                       .ethnicity = "All",
                                       .sex       = "All",
                                       .county    = "Tarrant",
                                       .age       = "All"
                                       ) {

  # added this line when converting to tarr_pop arrays instead of data frames. 2/17/2025
  if(inherits(.pop_df, "tarr_pop", which = FALSE)) .pop_df <- as.data.frame(.pop_df)

  # check that a data frame with the correct fields are available.  Checks only field names
  # population$census is used as the prototype for the required field names
  assert_that(.pop_df %has_name% (population$census |> dimnames() |> names()),
              msg = "The data frame passed in .pop_df does not have all the required fields for processing")

  # check that passed argument values are available for the data frame chosen
  avail <- get_available_values(.pop_df)

  argument_list <- list(year      = .year,
                        race      = .race,
                        ethnicity = .ethnicity,
                        sex       = .sex,
                        area.name = .county)

  arg_check <- purrr::map_lgl(names(argument_list) ,\(x) any(argument_list[[x]] %in% avail[[x]]))
  if(! all(arg_check)) {
    stop("Argument ", names(argument_list)[!arg_check], " does not contain a value")
  }

  ret <- .pop_df |>
    filter( year %in% argument_list$year ,
            race  %in% argument_list$race,
            ethnicity  %in% argument_list$ethnicity,
            sex %in% argument_list$sex,
            area.name %in% argument_list$area.name
    )

  argument_list$age.char  = as.character(.age)
  ret <- ret |>
    filter(age.char %in% argument_list$age.char)

  # return the filtered data frame with fields in the same order as .pop_df
  flds <- names(.pop_df)[names(.pop_df) %in% names(ret)]
  ret <- ret |>
    mutate( across(where(is.factor), droplevels)) |>
    arrange(year, area.name, sex) |>
    select(all_of(flds))

  attr(ret, "filter") <- argument_list

  return(ret)
}

# ==== Function available_values ====
#' Values available in the fields for the different population data frames.
#' Used internally
#'
#' @param df is the population data frame such as those created from the population list of tables.
#' @return a list of field names from df.  The field names that are either a factor of character vector.
#' When it is a factor the levels are returned, for a character vector the unique values.
#'
#' @keywords internal
get_available_values <- function(df) {
  # local function
  levels_or_unique <- function(field){
    cl <- class(field)[1]
    #browser()
    ret <- switch(EXPR = cl,
      ordered   = ,
      factor    = levels(field),
      numeric   = ,
      integer   = ,
      character = unique(field) |> sort()
    )
    ret
  }

  map(df |> select(!population), levels_or_unique)
}


# ====Function plot_age_group ===========
#' Plot Age Groups by Year
#'
#' Plots the age-group specific incidence or rates by year. Small bar charts are shown in multiple panels. Currently
#' rates are calculated for the county level only using population projections with migration scenario 1 . The returned
#' plot is formatted for external or internal use as specified in the .external argument. See return value for the
#' differences in the plots returned.
#'
#' @param .df is the dataframe that has the required date and age fields. All cases in .df with an age and date are used
#'   for the plot.
#' @param .dateFld is the unquoted date field in .df either date format or as an year integer.
#' @param .ageFld is the unquoted field in .df with the age used to calculate age group.
#' @param .condition is the condition as a character string and is used in the plot title.
#' @param .years are the year(s) to display age groups. Defaults to 2010 through the current year.
#' @param .age.groups is vector of integers defining the age group.  See the [rage::age.groups] list for pre-defined age
#'   groups.
#' @param .county is the county to use for the population denominator
#' @param .rates if TRUE (default) plots rates, FALSE plots incidence.
#' @param .fill is the color as text (e.g.,"navyblue") to use for filling the chart bars.
#' @param .external when TRUE (default) suppresses counts between 0 and .suppress.level
#' @param .suppress.level an integer (default is 10) with the minimum number of cases to show or calculate a rate.
#' @param .th is the ggplot theme to use. ggthemes::theme_tufte() is the default.
#'
#' @return small multiples of a ggplot bar chart for each year. The plot returned is dependent on the whether the plot
#'   is for external or internal use
#'
#'   External use plots,
#'   \itemize{
#'        \item Incidence for less than .suppress.level cases results in
#'        suppression.  Caution should be used with this chart as suppressed
#'        bars could be mis-interpreted as zero cases.  The total number of
#'        cases for each year are shown unless the total is below .suppress.level
#'        \item Rates are calculated with a minimum of .suppress.level cases and
#'        shown with a hollow bar for display purposes.
#'        }
#'
#'   Internal use plots
#'      \itemize{
#'        \item Incidence, all case counts are shown regardless of .suppress.level
#'        \item Rates are shown, however those calculated with .suppress.level
#'        or fewer cases are labeled as unstable and the bar displayed with
#'        increased transparency.  The legend will show these bars as "Unstable"
#'        }
#'
#' @seealso [rage::age.groups], variable to use pre-defined age categories
#' @export
plot_age_group_year <- function(.df, .dateFld, .ageFld, .condition,
                                .fill = "navy",
                                .years = 2010:lubridate::year(lubridate::today()),
                                .age.groups = rage::age.groups$Yr.10, .rates = TRUE,
                                .external = TRUE,
                                .suppress.level = 10,
                                .th = ggthemes::theme_tufte(ticks=F),
                                .county = "Tarrant"
                                 ) {
  # .df = dis
  # .dateFld = expr(Event.Date)
  # .ageFld = expr(AgeYrs)
  # .age.groups = age.groups$Yr.10
  # .condition = "Shigellosis"
  # .fill = "sienna"
  # .years = c(2010:2018)
  # .rates = F
  # .th = ggthemes::theme_tufte(ticks=F)
  # .external = T
  # .suppress.level = 5

  #checks for correct argument types
  stopifnot(is.data.frame(.df))
  stopifnot(is.character(.condition))
  stopifnot(is.numeric(.years) & length(.years) >0)
  stopifnot(base::tolower(.fill) %in% grDevices::colors())
  stopifnot(is.numeric(.age.groups))
  stopifnot(is.logical(.external))

  # quasiquote bare field names
  .dateFld <- rlang::enquo(.dateFld)
  .ageFld  <- rlang::enquo(.ageFld)


  # add transparency for the bar fill color to 75% opacity
  fill.alpha <- grDevices::adjustcolor(col = .fill,alpha.f = 0.3)
  fill.bar   <- grDevices::adjustcolor(col = .fill,alpha.f = 0.75)

  # check for NA in age and date fields and remove
  # if records are removed, then post warning
  origCount <- nrow(.df)
  .df <- .df %>%
    filter(!is.na(!!.ageFld),
           !is.na(!!.dateFld))
  if(origCount < nrow(.df)) {
    warning(paste(origCount-nrow(.df),"records had missing .ageFld or .dateFld and are not represented in the plot"))
  }
  rm(origCount)

  # df.proc is the processed form of .df using only the date, and age fields from
  # the original .df and adds a Years field.
  df.proc <- .df %>%
    mutate(Years = lubridate::year(!!.dateFld)) %>%
    filter(Years %in% .years) %>%
    select(Dates = !!.dateFld, !!.ageFld, Years) %>%
    mutate(Age.Group = age.cat(.ages = !!.ageFld, .by = .age.groups))

  # if no records are available, return an empty plot.
  if(nrow(df.proc) == 0){
    #warning("No records to plot")
    ret <- ggplot()+
      labs(title = paste(.condition,"no data"))
    return(ret)
  }

  # check that each requested year has cases,
  # if some of the requested years are not present
  # issue a warning
  years.present <- df.proc %>%
    distinct(Years) %>%
    pull(Years)

  if(any(!.years %in% years.present)){
    warning("Requested year(s): "
            , paste(.years[!.years %in% years.present],collapse = ", ")
            , " for ", .condition, " are not present in .df")
  }
  rm(years.present)

  # pl is the incidence object derived dataframe that is used to create the plot
  pl <- as.data.frame(with(df.proc, incidence2::incidence(dates = Dates, na_as_group = T,
                                                     interval = "year",
                                                     groups = Age.Group))) %>%
    mutate(Year = lubridate::year(dates)) %>%
    filter(Year %in% .years) %>%
    tidyr::gather(key = Age.Group, value = Cases, -dates, - Year) %>%
    mutate(Age.Group = factor(Age.Group,
                              levels = levels(df.proc$Age.Group),
                              labels = levels(df.proc$Age.Group),
                              ordered = T))

  # add in missing Years and Age.Groups so that ggplot will show them all
  # msg.years are those years present in the dataframe
  # where a data entry will need to be added for the missing factors
  msg.years  <- unique(lubridate::floor_date(pl$dates,"year"))

  # get parts of Age.Groups factor not represented in the data
  msg.factor <- levels(pl$Age.Group)[!levels(pl$Age.Group) %in% droplevels(pl$Age.Group)] %>%
    factor(.,levels = levels(pl$Age.Group), labels = levels(pl$Age.Group), ordered = T)

  # create record(s) with 0 cases for each year and missing age group
  msg.pl <- purrr::map(.x = msg.years,.f =  function(x)
    data.frame(dates = rep(x,length(msg.factor)),
               Age.Group = msg.factor,
               Cases = rep(0,length(msg.factor))) %>%
      mutate(Year = lubridate::year(dates))) %>%
    bind_rows() %>%
    select(names(pl))

  # add missing age groups with 0 cases to the dataframe
  pl <- bind_rows(pl,msg.pl)
  rm(msg.factor,msg.pl,msg.years)

  # if rates are requested, then calculate and set the column to use in the plot
  # suppress rates when making an external report for those <= .suppress.level
  # for internal reports, calculate the rate but show them as unstable for <= .suppress.level
  if(.rates){
    # retrieve population projections by .age.groups
    age.grp.pop <- retrieve_county_population(.pop_df = as.data.frame(population$texas.projections),
                                                    .year   = .years,
                                                    .ageGrp = .age.groups,
                                                    .county = .county) %>%
      ungroup() %>%
      mutate(Year = as.integer(year)) %>%
      select(Year, age.group, population)

    pl <- left_join(pl,age.grp.pop, by = c("Year" = "Year", "Age.Group" = "age.group")) %>%
      mutate(Rates = case_when(
                  !.external                     ~ Cases / population * 10^6,
                  Cases > .suppress.level        ~ Cases / population * 10^6,
                  Cases <= .suppress.level       ~ .suppress.level / population * 10^6),
                  #between(Cases,0,.suppress.level) & .external ~ .suppress.level / population * 10^6),
             Fill = case_when(
                  Cases > .suppress.level  ~ "Stable",
                  between(Cases,0,.suppress.level) & !.external ~ "Unstable",
                  between(Cases,0,.suppress.level) &  .external ~ "Suppressed"),
             Fill = factor(Fill))

    # yr.label is to label the individual years with the total number of cases in paranthesis
    yr.label <- pl %>%
      group_by(Year) %>%
      summarise(Cases = sum(Cases),
                PopTot = sum(population)) %>%
      ungroup() %>%
      mutate(Rate = Cases/PopTot * 10^6,
             Label = paste0(Year," (",format(Rate,nsmall=1, digits = 1),")")) %>%
      select(Year, Label)

    pl <- left_join(pl, yr.label, by = "Year")  # add Label to pl
    rm(yr.label)

    pal <- setNames(object = c(fill.bar,fill.alpha,NA),nm = c("Stable", "Unstable","Suppressed"))
    caption <-  c("",paste0("Suppressed rates are calculated with a minimum of ", .suppress.level," cases for display purposes"))[.external + 1]
    sc <- scale_fill_manual(values = pal, name = "Rates")
    yVar <- "Rates"
    yAxis <- "Cases/100,000"

  } else {  # format for incidence  counts
    # suppress when cases are below .suppress.level and the report is for external use
    # Note: unsuppressed field added so the total number of cases nia year would still be calculated when sypression in a age group occurs
    pl <- pl %>%
      mutate(unsuppressed = Cases,
             Fill = case_when(
                  Cases > .suppress.level ~ "Cases",
                  between(Cases,0,.suppress.level) & .external ~ "Suppressed",
                  TRUE ~ "Cases"),
             Cases = case_when(
                  between(Cases,0,.suppress.level) & .external ~ 0,
                  TRUE ~ Cases))

    yr.label <- pl %>%
      group_by(Year) %>%
      summarise(Cases = sum(unsuppressed)) %>%  # use unsuprressed to get total for the year
      ungroup() %>%
      mutate(Label = case_when(
        (Cases > .suppress.level | !.external) | Cases == 0 ~ paste0(Year," (",Cases,")"),
        TRUE ~ paste0(Year," (","suppressed",")"))) %>%
      select(Year, Label)

    # remove unsuprressed from dataframe so nothing shows up in the returned plot data
    pl <- dplyr::select(pl, -unsuppressed)

    pl <- left_join(pl, yr.label, by = "Year")  # add Label tp pl
    rm(yr.label)

    pal <- setNames(object = c(fill.bar,NA),nm = c("Cases","Suppressed"))
    caption <-  ""   # caption showing suppressed years
    sc <- scale_fill_manual(values = pal, name = "Cases", guide = FALSE)

    if(.external){
      caption.ages <- pl %>%
        filter(Fill == "Suppressed") %>%
        distinct(Age.Group) %>%
        pull(Age.Group)

      if(length(caption.ages) > 0){
        caption <- paste("Case counts between 1 and",.suppress.level, " are suppressed in some years for age groups\n",
                         paste(caption.ages, collapse = ", "))
      }
      rm(caption.ages)
    }

    yVar <- "Cases"
    yAxis <- yVar
  }

  # title and subtitles of the plot
  tle    <- paste0(.condition," Age-Group Specific ",yVar)
  if(length(unique(.years)) > 1){
    subtle <- "by Year (overall)"
  } else {
    subtle <- c("")
  }

  subtle <- paste(subtle,c(" - For Internal Use","")[.external + 1])


  color_pal <- setNames(object = c("white", .fill, "White",fill.alpha),
                        nm = c("Stable","Unstable", "Cases", "Suppressed"))

  ret <- pl %>%
    ggplot(aes(x = Age.Group, fill = Fill, color = Fill))+
    geom_bar(stat="identity", aes_string(y = yVar))+ #,color = "White")+
    scale_y_continuous(expand = expand_scale(mult = c(0,0.05)), breaks = scales::pretty_breaks())+
    scale_color_manual(values = color_pal, name = sc$name, guide = sc$guide)+
    sc  +
    .th +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "Grey40",linetype = "dotted"),
          panel.grid.minor = element_blank()#,
          #axis.ticks.y = element_line(color = "Grey60", linetype = "dotted")
    )+
    labs(title = tle, subtitle = subtle, x = "Age Groups", y = yAxis, caption = caption)+
    facet_wrap(facets = ~Label)

  return(ret)
}


# ====== Function retrieve_zip_code_population ========
#' Zip Code population estimates.
#'
#' Retrieves the zip code tabulation area population period estimates and margins of error for a given ending year. The
#' data is based on the American Community Survey estimates (table B01003) over a \strong{five year period}. For
#' example, when using "2017" as the end date, the actual years used are 2013 - 2017.
#'
#' @source \url{https://www.census.gov/programs-surveys/acs}
#'
#' @param .endYear is the ending year of the 5-year estimate.
#' @param .area is the geographic area like "Tarrant" (default), "Texas" or "US". Note that Tarrant is the only option
#'   currently implemented
#' @return A tibble with the following fields: \itemize{
#' \item end.year: the ending year of the five year estimate period as an integer
#' \item zip.code: the zip code as a character string
#' \item estimate:  the population estimate as numeric
#' \item moe: margin of error (i.e. standard error) and may be used to calculate confidence intervals.
#' }
#' @export
retrieve_zip_code_population <- function(.endYear, .area = "Tarrant"){

  # if(any(is.character(.endYear))){
  #   .endYear <-  as.numeric(.endYear)
  # }

  if(! all("2012" %in% dimnames(population$tarrant.zcta)[["end.year"]])) {
      rng <- range(population$tarrant.zcta[["end.year"]])
      stop("The .endYear argument was not in the range of ", rng[1]," - ",rng[2])
  }

  stopifnot(.area %in% c("Tarrant")) #, "Texas", "US"))

  ret <- population$tarrant.zcta[.endYear, , , drop = FALSE]

  # ret <- population$tarrant.zcta %>%
  #   dplyr::filter(end.year == .endYear)
  return(ret)
}

