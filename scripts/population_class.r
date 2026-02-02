# ------------------------------------------------------------------------------------------------------------------->
# Script: population_class.r
# Description:
#   Population class "tarr_pop" represents the data as a multidimensional array
#   The dimensions include year, area.name, sex, age.char, race, and ethnicity
#
#
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  Feb 7, 2025
# ------------------------------------------------------------------------------------------------------------------->

#' Create a tarr_pop Object
#'
#' A constructor for tarr_pop objects. The S3 class 'tarr_pop' is a sub-class of class 'table' and 'array'. In this
#' package, population data is held in specially formatted multidimensional arrays and classed as tarr_pop objects. This
#' reduces the size of the data as compared to data frames. Each of the population files downloaded from the Census
#' bureau, the Texas Demographic Center and National Cancer Institute (SEER files) are read as data frames, then
#' converted to arrays using the  [as.tarr_pop()] function which calls this constructor. The constructor is a simple
#' function to take an array and return a tarr_pop object. Minimal checking of the array is done as it assumes it is
#' already in the proper format. A convenient way to construct a tarr_pop is using the [as.tarr_pop()] function which
#' handles converting a data frame first, then calls  this constructor.
#'
#' tarr_pop objects may be used directly to select values in each dimension as well as grouping ages.  Manipulating a
#' tarr_pop object is approximately two orders of magnitude faster than similar operations using a data frame and uses
#' less memory. Since a tarr_pop object is also a table, it can be used with tables of like dimensions, for example to
#' calculate rates. However, data frames are often easier for a user to conceptualize and some manipulations are easier
#' to do in that format. To convert a tarr_pop object to a data frame use [as.data.frame()] and
#' [tibble::as_tibble()].
#'
#' @param x an array constructed from a population data frame. The data frame should have the following fields:
#' year, area.name, sex, age.char, race, ethnicity.  Note that data from the Texas Demographic Center may
#' not have ethnicity separate from the race categories.
#' @returns  a tarr_pop object with age_iv as an attribute if age,char is present
#' @export
#' @examples
#'
#' p <- population$census |>
#'   select(df, -c(fips, age.iv) |>
#'   as.tarr_pop(data_col = "pop")
#'
new_tarr_pop <- function(x, ...) {
  nms <- names(dimnames(x))
  if ("age.char" %in% nms) {
    ats <- age_to_iv(dimnames(x)[["age.char"]])
  } else {
    ats <- NA
  }
  cls <- c("tarr_pop", "table")
  d_col <- get_data_col(x)

  structure(x,
    age_iv = ats,
    data_col = d_col,
    ...,
    class = cls
  )
}


#' @export
print.tarr_pop <- function(x, ...){

  src   <- get_source(x)
  dms   <- dimnames(x) # the names and values for each dimension
  dms_names <- names(dms)   # nams for each dimension

  dms_names <- dms |>
    lengths() |>
    set_names(dms_names)

  dimensions <- paste(paste0(names(dms_names), " (", dms_names,")"), collapse = ", ")

  #recs <- reduce(.x = lengths(dms), .f = `*`) |> format(big.mark = ",")
  recs <- length(x)

  cat("Class <", paste(class(x), collapse = ", "), ">\n")
  cat(src["note"], "\n")
  cat("Sourced from: ", src["source"], "\n")
  cat("Dimensions: ", dimensions, "\n")
  cat("Data column when coverted to data frame: ", get_data_col(x), "\n")
  cat("Records as a data frame: ", recs, "\n")
  #x <- head(x)

  #print(attr(x, "dimnames"))
  #cat("\n")
  #str(x)

  #NextMethod()
  invisible(x)
}


#' Coerce to Data Frame
#'
#'  S3 method coerce a tarr_pop object to a data frame.
#'
#' @param x tarr_pop object
#' @param ... not used in this function
#'
#' @return a data.frame
#' @exportS3Method base::as.data.frame
#'
#' @examples
#' df <- as.data.frame(population$census)
#' glimpse(df)
#'
as.data.frame.tarr_pop <- function(x, stringsAsFactors = TRUE, responseName = get_data_col(x) %||% "value", ...) {
  df = NextMethod(stringsAsFactors = stringsAsFactors, responseName = responseName)
  polish_df(df, stringsAsFactors)
}

#' Coerce to a Tibble
#'
#'  S3 method for class 'tarr_pop'
#'
#' @param x tarr_pop object
#' @param ... not used in this function
#' @param n a character string to name the data column.  Default is the "data_col" attribute, or if it is missing  then
#' "value"
#'
#' @return a tibble
#' @exportS3Method tibble::as_tibble
#' @examples
#' df <- as_tibble(population$census)
#'
as_tibble.tarr_pop <- function(x,
                               stringsAsFactors = TRUE,
                               ...,
                               .name_repair = c("check_unique", "unique", "universal", "minimal")){

  # Creating a tiblle is done by leveraging the as.data.frame() as the NextMethod for as_tibble causes NA in the year
  # column.
  as.data.frame(x) |>
    as_tibble(.name_repair = .name_repair)

}


#' Finish polishing df
#'
#' "polishes" the df argument from as.dataframe.tarr_pop or as_tibble.tarr_pop.
#' This function is called from as.dataframe.tarr_pop or as_tibble.tarr_pop.  In the future
#' will probably also be called by as.data.table.tarr_pop as well.
#'
#' @param df a data frame object, or tibble or data.table.
#'
#' @returns the same object as in df however columns are changed to integer, double, character, and
#'   usually factors.
#' @keywords internal
polish_df <- function(df, stringsAsFactors = TRUE){
  df <- df[complete.cases(df), ] |>
    mutate(across(where(is_char_int), char_to_int),
           across(where(is_char_double), char_to_double))

  # order ages if they are in the tarr_pop object
  ages <- df[["age.char"]] |>
    (\(x) if(is.factor(x)) levels(x) else unique(x))()
  # is "All" used in the ages vector
  all_term <- ages[str_detect(ages, regex("all", ignore_case = TRUE))]

  if(! is.null(ages)){
    ages_no_all <- if(rlang::is_empty(all_term)) ages else ages[ages != all_term]
    age_ivs <- rage::as.age_group(ages_no_all) |>
      sort()

    age_levels <- c(as.character(age_ivs), all_term)
    df[["age.char"]] = ordered(df[["age.char"]], levels = age_levels)
  }

  df$year <- factor(df$year,
                    levels = levels(df$year) |> as.integer()|> sort() |> as.character(),
                    ordered = TRUE)

  # if(stringsAsFactors){
  #   df <- mutate(df,  across(is.character, ~ factor(x = .x, levels = unique(.x) |> sort())))
  # }

  df

}

# S3 Generic to Coerce to a tarr_pop
#' Coerce to a tarr_pop Object
#'
#' Used to coerce a data frame or an array/table to a tarr_pop object.
#' The is an S3 generic function, with methods for:
#' * [data.frame]: a data frame..
#' * array or table with the required dimensions.
#'
#' @param x object to coerce like a data frame or a table/array.
#' @param data_col a single character string indentifying the name of the column with population figures. Defaults to
#'   "population"
#' @param ... additional arguments to pass, not used
#'
#' @return a tarr_pop classed object
#' @export
#'
#' @examples
#' df <- county_population(population$census,
#'    year      = c("2010", "2020"),
#'    county    = "Tarrant",
#'    sex       = c("Female", "Male"),
#'    age       =  c(as.character(0:99), "100-104")) |>
#'    as.data.frame()  # convert from a tarr_pop to a data frame
#'
#'  # convert back to a tarr_pop which is a tye of array
#'  tarr_array <- as.tarrpop(df)
#'
as.tarr_pop <- function(x, data_col = "population", ...){
  UseMethod("as.tarr_pop")
}

# default method
#' @export
as.tarr_pop.default <- function(x, data_col = "population", ...){
  stop("No method exists for class ", class(x))
}

# method to coerce from a data frame to a tarr_pop
# checks the appropriate fields are in the data frame, throws an error if they are missing
#' @export
as.tarr_pop.data.frame <- function(x, data_col = "population", ...){
  assert_that(is.scalar(data_col),
              is.numeric(x[[data_col]]),
              x %has_name% c("year", "area.name", "sex", "age.char", "race", "ethnicity"))

  x |>
    select(- any_of(c("fips", "age.iv"))) |>   # population data frames may have these as fields that bloat the array size.
    droplevels() |>
    df_2_array(data_col = data_col) |>
    new_tarr_pop(...)
}

# method to coerce from an array to a tarr_pop
#' @export
as.tarr_pop.array <- function(x, data_col = "population", ...){
  assert_that(is_scalar_character(data_col))
  assert_that(is.numeric(x))
  assert_that( dimnames(x) %has_names% c("year", "area.name", "sex", "age.char", "race", "ethnicity"))
  attr(x, "data_col") <- data_col
  new_tarr_pop(x, ...)
}


# tarr_pop method for the index `[` operator
# the default for drop is not respected.
# if dimensions are dropped, then the retunred argument is no longer a tarr_pop, but just a table/array
#' @export
`[.tarr_pop` <- function(x, i, j, ..., drop = FALSE){
  res <- NextMethod(object = x, drop = drop)
  if( all(c("year", "area.name", "sex", "age.char", "race", "ethnicity")  %in% names(dimnames(res)))) {
    attr(res, "source") <- get_source(x)
    attr(res, "data_col") <- get_data_col(x)
    class(res) <- class(x)
  }
  res
}

# tarr_pop method for the index `[<-` operator
#' @export
`[<-.tarr_pop` <- function(x, i, j, ...){
  cls <- class(x)
  d_col <- get_data_col(x)
  res <- NextMethod()
  attr(res, "data_col") <- d_col
  #res <- new_tarr_pop(res, ...)
  res
}

#' @export
head.tarr_pop <- function(x, n = 6L, ...){
 orig_class <- class(x)
 src        <- get_source(x)
 ret        <- NextMethod()
 class(ret) <- class(x)
 attr(ret, "data_col") <- get_data_col(x)
 attr(ret, "source")   <- src
 ret
}

#' Retrieve the name of the data column
#'
#' tarr_pop objects will usually have a "data_col" attribute that is used to name the column that holds the data when
#' coercing to a data frame [as.data.frame()] or tibble [tibble::as_tibble()].  This convenience function gets that attribute.
#' @param x a is the tarr_pop object
#' @return  character string
#' @keywords internal
#' @export
get_data_col <- purrr::attr_getter("data_col")


#' Change population data frame to a tarr_pop object
#'
#' Only use in the package in the construction of the tarr_pop arrays
#'
#' @param x the population data frame
#' @param data_col is a single character string that contains the name of the data for the array, defaults to
#'   "population".
#' @keywords internal
#' @export
#' @returns a tarr_pop object
#'
dataframe_to_pop <- function(x, data_col = "population"){
  assert_that(is_scalar_character(data_col))
  x |>
    select(- any_of(c("fips", "age.iv"))) |>   # population data frames have these as fields that bloat the array size.
    droplevels() |>
    df_2_array(data_col = data_col) |>
    new_tarr_pop()
}

#' Autoplot a tarr_pop
#'
#'  Method for autoplot() to chart a tarr_pop.  If gender is available, will create a population pyramid, other wise a
#'  line chart by age
#' @param object a tarr_pop object
#' @param ...
#'
#' @return a ggplot
#' @exportS3Method ggplot2::autoplot
#'
#' @examples
#' p <- county_population(population$census, year = 2020, county = "Tarrant", gender = c("Female", "Male"), age = 0:100)
#' autoplot(p)
autoplot.tarr_pop <- function(object, ...,
                              proportional = FALSE,
                              fill = c("royalblue2", "palevioletred3"),
                              source = attr(object, "source")[["note"]]) {

  p <- as.data.frame(object)
  avail <- get_available_values(p)

  # check and do pyramid plots
  if( all(c("Female", "Male") %in% avail[["sex"]]) & all(! "All" %in% avail[["age.char"]]) ){
    area_pop <- p |>
      group_by(year, area.name) |>
      summarise(total_pop = sum(population)) |>
      ungroup()

    p <- p |>
      select(year, sex, age.char, area.name, population) |>
      mutate(population = ifelse(sex == "Male", -population, population)) |>
      filter(! is.na(age.char))

    p <- left_join(p, area_pop, by = c("year", "area.name")) |>
      mutate(Proportion = (population/total_pop) )

    ag <- p |>
      pull(age.char) |>
      as.character() |>
      rage::as.age_group() |>
      ivs::iv_start()


    metrics <- p |>
      group_by(year, area.name) |>
      summarise(mean_age = weighted.mean(x = ag, wt = population),
                total_pop = unique(total_pop),
                labels = paste0("Population: ",
                                format(total_pop, big.mark = ","))#, "\n",
                                #"Mean age: ", round(mean_age, 1))
      )


    if(p$area.name |> unique() |> length() > 7) p <- p |>
        filter(area.name %in% unique(area.name)[1:7])

    if(unique(p$year) |> length()> 5) p <- filter(p, year  %in%  unique(year)[1:5])

  pl <-   ggplot2::ggplot(p, ggplot2::aes(x = age.char,
                                          y = if(proportional) Proportion else population,
                                          fill = sex)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("Male" = fill[1], "Female" = fill[2])) +
    ggplot2::labs(title = paste0(sort(unique(p[["area.name"]])), collapse = ", ") |> paste("Population Pyramid"),
                  x = "Age", y = "Population") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank())+
    ggplot2::facet_grid(area.name ~ year) +
    ggplot2::geom_text(data = metrics, aes(x = Inf, y = Inf, label = labels),
              hjust = 1.5, vjust = 1.5, size = 3,
              inherit.aes = FALSE)

  pl <- if(proportional){
    pl + ggplot2::scale_y_continuous(breaks = seq(-0.08, 0.08, by = 0.01), labels = \(x) scales::percent(abs(x), accuracy = 0.1))
  } else {
    pl + ggplot2::scale_y_continuous(breaks = seq(-10^5, 10^5, by = 20000), labels = abs)
  }

  if(! is.null(source)) pl <- pl + labs(caption = paste("Source:", source))
  return(pl)

  }
}  # End of autoplot



#' Plot Method for tarr_pop Object
#'
#' Plots a population pyramid based on a tarr_pop
#'
#' @param x a tarr_pop object
#' @param ...
#'
#' @returns plots the data without returning anything
#' @export
plot.tarr_pop <- function(x, ...){
  #x = tmp
  years <- dimnames(x)["year"] |> unlist() |> as.integer() |> sort()
  areas <- dimnames(x)["area.name"] |> unlist()
  n_row <- length(years)
  n_col <- length(areas)


  pop    <- as.data.frame(x)
  pop <- pop[! is.na(pop$age.char), ]
  pop[is.na(pop$population)] <- 0
  limit  <- purrr::map_int(areas, \(x) pop[pop$area == x, "population"] |> max(na.rm = TRUE)) |>
    set_names(areas)
  #limit  <- max(x, na.rm = TRUE) # the limits of the x axis, always the max to show the largest population


  op <- par(mfrow = c(n_row, n_col),
            mar = c(.5,2,.5,1),
            oma = c(4,4,4,4)) # small margins
  on.exit(par(op))

  op2 <- options(scipen = 9999)
  on.exit(options(op2), add = TRUE)

  subset_pop <- compose(
    \(x)  pop[pop$year == x$year & pop$area.name == x$area, ]
  )

  grid <- expand.grid(seq(n_col), seq(n_row)) |>
    set_names(c("col", "row" ))

  yr_area <- expand.grid(areas, years) |>
    set_names(c("area", "year"))

  walk(seq.int(nrow(yr_area)), \(ndx) {
    row_col = grid[ndx,]

    yr_area[ndx,] |>
      subset_pop() |>
      make_pyramid(xlim = limit[yr_area$area[ndx]],
                   xaxis = row_col$row == n_row,
                   yaxis = row_col$col == 1,
                   year_label = row_col$col == n_col,
                   area_label = row_col$row == 1)
  })

  mtext(text = "Age", side = 2, outer = TRUE, line = 2)
  mtext(text = "Population", 1, outer = TRUE, line = 2)
}

# internal function for plot method
#' Make a population pyramid
#'
#'  Takes a data frame with population figures with area, year, sex and ages then creates a population
#'  pyramid.  Internally used, not exported.
#'
#' @param df a data frame converted from a tarr_pop object.  Expects one year and one area
#' @param xlim is a two vector of 1 numbers with the limits for the x axis.  The number repsents the maxima
#' of each end of the x axis.
#' @param col is the named vector of colors for male and female bars.
#'
#' @returns  plots the pyramid
make_pyramid <- function(df,
                         xlim = NULL,
                         xaxis = FALSE,
                         yaxis = FALSE,
                         area_label = FALSE,
                         year_label = FALSE,
                         col = c("Male" = "royalblue3", "Female" = "palevioletred3")){

  assertthat::is.scalar(xlim)

  df <- df |> arrange(age.char)
  male   <- df[df$sex == "Male",] |> mutate(population = population * -1)
  female <- df[df$sex == "Female",]

  area <- df$area.name |> unique()
  yr   <- df$year |> unique()
  ages <- df$age.char |> levels()

  if(is.null(xlim)){
    xlim <- max(df$population, na.rm = TRUE)
  }
  x_lim <- c(-xlim, xlim)

  barplot(male$population, horiz = TRUE, names.arg = if(yaxis) male$age.char else NA,
          xlim = x_lim, col = col["Male"], border = NA, xaxt = "n", las = 1)

  barplot(female$population, horiz = TRUE, add = TRUE, col = col["Female"], border = NA, xaxt = "n")
  if(xaxis) axis(1,
                 at = pretty(x_lim, n = 7),
                 labels = formatC(abs(pretty(x_lim, n= 7)), format = "d", big.mark = ",", widt = 6))

  if(year_label) mtext( unique(df$year), side = 4, outer = FALSE)
  if(area_label) mtext( unique(df$area), side = 3, outer = FALSE)
}



#' Split Population
#'
#' Splits a tarr_pop object by a population dimension.
#'
#' @param x a tarr_pop object
#' @param f the dimension name to use as the splitting variable. Must be a single character string
#' @param drop if the tarr_pop array should drop the dimension being split.  The default is FALSE
#' @param ... arguments to pass to other functions, not used.
#'
#' @returns a list of tarr_pop
#' @export
#'
#' @examples
#'  cen <- county_population(pop = census.estimates,  year = 2020:2023,
#'                           county = "Tarrant", sex = "All",
#'                           race = "All",
#'                           ethnicity = "All")
#'
#'  cen_list <-  split(x = cen, f = "year")
#'
split.tarr_pop <- function(x , f, drop = FALSE, ...){

  # Get dimension names of
  dim_names <- names(dimnames(x))
  # Validate dimension name
  assert_that( f %in% dim_names,
               msg = paste("Dimension name in 'f' not found in array. Valid dimensions:",
                           paste(dim_names, collapse = ", ")))
  assert_that( is.string(f) & is.scalar(f), msg = "The 'f' argument must be a character string with a length of one.\n")

  # Get target dimension index and values
  dim_values <- dimnames(x)[[f]]

  # Create a template index for all dimensions
  index_template <- rep(list(TRUE), length(dim_names)) |>
    set_names(nm = dim_names)

  # local function to actually subset tarr_pop. Not a pure function
  sub_arr <- function(value){
    index_template[[f]] <- value
    do.call(`[`, c(list(x), index_template, list(drop = drop)))
  }

  # create list of each subset as return value from the function
  map(dim_values, sub_arr)

}

