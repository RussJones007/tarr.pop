# ------------------------------------------------------------------------------------------------------------------->
# Script: tarr_pop_class.r
# Description:
#   Population class "tarr_pop" now wraps a DelayedArray / HDF5Array backend. The logical dimensions remain: year,
#   area.name, sex, age.char, race, ethnicity. Data is stored in a "handle" (DelayedArray / HDF5Array) plus explicit
#   dimnames metadata.
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones (original), re-factored for HDF5Array back-end
# Revisions:
#  January 16, 2026 - updates to new_tarr_pop() constructor to handle census and TDC dimension differences
# ------------------------------------------------------------------------------------------------------------------->

#' Create tarr_pop objects
#'
#' Create or test for objects of type "tarr_pop". DEPRECATED in favor of the poparray class.
#' This construcotr wraps construction of poparray object and will be removed once conversion to poarray is complete.
#'
#' Constructor for tarr_pop objects. The S3 class 'tarr_pop' wraps a DelayedArray / HDF5Array The underlying data is
#' held in `handle`, and standardized dimnames are kept in  `dimn`. Attributes are used for 'age_iv', 'data_col', and
#' 'source'.
#'
#' @param x A DelayedArray-compatible object, typically an HDF5Array or DelayedArray around an in-memory array.
#' @param dimnames_list Named list of dimnames, with names: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`.
#' @param data_col Name of the data column when converted to a data.frame/tibble.
#' @param source Optional metadata describing data source (list or character).
#' @param ... Reserved for future use.
#'
#' @return An object of class 'tarr_pop'.
#' @export
#' 
new_tarr_pop <- function(...) {
  .Deprecated("new_poparray")
  new_poparray(...)
}
# new_tarr_pop <- function(x,
#                          dimnames_list = dimnames(x),
#                          data_col = "population",
#                          source = NULL,
#                          ...) {
#   # Ensure we have a DelayedArray-compatible backend
#   if (!inherits(x, "DelayedArray")) {
#     x <- DelayedArray::DelayedArray(x)
#   }
#   
#   nms <- names(dimnames_list)
#   required_core <- c("year", "area.name", "sex", "age.char")
#   has_6d <- all(c("race", "ethnicity") %in% nms)
#   has_5d <- "race_eth" %in% nms
#   
#   if (!all(required_core %in% nms) || !(has_6d || has_5d)) {
#     stop("dimnames_list must have core dims (year, area.name, sex, age.char) and ",
#          "either 6D (race, ethnicity) OR 5D (race_eth). Found: ",
#          paste(nms, collapse = ", "))
#   }
#   
#   age_iv <- if ("age.char" %in% nms) age_to_iv(dimnames_list[["age.char"]]) else NA
#   
#   obj <- list(
#     handle = x,
#     dimn   = dimnames_list
#   )
#   attr(obj, "age_iv")   <- age_iv
#   attr(obj, "data_col") <- data_col
#   if (!is.null(source)) attr(obj, "source") <- source
#   class(obj) <- "tarr_pop"
#   obj
# }

# class inquiry
#' Is object a tarr_pop
#'
#' Checks that the object is of class tarr_pop
#' @param x object to check type or convert
#' @rdname new_tarr_pop
#' @export
is.tarr_pop <- function(x){
  rlang::inherits_any(x, "tarr_pop")
}

# Basic array-like methods -----------------------------------------------------

#' @export
dim.tarr_pop <- function(x) {
  base::dim(x$handle)
}

#' @export
dimnames.tarr_pop <- function(x) {
  x$dimn
}

#' @export
length.tarr_pop <- function(x) {
  prod(dim(x))
}

#' @export
names.tarr_pop <- function(x){
  dimnames(x) |> 
    names()
}

# Printing ---------------------------------------------------------------------

#' @export
print.tarr_pop <- function(x, ...) {

  src <- get_source(x)
  dms <- dimnames(x)              # named list for each dimension
  dms_names <- names(dms)

  dms_sizes <- dms |>
    lengths() |>
    stats::setNames(dms_names)

  dimensions <- paste(
    paste0(names(dms_sizes), " (", dms_sizes, ")"),
    collapse = ", "
  )

  recs <- length(x)

  cat("Class <tarr_pop>\n")
  if (!is.null(src) && !is.na(src[["note"]])) {
    cat("Series:", src[["note"]], "\n")
  }
  if (!is.null(src) && !is.na(src[["source"]])) {
    cat("Sourced: ", src[["source"]], "\n", sep = "")
  }
  cat("Length: ", format(recs, big.mark = ","), "\n", sep = "")
  cat("Dimensions: ", dimensions, "\n", sep = "")
  cat("Data column as data frame: '", data_col(x), "'\n", sep = "")

  invisible(x)
}

# Coercion to data.frame / tibble ---------------------------------------------

#' Coerce to Data Frame
#'
#' S3 method to coerce a tarr_pop object to a data frame. Internally realizes the DelayedArray/HDF5Array backend to a
#' base array (for the requested slice), then converts to a data.frame via as.table().
#'
#' @param x tarr_pop object
#' @param stringsAsFactors Passed to as.data.frame.table
#' @param responseName Name of the value column (defaults to 'data_col' attribute if present, otherwise 'population')
#' @param ... Not used
#'
#' @return a data.frame
#' @exportS3Method base::as.data.frame
as.data.frame.tarr_pop <- function(x,
                                   stringsAsFactors = TRUE,
                                   responseName = data_col(x) %||% "population",
                                   ...) {
  
  # materialize only the requested data
  arr <- as.array(x$handle)
  #Ensure the DelayArray or HDF5rray have the dimenames
  dimnames(arr) <- dimnames(x)
  
  df <- as.data.frame(
    as.table(arr),
    stringsAsFactors = stringsAsFactors,
    responseName = responseName
  )

  polish_df(df, stringsAsFactors)
}

#' Coerce to a Tibble
#'
#' S3 method for class 'tarr_pop'.
#' Coercion is done via as.data.frame.tarr_pop for consistency.
#'
#' @param x tarr_pop object
#' @param stringsAsFactors Logical, passed to as.data.frame.tarr_pop()
#' @param ... Not used
#' @param .name_repair Name repair strategy for tibble
#'
#' @return a tibble
#' @exportS3Method tibble::as_tibble
as_tibble.tarr_pop <- function(x,
                               stringsAsFactors = TRUE,
                               ...,
                               .name_repair = c("check_unique", "unique",
                                                "universal", "minimal")) {

  as.data.frame(x, stringsAsFactors = stringsAsFactors) |>
    tibble::as_tibble(.name_repair = .name_repair)
}

# Polish df --------------------------------------------------------------------

#' Finish polishing df
#'
#' "Polishes" the df argument from as.data.frame.tarr_pop or as_tibble.tarr_pop.
#' In future, may also be called by as.data.table.tarr_pop.
#'
#' @param df a data frame object, or tibble or data.table.
#' @param stringsAsFactors Logical, currently not used for factor conversion, but
#'   kept for compatibility.
#'
#' @returns the same object as df but with appropriate types / ordered factors.
#' @keywords internal
polish_df <- function(df, stringsAsFactors = TRUE) {
  
  # ensure that year and population or value columns are ineger or numeric
  df <- df[stats::complete.cases(df), ] |>
    dplyr::mutate(
      dplyr::across(where(is_char_int),    char_to_int),
      dplyr::across(where(is_char_double), char_to_double)
    )

  
  # Orders and sets levels of ages and age groups
  if(stringsAsFactors){
    # order ages if they are in the tarr_pop object and factors are desired
    ages <- df[["age.char"]] |>
      (\(x) if (is.factor(x)) levels(x) else unique(x))()
    # is "All" used in the ages vector
    all_term <- ages[stringr::str_detect(
      ages, stringr::regex("all", ignore_case = TRUE)
    )]
    
    if (!is.null(ages)) {
      ages_no_all <- if (rlang::is_empty(all_term)) ages else ages[ages != all_term]
      age_ivs <- rage::as.age_group(ages_no_all) |>
        sort()
      
      age_levels <- c(as.character(age_ivs), all_term)
      df[["age.char"]] <- ordered(df[["age.char"]], levels = age_levels)
    }
  }

  # Makes years an ordered factor if it is a factor. 
  if(is.factor(df$year)) {
  df$year <- factor(
    df$year,
    levels  = levels(df$year) |> as.integer() |> sort() |> as.character(),
    ordered = TRUE
  )}

  df
}

# Coerce to tarr_pop -----------------------------------------------------------

#' Coerce to a tarr_pop Object
#'
#' Used to coerce a data frame or an array/table to a tarr_pop object.
#'
#' @param x object to coerce like a data frame or a table/array.
#' @param data_col single character string with the population column name.
#' @param ... additional arguments to pass (see methods).
#'
#' @return a tarr_pop classed object
#' @export
as.tarr_pop <- function(x, data_col = "population", ...) {
  UseMethod("as.tarr_pop")
}

#' @export
as.tarr_pop.default <- function(x, data_col = "population", ...) {
  stop("No method exists for class ", paste(class(x), collapse = ", "))
}

#' @export
as.tarr_pop.data.frame <- function(x,
                                   data_col = "population",
                                   backend = c("hdf5", "delayed"),
                                   filepath = NULL,
                                   dataset = "/pop",
                                   chunkdim = NULL,
                                   level = 6L,
                                   ...) {
  assert_that(is.scalar(data_col),
              is.numeric(x[[data_col]]),
              x %has_name% c("year", "area.name", "sex", "age.char",
                             "race", "ethnicity"))

  backend <- match.arg(backend)

  # Strip unneeded columns, convert to array
  arr <- x |>
    dplyr::select(-dplyr::any_of(c("fips", "age.iv"))) |>
    droplevels() |>
    df_2_array(data_col = data_col)

  dn <- dimnames(arr)

  if (backend == "hdf5") {
    if (is.null(filepath)) {
      stop("For backend = 'hdf5', 'filepath' must be provided.")
    }
    handle <- HDF5Array::writeHDF5Array(
      arr,
      filepath = filepath,
      name     = dataset,
      chunkdim = chunkdim,
      level    = level
    )
  } else {
    handle <- DelayedArray::DelayedArray(arr)
  }

  new_tarr_pop(x = handle,
               dimnames_list = dn,
               data_col = data_col,
               ...)
}

#' @export
as.tarr_pop.array <- function(x, data_col = "population", ...) {
  assert_that(is_scalar_character(data_col))
  assert_that(is.numeric(x))
  assert_that(
    dimnames(x) %has_names%
      c("year", "area.name", "sex", "age.char", "race", "ethnicity")
  )

  handle <- DelayedArray::DelayedArray(x)
  new_tarr_pop(x = handle,
               dimnames_list = dimnames(x),
               data_col = data_col,
               ...)
}

# Subsetting -------------------------------------------------------------------

# tarr_pop method for the index `[` operator, DEPRACATED, using poparray
# If drop = TRUE and resulting object no longer has all six dims, the 
# return is the underlying subsetted HDF5Array rather than a tarr_pop.
#' @export
`[.tarr_pop` <- `[.poparray`
  
#' @export
head.tarr_pop <- function(x, n = 6L, ...) {
  # For clarity and simplicity: head() returns a tibble of first n rows
  tibble::as_tibble(x) |>
    utils::head(n)
}

# Accessors / helpers ----------------------------------------------------------

#' Get or set the name of the tarr_pop data column
#'
#' tarr_pop objects have a "data_col" attribute that is used to name the column that holds the numeric data when
#' coercing to a data frame [as.data.frame()] or tibble [tibble::as_tibble()]. This convenience function can retrieve or
#' set that attribute.
#'
#' @param x a tarr_pop object
#' @return character string
#' @export
data_col <- purrr::attr_getter("data_col")

#' @rdname data_col
#' @export

`data_col<-` <- function(x, values){
  assertthat::assert_that(is.string(values),
                          msg = "data_col must be a character string of length one")
  
  attr(x, "data_col") <- values
  x
}

#' Change population data frame to a tarr_pop object
#'
#' Internal helper, usually used only inside the package when constructing
#' tarr_pop cubes from data frames.
#'
#' @param x the population data frame
#' @param data_col single character string for the data column name
#' @param backend either "delayed" (in-memory DelayedArray) or "hdf5"
#' @param filepath HDF5 filepath when backend = "hdf5"
#' @param dataset HDF5 dataset name, defaults to "/pop"
#'
#' @keywords internal
#' @export
#' @returns a tarr_pop object
dataframe_to_pop <- function(x,
                             data_col = "population",
                             backend  = c("delayed", "hdf5"),
                             filepath = NULL,
                             dataset  = "/pop",
                             chunkdim = NULL,
                             level    = 6L) {
  backend <- match.arg(backend)
  as.tarr_pop(
    x,
    data_col = data_col,
    backend  = backend,
    filepath = filepath,
    dataset  = dataset,
    chunkdim = chunkdim,
    level    = level
  )
}

# autoplot / plot --------------------------------------------------------------

# autoplot.tarr_pop and plot.tarr_pop mostly operate on as.data.frame(x),
# which is already wired to pull from the HDF5-backed DelayedArray, so
# only minimal changes are needed.

#' Autoplot a tarr_pop
#'
#' Method for autoplot() to chart a tarr_pop.  If gender is available, will
#' create a population pyramid, otherwise a line chart by age.
#'
#' @param object a tarr_pop object
#' @param ...  additional arguments
#' @param proportional logical; if TRUE plot proportions instead of counts
#' @param fill named vector of colors for male/female
#' @param source caption for plot, defaults to tarr_pop source attribute
#'
#' @return a ggplot
#' @exportS3Method ggplot2::autoplot
autoplot.tarr_pop <- function(object, ...,
                              proportional = FALSE,
                              fill = c("royalblue2", "palevioletred3"),
                              source = attr(object, "source")[["note"]]) {

  p <- as.data.frame(object)
  avail <- get_available_values(p)

  # pyramid if both sexes present and age is not "All"
  if (all(c("Female", "Male") %in% avail[["sex"]]) &&
      all(!"All" %in% avail[["age.char"]])) {

    area_pop <- p |>
      dplyr::group_by(year, area.name) |>
      dplyr::summarise(total_pop = sum(population), .groups = "drop")

    p <- p |>
      dplyr::select(year, sex, age.char, area.name, population) |>
      dplyr::mutate(population = ifelse(sex == "Male", -population, population)) |>
      dplyr::filter(!is.na(age.char))

    p <- dplyr::left_join(p, area_pop, by = c("year", "area.name")) |>
      dplyr::mutate(Proportion = (population / total_pop))

    ag <- p |>
      dplyr::pull(age.char) |>
      as.character() |>
      rage::as.age_group() |>
      ivs::iv_start()

    metrics <- p |>
      dplyr::group_by(year, area.name) |>
      dplyr::summarise(
        mean_age = stats::weighted.mean(x = ag, w = population),
        total_pop = unique(total_pop),
        labels = paste0(
          "Population: ",
          format(total_pop, big.mark = ",")
        ),
        .groups = "drop"
      )

    if (length(unique(p$area.name)) > 7) {
      p <- dplyr::filter(p, area.name %in% unique(area.name)[1:7])
    }

    if (length(unique(p$year)) > 5) {
      p <- dplyr::filter(p, year %in% unique(year)[1:5])
    }

    pl <- ggplot2::ggplot(
      p,
      ggplot2::aes(
        x = age.char,
        y = if (proportional) Proportion else population,
        fill = sex
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("Male" = fill[1], "Female" = fill[2])
      ) +
      ggplot2::labs(
        title = paste0(
          sort(unique(p[["area.name"]])),
          collapse = ", "
        ) |>
          paste("Population Pyramid"),
        x = "Age",
        y = "Population"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank()
      ) +
      ggplot2::facet_grid(area.name ~ year) +
      ggplot2::geom_text(
        data        = metrics,
        ggplot2::aes(x = Inf, y = Inf, label = labels),
        hjust       = 1.5,
        vjust       = 1.5,
        size        = 3,
        inherit.aes = FALSE
      )

    pl <- if (proportional) {
      pl + ggplot2::scale_y_continuous(
        breaks = seq(-0.08, 0.08, by = 0.01),
        labels = \(x) scales::percent(abs(x), accuracy = 0.1)
      )
    } else {
      pl + ggplot2::scale_y_continuous(
        breaks = seq(-1e5, 1e5, by = 20000),
        labels = abs
      )
    }

    if (!is.null(source)) {
      pl <- pl + ggplot2::labs(caption = paste("Source:", source))
    }

    return(pl)
  }

  # Fallback could be added here for non-pyramid plots
  stop("autoplot.tarr_pop currently supports pyramid plots with Male/Female and non-'All' ages.")
}

#' Plot Method for tarr_pop Object
#'
#' Plots a population pyramid based on a tarr_pop
#'
#' @param x a tarr_pop object
#' @param ...  not used
#'
#' @returns plots the data without returning anything
#' @export
plot.tarr_pop <- function(x, ...) {

  years <- dimnames(x)[["year"]] |> as.integer() |> sort()
  areas <- dimnames(x)[["area.name"]]

  n_row <- length(years)
  n_col <- length(areas)

  pop <- as.data.frame(x)
  pop <- pop[!is.na(pop$age.char), ]
  pop[is.na(pop$population)] <- 0

  limit <- purrr::map_int(
    areas,
    \(a) pop[pop$area.name == a, "population"] |> max(na.rm = TRUE)
  ) |> stats::setNames(areas)

  op <- par(
    mfrow = c(n_row, n_col),
    mar   = c(.5, 2, .5, 1),
    oma   = c(4, 4, 4, 4)
  )
  on.exit(par(op), add = TRUE)

  op2 <- options(scipen = 9999)
  on.exit(options(op2), add = TRUE)

  subset_pop <- purrr::compose(
    \(z) pop[pop$year == z$year & pop$area.name == z$area, ]
  )

  grid <- expand.grid(seq_len(n_col), seq_len(n_row)) |>
    stats::setNames(c("col", "row"))

  yr_area <- expand.grid(areas, years) |>
    stats::setNames(c("area", "year"))

  purrr::walk(seq_len(nrow(yr_area)), \(ndx) {
    row_col <- grid[ndx, ]

    yr_area[ndx, ] |>
      subset_pop() |>
      make_pyramid(
        xlim       = limit[yr_area$area[ndx]],
        xaxis      = row_col$row == n_row,
        yaxis      = row_col$col == 1,
        year_label = row_col$col == n_col,
        area_label = row_col$row == 1
      )
  })

  mtext(text = "Age", side = 2, outer = TRUE, line = 2)
  mtext(text = "Population", side = 1, outer = TRUE, line = 2)
}

# internal function for plot method
#' Make a population pyramid
#'
#' Takes a data frame with population figures with area, year, sex and ages
#' then creates a population pyramid. Internally used, not exported.
#'
#' @param df a data frame converted from a tarr_pop object. Expects one year and
#'   one area.
#' @param xlim numeric scalar with the max abs limit for x-axis.
#' @param xaxis logical; draw x-axis if TRUE
#' @param yaxis logical; draw y-axis labels if TRUE
#' @param area_label logical; show area label
#' @param year_label logical; show year label
#' @param col named vector of colors for male and female bars.
#'
#' @returns plots the pyramid
make_pyramid <- function(df,
                         xlim = NULL,
                         xaxis = FALSE,
                         yaxis = FALSE,
                         area_label = FALSE,
                         year_label = FALSE,
                         col = c("Male" = "royalblue3",
                                 "Female" = "palevioletred3")) {

  assertthat::is.scalar(xlim)

  df <- dplyr::arrange(df, age.char)
  male   <- df[df$sex == "Male", ]   |> dplyr::mutate(population = population * -1)
  female <- df[df$sex == "Female", ]

  area <- unique(df$area.name)
  yr   <- unique(df$year)

  if (is.null(xlim)) {
    xlim <- max(df$population, na.rm = TRUE)
  }
  x_lim <- c(-xlim, xlim)

  graphics::barplot(
    male$population,
    horiz = TRUE,
    names.arg = if (yaxis) male$age.char else NA,
    xlim  = x_lim,
    col   = col["Male"],
    border = NA,
    xaxt  = "n",
    las   = 1
  )

  graphics::barplot(
    female$population,
    horiz  = TRUE,
    add    = TRUE,
    col    = col["Female"],
    border = NA,
    xaxt   = "n"
  )

  if (xaxis) {
    at <- pretty(x_lim, n = 7)
    graphics::axis(
      1,
      at = at,
      labels = formatC(
        abs(at),
        format   = "d",
        big.mark = ",",
        width    = 6
      )
    )
  }

  if (year_label) graphics::mtext(unique(df$year), side = 4, outer = FALSE)
  if (area_label) graphics::mtext(unique(df$area.name), side = 3, outer = FALSE)
}

# Split ------------------------------------------------------------------------

#' Split Population
#'
#' Splits a tarr_pop object by dimension values.
#'
#' @param x a tarr_pop object
#' @param f the dimension name to use as the splitting variable. Must be a
#'   single character string.
#' @param drop if the tarr_pop array should drop the dimension being split.
#'   The default is FALSE.
#' @param ... arguments to pass to other functions, not used.
#'
#' @returns a list of tarr_pop
#' @export
split.tarr_pop <- function(x, f, drop = FALSE, ...) {

  dim_names <- names(dimnames(x))

  assert_that(
    f %in% dim_names,
    msg = paste(
      "Dimension name in 'f' not found in array. Valid dimensions:",
      paste(dim_names, collapse = ", ")
    )
  )
  assert_that(
    is.string(f) & is.scalar(f),
    msg = "The 'f' argument must be a character string with length one.\n"
  )

  dim_values <- dimnames(x)[[f]]

  # template index for all dimensions
  index_template <- rep(list(TRUE), length(dim_names)) |>
    stats::setNames(nm = dim_names)

  sub_arr <- function(value) {
    index_template[[f]] <- value
    do.call(
      `[`,
      c(list(x), index_template, list(drop = drop))
    )
  }

  purrr::map(dim_values, sub_arr) |> 
    set_names(dim_values)
}


# Numeric operations -------------------------------------------------------------------------------------------------------------

#' @export
as.double.tarr_pop <- function(x, ...){
  as.double(x$handle)
}

#' @export
Summary.tarr_pop <- function(..., na.rm = FALSE) {
  args <- list(...)
  #vals <- lapply(args, \(x) if( is.tarr_pop(x)) as.numeric(x$handle) else as.numeric(x)) |> 
  #  unlist(use.names = FALSE)
  
  vals <- lapply(args, \(x) as.numeric(x)) |> 
    unlist(use.names = FALSE)
  
  # vals <- unlist(lapply(args, function(x) {
  #   if (inherits(x, "tarr_pop")) as.numeric(x) else as.numeric(x)
  # }), use.names = FALSE)
  
  switch(.Generic,
         "sum"  = base::sum(vals,  na.rm = na.rm),
         "prod" = base::prod(vals, na.rm = na.rm),
         "min"  = base::min(vals,  na.rm = na.rm),
         "max"  = base::max(vals,  na.rm = na.rm),
         "range"= base::range(vals, na.rm = na.rm),
         "any"  = base::any(vals),
         "all"  = base::all(vals),
         stop("Unsupported Summary op for tarr_pop: ", .Generic)
  )
}
