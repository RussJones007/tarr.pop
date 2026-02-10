# utils.r
#
# Utility functions for this package
# Functions to make an array from a data frame and vice-versa
# -----------------------------------------------------------------------------------------------------------------


#' Data Frame to Array
#'
#' A data frame can be made into an array with one column serving as the source of data, and the other columns becoming
#' dimensions. This function takes a data frame and returns an array with dimnames and values. To ensure the array
#' is complete, all combinations of the data frame fields must be present.  This function ensures this is the case.
#' Generally, arrays are a more efficient means of storing data compared to data frames. Using the indexing operator to
#' return subsets is very fast. This function is used internally, but is exposed for use outside the package.  The
#' complimentary array_2_df() is the reverse of this function.
#'
#' @param df a data frame with one column that will act as the data in an array.
#' @param data_col a single character string containing the column name for the source of data used to construct the
#'   array.
#'
#' @return an array with dimensions ncol() - 1. The array will have the "data_col" attribute, a character string with the
#' original name of the column that contained the data.
#' @keywords internal
#' @export
#'
#' @examples
#' # convert a large data frame into an array
#' df <- population$census |>
#'    as.data.frame()
#'
#' arr <- df_2_array(df, data_col = "population")
#'
#' dims(arr)
#' dimnames(arr)
#'
df_2_array <- function(df, data_col = "value"){
  assertthat::assert_that(is.scalar(data_col))
  assertthat::assert_that(df %has_name% data_col)

  fields <- names(df)[names(df) != data_col]
  df <- tidyr::complete(df, !!!rlang::syms(fields))  # ensure every level is available
  df <- arrange(df, pick(rev(fields)))               # arrange the data frame starting with the right most non-data column

  dim_lens <- map(fields, \(col) length(unique(df[[col]]))) |> unlist(use.names = FALSE)
  dim_nms <- map(fields, \(col) df[[col]] |>  as.character() |> unique()) |>
    set_names(fields)

  ret <- array(data = df[[data_col]], dim = dim_lens, dimnames = dim_nms)
  attr(ret, "data_col") <- data_col
  return(ret)
}


#' Array to a Data Frame
#'
#'  Constructs a data frame from an array. The complimentary df_2_array() function does the opposite.  Note that
#'  reconstruction of a data frame from an array is not perfect.  All dimension values in an array are stored as
#'  character strings. For those dimension values that consist of numeric character strings (e.g., "123.45"), this
#'  function will convert that dimension to a numeric column in the resulting data frame.  Likewise those that have
#'  integer character values(e.g. 123) will be converted to integer.  All of the other values are changed to
#'  character vectors or factors.
#'  What this means is that a data .frame (df) with an an ordered factor will have an regular factor when arr <-
#'  df_to_array(df) and then df_new <- arrary_to_df(arr). Therefore df may not equal df_new
#'
#'
#' @param arr an array object
#' @param data_col a character string naming the column that will contain the data in the resulting data frame.
#' By default the data column will named from the attribute "data_col" if it is present, or "value" if it is not.
#'
#' @return a data frame with the number of columns equal to the array dimensions + 1
#' @keywords internal
#' @export
#'
#' @examples
#' #Convert a population array to a data frame
#' df <- array_2_df(population$census)
#' head(df)
#'
array_2_df <- function(
    arr,
    ...,
    stringAsFactors = FALSE,
    data_col = c(data_col(arr), "value") ) {

  # Convert array to data frame
  data_col = match.arg(data_col)
  df <- as.data.frame.table(arr, ..., stringAsFactors  = stringAsFactors, responseName = data_col)
  df <- df[complete.cases(df), ]

  #Convert Var columns to appropriate types if possible
  df <- df |>
    mutate(across(where(is_char_int), char_to_int),
           across(where(is_char_double), char_to_double))

  return(df)
}




# internal only, is a vector of character string appears to be integers or double, or character only
is_char_int <- function(vec) {( !is.factor(vec) & all(grepl("^[0-9]+$",x =  vec)) )}
is_char_double <- function(vec) {( !is.factor(vec) & all(grepl("^[0-9]+\\.?[0-9]*$", x = vec)) )}
char_to_int <- purrr::compose(as.integer, as.character)
char_to_double <- purrr::compose(as.double, as.character)


# Intervals as.character method ================
# #' Internal function to convert a double interval to a character string
# #'
# #' @param x a vector of ivs_iv
# #' @param ...
# #'
# #' @return a character vector
# #' @noRd
# #' @export
#as.character.ivs_iv <- function(x, ...){
#   stringr::str_glue('[{iv_start(x)}, {iv_end(x)})')
# }



# strictly used for converting age.char as it makes assumptions about the character vector
age_to_iv <- function(x, ...){
  assert_that(is.character(x))

  NAs <- rep(NA_real_, length(x))
  ret <- ivs::new_iv(NAs, NAs)

  singles <- stringr::str_detect(x, "^\\d+$")
  #ret ivs::iv(start = as.integer(x[singles]), as.integer(x[singles])) + 1)
  ret[singles] <- ivs::iv(start = as.double(x[singles]),end = as.double(x[singles]) + 1, ptype = integer())

  inter_ranges <- stringr::str_detect(x, "-")
  ret[inter_ranges] <- ivs::iv(as.double(stringr::str_extract(x[inter_ranges], "^\\d+")),
                               as.double(stringr::str_extract(x[inter_ranges], "\\d+$"))
  )

  aboves <- stringr::str_detect(x, "\\+")
  ret[aboves] <- ivs::iv(as.double(stringr::str_extract(x[aboves], "^\\d+")), Inf )

  alls <- stringr::str_detect(x, "All")
  ret[alls] <- ivs::iv(0, Inf)

  ret
}

# format ivs is not exported from the ivs package.  This function make it available for this package.
# @export
# format.ivs_iv <- function(x, ...){
#   ivs:::format.ivs_iv(x, ...)
# }



# Array dimension label functions ---------------------------------------------------------------------------------

#' Drop Level(s) from an Array Dimension
#' Internal function to handle dropping one or more levels from an array dimension
#' @param arr an array
#' @param dim_name is the name of the dimension to drop the level from
#' @param drop_values are the specific values to drop
#' @returns the modified array
#'
#' @examples
#' tmp <- drop_array_level(census, "age.char", "All")
drop_array_level <- function(arr, dim_name, drop_values) {
  # Find dimension index from its name
  dim_index <- which(names(dimnames(arr)) == dim_name)

  if (length(dim_index) == 0) {
    stop("Dimension name not found: ", dim_name)
  }

  # Get current labels and logical index of values to keep
  current_labels <- dimnames(arr)[[dim_index]]
  keep_idx <- !current_labels %in% drop_values
  if(! any(keep_idx)) stop("Dropping all values from dimension '", dim_name, "' is not allowed.")

  # Create a list of subscripts for each dimension
  subscripts <- rep(list(quote(expr = )), length(dim(arr)))
  subscripts[[dim_index]] <- keep_idx

  # Subset the array using do.call to handle arbitrary dimensions
  result <- do.call(`[`, c(list(arr), subscripts, drop = FALSE))

  # Clean up dimnames
  dimnames(result)[[dim_index]] <- current_labels[keep_idx]

  return(result)
}

#' Assign new values to a dimension
#'
#' Assign a new character vector a levels to a dimension.  Missing values from the current levels are dropped before
#' assigning the new values.
#' @param arr is the array to receive the new value
#' @param dimname the dimension name
#' @param char_vec is the character vector to assign to dimension
#' @return the modified array
assign_array_levels <- function(arr, dimname, char_vec){
  dim_ndx <- which(names(dimnames(arr)) %in% dimname)
  assertthat::assert_that(length(dim_ndx) > 0,
                          msg = str_glue("dimname '{dimname}' is not one of the dimensions for the array"))
  assertthat::assert_that(is.array(arr))
  assertthat::assert_that(is.character(char_vec) & (length(char_vec) > 0),
                          msg = "char_vec must be a character vector with at least one entry")

  # Drop any levels in the current dimension that are not in char_vec
  current_levels <- dim_labels(arr, dimname)
  drop_levels <- current_levels[!current_levels %in% char_vec]
  arr <- drop_array_level(arr, dimname, drop_levels)

  # assign the new levels which must be in the same order as the original
  #dimnames(arr)[[dimname]] <- dim_labels(arr)
  arr
}


#' Dimension Labels
#'
#'  Retrieves the labels for an array dimension with the option to remove one or more labels.
#'  These functions are useful when sub-setting arrays, especially when choosing elements from a tarr_pop array.
#'
#' Simple wrapper functions around dim_label() to retrieve and assign each dimension label values that are included in a
#' tarr_pop object.
#' * ages() the age.char dimension, ages(arr) <- values
#' * sexes() the sex dimension
#' * races() the race dimension
#' * ethnicities() the ethnicity dimension
#' * areas() the areas (counties) dimension
#' * years() the years dimension. Note that this conflicts with the same function name from the lubridate package.
#'           The best practice is to use tarr.population::years()
#'
#'
#' @param arr  a tarr_pop array object
#' @param dimname the name of the dimension to get the labels return
#' @param remove is a character vector which will be removed when returning the labels for a dimension.
#'   Alternatively this can be a regex expression created by the [stringr::regex()] function
#' @param ... used by the wrapper functions to pass arguments to the dim_labels() function.
#'
#' @returns a character vector of labels for the chosen dimension#'
#' @export
#' @examples
#'
#' selected_ages <- dim_labels(census, "age.char", remove = c("All", "85 +"))
#' selected_ages
#' cen <- census[year = "2020",
#'              area.name = "Tarrant",
#'              sex = "All",
#'              age.char = selected_ages,
#'              race = "All",
#'              ethnicity = "All",
#'              drop = FALSE]
#'
#' dimnames(cen)["age.char"]
#'
#' dim_labels(arr = cen, dimname = "age.char", remove =  "All")
#'
#' # or using ages() without removing any age group
#' ages(cen)
#' dim_labels(cen, dimname = "race", remove = stringr::regex(combination))
#'
dim_labels <- function(arr, dimname, remove = NULL){
  checkmate::assert_class(x = arr, classes = "poparray")
  checkmate::assert(
    check_string(dimname),
    check_choice(dimname, names(dimnames(arr)))
  )

  ret <- dimnames(arr)[[dimname]]
  if(! is.null(remove)){
    if (inherits(x = remove, what = "stringr_regex")) {
      ret <- ret[str_detect(ret, remove, negate = TRUE)]
    } else {
      ret <- ret[! ret  %in% remove]
    }
  }
  ret
}



## Functions to retrieve or assign dimension labels -------------------------

#' Factory function to generate wrapper functions for retrieving specific dimension labels from tarr_pop objects
#' @param dimension should be a valid dimension of the array
label_factory <- function(dimension){
  force(dimension)
  function(arr, ...)
  {
    dim_labels(arr, dimname = dimension, ...)
  }
}

#' Factory function to assign dimension levels(labels) in an array.  Wrapper around assign_array_levels().
#' The returned function accepts the array and value.
#'
#' @param dimension is the name of the dimension
#' @rdname dim_labels
#' @returns a function to assign x (an array), levels passed as values, to the specified dimension name
# @export
#'
#' @examples
#' # to do
assign_label_factory <- function(dim_name){
  force(dim_name)
  function(x,  values){
    assign_array_levels(arr = x, dimname = dim_name, char_vec = values)
  }
}

#' @rdname dim_labels
#' @export
ages <- label_factory("age.char")

#' @rdname dim_labels
#' @export
`ages<-` <- assign_label_factory("age.char")


#' @rdname dim_labels
#' @export
races <- label_factory("race")

#' @rdname dim_labels
#' @export
`races<-` <- assign_label_factory("race")

#' @rdname dim_labels
#' @export
ethnicities <- label_factory("ethnicity")

#' @rdname dim_labels
#' @export
`ethnicities<-` <- assign_label_factory("ethnicity")

#' @rdname dim_labels
#' @export
sexes <- label_factory("sex")

#' @rdname dim_labels
#' @export
`sexes<-` <- assign_label_factory("sex")

#' @rdname dim_labels
#' @export
areas <- label_factory("area.name")

#' @rdname dim_labels
#' @export
`areas<-` <- assign_label_factory("area.name")

#' @rdname dim_labels
#' @export
years <- label_factory("year")

#' @rdname dim_labels
#' @export
`years<-` <- assign_label_factory("year")


# --- Convenience function to set source and url attributes for objects
#' Set source attributes
#'
#' Attributes like the source program, url, and date created.
#'
#' @param obj the object to set the source attributes for
#' @param nm the name of the source, e.g. "Texas Demographic Center"
#' @param url the url of the source, e.g. "https://demographics.texas.gov"
#'
#' @return the object with the source attribute set to include name of source,
#' url, and the date the object was last created/revised set to today()
set_source_url <- function(obj, nm, url, pop_type = NULL) {
  attr(obj, "source") <- c(
    note = nm,
    population_type = pop_type,
    source = url,
    updated = as.character(today())
  )
  obj
}



#' Retrieve the tarr_pop source
#'
#' Gets the list of information contained in "source" attribute of 'x'.  Objects from tarr_pop have a source attributes
#' as possibly other objects.
#' @param x is the object with a source attribute, like a tarr_pop array.
#' @return a named list of character  - names are note, source and updated.
#' @keywords internal
#' @export
get_source <- function(obj){
  ret <- attr(obj, which = "source", exact = TRUE)
  if(is.null(ret)) ret <- c(note = "", source = "Not given", updated = "Unknown", population_type = "Unknown")
  ret
}


# Mathmatical and summary operatios -------------------------------------------------------------------------------
sum.poparray(x, ..., na.rm = FALSE){
  DelayedArray::
  
  
}



# is_missing argument -------------------------------------------------------------------
# Checks if an expression is missing, such as missing arguments or when looking for missing
# in ... type arguments
is_missing_arg <- function(e){identical (e, quote(expr=))}

