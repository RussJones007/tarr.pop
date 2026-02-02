# pop_retriev_2.r
#
#  This is a re-write for getting population figures
# Introduces the generic function "county_population"

#' County population
#'
#' Using a population data frame or tarr_pop (table/array) object, retrieves one or more county population figures. The
#' function arguments are used to retrieve and limit the population data to the county, year, and demographics of
#' interest. Additional functions are used to collapse on categories, for example collapsing ages into age groups
#' [group_ages()]. The preferred option is to use a tarr_pop from either the [population] or [seer] list as a tarr_pop
#' objectit is approximately 140 times faster than using a data frame, and uses 20% of the memory.
#'
#' @param pop the data source such as a data frame , or a tarr_pop (table) object from the [population] list.
#' @param year an integer vector of the year(s) of data to return. When multiple years are specified in a an integer
#'   vector, each year will be returned.
#' @param county a character vector the county name(s) in title case to return. The default is "Tarrant"
#' @param sex a character vector  of "Female", "Male", "All". The default is c("Female", "Male").
#' @param age are the ages. All the ages as separate entries is the default.  Pass "ALL" to get all the ages combined.
#'   Passing a vector of "cut points generated manually or using seq() function or the ':' operator, e.g., 10:25 for
#'   those tarr_pop that have single year ages will cause only those ages to be returned
#'   A character vector may also be passed to obtain  age groups such as "85 +" or "1-5".
#' @param race a character vector of race entries. The default is all races returned. Select races can be done by using
#'   a character vector like 'c("Asian", "Black")'.  Allowed values are dependent on the the data source passed in the
#'   pop argument. Refer to the [population] help topic for race categories that may be used for each data source.  Using "All for this argument
#'   will cause the returned data set show data with all races combined.
#' @param ethnicity a character vector with the following values "Hispanic", or "Non-Hispanic", "All". The default
#' is 'c("Hispanic", "Non-Hispanic").  If you ue "All" by iteslf, all ethinicites are retunred as a combined popuation.
#' @param ... additional arguments to pass, not currently implemented.
#'
#' @return a data.frame or table object containing the population figures.  The data frame column and the tarr_pop
#' dimension names are the same: \itemize{
#' \item year: the year(s) as an integer
#' \item fips:  the county FIPS code as a character string (not present in a tarr_pop)
#' \item area.name: name of the Texas county in title case. (e.g., El Paso)
#' \item race:  the race for each record, defaults to "All"
#' \item sex: can be "Female", "Male", or "All" which is the total of male and female.
#' \item age: character string of the age, or age group e.g. "5" or "1-5".
#' \item population: is the population figure for the combination of the names above.
#' }
#'
#' @export
#'
#' @examples
#' # Retrieve the population estimates form the census bureau for 2022 for Tarrant and Johnson counties as a tarr_pop
#' pop_22 <- county_population(pop = population$census.estimates, year = "2022", county = c("Tarrant", "Johnson"))
#'
#' # The dimensions in the table
#' dimnames(pop_22) |> names()
#'
#' # How the population was subsetted
#' attr(pop_22, "filter")
#'
#' # Convert to a data framne
#' pop_22_df <- as.data.frame(pop_22)
#'
#' # total population
#' pop_22_df$population |> sum()
#'
county_population <- function(pop,
                              year,
                              county    = "Tarrant",
                              sex       = "All",
                              age       = "All",
                              race      = "All",
                              ethnicity = "All",
                              ...
){  UseMethod("county_population") }

#' County Population from Data Frame
#'
#' @inheritParams county_population
#' @return dataframe with the requested population \itemize{
#' \item year: the year(s) as an integer
#' \item fips:  the county FIPS code as a character string
#' \item area.name: name of the Texas county in title case. (e.g., El Paso)
#' \item race:  the race for each record, defaults to "All"
#' \item sex: can be "Female", "Male", or "All" which is the total of male and female.
#' \item age: character string of the age, or age group e.g. "5" or "1-5".
#' \item population: is the population for the group.
#' }
#' @export
county_population.data.frame <- function(pop,
                                         year,
                                         county    = "Tarrant",
                                         sex       = "All",
                                         age       = "All",
                                         race      = "All",
                                         ethnicity = "All",
                                         ...
){
  # check that a data frame with the correct fields are available.  Checks only field names
  # population$census is used as the prototype for the required field names
  assert_that(pop %has_name% names(population$census |> dimnames() |> names()),
              msg = "The data frame passed in .pop_df does not have all the required fields for processing")

  # check that passed argument values are available for the data frame chosen
  avail <- get_available_values(.pop_df)

  argument_list <- list(year      = year,
                        race      = race,
                        ethnicity = ethnicity,
                        sex       = sex,
                        area.name = county)

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
#}
#
  # return the filtered data frame with fields in the same order as .pop_df
  flds <- names(.pop_df)[names(.pop_df) %in% names(ret)]
  ret <- ret |>
    mutate( across(where(is.factor), droplevels)) |>
    arrange(year, area.name,age.iv, sex) |>
    select(all_of(flds))

  attr(ret, "filter") <- argument_list
  return(ret)
}


#' County Population dataset
#'
#'  From a tarr_pop, which is a type of HDF5 array object, a subset of data can be created. You may use the
#'  table as needed with something like a a table of disease counts to create rates.
#'  Often it is easier to manipulate the subset as a data frame in which case the function [as.data.frame()].
#'
#' @inheritParams county_population
#' @return a HDF5 array of integers containing population figures subset.  Each dimension of the table corresponds
#' to the function arguments.  The returned item has a "filter" attribute that contains the arguments used to create
#' the returned tarr_pop.
#'
#' @export
#'

#' County population from chunked array (lazy by default)
#' @param dataset which cube to query (estimates/census variants)
#' @param area.name county names or IDs (NULL = all)
#' @param sex,age,race,ethnicity filter values; numeric (indices) or character (levels)
#' @param years vector of years (NULL = all)
#' @param collect if TRUE, return a realized base R array
#' @param as_tibble if TRUE, return a long tibble (implies collect)
county_population.tarr_pop <- function(dataset,
                              area.name = NULL,
                              sex = NULL,
                              age = NULL,
                              race = NULL,
                              ethnicity = NULL,
                              years = NULL,
                              collect = FALSE,
                              as_tibble = FALSE) {
  x <- open_cube(dataset)
  id <- resolve_indices(x,
                        area.name = area.name, sex = sex, age = age,
                        race = race, ethnicity = ethnicity, years = years)

  # slice in array order: age × sex × race × ethnicity × county × year
  slc <- x[id$age, id$sex, id$race, id$ethnicity, id$county, id$year, drop = FALSE]

  if (as_tibble) {
    collect <- TRUE
  }
  if (!collect) return(slc)  # lazy DelayedArray view

  arr <- as.array(slc)       # materialize now

  if (!as_tibble) return(arr)

  # tidy long representation
  dn <- dimnames(slc)
  aperm(arr, c(5, 6, 1, 2, 3, 4)) |>
    as.data.frame.table(responseName = "population", stringsAsFactors = FALSE) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    dplyr::rename(county = Var1, year = Var2, age = Var3,
                  sex = Var4, race = Var5, ethnicity = Var6) |>
    dplyr::mutate(year = as.integer(year),
                  population = as.integer(population))
}



#' Ages into Groups
#'
#' The ages dimension or column name is set to age groups and the count data (population) is summed in the new
#' returned object.
#'
#' @param pop a tarr_pop (array) object with an age dimension name or column
#' @param age_groups The desired age groups to use, this can be a vector that [rage::as.age_group()] accepts such
#' as a numeric or character vector.  Character vectors must conform to the format expected  by [rage::as.age_group()]
#' @param dimension_name The dimension or column name that contains the age data.  "age.char" is the default.
#'
#' @returns The object type as in the pop argument, age groups collapsed and the count data (population) summed.
#' @export
#'
#' @examples
#' census <- county_population(
#'      population$census.bureau$census(),
#'      year = c("2010", "2020"),
#'      county = "Tarrant",
#'      sex = c("Female", "Male"),
#'      age  =  c(as.character(0:99), "100-104"),
#'      race = "All",
#'      ethnicity = c("Hispanic", "Non-Hispanic"),
#'      drop = FALSE)
#'
#' census_age_grouped <- group_ages(pop = census, rage::age.groups$ILI)
group_ages <- function(pop, age_groups, dimension_name = "age.char"){

  assertthat::assert_that(inherits(pop, "tarr_pop"))

  # get the ages in pop without "All" being present and covert to age_group
  age <- ages(pop, remove = "All") |>
    rage::as.age_group()

  # take the age_groups argument and convert to age_group object
  age_groups <-  rage::as.age_group(age_groups)

  # locate where the original age categories and desired age groups overlap and align them
  al <- ivs::iv_locate_overlaps(needles = age,
                                haystack = age_groups,
                                relationship = "many-to-one") |>
    ivs::iv_align(age, age_groups, locations = _)

  # create a named list with desired age groups as the names, and the current age categories as the vector.
  grouping <- map(age_groups, ~ al[al$haystack == .x, "needles" ] |>
                    as.character() |>
                    unlist(use.names = FALSE)) |>
    set_names(unique(age_groups) |> as.character())

  ret <- group_array_by_levels(arr = pop, dim_name = dimension_name, groups = grouping, keep_unmapped = "keep")
  return(ret)

  dim_nms <- names(dimnames(pop))
  dim_ndx <- which(dim_nms == dimension_name)

  #dimnames(pop)[[dimension_name]] <- age_grps
  unique_grps <- unique(age_grps)
  new_dims <- dim(pop)
  new_dims[dim_ndx] <- length(unique_grps)

  result <- array(0, dim = new_dims)
  marg <- dim_nms[-dim_ndx]  # Exclude age dimension from margins

  # Create index template for array assignment
  indices <- rep(list(alist(,,)[[1]]), length(dim(pop)))

  for(i in seq_along(unique_grps)) {
    group <- unique_grps[i]
    ndx <- which(dimnames(pop)[[dimension_name]] == group)

    # Dynamic subsetting for age dimension
    age_subset <- list(ndx)
    names(age_subset) <- dimension_name

    # Sum across the age groups
    summed <- apply(
      X = abind::asub(pop, age_subset, dims = dim_ndx, drop = FALSE),
      MARGIN = marg,
      FUN = sum
    )

    # Dynamic assignment to result array
    indices[[dim_ndx]] <- i
    result <- do.call(`[<-`, c(list(result), indices, list(value = summed)))
  }

  # Update dimension names
  new_dimnames <- dimnames(pop)
  new_dimnames[[dimension_name]] <- unique_grps
  dimnames(result) <- new_dimnames

  structure(result,
            data_col = get_data_col(pop),
            filter = attr(pop, "filter"),
            source = attr(pop, "source"),
            class = class(pop))
}


#' Group an Array Dimension
#'
#' Groups specified dimension into categories and sums population counts.
#'
#' @param arr An array such as a `tarr_pop` array or `tarr_pop_df` object
#' @param dim_name Name of dimension to group
#' @param groups Group specification which maps the new levels to those levels being collapsed. This can be a named list
#'   with the name of the group and values being what will be in the group, or a named vector mapping each new group to
#'   an individual level.
#' @param keep_unmapped a character vector of length one flagging on what happens when not all the levels are included
#'   in groups.  The values this argument can be:
#'   "error"  - throw an error and stop if all the original levels are not included.
#'   "drop"   - exclude un-mapped levels entirely.
#'   "keep"   - keep un-mapped levels with their original labels.
#'   "other"  - all un-mapped levels are summarized and placed under the other_label.
#'  @param other_label is where any levels not included in groups, but "other" is set, are summed into this category.
#'    "Other" is the default level name.
#' @returns Object with grouped dimension and summed counts
#'
#' @examples
#' # Group age dimension
#' # Coming soon
group_array_by_levels <- function(arr,
                                  dim_name,
                                  groups,
                                  keep_unmapped = c("error", "drop", "keep"),
                                  other_label = "Other") {
  # --- Validate inputs ------------------------------------------------------->
  assert_that(is.array(arr),
              ! is.null(dimnames(arr)),
              ! is.null(names(dimnames(arr))),
              msg =  "'arr' must be an array with named dimesions that have named levels")

  keep_unmapped <- match.arg(keep_unmapped)

  # --- Locate the target dimension by NAME ---------------------------------->
  dim_index <- which(names(dimnames(arr)) == dim_name)
  assert_that(length(dim_index) > 0,
              msg = str_glue("Dimension name '{dim_name}' was not found"))
  assert_that(is.scalar(dim_index),
              msg = str_glue("Duplicate '{dim_name}' names detected "))

  old_levels <- dimnames(arr)[[dim_index]]
  assert_that(! is.null(old_levels),
              msg = str_glue("'{dim_name}' dimension has no names (levels)"))

  # --- Build an old->new level MAP from `groups` ----------------------------->
  # Accept either:
  #  - named list: list("groupA" = c("a","b"), "groupB" = c("c"))
  #  - named character vector: c(a="groupA", b="groupA", c="groupB")
  if (is.list(groups)) {
    if (is.null(names(groups)) || any(names(groups) == "")) {
      stop("`groups` list must be named; names are the new grouped labels.")
    }
    # Flatten to a named character vector: names = old levels, values = new group
    level_map <- unlist(
      lapply(names(groups), function(g) {
        olds <- groups[[g]]
        if (!is.character(olds)) {
          stop("Each element in `groups` list must be a character vector of old levels.")
        }
        stats::setNames(rep(g, length(olds)), olds)
      }),
      use.names = TRUE
    )
  } else if (is.character(groups) && !is.null(names(groups))) {
    # Already an old -> new mapping
    level_map <- groups
  } else {
    stop(
      "`groups` must be either a named list of character vectors ",
      "(new_group = c(old1, old2, ...)) or a named character vector ",
      "mapping old_level -> new_group."
    )
  }

  # --- Determine new label for every old level -------------------------------->
  # Map known olds; produce NA for unknowns (unmapped)
  mapped <- level_map[old_levels]
  # `mapped` is a character vector aligned to old_levels; NAs where unmapped
  unmapped <- is.na(mapped)

  if (any(unmapped)) {
    unknown_levels <- old_levels[unmapped]
    if (keep_unmapped == "error") {
      stop(
        "Unmapped levels in '", dim_name, "': ",
        paste(unknown_levels, collapse = ", "),
        ". Provide a mapping or choose keep_unmapped = 'drop' or 'keep'."
      )
    } else if (keep_unmapped == "keep") {
      # Keep them under their original labels
      mapped[unmapped] <- unknown_levels
    } else { # "drop"
      # leave as NA; we will drop these columns during aggregation
      # (no action here)
    }
  }

  # Final set of new group labels (including 'Other' if user had chosen that label explicitly)
  new_groups <- unique(mapped[!is.na(mapped)])

  # --- Fast aggregation along the named dimension ---------------------------->
  # 1) Permute so target dim is LAST
  d <- dim(arr)
  perm <- c(setdiff(seq_along(d), dim_index), dim_index)
  arr_perm <- aperm(arr, perm)

  # 2) Reshape to 2D: rows = product(other dims), cols = length(old_levels)
  n_cols <- length(old_levels)
  n_rows <- prod(d[-dim_index])
  mat <- matrix(arr_perm, nrow = n_rows, ncol = n_cols)

  # 3) Build a column-group index for aggregation
  #    For each new group label, sum columns where mapped == that label
  keep_cols <- !is.na(mapped)
  mat_kept <- mat[, keep_cols, drop = FALSE]
  mapped_kept <- mapped[keep_cols]

  # If everything got dropped, fail gracefully
  if (ncol(mat_kept) == 0L) {
    stop("After applying mapping and `keep_unmapped = 'drop'`, no levels remain.")
  }

  # Aggregate columns by new group using rowsums over each group
  # Pre-allocate result matrix
  k <- length(unique(mapped_kept))
  mat_out <- matrix(0, nrow = n_rows, ncol = k)
  colnames(mat_out) <- unique(mapped_kept)

  # Preserve the order of groups as they first appear in `mapped_kept`
  group_order <- match(unique(mapped_kept), unique(mapped_kept))
  # Sum columns for each group
  for (g in seq_len(k)) {
    cols_g <- mapped_kept == colnames(mat_out)[g]
    # Sum across selected columns (NA-safe: sum treats NA as NA unless na.rm=TRUE)
    mat_out[, g] <- if (any(cols_g)) {
      # Use base rowSums for speed
      rowSums(mat_kept[, cols_g, drop = FALSE], na.rm = TRUE)
    } else {
      0
    }
  }

  # 4) Reshape back to array:
  #    new dims = original dims but replace target dim length by k
  new_dims <- d
  new_dims[dim_index] <- k

  # Build new dimnames: same as original, but with grouped labels on target dim
  new_dimnames <- dimnames(arr)
  new_dimnames[[dim_index]] <- colnames(mat_out)

  # Rebuild with target dim last, then aperm back
  arr_out_last <- array(mat_out, dim = c(d[setdiff(seq_along(d), dim_index)], k))
  # Inverse permutation to original order
  inv_perm <- match(seq_along(perm), perm)
  arr_out <- aperm(arr_out_last, inv_perm)
  dimnames(arr_out) <- new_dimnames
  attr(arr_out, "data_col") <- attr(arr, "data_col")
  class(arr_out) <- class(arr)

  return(arr_out)
}
