# -------------------------------------------------------------------------------------->
# Script: poparray_classr
# Description:
#   Constrcotr and methods for generics for the poparray class.  This is a refactoring of the older tarr_pop class
#   with more specific contracted behavior enforing a time and area dimesions allowing for other dimseions
#   to be added to an array as options. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: `Feb 3, 2026
# Revised:
# -------------------------------------------------------------------------------------->

#' Construct a poparray
#'
#' Creates a poparray object: a lazy, role-aware population cube backed by a `DelayedArray` (often an `HDF5Array`) with
#' explicit dimension labels and enforced invariants for time and area dimensions.
#'
#' A poparray must include exactly one time dimension and one area dimension (by default `"year"` and `"area.name"`),
#' and all other dimensions are treated as optional stratification dimensions.
#'
#' The constructor is lazy-first: it wraps non-delayed inputs in `DelayedArray::DelayedArray()` and does not realize the
#' full array in memory.
#'
#' @param x A `DelayedArray`-compatible object (e.g., `HDF5Array`, `DelayedArray`, or an in-memory array/matrix that can
#'   be wrapped).
#' @param dimnames_list Named list of dimension labels. Must have the same number of elements as `dim(x)`, and each
#'   element length must match the corresponding dimension extent. Dimension labels must be unique within each
#'   dimension.
#' @param data_col Single character string giving the value-column name to use when coercing to tabular formats (e.g.,
#'   via `as.data.frame()`/`as_tibble()` methods).
#' @param source Optional metadata describing the data source/provenance (character or list).
#' @param time_dim Single character string naming the time dimension. Defaults to `"year"`. If not `"year"`, it must be
#'   supplied explicitly and must exist in `names(dimnames_list)`. The labels for this dimension must be ordered.
#' @param area_dim Single character string naming the area dimension. Defaults to `"area.name"`. Must exist in
#'   `names(dimnames_list)`.
#' @param ... Reserved for future use.
#'
#' @details The resulting object stores:
#' - `handle`: the delayed backend (numeric array-like data),
#' - `dimn`: the explicit dimnames list,
#' - attributes including `dimroles` (a named list with `time`, `area`, and
#' `strata`) and `data_col`.
#'
#' @return An S3 object of class `"poparray"`.
#'
#' @examples
#' \dontrun{
#' library(DelayedArray)
#'
#' arr <- array(1:12, dim = c(3, 4))
#' dimnames(arr) <- list(
#'   year = c("2020", "2021", "2022"),
#'   area.name = paste0("A", 1:4)
#' )
#' 
#' pa <- new_poparray(arr)
#' pa
#'
#' # Non-default roles (must be explicit and ordered)
#' dimnames(arr) <- list(
#'   time = c("2020", "2021", "2022"),
#'   area.name = paste0("A", 1:4)
#' )
#' pa2 <- new_poparray(arr, time_dim = "time", area_dim = "area.name")
#' }
#'
#' @export
new_poparray <- function(x,
                         dimnames_list = dimnames(x),
                         data_col = "population",
                         source = NULL,
                         time_dim = "year",
                         area_dim = "area.name",
                         ...) {
  
  if (!inherits(x, "DelayedArray")) x <- DelayedArray::DelayedArray(x)
  
  if (is.null(dimnames_list) || !is.list(dimnames_list) || is.null(names(dimnames_list))) {
    cli::cli_abort("{.arg dimnames_list} must be a named list of dimension labels.")
  }
  
  nms <- names(dimnames_list)
  
  if (!is.character(time_dim) || length(time_dim) != 1) cli::cli_abort("{.arg time_dim} must be length 1.")
  if (!is.character(area_dim) || length(area_dim) != 1) cli::cli_abort("{.arg area_dim} must be length 1.")
  if (!time_dim %in% nms) cli::cli_abort("Time dim {.val {time_dim}} not found in dimnames_list.")
  if (!area_dim %in% nms) cli::cli_abort("Area dim {.val {area_dim}} not found in dimnames_list.")
  if (identical(time_dim, area_dim)) cli::cli_abort("{.arg time_dim} and {.arg area_dim} must be different.")
  if(length(data_col) !=1) cli::cli_abort("{arg data_col} must be length 1")
  
  obj <- list(handle = x, dimn = dimnames_list)
  
  
  attr(obj, "data_col") <- data_col
  attr(obj, "source")   <- source
  attr(obj, "dimroles") <- list(
    time   = time_dim,
    area   = area_dim,
    strata = setdiff(nms, c(time_dim, area_dim))
  )
  
  class(obj) <- "poparray"
  validate_poparray(obj)
  obj
}


#' Validate poparray object
#'
#' @param x a poparray object to validte
#'
#' @returns x, otherwise throws an error
#' @keywords internal
validate_poparray <- function(x) {
  if (!inherits(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray}.")
  }
  
  d <- dim(x$handle)
  if (is.null(d)) cli::cli_abort("poparray backend has no dimensions.")
  
  dn <- x$dimn
  if (!is.list(dn) || is.null(names(dn))) {
    cli::cli_abort("{.arg dimnames_list} must be a named list.")
  }
  if (length(dn) != length(d)) {
    cli::cli_abort("dimnames_list has {length(dn)} dims but backend has {length(d)}.")
  }
  
  for (k in seq_along(d)) {
    nm <- names(dn)[k]
    if (length(dn[[k]]) != d[[k]]) {
      cli::cli_abort("Dim {.val {nm}} labels have length {length(dn[[k]])} but extent is {d[[k]]}.")
    }
    if (anyDuplicated(dn[[k]]) > 0) {
      cli::cli_abort("Dim {.val {nm}} has duplicated labels; labels must be unique.")
    }
  }
  
  roles <- attr(x, "dimroles", exact = TRUE)
  if (is.null(roles) || is.null(roles$time) || is.null(roles$area)) {
    cli::cli_abort("poparray must have {.field dimroles} with {.field time} and {.field area}.")
  }
  if (!is.character(roles$time) || length(roles$time) != 1) cli::cli_abort("{.field dimroles$time} must be length 1.")
  if (!is.character(roles$area) || length(roles$area) != 1) cli::cli_abort("{.field dimroles$area} must be length 1.")
  if (identical(roles$time, roles$area)) cli::cli_abort("Time and area dims must be different.")
  if (!roles$time %in% names(dn)) cli::cli_abort("Time dim {.val {roles$time}} not found in dimnames.")
  if (!roles$area %in% names(dn)) cli::cli_abort("Area dim {.val {roles$area}} not found in dimnames.")
  
  # Ordered time labels
  tlab <- dn[[roles$time]] |> as.character()
  if(anyNA(tlab)) cli::cli_abort("Time dim {.val {roles$time}} cannot have any NA values.")
  
  
  if (all(("^[-+]?[0-9]+$", tlab))) {
    tnum <- as.integer(tlab)
    if (is.unsorted(tnum, strictly = FALSE)) {
      cli::cli_abort("Time dim {.val {roles$time}} labels must be ordered (increasing).")
    }
  } else {
    if (is.unsorted(tlab, strictly = FALSE)) {
      cli::cli_abort("Time dim {.val {roles$time}} labels must be ordered.")
    }
  }
  
  invisible(x)
}



#' @export
is.poparray <- function(x) inherits(x, "poparray")


#  Dimension names and labels -----------------------

#' Get dimensions of a poparray
#'
#' Returns the array dimensions of a `poparray`. This delegates to the delayed
#' backend and is a cheap, metadata-only operation (it does not realize the
#' full array).
#'
#' @param x A poparray.
#'
#' @return An integer vector giving the extents of each dimension.
#' @export
dim.poparray <- function(x) {
  d <- base::dim(x$handle)
  
  if (is.null(d)) {
    cli::cli_abort("poparray backend has no dimensions.")
  }
  
  dn <- dimnames(x)
  if (!is.null(dn) && length(dn) != length(d)) {
    cli::cli_abort("poparray dimnames ({length(dn)}) do not match backend dims ({length(d)}).")
  }
  
  d
}



#' Get dimension names for a poparray
#' 
#'
#' Returns the named list of dimension labels stored in the poparray metadata.
#' This is a metadata-only operation and does not realize the delayed backend.
#'
#' @param x A poparray.
#' @param ... Unused.
#'
#' @return A named list of dimension labels (one character vector per dimension).
#' @export
dimnames.poparray <- function(x, ...) {
  dn <- x$dimn
  
  if (is.null(dn) || !is.list(dn) || is.null(names(dn))) {
    cli::cli_abort("poparray has no valid {.field dimn} dimnames metadata.")
  }
  
  d <- dim(x$handle)
  if (!is.null(d) && length(dn) != length(d)) {
    cli::cli_abort("poparray dimnames ({length(dn)}) do not match backend dims ({length(d)}).")
  }
  
  dn
}

#' @export
names.poparray <- function(x) {
  names(dimnames(x))
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


# Indexing operator ---------------------------
# 
#' @exportS3Method "[", poparray
#' Subset a poparray
#'
#' Subsets the delayed backend and updates the stored dimnames metadata. By default (`drop = FALSE`) the result remains
#' a `poparray`. If `drop = TRUE` and subsetting would drop either the time or area dimension (per `dimroles`), the
#' method returns the underlying subsetted `DelayedArray` instead of a `poparray`.
#' 
#' @param x A poparray.
#' @param ... Indices, either positional (like base arrays) or named by dimension (e.g., `x[year = "2020", sex =
#'   "Female"]`). Missing indices in positional form are treated as `TRUE` (select all).
#' @param drop Logical; passed to the backend `[` call.
#'
#' @return A `poparray` or (if `drop = TRUE` drops time/area) a `DelayedArray`.
#' @exportS3Method "[" poparray
`[.poparray` <- function(x, ..., drop = FALSE) {
  dim_names <- names(x)
  roles <- attr(x, "dimroles", exact = TRUE)
  
  nd <- length(dim(x$handle))
  if (length(dim_names) != nd) {
    cli::cli_abort("poparray dimnames are inconsistent with backend dimensions.")
  }
  
  # Capture and evaluate indices; treat "missing" in ... as TRUE (select all)
  dots <- as.list(substitute(list(...)))[-1L]
  idx <- lapply(dots, \(e) {
    if (is_missing_arg(e)) TRUE else eval(e, parent.frame())
  })
  
  ndx <- rep(list(TRUE), nd)
  
  index_names <- names(idx)
  if (is.null(index_names)) index_names <- character(0)
  
  # Named vs positional dispatch
  if (length(index_names) > 0) {
    bad <- setdiff(index_names, dim_names)
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "Unknown dimension name(s) in subset: {paste(bad, collapse = ', ')}.",
        "i" = "Valid dimensions are: {paste(dim_names, collapse = ', ')}."
      ))
    }
    ndx[match(index_names, dim_names)] <- idx
  } else {
    if (length(idx) > nd) cli::cli_abort("Too many indices for poparray.")
    ndx[seq_along(idx)] <- idx
  }
  
  # Subset delayed backend (still lazy)
  h_sub <- do.call(`[`, c(list(x$handle), ndx, list(drop = drop)))
  
  # Rebuild dimnames metadata to reflect the selection
  dn0 <- dimnames(x)
  dn <- dn0
  
  for (k in seq_len(nd)) {
    sel <- ndx[[k]]
    nm <- dim_names[[k]]
    this <- dn0[[nm]]
    
    if (isTRUE(identical(sel, TRUE))) next
    
    if (is.numeric(sel) || is.logical(sel)) {
      dn[[nm]] <- this[sel]
    } else {
      # character (or other atomic) selection by labels; preserve user order
      dn[[nm]] <- sel[sel %in% this]
    }
  }
  
  # If drop=TRUE would drop time or area, return the raw subsetted DelayedArray
  if (isTRUE(drop)) {
    if (length(dn[[roles$time]]) == 1L || length(dn[[roles$area]]) == 1L) {
      return(h_sub)
    }
  }
  
  # Otherwise return a valid poparray slice, preserving metadata/roles
  new_poparray(
    x = h_sub,
    dimnames_list = dn,
    data_col = attr(x, "data_col", exact = TRUE),
    source = attr(x, "source", exact = TRUE),
    time_dim = roles$time,
    area_dim = roles$area
  )
}
