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
#' explicit dimension labels.  Dimensions representing time and area are required (invariants). 
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
  if(length(data_col) !=1 || is.na(data_col) || ! is.character(data_col) )   cli::cli_abort("{.arg data_col} must be length 1 and not NA")
  
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
  
  
  if (all(grepl("^[-+]?[0-9]+$", tlab))) {
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


#  dim,  names, labels, and length  -----------------------

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

#' Length of a poparray
#'
#' Returns the total number of cells in the poparray, equivalent to
#' `prod(dim(x))`. This is a metadata-only operation and does not realize the
#' delayed backend.
#'
#' @param x A poparray.
#'
#' @return A numeric scalar giving the total number of elements.
#' @export
length.poparray <- function(x) {
  d <- dim(x)
  if (is.null(d)) cli::cli_abort("poparray backend has no dimensions.")
  as.numeric(prod(d))
}


# Print and summary methods -----------------------------------------------------------------------------------------

#' @exportS3Method base::print
print.poparray <- function(x, ...) {
  src <- get_source(x)
  roles <- attr(x, "dimroles", exact = TRUE)
  
  dms <- dimnames(x)
  dms_names <- names(dms)
  
  dms_sizes <- lengths(dms)
  names(dms_sizes) <- dms_names
  
  dimensions <- paste(
    paste0(names(dms_sizes), " (", dms_sizes, ")"),
    collapse = ", "
  )
  
  recs <- length(x)
  
  cat("Class <poparray>\n")
  cat("Series: ", src[["note"]], "\n", sep = "")
  cat("Sourced: ", src[["source"]], "\n", sep = "")
  cat("Updated: ", src[["updated"]], "\n", sep = "")
  cat("Length: ", format(recs, big.mark = ","), "\n", sep = "")
  cat("Roles: time = '", roles$time, "', area = '", roles$area, "'\n", sep = "")
  cat("Dimensions: ", dimensions, "\n", sep = "")
  cat("Data column as data frame: '", data_col(x), "'\n", sep = "")
  
  invisible(x)
}

#' Summary of a poparray (may scan backend)
#'
#' Computes basic summaries of the numeric values in a poparray. This operation
#' preserves delayed execution where possible, but it may still require scanning
#' the full backend and can be expensive for large cubes.
#'
#' @param object A poparray.
#' @param ... Passed to DelayedArray reduction functions (if used).
#'
#' @return A named numeric vector (class `summaryDefault`) similar to
#'   `summary.numeric()`.
#' @export
summary.poparray <- function(object, ...) {
  x <- object
  validate_poparray(x)
  
  h <- x$handle
  
  # Prefer DelayedArray reductions (avoid as.array())
  # NOTE: these return small realized scalars.
  n_na <- DelayedArray::sum(is.na(h), na.rm = TRUE, ...)
  
  s <- DelayedArray::sum(h, na.rm = TRUE, ...)
  mn <- DelayedArray::min(h, na.rm = TRUE, ...)
  mx <- DelayedArray::max(h, na.rm = TRUE, ...)
  mu <- DelayedArray::mean(h, na.rm = TRUE, ...)
  
  ret <- c(
    Min. = as.numeric(mn),
    Mean = as.numeric(mu),
    Max. = as.numeric(mx),
    Sum  = as.numeric(s),
    NA.s = as.numeric(n_na)
  )
  
  class(ret) <- "summaryDefault"
  ret
}




# Indexing operator ----------------------------------------------------
# 
#' Subset a poparray
#'
#' Subsets the poparray object and updates the stored dimnames metadata. By default (`drop = FALSE`) the result remains
#' a `poparray`. If `drop = TRUE` and subsetting would drop either the `dimroles` time or area dimensions , the
#' method returns the underlying subsetted `DelayedArray` instead of a `poparray`.
#' 
#' @param x A poparray.
#' @param ... Indices, either positional (like base arrays) or named by dimension (e.g., `x[year = "2020", sex =
#'   "Female"]`). Missing indices in positional form are treated as `TRUE` (select all).
#' @param drop Logical; passed to the backend `[` call.
#'
#' @return A `poparray` or (if `drop = TRUE` drops time/area) a `DelayedArray`.
#' @export
`[.poparray` <- function(x, ..., drop = FALSE) {
  dim_names <- names(x)
  
  # check that the poparry object has the proper roles available.  This handles manually made poparrays that are missing
  # required attributes
  validate_poparray((x))
  
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
      sel_chr  <- as.character(sel)
      this_chr <- as.character(this)
      # check that selected labesl actually exist, if not indicates an error. 
      unknown  <- setdiff(sel_chr, this_chr)
      if(length(unknown) > 0){
        cli::cli_abort(c(
          "Unknown label(s) in dim {.val {nm}}.",
          "i" = "Unknown: {paste(utils::head(unknown, 10), collapse = ', ')}{if (length(unknown) > 10) ', ...' else ''}.",
          "i" = "Valid labels example: {paste(utils::head(this_chr, 10), collapse = ', ')}{if (length(this_chr) > 10) ', ...' else ''}."
        ))
      }
      dn[[nm]] <- sel_chr
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



# Coere to double, data.frame, or tibble functions ------------------------------------------------------------------------

#' Coerce poparray values to double (lazy)
#'
#' Returns a poparray with the same dimensions/metadata but with the delayed
#' backend coerced to double storage where possible.
#'
#' @param x A poparray.
#' @param ... Unused.
#'
#' @return A poparray backed by a double-typed delayed array.
#' @export
as.double.poparray <- function(x, ...) {
  validate_poparray(x)
  
  h2 <- as.double(x$handle)  # should remain delayed if DelayedArray supports it
  
  new_poparray(
    x = h2,
    dimnames_list = dimnames(x),
    data_col = attr(x, "data_col", exact = TRUE),
    source = attr(x, "source", exact = TRUE),
    time_dim = attr(x, "dimroles", exact = TRUE)$time,
    area_dim = attr(x, "dimroles", exact = TRUE)$area
  )
}


#' Coerce poparray to data frame (EAGER)
#'
#' S3 method to coerce a poparray to a data frame. This method **realizes** the delayed backend (for the current
#' poparray slice) and converts it to a long data.frame via `as.table()` semantics (one row per cell).
#'
#' For large cubes, subset first (e.g., restrict years/areas) to avoid materializing an unmanageably large array.
#'
#' @param x A poparray.
#' @param stringsAsFactors Passed to `as.data.frame.table()`.
#' @param responseName Name of the value column (defaults to `data_col(x)`).
#' @param bytes_threshold is the number of bytes to warnn you shen a large data frame isbing retunrd. Default is 40 MB.
#' @param ... Passed to `as.data.frame.table()` (rarely needed).
#'
#' @return A data.frame with one column per dimension plus `responseName`.
#' @export
as.data.frame.poparray <- function(x,
                                   stringsAsFactors = TRUE,
                                   responseName = data_col(x),
                                   bytes_threshold = 40e6,
                                   ...) {
  validate_poparray(x)
  # Warn when realizing the DelayedArray
  warn_if_realization_large(x, bytes_threshold = bytes_threshold)
  
  # EAGER: materialize the current slice
  arr <- as.array(x$handle)
  dimnames(arr) <- dimnames(x)
  
  df <- as.data.frame(
    as.table(arr),
    stringsAsFactors = stringsAsFactors,
    responseName = responseName,
    ...
  )
  
  polish_df(df, stringsAsFactors = stringsAsFactors, time_dim = attr(x, "dimroles", exact = TRUE)$time)
}

#' Coerce poparray to tibble (EAGER)
#'
#' Coercion is done via `as.data.frame.poparray()` for consistency, then
#' converted to a tibble.
#'
#' @param x A poparray.
#' @param stringsAsFactors Logical; passed to `as.data.frame.poparray()`.
#' @param ... Passed to `as.data.frame.poparray()`.
#' @param .name_repair Name repair strategy for tibble.
#'
#' @return A tibble.
#' @export
as_tibble.poparray <- function(x,
                               stringsAsFactors = TRUE,
                               ...,
                               .name_repair = c("check_unique", "unique",
                                                "universal", "minimal")) {
  as.data.frame(x, stringsAsFactors = stringsAsFactors, ...) |>
    tibble::as_tibble(.name_repair = .name_repair)
}




# Split a poparry -------------------------------------------------------------------------------------------------

#' Split a poparray by a dimension
#'
#' Splits a poparray into a named list of slices, one per label of a chosen
#' dimension. This method preserves laziness by subsetting the delayed backend.
#'
#' If `drop = FALSE` (default), each element is a `poparray`. If `drop = TRUE`
#' and the split dimension is the time or area role (per `dimroles`), each slice
#' will drop that role and the result elements will be a subsetted `DelayedArray`
#' objects as the time and area roles are required for a poparray.
#'
#' @param x A poparray.
#' @param f The dimension to split by: either a single dimension name (character
#'   scalar) or a single integer position.
#' @param drop Logical; whether to drop dimensions in the subset, passed to `[`.
#' @param ... Unused.
#'
#' @return A named list of poparray (or DelayedArray) slices.
#' @export
split.poparray <- function(x, f, drop = FALSE, ...) {
  validate_poparray(x)
  
  dim_names <- names(x)
  nd <- length(dim(x$handle))
  roles <- attr(x, "dimroles", exact = TRUE)
  
  # Resolve split dimension name + position
  if (is.character(f) && length(f) == 1) {
    # role keywords
    if (identical(f, "time")) f <- roles$time
    if (identical(f, "area")) f <- roles$area
    
    if (!f %in% dim_names) {
      cli::cli_abort(c(
        "{.arg f} is not a valid dimension/role for this poparray.",
        "i" = "Use a dimension name, or one of: {.val time}, {.val area}.",
        "i" = "Valid dimensions are: {paste(dim_names, collapse = ', ')}."
      ))
    }
    split_dim <- f
    split_pos <- match(f, dim_names)
    
  } else if (is.numeric(f) && length(f) == 1) {
    split_pos <- as.integer(f)
    if (is.na(split_pos) || split_pos < 1L || split_pos > nd) {
      cli::cli_abort("{.arg f} (position) must be between 1 and {nd}.")
    }
    split_dim <- dim_names[[split_pos]]
    
  } else {
    cli::cli_abort("{.arg f} must be a single dimension name, role ('time'/'area'), or a single position.")
  }
  
  labs <- dimnames(x)[[split_dim]]
  if (length(labs) == 0) return(stats::setNames(list(), character(0)))
  
  out <- lapply(labs, \(lab) {
    # Named indexing, so dimension order doesn't matter
    do.call(`[`, list(x, drop = drop, structure(list(lab), names = split_dim)))
  })
  
  purr::set_names(out, labs)
}


# Operators -------------------------------------------------------------------------------------------------------

#' @export
sum.poparray <- function(x, ..., na.rm = FALSE) {
  a <- x$data
  # sum() on DelayedArray triggers block processing / delayed reduction
  base::sum(a, ..., na.rm = na.rm)
}

#' @export
mean.poparray <- function(x, ..., na.rm = FALSE) {
  a <- x$data
  base::mean(a, ..., na.rm = na.rm)
}

#' @export
sd.poparray <- function(x, ..., na.rm = FALSE) {
  a <- x$data
  # For general DelayedArray, sd() may or may not be specialized;
  # safest is a two-pass block reduction if you need guaranteed behavior.
  # (See next section.)
  stats::sd(as.vector(a), na.rm = na.rm)
}

#' @export
Summary.poparray <- function(..., na.rm = FALSE) {
  args <- list(...)
  # Support min(x) / max(x) where first arg is poparray
  x <- args[[1L]]
  a <- x$data
  
  fun <- .Generic
    do.call(fun, c(list(a), args[-1L], list(na.rm = na.rm)))
  # if (fun %in% c("min", "max", "range")) {
  #   do.call(fun, c(list(a), args[-1L], list(na.rm = na.rm)))
  # } else {
  #   stop(sprintf("Summary(%s) not implemented for poparray.", fun), call. = FALSE)
  # }
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
`data_col<-` <- function(x, values) {
  checkmate::assert_character(values, len = 1, any.missing = FALSE)
  attr(x, "data_col") <- values
  x
}


#' Warn for large realized array
#'
#' Used for the side effect of issuing a warning when a realized array is very large.
#'
#' @param x A poparray.
#' @param bytes_threshold Threshold in bytes for warning (default ~400 MB).
#' @returns x invisibly.
#' @keywords internal
warn_if_realization_large <- function(x, bytes_threshold = 5e7 * 8) {
  validate_poparray(x)
  
  t <- tolower(DelayedArray::type(x$handle))
  
  bytes_per_cell <- switch(
    t,
    "integer" = 4,
    "double"  = 8,
    "numeric" = 8,
    "logical" = 1,
    8 # default conservative fallback
  )
  
  n_cells <- as.numeric(prod(dim(x)))
  if (!is.finite(n_cells)) return(invisible(x))
  
  est_bytes <- n_cells * as.numeric(bytes_per_cell)
  
  if (est_bytes >= bytes_threshold) {
    est_mb <- est_bytes / 1024^2
    cli::cli_warn(c(
      "Coercing this poparray to an in-memory array is {.emph EAGER} and may use substantial memory.",
      "i" = "Cells: {format(n_cells, big.mark = ',')}.",
      "i" = "Backend type: {.val {t}} (~{bytes_per_cell} bytes/cell).",
      "i" = "Estimated realized array size: ~{format(round(est_mb, 1), nsmall = 1)} MB (array only).",
      "i" = "Consider subsetting years/areas (or another dimension) before calling as.data.frame() / as.array()."
    ))
  }
  
  invisible(x)
}

#' Polish data frames after coercing
#'
#'  Used by as.data.frame() as a suport function
#'
#' @param df 
#' @param stringsAsFactors 
#' @param time_dim 
#'
#' @returns a data frame
#' @keywords internal
#' @examples
#' #todo
polish_df <- function(df,
                      stringsAsFactors = TRUE,
                      time_dim = "year") {
  df <- df[stats::complete.cases(df), ] |>
    dplyr::mutate(
      dplyr::across(where(is_char_int),    char_to_int),
      dplyr::across(where(is_char_double), char_to_double)
    )
  
  if (stringsAsFactors && "age.char" %in% names(df)) {
    ages <- df[["age.char"]] |>
      (\(x) if (is.factor(x)) levels(x) else unique(x))()
    
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
  
  # Order time dimension if it's a factor
  if (time_dim %in% names(df) && is.factor(df[[time_dim]])) {
    lev <- levels(df[[time_dim]])
    # if levels are integer-like, sort numerically; else sort lexicographically
    if (all(grepl("^[-+]?[0-9]+$", lev))) {
      lev2 <- sort(as.integer(lev))
      lev2 <- as.character(lev2)
    } else {
      lev2 <- sort(lev)
    }
    df[[time_dim]] <- factor(df[[time_dim]], levels = lev2, ordered = TRUE)
  }
  
  df
}

