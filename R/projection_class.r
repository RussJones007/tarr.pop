# -------------------------------------------------------------------------------------->
# Script: projection_class.r
# Description:
#   Defines the pop_projection class, constructor, validator, corcison, subsetting, and
#   print methods.  The plot method is defined in projection_plot.r script as it is quite long. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 18, 2026 
# Revised: Februay 10, 2026, renamed class to poarray_projection
# -------------------------------------------------------------------------------------->


# pop_projection class -------------------------------------------------------------------------------------------

#' Projection class
#'
#' @description
#' An S3 class representing a time-based projection. The class is a wrapper around an HDF5Array that contains the
#' projected population and a dimension of the error. Objects of this class are typically created by [project()].  
#' 
#' @section Structure:
#' A `pop_projection` object is a list with a HDF5Array cube as a member. The cube is very similar to a poparray but
#' wheres athe poparray figures are for population, these are projected population figures and another set of figures
#' are added that represents the standard error and labeled as "stderr".
#'
#' Attributes included:
#' *   **level** is the confidence level used
#' *   **methods_used** are the unique time series forecasting methods used across all cells
#' *   **n_fallback** are the number of cells that required fallback as the forecasting method .
#' *   **source** is the  "projected from" plus the original poparray source
#' *   **base_years** are the base years or time unit used to project/forecast
#' *   **created** is the date and time stamp at time the object was created. 
#'
#' @seealso
#' * [project()] to create a projection.
#' * [as.poparray.pop_projection()] to select one of the poparray objects
#'
#' @name pop_projection
#' @docType class
#' @keywords internal
NULL

# ---- small utilities ---------------------------------------------------------

normalize_level <- function(level) {
  assert(
    check_double(level, lower = 0.5, upper = 0.99, any.missing = FALSE, len = 1),
    check_double(level, lower = 50, upper =  99, any.missing = FALSE,   len = 1)
  )
  
  if (level > 0 && level < 1) return(as.numeric(level))
  if (level > 1 && level < 100) return(as.numeric(level) / 100)
  stop(
    "`level` must be in (0.5,1) (e.g., 0.95) or (1,100) (e.g., 95).",
    call. = FALSE
  )
}

tp_dimnames <- function(tp) dimnames(tp)
tp_dim <- function(tp) dim(tp)

tp_dimnames_equal <- function(a, b) {
  da <- tp_dimnames(a)
  db <- tp_dimnames(b)
  identical(da, db)
}

tp_time_dim_name <- function(tp) {
  
  dn <- tp_dimnames(tp)
  time_dm <- time_role(tp)
  if (!is.null(names(dn)) && time_dm %in% names(dn)) return(time_dm)
  stop("poparray_projection requires a time role in poparray object", call. = FALSE)
}

# ---- scale guard -------------------------------------------------------------

check_projection_scale <- function(tp,
                                   max_models = getOption("poparray.max_models", 1500L),
                                   ask = getOption("poparray.ask_before_large_projection", TRUE)) {
  
  dn <- tp_dimnames(tp)
  time_nm <- tp_time_dim_name(tp)
  
  if (is.null(names(dn))) return(invisible(TRUE))
  
  other_dims <- setdiff(names(dn), time_nm)
  if (!length(other_dims)) return(invisible(TRUE))
  
  n_models <- prod(vapply(dn[other_dims], length, integer(1)))
  
  if (n_models <= max_models) return(invisible(TRUE))
  
  msg <- paste0(
    "This projection will fit approximately ",
    format(n_models, big.mark = ","),
    " independent time series.\n",
    "Consider filtering the cube before projecting."
  )
  
  if (interactive() && isTRUE(ask)) {
    ok <- utils::askYesNo(paste0(msg, "\nProceed?"))
    if (isTRUE(ok)) return(invisible(TRUE))
    stop("Projection aborted by user.", call. = FALSE)
  }
  
  stop(
    paste0(
      msg,
      "\nRefusing to proceed without explicit confirmation."
    ),
    call. = FALSE
  )
}

# ---- constructor + validator -------------------------------------------------

#' @keywords internal
validate_poparray_projection <- function(x) {
  # ---- basic structure ----
  if (!inherits(x, "poparray_projection")) {
    cli::cli_abort(
      "{.arg x} must inherit from {.cls poparray_projection}.",
      call = rlang::caller_env()
    )
  }
  
  if (!is.list(x)) {
    cli::cli_abort(
      "{.cls poparray_projection} must be a list.",
      call = rlang::caller_env()
    )
  }
  
  req <- "stat"
  missing_req <- setdiff(req, names(x))
  if (length(missing_req)) {
    cli::cli_abort(
      c(
        "{.cls poparray_projection} is missing required component.",
        "x" = "Missing: {.val {missing_req |> paste(collapse = ', ')}}."
      ),
      call = rlang::caller_env()
    )
  }
  
  # Confirm required time dimension exists
  time_nm <- tp_time_dim_name(x$projected)
  years_avail <- tp_dimnames(x$projected)[[time_nm]]
  years_chr <- as.character(years_avail)
  
  # ---- attributes ----
  level <- attr(x, "level", exact = TRUE)
  if (is.null(level)) {
    cli::cli_abort(
      "{.cls poparray_projection} is missing attribute {.field level}.",
      call = rlang::caller_env()
    )
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level < 0.5 || level > 0.99) {
    cli::cli_abort(
      "{.field level} must be a single numeric value between 0.5 and 0.99.",
      call = rlang::caller_env()
    )
  }
  
  method <- attr(x, "method", exact = TRUE)
  if (is.null(method)) {
    cli::cli_abort(
      "{.cls poparray_projection} is missing attribute {.field method}.",
      call = rlang::caller_env()
    )
  }
  method <- toupper(as.character(method))
  method_choices <- c("ARIMA", "ETS", "CAGR")
  if (!length(method) || length(method) != 1L || is.na(method) || !method %in% method_choices) {
    cli::cli_abort(
      c(
        "{.field method} must be one of: {.val {method_choices |> paste(collapse = ', ')}}.",
        "i" = "You supplied: {.val {method}}."
      ),
      call = rlang::caller_env()
    )
  }
  
  
  source <- get_source(x)[1]
  #if(length(source > 1)) attr(x, "source") <- source[1]
  
  if (is.null(source) || length(source) != 1L || is.na(source) || !is.character(source)) {
    cli::cli_abort(
      "{.field source} must be a length-1 character value.",
      call = rlang::caller_env()
    )
  }
  
  base_years <- attr(x, "base_years", exact = TRUE)
  if (is.null(base_years)) {
    cli::cli_abort(
      "{.cls poparray_projection} is missing attribute {.field base_years}.",
      call = rlang::caller_env()
    )
  }
  
  msg <- checkmate::check_atomic_vector(base_years, min.len = 1, any.missing = FALSE)
  if (!identical(msg, TRUE)) {
    cli::cli_abort(
      c("{.field base_years} must be a non-empty vector.", "x" = msg),
      call = rlang::caller_env()
    )
  }
  # base_chr <- as.character(base_years)
  # if (!all(base_chr %in% years_chr)) {
  #   missing_years <- setdiff(unique(base_chr), unique(years_chr))
  #   cli::cli_abort(
  #     c(
  #       "{.field base_years} must be contained in the cube's year labels.",
  #       "x" = "These years are not present: {.val {missing_years |> paste(collapse = ', ')}}."
  #     ),
  #     call = rlang::caller_env()
  #   )
  # }
  
  created <- attr(x, "created", exact = TRUE)
  if (is.null(created) || !inherits(created, "POSIXt")) {
    cli::cli_abort(
      "{.field created} must be a POSIXct/POSIXlt timestamp.",
      call = rlang::caller_env()
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
new_poparray_projection <- function(
    data,
    level,
    method,
    source,
    base_years
) {
  
  checkmate::assert_class(data, "DelayedArray")
  
  dn <- dimnames(data)
  
  if (is.null(dn) || !"stat" %in% names(dn)) {
    cli::cli_abort("Projection array must contain a 'stat' dimension.")
  }
  
  stat_levels <- dn[["stat"]]
  
  required_levels <- c("projection", "std_error")
  
  if (!identical(stat_levels, required_levels)) {
    cli::cli_abort(
      c(
        "The 'stat' dimension must be exactly:",
        "x projection",
        "x std_error"
      )
    )
  }
  
  structure(
    list(data = data),
    level      = level,
    method     = method,
    source     = source,
    base_years = base_years,
    class = "poparray_projection"
  )
}

#' Construct a poparray_projection object
#'
#' @param projection A DelayedArray containing projected population values
#' @param std_error A DelayedArray containing standard errors
#' @param level Confidence level associated with standard errors (e.g., 0.95)
#' @param method Projection method name
#' @param source Data source description
#' @param base_years Numeric vector of base years used in projection
#'
#' @return A poparray_projection object
#' @export
poparray_projection <- function(
    projection,
    std_error,
    level,
    method,
    source,
    base_years
) {
  
  checkmate::assert_class(projection, "DelayedArray")
  checkmate::assert_class(std_error, "DelayedArray")
  
  if (!identical(dim(projection), dim(std_error))) {
    cli::cli_abort("projection and std_error must have identical dimensions.")
  }
  
  if (!identical(dimnames(projection), dimnames(std_error))) {
    cli::cli_abort("projection and std_error must have identical dimnames.")
  }
  
  # Ensure DelayedArray::abind exists
  if (!exists("abind", where = asNamespace("DelayedArray"))) {
    cli::cli_abort("DelayedArray::abind() is not available in this version.")
  }
  
  combined <- DelayedArray::abind(
    projection,
    std_error,
    along = length(dim(projection)) + 1
  )
  
  dimnames(combined) <- c(
    dimnames(projection),
    list(stat = c("projection", "std_error"))
  )
  
  browser()
  new_poparray_projection(
    data       = combined,
    level      = level,
    method     = method,
    source     = source,
    base_years = base_years
  )
}



# poparray_projection data access helpers -------------------------------------------------------------------------

#' @export
projection <- function(x) {
  x$data[,,,,,, "projection", drop = FALSE]
}

#' @export
std_error <- function(x) {
  x$data[,,,,,, "std_error", drop = FALSE]
}

#' @export
confint.poparray_projection <- function(x, level = 0.95, ...) {
  
  z <- stats::qnorm(1 - (1 - level)/2)
  
  proj <- projection(x)
  se   <- std_error(x)
  
  lower <- proj - z * se
  upper <- proj + z * se
  
  list(lower = lower, upper = upper)
}


# print ------------------------------------------------------------------

#' @export
print.poparray_projection <- function(x, ...) {
  validate_poparray_projection(x)
  
  cat("<poparray_projection>\n")
  cat("  method: ", attr(x, "method"), "\n", sep = "")
  cat("  level:  ", attr(x, "level"), "\n", sep = "")
  cat("  source: ", attr(x, "source"), "\n", sep = "")
  cat(
    "  base years: ",
    paste0(range(attr(x, "base_years")), collapse = "–"),
    " (n=", length(attr(x, "base_years")), ")\n",
    sep = ""
  )
  
  dn <- tp_dimnames(x$projected)
  dims <- tp_dim(x$projected)
  
  if (!is.null(names(dn))) {
    cat("  dims:\n")
    for (i in seq_along(dims)) {
      cat("    - ", names(dn)[i], ": ", dims[i], "\n", sep = "")
    }
  }
  
  invisible(x)
}

# ---- subsetting --------------------------------------------------------------

#' @export
`[.poparray_projection` <- function(x, ..., drop = FALSE) {
  
  
  # Subset underlying DelayedArray
  subset_data <- x$data[..., drop = drop]
  
  dn <- dimnames(subset_data)
  
  # If stat dimension still exists, return projection object
  if (!is.null(dn) && "stat" %in% names(dn)) {
    browser()
    return(
      new_poparray_projection(
        data       = subset_data,
        level      = attr(x, "level"),
        method     = attr(x, "method"),
        source     = attr(x, "source"),
        base_years = attr(x, "base_years")
      )
    )
  }
  
  # Otherwise stat dimension was removed → return poparray
  # assuming poparray constructor exists
  poparray(subset_data)
}

# ---- coercion ---------------------------------------------------------------

#' Coerce a poparray_projection to a poparray
#' 
#' at pop_projection consists of three poparray cubes, this coercion returns a single poparray object
#' based on the which argument. 
#'
#' @param x a pop_projection object
#' @param which whn x is pop_projection object, you choose which cube to make into a single poparray: 
#'     *  projected
#'     *  lower
#'     *  upper
#' @param ...
#'
#' @export
as.poparray.pop_projection <- function(x,
                                        which = c("projection", "std_error"),
                                        ...) {
  validate_poparray_projection(x)
  pa <- x$handle
  ndx_name      <- match.arg(which)
  nms <- names(dimnames(pa))
  names_length <- length(nms)
  stat_position <- which(nms == "stat")
  
  indexed_list   <- vector("list", length = names_length)
  indexed_list[] <- TRUE
  indexed_list[stat_position] <- ndx_name
  
  res <- x[indexed_list, drop = TRUE]
  
  # index of stat dimension
  new_poparray(res, dimnames_list = dimnames(res))
  
  attr(res, "projection_level") <- attr(x, "level")
  attr(res, "projection_method") <- attr(x, "method")
  attr(res, "projection_base_years") <- attr(x, "base_years")
  attr(res, "source") <- attr(x, "source")
  
  tp
}

# ---- tabular coercion --------------------------------------------------------

projection_to_df <- function(x,
                             include_level = TRUE,
                             include_model = TRUE,
                             ...) {
  
  pf <- as.data.frame(x$projected, ...)
  lo <- as.data.frame(x$lower, ...)
  up <- as.data.frame(x$upper, ...)
  
  val_pf <- names(pf)[ncol(pf)]
  val_lo <- names(lo)[ncol(lo)]
  val_up <- names(up)[ncol(up)]
  
  dims <- names(pf)[-ncol(pf)]
  
  if (!identical(pf[dims], lo[dims]) || !identical(pf[dims], up[dims])) {
    stop("Dimension rows are not aligned across forecast cubes.", call. = FALSE)
  }
  
  out <- pf
  names(out)[names(out) == val_pf] <- "projected"
  out$lower <- lo[[val_lo]]
  out$upper <- up[[val_up]]
  
  if (isTRUE(include_level)) {
    out$level <- attr(x, "level")
  }
  
  if(isTRUE(include_model)) {
    out$model <- attr(x, "method")
  }
  
  out
}



#' Corerce pop_projection to a data frame 
#' 
#' Transforms the  poparray in x to a data frame or tibble.  Causes a realization of all data in the lower, upper,
#'and  population_project cubes. 
#'
#' @param x a pop_projection object
#' @param ... 
#' @param include_level the default TRUE means add a column with the confidence level used for the projection
#' @param include_model the default TRUE causes a column has the model used fo the projection
#'
#' @returns a data frame for as.data.frame() and tibble for as_tibble()
#' @export
#'
#' @examples
#' # TO DO
as.data.frame.pop_projection <- function(x, ..., include_level = TRUE, include_model = TRUE) {
  projection_to_df(x, include_level = include_level, ...)
}

#' @rdname as.data.frame.pop_projection
#' @export
as_tibble.pop_projection <- function(x, ..., include_level = TRUE, include_model = TRUE) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required for as_tibble().", call. = FALSE)
  }
  tibble::as_tibble(
    projection_to_df(x, include_level = include_level, ...)
  )
}

