# -------------------------------------------------------------------------------------->
# Script: projection_class.r
# Description:
#   Defines the poparray_projection class, constructor, validator, coercion, subsetting, and
#   print methods.  The plot method is defined in projection_plot.r script as it is quite long. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 18, 2026 
# Revised: Februay 10, 2026, renamed class to poparray_projection
# -------------------------------------------------------------------------------------->


# poparray_projection class -------------------------------------------------------------------------------------------

#' Projection class
#'
#' @description
#' An S4 class representing a time-based projection. The class extends
#' `DelayedArray` and stores uncertainty as a named `stat` dimension with levels
#' `projection` and `std_error`. Objects are typically created by [project()].
#' 
#' @section Structure:
#' A `poparray_projection` object is an S4 subclass of `DelayedArray`.
#' The cube includes time/area/strata dimensions plus `stat`.
#'
#' Slots included:
#' *   **time_role** time dimension name.
#' *   **area_role** area dimension name.
#' *   **strata_roles** non-time/non-area/non-stat dimensions.
#' *   **level** confidence level used by the projection.
#' *   **method** projection method (ARIMA/ETS/CAGR).
#' *   **source** provenance metadata list.
#' *   **base_years** base years or time units used for projection/forecast.
#' *   **data_col** value column name for tabular coercion.
#' *   **created** creation timestamp.
#'
#' @seealso
#' * [project()] to create a projection.
#' * [as.poparray.poparray_projection()] to coerce to a `poparray`
#'
#' @name poparray_projection
#' @docType class
#' @keywords internal
NULL

setClass(
  "poparray_projection",
  contains = "DelayedArray",
  slots = c(
    time_role = "character",
    area_role = "character",
    strata_roles = "character",
    level = "numeric",
    method = "character",
    source = "list",
    base_years = "vector",
    data_col = "character",
    created = "POSIXct"
  )
)

setValidity("poparray_projection", function(object) {
  dn <- dimnames(object)
  if (is.null(dn) || is.null(names(dn)) || !"stat" %in% names(dn)) {
    return("poparray_projection must contain named dimnames with a 'stat' dimension.")
  }

  stat_levels <- as.character(dn[["stat"]])
  allowed_levels <- c("projection", "std_error")
  if (!length(stat_levels) || anyNA(stat_levels) || any(!stat_levels %in% allowed_levels)) {
    return("The 'stat' dimension must contain a non-empty subset of: projection, std_error.")
  }

  if (length(object@time_role) != 1L || !nzchar(object@time_role)) {
    return("slot 'time_role' must be a single non-empty character string.")
  }
  if (length(object@area_role) != 1L || !nzchar(object@area_role)) {
    return("slot 'area_role' must be a single non-empty character string.")
  }
  if (identical(object@time_role, object@area_role)) {
    return("time_role and area_role must be different.")
  }
  if (!object@time_role %in% names(dn) || !object@area_role %in% names(dn)) {
    return("time_role and area_role must exist in projection dimnames.")
  }
  if ("stat" %in% c(object@time_role, object@area_role, object@strata_roles)) {
    return("'stat' cannot be assigned as time, area, or strata role.")
  }
  if (anyDuplicated(c(object@time_role, object@area_role, object@strata_roles)) > 0) {
    return("time/area/strata roles cannot contain duplicates.")
  }

  if (length(object@level) != 1L || is.na(object@level) || object@level < 0.5 || object@level > 0.99) {
    return("slot 'level' must be a single numeric value between 0.5 and 0.99.")
  }

  method_choices <- c("ARIMA", "ETS", "CAGR")
  if (length(object@method) != 1L || is.na(object@method) || !(toupper(object@method) %in% method_choices)) {
    return("slot 'method' must be one of: ARIMA, ETS, CAGR.")
  }

  if (length(object@data_col) != 1L || is.na(object@data_col) || !nzchar(object@data_col)) {
    return("slot 'data_col' must be a single non-empty character string.")
  }

  req_source <- c("note", "source", "updated")
  missing_source <- setdiff(req_source, names(object@source))
  if (length(missing_source)) {
    return("slot 'source' must include fields: note, source, updated.")
  }

  msg <- checkmate::check_atomic_vector(object@base_years, min.len = 1, any.missing = FALSE)
  if (!identical(msg, TRUE)) {
    return("slot 'base_years' must be a non-empty atomic vector with no missing values.")
  }

  if (!inherits(object@created, "POSIXt")) {
    return("slot 'created' must be a POSIXct/POSIXlt timestamp.")
  }

  TRUE
})

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

normalize_projection_source <- function(source) {
  if (is.null(source)) {
    return(list(
      note = "Projection from unknown source",
      source = "unknown",
      updated = "Unknown"
    ))
  }
  
  src <- if (is.list(source)) source else as.list(source)
  
  if (is.null(names(src))) {
    cli::cli_abort("{.arg source} must be a named list (or named atomic vector coercible to list).")
  }
  
  if (is.null(src$note) || !length(src$note)) src$note <- "Projection source"
  if (is.null(src$source) || !length(src$source)) src$source <- "unknown"
  if (is.null(src$updated) || !length(src$updated)) src$updated <- "Unknown"
  
  src
}

pp_handle <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) {
    return(methods::as(x, "DelayedArray"))
  }
  if (is.list(x) && !is.null(x$handle) && is(x$handle, "DelayedArray")) {
    return(x$handle)
  }
  cli::cli_abort("Expected a poparray_projection object with a DelayedArray backend.")
}

pp_roles <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) {
    return(list(time = x@time_role, area = x@area_role, strata = x@strata_roles))
  }
  attr(x, "dimroles", exact = TRUE)
}

pp_level <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) return(x@level)
  attr(x, "level", exact = TRUE)
}

pp_method <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) return(x@method)
  attr(x, "method", exact = TRUE)
}

pp_source <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) return(x@source)
  attr(x, "source", exact = TRUE)
}

pp_base_years <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) return(x@base_years)
  attr(x, "base_years", exact = TRUE)
}

pp_data_col <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) return(x@data_col)
  attr(x, "data_col", exact = TRUE)
}

pp_created <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) return(x@created)
  attr(x, "created", exact = TRUE)
}

tp_dimnames <- function(parray) dimnames(parray)
tp_dim <- function(parray) dim(parray)

tp_dimnames_equal <- function(a, b) {
  da <- tp_dimnames(a)
  db <- tp_dimnames(b)
  identical(da, db)
}

tp_time_dim_name <- function(parray) {
  
  dn <- tp_dimnames(parray)
  time_dm <- time_role(parray)
  if (!is.null(names(dn)) && time_dm %in% names(dn)) return(time_dm)
  stop("poparray_projection requires a time role in poparray object", call. = FALSE)
}

# ---- scale guard -------------------------------------------------------------

check_projection_scale <- function(parray,
                                   max_models = getOption("poparray.max_models", 1500L),
                                   ask = getOption("poparray.ask_before_large_projection", TRUE)) {
  
  dn <- tp_dimnames(parray)
  time_nm <- tp_time_dim_name(parray)
  
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

infer_projection_roles <- function(dn_names, dimroles = NULL) {
  if (is.null(dimroles)) {
    non_stat <- setdiff(dn_names, "stat")
    if (!length(non_stat)) cli::cli_abort("Projection dimnames must include at least one non-'stat' dimension.")
    time_nm <- if ("year" %in% non_stat) "year" else non_stat[[1L]]
    area_candidates <- setdiff(non_stat, time_nm)
    area_nm <- if ("area.name" %in% area_candidates) "area.name" else area_candidates[[1L]]
    if (is.na(area_nm) || !length(area_nm)) {
      cli::cli_abort("Projection dimnames must include both time and area dimensions.")
    }
    return(list(time = time_nm, area = area_nm, strata = setdiff(non_stat, c(time_nm, area_nm))))
  }

  if (!is.list(dimroles) || is.null(dimroles$time) || is.null(dimroles$area)) {
    cli::cli_abort("{.arg dimroles} must be a list with {.field time} and {.field area}.")
  }
  if (!dimroles$time %in% dn_names || !dimroles$area %in% dn_names) {
    cli::cli_abort("{.arg dimroles$time} and {.arg dimroles$area} must exist in projection dimension names.")
  }
  if (identical(dimroles$time, dimroles$area)) {
    cli::cli_abort("{.arg dimroles$time} and {.arg dimroles$area} must be different.")
  }

  list(
    time = as.character(dimroles$time)[1L],
    area = as.character(dimroles$area)[1L],
    strata = as.character(dimroles$strata %||% setdiff(dn_names, c(dimroles$time, dimroles$area, "stat")))
  )
}

new_poparray_projection_s4 <- function(
    x,
    level,
    method,
    source,
    base_years,
    time_dim,
    area_dim,
    data_col = "population",
    created = Sys.time()
) {
  checkmate::assert_class(x, "DelayedArray")

  dn <- dimnames(x)
  if (is.null(dn) || is.null(names(dn)) || !"stat" %in% names(dn)) {
    cli::cli_abort("Projection array must contain named dimnames with a 'stat' dimension.")
  }

  stat_levels <- as.character(dn[["stat"]])
  required_levels <- c("projection", "std_error")
  if (!length(stat_levels) || anyNA(stat_levels) || any(!stat_levels %in% required_levels)) {
    cli::cli_abort(
      c(
        "The {.field stat} dimension must contain valid levels.",
        "x" = "Allowed levels: {.val projection}, {.val std_error}."
      )
    )
  }

  lvl <- normalize_level(level)
  mth <- toupper(as.character(method)[1L])
  if (!mth %in% c("ARIMA", "ETS", "CAGR")) {
    cli::cli_abort("{.arg method} must be one of {.val ARIMA}, {.val ETS}, {.val CAGR}.")
  }
  src <- normalize_projection_source(source)
  checkmate::assert_atomic_vector(base_years, min.len = 1, any.missing = FALSE)
  checkmate::assert_string(data_col, min.chars = 1)

  nms <- names(dn)
  if (!time_dim %in% nms || !area_dim %in% nms) {
    cli::cli_abort("{.arg time_dim} and {.arg area_dim} must be present in projection dimnames.")
  }
  if (identical(time_dim, area_dim)) {
    cli::cli_abort("{.arg time_dim} and {.arg area_dim} must be different.")
  }

  new(
    "poparray_projection",
    x,
    time_role = as.character(time_dim),
    area_role = as.character(area_dim),
    strata_roles = setdiff(nms, c(time_dim, area_dim, "stat")),
    level = as.numeric(lvl),
    method = as.character(mth),
    source = src,
    base_years = base_years,
    data_col = as.character(data_col),
    created = as.POSIXct(created)
  )
}

#' @keywords internal
validate_poparray_projection <- function(x) {
  if (isS4(x) && is(x, "poparray_projection")) {
    msg <- methods::validObject(x, test = TRUE)
    if (!isTRUE(msg)) cli::cli_abort(msg, call = rlang::caller_env())
    return(invisible(TRUE))
  }

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
  
  req <- "handle"
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
  
  checkmate::assert_class(x$handle, "DelayedArray")
  
  dn <- dimnames(x$handle)
  if (is.null(dn) || is.null(names(dn)) || !"stat" %in% names(dn)) {
    cli::cli_abort(
      "{.cls poparray_projection} must contain a named {.field stat} dimension in {.field handle}.",
      call = rlang::caller_env()
    )
  }
  stat_levels <- as.character(dn[["stat"]])
  allowed_levels <- c("projection", "std_error")
  if (!length(stat_levels) || anyNA(stat_levels) || any(!stat_levels %in% allowed_levels)) {
    cli::cli_abort(
      "{.field handle} {.field stat} labels must be a non-empty subset of {.val projection, std_error}.",
      call = rlang::caller_env()
    )
  }
  
  roles <- attr(x, "dimroles", exact = TRUE)
  if (is.null(roles) || !is.list(roles) || is.null(roles$time) || is.null(roles$area)) {
    cli::cli_abort(
      "{.cls poparray_projection} must include {.field dimroles} with {.field time} and {.field area}.",
      call = rlang::caller_env()
    )
  }
  if (!roles$time %in% names(dn) || !roles$area %in% names(dn)) {
    cli::cli_abort(
      "{.field dimroles$time} and {.field dimroles$area} must exist in {.field handle} dimension names.",
      call = rlang::caller_env()
    )
  }
  
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
  
  
  source <- attr(x, "source", exact = TRUE)
  if (is.null(source)) {
    cli::cli_abort(
      "{.field source} must be present and use poparray-style named source metadata.",
      call = rlang::caller_env()
    )
  }
  source <- if (is.list(source)) source else as.list(source)
  req_source <- c("note", "source", "updated")
  missing_source <- setdiff(req_source, names(source))
  if (length(missing_source)) {
    cli::cli_abort(
      c(
        "{.field source} is missing required fields.",
        "x" = "Missing: {.val {missing_source |> paste(collapse = ', ')}}."
      ),
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
    handle,
    level,
    method,
    source,
    base_years,
    dimroles = NULL,
    data_col = "population",
    created = Sys.time()
) {
  dn <- dimnames(handle)
  if (is.null(dn) || is.null(names(dn))) {
    cli::cli_abort("Projection array must have named dimnames.")
  }
  roles <- infer_projection_roles(names(dn), dimroles = dimroles)

  new_poparray_projection_s4(
    x = handle,
    level = level,
    method = method,
    source = source,
    base_years = base_years,
    time_dim = roles$time,
    area_dim = roles$area,
    data_col = data_col,
    created = created
  )
}

#' Construct a poparray_projection object
#'
#' @param projection A DelayedArray containing projected population values
#' @param std_error A DelayedArray containing standard errors
#' @param level Confidence level associated with standard errors (e.g., 0.95)
#' @param method Projection method name
#' @param source Poparray-style source metadata (named list or named atomic vector coercible
#'   to list). Expected fields include `note`, `source`, and `updated`.
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
    base_years,
    dimroles = NULL,
    data_col = "population"
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
  
  roles <- infer_projection_roles(names(dimnames(combined)), dimroles = dimroles)

  new_poparray_projection_s4(
    x = combined,
    level = level,
    method = method,
    source = source,
    base_years = base_years,
    time_dim = roles$time,
    area_dim = roles$area,
    data_col = data_col
  )
}



# poparray_projection data access helpers -------------------------------------------------------------------------

#' Extract projected values from a poparray_projection
#'
#' Returns the delayed array slice where the `stat` dimension is
#' `"projection"`.
#'
#' @param x A `poparray_projection` object.
#'
#' @return A `DelayedArray` slice retaining all dimensions, with `stat`
#' restricted to `"projection"` (`drop = FALSE`).
#' @export
projection <- function(x) {
  h <- pp_handle(x)
  dn <- dimnames(h)
  if (is.null(dn) || is.null(names(dn)) || !"stat" %in% names(dn)) {
    cli::abort("Projection data must contain a named 'stat' dimension.")
  }
  if (!"projection" %in% dn[["stat"]]) {
    cli::abort("Projection data does not contain the 'projection' level in the 'stat' dimension.")
  }
  
  stat_k <- match("stat", names(dn))
  idx <- rep(list(TRUE), length(dim(h)))
  idx[[stat_k]] <- "projection"
  
  do.call(`[`, c(list(h), idx, list(drop = FALSE)))
}

#' Extract standard errors from a poparray_projection
#'
#' Returns the delayed array slice where the `stat` dimension is
#' `"std_error"`.
#'
#' @param x A `poparray_projection` object.
#'
#' @return A `DelayedArray` slice retaining all dimensions, with `stat`
#' restricted to `"std_error"` (`drop = FALSE`).
#' @export
std_error <- function(x) {
  h <- pp_handle(x)
  dn <- dimnames(h)
  if (is.null(dn) || is.null(names(dn)) || !"stat" %in% names(dn)) {
    cli::abort("Projection data must contain a named 'stat' dimension.")
  }
  if (!"std_error" %in% dn[["stat"]]) {
    cli::abort("Projection data does not contain the 'std_error' level in the 'stat' dimension.")
  }

  stat_k <- match("stat", names(dn))
  idx <- rep(list(TRUE), length(dim(h)))
  idx[[stat_k]] <- "std_error"

  do.call(`[`, c(list(h), idx, list(drop = FALSE)))
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

setMethod("show", "poparray_projection", function(object) {
  validate_poparray_projection(object)
  src <- pp_source(object)
  src <- if (is.list(src)) src else as.list(src)

  cat("<poparray_projection>\n")
  cat("  method: ", pp_method(object), "\n", sep = "")
  cat("  level:  ", pp_level(object), "\n", sep = "")
  cat("  source note: ", as.character(src$note %||% "Unknown"), "\n", sep = "")
  cat("  source ref:  ", as.character(src$source %||% "Unknown"), "\n", sep = "")
  cat("  source date: ", as.character(src$updated %||% "Unknown"), "\n", sep = "")
  byr <- pp_base_years(object)
  cat(
    "  base years: ",
    paste0(range(byr), collapse = "–"),
    " (n=", length(byr), ")\n",
    sep = ""
  )

  dms <- dimnames(pp_handle(object))
  dms_sizes <- lengths(dms)
  names(dms_sizes) <- names(dms)
  dimensions <- paste(paste0(names(dms_sizes), " (", dms_sizes, ")"), collapse = ", ")
  cat("Dimensions: ", dimensions, "\n", sep = "")
  invisible(object)
})

#' @export
print.poparray_projection <- function(x, ...) {
  .Deprecated(msg = "print.poparray_projection() is deprecated; use show() dispatch for S4 poparray_projection.")
  if (isS4(x) && is(x, "poparray_projection")) return(show(x))
  validate_poparray_projection(x)
  src <- pp_source(x)
  src <- if (is.list(src)) src else as.list(src)
  cat("<poparray_projection>\n")
  cat("  method: ", pp_method(x), "\n", sep = "")
  cat("  level:  ", pp_level(x), "\n", sep = "")
  cat("  source note: ", as.character(src$note %||% "Unknown"), "\n", sep = "")
  cat("  source ref:  ", as.character(src$source %||% "Unknown"), "\n", sep = "")
  cat("  source date: ", as.character(src$updated %||% "Unknown"), "\n", sep = "")
  byr <- pp_base_years(x)
  cat("  base years: ", paste0(range(byr), collapse = "–"), " (n=", length(byr), ")\n", sep = "")
  dms <- dimnames(pp_handle(x))
  dms_sizes <- lengths(dms)
  names(dms_sizes) <- names(dms)
  cat("Dimensions: ", paste(paste0(names(dms_sizes), " (", dms_sizes, ")"), collapse = ", "), "\n", sep = "")
  invisible(x)
}

# ---- subsetting --------------------------------------------------------------

setMethod(
  "[",
  signature(x = "poparray_projection"),
  function(x, ..., drop = FALSE) {
    dn0 <- dimnames(x)
    dnm <- names(dn0)
    nd <- length(dn0)

    dots <- as.list(substitute(list(...)))[-1L]
    idx <- lapply(dots, \(e) {
      if (identical(e, quote(expr = ))) TRUE else eval(e, parent.frame())
    })

    ndx <- rep(list(TRUE), nd)
    idx_names <- names(idx)
    if (is.null(idx_names)) idx_names <- character(0)

    if (length(idx_names) > 0) {
      bad <- setdiff(idx_names, dnm)
      if (length(bad) > 0) {
        cli::cli_abort(c(
          "Unknown dimension name(s) in subset: {paste(bad, collapse = ', ')}.",
          "i" = "Valid dimensions are: {paste(dnm, collapse = ', ')}."
        ))
      }
      ndx[match(idx_names, dnm)] <- idx
    } else {
      if (length(idx) > nd) cli::cli_abort("Too many indices for projection object.")
      ndx[seq_along(idx)] <- idx
    }

    subset_data <- do.call(`[`, c(list(methods::as(x, "DelayedArray")), ndx, list(drop = drop)))
    dn <- dimnames(subset_data)

    if (!is.null(dn) && !is.null(names(dn)) && "stat" %in% names(dn)) {
      return(
        new_poparray_projection_s4(
          x = subset_data,
          level = x@level,
          method = x@method,
          source = x@source,
          base_years = x@base_years,
          time_dim = x@time_role,
          area_dim = x@area_role,
          data_col = x@data_col,
          created = x@created
        )
      )
    }

    if (is.null(dn) || is.null(names(dn)) ||
        !x@time_role %in% names(dn) || !x@area_role %in% names(dn)) {
      return(subset_data)
    }

    new_poparray(
      x = subset_data,
      dimnames_list = dn,
      data_col = x@data_col %||% "population",
      source = x@source,
      time_dim = x@time_role,
      area_dim = x@area_role
    )
  }
)

#' @export
`[.poparray_projection` <- function(x, ..., drop = FALSE) {
  if (isS4(x) && is(x, "poparray_projection")) {
    cli::cli_abort("S3 `[.poparray_projection` should not be used for S4 projection objects.")
  }
  h <- pp_handle(x)
  dn0 <- dimnames(h)
  dnm <- names(dn0)
  nd <- length(dn0)
  
  dots <- as.list(substitute(list(...)))[-1L]
  idx <- lapply(dots, \(e) {
    if (identical(e, quote(expr = ))) TRUE else eval(e, parent.frame())
  })
  
  ndx <- rep(list(TRUE), nd)
  idx_names <- names(idx)
  if (is.null(idx_names)) idx_names <- character(0)
  
  if (length(idx_names) > 0) {
    bad <- setdiff(idx_names, dnm)
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "Unknown dimension name(s) in subset: {paste(bad, collapse = ', ')}.",
        "i" = "Valid dimensions are: {paste(dnm, collapse = ', ')}."
      ))
    }
    ndx[match(idx_names, dnm)] <- idx
  } else {
    if (length(idx) > nd) cli::cli_abort("Too many indices for projection object.")
    ndx[seq_along(idx)] <- idx
  }
  
  subset_data <- do.call(`[`, c(list(h), ndx, list(drop = drop)))
  dn <- dimnames(subset_data)
  
  # If stat dimension still exists, return projection object
  if (!is.null(dn) && !is.null(names(dn)) && "stat" %in% names(dn)) {
     
    return(
      new_poparray_projection(
        handle     = subset_data,
        level      = pp_level(x),
        method     = pp_method(x),
        source     = pp_source(x),
        base_years = pp_base_years(x),
        dimroles   = pp_roles(x),
        data_col   = pp_data_col(x)
      )
    )
  }
  
  roles <- pp_roles(x)
  if (is.null(dn) || is.null(names(dn)) ||
      !roles$time %in% names(dn) || !roles$area %in% names(dn)) {
    return(subset_data)
  }
  
  new_poparray(
    x = subset_data,
    dimnames_list = dn,
    data_col = pp_data_col(x) %||% "population",
    source = pp_source(x),
    time_dim = roles$time,
    area_dim = roles$area
  )
}

# ---- coercion ---------------------------------------------------------------

#' Coerce a poparray_projection to a poparray
#'
#' Returns a `poparray` wrapping the same delayed backend in `handle`,
#' preserving time/area roles and retaining the `stat` dimension.
#'
#' @param x a poparray_projection object
#' @param ...
#'
#' @export
as.poparray.poparray_projection <- function(x, ...) {
  validate_poparray_projection(x)
  roles <- pp_roles(x)
  h <- pp_handle(x)
  dn <- dimnames(h)
  res <- new_poparray(
    x = h,
    dimnames_list = dn,
    data_col = pp_data_col(x) %||% "population",
    source = pp_source(x),
    time_dim = roles$time,
    area_dim = roles$area
  )
  
  attr(res, "projection_level") <- pp_level(x)
  attr(res, "projection_method") <- pp_method(x)
  attr(res, "projection_base_years") <- pp_base_years(x)
  attr(res, "source") <- pp_source(x)
  
  res
}

# ---- tabular coercion --------------------------------------------------------

projection_to_df <- function(x,
                             include_level = TRUE,
                             include_model = TRUE,
                             include_confidence = FALSE,
                             ...) {
  validate_poparray_projection(x)
  h <- pp_handle(x)
  arr <- as.array(h)
  dimnames(arr) <- dimnames(h)
  
  long <- as.data.frame(
    as.table(arr),
    stringsAsFactors = FALSE,
    responseName = "value",
    ...
  )
  
  out <- tidyr::pivot_wider(
    long,
    names_from = stat,
    values_from = value
  )
  
  if (isTRUE(include_confidence)) {
    lvl <- pp_level(x)
    z <- stats::qnorm(1 - (1 - lvl) / 2)
    out$lower <- out$projection - z * out$std_error
    out$upper <- out$projection + z * out$std_error
  }
  
  if (isTRUE(include_level)) {
    out$level <- pp_level(x)
  }
  
  if(isTRUE(include_model)) {
    out$model <- pp_method(x)
  }
  
  # Preserve projection metadata on tabular outputs for downstream provenance use.
  attr(out, "source") <- pp_source(x)
  attr(out, "method") <- pp_method(x)
  attr(out, "level") <- pp_level(x)
  attr(out, "base_years") <- pp_base_years(x)
  attr(out, "created") <- pp_created(x)
  
  out
}



#' Coerce poparray_projection to a data frame 
#' 
#' Transforms the delayed array in `x` to a data frame or tibble. This is an eager
#' realization. The output contains `projection` and `std_error` columns.
#'
#' @param x a poparray_projection object
#' @param ... 
#' @param include_level the default is FALSE. When TRUE means add a column with the confidence level used for the
#'   projection. The confidence level is also available in the attributes, attr(x, "level")
#' @param include_model the default FALSE.  When TRUE causes a column with the model name used for the projection
#' @param include_confidence default FALSE; when TRUE adds `lower` and `upper`
#'   confidence-limit columns computed from `projection`, `std_error`, and the
#'   object's confidence `level`.
#'
#' @returns a data frame for as.data.frame() and tibble for as_tibble()
#' @export
#'
#' @examples
#' # TO DO
as.data.frame.poparray_projection <- function(x,
                                              ...,
                                              include_level = FALSE,
                                              include_model = FALSE,
                                              include_confidence = FALSE) {
  out <- projection_to_df(
    x,
    include_level = include_level,
    include_model = include_model,
    include_confidence = include_confidence,
    ...
  )
  
  # Keep projection metadata, then return a base data.frame (not tibble).
  src <- attr(out, "source", exact = TRUE)
  mth <- attr(out, "method", exact = TRUE)
  lvl <- attr(out, "level", exact = TRUE)
  byr <- attr(out, "base_years", exact = TRUE)
  crt <- attr(out, "created", exact = TRUE)
  
  out <- base::as.data.frame(out, stringsAsFactors = FALSE)
  
  attr(out, "source") <- src
  attr(out, "method") <- mth
  attr(out, "level") <- lvl
  attr(out, "base_years") <- byr
  attr(out, "created") <- crt
  
  out
}

#' @rdname as.data.frame.poparray_projection
#' @export
as_tibble.poparray_projection <- function(x,
                                          ...,
                                          include_level = FALSE,
                                          include_model = FALSE,
                                          include_confidence = FALSE) {
  df <- projection_to_df(
    x,
    include_level = include_level,
    include_model = include_model,
    include_confidence = include_confidence,
    ...
  )
  out <- tibble::as_tibble(df)
  
  #source <- normalize_projection_source(source)
  attr(out, "source") <- attr(df, "source", exact = TRUE)
  attr(out, "method") <- attr(df, "method", exact = TRUE)
  attr(out, "level") <- attr(df, "level", exact = TRUE)
  attr(out, "base_years") <- attr(df, "base_years", exact = TRUE)
  attr(out, "created") <- attr(df, "created", exact = TRUE)
  
  out
}


  
