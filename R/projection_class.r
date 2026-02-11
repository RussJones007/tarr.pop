# -------------------------------------------------------------------------------------->
# Script: projection_class.r
# Description:
#   Defines the pop_projection class, constructor, validator, corcison, subsetting, and
#   print methods.  The plot method is defined in projection_plot.r script as it is quite long. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 18, 2026 
# Revised:
# -------------------------------------------------------------------------------------->


# pop_projection class -------------------------------------------------------------------------------------------

#' Projection object
#'
#' @description
#' An S3 class representing a time-based projection.
#'
#' Objects of this class are typically created by [project()].  It contains the predicted population for each cell
#' in the original matrix . 
#'
#' @section Structure:
#' A `pop_projection` object is a list with three poparray cubes:
#'
#' * lower: is the lower limits of the calculated confidence interval.
#' * projected: The projected population figures. 
#' * upper: is the upper limit of the calculated confidenceinterval.
#' 
#' Attributes included:
#' *   **level** is the confidence level used
#' *   **methods_used** are the unique methods actually used across all cells
#' *   **n_fallback** are the number of cells that required fallback as the forecasting method .
#' *   **source** is the  "projected from" plus the original poparray source
#' *   **base_years** are the base years used to project/forecast
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
  # if (length(level) != 1L || is.na(level)) {
  #   stop("`level` must be a single non-missing number.", call. = FALSE)
  # }
  # if (!is.numeric(level)) {
  #   stop("`level` must be numeric.", call. = FALSE)
  # }
  
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
  if (!is.null(names(dn)) && "year" %in% names(dn)) return("year")
  stop("pop_projection requires a `year` dimension.", call. = FALSE)
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
validate_pop_projection <- function(x) {
  # ---- basic structure ----
  if (!inherits(x, "pop_projection")) {
    cli::cli_abort(
      "{.arg x} must inherit from {.cls pop_projection}.",
      call = rlang::caller_env()
    )
  }
  
  if (!is.list(x)) {
    cli::cli_abort(
      "{.cls pop_projection} must be a list.",
      call = rlang::caller_env()
    )
  }
  
  req <- c("projected", "lower", "upper")
  missing_req <- setdiff(req, names(x))
  if (length(missing_req)) {
    cli::cli_abort(
      c(
        "{.cls pop_projection} is missing required components.",
        "x" = "Missing: {.val {missing_req |> paste(collapse = ', ')}}."
      ),
      call = rlang::caller_env()
    )
  }
  
  # ---- cube type checks ----
  bad_tp <- c(
    projected = !is.poparray(x$projected),
    lower = !is.poparray(x$lower),
    upper = !is.poparray(x$upper)
  )
  if (any(bad_tp)) {
    cli::cli_abort(
      c(
        "All components of a {.cls pop_projection} must be {.cls poparray} objects.",
        "x" = "Invalid: {.val {names(bad_tp)[bad_tp] |> paste(collapse = ', ')}}."
      ),
      call = rlang::caller_env()
    )
  }
  
  # ---- dimension consistency ----
  if (!identical(tp_dim(x$projected), tp_dim(x$lower)) ||
      !identical(tp_dim(x$projected), tp_dim(x$upper))) {
    cli::cli_abort(
      "{.cls pop_projection} cubes must have identical dimensions.",
      call = rlang::caller_env()
    )
  }
  
  if (!tp_dimnames_equal(x$projected, x$lower) ||
      !tp_dimnames_equal(x$projected, x$upper)) {
    cli::cli_abort(
      "{.cls pop_projection} cubes must have identical dimnames.",
      call = rlang::caller_env()
    )
  }
  
  # Confirm required time dimension exists (currently hard-coded to `year`)
  time_nm <- tp_time_dim_name(x$projected)
  years_avail <- tp_dimnames(x$projected)[[time_nm]]
  years_chr <- as.character(years_avail)
  
  # ---- attributes ----
  level <- attr(x, "level", exact = TRUE)
  if (is.null(level)) {
    cli::cli_abort(
      "{.cls pop_projection} is missing attribute {.field level}.",
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
      "{.cls pop_projection} is missing attribute {.field method}.",
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
      "{.cls pop_projection} is missing attribute {.field base_years}.",
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


new_pop_projection <- function(projected,
                                lower,
                                upper,
                                level = 0.95,
                                method,
                                source,
                                base_years) {
  level <- normalize_level(level)
  
  # ---- Type / class-ish checks (domain-specific) ----
  bad_pop <- c(
    projected = !is.poparray(projected),
    lower = !is.poparray(lower),
    upper = !is.poparray(upper)
  )
  
  if (any(bad_pop)) {
    cli::cli_abort(
      c(
        "{.arg projected}, {.arg lower}, and {.arg upper} must all be {.cls poparray} objects.",
        "x" = "Invalid inputs: {names(bad_pop)[bad_pop] |> paste(collapse = ', ')}."
      ),
      call = rlang::caller_env()
    )
  }
  
  # ---- Dimension checks ----
  if (!identical(tp_dim(projected), tp_dim(lower)) ||
      !identical(tp_dim(projected), tp_dim(upper))) {
    cli::cli_abort(
      "{.arg projected}, {.arg lower}, and {.arg upper} must have identical dimensions.",
      call = rlang::caller_env()
    )
  }
  
  if (!tp_dimnames_equal(projected, lower) ||
      !tp_dimnames_equal(projected, upper)) {
    cli::cli_abort(
      "{.arg projected}, {.arg lower}, and {.arg upper} must have identical dimnames.",
      call = rlang::caller_env()
    )
  }
  
  # ---- method ----
  method <- toupper(as.character(method))
  method_choices <- c("ARIMA", "ETS", "CAGR")
  
  if (!method %in% method_choices) {
    cli::cli_abort(
      c(
        "{.arg method} must be one of: {method_choices |> paste(collapse = ', ')}.",
        "i" = "You supplied: {.val {method}}."
      ),
      call = rlang::caller_env()
    )
  }
  
  # ---- base_years ----
  # checkmate handles the "missing or empty" case neatly if we test missing first.
  if (missing(base_years)) {
    cli::cli_abort("{.arg base_years} must be supplied.", call = rlang::caller_env())
  }
  
  msg <- checkmate::check_atomic_vector(base_years, min.len = 1, any.missing = FALSE)
  if (!identical(msg, TRUE)) {
    cli::cli_abort(
      c("{.arg base_years} must be a non-empty vector.", "x" = msg),
      call = rlang::caller_env()
    )
  }
  
  time_nm <- tp_time_dim_name(projected)
  years_avail <- tp_dimnames(projected)[[time_nm]]
  
  base_chr <- as.character(base_years)
  years_chr <- as.character(years_avail)
  
  
  
  # if (!all(base_chr %in% years_chr)) {
  #   missing_years <- setdiff(unique(base_chr), unique(years_chr))
  #   cli::cli_abort(
  #     c(
  #       "{.arg base_years} must be contained in the cube's year labels.",
  #       "x" = "These years are not present: {missing_years |> paste(collapse = ', ')}."
  #     ),
  #     call = rlang::caller_env()
  #   )
  # }
  # 
  # ---- Construct object ----
  x <- list(
    projected = projected,
    lower = lower,
    upper = upper
  )
  class(x) <- "pop_projection"
  
  attr(x, "level") <- level
  attr(x, "method") <- method
  attr(x, "source") <- as.character(source)
  attr(x, "base_years") <- base_chr
  attr(x, "created") <- Sys.time()
  
  validate_pop_projection(x)
  x
}

# ---- print ------------------------------------------------------------------

#' @export
print.pop_projection <- function(x, ...) {
  validate_pop_projection(x)
  
  cat("<pop_projection>\n")
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
`[.pop_projection` <- function(x, ..., drop = FALSE) {
  validate_pop_projection(x)
  
  pf <- x$projected[..., drop = drop]
  lo <- x$lower[..., drop = drop]
  up <- x$upper[..., drop = drop]
  
  if (!is.poparray(pf)) {
    return(list(projected = pf, lower = lo, upper = up))
  }
  
  y <- list(
    projected = pf,
    lower = lo,
    upper = up
  )
  class(y) <- "pop_projection"
  
  for (nm in c("level", "method", "source", "base_years", "created")) {
    attr(y, nm) <- attr(x, nm, exact = TRUE)
  }
  
  y
}

# ---- coercion ---------------------------------------------------------------

#' Coerce a pop_projection to a poparray
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
                                        which = c("projected", "lower", "upper"),
                                        ...) {
  validate_pop_projection(x)
  which <- match.arg(which)
  
  tp <- x[[which]]
  
  attr(tp, "projection_level") <- attr(x, "level")
  attr(tp, "projection_method") <- attr(x, "method")
  attr(tp, "projection_base_years") <- attr(x, "base_years")
  attr(tp, "source") <- attr(x, "source")
  
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

