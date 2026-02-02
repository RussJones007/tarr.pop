# -------------------------------------------------------------------------------------->
# Script: projection.r
# 
# Projection API + engine interface 
#
# This file defines:
#   - project(): user-facing entry point
#   - project_cube(): applies a 1D engine across a tarr_pop cube
#   - run_projection_engine(): dispatcher
#   - engine_*(): per-series forecasting engines (placeholders here)
#
# Requirements:
#   - one confidence band per call (level)
#   - preserve input schema (works for 3D, 5D, 6D, ...)
#   - no silent full realization (only per-series vectors are realized)
#   - interactive guard for large unfiltered cubes
# ------------------------------------------------------------------------------
# NOTE: normalize_level(), check_projection_scale(), new_tarr_projection()
# are defined in projection_classes.R
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 14, 2026
# Revised:  Refactored January 19, 2026
# -------------------------------------------------------------------------------------->

# ---- helpers -----------------------------------------------------------------

is_numeric_year_labels <- function(years_chr) {
  y <- suppressWarnings(as.integer(as.character(years_chr)))
  all(!is.na(y))
}

make_future_year_labels <- function(base_years_chr, h) {
  if (!is_numeric_year_labels(base_years_chr)) {
    stop(
      "Year labels must be coercible to integer to construct future years.",
      call. = FALSE
    )
  }
  base_int <- as.integer(as.character(base_years_chr))
  last_y <- max(base_int)
  as.character(seq.int(last_y + 1L, last_y + as.integer(h)))
}

# assign a length-h vector into an array slice along the year dimension
assign_year_slice <- function(arr, year_k, fixed_k_list, values) {
  nd <- length(dim(arr))
  idx <- vector("list", nd)
  for (k in seq_len(nd)) idx[[k]] <- TRUE
  idx[[year_k]] <- seq_along(values)
  
  if (length(fixed_k_list)) {
    for (nm in names(fixed_k_list)) {
      k <- as.integer(nm)
      idx[[k]] <- fixed_k_list[[nm]]
    }
  }
  
  do.call(`[<-`, c(list(arr), idx, list(value = values)))
}

# Extract a single (base-years) series as a numeric vector for a fixed non-year cell
extract_series <- function(tp, year_k, fixed_k_list) {
  nd <- length(dim(tp))
  idx <- vector("list", nd)
  for (k in seq_len(nd)) idx[[k]] <- TRUE
  idx[[year_k]] <- TRUE
  if (length(fixed_k_list)) {
    for (nm in names(fixed_k_list)) {
      k <- as.integer(nm)
      idx[[k]] <- fixed_k_list[[nm]]
    }
  }
  y <- do.call(`[`, c(list(tp), idx, list(drop = TRUE)))
  as.numeric(y)
}

# Build a tarr_pop from an in-memory array, inheriting basic metadata
# (we keep this small and explicit; you can later swap to HDF5-backed outputs).
tarr_pop_from_array_like <- function(arr, template, dimnames_list) {
  src <- attr(template, "source", exact = TRUE)
  dc  <- attr(template, "data_col", exact = TRUE) %||% "population"
  
  new_tarr_pop(
    x = DelayedArray::DelayedArray(arr),
    dimnames_list = dimnames_list,
    data_col = dc,
    source = src
  )
}

# ---- engine interface ---------------------------------------------------------

#' @keywords internal
run_projection_engine <- function(method, y, years, h, level, ...) {
  method <- toupper(match.arg(toupper(method), c("ARIMA", "ETS", "CAGR")))
  
  res <- switch(
    method,
    ARIMA = engine_arima(y = y, years = years, h = h, level = level, ...),
    ETS   = engine_ets(y = y, years = years, h = h, level = level, ...),
    CAGR  = engine_cagr(y = y, years = years, h = h, level = level, ...),
    stop("Unknown method.", call. = FALSE)
  )
  
  validate_engine_result(res, h = h)
  res
}

validate_engine_result <- function(res, h) {
  req <- c("projected", "lower", "upper", "base_years", "method")
  if (!is.list(res) || !all(req %in% names(res))) {
    stop("Engine result must include: ", paste(req, collapse = ", "), call. = FALSE)
  }
  if (length(res$projected) != h ||
      length(res$lower) != h ||
      length(res$upper) != h) {
    stop("Engine result vectors must all be length `h`.", call. = FALSE)
  }
  if (!is.character(res$method) || length(res$method) != 1L ||
      !res$method %in% c("ARIMA", "ETS", "CAGR")) {
    stop("Engine result `method` must be one of ARIMA/ETS/CAGR.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- engine placeholders (implement later) -----------------------------------

engine_arima <- function(y, years, h, level, ...) {
  years_int <- suppressWarnings(as.integer(as.character(years)))
  if (anyNA(years_int)) {
    stop("`years` must be coercible to integer for ARIMA.", call. = FALSE)
  }
  if (length(y) != length(years_int)) {
    stop("`y` and `years` must have the same length.", call. = FALSE)
  }

  # ARIMA engines do not like NA; interpolate if needed (still 1D).
  if (all(is.na(y))) {
    y <- rep(0, length(y))
  }

  if (any(is.na(y))) {
    y <- as.numeric(forecast::na.interp(stats::ts(y, frequency = 1)))
  }

  # Ensure annual, ordered series.
  ord <- order(years_int)
  years_int <- years_int[ord]
  y <- as.numeric(y[ord])

  # If years are not consecutive, treat as evenly spaced (still a valid ts).
  y_ts <- stats::ts(y, start = min(years_int), frequency = 1)

  fit <- forecast::auto.arima(
    y_ts,
    seasonal = FALSE,
    ...
  )

  fc <- forecast::forecast(fit, h = h, level = level * 100)

  mean_v <- as.numeric(fc$mean)
  lo_v <- as.numeric(fc$lower[, 1L])
  up_v <- as.numeric(fc$upper[, 1L])

  lo_v <- pmax(0, lo_v)
  mean_v <- pmax(0, mean_v)
  up_v <- pmax(0, up_v)

  list(
    projected = mean_v,
    lower = lo_v,
    upper = up_v,
    base_years = as.character(years_int),
    method = "ARIMA"
  )
}

engine_ets <- function(y, years, h, level, ...) {
  years_int <- suppressWarnings(as.integer(as.character(years)))
  if (anyNA(years_int)) {
    stop("`years` must be coercible to integer for ETS.", call. = FALSE)
  }
  if (length(y) != length(years_int)) {
    stop("`y` and `years` must have the same length.", call. = FALSE)
  }

  if (all(is.na(y))) {
    y <- rep(0, length(y))
  }
  if (any(is.na(y))) {
    y <- as.numeric(forecast::na.interp(stats::ts(y, frequency = 1)))
  }

  ord <- order(years_int)
  years_int <- years_int[ord]
  y <- as.numeric(y[ord])
  y_ts <- stats::ts(y, start = min(years_int), frequency = 1)

  fit <- forecast::ets(y_ts, ...)
  fc <- forecast::forecast(fit, h = h, level = level * 100)

  mean_v <- as.numeric(fc$mean)
  lo_v <- as.numeric(fc$lower[, 1L])
  up_v <- as.numeric(fc$upper[, 1L])

  lo_v <- pmax(0, lo_v)
  mean_v <- pmax(0, mean_v)
  up_v <- pmax(0, up_v)

  list(
    projected = mean_v,
    lower = lo_v,
    upper = up_v,
    base_years = as.character(years_int),
    method = "ETS"
  )
}

engine_cagr <- function(y, years, h, level, ...) {
  years_int <- suppressWarnings(as.integer(as.character(years)))
  if (anyNA(years_int)) {
    stop("`years` must be coercible to integer for CAGR.", call. = FALSE)
  }
  if (length(y) != length(years_int)) {
    stop("`y` and `years` must have the same length.", call. = FALSE)
  }

  if (all(is.na(y))) {
    y <- rep(0, length(y))
  }
  if (any(is.na(y))) {
    y <- as.numeric(forecast::na.interp(stats::ts(y, frequency = 1)))
  }

  ord <- order(years_int)
  years_int <- years_int[ord]
  y <- as.numeric(y[ord])

  n <- length(y)
  if (n < 2L) {
    stop("CAGR requires at least 2 base observations.", call. = FALSE)
  }

  alpha <- (1 + level) / 2
  z <- stats::qnorm(alpha)

  # Prefer multiplicative (log) growth if strictly positive; else fall back to
  # additive growth. Both return approximate intervals.
  if (all(y > 0)) {
    r <- diff(log(y))
    mu <- mean(r)
    sig <- stats::sd(r)
    if (is.na(sig)) sig <- 0

    steps <- seq_len(h)
    mean_v <- y[n] * exp(mu * steps)
    se <- sig * sqrt(steps)
    lo_v <- y[n] * exp(mu * steps - z * se)
    up_v <- y[n] * exp(mu * steps + z * se)
  } else {
    d <- diff(y)
    mu <- mean(d)
    sig <- stats::sd(d)
    if (is.na(sig)) sig <- 0

    steps <- seq_len(h)
    mean_v <- y[n] + mu * steps
    se <- sig * sqrt(steps)
    lo_v <- mean_v - z * se
    up_v <- mean_v + z * se
  }

  lo_v <- pmax(0, lo_v)
  mean_v <- pmax(0, mean_v)
  up_v <- pmax(0, up_v)

  list(
    projected = as.numeric(mean_v),
    lower = as.numeric(lo_v),
    upper = as.numeric(up_v),
    base_years = as.character(years_int),
    method = "CAGR"
  )
}

# ---- cube runner --------------------------------------------------------------

#' @keywords internal
project_cube <- function(tp, h, level, method, guard = TRUE, ...) {
  if (!inherits(tp, "tarr_pop")) stop("`tp` must be a tarr_pop.", call. = FALSE)
  
  h <- as.integer(h)
  if (length(h) != 1L || is.na(h) || h < 1L) stop("`h` must be a positive integer.", call. = FALSE)
  
  level <- normalize_level(level)
  
  if (isTRUE(guard)) check_projection_scale(tp)
  
  dn <- dimnames(tp)
  if (is.null(names(dn))) {
    stop("tarr_pop must have named dimensions (including 'year').", call. = FALSE)
  }
  time_nm <- tp_time_dim_name(tp)
  year_k <- match(time_nm, names(dn))
  
  base_years_chr <- as.character(dn[[time_nm]])
  future_years_chr <- make_future_year_labels(base_years_chr, h = h)
  
  # Output dimnames: preserve all dims; replace year labels with future years
  out_dn <- dn
  out_dn[[time_nm]] <- future_years_chr
  
  in_dim <- dim(tp)
  out_dim <- in_dim
  out_dim[[year_k]] <- h
  
  # In-memory arrays for now (explicit). Later you can swap in an HDF5 writer.
  pf_arr <- array(NA_real_, dim = out_dim, dimnames = out_dn)
  lo_arr <- array(NA_real_, dim = out_dim, dimnames = out_dn)
  up_arr <- array(NA_real_, dim = out_dim, dimnames = out_dn)
  
  other_k <- setdiff(seq_along(in_dim), year_k)
  
  # Build a grid of integer indices for other dims
  if (length(other_k)) {
    other_levels <- lapply(other_k, function(k) seq_len(in_dim[[k]]))
    names(other_levels) <- as.character(other_k)
    grid <- expand.grid(other_levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  } else {
    grid <- NULL
  }
  
  base_years_used <- NULL
  
  if (is.null(grid)) {
    # 1D year-only cube
    y <- extract_series(tp, year_k = year_k, fixed_k_list = list())
    res <- run_projection_engine(method = method, y = y, years = base_years_chr, h = h, level = level, ...)
    
    pf_arr <- assign_year_slice(pf_arr, year_k, fixed_k_list = list(), values = res$projected)
    lo_arr <- assign_year_slice(lo_arr, year_k, fixed_k_list = list(), values = res$lower)
    up_arr <- assign_year_slice(up_arr, year_k, fixed_k_list = list(), values = res$upper)
    
    base_years_used <- as.character(res$base_years)
    
  } else {
    for (i in seq_len(nrow(grid))) {
      fixed_k_list <- as.list(grid[i, , drop = FALSE])
      names(fixed_k_list) <- names(grid) # dim positions as character
      
      y <- extract_series(tp, year_k = year_k, fixed_k_list = fixed_k_list)
      
      res <- run_projection_engine(method = method, y = y, years = base_years_chr, h = h, level = level, ...)
      
      pf_arr <- assign_year_slice(pf_arr, year_k, fixed_k_list = fixed_k_list, values = res$projected)
      lo_arr <- assign_year_slice(lo_arr, year_k, fixed_k_list = fixed_k_list, values = res$lower)
      up_arr <- assign_year_slice(up_arr, year_k, fixed_k_list = fixed_k_list, values = res$upper)
      
      if (is.null(base_years_used)) base_years_used <- as.character(res$base_years)
    }
  }
  
  # Wrap arrays into tarr_pop using new_tarr_pop()
  population_forecast_tp <- tarr_pop_from_array_like(pf_arr, template = tp, dimnames_list = out_dn)
  lower_tp              <- tarr_pop_from_array_like(lo_arr, template = tp, dimnames_list = out_dn)
  upper_tp              <- tarr_pop_from_array_like(up_arr, template = tp, dimnames_list = out_dn)
  
  # Build projection provenance source string
  orig_source <- attr(tp, "source", exact = TRUE)
  if (is.null(orig_source) || (is.character(orig_source) && !nzchar(orig_source))) {
    orig_source <- "unknown"
  }
  source <- paste0("Projection from ", orig_source[1])
  new_tarr_projection(
    projected = population_forecast_tp,
    lower = lower_tp,
    upper = upper_tp,
    level = level,
    method = method,
    source = source,
    base_years = base_years_used
  )
}

# ---- method inference --------------------------------------------------------

#' @keywords internal
infer_projection_method <- function(n_base_years) {
  n <- as.integer(n_base_years)
  if (length(n) != 1L || is.na(n)) {
    stop("`n_base_years` must be a single non-missing integer.", call. = FALSE)
  }
  
  if (n < 5L) {
    stop("At least 5 base years are required to project.", call. = FALSE)
  }

  # select engine based on the number of base years available
  if (n <= 7L) return("ETS")
  if (n <= 10L) return("CAGR")
  "ARIMA"
}

#' @keywords internal
infer_projection_method_from_tp <- function(tp, time_dim = NULL) {
  dn <- dimnames(tp)
  if (is.null(names(dn))) {
    stop("tarr_pop must have named dimensions to infer the time axis.", call. = FALSE)
  }
  
  time_nm <- if (is.null(time_dim)) tp_time_dim_name(tp) else time_dim
  if (!time_nm %in% names(dn)) {
    stop("`time_dim` must match a dimension name in `tp`.", call. = FALSE)
  }
  
  n_base_years <- length(dn[[time_nm]])
  infer_projection_method(n_base_years)
}


# ---- user-facing API ----------------------------------------------------------

#' Project a population cube forward in time
#'
#' Fits independent time-series models for each non-time cell of a `tarr_pop`
#' cube and forecasts population counts for `h` future years. Each unique
#' combination of non-time dimensions (e.g., county × sex × age × race ×
#' ethnicity) is modeled separately.
#'
#' The projection method may be `"auto"`, `"ETS"`, `"CAGR"`, or `"ARIMA"`.
#' For most use cases, `"auto"` is recommended and will select the appropriate
#' engine based on the number of base years available in the input cube.
#'
#' The returned `tarr_projection` object contains three `tarr_pop` cubes:
#' `projected`, `lower`, and `upper`, along with projection metadata.
#'
#' ## Engine methods
#'
#' * **ETS** — *Exponential Smoothing State-Space models* (Error–Trend–Seasonal).
#'   Suitable for short time series (5–7 years) where flexible trend estimation
#'   is needed and over-parameterization should be avoided.
#'
#' * **CAGR** — *Compound Annual Growth Rate*. Assumes a constant annual growth
#'   rate estimated from the base years and projects forward deterministically.
#'   Used for moderate-length series (8–10 years) where stable growth is
#'   preferred over stochastic modeling.
#'
#' * **ARIMA** — *Autoregressive Integrated Moving Average* models.
#'   Used for longer time series (>10 years) where temporal autocorrelation
#'   and differencing can be reliably estimated.
#'
#' ## Projection metadata
#'
#' The following metadata are stored as attributes on the returned
#' `tarr_projection` object:
#'
#' * **method** — The projection engine used (`"ETS"`, `"CAGR"`, or `"ARIMA"`).
#' * **level** — The confidence level used when computing the `upper` and
#'   `lower` bounds.
#' * **source** — A character string describing the projection origin, including
#'   source information from the originating `tarr_pop`.
#' * **base_years** — The number of historical years used to fit the model.
#' * **created** — Timestamp indicating when the projection was generated.
#'
#' @param tp A `tarr_pop` object (filtered or unfiltered) containing a time
#'   dimension.
#' @param h Integer forecast horizon (number of future years).
#' @param level Confidence level for interval estimation (default `0.95`).
#'   Values such as `0.95` or `95` are both accepted.
#' @param method The projection method: `"auto"`, `"ETS"`, `"CAGR"`, or `"ARIMA"`.
#'   If `"auto"`, the method is inferred from the number of base years in `"tp"`:
#'
#'   * 5-7 years uses `"ETS"`
#'   * 8-10 years uses  `"CAGR"`
#'   * \>10 years uses `"ARIMA"`
#'   
#'   Fewer than 5 base years results in an error.
#' @param time_dim Name of the time dimension used for inference. Defaults to
#'   `"year"`, which is common in `tarr_pop` objects.
#' @param guard Logical; if `TRUE`, prompts in interactive sessions when the
#'   implied number of independent models is large.
#' @param ... Additional arguments passed to the selected engine implementation.
#'
#' @return A `tarr_projection` object.
#' @examples
#' # A tiny synthetic 3D cube: year × area.name × sex
#' dn <- list(
#'   year = as.character(2015:2021),     # 7 base years => "auto" selects ETS
#'   area.name = "Tarrant",
#'   sex = "female"
#' )
#' arr <- array(seq_along(dn$year), dim = c(7, 1, 1), dimnames = dn)
#'
#' tp <- new_tarr_pop(
#'   x = DelayedArray::DelayedArray(arr),
#'   dimnames_list = dn,
#'   data_col = "population",
#'   source = "example"
#' )
#'
#' \dontrun{
#'   pr <- project(tp, h = 3, method = "auto", level = 0.95, guard = FALSE)
#'   pr
#'   as.tarr_pop(pr, which = "projected")
#' }
#' 
#' @seealso
#' * [tarr_projection] for the returned object structure.
#' * [as.tarr_pop.tarr_projection()] to extract one of the three cubes.
#' * [as.data.frame.tarr_projection()] and [as_tibble.tarr_projection()] for tabular output.
#' * [plot.tarr_projection()] for visualization (added in the next step).
#' 
#' @export
project <- function(tp,
                    h,
                    level = 0.95,
                    method = c("auto", "ARIMA", "ETS", "CAGR"),
                    time_dim = NULL,
                    guard = TRUE,
                    ...) {
  
  method <- match.arg(method)
  
  if (identical(method, "auto")) {
    method <- infer_projection_method_from_tp(tp, time_dim = time_dim)
  }
  
  project_cube(tp = tp, h = h, level = level, method = method, guard = guard, ...)
}
