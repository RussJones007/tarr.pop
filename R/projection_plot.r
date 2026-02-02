# -------------------------------------------------------------------------------------->
# Script: projection_plot.r
# Description:
#   the plot() funcion for class tarr_projection 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones with assistance from 
# AI:  chatGPT 5.2 tarr_pop project
# Created: January 19, 2026
# Revised:
# -------------------------------------------------------------------------------------->

# ---- plotting ---------------------------------------------------------------

#' Plot a population projection
#'
#' Visualizes a `tarr_projection` object either as a time series of total
#' population with uncertainty bounds or as a grid of population pyramids.
#'
#' ## Plot types
#'
#' * **`"ts"` (default)** — Plots the total projected population for each
#'   time period as a line, aggregated across all non-time dimensions.
#'   Uncertainty is shown as a shaded ribbon between the `lower` and `upper`
#'   projection bounds. This plot always uses all three projection cubes and
#'   ignores area-specific arguments.
#'
#' * **`"pyramid"`** — Displays population pyramids based on the
#'   `projected` cube only. Pyramids are arranged in a grid with
#'   **years in rows** and **areas in columns**.
#'
#' ## Area and year selection for population pyramids
#'
#' For `type = "pyramid"`:
#'
#' * Up to `max_years` (default 5) of the **most recent years** are shown.
#'   This reflects the assumption that forecast accuracy decreases as
#'   projections extend further from the final base year.
#'
#' * Up to `max_areas` (default 3, maximum 7) areas are shown.
#'   If `areas` is `NULL`, the first areas appearing in the cube are used.
#'
#' * If `areas` is supplied, it must be a character vector of area names
#'   present in the cube; otherwise an error is thrown.
#'
#' * If the total number of panels (years × areas) exceeds 16, a warning is
#'   issued because interpretation may become difficult.
#'
#' Population pyramid plots require the presence of `sex` and `age.char`
#' dimensions in the underlying cube.
#'
#' ## Additional graphical parameters
#'
#' Additional arguments supplied via `...` are forwarded to the underlying
#' plotting functions.
#'
#' For `type = "ts"`, the following optional parameters are recognized:
#'
#' * `ribbon_col` — Fill color for the uncertainty ribbon
#'   (default: semi-transparent gray).
#' * `ribbon_border` — Border color for the ribbon (default: `NA`).
#'
#' Other graphical parameters (e.g., `lwd`, `col`, `cex`) are passed to the
#' base plotting functions.
#'
#' @param x A `tarr_projection` object.
#' @param type Plot type: `"ts"` for a time-series plot of total population
#'   or `"pyramid"` for a grid of population pyramids.
#' @param areas Optional character vector of area names to plot when
#'   `type = "pyramid"`. If `NULL`, the first `max_areas` areas are used.
#' @param max_areas Maximum number of areas to display when `areas` is `NULL`.
#'   Defaults to 3 and may not exceed 7.
#' @param max_years Maximum number of years to display for population pyramids.
#'   Defaults to 5 and may not exceed 5. The most recent years are used.
#' @param ... Additional graphical parameters passed to the plotting functions.
#'
#' @return Invisibly returns the input `tarr_projection` object.
#'
#' @seealso
#' * [project()] for creating population projections
#' * [tarr_projection] for the projection object structure
#' * [plot.tarr_pop()] for population pyramid plotting
#' * [as.tarr_pop.tarr_projection()] to extract individual projection cubes
#'
#' @examples
#' # Create a small synthetic projection example
#' dn <- list(
#'   year = as.character(2016:2022),   # 7 years -> "auto" selects ETS
#'   area.name = c("Area A", "Area B"),
#'   sex = c("female", "male"),
#'   age.char = c("0-4", "5-9", "10-14")
#' )
#'
#' arr <- array(
#'   sample(100:500, prod(lengths(dn)), replace = TRUE),
#'   dim = lengths(dn),
#'   dimnames = dn
#' )
#'
#' tp <- new_tarr_pop(
#'   x = DelayedArray::DelayedArray(arr),
#'   dimnames_list = dn,
#'   data_col = "population",
#'   source = "example"
#' )
#'
#' \dontrun{
#'   pr <- project(tp, h = 3, method = "auto")
#'
#'   # Time series of total population with uncertainty ribbon
#'   plot(pr, type = "ts", lwd = 2)
#'
#'   # Population pyramids for selected areas
#'   plot(pr, type = "pyramid", areas = "Area A")
#' }
#' 
#' @export
plot.tarr_projection <- function(x,
                                 type = c("ts", "pyramid"),
                                 areas = NULL,
                                 max_areas = 3,
                                 max_years = 5,
                                 ...) {
  validate_tarr_projection(x)
  
  type <- match.arg(type)
  
  # ---- validate caps ----
  max_areas <- as.integer(max_areas)
  if (length(max_areas) != 1L || is.na(max_areas) || max_areas < 1L || max_areas > 7L) {
    cli::cli_abort(
      "{.arg max_areas} must be an integer between 1 and 7.",
      call = rlang::caller_env()
    )
  }
  
  max_years <- as.integer(max_years)
  if (length(max_years) != 1L || is.na(max_years) || max_years < 1L || max_years > 5L) {
    cli::cli_abort(
      "{.arg max_years} must be an integer between 1 and 5.",
      call = rlang::caller_env()
    )
  }
  
  # Time dim name is centralized here (you removed the duplicate in projection.r)
  time_nm <- tp_time_dim_name(x$projected)
  
  if (identical(type, "ts")) {
    # "ts" ignores area selection by design; it plots totals over time with ribbon
    return(
      plot_ts_tarr_projection(
        x = x,
        time_nm = time_nm,
        ...
      )
    )
    
  }
  
  # ---- pyramid selection / checks ----
  dn <- tp_dimnames(x$projected)
  if (is.null(names(dn))) {
    cli::cli_abort(
      "{.cls tarr_pop} cubes must have named dimensions to plot pyramids.",
      call = rlang::caller_env()
    )
  }
  
  req_dims <- c("area.name", "sex", "age.char", time_nm)
  missing_dims <- setdiff(req_dims, names(dn))
  if (length(missing_dims)) {
    cli::cli_abort(
      c(
        "Population pyramid plotting requires dimensions: {.val {req_dims |> paste(collapse = ', ')}}.",
        "x" = "Missing: {.val {missing_dims |> paste(collapse = ', ')}}."
      ),
      call = rlang::caller_env()
    )
  }
  
  # Select areas
  area_all <- as.character(dn[["area.name"]])
  
  if (is.null(areas)) {
    n_take <- min(max_areas, length(area_all))
    areas_sel <- area_all[seq_len(n_take)]
    if (length(area_all) > n_take) {
      cli::cli_warn(
        c(
          "Plotting first {n_take} of {length(area_all)} {.field area.name} labels.",
          "i" = "Use {.arg areas} to choose specific areas."
        )
      )
    }
  } else {
    areas_sel <- as.character(areas)
    if (!length(areas_sel) || anyNA(areas_sel) || any(!nzchar(areas_sel))) {
      cli::cli_abort(
        "{.arg areas} must be a non-empty character vector of area labels.",
        call = rlang::caller_env()
      )
    }
    missing_areas <- setdiff(unique(areas_sel), unique(area_all))
    if (length(missing_areas)) {
      cli::cli_abort(
        c(
          "{.arg areas} must all be present in the cube's {.field area.name} labels.",
          "x" = "These areas are not present: {.val {missing_areas |> paste(collapse = ', ')}}."
        ),
        call = rlang::caller_env()
      )
    }
  }
  
  # Select years: most recent max_years
  years_all <- as.character(dn[[time_nm]])
  n_years <- min(max_years, length(years_all))
  years_sel <- utils::tail(years_all, n_years)
  
  # Warn if too many panels
  n_panels <- length(areas_sel) * length(years_sel)
  if (n_panels > 16) {
    cli::cli_warn(
      c(
        "Plotting {n_panels} panels ({length(years_sel)} years × {length(areas_sel)} areas).",
        "i" = "Interpretation may be difficult; consider reducing {.arg areas} or {.arg max_years}."
      )
    )
  }
  
  plot_pyramid_tarr_projection(
    x = x,
    time_nm = time_nm,
    years = years_sel,
    areas = areas_sel,
    ...
  )
}

# ---- internal plot helpers --------------------------------------------------------

#' @keywords internal
tp_totals_by_time <- function(tp, time_nm) {
  # Returns a numeric vector of totals by time dimension.
  # Only realizes length-n_time output (safe for HDF5-backed arrays).
  dn <- dimnames(tp)
  if (is.null(names(dn)) || !time_nm %in% names(dn)) {
    cli::cli_abort(
      "Time dimension {.val {time_nm}} not found in tarr_pop dimnames.",
      call = rlang::caller_env()
    )
  }
  
  k_time <- match(time_nm, names(dn))
  
  # DelayedArray::apply will materialize only the result vector (length = n_time)
  out <- DelayedArray::apply(
    tp$handle,
    MARGIN = k_time,
    FUN = function(z) sum(z, na.rm = TRUE)
  )
  
  as.numeric(out)
}

#' @keywords internal
subset_tp_named <- function(tp, selections, drop = FALSE) {
  # selections: named list where names are dimension names, values are
  #   character labels (or vectors of labels) to select.
  dn <- dimnames(tp)
  dnm <- names(dn)
  if (is.null(dnm)) {
    cli::cli_abort(
      "tarr_pop must have named dimensions for named subsetting.",
      call = rlang::caller_env()
    )
  }
  
  idx <- rep(list(TRUE), length(dn))
  
  for (nm in names(selections)) {
    if (!nm %in% dnm) {
      cli::cli_abort(
        c(
          "Unknown dimension name in {.arg selections}.",
          "x" = "{.val {nm}} is not a dimension of the cube.",
          "i" = "Valid dimensions: {.val {dnm |> paste(collapse = ', ')}}."
        ),
        call = rlang::caller_env()
      )
    }
    k <- match(nm, dnm)
    idx[[k]] <- selections[[nm]]
  }
  
  do.call(`[`, c(list(tp), idx, list(drop = drop)))
}

# ---- plot implementations ----------------------------------------------------

#' @keywords internal
plot_ts_tarr_projection <- function(x, time_nm, ...) {
  
  dots <- list(...)
  
  # ---- extract ribbon-specific args ----
  ribbon_col <- dots$ribbon_col %||% grDevices::adjustcolor("gray70", alpha.f = 0.5)
  ribbon_border <- dots$ribbon_border %||% NA
  
  # Remove custom args so they don't leak to plot()
  dots$ribbon_col <- NULL
  dots$ribbon_border <- NULL
  
  # ---- totals by time (length-n_years only) ----
  years_chr <- as.character(dimnames(x$projected)[[time_nm]])
  
  y_hat <- tp_totals_by_time(x$projected, time_nm = time_nm)
  y_lo  <- tp_totals_by_time(x$lower,     time_nm = time_nm)
  y_hi  <- tp_totals_by_time(x$upper,     time_nm = time_nm)
  
  years_int <- suppressWarnings(as.integer(years_chr))
  is_num_year <- !anyNA(years_int)
  
  if (is_num_year) {
    xx <- years_int
    
    do.call(
      graphics::plot,
      c(
        list(
          x = xx,
          y = y_hat,
          type = "l",
          xlab = time_nm,
          ylab = "Population"
        ),
        dots
      )
    )
    
    graphics::polygon(
      x = c(xx, rev(xx)),
      y = c(y_hi, rev(y_lo)),
      col = ribbon_col,
      border = ribbon_border
    )
    
    graphics::lines(xx, y_hat)
    
  } else {
    xx <- seq_along(years_chr)
    
    do.call(
      graphics::plot,
      c(
        list(
          x = xx,
          y = y_hat,
          type = "l",
          xaxt = "n",
          xlab = time_nm,
          ylab = "Population"
        ),
        dots
      )
    )
    
    graphics::axis(1, at = xx, labels = years_chr)
    
    graphics::polygon(
      x = c(xx, rev(xx)),
      y = c(y_hi, rev(y_lo)),
      col = ribbon_col,
      border = ribbon_border
    )
    
    graphics::lines(xx, y_hat)
  }
  
  # ---- titles ----
  lvl <- attr(x, "level", exact = TRUE)
  mth <- attr(x, "method", exact = TRUE)
  
  graphics::title(
    main = "Projected total population",
    sub  = paste0("method: ", mth, " | level: ", lvl)
  )
  
  invisible(x)
}

#' @keywords internal
plot_pyramid_tarr_projection <- function(x, time_nm, years, areas, ...) {
  # Delegate to existing plot.tarr_pop pyramid logic by subsetting forecast cube
  # to the selected years × areas (and keeping all other dims).
  tp <- x$projected
  
  # Subset selected years (most recent) and areas
  sel <- list()
  sel[[time_nm]] <- years
  sel[["area.name"]] <- areas
  
  tp_sub <- subset_tp_named(tp, selections = sel, drop = FALSE)
  
  # Delegate (your plot.tarr_pop already builds a year×area grid)
  plot(tp_sub, ...)
  
  invisible(x)
}



