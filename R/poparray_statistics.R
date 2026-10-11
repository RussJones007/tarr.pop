#' Check overlap safety for cell-distribution statistics
#' @param x A poparray.
#' @param strict Whether to error rather than warn.
#' @param allow_overlap Explicit override.
#' @param override_supported Whether this public method supports overrides.
#' @keywords internal
#' @noRd
pa_check_stat_overlap <- function(x, strict, allow_overlap, override_supported = TRUE) {
  checkmate::assert_flag(strict)
  checkmate::assert_flag(allow_overlap)
  if (allow_overlap) return(invisible(TRUE))
  dn <- dimnames(x)
  unsafe <- names(dn)[vapply(dim_semantics(x), function(sem) {
    pa_dim_has_overlap_risk(sem, dn[[sem@dim_name]], dn)
  }, logical(1))]
  if (length(unsafe)) {
    msg <- c("Unsafe cell summary blocked for {.cls poparray}.",
      "i" = "Unsafe dimensions: {.val {unsafe}}.",
      "i" = "Use {.code overlaps(x)} to inspect risk, then filter to non-overlapping levels.")
    if (override_supported) msg <- c(msg,
      "i" = "Set {.arg allow_overlap = TRUE} to override, or {.arg strict = FALSE} to warn and continue.")
    if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
  }
  invisible(TRUE)
}

#' Build metadata-only applicability plans for statistics
#' @param x A poparray.
#' @keywords internal
#' @noRd
pa_stat_plans <- function(x) {
  dn <- dimnames(x)
  purrr::compact(purrr::imap(dim_semantics(x), function(sem, nm) {
    app <- pa_dim_applicability(sem)
    if (is.null(app)) return(NULL)
    periods <- pa_applicability_indices(app, nm, dn)
    list(target = match(nm, names(dn)), controller = match(app$by, names(dn)),
      periods = periods, levels = purrr::map(app$schemas, function(schema) {
        match(schema$levels, dn[[nm]])
      }))
  }))
}

#' Select applicable values from one bounded block
#' @param block An in-memory source block.
#' @param viewport Current source viewport.
#' @param plans Metadata applicability plans.
#' @keywords internal
#' @noRd
pa_stat_block_values <- function(block, viewport, plans) {
  sizes <- dim(block)
  starts <- IRanges::start(viewport)
  coordinates <- function(k) {
    before <- if (k == 1L) 1 else prod(sizes[seq_len(k - 1L)])
    after <- if (k == length(sizes)) 1 else prod(sizes[seq.int(k + 1L, length(sizes))])
    rep(rep(seq.int(starts[k], length.out = sizes[k]), each = before), times = after)
  }
  keep <- purrr::reduce(plans, function(keep, plan) {
    target <- coordinates(plan$target)
    controller <- coordinates(plan$controller)
    applicable <- purrr::reduce(seq_along(plan$periods), function(valid, i) {
      valid | (controller %in% plan$periods[[i]] & target %in% plan$levels[[i]])
    }, .init = rep(FALSE, length(block)))
    keep & applicable
  }, .init = rep(TRUE, length(block)))
  as.numeric(block)[keep]
}

#' Accumulate stable scalar statistics without realizing a cube
#' @param x A poparray.
#' @param na.rm Remove applicable missing values.
#' @param finite Keep finite values only, for range.
#' @keywords internal
#' @noRd
pa_stat_state <- function(x, na.rm = FALSE, finite = FALSE) {
  validate_poparray(x)
  checkmate::assert_flag(na.rm)
  checkmate::assert_flag(finite)
  plans <- pa_stat_plans(x)
  initial <- list(n = 0, total = 0, anchor = NULL, mean = 0, m2 = 0,
    min = Inf, max = -Inf, has_na = FALSE, has_nan = FALSE,
    pos_inf = FALSE, neg_inf = FALSE)
  if (any(dim(x) == 0L)) return(initial)
  # A regular grid enforces the memory budget even when HDF5 chunks are larger.
  max_cells <- max(1, floor(DelayedArray::getAutoBlockSize() / 8))
  spacings <- purrr::reduce(seq_along(dim(x)), function(spacing, k) {
    spacing[k] <- min(dim(x)[k], max(1, floor(max_cells / prod(spacing))))
    spacing
  }, .init = rep(1L, length(dim(x))))
  grid <- DelayedArray::RegularArrayGrid(dim(x), spacings = as.integer(spacings))
  DelayedArray::blockReduce(function(block, init) {
    vals <- pa_stat_block_values(block, DelayedArray::currentViewport(), plans)
    if (finite) vals <- vals[is.finite(vals)]
    if (!na.rm && anyNA(vals)) {
      init$has_na <- init$has_na || any(is.na(vals) & !is.nan(vals))
      init$has_nan <- init$has_nan || any(is.nan(vals))
    }
    vals <- vals[!is.na(vals)]
    if (!length(vals)) return(init)
    init$min <- min(init$min, min(vals))
    init$max <- max(init$max, max(vals))
    init$pos_inf <- init$pos_inf || any(vals == Inf)
    init$neg_inf <- init$neg_inf || any(vals == -Inf)
    init$total <- init$total + length(vals)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(init)
    # Center on a fixed observed value to avoid cancellation for large counts.
    if (is.null(init$anchor)) init$anchor <- vals[[1L]]
    centered <- vals - init$anchor
    n <- length(centered)
    mu <- base::mean(centered)
    m2 <- sum((centered - mu)^2)
    delta <- mu - init$mean
    combined <- init$n + n
    init$m2 <- if (!init$n) m2 else
      init$m2 + m2 + delta^2 * (init$n / combined) * n
    init$mean <- if (!init$n) mu else init$mean + delta * (n / combined)
    init$n <- combined
    init
  }, methods::as(x, "DelayedArray"), init = initial, grid = grid)
}

#' Blockwise scalar summaries of population cell counts
#'
#' These statistics describe cell counts, not individual residents or their ages.
#' Only simultaneously applicable cells contribute; structural cells are excluded
#' even when they contain non-missing values. Applicable missing values follow
#' `na.rm`. Bounded source blocks are read, never the full cube at once.
#' Mean, variance and standard deviation check current overlap risk. Extrema do
#' not require a partition. Sample variance uses denominator `n - 1` and shares
#' a stable centered accumulator with standard deviation.
#'
#' @param x A `poparray`.
#' @param ... For mean, optional `strict` and `allow_overlap` controls. For
#'   extrema, additional numeric values or `poparray` objects to include.
#' @param trim Must be zero; trimmed means are not implemented.
#' @param na.rm Logical; remove applicable missing values? Defaults to `FALSE`.
#' @param y Must be `NULL`; covariance is not implemented.
#' @param use Missing-value policy as in [stats::var()]. With a single variable,
#'   complete-case policies remove missing values; `all.obs` errors on them.
#' @param finite For range, exclude all non-finite values.
#' @return Mean, sample variance, standard deviation, minimum or maximum as a
#'   numeric scalar; range as a numeric vector of length two. Empty applicable
#'   input gives `NaN` for mean, `NA` for variance/SD, and warned `Inf`/`-Inf`
#'   extrema, matching base R. Variance/SD are `NA` with fewer than two values.
#' @details `mean()` accepts `strict = FALSE` to warn on overlap risk or
#'   `allow_overlap = TRUE` to override it through `...`. Variance and standard
#'   deviation retain the standard R argument lists and always block unresolved
#'   overlap risk. Filter to a safe set of levels before computing them.
#' @name poparray_statistics
NULL

#' @rdname poparray_statistics
#' @export
setMethod("mean", "poparray", function(x, trim = 0, na.rm = FALSE, ...) {
  checkmate::assert_number(trim, lower = 0, upper = 0)
  dots <- list(...)
  checkmate::assert_subset(names(dots), c("strict", "allow_overlap"))
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    cli::cli_abort("Additional mean arguments must be named overlap controls.")
  }
  pa_check_stat_overlap(x, dots$strict %||% TRUE, dots$allow_overlap %||% FALSE)
  s <- pa_stat_state(x, na.rm)
  if (s$has_na) return(NA_real_)
  if (s$has_nan || (s$pos_inf && s$neg_inf)) return(NaN)
  if (s$pos_inf) return(Inf)
  if (s$neg_inf) return(-Inf)
  if (!s$n) return(NaN)
  s$anchor + s$mean
})

#' @rdname poparray_statistics
#' @export
setMethod("var", "poparray",
  function(x, y = NULL, na.rm = FALSE, use) {
    if (!is.null(y)) cli::cli_abort("Covariance with {.arg y} is not implemented for {.cls poparray}; omit {.arg y}.")
    pa_check_stat_overlap(x, TRUE, FALSE, override_supported = FALSE)
    checkmate::assert_flag(na.rm)
    if (missing(use)) use <- if (na.rm) "na.or.complete" else "everything"
    choices <- c("all.obs", "complete.obs", "pairwise.complete.obs", "everything", "na.or.complete")
    i <- pmatch(use, choices)
    if (length(i) != 1L || is.na(i)) cli::cli_abort("Invalid {.arg use} argument.")
    use <- choices[[i]]
    s <- pa_stat_state(x, na.rm = !use %in% c("everything", "all.obs"))
    if (use == "all.obs" && (s$has_na || s$has_nan)) cli::cli_abort("missing observations in cov/cor")
    if (use %in% c("complete.obs", "pairwise.complete.obs") && !s$total) cli::cli_abort("no complete element pairs")
    if (s$has_na || s$has_nan || s$total < 2) return(NA_real_)
    if (s$pos_inf || s$neg_inf) return(NaN)
    s$m2 / (s$n - 1)
  })

#' @rdname poparray_statistics
#' @export
setMethod("sd", "poparray", function(x, na.rm = FALSE) {
  sqrt(var(x, na.rm = na.rm))
})

#' Compute extrema for a cube and optional additional arguments
#' @param x A poparray.
#' @param dots Additional values.
#' @param na.rm Remove missing values.
#' @param finite Exclude non-finite values.
#' @param which Extremum to compute.
#' @keywords internal
#' @noRd
pa_stat_extrema <- function(x, dots, na.rm, finite = FALSE, which = "range") {
  values <- purrr::map(c(list(x), dots), function(value) {
    if (!is(value, "poparray")) return(value)
    s <- pa_stat_state(value, na.rm, finite)
    if (s$has_na) return(NA_real_)
    if (s$has_nan) return(NaN)
    if (!s$total) return(numeric())
    c(s$min, s$max)
  })
  args <- c(values, list(na.rm = na.rm))
  if (which == "range") args$finite <- finite
  do.call(get(which, envir = baseenv()), args)
}

#' @rdname poparray_statistics
#' @export
setMethod("min", "poparray", function(x, ..., na.rm = FALSE) {
  pa_stat_extrema(x, list(...), na.rm, which = "min")
})

#' @rdname poparray_statistics
#' @export
setMethod("max", "poparray", function(x, ..., na.rm = FALSE) {
  pa_stat_extrema(x, list(...), na.rm, which = "max")
})

#' @rdname poparray_statistics
#' @export
setMethod("range", "poparray", function(x, ..., finite = FALSE, na.rm = FALSE) {
  pa_stat_extrema(x, list(...), na.rm, finite, which = "range")
})
