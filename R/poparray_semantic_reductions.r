#' @keywords internal
#' @noRd
pa_has_interval_overlap <- function(labels) {
  if (!length(labels) || length(labels) <= 1L) {
    return(FALSE)
  }

  bnd <- tryCatch(tp_age_bounds(as.character(labels)), error = function(e) NULL)
  if (is.null(bnd) || anyNA(bnd$start) || anyNA(bnd$end)) {
    return(TRUE)
  }

  ord <- order(bnd$start, bnd$end)
  start <- bnd$start[ord]
  end <- bnd$end[ord]

  max_end <- end[[1L]]
  if (length(start) > 1L) {
    for (i in 2:length(start)) {
      if (start[[i]] < max_end) {
        return(TRUE)
      }
      max_end <- max(max_end, end[[i]])
    }
  }

  FALSE
}

pa_dim_has_overlap_risk <- function(sem, labels) {
  if (pa_is_partition(sem)) {
    return(FALSE)
  }

  labs <- unique(as.character(labels))
  if (length(labs) <= 1L) {
    return(FALSE)
  }

  if (pa_is_interval(sem)) {
    return(pa_has_interval_overlap(labs))
  }

  if (length(sem@overlap_levels) > 0L) {
    active_overlap_levels <- intersect(labs, as.character(sem@overlap_levels))
    return(length(active_overlap_levels) > 0L)
  }

  TRUE
}

#' Semantic guard for sum(poparray)
#'
#' Enforces strict epidemiologic safeguards by default: reductions are blocked
#' when any remaining dimension has derived overlap risk.
#'
#' @param x A poparray.
#' @param ... Additional arguments. Supports `strict` (default `TRUE`) and
#'   `allow_overlap` (default `FALSE`) as semantic-guard controls; remaining
#'   arguments are passed to the delayed reduction.
#' @param na.rm Logical; remove missing values?
#'
#' @return Numeric scalar.
#' @export
setMethod(
  "sum",
  "poparray",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    strict <- dots[["strict"]] %||% TRUE
    allow_overlap <- dots[["allow_overlap"]] %||% FALSE
    dots[["strict"]] <- NULL
    dots[["allow_overlap"]] <- NULL

    dsem <- dim_semantics(x)
    dn <- dimnames(x)
    is_unsafe <- vapply(
      names(dsem),
      function(nm) pa_dim_has_overlap_risk(dsem[[nm]], dn[[nm]]),
      logical(1)
    )
    unsafe_dims <- names(dsem)[is_unsafe]

    if (length(unsafe_dims) > 0L && !isTRUE(allow_overlap)) {
      msg <- c(
        "Unsafe reduction blocked for {.cls poparray}.",
        "i" = "Unsafe dimensions: {.val {paste(unsafe_dims, collapse = ', ')}}.",
        "i" = "Set {.arg allow_overlap = TRUE} to bypass, or {.arg strict = FALSE} to warn and continue."
      )
      if (isTRUE(strict)) {
        cli::cli_abort(msg)
      }
      cli::cli_warn(msg)
    }

    do.call(
      base::sum,
      c(list(methods::as(x, "DelayedArray")), dots, list(na.rm = na.rm))
    )
  }
)
