#' Semantic guard for sum(poparray)
#'
#' Enforces strict epidemiologic safeguards by default: reductions are blocked
#' when any remaining dimension is marked non-exclusive or overlapping.
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
    is_unsafe <- vapply(
      dsem,
      function(ent) isFALSE(ent$exclusive) || isTRUE(ent$overlapping),
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
