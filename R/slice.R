#' Slice positions within a poparray dimension
#'
#' `dplyr::slice()` selects integer positions along one explicitly named
#' dimension, rather than rows of a table. Selection is lazy and retains all
#' dimensions, even when a dimension has zero or one selected labels.
#'
#' @param .data A `poparray`.
#' @param ... Integer position vectors or expressions evaluated in the caller's
#'   environment. Positive positions keep labels; negative positions exclude
#'   labels. Zeros and out-of-range positions are ignored. All nonzero positions
#'   must have the same sign. Missing, fractional, and duplicated retained
#'   positions are rejected. With no expressions, return the input unchanged.
#' @param .by Must be `NULL`; grouped slicing is not supported.
#' @param .preserve Must be `FALSE`; grouped slicing is not supported.
#' @param .dimension A single dimension name, supplied as a character string.
#'
#' @return A delayed, HDF5-backed `poparray` with selected labels and updated
#'   applicability metadata. Roles, provenance, and value-column metadata remain
#'   intact. No population values are extracted or backing files modified.
#' @details Positions refer to the current dimension after any earlier filtering
#'   or relocation. Labels are not evaluated as table columns; helpers such as
#'   `dplyr::n()` and the separate `slice_*()` generics are not supported here.
#'   Use `length(dim_labels(pop, "year"))` to obtain a dimension's size.
#'
#'   Unlike table slicing, duplicate selections are rejected to prevent repeated
#'   population categories. Time selections must retain their existing order.
#'   Other dimensions may be reordered. No labels are renamed, age intervals
#'   split, or reporting schemas harmonized. Applicability is trimmed by the
#'   existing named array-subsetting method, and reduction guards remain active.
#' @seealso [filter.poparray()], [relocate.poparray()]
#' @importFrom dplyr slice
#' @exportS3Method slice poparray
#' @examples
#' path <- system.file("extdata", "tdc_example.h5", package = "tarr.pop")
#' pop <- open_poparray(filepath = path)
#' first_years <- dplyr::slice(pop, 1:2, .dimension = "year")
#' times(first_years)
#' without_first <- dplyr::slice(pop, -1, .dimension = "area.name")
#' areas(without_first)
slice.poparray <- function(.data, ..., .by = NULL, .preserve = FALSE,
                           .dimension) {
  validate_poparray(.data)
  if (!rlang::quo_is_null(rlang::enquo(.by)) || !identical(.preserve, FALSE)) {
    cli::cli_abort("Grouped slicing with {.arg .by} or {.arg .preserve} is not supported for a poparray.")
  }
  if (missing(.dimension)) {
    cli::cli_abort("Supply {.arg .dimension} as a single dimension name.")
  }
  checkmate::assert_string(.dimension, min.chars = 1)
  dn <- dimnames(.data)
  if (!.dimension %in% names(dn)) {
    cli::cli_abort("Unknown dimension {.val {(.dimension)}}.")
  }
  expressions <- rlang::enquos(...)
  if (any(nzchar(names(expressions)))) {
    cli::cli_abort("Slice positions must be unnamed; select the axis with {.arg .dimension}.")
  }
  if (!length(expressions)) return(.data)
  positions <- lapply(expressions, function(expression) {
    value <- rlang::eval_tidy(expression)
    checkmate::assert_numeric(value, any.missing = FALSE, finite = TRUE)
    checkmate::assert_integerish(value, any.missing = FALSE)
    value
  }) |> unlist(use.names = FALSE)
  if (any(positions > 0) && any(positions < 0)) {
    cli::cli_abort("Slice positions must be all positive or all negative, ignoring zeros.")
  }
  size <- length(dn[[.dimension]])
  if (any(positions < 0)) {
    excluded <- -positions[positions < 0 & positions >= -size]
    selected <- setdiff(seq_len(size), excluded)
  } else {
    selected <- as.integer(positions[positions > 0 & positions <= size])
  }
  if (anyDuplicated(selected)) {
    cli::cli_abort("Duplicated slice positions would repeat population categories.")
  }
  if (identical(.dimension, time_role(.data)) && is.unsorted(selected)) {
    cli::cli_abort("Slice positions for the time dimension must retain increasing order.")
  }
  indices <- stats::setNames(list(selected), .dimension)
  do.call(`[`, c(list(.data), indices, list(drop = FALSE)))
}
