#' Relocate poparray dimensions lazily
#'
#' `dplyr::relocate()` changes dimension order using tidy-select expressions.
#' Population values remain in their original HDF5 dataset; a delayed
#' permutation changes the view. All dimension labels and semantic contracts
#' follow their named dimensions, including time/area roles and applicability.
#'
#' @param .data A `poparray`.
#' @param ... Tidy-select expressions identifying dimensions to move, in their
#'   requested order. Renaming is not supported; use [rename.poparray()] first.
#' @param .before,.after Tidy-select destination expressions. Supply at most one.
#'   With neither, move selected dimensions to the front. With several destination
#'   dimensions, move before the first or after the last in the existing order.
#'
#' @return An HDF5-backed, delayed `poparray` with reordered dimensions and
#'   semantic metadata. No population values are extracted or files rewritten.
#' @details R arrays use column-major order: the first subscript varies fastest.
#'   This describes logical traversal, not a rule that moving a frequently used
#'   dimension first always improves HDF5 access. Chunk geometry, access patterns,
#'   and block processing also affect performance. Relocation does not rechunk
#'   or physically reorder the stored dataset. Benchmark the intended workflow
#'   before treating dimension order as an optimization.
#'
#'   Dimension selectors operate on names, not labels or population values.
#'   The relative order of unselected dimensions is preserved. Empty selection
#'   returns the input unchanged. No dimensions are dropped or labels reordered.
#' @seealso [slice.poparray()], [rename.poparray()]
#' @importFrom dplyr relocate
#' @exportS3Method relocate poparray
#' @examples
#' path <- system.file("extdata", "tdc_example.h5", package = "tarr.pop")
#' pop <- open_poparray(filepath = path)
#' age_first <- dplyr::relocate(pop, age.char, .before = year)
#' names(dimnames(age_first))
#' area_last <- dplyr::relocate(pop, area.name, .after = dplyr::last_col())
#' names(dimnames(area_last))
relocate.poparray <- function(.data, ..., .before = NULL, .after = NULL) {
  validate_poparray(.data)
  dn <- dimnames(.data)
  dimension_names <- names(dn)
  selector_data <- stats::setNames(seq_along(dn), dimension_names)
  select_dimensions <- function(expression) {
    tidyselect::eval_select(expression, data = selector_data,
                            allow_rename = FALSE, allow_predicates = FALSE)
  }
  before <- rlang::enquo(.before)
  after <- rlang::enquo(.after)
  has_before <- !rlang::quo_is_null(before)
  has_after <- !rlang::quo_is_null(after)
  if (has_before && has_after) {
    cli::cli_abort("Supply only one of {.arg .before} and {.arg .after}.")
  }
  selected <- unname(select_dimensions(rlang::expr(c(...))))
  # Resolve the destination even for an empty selection, so invalid names fail.
  destination <- if (has_before) {
    anchors <- unname(select_dimensions(before))
    if (length(anchors)) min(anchors) else 1L
  } else if (has_after) {
    anchors <- unname(select_dimensions(after))
    if (length(anchors)) max(anchors) + 1L else length(dn) + 1L
  } else 1L
  if (!length(selected)) return(.data)
  remaining <- setdiff(seq_along(dn), selected)
  permutation <- c(remaining[remaining < destination], selected,
                   remaining[remaining >= destination])
  if (identical(permutation, seq_along(dn))) return(.data)
  reordered_names <- dimension_names[permutation]
  dx <- methods::as(.data, "DelayedArray")
  out <- DelayedArray::aperm(dx, perm = permutation)
  dimnames(out) <- dn[permutation]
  new_poparray(out, dimnames_list = dn[permutation],
               data_col = data_col(.data), source = get_source(.data),
               time_dim = time_role(.data), area_dim = area_role(.data),
               dim_semantics = dim_semantics(.data)[reordered_names],
               group_vars = .data@group_vars)
}
