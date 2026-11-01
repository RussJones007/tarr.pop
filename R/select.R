# -------------------------------------------------------------------------------------->
# Script: select.R
# Description:
#   Selecting dimensions aggregates omitted axes through guarded blockwise HDF5 reductions; it does not
#   discard their population counts. Required time and area roles remain as singleton axes when omitted.
#
#   Implements select.poparray() with tidyselect names, ordering, and renaming. It composes collapse,
#   relocation, and metadata-aware renaming, then validates the result; pure reorder and rename selections
#   remain lazy.
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: 2026-10-29 (first recorded Git commit)
# -------------------------------------------------------------------------------------->

#' Select, reorder, and rename poparray dimensions
#'
#' The structural implementation is also available to `demographic_array`
#' subclasses. Reconstruction preserves their concrete class and metadata;
#' projection statistical axes retain their additional restrictions.
#'
#' Select dimensions using tidyselect names, ranges, negative selections, and name-based helpers. Predicate helpers such
#' as `where()` are not supported. Unlike selection of data-frame columns, omitting a dimension aggregates its levels
#' with [collapse_all()] so that counts are preserved. Omitted dimensions are collapsed in their original array order,
#' independently of the requested output order.
#'
#' Omitted dimensions are dropped except for the required time and area roles, which remain as singleton dimensions and
#' are appended in their original relative order. An empty selection collapses every dimension and retains these two
#' roles. Existing overlap and applicability guards apply to ensure data integrity, including restrictions on collapsing
#' or dropping applicability controllers.
#'
#' Renaming uses `new_name = old_name`. Roles, grouping, semantic dimension names, and applicability references follow
#' the result. Reordering and renaming are lazy; aggregation computes bounded blocks into temporary HDF5 storage without
#' realizing the full cube in memory. The input is unchanged.
#'
#' @param .data A `poparray`.
#' @param ... Tidy-select expressions selecting dimensions, not category levels.
#' @return A HDF5-backed `poparray` with selected dimensions followed by any omitted required dimensions. Invalid or
#'   unsafe collapses throw an error.
#' @seealso [collapse_all()], [rename.poparray()], [relocate.poparray()]
#' @importFrom dplyr select
#' @exportS3Method select poparray
#' @examples
#' \dontrun{
#' pop |> dplyr::select(year, county = area.name)
#' pop |> dplyr::select(-sex)
#' }
select.poparray <- function(.data, ...) {
  validate_demographic_array(.data)
  original_names <- names(dimnames(.data))
  selection <- tidyselect::eval_select(
    rlang::expr(c(...)),
    data = stats::setNames(seq_along(original_names), original_names),
    allow_predicates = FALSE
  )
  if (anyDuplicated(unname(selection))) {
    cli::cli_abort("A dimension cannot be selected more than once")
  }
  selected_names <- original_names[unname(selection)]
  omitted <- setdiff(original_names, selected_names)
  if (!is(.data, "poparray") && length(omitted)) {
    cli::cli_abort(c("Omitting dimensions requires a subclass-specific reduction rule.",
      "i" = "Projection uncertainty cannot be aggregated by select(); use as.poparray() for count-only reductions."))
  }
  required <- c(time_role(.data), area_role(.data))
  retained <- omitted[omitted %in% required]
  output_names <- c(names(selection), retained)
  if (anyNA(output_names) || any(!nzchar(trimws(output_names))) ||
      anyDuplicated(output_names)) {
    cli::cli_abort(
      "Resulting dimension names must not be missing, blank, or duplicated. Names must also be unique across retained time and area dimensions."
    )
  }

  out <- purrr::reduce(omitted, function(current, dimension) {
    collapse_all(current, dim = dimension, drop = !dimension %in% required)
  }, .init = .data)
  ordered_names <- c(selected_names, retained)
  out <- dplyr::relocate(out, tidyselect::all_of(ordered_names))
  if (!identical(output_names, ordered_names)) {
    out <- pa_rename_dimensions(out, output_names)
  }
  validate_demographic_array(out)
  out
}

#' @rdname select.poparray
#' @exportS3Method select demographic_array
select.demographic_array <- select.poparray
