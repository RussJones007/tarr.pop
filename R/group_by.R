#' Group a poparray by dimensions
#'
#' `group_by()` records the dimensions whose coordinate combinations will remain
#' distinct during [summarise.poparray()]. Grouping and ungrouping only change
#' metadata: no population values are read, labels reordered, or files modified.
#' The object remains an S4 `poparray`, rather than a grouped data frame.
#'
#' @param .data,x A `poparray`.
#' @param ... In `group_by()`, tidy-select expressions selecting dimension names.
#'   Computed groups and renaming are not supported. In `ungroup()`, dimensions
#'   to remove from grouping; with no arguments all grouping is removed.
#' @param .add Add to existing grouping instead of replacing it?
#' @param .drop Must be `TRUE`. Cube coordinates are explicit; unused factor
#'   levels and row-based empty-group dropping are not supported.
#' @return A delayed, HDF5-backed `poparray` with updated grouping metadata.
#'   `group_vars()` returns an ordered character vector and `groups()` a list
#'   of symbols. Neither accessor reads population values.
#' @details Grouping selects existing axes; it does not combine categories into
#'   new age bands. Use [group_ages()] or [collapse_dim()] for category grouping.
#'   Coordinate filtering and slicing continue to select named axes, without
#'   interpreting labels or population values as table rows. Grouping follows
#'   subsetting, dimension renaming, and relocation. Removed dimensions leave the
#'   grouping. Grouping is an in-memory analysis setting: saving and reopening a
#'   cube returns an ungrouped object.
#' @seealso [summarise.poparray()], [rename.poparray()]
#' @importFrom dplyr group_by ungroup group_vars groups
#' @exportS3Method group_by poparray
#' @examples
#' path <- system.file("extdata", "tdc_example.h5", package = "tarr.pop")
#' pop <- open_poparray(filepath = path)
#' grouped <- dplyr::group_by(pop, year, area.name)
#' dplyr::group_vars(grouped)
#' dplyr::group_vars(dplyr::ungroup(grouped, area.name))
#' dplyr::group_vars(dplyr::ungroup(grouped))
group_by.poparray <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  validate_poparray(.data)
  checkmate::assert_flag(.add)
  if (!identical(.drop, TRUE)) {
    cli::cli_abort("{.arg .drop} must be TRUE; factor-level groups are not supported for a poparray.")
  }
  selectors <- rlang::enquos(...)
  selected <- pa_select_group_dimensions(.data, rlang::expr(c(!!!selectors)))
  existing <- if (.add) .data@group_vars else character()
  pa_set_group_vars(.data, union(existing, selected))
}

#' @rdname group_by.poparray
#' @exportS3Method ungroup poparray
ungroup.poparray <- function(x, ...) {
  validate_poparray(x)
  selectors <- rlang::enquos(...)
  remaining <- if (!length(selectors)) character() else
    setdiff(x@group_vars, pa_select_group_dimensions(x, rlang::expr(c(!!!selectors))))
  pa_set_group_vars(x, remaining)
}

#' @rdname group_by.poparray
#' @exportS3Method group_vars poparray
group_vars.poparray <- function(x) {
  validate_poparray(x)
  x@group_vars
}

#' @rdname group_by.poparray
#' @exportS3Method groups poparray
groups.poparray <- function(x) {
  rlang::syms(group_vars.poparray(x))
}

#' Select grouping axes without reading values
#' @param x A poparray.
#' @param expression A tidy-select expression or quosure.
#' @return Dimension names in selection order.
#' @noRd
pa_select_group_dimensions <- function(x, expression) {
  dn <- dimnames(x)
  indices <- tidyselect::eval_select(expression,
    data = stats::setNames(seq_along(dn), names(dn)),
    allow_rename = FALSE, allow_predicates = FALSE)
  names(dn)[unname(indices)]
}

#' Assign valid grouping axes to an in-memory cube view
#' @param x A poparray.
#' @param dimensions Ordered grouping dimension names.
#' @return A grouped or ungrouped poparray.
#' @noRd
pa_set_group_vars <- function(x, dimensions) {
  checkmate::assert_character(dimensions, any.missing = FALSE, unique = TRUE)
  if (any(!dimensions %in% names(dimnames(x)))) {
    cli::cli_abort("Grouping dimensions must exist in the current cube.")
  }
  x@group_vars <- dimensions
  x
}
