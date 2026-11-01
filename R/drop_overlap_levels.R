# -------------------------------------------------------------------------------------->
# Script: drop_overlap_levels.R
# Description:
#   Removing declared overlap labels is lazy dimension subsetting and does not infer how to reconcile
#   overlapping age intervals or unknown categories.
#
#   drop_overlap_levels() removes active declared labels from selected dimensions through the existing
#   subsetting method, which updates semantic metadata and retains aggregation guards.
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: 2026-10-10 (first recorded Git commit)
# -------------------------------------------------------------------------------------->

#' Remove declared overlap-causing dimension levels
#'
#' Lazily subsets a population cube, removing active labels declared in each
#' selected dimension's `overlap_levels`. Dimensions, roles, and source metadata
#' are retained. The existing subsetting method prunes absent overlap declarations,
#' updates confirmed safe subsets to partitions, and trims applicability.
#' No population values are read.
#'
#' This removes declared labels only: it does not choose among overlapping age
#' intervals or resolve period-specific overlap. An empty declaration can mean
#' unknown overlap. Existing aggregation guards still apply to the result.
#'
#' @param x A `poparray` object.
#' @param dim Non-empty character vector of dimension names. To select every
#'   dimension explicitly, use `names(dimnames(x))`.
#' @return A lazily subsetted `poparray`. A dimension with no active declared
#'   overlap levels is unchanged. Removing all levels of a dimension is an error.
#' @examples
#' \dontrun{
#' drop_overlap_levels(x, dim = "race")
#' drop_overlap_levels(x, dim = names(dimnames(x)))
#' }
#' @export
drop_overlap_levels <- function(x, dim) {
  if (!is(x, "poparray")) cli::cli_abort("{.arg x} must be a {.cls poparray}.")
  validate_poparray(x)
  checkmate::assert_character(dim, min.len = 1L, any.missing = FALSE, unique = TRUE)
  dn <- dimnames(x)
  checkmate::assert_subset(dim, names(dn))
  sem <- dim_semantics(x)
  keep <- lapply(dim, function(nm) setdiff(dn[[nm]], sem[[nm]]@overlap_levels))
  names(keep) <- dim
  empty <- dim[lengths(keep) == 0L]
  if (length(empty)) cli::cli_abort("Removing overlap levels would empty dimension {.val {empty}}.")
  changed <- dim[!vapply(dim, function(nm) identical(keep[[nm]], dn[[nm]]), logical(1))]
  if (!length(changed)) return(x)
  indices <- lapply(names(dn), function(nm) if (nm %in% changed) keep[[nm]] else TRUE)
  do.call(`[`, c(list(x), unname(indices), list(drop = FALSE)))
}
