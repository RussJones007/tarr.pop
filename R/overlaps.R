#' Inspect declared and current overlap in a population cube
#'
#' Reports overlap metadata for every dimension without reading population
#' values or changing the cube. Current risk uses the same interval and
#' applicability-aware checks as guarded reductions.
#'
#' @param x A `poparray` object.
#' @return A list named by dimension, in array order. Each entry contains:
#'   * `original_levels`: The intrinsic `DimSemantics@overlap_levels`
#'     declaration, retained after filtering. These are known overlap-causing
#'     labels, not a complete history of the original cube's labels or overlaps.
#'   * `current_levels`: Declared overlap-causing labels still present in the
#'     current dimension's labels. Presence does not establish simultaneous
#'     applicability or overlap when only one level remains.
#'   * `has_overlap`: Logical scalar indicating current overlap **risk**, as
#'     evaluated by the package's reduction guards. Unrecognized intervals and
#'     multi-level sets with unknown overlap are conservatively reported as
#'     `TRUE`; it does not always mean overlap has been positively established.
#' @details
#' Empty `original_levels` does not prove safety: interval overlap may be derived
#' from active labels, and overlap for a set may be unknown. Conversely, a
#' declared level can remain present while `has_overlap` is `FALSE`, for example
#' when it is the only active level or when applicability separates overlapping
#' union labels into different periods. A `FALSE` result is specific to the
#' existing guards, not a guarantee that every measure can be aggregated.
#' No attempt is made to reconstruct overlap declarations that were never stored.
#' @seealso [drop_overlap_levels()], [dim_semantics()]
#' @examples
#' \dontrun{
#' overlaps(x)$race
#' overlaps(drop_overlap_levels(x, dim = "race"))$race
#' }
#' @export
setGeneric("overlaps", function(x) standardGeneric("overlaps"))

#' @rdname overlaps
#' @export
setMethod("overlaps", "poparray", function(x) {
  validate_poparray(x)
  dn <- dimnames(x)
  sem <- dim_semantics(x)
  result <- lapply(names(dn), function(nm) {
    entry <- sem[[nm]]
    list(original_levels = entry@overlap_levels,
      current_levels = intersect(entry@overlap_levels, as.character(dn[[nm]])),
      has_overlap = pa_dim_has_overlap_risk(entry, dn[[nm]], dn))
  })
  names(result) <- names(dn)
  result
})
