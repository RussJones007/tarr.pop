# -------------------------------------------------------------------------------------->
# Script: demographic_array.R
# Description:
#   A virtual S4 DelayedArray superclass shares dimensional and semantic structure without imposing
#   count or uncertainty reduction rules. Concrete subclasses own statistical meaning and reconstruction.
#
#   Defines shared slots, structural validity, special-axis dispatch, and reconstruction interfaces.
#   Helpers validate demographic objects and preserve concrete classes during lazy transformations.
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: October 31, 2026
# -------------------------------------------------------------------------------------->

#' Virtual superclass for demographic arrays
#'
#' Shares named dimensions, distinct time and area roles, current dimension
#' semantics, a value-column name, and source provenance across delayed arrays.
#' Construct a concrete [poparray] or [poparray_projection], not this class.
#' Statistical reduction rules and persistence remain subclass responsibilities.
#' The superclass itself does not require an HDF5 backend.
#'
#' @slot time_role,area_role Names of the distinct time and area dimensions.
#' @slot strata_roles Population stratification axes, excluding special axes.
#' @slot dim_semantics Named current per-dimension semantic contracts.
#' @slot data_col Name used for the value column in tabular representations.
#' @slot source Named source provenance list.
#' @name demographic_array-class
#' @aliases demographic_array
#' @exportClass demographic_array
NULL

setClass("demographic_array", contains = c("DelayedArray", "VIRTUAL"),
  slots = c(time_role = "character", area_role = "character",
    strata_roles = "character", dim_semantics = "list",
    data_col = "character", source = "list"))

#' Identify subclass-specific statistical axes
#' @param x A demographic array.
#' @return Character vector of special axis names, excluded from strata roles.
#' @noRd
setGeneric("da_special_axes", function(x) standardGeneric("da_special_axes"))
setMethod("da_special_axes", "demographic_array", function(x) character())

setValidity("demographic_array", function(object) {
  dn <- dimnames(object)
  nms <- names(dn)
  if (is.null(dn) || is.null(nms)) return("dimnames must exist and be named.")
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    return("Dimension names must be unique, non-missing, and non-empty.")
  }
  for (role in c("time_role", "area_role")) {
    value <- methods::slot(object, role)
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      return(paste0("slot '", role, "' must be a single non-empty character string."))
    }
    if (!value %in% nms) return(paste0(role, " must match a dimension name."))
  }
  if (identical(object@time_role, object@area_role)) {
    return("time_role and area_role must be different.")
  }
  expected <- setdiff(nms, c(object@time_role, object@area_role, da_special_axes(object)))
  if (!identical(object@strata_roles, expected)) {
    return("strata_roles must match the non-time/non-area/non-special dimensions.")
  }
  if (length(dn) != length(dim(object)) || !identical(unname(lengths(dn)), as.integer(dim(object)))) {
    return("dimnames must align with dimension extents.")
  }
  if (length(object@data_col) != 1L || is.na(object@data_col) || !nzchar(object@data_col)) {
    return("slot 'data_col' must be a single non-empty character string.")
  }
  tryCatch({
    validate_dim_semantics(object@dim_semantics, nms,
      object@time_role, object@area_role, dn)
    TRUE
  }, error = function(e) conditionMessage(e))
})

#' Validate shared structural metadata without reading array values
#' @param x A demographic array.
#' @return The object invisibly, or an informative error.
#' @noRd
validate_demographic_array <- function(x) {
  if (!is(x, "demographic_array")) cli::cli_abort("{.arg x} must be a {.cls demographic_array}.")
  result <- methods::validObject(x, test = TRUE)
  if (!isTRUE(result)) cli::cli_abort(result)
  invisible(x)
}

#' Reconstruct a concrete subclass after a structural transformation
#' @param x Original demographic array.
#' @param backend Transformed delayed backend.
#' @param dimnames_list Current named dimension labels.
#' @param dim_semantics Current aligned semantic entries.
#' @param time_dim,area_dim Current required role names.
#' @param group_vars Analysis grouping, used only by population count arrays.
#' @param statistic Selected statistic when its singleton axis was removed.
#' @return A concrete demographic array or the underlying delayed result.
#' @noRd
setGeneric("da_reconstruct", function(x, backend, dimnames_list,
    dim_semantics, time_dim = x@time_role, area_dim = x@area_role,
    group_vars = character(), statistic = NULL) standardGeneric("da_reconstruct"))
