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
#' @slot time_role Name of the time dimension.
#' @slot area_role Name of the distinct area dimension.
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
    group_vars = character(), statistic = NULL) standardGeneric("da_reconstruct"), signature = "x")

#' Get the time role name for a demographic array
#'
#' @param x A demographic_array subclass.
#' @return A single character string naming the time dimension.
#' @export
time_role <- function(x) {
  validate_demographic_array(x)
  return(x@time_role)
}

#' Get the area role name for a demographic array
#'
#' @param x A demographic_array subclass.
#' @return A single character string naming the area dimension.
#' @export
area_role <- function(x) {
  validate_demographic_array(x)
  return(x@area_role)
}




#' Get or set the demographic array value-column name
#'
#' Reads or updates the authoritative `data_col` slot on a demographic array.
#' Canonical HDF5 paths retain their metadata-admin interface; other legacy
#' objects retain the attribute fallback. This operation reads no numeric data.
#'
#' @param x A demographic_array subclass or canonical HDF5 cube path.
#' @param value New value-column name for replacement.
#' @return Character string.
#' @export
data_col <- function(x) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(read_cube_data_col_impl(x))
  }
  if (is(x, "demographic_array")) return(x@data_col)
  attr(x, "data_col", exact = TRUE)
}

#' Get or set the current demographic dimension semantics
#'
#' Returns the per-dimension semantic contract used for guarded
#' reductions and metadata persistence.
#' Inspect applicability with `dim_semantics(x)[[dimension]]@applicability`.
#' `NULL` preserves legacy behavior. Otherwise `by` identifies the controlling
#' dimension and `schemas` lists inclusive `from`/`through` label ranges and
#' canonical `levels`; NULL endpoints span the current domain. Dimension labels
#' may be the union across schemas, while overlap is checked among simultaneously
#' applicable current levels. Applicability is semantic metadata, not provenance.
#' Reading or validating it does not read population values. For cube paths,
#' replacement writes only metadata and retains existing metadata-role controls.
#'
#' @param x A demographic_array subclass or canonical HDF5 cube path.
#' @param value Named list of DimSemantics entries for replacement.
#' @return Named list with one `DimSemantics` object per dimension.
#' @export
dim_semantics <- function(x) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(read_cube_dim_semantics_impl(x))
  }
  if (!is(x, "demographic_array")) {
    cli::cli_abort("{.arg x} must be a {.cls demographic_array} or an HDF5 cube path.")
  }
  x@dim_semantics
}

#' @rdname data_col
#' @export
`data_col<-` <- function(x, value) {
  checkmate::assert_character(value, len = 1, any.missing = FALSE)
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    write_cube_data_col_impl(x, value)
    return(x)
  }
  if (is(x, "demographic_array")) {
    x@data_col <- value
    validate_demographic_array(x)
  }
  attr(x, "data_col") <- value
  x
}

#' @rdname dim_semantics
#' @export
`dim_semantics<-` <- function(x, value) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    write_cube_dim_semantics_impl(x, value)
    return(x)
  }
  if (!is(x, "demographic_array")) {
    cli::cli_abort("{.arg x} must be a {.cls demographic_array} or an HDF5 cube path.")
  }
  if (is.null(value) || !is.list(value) || is.null(names(value))) {
    cli::cli_abort("{.arg value} must be a named list of DimSemantics entries.")
  }
  validate_dim_semantics(
    dim_semantics = value,
    dim_names = names(dimnames(x)),
    time_dim = time_role(x),
    area_dim = area_role(x),
    dimnames_list = dimnames(x)
  )
  x@dim_semantics <- value
  validate_demographic_array(x)
  x
}



#' Align semantic contracts with a transformed domain
#' @param dim_semantics Original named semantic contracts.
#' @param before_dimnames,after_dimnames Original and transformed named labels.
#' @param non_partition_dims Special statistical axes that must not become partitions.
#' @return Aligned semantic contracts with pruned applicability and declarations.
#' @noRd
subset_dim_semantics <- function(dim_semantics, before_dimnames, after_dimnames,
                                 non_partition_dims = character()) {
  before_names <- names(before_dimnames)
  after_names <- names(after_dimnames)
  out <- dim_semantics[after_names]
  if (!identical(before_names[before_names %in% after_names], after_names)) {
    names(out) <- after_names
  }
  out <- lapply(out, pa_subset_applicability,
                before_dimnames = before_dimnames, after_dimnames = after_dimnames)
  out <- lapply(out, function(sem) {
    labels <- after_dimnames[[sem@dim_name]]
    declared <- sem@overlap_levels
    retained <- intersect(declared, labels)
    if (identical(declared, retained)) return(sem)
    # Only infer disjointness for a true subset, using the original declaration
    # before an empty overlap list would acquire the meaning "unknown".
    safe_subset <- !sem@dim_name %in% non_partition_dims && !length(retained) &&
      all(labels %in% before_dimnames[[sem@dim_name]]) &&
      !pa_dim_has_overlap_risk(sem, labels, after_dimnames)
    pa_update_dim_semantics(sem,
      overlap_levels = retained,
      partition_type = if (safe_subset) "partition" else sem@partition_type)
  })
  out
}


# Indexing operator ----------------------------------------------------
#
# Subset a poparray
#
# Subsets the poparray object. By default (`drop = FALSE`) the result remains
# a `poparray`. If `drop = TRUE` and sub-setting drops either roles of  time or area dimensions , the
# method returns the underlying sub-setted `DelayedArray` instead of a `poparray`.
#
# @param x A poparray.
# @param ... Indices, either positional (like base arrays) or named by dimension (e.g., `x[year = "2020", sex =
#   "Female"]`). Missing indices in positional form are treated as `TRUE` (select all).
# @param drop Logical; passed to the backend `[` call.
#
#' Subset a demographic array lazily
#'
#' Subsets a `poparray` while preserving dimension metadata and roles whenever the resulting object still contains both
#' the time and area dimensions. The method defaults to `drop = FALSE`, so selecting a single time or area level does
#' not silently strip the `poparray` class.
#'
#' Named subscripts are supported and are matched against dimension names.
#' Applicability ranges and levels are trimmed using metadata only. If dropping
#' a dimension removes an applicability controller, the raw delayed subset is
#' returned because the remaining schemas cannot be interpreted as a `poparray`.
#'
#' @param x A `demographic_array` subclass.
#' @param ... Indices, either positional (like base arrays) or named by dimension (for example `x[year = "2020", sex =
#'   "Female"]`). Missing positional indices are treated as `TRUE`.
#' @param drop Logical; passed to the backend `[` call. Defaults to `FALSE`.
#'
#' @return A `poparray` when both role dimensions remain after subsetting; otherwise the underlying delayed subset.
#' @name subset-poparray
#' @aliases [,poparray-method [,demographic_array-method [,poparray_projection-method
#' @details Shared reconstruction retains the concrete class and metadata.
#' Projection arrays retain their class while `stat` remains. Dropping only a
#' singleton projection statistic can return a count array; dropping a standard
#' error statistic returns the raw delayed result. Required roles and applicable
#' controller axes must remain for a demographic result to be returned.
NULL

# Subset helpers and methods ----------------------------------------------------------------

#' Evaluate captured subset expressions for a poparray
#'
#' Evaluates captured `[` arguments in the caller environment while preserving
#' omitted indices as `TRUE`, which the backend interprets as "keep all labels".
#'
#' @param exprs List of unevaluated subset expressions.
#' @param env Environment in which to evaluate the expressions.
#'
#' @return List of evaluated subset indices.
#' @keywords internal
evaluate_poparray_subset_exprs <- function(exprs, env) {
  lapply(exprs, function(expr) {
    if (identical(expr, quote(expr = ))) TRUE else eval(expr, env)
  })
}

#' Resolve poparray subset indices to full positional form
#'
#' Converts a mixture of positional and named-dimension subset indices into a
#' full-length positional index list aligned with the array dimension order.
#'
#' @param idx Evaluated subset indices.
#' @param idx_names Optional names for `idx`, used to match named dimensions.
#' @param dim_order Character vector of dimension names in array order.
#'
#' @return Full-length list of backend-ready positional indices.
#' @keywords internal
resolve_poparray_subset_indices <- function(idx, idx_names, dim_order) {
  nd <- length(dim_order)
  full_idx <- rep(list(TRUE), nd)
  used <- rep(FALSE, nd)
  next_pos <- 1L

  for (k in seq_along(idx)) {
    nm <- idx_names[[k]] %||% ""

    if (nzchar(nm)) {
      pos <- match(nm, dim_order)
      if (is.na(pos)) {
        cli::cli_abort(c(
          "Unknown dimension name in subset: {.val {nm}}.",
          "i" = "Valid dimensions are: {paste(dim_order, collapse = ', ')}."
        ))
      }
    } else {
      while (next_pos <= nd && used[[next_pos]]) {
        next_pos <- next_pos + 1L
      }
      if (next_pos > nd) {
        cli::cli_abort("Too many indices for {.cls poparray}.")
      }
      pos <- next_pos
    }

    if (used[[pos]]) {
      cli::cli_abort("Duplicate index supplied for dimension {.val {dim_order[[pos]]}}.")
    }

    full_idx[[pos]] <- idx[[k]]
    used[[pos]] <- TRUE
  }

  full_idx
}

#' Derive subset dimnames without consulting the backend
#'
#' Computes the post-subset dimnames from the original labels and the resolved
#' subset indices. This avoids asking a subsetted `DelayedArray` for
#' `dimnames()`, which can trigger HDF5 metadata reads.
#'
#' @param before_dimnames Named list of dimnames before subsetting.
#' @param full_idx Full-length positional subset index list.
#' @param drop Logical indicating whether length-1 dimensions should be dropped.
#'
#' @return Named list of dimnames after subsetting.
#' @keywords internal
subset_dimnames_from_index <- function(before_dimnames, full_idx, drop = FALSE) {

  out_label <- function(labs, sel){
    switch(
      typeof(sel),
      "NULL"      = labs,
      "character" = labs[match(sel, labs)],
      "logical"   = if (isTRUE(sel)) labs else labs[sel],
      "integer"   = labs[sel],
      "double"    = labs[sel],
      cli::cli_abort("Unsupported subset index type in subset_dimnames_from_index().")
    )
  }

  out <- purrr::map2(before_dimnames, full_idx, out_label)

  if (isTRUE(drop)) {
    out <- out[lengths(out) != 1L]
  }

  out
}

#' Rewrap a subsetted delayed backend as a poparray when roles remain
#'
#' Reconstructs a `poparray` from a subsetted delayed backend using already
#' computed metadata. The helper returns the raw delayed result when role
#' dimensions were dropped.
#'
#' @param x Source `poparray`.
#' @param out Subsetted delayed backend.
#' @param before_dimnames Named list of pre-subset dimnames.
#' @param after_dimnames Named list of post-subset dimnames computed in memory.
#' @param statistic Selected projection statistic if its axis was dropped.
#'
#' @return A `poparray` when both role dimensions remain; otherwise `out`.
#' @keywords internal
wrap_subset_poparray <- function(x, out, before_dimnames, after_dimnames, statistic = NULL) {
  if (!is(out, "DelayedArray")) {
    return(out)
  }

  if (is.null(after_dimnames) || is.null(names(after_dimnames))) {
    return(out)
  }
  if (!x@time_role %in% names(after_dimnames) || !x@area_role %in% names(after_dimnames)) {
    return(out)
  }
  controllers <- vapply(x@dim_semantics[names(after_dimnames)], function(sem) {
    applicability <- pa_dim_applicability(sem)
    if (is.null(applicability)) "" else applicability$by
  }, character(1))
  if (any(nzchar(controllers) & !controllers %in% names(after_dimnames))) return(out)

  updated_dim_semantics <- subset_dim_semantics(
    dim_semantics = x@dim_semantics,
    before_dimnames = before_dimnames,
    after_dimnames = after_dimnames,
    non_partition_dims = da_special_axes(x)
  )

  da_reconstruct(
    x = x, backend = out,
    dimnames_list = after_dimnames,
    time_dim = x@time_role,
    area_dim = x@area_role,
    dim_semantics = updated_dim_semantics,
    group_vars = if (is(x, "poparray")) intersect(x@group_vars, names(after_dimnames)) else character(),
    statistic = statistic
  )
}


## `[` operator for poparray -------------
setMethod(
  "[",
  signature(x = "demographic_array", i = "ANY", j = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    validate_demographic_array(x)
    before_dimnames <- dimnames(x)
    dim_order <- names(before_dimnames)

    h <- methods::as(x, "DelayedArray")
    if(is.null(dimnames(h))) dimnames(h) <- before_dimnames

    extra_dots <- as.list(substitute(list(...)))[-1L]
    extra_names <- names(extra_dots) %||% rep("", length(extra_dots))
    has_named_extras <- any(nzchar(extra_names))

    if (isTRUE(has_named_extras)) {
      subset_exprs <- extra_dots
      if (!missing(i)) subset_exprs <- c(list(substitute(i)), subset_exprs)
      if (!missing(j)) {
        subset_exprs <- append(subset_exprs, list(substitute(j)), after = as.integer(!missing(i)))
      }
    } else {
      subset_exprs <- as.list(substitute(list(i, j, ...)))[-1L]
    }

    idx_names <- names(subset_exprs) %||% rep("", length(subset_exprs))
    idx <- evaluate_poparray_subset_exprs(subset_exprs, parent.frame())
    full_idx <- resolve_poparray_subset_indices(
      idx = idx,
      idx_names = idx_names,
      dim_order = dim_order
    )
    after_dimnames <- subset_dimnames_from_index(
      before_dimnames = before_dimnames,
      full_idx = full_idx,
      drop = drop
    )

    out <- do.call(`[`, c(list(h), full_idx, list(drop = drop)))

    wrap_subset_poparray(
      x = x,
      out = out,
      before_dimnames = before_dimnames,
      after_dimnames = after_dimnames,
      statistic = subset_dimnames_from_index(before_dimnames, full_idx, drop = FALSE)[["stat"]]
    )
  }
)
