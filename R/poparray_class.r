# -------------------------------------------------------------------------------------->
# Script: poparray_classr
# Description:
#   Constrcotr and methods for generics for the poparray class.  This is a refactoring of the older tarr_pop class
#   with more specific contracted behavior enforing a time and area dimesions allowing for other dimseions
#   to be added to an array as options. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: `Feb 3, 2026
# Revised: Feb 16, 2026 - multiple bug fixes nd term updates
# -------------------------------------------------------------------------------------->

setClass(
  "poparray",
  contains = "DelayedArray",
  slots = c(
    time_role = "character",
    area_role = "character",
    strata_roles = "character",
    dim_semantics = "list",
    data_col = "character",
    source = "list"
  )
)

#' Poparray class
#'
#' S4 class for population arrays or often referred to as cubes. `poparray` extends the Bioconductor `DelayedArray` and
#' adds explicit role and metadata slots. Memory is saved and speed increased by opening a poparray cube from disk and
#' manipulating the dimensions/metadata before realizing (loading) the selected array/cube in memory.  Typical work flow is
#' to open the cube via the [open_poparray()] function, filtering the dimension levels that are needed through the `[`
#' index operator or filter() function.  Dimension levels can also be collapsed using [collapse_dim()].  The cube can
#' then be relaized [as.array()], [as.data.frame()], or [tibble::as_tibble()].
#'
#' @slot time_role Name of the time dimension.
#' @slot area_role Name of the area dimension.
#' @slot strata_roles Character vector of optional stratification dimensions.
#' @slot dim_semantics Named list of `DimSemantics` objects (one per dimension).
#' @slot data_col Name used for value column in tabular coercions.
#' @slot source Provenance metadata as a named list.
#'
#' @name poparray
#' @docType class
NULL

#' Validate dim semantics contract
#'
#' @param dim_semantics Named list of per-dimension `DimSemantics` objects.
#' @param dim_names Character vector of dimension names.
#' @param time_dim Time dimension name.
#' @param area_dim Area dimension name.
#'
#' @return Invisibly TRUE, otherwise errors.
#' @keywords internal
validate_dim_semantics <- function(dim_semantics, dim_names, time_dim, area_dim) {
  if (is.null(dim_semantics) || !is.list(dim_semantics) || is.null(names(dim_semantics))) {
    cli::cli_abort("{.arg dim_semantics} must be a named list.")
  }

  if (!identical(as.character(names(dim_semantics)), as.character(dim_names))) {
    cli::cli_abort(
      "{.arg dim_semantics} names must exactly match names(dim(x)) in the same order."
    )
  }

  for (nm in dim_names) {
    ent <- dim_semantics[[nm]]
    if (is.null(ent) || !S7::S7_inherits(ent, DimSemantics)) {
      cli::cli_abort(
        "dim_semantics entry for dimension {.val {nm}} must inherit from {.cls DimSemantics}."
      )
    }
    if (!identical(ent@dim_name, nm)) {
      cli::cli_abort("dim_semantics[{.val {nm}}]@dim_name must equal {.val {nm}}.")
    }
  }

  for (required_partition in c(time_dim, area_dim)) {
    if (!pa_is_partition(dim_semantics[[required_partition]])) {
      cli::cli_abort(
        "Role dimension {.val {required_partition}} must have partition_type = {.val partition}."
      )
    }
  }

  invisible(TRUE)
}

pa_default_dim_domain <- function(dim_name, time_dim, area_dim) {
  if (identical(dim_name, time_dim)) return("time")
  if (identical(dim_name, area_dim)) return("area")
  dim_name
}

pa_default_dim_scale_type <- function(dim_name, time_dim) {
  if (identical(dim_name, time_dim)) return("interval")
  "nominal"
}

pa_as_dim_semantics_entry <- function(entry, dim_name, time_dim, area_dim) {
  default <- new_dim_semantics(
    dim_name = dim_name,
    domain = pa_default_dim_domain(dim_name, time_dim, area_dim),
    scale_type = pa_default_dim_scale_type(dim_name, time_dim),
    partition_type = if (dim_name %in% c(time_dim, area_dim)) "partition" else "set",
    validated = dim_name %in% c(time_dim, area_dim),
    overlap_levels = character(),
    notes = character()
  )

  if (S7::S7_inherits(entry, DimSemantics)) {
    if (!identical(entry@dim_name, dim_name)) {
      return(pa_update_dim_semantics(entry, dim_name = dim_name))
    }
    return(entry)
  }

  if (is.null(entry) || !is.list(entry)) {
    return(default)
  }

  if (all(c("domain", "scale_type", "partition_type", "validated") %in% names(entry))) {
    return(new_dim_semantics(
      dim_name = dim_name,
      domain = as.character(entry$domain)[[1L]],
      scale_type = as.character(entry$scale_type)[[1L]],
      partition_type = as.character(entry$partition_type)[[1L]],
      validated = as.logical(entry$validated)[[1L]],
      overlap_levels = as.character(entry$overlap_levels %||% character()),
      notes = as.character(entry$notes %||% character())
    ))
  }

  if ("class" %in% names(entry)) {
    cls <- as.character(entry$class)[[1L]]
    pt <- switch(cls, partition = "partition", set = "set", "unknown")
    return(new_dim_semantics(
      dim_name = dim_name,
      domain = pa_default_dim_domain(dim_name, time_dim, area_dim),
      scale_type = pa_default_dim_scale_type(dim_name, time_dim),
      partition_type = pt,
      validated = as.logical(entry$validated %||% FALSE)[[1L]],
      overlap_levels = character(),
      notes = character()
    ))
  }

  default
}

default_dim_semantics <- function(dim_names, time_dim, area_dim) {
  out <- lapply(dim_names, function(d) {
    new_dim_semantics(
      dim_name = d,
      domain = pa_default_dim_domain(d, time_dim, area_dim),
      scale_type = pa_default_dim_scale_type(d, time_dim),
      partition_type = if (d %in% c(time_dim, area_dim)) "partition" else "set",
      validated = d %in% c(time_dim, area_dim),
      overlap_levels = character(),
      notes = character()
    )
  })
  names(out) <- dim_names
  out
}

ensure_dim_semantics <- function(dim_semantics, dim_names, time_dim, area_dim) {
  defaults <- default_dim_semantics(dim_names, time_dim, area_dim)
  if (is.null(dim_semantics) || !is.list(dim_semantics)) {
    return(defaults)
  }
  in_names <- names(dim_semantics)
  if (is.null(in_names)) {
    return(defaults)
  }
  out <- defaults
  for (nm in intersect(dim_names, in_names)) {
    out[[nm]] <- pa_as_dim_semantics_entry(
      entry = dim_semantics[[nm]],
      dim_name = nm,
      time_dim = time_dim,
      area_dim = area_dim
    )
  }
  out
}

subset_dim_semantics <- function(dim_semantics, before_dimnames, after_dimnames) {
  before_names <- names(before_dimnames)
  after_names <- names(after_dimnames)
  out <- dim_semantics[after_names]
  if (!identical(before_names[before_names %in% after_names], after_names)) {
    names(out) <- after_names
  }
  out
}

is_hdf5_backed_delayed <- function(x) {
  if (!is(x, "DelayedArray")) return(FALSE)
  if (is(x, "HDF5Array")) return(TRUE)
  sd <- tryCatch(DelayedArray::seed(x), error = function(e) NULL)
  seed_has_hdf5 <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is(obj, "HDF5ArraySeed") || is(obj, "HDF5Array")) return(TRUE)
    if (isS4(obj)) {
      for (sn in methods::slotNames(obj)) {
        v <- methods::slot(obj, sn)
        if (seed_has_hdf5(v)) return(TRUE)
      }
    }
    if (is.list(obj)) {
      for (v in obj) {
        if (seed_has_hdf5(v)) return(TRUE)
      }
    }
    FALSE
  }
  seed_has_hdf5(sd)
}

validate_hdf5_metadata_shape <- function(x) {
  sd <- tryCatch(DelayedArray::seed(x), error = function(e) NULL)
  if (is.null(sd) || !is(sd, "HDF5ArraySeed")) return(TRUE)
  fp <- tryCatch(sd@filepath, error = function(e) "")
  ds <- tryCatch(sd@name, error = function(e) "")
  if (!nzchar(fp) || !file.exists(fp) || !nzchar(ds)) return(TRUE)
  if (!grepl("^cube/population$", ds) && !grepl("^/cube/population$", ds)) return(TRUE)
  info <- tryCatch(rhdf5::h5ls(fp), error = function(e) NULL)
  if (is.null(info)) return("Unable to inspect HDF5 metadata layout.")
  has_meta_group <- any(info$group == "/cube" & info$name == "metadata")
  has_dim_order <- any(info$group == "/cube/metadata" & info$name == "dim_order")
  has_dimnames_group <- any(info$group == "/cube/metadata" & info$name == "dimnames")
  if (!has_meta_group) return("Missing required HDF5 metadata group: cube/metadata")
  if (!has_dim_order) return("Missing required HDF5 dataset: cube/metadata/dim_order")
  if (!has_dimnames_group) return("Missing required HDF5 group: cube/metadata/dimnames")
  dim_order <- as.character(rhdf5::h5read(fp, "cube/metadata/dim_order"))
  dn <- dimnames(x)
  nms <- names(dn)
  if (is.null(nms) || !identical(as.character(dim_order), as.character(nms))) {
    return("HDF5 cube/metadata/dim_order does not match object dimension names.")
  }
  TRUE
}

setValidity("poparray", function(object) {
  dn <- dimnames(object)
  if (is.null(dn) || is.null(names(dn))) {
    return("dimnames must exist and be named.")
  }
  if (length(object@time_role) != 1L || !nzchar(object@time_role)) {
    return("slot 'time_role' must be a single non-empty character string.")
  }
  if (length(object@area_role) != 1L || !nzchar(object@area_role)) {
    return("slot 'area_role' must be a single non-empty character string.")
  }
  if (identical(object@time_role, object@area_role)) {
    return("time_role and area_role must be different.")
  }
  if (!object@time_role %in% names(dn)) {
    return("time_role must match a dimension name.")
  }
  if (!object@area_role %in% names(dn)) {
    return("area_role must match a dimension name.")
  }
  if (anyDuplicated(c(object@time_role, object@area_role, object@strata_roles)) > 0) {
    return("Roles cannot contain duplicates.")
  }
  if (is.null(object@dim_semantics) || !is.list(object@dim_semantics) || is.null(names(object@dim_semantics))) {
    return("slot 'dim_semantics' must be a named list.")
  }
  if (!identical(names(object@dim_semantics), names(dn))) {
    return("slot 'dim_semantics' names must exactly match dimension names.")
  }
  for (nm in names(dn)) {
    ent <- object@dim_semantics[[nm]]
    if (!S7::S7_inherits(ent, DimSemantics)) {
      return("slot 'dim_semantics' entries must inherit from DimSemantics.")
    }
    if (!identical(ent@dim_name, nm)) {
      return("each dim_semantics entry must have @dim_name matching its dimension name.")
    }
  }
  d <- lengths(dn)
  if (length(dn) != length(d)) {
    return("dimnames must align with dimensions.")
  }
  for (k in seq_along(d)) {
    lbl <- dn[[k]]
    if (is.null(lbl) || length(lbl) != d[[k]]) {
      return("Each dimnames entry must have length matching the dimension extent.")
    }
  }
  h5_meta_chk <- validate_hdf5_metadata_shape(object)
  if (!isTRUE(h5_meta_chk)) return(h5_meta_chk)
  TRUE
})

#' Construct a poparray
#'
#' Creates a role-aware `poparray` that extends `DelayedArray` and stores role and
#' provenance metadata in slots.
#'
#' @param x A `DelayedArray` object.
#' @param dimnames_list Named list of dimension labels.
#' @param data_col Single character string giving the value-column name.
#' @param source Optional metadata describing provenance.
#' @param time_dim Single character string naming the time dimension.
#' @param area_dim Single character string naming the area dimension.
#' @param dim_semantics Named list of per-dimension `DimSemantics` objects.
#' @param validate_semantics Logical scalar. Internal control for staged
#'   migration; if `FALSE`, strict contract checks are skipped but
#'   `dim_semantics` is still populated.
#' @param ... Reserved for future use.
#'
#' @return An S4 object of class `"poparray"`.
#' @keywords internal
.new_poparray_internal <- function(x,
                                   dimnames_list = dimnames(x),
                                   data_col = "population",
                                   source = NULL,
                                   time_dim = "year",
                                   area_dim = "area.name",
                                   dim_semantics = NULL,
                                   validate_semantics = TRUE,
                                   ...) {
  if (!is(x, "DelayedArray")) {
    cli::cli_abort("{.arg x} must be a {.cls DelayedArray}.")
  }
  if (!is_hdf5_backed_delayed(x)) {
    cli::cli_abort("poparray must be backed by an HDF5Array seed.")
  }
  if (is.null(dimnames_list) || !is.list(dimnames_list) || is.null(names(dimnames_list))) {
    cli::cli_abort("{.arg dimnames_list} must be a named list of dimension labels.")
  }
  if (length(data_col) != 1 || is.na(data_col) || !is.character(data_col)) {
    cli::cli_abort("{.arg data_col} must be length 1 and not NA.")
  }
  nms <- names(dimnames_list)
  if (!is.character(time_dim) || length(time_dim) != 1 || !time_dim %in% nms) {
    cli::cli_abort("{.arg time_dim} must be a dimension name in {.arg dimnames_list}.")
  }
  if (!is.character(area_dim) || length(area_dim) != 1 || !area_dim %in% nms) {
    cli::cli_abort("{.arg area_dim} must be a dimension name in {.arg dimnames_list}.")
  }
  if (identical(time_dim, area_dim)) {
    cli::cli_abort("{.arg time_dim} and {.arg area_dim} must be different.")
  }
  if (isTRUE(validate_semantics)) {
    if (is.null(dim_semantics)) {
      cli::cli_abort("{.arg dim_semantics} is required and cannot be NULL when {.arg validate_semantics = TRUE}.")
    }
    validate_dim_semantics(
      dim_semantics = dim_semantics,
      dim_names = nms,
      time_dim = time_dim,
      area_dim = area_dim
    )
  } else {
    dim_semantics <- ensure_dim_semantics(
      dim_semantics = dim_semantics,
      dim_names = nms,
      time_dim = time_dim,
      area_dim = area_dim
    )
  }
  dimnames(x) <- dimnames_list
  src <- if (is.null(source)) list() else as.list(source)
  obj <- new(
    "poparray",
    x,
    time_role = time_dim,
    area_role = area_dim,
    strata_roles = setdiff(nms, c(time_dim, area_dim)),
    dim_semantics = dim_semantics,
    data_col = data_col,
    source = src
  )
  # Transitional compatibility for remaining S3 wrappers/shims.
  attr(obj, "data_col") <- data_col
  attr(obj, "source") <- src
  attr(obj, "dimroles") <- list(time = time_dim, area = area_dim, strata = setdiff(nms, c(time_dim, area_dim)))
  obj
}

#' Construct a poparray
#'
#' Creates a role-aware `poparray` that extends `DelayedArray` and stores role
#' and provenance metadata in slots.
#'
#' @inheritParams .new_poparray_internal
#' @return An S4 object of class `"poparray"`.
#' @export
new_poparray <- function(x,
                         dimnames_list = dimnames(x),
                         data_col = "population",
                         source = NULL,
                         time_dim = "year",
                         area_dim = "area.name",
                         dim_semantics = NULL,
                         ...) {
  if (is.null(dim_semantics)) {
    cli::cli_abort("{.arg dim_semantics} is required and cannot be NULL.")
  }
  .new_poparray_internal(
    x = x,
    dimnames_list = dimnames_list,
    data_col = data_col,
    source = source,
    time_dim = time_dim,
    area_dim = area_dim,
    dim_semantics = dim_semantics,
    validate_semantics = TRUE,
    ...
  )
}

setMethod("show", "poparray", function(object) {
  src <- get_source(object)
  dms <- dimnames(object)
  dms_sizes <- lengths(dms)
  names(dms_sizes) <- names(dms)
  dimensions <- paste(paste0(names(dms_sizes), " (", dms_sizes, ")"), collapse = ", ")
  cat("<poparray>\n")
  cat("Series: ", src[["note"]] %||% "", "\n", sep = "")
  cat("Sourced: ", src[["source"]] %||% "Not given", "\n", sep = "")
  cat("Updated: ", src[["updated"]] %||% "Unknown", "\n", sep = "")
  cat("Length: ", format(as.numeric(prod(dim(object))), big.mark = ","), "\n", sep = "")
  cat("Roles: time = '", object@time_role, "', area = '", object@area_role, "'\n", sep = "")
  cat("Dimensions: ", dimensions, "\n", sep = "")
  cat("Data column as data frame: '", object@data_col, "'\n", sep = "")
  invisible(object)
})

setMethod("collapse_dim", "poparray", collapse_dim_poparray_impl)

setMethod(
  "[",
  signature(x = "poparray"),
  function(x, ..., drop = FALSE) {
    before_dimnames <- dimnames(x)
    out <- callNextMethod()
    if (isTRUE(drop) || !is(out, "DelayedArray")) {
      return(out)
    }
    after_dimnames <- dimnames(out)
    if (is.null(after_dimnames) || is.null(names(after_dimnames))) {
      return(out)
    }
    if (!x@time_role %in% names(after_dimnames) || !x@area_role %in% names(after_dimnames)) {
      return(out)
    }
    updated_dim_semantics <- subset_dim_semantics(
      dim_semantics = x@dim_semantics,
      before_dimnames = before_dimnames,
      after_dimnames = after_dimnames
    )
    new_poparray(
      x = out,
      dimnames_list = dimnames(out),
      data_col = x@data_col,
      source = x@source,
      time_dim = x@time_role,
      area_dim = x@area_role,
      dim_semantics = updated_dim_semantics
    )
  }
)


#' Validate poparray object
#'
#' @param x a poparray object to validte
#'
#' @returns x, otherwise throws an error
#' @keywords internal
validate_poparray <- function(x) {
  if (!is(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray}.")
  }
  msg <- methods::validObject(x, test = TRUE)
  if (!isTRUE(msg)) cli::cli_abort(msg)
  dn <- dimnames(x)
  
  # Ordered time labels
  tlab <- dn[[x@time_role]] |> as.character()
  if(anyNA(tlab)) cli::cli_abort("Time dim {.val {x@time_role}} cannot have any NA values.")
  
  
  if (all(grepl("^[-+]?[0-9]+$", tlab))) {
    tnum <- as.integer(tlab)
    if (is.unsorted(tnum, strictly = FALSE)) {
      cli::cli_abort("Time dim {.val {x@time_role}} labels must be ordered (increasing).")
    }
  } else {
    if (is.unsorted(tlab, strictly = FALSE)) {
      cli::cli_abort("Time dim {.val {x@time_role}} labels must be ordered.")
    }
  }
  
  invisible(x)
}


# Attribute retrieval helpers ------------------------------------------------------------------------------------------

#' Get the time role name for a poparray
#'
#' @param x A poparray.
#' @return A single character string naming the time dimension.
#' @export
time_role <- function(x) {
  validate_poparray(x)
  return(x@time_role)
}

#' Get the area role name for a poparray
#'
#' @param x A poparray.
#' @return A single character string naming the area dimension.
#' @export
area_role <- function(x) {
  validate_poparray(x)
  return(x@area_role)
}




#' @export
is.poparray <- function(x) is(x, "poparray")


#  dim,  names, labels, and length  -----------------------

#' @export
names.poparray <- function(x) {
  names(dimnames(x))
}

#' Length of a poparray
#'
#' Returns the total number of cells in the poparray, equivalent to
#' `prod(dim(x))`. This is a metadata-only operation and does not realize the
#' delayed backend.
#'
#' @param x A poparray.
#'
#' @return A numeric scalar giving the total number of elements.
#' @export
length.poparray <- function(x) {
  d <- dim(x)
  if (is.null(d)) cli::cli_abort("poparray backend has no dimensions.")
  as.numeric(prod(d))
}


# Print and summary methods -----------------------------------------------------------------------------------------

#' Summary of a poparray (may scan backend)
#'
#' Computes basic summaries of the numeric values in a poparray. This operation
#' preserves delayed execution where possible, but it may still require scanning
#' the full backend and can be expensive for large cubes.
#'
#' @param object A poparray.
#' @param ... Passed to DelayedArray reduction functions (if used).
#'
#' @return A named numeric vector (class `summaryDefault`) similar to
#'   `summary.numeric()`.
#' @export
summary.poparray <- function(object, ...) {
  x <- object
  validate_poparray(x)
  
  h <- x
  
  # Prefer DelayedArray reductions (avoid as.array())
  # NOTE: these return small realized scalars.
  n_na <- DelayedArray::sum(is.na(h), na.rm = TRUE, ...)
  
  s <- DelayedArray::sum(h, na.rm = TRUE, ...)
  mn <- DelayedArray::min(h, na.rm = TRUE, ...)
  mx <- DelayedArray::max(h, na.rm = TRUE, ...)
  mu <- DelayedArray::mean(h, na.rm = TRUE, ...)
  
  ret <- c(
    Min. = as.numeric(mn),
    Mean = as.numeric(mu),
    Max. = as.numeric(mx),
    Sum  = as.numeric(s),
    NA.s = as.numeric(n_na)
  )
  
  class(ret) <- "summaryDefault"
  ret
}

# Indexing operator ----------------------------------------------------
# 
# Subset a poparray
#
# Subsets the poparray object and updates the stored dimnames metadata. By default (`drop = FALSE`) the result remains
# a `poparray`. If `drop = TRUE` and subsetting would drop either the `dimroles` time or area dimensions , the
# method returns the underlying subsetted `DelayedArray` instead of a `poparray`.
# 
# @param x A poparray.
# @param ... Indices, either positional (like base arrays) or named by dimension (e.g., `x[year = "2020", sex =
#   "Female"]`). Missing indices in positional form are treated as `TRUE` (select all).
# @param drop Logical; passed to the backend `[` call.
#

# Coerce to poparray' ---------------------------------------------------------------------------------------------

#' Coerce to a poparray Object
#'
#' Used to coerce a data frame or an array/table to a poparray object.
#'
#' @param x object to coerce like a data frame or a table/array.
#' @param data_col single character string with the population column name.
#' @param ... additional arguments to pass (see methods).
#'
#' @return a poparray classed object
#' @export
as.poparray <- function(x, data_col = "population", ...) {
  UseMethod("as.poparray")
}

#' @export
as.poparray.default <- function(x, data_col = "population", ...) {
  stop("No method exists for class ", paste(class(x), collapse = ", "))
}

# @export
# as.poparray.data.frame <- function(x,
#                                    data_col = "population",
#                                    backend = c("hdf5", "delayed"),
#                                    filepath = NULL,
#                                    dataset = "/pop",
#                                    chunkdim = NULL,
#                                    level = 6L,
#                                    ...) {
#   assert_that(is.scalar(data_col),
#               is.numeric(x[[data_col]]),
#               x %has_name% c("year", "area.name", "sex", "age.char",
#                              "race", "ethnicity"))
#   
#   backend <- match.arg(backend)
#   
#   # Strip unneeded columns, convert to array
#   arr <- x |>
#     dplyr::select(-dplyr::any_of(c("fips", "age.iv"))) |>
#     droplevels() |>
#     df_2_array(data_col = data_col)
#   
#   dn <- dimnames(arr)
#   
#   if (backend == "hdf5") {
#     if (is.null(filepath)) {
#       stop("For backend = 'hdf5', 'filepath' must be provided.")
#     }
#     handle <- HDF5Array::writeHDF5Array(
#       arr,
#       filepath = filepath,
#       name     = dataset,
#       chunkdim = chunkdim,
#       level    = level
#     )
#   } else {
#     handle <- DelayedArray::DelayedArray(arr)
#   }
#   
#   new_poparray(x = handle,
#                dimnames_list = dn,
#                data_col = data_col,
#                ...)
# }

#' @rdname as.poparray
#' @param filepath Optional HDF5 file path used to persist the array before
#'   constructing a `poparray`. If `NULL`, `HDF5Array` uses its default.
#' @param name Dataset path. Must be `"cube/population"` for the canonical
#'   poparray cube schema.
#' @param chunkdim Optional HDF5 chunk dimensions, or `"auto"`.
#' @param level Compression level (0-9) passed to `writeHDF5Array()`.
#' @export
as.poparray.array <- function(x,
                              data_col = "population",
                              filepath = NULL,
                              name = "cube/population",
                              chunkdim = "auto",
                              level = 6L,
                              ...) {
  checkmate::assert_character(data_col, len = 1, any.missing = FALSE)
  assert_that(is.numeric(x))
  req_dims <- c("year", "area.name")
  dn <- dimnames(x)
  if (is.null(dn) || is.null(names(dn))) {
    cli::cli_abort("{.arg x} must have named dimnames.")
  }
  missing_dims <- setdiff(req_dims, names(dn))
  if (length(missing_dims) > 0) {
    cli::cli_abort("Missing required dimensions in {.arg x}: {paste(missing_dims, collapse = ', ')}")
  }
  if (!identical(name, "cube/population")) {
    cli::cli_abort("{.arg name} must be {.val cube/population} for poparray cubes.")
  }

  dots <- list(...)
  time_dim <- dots$time_dim %||% "year"
  area_dim <- dots$area_dim %||% "area.name"
  src <- dots$source %||% list()
  if (is.null(filepath)) {
    filepath <- tempfile("poparray_cube_", fileext = ".h5")
  }

  pa_write_poparray_cube(
    x = x,
    filepath = filepath,
    dimnames_list = dn,
    overwrite = FALSE,
    chunkdim = chunkdim,
    level = level,
    time_dim = time_dim,
    area_dim = area_dim,
    source = src,
    data_col = data_col
  )

  delayed_x <- HDF5Array::HDF5Array(filepath = filepath, name = "cube/population")
  dimnames(delayed_x) <- dn

  new_poparray(x = delayed_x,
               dimnames_list = dn,
               data_col = data_col,
               source = src,
               time_dim = time_dim,
               area_dim = area_dim,
               dim_semantics = default_dim_semantics(names(dn), time_dim, area_dim),
               ...)
}



# Coere to double, data.frame, or tibble functions ------------------------------------------------------------------------

#' Coerce poparray values to double (lazy)
#'
#' Returns a poparray with the same dimensions/metadata but with the delayed
#' backend coerced to double storage where possible.
#'
#' @param x A poparray.
#' @param ... Unused.
#'
#' @return A poparray backed by a double-typed delayed array.
#' @export
as.double.poparray <- function(x, ...) {
  validate_poparray(x)
  
  h2 <- as.double(methods::as(x, "DelayedArray"))  # should remain delayed if DelayedArray supports it
  
  new_poparray(
    x = h2,
    dimnames_list = dimnames(x),
    data_col = data_col(x),
    source = get_source(x),
    time_dim = time_role(x),
    area_dim = area_role(x),
    dim_semantics = dim_semantics(x)
  )
}


#' Coerce poparray to data frame (EAGER)
#'
#' S3 method to coerce a poparray to a data frame. This method **realizes** the delayed backend (for the current
#' poparray slice) and converts it to a long data.frame via `as.table()` semantics (one row per cell).
#'
#' For large cubes, subset first (e.g., restrict years/areas) to avoid materializing an unmanageably large array.
#'
#' @param x A poparray.
#' @param stringsAsFactors Passed to `as.data.frame.table()`.
#' @param responseName Name of the value column (defaults to `data_col(x)`).
#' @param bytes_threshold is the number of bytes to warnn you shen a large data frame isbing retunrd. Default is 40 MB.
#' @param ... Passed to `as.data.frame.table()` (rarely needed).
#'
#' @return A data.frame with one column per dimension plus `responseName`.
#' @export
as.data.frame.poparray <- function(x,
                                   stringsAsFactors = TRUE,
                                   responseName = data_col(x),
                                   bytes_threshold = 40e6,
                                   ...) {
  validate_poparray(x)
  # Warn when realizing the DelayedArray
  warn_if_realization_large(x, bytes_threshold = bytes_threshold)
  
  # EAGER: materialize the current slice
  arr <- as.array(x)
  dimnames(arr) <- dimnames(x)
  
  df <- as.data.frame(
    as.table(arr),
    stringsAsFactors = stringsAsFactors,
    responseName = responseName,
    ...
  )

  polish_df(df = df, 
            stringsAsFactors = stringsAsFactors, 
            time_dim = time_role(x))
}

#' Coerce poparray to tibble (EAGER)
#'
#' Coercion is done via `as.data.frame.poparray()` for consistency, then
#' converted to a tibble.
#'
#' @param x A poparray.
#' @param stringsAsFactors Logical; passed to `as.data.frame.poparray()`.
#' @param ... Passed to `as.data.frame.poparray()`.
#' @param .name_repair Name repair strategy for tibble.
#'
#' @return A tibble.
#' @export
as_tibble.poparray <- function(x,
                               stringsAsFactors = TRUE,
                               ...,
                               .name_repair = c("check_unique", "unique",
                                                "universal", "minimal")) {
  as.data.frame(x, stringsAsFactors = stringsAsFactors, ...) |>
    tibble::as_tibble(.name_repair = .name_repair)
}




# Split a poparry -------------------------------------------------------------------------------------------------

#' Split a poparray by a dimension
#'
#' Splits a poparray into a named list of slices, one per label of a chosen
#' dimension. This method preserves laziness by subsetting the delayed backend.
#'
#' If `drop = FALSE` (default), each element is a `poparray`. If `drop = TRUE`
#' and the split dimension is the time or area role (per `dimroles`), each slice
#' will drop that role and the result elements will be a subsetted `DelayedArray`
#' objects as the time and area roles are required for a poparray.
#'
#' @param x A poparray.
#' @param f The dimension to split by: either a single dimension name (character
#'   scalar) or a single integer position.
#' @param drop Logical; whether to drop dimensions in the subset, passed to `[`.
#' @param ... Unused.
#'
#' @return A named list of poparray (or DelayedArray) slices.
#' @export
split.poparray <- function(x, f, drop = FALSE, ...) {
  validate_poparray(x)
  
  dim_names <- names(dimnames(x))
  nd <- length(dim(x))
  roles <- list(time = time_role(x), area = area_role(x))
  
  # Resolve split dimension name + position
  if (is.character(f) && length(f) == 1) {
    # role keywords
    if (identical(f, "time")) f <- roles$time
    if (identical(f, "area")) f <- roles$area
    
    if (!f %in% dim_names) {
      cli::cli_abort(c(
        "{.arg f} is not a valid dimension/role for this poparray.",
        "i" = "Use a dimension name, or one of: {.val time}, {.val area}.",
        "i" = "Valid dimensions are: {paste(dim_names, collapse = ', ')}."
      ))
    }
    split_dim <- f
    split_pos <- match(f, dim_names)
    
  } else if (is.numeric(f) && length(f) == 1) {
    split_pos <- as.integer(f)
    if (is.na(split_pos) || split_pos < 1L || split_pos > nd) {
      cli::cli_abort("{.arg f} (position) must be between 1 and {nd}.")
    }
    split_dim <- dim_names[[split_pos]]
    
  } else {
    cli::cli_abort("{.arg f} must be a single dimension name, role ('time'/'area'), or a single position.")
  }
  
  labs <- dimnames(x)[[split_dim]]
  if (length(labs) == 0) return(stats::setNames(list(), character(0)))
  
  out <- lapply(labs, \(lab) {
    ndx <- rep(list(TRUE), nd)
    ndx[[split_pos]] <- lab
    do.call(`[`, c(list(x), unname(ndx), list(drop = drop)))
  })
  
  purrr::set_names(out, labs)
}

#' Apply a function by poparray dimension groups
#'
#' Splits a `poparray` by one dimension (or role alias) and applies `FUN`
#' to each slice.
#'
#' @param data A `poparray`.
#' @param INDICES Dimension selector used for grouping. Accepts a single
#'   dimension name, role alias (`"time"` or `"area"`), or a single integer
#'   dimension position.
#' @param FUN Function applied to each grouped slice.
#' @param ... Additional arguments passed to `FUN`.
#' @param simplify Logical; if `TRUE` (default), attempt to simplify results
#'   with `base::simplify2array()`.
#' @param drop Logical; passed to [split.poparray()] and then `[.poparray` for
#'   each slice.
#'
#' @return A named list of results, or a simplified array/vector when
#'   `simplify = TRUE`.
#' @export
by.poparray <- function(data, INDICES, FUN, ..., simplify = TRUE, drop = FALSE) {
  validate_poparray(data)
  if (!is.function(FUN)) cli::cli_abort("{.arg FUN} must be a function.")
  
  slices <- split(data, f = INDICES, drop = drop)
  out <- lapply(slices, FUN, ...)
  
  if (isTRUE(simplify)) {
    return(base::simplify2array(out))
  }
  out
}


# Operators -------------------------------------------------------------------------------------------------------

#' @export
sd.poparray <- function(x, ..., na.rm = FALSE) {
  a <- methods::as(x, "DelayedArray")
  # For general DelayedArray, sd() may or may not be specialized;
  # safest is a two-pass block reduction if you need guaranteed behavior.
  # (See next section.)
  stats::sd(as.vector(a), na.rm = na.rm)
}

# Accessors / helpers ----------------------------------------------------------

#' Get or set the name of the poparray data column
#'
#' poparray objects have a `data_col` attribute that is used to name the column that holds the numeric data when
#' coercing to a data frame [as.data.frame()] or tibble [tibble::as_tibble()]. This convenience function can retrieve or
#' set that attribute.
#'
#' @param x A poparray object.
#' @return Character string.
#' @export
data_col <- function(x) {
  if (is(x, "poparray")) return(x@data_col)
  attr(x, "data_col", exact = TRUE)
}

#' Get dim semantics contract for a poparray
#'
#' Returns the read-only per-dimension semantic contract used for guarded
#' reductions and metadata persistence.
#'
#' @param x A poparray.
#' @return Named list with one `DimSemantics` object per dimension.
#' @export
dim_semantics <- function(x) {
  if (!is(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray}.")
  }
  x@dim_semantics
}

#' @rdname data_col
#' @export
`data_col<-` <- function(x, values) {
  checkmate::assert_character(values, len = 1, any.missing = FALSE)
  if (is(x, "poparray")) {
    x@data_col <- values
  }
  attr(x, "data_col") <- values
  x
}


#' Warn for large realized array
#'
#' Used for the side effect of issuing a warning when a realized array is very large.
#'
#' @param x A poparray.
#' @param bytes_threshold Threshold in bytes for warning (default ~400 MB).
#' @returns x invisibly.
#' @keywords internal
warn_if_realization_large <- function(x, bytes_threshold = 5e7 * 8) {
  validate_poparray(x)
  
  t <- tolower(DelayedArray::type(x))
  
  bytes_per_cell <- switch(
    t,
    "integer" = 4,
    "double"  = 8,
    "numeric" = 8,
    "logical" = 1,
    8 # default conservative fallback
  )
  
  n_cells <- as.numeric(prod(dim(x)))
  if (!is.finite(n_cells)) return(invisible(x))
  
  est_bytes <- n_cells * as.numeric(bytes_per_cell)
  
  if (est_bytes >= bytes_threshold) {
    est_mb <- est_bytes / 1024^2
    cli::cli_warn(c(
      "Coercing this poparray to an in-memory array is {.emph EAGER} and may use substantial memory.",
      "i" = "Cells: {format(n_cells, big.mark = ',')}.",
      "i" = "Backend type: {.val {t}} (~{bytes_per_cell} bytes/cell).",
      "i" = "Estimated realized array size: ~{format(round(est_mb, 1), nsmall = 1)} MB (array only).",
      "i" = "Consider subsetting years/areas (or another dimension) before calling as.data.frame() / as.array()."
    ))
  }
  
  invisible(x)
}

#' Polish data frames after coercing
#'
#'  Used by as.data.frame() as a suport function
#'
#' @param df 
#' @param stringsAsFactors 
#' @param time_dim 
#'
#' @returns a data frame
#' @keywords internal
#' @examples
#' #todo
polish_df <- function(df,
                      stringsAsFactors = TRUE,
                      time_dim = "year") {
  
  df <- df[stats::complete.cases(df), ] |>
    dplyr::mutate(
      dplyr::across(where(is_char_int),    char_to_int),
      dplyr::across(where(is_char_double), char_to_double)
    )
  
  if (stringsAsFactors && "age.char" %in% names(df)) {
    ages <- df[["age.char"]] |>
      (\(x) if (is.factor(x)) levels(x) else unique(x))()
    
    all_term <- ages[stringr::str_detect(
      ages, stringr::regex("all", ignore_case = TRUE)
    )]
    
    if (!is.null(ages)) {
      ages_no_all <- if (rlang::is_empty(all_term)) ages else ages[ages != all_term]
      age_ivs <- rage::as.age_group(ages_no_all) |>
        sort()
      
      age_levels <- c(as.character(age_ivs), all_term)
      df[["age.char"]] <- ordered(df[["age.char"]], levels = age_levels)
    }
  }
  
  # Order time dimension if it's a factor
  if (time_dim %in% names(df) && is.factor(df[[time_dim]])) {
    lev <- levels(df[[time_dim]])
    # if levels are integer-like, sort numerically; else sort lexicographically
    if (all(grepl("^[-+]?[0-9]+$", lev))) {
      lev2 <- sort(as.integer(lev))
      lev2 <- as.character(lev2)
    } else {
      lev2 <- sort(lev)
    }
    df[[time_dim]] <- factor(df[[time_dim]], levels = lev2, ordered = TRUE)
  }
  
  df
}
