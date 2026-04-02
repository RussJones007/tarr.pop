# -------------------------------------------------------------------------------------->
# Script: cube_metadata_admin.R
# Description:
#   Controlled metadata-admin helpers for cube metadata editing.
# -------------------------------------------------------------------------------------->

tarr_pop_metadata_role <- function() {
  opt <- getOption("tarr.pop.metadata_role")
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    return(tolower(opt))
  }
  tolower(Sys.getenv("TARR_POP_METADATA_ROLE", unset = "reader"))
}

require_cube_metadata_admin <- function(action = "edit cube metadata") {
  role <- tarr_pop_metadata_role()
  if (!role %in% c("admin", "editor")) {
    cli::cli_abort(c(
      "Insufficient role for {.val {action}}.",
      "i" = "Set option {.code tarr.pop.metadata_role = 'admin'} or env var {.envvar TARR_POP_METADATA_ROLE=admin}."
    ))
  }
  invisible(role)
}

normalize_cube_roles <- function(roles, dim_order) {
  if (!is.list(roles)) {
    cli::cli_abort("{.arg roles} must be a named list with {.field time} and {.field area}.")
  }
  time_dim <- as.character(roles$time %||% NA_character_)[1L]
  area_dim <- as.character(roles$area %||% NA_character_)[1L]
  strata <- roles$strata %||% setdiff(dim_order, c(time_dim, area_dim))
  strata <- as.character(strata)

  if (is.na(time_dim) || !nzchar(time_dim) || !time_dim %in% dim_order) {
    cli::cli_abort("{.arg roles$time} must be one of the cube dimension names.")
  }
  if (is.na(area_dim) || !nzchar(area_dim) || !area_dim %in% dim_order) {
    cli::cli_abort("{.arg roles$area} must be one of the cube dimension names.")
  }
  if (identical(time_dim, area_dim)) {
    cli::cli_abort("{.arg roles$time} and {.arg roles$area} must be different.")
  }
  strata <- unique(strata[strata %in% setdiff(dim_order, c(time_dim, area_dim))])

  list(time = time_dim, area = area_dim, strata = strata)
}

write_cube_roles_metadata <- function(path, roles) {
  pa_h5_write_dataset(path, "cube/metadata/roles/time", as.character(roles$time))
  pa_h5_write_dataset(path, "cube/metadata/roles/area", as.character(roles$area))
  pa_h5_write_dataset(path, "cube/metadata/roles/strata", as.character(roles$strata))
  invisible(TRUE)
}

normalize_cube_source <- function(source) {
  src <- pa_normalize_source(source)
  list(
    note = as.character(src$note),
    source = as.character(src$source),
    updated = as.character(src$updated),
    population_type = as.character(src$population_type)
  )
}

write_cube_source_metadata <- function(path, source) {
  src <- normalize_cube_source(source)
  pa_h5_write_dataset(path, "cube/metadata/source/note", src$note)
  pa_h5_write_dataset(path, "cube/metadata/source/source", src$source)
  pa_h5_write_dataset(path, "cube/metadata/source/updated", src$updated)
  pa_h5_write_dataset(path, "cube/metadata/source/population_type", src$population_type)
  invisible(TRUE)
}

#' Read dim_semantics metadata from a cube file
#'
#' Reads the canonical `cube/metadata/dim_semantics/*/*` metadata from an HDF5
#' cube without constructing a full `poparray`.
#'
#' @param path HDF5 file path.
#' @param validate Logical; when `TRUE` (default), validate the returned
#'   semantics against the cube's dimension order and roles.
#'
#' @return Named list of `DimSemantics` entries.
#' @export
read_cube_dim_semantics <- function(path, validate = TRUE) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  roles <- read_roles_from_cube(path, meta = meta)
  out <- read_dim_semantics_from_cube(
    path = path,
    dim_order = meta$dim_order,
    time_dim = roles$time,
    area_dim = roles$area,
    meta = meta
  )

  if (isTRUE(validate)) {
    validate_dim_semantics(
      dim_semantics = out,
      dim_names = meta$dim_order,
      time_dim = roles$time,
      area_dim = roles$area
    )
  }

  out
}

#' Write dim_semantics metadata to a cube file
#'
#' Overwrites the canonical `cube/metadata/dim_semantics/*/*` tree in an
#' existing HDF5 cube. This is an admin-only metadata operation and does not
#' rewrite `cube/population`.
#'
#' @param path HDF5 file path.
#' @param dim_semantics Named list of `DimSemantics` entries.
#' @param validate Logical; when `TRUE` (default), validate `dim_semantics`
#'   against the cube's dimension order and roles before writing.
#'
#' @return Invisibly returns the normalized cube path.
#' @export
write_cube_dim_semantics <- function(path, dim_semantics, validate = TRUE) {
  require_cube_metadata_admin("write dim_semantics metadata")
  checkmate::assert_string(path, min.chars = 1)

  meta <- get_cube_metadata_cached(path)
  roles <- read_roles_from_cube(path, meta = meta)
  dsem <- ensure_dim_semantics(
    dim_semantics = dim_semantics,
    dim_names = meta$dim_order,
    time_dim = roles$time,
    area_dim = roles$area
  )

  if (isTRUE(validate)) {
    validate_dim_semantics(
      dim_semantics = dsem,
      dim_names = meta$dim_order,
      time_dim = roles$time,
      area_dim = roles$area
    )
  }

  try(rhdf5::h5delete(path, "cube/metadata/dim_semantics"), silent = TRUE)
  pa_write_dim_semantics_fieldwise(
    filepath = path,
    dim_semantics = dsem,
    dim_order = meta$dim_order
  )
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Edit dim_semantics metadata in a cube file
#'
#' Reads `dim_semantics`, applies `FUN`, validates the result, and writes the
#' updated semantics back to the cube. This is an admin-only metadata
#' operation.
#'
#' @param path HDF5 file path.
#' @param FUN Function called as `FUN(dim_semantics, ...)`.
#' @param ... Additional arguments passed to `FUN`.
#' @param validate Logical; when `TRUE` (default), validate the updated
#'   semantics before writing.
#'
#' @return Invisibly returns the updated `dim_semantics` list.
#' @export
edit_cube_dim_semantics <- function(path, FUN, ..., validate = TRUE) {
  require_cube_metadata_admin("edit dim_semantics metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_function(FUN)

  current <- read_cube_dim_semantics(path, validate = validate)
  updated <- FUN(current, ...)
  if (!is.list(updated) || is.null(names(updated))) {
    cli::cli_abort("{.arg FUN} must return a named list of DimSemantics entries.")
  }
  write_cube_dim_semantics(path, updated, validate = validate)
  invisible(updated)
}

#' Read roles metadata from a cube file
#'
#' Reads the canonical `cube/metadata/roles/*` metadata from an HDF5 cube.
#'
#' @param path HDF5 file path.
#'
#' @return Named list with `time`, `area`, and `strata`.
#' @export
read_cube_roles <- function(path) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  normalize_cube_roles(read_roles_from_cube(path, meta = meta), meta$dim_order)
}

#' Write roles metadata to a cube file
#'
#' Overwrites the canonical `cube/metadata/roles/*` datasets in an existing HDF5
#' cube. This is an admin-only metadata operation and does not rewrite
#' `cube/population`.
#'
#' @param path HDF5 file path.
#' @param roles Named list with `time`, `area`, and optional `strata`.
#'
#' @return Invisibly returns the normalized cube path.
#' @export
write_cube_roles <- function(path, roles) {
  require_cube_metadata_admin("write cube roles metadata")
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  roles <- normalize_cube_roles(roles, meta$dim_order)
  write_cube_roles_metadata(path, roles)
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Edit roles metadata in a cube file
#'
#' Reads roles metadata, applies `FUN`, validates the result, and writes the
#' updated roles back to the cube. This is an admin-only metadata operation.
#'
#' @param path HDF5 file path.
#' @param FUN Function called as `FUN(roles, ...)`.
#' @param ... Additional arguments passed to `FUN`.
#'
#' @return Invisibly returns the updated roles list.
#' @export
edit_cube_roles <- function(path, FUN, ...) {
  require_cube_metadata_admin("edit cube roles metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_function(FUN)
  current <- read_cube_roles(path)
  updated <- FUN(current, ...)
  write_cube_roles(path, updated)
  invisible(updated)
}

#' Read source metadata from a cube file
#'
#' Reads the canonical `cube/metadata/source/*` metadata from an HDF5 cube.
#'
#' @param path HDF5 file path.
#'
#' @return Named list of source/provenance fields.
#' @export
read_cube_source <- function(path) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  normalize_cube_source(read_source_from_cube(path, meta = meta))
}

#' Write source metadata to a cube file
#'
#' Overwrites the canonical `cube/metadata/source/*` datasets in an existing
#' HDF5 cube. This is an admin-only metadata operation and does not rewrite
#' `cube/population`.
#'
#' @param path HDF5 file path.
#' @param source Named list or named atomic vector of source/provenance fields.
#'
#' @return Invisibly returns the normalized cube path.
#' @export
write_cube_source <- function(path, source) {
  require_cube_metadata_admin("write cube source metadata")
  checkmate::assert_string(path, min.chars = 1)
  write_cube_source_metadata(path, source)
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Edit source metadata in a cube file
#'
#' Reads source metadata, applies `FUN`, normalizes the result, and writes the
#' updated source metadata back to the cube. This is an admin-only metadata
#' operation.
#'
#' @param path HDF5 file path.
#' @param FUN Function called as `FUN(source, ...)`.
#' @param ... Additional arguments passed to `FUN`.
#'
#' @return Invisibly returns the updated source metadata list.
#' @export
edit_cube_source <- function(path, FUN, ...) {
  require_cube_metadata_admin("edit cube source metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_function(FUN)
  current <- read_cube_source(path)
  updated <- FUN(current, ...)
  write_cube_source(path, updated)
  invisible(updated)
}

#' Read data column metadata from a cube file
#'
#' Reads the canonical `cube/metadata/data_col` value from an HDF5 cube.
#'
#' @param path HDF5 file path.
#'
#' @return Length-1 character scalar.
#' @export
read_cube_data_col <- function(path) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  as.character(read_data_col_from_cube(path, meta = meta))
}

#' Write data column metadata to a cube file
#'
#' Overwrites the canonical `cube/metadata/data_col` dataset in an existing HDF5
#' cube. This is an admin-only metadata operation and does not rewrite
#' `cube/population`.
#'
#' @param path HDF5 file path.
#' @param data_col Length-1 character scalar naming the value column.
#'
#' @return Invisibly returns the normalized cube path.
#' @export
write_cube_data_col <- function(path, data_col) {
  require_cube_metadata_admin("write cube data_col metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_string(data_col, min.chars = 1)
  pa_h5_write_dataset(path, "cube/metadata/data_col", as.character(data_col))
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Edit data column metadata in a cube file
#'
#' Reads `data_col`, applies `FUN`, validates the result, and writes the updated
#' value back to the cube. This is an admin-only metadata operation.
#'
#' @param path HDF5 file path.
#' @param FUN Function called as `FUN(data_col, ...)`.
#' @param ... Additional arguments passed to `FUN`.
#'
#' @return Invisibly returns the updated data column name.
#' @export
edit_cube_data_col <- function(path, FUN, ...) {
  require_cube_metadata_admin("edit cube data_col metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_function(FUN)
  current <- read_cube_data_col(path)
  updated <- FUN(current, ...)
  if (!is.character(updated) || length(updated) != 1L || !nzchar(updated)) {
    cli::cli_abort("{.arg FUN} must return a non-empty character(1) for {.arg data_col}.")
  }
  write_cube_data_col(path, updated)
  invisible(updated)
}

normalize_cube_metadata_bundle <- function(metadata, dim_order) {
  if (!is.list(metadata)) {
    cli::cli_abort("{.arg metadata} must be a named list.")
  }

  out <- list(
    roles = normalize_cube_roles(metadata$roles, dim_order),
    source = normalize_cube_source(metadata$source),
    data_col = {
      dc <- metadata$data_col
      if (!is.character(dc) || length(dc) != 1L || !nzchar(dc)) {
        cli::cli_abort("{.arg metadata$data_col} must be a non-empty character(1).")
      }
      as.character(dc)
    }
  )

  out$dim_semantics <- ensure_dim_semantics(
    dim_semantics = metadata$dim_semantics,
    dim_names = dim_order,
    time_dim = out$roles$time,
    area_dim = out$roles$area
  )
  validate_dim_semantics(
    dim_semantics = out$dim_semantics,
    dim_names = dim_order,
    time_dim = out$roles$time,
    area_dim = out$roles$area
  )
  out
}

#' Read canonical cube metadata as one bundle
#'
#' Reads the canonical metadata fields used to construct a `poparray` from an
#' HDF5 cube: roles, source, `data_col`, and `dim_semantics`.
#'
#' @param path HDF5 file path.
#' @param validate Logical; when `TRUE` (default), validate the bundled
#'   metadata for cross-field consistency.
#'
#' @return Named list with `roles`, `source`, `data_col`, and `dim_semantics`.
#' @export
read_cube_metadata_admin <- function(path, validate = TRUE) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  out <- list(
    roles = read_cube_roles(path),
    source = read_cube_source(path),
    data_col = read_cube_data_col(path),
    dim_semantics = read_cube_dim_semantics(path, validate = FALSE)
  )

  if (isTRUE(validate)) {
    out <- normalize_cube_metadata_bundle(out, meta$dim_order)
  }

  out
}

#' Write canonical cube metadata as one bundle
#'
#' Writes roles, source, `data_col`, and `dim_semantics` back to an existing HDF5
#' cube as one validated metadata transaction. This is an admin-only metadata
#' operation and does not rewrite `cube/population`.
#'
#' @param path HDF5 file path.
#' @param metadata Named list with `roles`, `source`, `data_col`, and
#'   `dim_semantics`.
#' @param validate Logical; when `TRUE` (default), validate the bundled
#'   metadata for cross-field consistency before writing.
#'
#' @return Invisibly returns the normalized cube path.
#' @export
write_cube_metadata_admin <- function(path, metadata, validate = TRUE) {
  require_cube_metadata_admin("write bundled cube metadata")
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)

  bundled <- if (isTRUE(validate)) {
    normalize_cube_metadata_bundle(metadata, meta$dim_order)
  } else {
    metadata
  }

  write_cube_roles_metadata(path, bundled$roles)
  write_cube_source_metadata(path, bundled$source)
  pa_h5_write_dataset(path, "cube/metadata/data_col", as.character(bundled$data_col))
  try(rhdf5::h5delete(path, "cube/metadata/dim_semantics"), silent = TRUE)
  pa_write_dim_semantics_fieldwise(
    filepath = path,
    dim_semantics = bundled$dim_semantics,
    dim_order = meta$dim_order
  )
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Edit canonical cube metadata as one bundle
#'
#' Reads the canonical metadata bundle, applies `FUN`, validates the result, and
#' writes the updated metadata back to the cube. This is an admin-only metadata
#' operation.
#'
#' @param path HDF5 file path.
#' @param FUN Function called as `FUN(metadata, ...)`.
#' @param ... Additional arguments passed to `FUN`.
#' @param validate Logical; when `TRUE` (default), validate the bundled
#'   metadata for cross-field consistency before writing.
#'
#' @return Invisibly returns the updated metadata bundle.
#' @export
edit_cube_metadata_admin <- function(path, FUN, ..., validate = TRUE) {
  require_cube_metadata_admin("edit bundled cube metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_function(FUN)

  current <- read_cube_metadata_admin(path, validate = validate)
  updated <- FUN(current, ...)
  if (!is.list(updated)) {
    cli::cli_abort("{.arg FUN} must return a named metadata list.")
  }
  write_cube_metadata_admin(path, updated, validate = validate)
  invisible(updated)
}
