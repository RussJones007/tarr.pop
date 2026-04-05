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

read_cube_dim_semantics_impl <- function(path, validate = TRUE) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  cube_roles <- read_roles_from_cube(path, meta = meta)
  out <- read_dim_semantics_from_cube(
    path = path,
    dim_order = meta$dim_order,
    time_dim = cube_roles$time,
    area_dim = cube_roles$area,
    meta = meta
  )

  if (isTRUE(validate)) {
    validate_dim_semantics(
      dim_semantics = out,
      dim_names = meta$dim_order,
      time_dim = cube_roles$time,
      area_dim = cube_roles$area
    )
  }

  out
}

read_cube_roles_impl <- function(path) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  normalize_cube_roles(read_roles_from_cube(path, meta = meta), meta$dim_order)
}

read_cube_source_impl <- function(path) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  normalize_cube_source(read_source_from_cube(path, meta = meta))
}

read_cube_data_col_impl <- function(path) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  as.character(read_data_col_from_cube(path, meta = meta))
}

read_cube_metadata_admin_impl <- function(path, validate = TRUE) {
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  out <- list(
    roles = read_cube_roles_impl(path),
    source = read_cube_source_impl(path),
    data_col = read_cube_data_col_impl(path),
    dim_semantics = read_cube_dim_semantics_impl(path, validate = FALSE)
  )

  if (isTRUE(validate)) {
    out <- normalize_cube_metadata_bundle(out, meta$dim_order)
  }

  out
}

write_cube_dim_semantics_impl <- function(path, dim_semantics, validate = TRUE) {
  require_cube_metadata_admin("write dim_semantics metadata")
  checkmate::assert_string(path, min.chars = 1)

  meta <- get_cube_metadata_cached(path)
  cube_roles <- read_roles_from_cube(path, meta = meta)
  dsem <- ensure_dim_semantics(
    dim_semantics = dim_semantics,
    dim_names = meta$dim_order,
    time_dim = cube_roles$time,
    area_dim = cube_roles$area
  )

  if (isTRUE(validate)) {
    validate_dim_semantics(
      dim_semantics = dsem,
      dim_names = meta$dim_order,
      time_dim = cube_roles$time,
      area_dim = cube_roles$area
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

write_cube_roles_impl <- function(path, roles) {
  require_cube_metadata_admin("write cube roles metadata")
  checkmate::assert_string(path, min.chars = 1)
  meta <- get_cube_metadata_cached(path)
  roles <- normalize_cube_roles(roles, meta$dim_order)
  write_cube_roles_metadata(path, roles)
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

write_cube_source_impl <- function(path, source) {
  require_cube_metadata_admin("write cube source metadata")
  checkmate::assert_string(path, min.chars = 1)
  write_cube_source_metadata(path, source)
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

write_cube_data_col_impl <- function(path, data_col) {
  require_cube_metadata_admin("write cube data_col metadata")
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_string(data_col, min.chars = 1)
  pa_h5_write_dataset(path, "cube/metadata/data_col", as.character(data_col))
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Get cube roles from a poparray or cube file
#'
#' Returns dimension role metadata from either an in-memory `poparray` or an
#' HDF5 cube path.
#'
#' @param x A `poparray` or HDF5 cube path.
#'
#' @return Named list with `time`, `area`, and `strata`.
#' @export
roles <- function(x) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(read_cube_roles_impl(x))
  }
  if (is(x, "poparray")) {
    dn <- names(dimnames(x))
    return(list(
      time = time_role(x),
      area = area_role(x),
      strata = setdiff(dn, c(time_role(x), area_role(x)))
    ))
  }
  cli::cli_abort("{.arg x} must be a {.cls poparray} or an HDF5 cube path.")
}

#' @rdname roles
#' @export
`roles<-` <- function(x, value) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    write_cube_roles_impl(x, value)
    return(x)
  }
  if (!is(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray} or an HDF5 cube path.")
  }
  dn <- names(dimnames(x))
  val <- normalize_cube_roles(value, dn)
  x@time_role <- val$time
  x@area_role <- val$area
  x@strata_roles <- val$strata
  validate_poparray(x)
  x
}

#' Get source metadata from a poparray or cube file
#'
#' Returns source/provenance metadata from either an in-memory `poparray` or an
#' HDF5 cube path.
#'
#' @param x A `poparray` or HDF5 cube path.
#'
#' @return Named list of source/provenance fields.
#' @export
source_meta <- function(x) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(read_cube_source_impl(x))
  }
  if (is(x, "poparray")) {
    return(normalize_cube_source(get_source(x)))
  }
  cli::cli_abort("{.arg x} must be a {.cls poparray} or an HDF5 cube path.")
}

#' @rdname source_meta
#' @export
`source_meta<-` <- function(x, value) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    write_cube_source_impl(x, value)
    return(x)
  }
  if (!is(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray} or an HDF5 cube path.")
  }
  x@source <- normalize_cube_source(value)
  x
}

#' Get canonical cube metadata as one bundle
#'
#' Returns the canonical metadata bundle from either an in-memory `poparray` or
#' an HDF5 cube path.
#'
#' @param x A `poparray` or HDF5 cube path.
#'
#' @return Named list with `roles`, `source`, `data_col`, and `dim_semantics`.
#' @export
cube_metadata <- function(x) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(read_cube_metadata_admin_impl(x))
  }
  if (is(x, "poparray")) {
    return(list(
      roles = roles(x),
      source = source_meta(x),
      data_col = data_col(x),
      dim_semantics = dim_semantics(x)
    ))
  }
  cli::cli_abort("{.arg x} must be a {.cls poparray} or an HDF5 cube path.")
}

#' @rdname cube_metadata
#' @export
`cube_metadata<-` <- function(x, value) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    write_cube_metadata_admin_impl(x, value)
    return(x)
  }
  if (!is(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray} or an HDF5 cube path.")
  }
  dn <- names(dimnames(x))
  bundled <- normalize_cube_metadata_bundle(value, dn)
  x <- `roles<-`(x, bundled$roles)
  x@source <- bundled$source
  x@data_col <- bundled$data_col
  x@dim_semantics <- bundled$dim_semantics
  validate_poparray(x)
  x
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

write_cube_metadata_admin_impl <- function(path, metadata, validate = TRUE) {
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
