# -------------------------------------------------------------------------------------->
# Script: cube_io.R
# Description:
#   Shared I/O utilities for writing poparray cubes to the canonical HDF5 schema:
#   - cube/population
#   - cube/metadata/*
# -------------------------------------------------------------------------------------->

pa_bytes_per_cell <- function(type = "double") {
  t <- tolower(as.character(type)[1L])
  switch(
    t,
    integer = 4L,
    logical = 1L,
    raw = 1L,
    double = 8L,
    numeric = 8L,
    8L
  )
}

#' Guess an HDF5 chunk shape for a poparray cube
#'
#' Chooses chunk dimensions with a bias toward larger chunks along time and area,
#' while respecting a target chunk byte size.
#'
#' @param dim Integer dimension vector.
#' @param dimnames_list Named dimnames list aligned with `dim`.
#' @param time_dim Time dimension name.
#' @param area_dim Area dimension name.
#' @param target_chunk_bytes Target chunk size in bytes.
#' @param type Storage type string (e.g., `"double"`, `"integer"`).
#'
#' @return Integer chunk dimensions.
#' @keywords internal
pa_guess_chunkdim <- function(dim,
                              dimnames_list,
                              time_dim = "year",
                              area_dim = "area.name",
                              target_chunk_bytes = 1e6,
                              type = "double") {
  dim <- as.integer(dim)
  if (!length(dim) || any(is.na(dim)) || any(dim < 1L)) {
    cli::cli_abort("{.arg dim} must be positive integers.")
  }
  if (!is.list(dimnames_list) || is.null(names(dimnames_list))) {
    cli::cli_abort("{.arg dimnames_list} must be a named list.")
  }
  if (length(dimnames_list) != length(dim)) {
    cli::cli_abort("{.arg dimnames_list} length must match {.arg dim}.")
  }

  nms <- names(dimnames_list)
  target_cells <- max(1L, floor(as.numeric(target_chunk_bytes) / pa_bytes_per_cell(type)))

  chunk <- rep.int(1L, length(dim))
  names(chunk) <- nms

  time_k <- match(time_dim, nms, nomatch = 0L)
  area_k <- match(area_dim, nms, nomatch = 0L)

  if (time_k > 0L) chunk[[time_k]] <- min(dim[[time_k]], 64L)
  if (area_k > 0L) chunk[[area_k]] <- min(dim[[area_k]], 16L)

  other_k <- setdiff(seq_along(dim), c(time_k, area_k))
  if (length(other_k)) {
    chunk[other_k] <- pmin.int(dim[other_k], 4L)
  }

  # If over budget, shrink largest dimensions first.
  while (prod(chunk) > target_cells && any(chunk > 1L)) {
    k <- which.max(chunk)
    chunk[[k]] <- max(1L, as.integer(floor(chunk[[k]] / 2L)))
  }

  # Grow toward target with priority time -> area -> others.
  grow_order <- c(time_k, area_k, other_k)
  grow_order <- grow_order[grow_order > 0L]
  grew <- TRUE
  while (isTRUE(grew)) {
    grew <- FALSE
    for (k in grow_order) {
      if (chunk[[k]] >= dim[[k]]) next
      cand <- chunk
      cand[[k]] <- min(dim[[k]], as.integer(cand[[k]] * 2L))
      if (prod(cand) <= target_cells) {
        chunk <- cand
        grew <- TRUE
      }
    }
  }

  as.integer(chunk)
}

pa_h5_delete_if_exists <- function(file, name) {
  try(rhdf5::h5delete(file = file, name = name), silent = TRUE)
}

pa_h5_write_dataset <- function(file, name, x) {
  pa_h5_delete_if_exists(file, name)
  rhdf5::h5write(obj = x, file = file, name = name)
}

pa_h5_group_exists <- function(file, group) {
  grp <- if (startsWith(group, "/")) group else paste0("/", group)
  info <- tryCatch(rhdf5::h5ls(file), error = function(e) NULL)
  if (is.null(info)) return(FALSE)
  parent <- dirname(grp)
  if (identical(parent, ".")) parent <- "/"
  nm <- basename(grp)
  any(info$group == parent & info$name == nm)
}

pa_h5_create_group <- function(file, group) {
  grp <- if (startsWith(group, "/")) sub("^/", "", group) else group
  if (!pa_h5_group_exists(file, grp)) {
    try(rhdf5::h5createGroup(file = file, group = grp), silent = TRUE)
  }
  invisible(TRUE)
}

pa_normalize_source <- function(source = NULL) {
  src <- if (is.null(source)) list() else as.list(source)
  if (is.null(src$note) || !length(src$note)) src$note <- ""
  if (is.null(src$source) || !length(src$source)) src$source <- "Not given"
  if (is.null(src$updated) || !length(src$updated)) src$updated <- as.character(Sys.Date())
  if (is.null(src$population_type) || !length(src$population_type)) src$population_type <- "Unknown"
  src
}

pa_write_poparray_metadata <- function(filepath,
                                       dimnames_list,
                                       time_dim,
                                       area_dim,
                                       source = NULL,
                                       data_col = "population",
                                       registry = NULL,
                                       schema_version = "1.0.0") {
  pa_h5_create_group(filepath, "cube")
  pa_h5_create_group(filepath, "cube/metadata")
  pa_h5_create_group(filepath, "cube/metadata/registry")
  pa_h5_create_group(filepath, "cube/metadata/roles")
  pa_h5_create_group(filepath, "cube/metadata/source")
  pa_h5_create_group(filepath, "cube/metadata/dimnames")

  dim_order <- names(dimnames_list)
  strata <- setdiff(dim_order, c(time_dim, area_dim))
  src <- pa_normalize_source(source)

  pa_h5_write_dataset(filepath, "cube/metadata/schema_version", as.character(schema_version))
  pa_h5_write_dataset(filepath, "cube/metadata/migrated_on", as.character(Sys.time()))
  pa_h5_write_dataset(filepath, "cube/metadata/data_col", as.character(data_col))

  pa_h5_write_dataset(filepath, "cube/metadata/roles/time", as.character(time_dim))
  pa_h5_write_dataset(filepath, "cube/metadata/roles/area", as.character(area_dim))
  pa_h5_write_dataset(filepath, "cube/metadata/roles/strata", as.character(strata))
  pa_h5_write_dataset(filepath, "cube/metadata/dim_order", as.character(dim_order))

  for (d in dim_order) {
    pa_h5_write_dataset(filepath, paste0("cube/metadata/dimnames/", d), as.character(dimnames_list[[d]]))
  }

  pa_h5_write_dataset(filepath, "cube/metadata/source/note", as.character(src$note))
  pa_h5_write_dataset(filepath, "cube/metadata/source/source", as.character(src$source))
  pa_h5_write_dataset(filepath, "cube/metadata/source/updated", as.character(src$updated))
  pa_h5_write_dataset(filepath, "cube/metadata/source/population_type", as.character(src$population_type))

  if (!is.null(registry)) {
    reg <- if (is.data.frame(registry)) as.list(registry[1, , drop = FALSE]) else as.list(registry)
    for (k in names(reg)) {
      pa_h5_write_dataset(filepath, paste0("cube/metadata/registry/", k), as.character(reg[[k]]))
    }
  }

  invisible(TRUE)
}

#' Write an array-like object to canonical poparray HDF5 cube schema
#'
#' Writes data to `cube/population` and metadata to `cube/metadata/*`.
#'
#' @param x Array-like object (`DelayedArray`-compatible).
#' @param filepath Output HDF5 path. If `NULL`, a tempfile is used.
#' @param dimnames_list Named dimnames list; defaults to `dimnames(x)`.
#' @param overwrite Overwrite existing file?
#' @param chunkdim Integer chunk dimensions, `NULL`, or `"auto"`.
#' @param level Compression level (0-9).
#' @param time_dim Time dimension name.
#' @param area_dim Area dimension name.
#' @param source Source metadata list/vector.
#' @param data_col Value column label.
#' @param registry Optional registry metadata list/data.frame (one row).
#' @param target_chunk_bytes Target bytes for auto chunking.
#'
#' @return List with `filepath`, `dataset`, and `chunkdim`.
#' @keywords internal
pa_write_poparray_cube <- function(x,
                                   filepath = NULL,
                                   dimnames_list = dimnames(x),
                                   overwrite = FALSE,
                                   chunkdim = "auto",
                                   level = 6L,
                                   time_dim = "year",
                                   area_dim = "area.name",
                                   source = NULL,
                                   data_col = "population",
                                   registry = NULL,
                                   target_chunk_bytes = 1e6) {
  if (is.null(filepath)) {
    filepath <- tempfile("poparray_cube_", fileext = ".h5")
  }
  if (!is.character(filepath) || length(filepath) != 1L || !nzchar(filepath)) {
    cli::cli_abort("{.arg filepath} must be a non-empty file path.")
  }
  out_dir <- dirname(filepath)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(filepath)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort("File already exists: {.file {filepath}}. Set {.arg overwrite = TRUE} to replace.")
    }
    ok <- file.remove(filepath)
    if (!isTRUE(ok)) cli::cli_abort("Could not remove existing file: {.file {filepath}}.")
  }

  rhdf5::h5createFile(filepath)
  pa_h5_create_group(filepath, "cube")

  if (!is(x, "DelayedArray")) x <- DelayedArray::DelayedArray(x)

  dn <- dimnames_list
  if (is.null(dn) || !is.list(dn) || is.null(names(dn))) {
    cli::cli_abort("{.arg dimnames_list} must be a named list.")
  }
  if (length(dn) != length(dim(x))) {
    cli::cli_abort("{.arg dimnames_list} length must match array dimensionality.")
  }
  if (!time_dim %in% names(dn)) cli::cli_abort("Time dim {.val {time_dim}} not found in dimnames.")
  if (!area_dim %in% names(dn)) cli::cli_abort("Area dim {.val {area_dim}} not found in dimnames.")

  if (is.null(chunkdim) || (is.character(chunkdim) && length(chunkdim) == 1L && identical(chunkdim, "auto"))) {
    chunkdim <- pa_guess_chunkdim(
      dim = dim(x),
      dimnames_list = dn,
      time_dim = time_dim,
      area_dim = area_dim,
      target_chunk_bytes = target_chunk_bytes,
      type = DelayedArray::type(x)
    )
  } else {
    chunkdim <- as.integer(chunkdim)
    if (length(chunkdim) != length(dim(x)) || any(is.na(chunkdim)) || any(chunkdim < 1L)) {
      cli::cli_abort("{.arg chunkdim} must be positive integers with length equal to number of dimensions.")
    }
  }

  HDF5Array::writeHDF5Array(
    x = x,
    filepath = filepath,
    name = "cube/population",
    chunkdim = chunkdim,
    level = as.integer(level)
  )

  pa_write_poparray_metadata(
    filepath = filepath,
    dimnames_list = dn,
    time_dim = time_dim,
    area_dim = area_dim,
    source = source,
    data_col = data_col,
    registry = registry
  )

  list(
    filepath = normalizePath(filepath, winslash = "/", mustWork = TRUE),
    dataset = "cube/population",
    chunkdim = as.integer(chunkdim)
  )
}

#' Save a poparray cube to HDF5 using the canonical schema
#'
#' @param x A `poparray`.
#' @param filepath Output HDF5 file path.
#' @param overwrite Logical; overwrite if file exists.
#' @param chunkdim Integer chunk dimensions or `"auto"`.
#' @param level Compression level (0-9).
#' @param registry Optional registry metadata list/data.frame (one row).
#' @param target_chunk_bytes Target bytes for auto chunking.
#'
#' @return Invisibly returns a list with `filepath`, `dataset`, and `chunkdim`.
#' @export
save_poparray <- function(x,
                          filepath,
                          overwrite = FALSE,
                          chunkdim = "auto",
                          level = 6L,
                          registry = NULL,
                          target_chunk_bytes = 1e6) {
  if (!is(x, "poparray")) {
    cli::cli_abort("{.arg x} must be a {.cls poparray}.")
  }

  dn <- dimnames(x)
  time_dim <- time_role(x)
  area_dim <- area_role(x)
  source <- get_source(x)
  col_name <- data_col(x)
  x <- methods::as(x, "DelayedArray")

  out <- pa_write_poparray_cube(
    x = x,
    filepath = filepath,
    dimnames_list = dn,
    overwrite = overwrite,
    chunkdim = chunkdim,
    level = level,
    time_dim = time_dim,
    area_dim = area_dim,
    source = source,
    data_col = col_name,
    registry = registry,
    target_chunk_bytes = target_chunk_bytes
  )

  invisible(out)
}

#' Create and save a poparray cube with canonical HDF5 schema
#'
#' Minimal user-facing helper to write a cube to:
#' - `cube/population`
#' - `cube/metadata/*`
#'
#' @param x A `poparray`.
#' @param filepath Output HDF5 file path.
#' @param series_id Series identifier written to registry metadata.
#' @param chunkdim Integer chunk dimensions or `"auto"`.
#' @param overwrite Logical; overwrite existing file.
#' @param registry Optional extra registry fields (named list or one-row data.frame).
#'
#' @return Invisibly returns a list with `filepath`, `dataset`, and `chunkdim`.
#' @export
create_poparray <- function(x,
                            filepath,
                            series_id,
                            chunkdim = "auto",
                            overwrite = FALSE,
                            registry = NULL) {
  checkmate::assert_string(series_id, min.chars = 1)
  checkmate::assert_string(filepath, min.chars = 1)

  reg <- as.list(registry %||% list())
  reg$series_id <- series_id
  if (is.null(reg$filename) || !length(reg$filename)) {
    reg$filename <- basename(filepath)
  }

  out <- save_poparray(
    x = x,
    filepath = filepath,
    overwrite = overwrite,
    chunkdim = chunkdim,
    registry = reg
  )

  invisible(out)
}
