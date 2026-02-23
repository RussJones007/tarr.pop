# -------------------------------------------------------------------------------------->
# Script: open_pop_array.r
# Description:
#   Open poparray cubes using metadata stored inside each HDF5 file under:
#   - cube/population
#   - cube/metadata/*
# -------------------------------------------------------------------------------------->

#' Resolve extdata directory for this package
#'
#' @return Absolute path to extdata directory.
#' @keywords internal
resolve_extdata_dir <- function() {
  pkg <- utils::packageName()
  if (length(pkg) == 1L && !is.na(pkg) && nzchar(pkg)) {
    ext <- system.file("extdata", package = pkg)
    if (nzchar(ext) && dir.exists(ext)) {
      return(ext)
    }
  }

  local_ext <- file.path(getwd(), "inst", "extdata")
  if (dir.exists(local_ext)) {
    return(normalizePath(local_ext, winslash = "/", mustWork = TRUE))
  }

  stop("Could not locate package extdata directory.")
}

#' Read a scalar metadata value from HDF5
#'
#' @param path HDF5 file path.
#' @param name HDF5 dataset path.
#'
#' @return Length-1 character scalar.
#' @keywords internal
h5_read_scalar_chr <- function(path, name) {
  val <- rhdf5::h5read(path, name)
  as.character(val[[1L]])
}

h5_dataset_exists <- function(path, dataset) {
  info <- rhdf5::h5ls(path)
  ds <- if (startsWith(dataset, "/")) dataset else paste0("/", dataset)
  grp <- dirname(ds)
  if (identical(grp, ".")) grp <- "/"
  nm <- basename(ds)
  any(info$group == grp & info$name == nm)
}

#' Read scalar metadata if present
#'
#' @param path HDF5 file path.
#' @param name HDF5 dataset path.
#'
#' @return Length-1 character scalar or NULL.
#' @keywords internal
h5_read_scalar_chr_if_present <- function(path, name) {
  if (!h5_dataset_exists(path, name)) return(NULL)
  h5_read_scalar_chr(path, name)
}

#' Check if a file has the cube metadata schema
#'
#' @param path HDF5 file path.
#'
#' @return Logical scalar.
#' @keywords internal
h5_has_cube_schema <- function(path) {
  info <- rhdf5::h5ls(path)
  has_pop <- any(info$group == "/cube" & info$name == "population")
  has_dim_order <- any(info$group == "/cube/metadata" & info$name == "dim_order")
  has_dimnames <- any(info$group == "/cube/metadata" & info$name == "dimnames")
  has_time <- any(info$group == "/cube/metadata/roles" & info$name == "time")
  has_area <- any(info$group == "/cube/metadata/roles" & info$name == "area")
  has_pop && has_dim_order && has_dimnames && has_time && has_area
}

#' Read one series index row from canonical metadata
#'
#' @param path HDF5 file path.
#'
#' @return One-row data.frame.
#' @keywords internal
read_series_row <- function(path) {
  series_id <- h5_read_scalar_chr_if_present(path, "cube/metadata/series_id")
  if (is.null(series_id) || !nzchar(series_id)) {
    series_id <- h5_read_scalar_chr_if_present(path, "cube/metadata/source/note")
  }
  if (is.null(series_id) || !nzchar(series_id)) {
    series_id <- h5_read_scalar_chr_if_present(path, "cube/metadata/registry/series_id")
  }
  if (is.null(series_id) || !nzchar(series_id)) {
    series_id <- tools::file_path_sans_ext(basename(path))
  }

  out <- data.frame(
    series_id = as.character(series_id),
    filepath = normalizePath(path, winslash = "/", mustWork = TRUE),
    filename = basename(path),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  geo <- h5_read_scalar_chr_if_present(path, "cube/metadata/geo")
  if (is.null(geo) || !nzchar(geo)) {
    geo <- h5_read_scalar_chr_if_present(path, "cube/metadata/registry/geo")
  }
  if (!is.null(geo) && nzchar(geo)) out$geo <- as.character(geo)

  ext_year <- h5_read_scalar_chr_if_present(path, "cube/metadata/extendable_year")
  if (is.null(ext_year) || !nzchar(ext_year)) {
    ext_year <- h5_read_scalar_chr_if_present(path, "cube/metadata/registry/extendable_year")
  }
  if (!is.null(ext_year) && nzchar(ext_year)) out$extendable_year <- as.character(ext_year)

  source <- h5_read_scalar_chr_if_present(path, "cube/metadata/source/source")
  if (!is.null(source) && nzchar(source)) out$source <- as.character(source)

  population_type <- h5_read_scalar_chr_if_present(path, "cube/metadata/source/population_type")
  if (!is.null(population_type) && nzchar(population_type)) out$population_type <- as.character(population_type)

  out
}

#' Backward-compatible alias for legacy registry row reader
#'
#' @param path HDF5 file path.
#'
#' @return One-row data.frame.
#' @keywords internal
read_registry_row <- function(path) {
  read_series_row(path)
}

#' Series registry read from migrated HDF5 metadata
#'
#' Scans `inst/extdata/*.h5` and builds the registry from canonical
#' `cube/metadata/*` fields in migrated files.
#'
#' @return data.frame of available series.
#' @keywords internal
tarr_series_registry <- function() {
  ext_dir <- resolve_extdata_dir()
  files <- sort(Sys.glob(file.path(ext_dir, "*.h5")))
  if (length(files) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  keep <- vapply(files, h5_has_cube_schema, logical(1))
  files <- files[keep]
  if (length(files) == 0L) {
    stop("No migrated cubes with /cube/metadata were found in extdata.")
  }

  rows <- lapply(files, read_series_row)
  reg <- dplyr::bind_rows(rows)
  if ("series_id" %in% names(reg)) {
    reg <- reg[order(reg$series_id), , drop = FALSE]
  }
  rownames(reg) <- NULL
  reg
}

#' Read dimnames metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Named list of character vectors.
#' @keywords internal
read_dimnames_from_cube <- function(path) {
  dim_order <- as.character(rhdf5::h5read(path, "cube/metadata/dim_order"))
  out <- lapply(dim_order, function(d) {
    as.character(rhdf5::h5read(path, paste0("cube/metadata/dimnames/", d)))
  })
  names(out) <- dim_order
  out
}

#' Read dimension roles metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Named list with time, area, strata.
#' @keywords internal
read_roles_from_cube <- function(path) {
  list(
    time = h5_read_scalar_chr(path, "cube/metadata/roles/time"),
    area = h5_read_scalar_chr(path, "cube/metadata/roles/area"),
    strata = as.character(rhdf5::h5read(path, "cube/metadata/roles/strata"))
  )
}

#' Read source metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Named character vector with source fields.
#' @keywords internal
read_source_from_cube <- function(path) {
  c(
    note = h5_read_scalar_chr(path, "cube/metadata/source/note"),
    source = h5_read_scalar_chr(path, "cube/metadata/source/source"),
    updated = h5_read_scalar_chr(path, "cube/metadata/source/updated"),
    population_type = h5_read_scalar_chr(path, "cube/metadata/source/population_type")
  )
}

#' Validate migrated dimnames against cube dimensions
#'
#' @param h5_handle Delayed HDF5Array handle.
#' @param dimn Named dimnames list.
#' @param series_id Series identifier.
#'
#' @return Invisibly TRUE.
#' @keywords internal
validate_labels_against_cube <- function(h5_handle, dimn, series_id) {
  d <- dim(h5_handle)
  if (length(d) != length(dimn)) {
    stop(
      "Dimension count mismatch for series '", series_id,
      "': cube=", length(d), ", metadata=", length(dimn), "."
    )
  }
  lens <- vapply(dimn, length, integer(1))
  if (!all(as.integer(d) == as.integer(lens))) {
    stop(
      "Dimension length mismatch for series '", series_id, "': cube dim() ",
      paste(d, collapse = " x "), " vs metadata ",
      paste(lens, collapse = " x "), "."
    )
  }
  invisible(TRUE)
}

#' Backward-compatible alias for open_poparray()
#'
#' @inheritParams open_poparray
#' @rdname open_poparray
#' @export
open_tarr_pop <- function(...) {
  open_poparray(...)
}

#' Open a migrated population cube
#'
#' Opens a population series from HDF5 and constructs a `poparray` using
#' metadata stored in the same file under `cube/metadata`.
#'
#' @param series_id Name of the population series.
#' @param dataset HDF5 dataset path for numeric cube data.
#'   Defaults to `"cube/population"` for the migrated cube schema.
#' @param data_col Name of the value column when coercing to a data frame.
#'
#' @details
#' The function reads all semantic metadata from the same HDF5 file:
#' - dimension order and labels from `cube/metadata/dim_order` and
#'   `cube/metadata/dimnames/*`,
#' - dimension roles from `cube/metadata/roles/*`,
#' - source/provenance fields from `cube/metadata/source/*`.
#'
#' @returns A poparray.
#' @export
open_poparray <- function(series_id,
                          dataset = "cube/population",
                          data_col = "population") {
  reg <- tarr_series_registry()
  row <- reg[reg$series_id == series_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("Unknown series_id: ", series_id)
  }

  path <- row$filepath[[1L]]

  if (!file.exists(path)) {
    stop("HDF5 file not found for series '", series_id, "': ", path)
  }

  h5    <- HDF5Array::HDF5Array(filepath = path, name = dataset)
  dimn  <- read_dimnames_from_cube(path)
  roles <- read_roles_from_cube(path)
  src   <- read_source_from_cube(path)

  validate_labels_against_cube(h5, dimn, series_id)
  dimnames(h5) <- dimn

  new_poparray(
    x             = h5,
    dimnames_list = dimn,
    data_col      = data_col,
    source        = src,
    time_dim      = roles$time,
    area_dim      = roles$area
  )
}
