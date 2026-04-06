# -------------------------------------------------------------------------------------->
# Script: open_pop_array.r
# Description:
#   Open poparray cubes using metadata stored inside each HDF5 file under:
#   - cube/population
#   - cube/metadata/*
# -------------------------------------------------------------------------------------->

#' Resolve extdata directory for this package
#'
#' @param strict Logical; error if the directory cannot be located.
#'
#' @return Absolute path to extdata directory.
#' @keywords internal
resolve_extdata_dir <- function(strict = TRUE) {
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

  if (isTRUE(strict)) {
    stop("Could not locate package extdata directory.")
  }

  NULL
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

cube_info_has_dataset <- function(info, dataset) {
  ds <- if (startsWith(dataset, "/")) dataset else paste0("/", dataset)
  grp <- dirname(ds)
  if (identical(grp, ".")) grp <- "/"
  nm <- basename(ds)
  any(info$group == grp & info$name == nm)
}

h5_dataset_exists <- function(path, dataset, info = NULL) {
  if (is.null(info)) {
    info <- rhdf5::h5ls(path)
  }
  cube_info_has_dataset(info, dataset)
}

#' Read scalar metadata if present
#'
#' @param path HDF5 file path.
#' @param name HDF5 dataset path.
#'
#' @return Length-1 character scalar or NULL.
#' @keywords internal
h5_read_scalar_chr_if_present <- function(path, name, info = NULL) {
  if (!h5_dataset_exists(path, name, info = info)) return(NULL)
  h5_read_scalar_chr(path, name)
}

#' Check if a file has the cube metadata schema
#'
#' @param path HDF5 file path.
#'
#' @return Logical scalar.
#' @keywords internal
h5_has_cube_schema <- function(path) {
  info <- tryCatch(rhdf5::h5ls(path), error = function(e) NULL)
  if (is.null(info)) {
    return(FALSE)
  }
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
  info <- rhdf5::h5ls(path)

  series_id <- h5_read_scalar_chr_if_present(path, "cube/metadata/series_id", info = info)
  if (is.null(series_id) || !nzchar(series_id)) {
    series_id <- h5_read_scalar_chr_if_present(path, "cube/metadata/source/note", info = info)
  }
  if (is.null(series_id) || !nzchar(series_id)) {
    series_id <- h5_read_scalar_chr_if_present(path, "cube/metadata/registry/series_id", info = info)
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

  geo <- h5_read_scalar_chr_if_present(path, "cube/metadata/geo", info = info)
  if (is.null(geo) || !nzchar(geo)) {
    geo <- h5_read_scalar_chr_if_present(path, "cube/metadata/registry/geo", info = info)
  }
  if (!is.null(geo) && nzchar(geo)) out$geo <- as.character(geo)

  ext_year <- h5_read_scalar_chr_if_present(path, "cube/metadata/extendable_year", info = info)
  if (is.null(ext_year) || !nzchar(ext_year)) {
    ext_year <- h5_read_scalar_chr_if_present(path, "cube/metadata/registry/extendable_year", info = info)
  }
  if (!is.null(ext_year) && nzchar(ext_year)) out$extendable_year <- as.character(ext_year)

  source <- h5_read_scalar_chr_if_present(path, "cube/metadata/source/source", info = info)
  if (!is.null(source) && nzchar(source)) out$source <- as.character(source)

  population_type <- h5_read_scalar_chr_if_present(path, "cube/metadata/source/population_type", info = info)
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

cube_registry_inventory <- function(root = resolve_cube_dir()) {
  scan_dir <- resolve_cube_base_dir(root)
  files <- sort(list.files(
    scan_dir,
    pattern = "\\.(h5|hdf5)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  ))

  if (length(files) == 0L) {
    return(data.frame(
      filepath = character(),
      filename = character(),
      file_created = as.POSIXct(character()),
      file_modified = as.POSIXct(character()),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  info <- file.info(files)

  data.frame(
    filepath = files,
    filename = basename(files),
    file_created = as.POSIXct(info$ctime, origin = "1970-01-01", tz = "UTC"),
    file_modified = as.POSIXct(info$mtime, origin = "1970-01-01", tz = "UTC"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

read_cube_registry_cache <- function(cache_file) {
  if (is.null(cache_file) || !file.exists(cache_file)) {
    return(NULL)
  }

  out <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  if (!is.data.frame(out)) {
    return(NULL)
  }

  req <- c("filepath", "filename", "file_created", "file_modified")
  if (!all(req %in% names(out))) {
    return(NULL)
  }

  out
}

write_cube_registry_cache <- function(registry, cache_file) {
  if (is.null(cache_file)) {
    return(invisible(registry))
  }

  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(registry, cache_file)
  invisible(registry)
}

cube_registry_cache_current <- function(cache, inventory) {
  if (!is.data.frame(cache) || !is.data.frame(inventory)) {
    return(FALSE)
  }

  if (nrow(cache) != nrow(inventory)) {
    return(FALSE)
  }

  cols <- c("filepath", "filename", "file_created", "file_modified")
  if (!all(cols %in% names(cache))) {
    return(FALSE)
  }

  identical(as.character(cache$filepath), as.character(inventory$filepath)) &&
    identical(as.character(cache$filename), as.character(inventory$filename)) &&
    identical(as.numeric(cache$file_created), as.numeric(inventory$file_created)) &&
    identical(as.numeric(cache$file_modified), as.numeric(inventory$file_modified))
}

#' Series registry read from migrated HDF5 metadata
#'
#' Scans the cube `base/` directory recursively for HDF5 files and builds the
#' registry from canonical `cube/metadata/*` fields in migrated files. When a
#' cache file is available under `cache/cube_registry.rds`, it is reused until
#' the file inventory changes.
#'
#' @return data.frame of available series.
#' @keywords internal
.cube_metadata_cache <- new.env(parent = emptyenv())
.tarr_series_registry_cache <- memoise::cache_memory()

cube_metadata_cache_key <- function(path) {
  info <- file.info(path)
  if (!isTRUE(file.exists(path)) || is.na(info$mtime[[1L]])) {
    stop("HDF5 file not found for metadata cache: ", path)
  }

  paste(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    format(info$mtime[[1L]], tz = "UTC", usetz = TRUE),
    sep = "::"
  )
}

get_cube_metadata <- function(path) {
  info <- rhdf5::h5ls(path)
  dim_order <- as.character(rhdf5::h5read(path, "cube/metadata/dim_order"))

  dimnames <- lapply(dim_order, function(d) {
    as.character(rhdf5::h5read(path, paste0("cube/metadata/dimnames/", d)))
  })
  names(dimnames) <- dim_order

  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    info = info,
    dim_order = dim_order,
    dimnames = dimnames,
    roles = list(
      time = h5_read_scalar_chr(path, "cube/metadata/roles/time"),
      area = h5_read_scalar_chr(path, "cube/metadata/roles/area"),
      strata = as.character(rhdf5::h5read(path, "cube/metadata/roles/strata"))
    ),
    source = c(
      note = h5_read_scalar_chr(path, "cube/metadata/source/note"),
      source = h5_read_scalar_chr(path, "cube/metadata/source/source"),
      updated = h5_read_scalar_chr(path, "cube/metadata/source/updated"),
      population_type = h5_read_scalar_chr(path, "cube/metadata/source/population_type")
    ),
    data_col = {
      val <- h5_read_scalar_chr_if_present(path, "cube/metadata/data_col", info = info)
      if (is.null(val) || !nzchar(val)) "population" else val
    }
  )
}

get_cube_metadata_cached <- function(path) {
  key <- cube_metadata_cache_key(path)

  if (exists(key, envir = .cube_metadata_cache, inherits = FALSE)) {
    return(get(key, envir = .cube_metadata_cache, inherits = FALSE))
  }

  meta <- get_cube_metadata(path)
  assign(key, meta, envir = .cube_metadata_cache)
  meta
}

read_dim_semantics_entry <- function(meta, base, dim_name_fallback) {
  path <- meta$path
  info <- meta$info

  read_if_present <- function(field) {
    ds <- paste0(base, "/", field)
    if (cube_info_has_dataset(info, ds)) {
      h5_read_scalar_chr(path, ds)
    } else {
      NULL
    }
  }

  list(
    dim_name = read_if_present("dim_name") %||% dim_name_fallback,
    domain = h5_read_scalar_chr(path, paste0(base, "/domain")),
    scale_type = h5_read_scalar_chr(path, paste0(base, "/scale_type")),
    partition_type = h5_read_scalar_chr(path, paste0(base, "/partition_type")),
    validated = tolower(h5_read_scalar_chr(path, paste0(base, "/validated"))) == "true",
    overlap_levels = if (cube_info_has_dataset(info, paste0(base, "/overlap_levels"))) {
      as.character(rhdf5::h5read(path, paste0(base, "/overlap_levels")))
    } else {
      character()
    },
    notes = if (cube_info_has_dataset(info, paste0(base, "/notes"))) {
      as.character(rhdf5::h5read(path, paste0(base, "/notes")))
    } else {
      character()
    }
  )
}

parse_dim_semantics_from_meta <- function(meta, dim_order = meta$dim_order, time_dim, area_dim) {
  has_group <- any(meta$info$group == "/cube/metadata" & meta$info$name == "dim_semantics")
  if (!isTRUE(has_group)) {
    cli::cli_abort(
      "Missing required metadata group {.val cube/metadata/dim_semantics}. This cube must be migrated before opening."
    )
  }

  out <- lapply(dim_order, function(d) {
    base <- paste0("cube/metadata/dim_semantics/", d)

    new_fields <- c("domain", "scale_type", "partition_type", "validated", "overlap_levels", "notes")
    has_new <- all(vapply(new_fields, function(fld) {
      cube_info_has_dataset(meta$info, paste0(base, "/", fld))
    }, logical(1)))

    if (isTRUE(has_new)) {
      entry <- read_dim_semantics_entry(meta, base, d)

      return(new_dim_semantics(
        dim_name = entry$dim_name,
        domain = entry$domain,
        scale_type = entry$scale_type,
        partition_type = entry$partition_type,
        validated = entry$validated,
        overlap_levels = entry$overlap_levels,
        notes = entry$notes
      ))
    }

    legacy_fields <- c("class", "validated")
    for (fld in legacy_fields) {
      ds <- paste0(base, "/", fld)
      if (!cube_info_has_dataset(meta$info, ds)) {
        cli::cli_abort("Missing dim_semantics dataset {.val {ds}}.")
      }
    }

    cls <- h5_read_scalar_chr(meta$path, paste0(base, "/class"))
    pt <- switch(cls, partition = "partition", set = "set", "unknown")
    new_dim_semantics(
      dim_name = d,
      domain = pa_default_dim_domain(d, time_dim, area_dim),
      scale_type = pa_default_dim_scale_type(d, time_dim),
      partition_type = pt,
      validated = tolower(h5_read_scalar_chr(meta$path, paste0(base, "/validated"))) == "true",
      overlap_levels = character(),
      notes = character()
    )
  })
  names(out) <- dim_order
  out
}

reset_poparray_cache <- function() {
  rm(list = ls(envir = .cube_metadata_cache, all.names = TRUE), envir = .cube_metadata_cache)
  memoise::forget(.tarr_series_registry_memoised)
  invisible(TRUE)
}

.tarr_series_registry_impl <- function(cube_dir, inventory) {
  cache_file <- cube_registry_cache_file(cube_dir)

  if (nrow(inventory) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  cache <- read_cube_registry_cache(cache_file)
  if (cube_registry_cache_current(cache, inventory)) {
    return(cache)
  }

  keep <- vapply(inventory$filepath, h5_has_cube_schema, logical(1))
  files <- inventory$filepath[keep]
  inventory <- inventory[keep, , drop = FALSE]
  if (length(files) == 0L) {
    stop("No migrated cubes with /cube/metadata were found in cube storage.")
  }

  rows <- lapply(files, read_series_row)
  reg <- dplyr::bind_rows(rows)
  reg <- dplyr::left_join(
    reg,
    inventory,
    by = c("filepath", "filename")
  )
  if ("series_id" %in% names(reg)) {
    reg <- reg[order(reg$series_id), , drop = FALSE]
  }
  rownames(reg) <- NULL
  write_cube_registry_cache(reg, cache_file)
  reg
}

.tarr_series_registry_memoised <- memoise::memoise(
  .tarr_series_registry_impl,
  cache = .tarr_series_registry_cache
)

tarr_series_registry <- function(root = resolve_cube_dir()) {
  cube_dir <- normalizePath(root, winslash = "/", mustWork = TRUE)
  inventory <- cube_registry_inventory(cube_dir)
  .tarr_series_registry_memoised(cube_dir, inventory)
}

#' Read dimnames metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Named list of character vectors.
#' @keywords internal
read_dimnames_from_cube <- function(path, meta = NULL) {
  if (is.null(meta)) {
    meta <- get_cube_metadata_cached(path)
  }
  meta$dimnames
}

#' Read dimension roles metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Named list with time, area, strata.
#' @keywords internal
read_roles_from_cube <- function(path, meta = NULL) {
  if (is.null(meta)) {
    meta <- get_cube_metadata_cached(path)
  }
  meta$roles
}

#' Read source metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Named character vector with source fields.
#' @keywords internal
read_source_from_cube <- function(path, meta = NULL) {
  if (is.null(meta)) {
    meta <- get_cube_metadata_cached(path)
  }
  meta$source
}

#' Read data column metadata from migrated cube
#'
#' @param path HDF5 file path.
#'
#' @return Length-1 character scalar.
#' @keywords internal
read_data_col_from_cube <- function(path, meta = NULL) {
  if (is.null(meta)) {
    meta <- get_cube_metadata_cached(path)
  }
  meta$data_col
}

#' Read dim_semantics metadata from migrated cube
#'
#' @param path HDF5 file path.
#' @param dim_order Character dimension order vector.
#' @param time_dim Time role dimension name.
#' @param area_dim Area role dimension name.
#'
#' @return Named list with one semantic entry per dimension.
#' @keywords internal
read_dim_semantics_from_cube <- function(path, dim_order, time_dim, area_dim, meta = NULL) {
  if (is.null(meta)) {
    meta <- get_cube_metadata_cached(path)
  }
  parse_dim_semantics_from_meta(
    meta = meta,
    dim_order = dim_order,
    time_dim = time_dim,
    area_dim = area_dim
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
#' - per-dimension semantics from `cube/metadata/dim_semantics/*/*`.
#'
#' @returns A poparray.
#' @export
open_poparray <- function(series_id,
                          dataset = "cube/population",
                          data_col = NULL) {
  reg <- tarr_series_registry()
  row <- reg[reg$series_id == series_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("Unknown series_id: ", series_id)
  }

  path <- row$filepath[[1L]]

  if (!file.exists(path)) {
    stop("HDF5 file not found for series '", series_id, "': ", path)
  }

  meta <- get_cube_metadata_cached(path)
  h5    <- HDF5Array::HDF5Array(filepath = path, name = dataset)
  dimn  <- read_dimnames_from_cube(path, meta = meta)
  roles <- read_roles_from_cube(path, meta = meta)
  dsem  <- read_dim_semantics_from_cube(path, names(dimn), roles$time, roles$area, meta = meta)
  src   <- read_source_from_cube(path, meta = meta)
  dc    <- if (is.null(data_col)) read_data_col_from_cube(path, meta = meta) else data_col

  validate_labels_against_cube(h5, dimn, series_id)
  dimnames(h5) <- dimn

  new_poparray(
    x             = h5,
    dimnames_list = dimn,
    data_col      = dc,
    source        = src,
    time_dim      = roles$time,
    area_dim      = roles$area,
    dim_semantics = dsem
  )
}
