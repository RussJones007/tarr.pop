# -------------------------------------------------------------------------------------->
# Script: io_h5.R
# Description:
#   Low-level HDF5 I/O helpers for building cubes directly into inst/extdata/*.h5.
#   These functions operate on a "spec" from series_specs.R and numeric/array slabs
#   produced by ETL scripts (build_seer.R, build_census.R, build_tdc.R, ...).
#
# Design goals:
#   - No ETL in this file. Only HDF5 mechanics.
#   - Support arbitrary dimension counts (2D ZCTA, 5D TDC race.eth, 6D canonical cubes).
#   - Support extendable leading dimension (typically year/end.year) for append workflows.
#
# Dependencies:
#   - rhdf5 (Bioconductor) for writing HDF5
#
# Notes:
#   * HDF5 does not store R dimnames the same way arrays do. We keep dim labels in /data
#     and re-attach at open time. Still, we DO store some useful attributes in the .h5.
#   * These helpers assume the dataset is numeric (integer or double).
#
# Usage pattern:
#   path <- tarr_h5_path(spec$filename)
#   h5_create_cube(path, spec, dim_lengths = c(year=..., area.name=..., ...))
#   h5_write_slab(path, spec, i = year_index, slab = block_array)
#   h5_append_slab(path, spec, new_label = "2024", slab = block_array)
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 4, 2026
# Revised:
# -------------------------------------------------------------------------------------->
#

# basic utilities ----------------------------

tarr_h5_path <- function(filename, root = "inst/extdata") {
  file.path(root, filename)
}

.stopf <- function(...) stop(sprintf(...), call. = FALSE)

require_rhdf5 <- function() {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    .stopf("Package 'rhdf5' is required for build-time HDF5 writing. Install via Bioconductor.")
  }
  invisible(TRUE)
}

# Convert a named chunk vector (names align to spec$dims) to an integer vector in dim order.
# If spec$chunk is already unnamed numeric of correct length, it is used as-is.
resolve_chunk <- function(spec, dim_lengths) {
  ch <- spec$chunk
  if (is.null(ch)) return(NULL)
  
  if (is.null(names(ch))) {
    if (length(ch) != length(spec$dims)) {
      .stopf("Spec '%s': chunk must be length %d (got %d).",
             spec$series_id, length(spec$dims), length(ch))
    }
    return(as.integer(ch))
  }
  
  # Named chunk vector
  out <- rep(NA_integer_, length(spec$dims))
  names(out) <- spec$dims
  out[names(ch)] <- as.integer(ch)
  
  # Fill any missing chunk entries with full dim length (safe default)
  miss <- is.na(out)
  if (any(miss)) {
    out[miss] <- as.integer(dim_lengths[names(out)[miss]])
  }
  as.integer(out)
}

# Determine which dimension is the extendable "leading" dim.
# Convention: it's the first dimension in spec$dims (year, end.year, etc.)
leading_dim <- function(spec) spec$dims[[1]]

# HDF5 uses "Inf" maxdims for extendable datasets. We'll represent as NA in R then translate.
resolve_maxdims <- function(spec, dim_lengths) {
  md <- as.numeric(dim_lengths[spec$dims])
  if (isTRUE(spec$extendable_year)) {
    md[1] <--1  # translate to Inf-ish in rhdf5 (it accepts NA for unlimited)
  }
  md
}

# create / inspect datasets ----------------------------

h5_exists_dataset <- function(path, dataset) {
  require_rhdf5()
  if (!file.exists(path)) return(FALSE)
  info <- rhdf5::h5ls(path)
  any(info$group == dirname(dataset) & info$name == basename(dataset))
}

h5_delete_file <- function(path) {
  if (file.exists(path)) unlink(path)
  invisible(TRUE)
}

h5_create_cube <- function(path,
                           spec,
                           dim_lengths,
                           overwrite = FALSE,
                           attributes = NULL) {
  require_rhdf5()
  
  # Validate dim_lengths
  if (is.null(names(dim_lengths))) .stopf("dim_lengths must be a named vector keyed by dim names.")
  if (!all(spec$dims %in% names(dim_lengths))) {
    missing <- setdiff(spec$dims, names(dim_lengths))
    .stopf("Spec '%s': dim_lengths missing entries for: %s",
           spec$series_id, paste(missing, collapse = ", "))
  }
  
  # Handle overwrite
  if (file.exists(path)) {
    if (!overwrite) {
      # If file exists but dataset does not, we can still add dataset; but simplest is to stop.
      if (h5_exists_dataset(path, spec$dataset)) {
        .stopf("HDF5 file exists and dataset exists: %s [%s]. Set overwrite=TRUE to rebuild.",
               path, spec$dataset)
      }
    } else {
      # remove the file entirely to avoid half-old layouts
      unlink(path)
    }
  }
  
  # Ensure folder exists
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  # Create file
  rhdf5::h5createFile(path)
  
  # Ensure group exists (e.g., "/")
  grp <- dirname(spec$dataset)
  if (!identical(grp, "/")) {
    # create nested groups if needed
    parts <- strsplit(sub("^/+", "", grp), "/", fixed = TRUE)[[1]]
    cur <- "/"
    for (p in parts) {
      nxt <- file.path(cur, p)
      if (!rhdf5::h5exists(path, nxt)) rhdf5::h5createGroup(path, nxt)
      cur <- nxt
    }
  }
  
  dims_vec <- as.integer(dim_lengths[spec$dims])
  maxdims  <- resolve_maxdims(spec, dim_lengths)
  chunk    <- resolve_chunk(spec, dim_lengths)
  level    <- if (!is.null(spec$gzip_level)) as.integer(spec$gzip_level) else NULL
  
  # Create dataset
  rhdf5::h5createDataset(
    file = path,
    dataset = spec$dataset,
    dims = dims_vec,
    maxdims = maxdims,
    storage.mode = spec$storage_mode %||% "integer",
    chunk = chunk,
    level = level
  )
  
  # Attributes (helpful for debugging / validation)
  # Don't store dimnames (we keep those in /data), but store the contract.
  rhdf5::h5writeAttribute(attr = spec$source_key,  h5obj = path, h5loc = spec$dataset, name = "source_key")
  rhdf5::h5writeAttribute(attr = spec$type_key,    h5obj = path, h5loc = spec$dataset, name = "type_key")
  rhdf5::h5writeAttribute(attr = spec$geo,         h5obj = path, h5loc = spec$dataset, name = "geo")
  rhdf5::h5writeAttribute(attr = spec$dims,        h5obj = path, h5loc = spec$dataset, name = "dims")
  rhdf5::h5writeAttribute(attr = isTRUE(spec$extendable_year), h5obj = path, 
                          h5loc = spec$dataset, name = "extendable_year")
  
  if (!is.null(attributes)) {
    nms <- names(attributes)
    for (nm in names(attributes)) {
      rhdf5::h5writeAttribute(attr = attributes[[nm]], h5obj = path, h5loc = spec$dataset, name = nm)
    }
    
  }
  
  invisible(TRUE)
}

# Null-coalescing operator for convenience
`%||%` <- function(x, y) if (is.null(x)) y else x

h5_dim <- function(path, dataset) {
  require_rhdf5()
  info <- rhdf5::h5ls(path)
  i <- which(info$group == dirname(dataset) & info$name == basename(dataset))
  if (!length(i)) .stopf("Dataset not found: %s [%s]", path, dataset)
  # rhdf5 doesn't directly return dims in h5ls; we can read with h5readAttributes?
  # easiest: use h5read with index to infer, but expensive.
  # We'll use h5readAttributes + rely on dim_lengths from spec for build-time operations.
  attr <- rhdf5::h5readAttributes(path, dataset)
  list(attributes = attr)
}

# writing slabs ----------------------------

# Internal: build a hyperslab "index" list for h5write.
# We write along the first dimension at position i (scalar), and full ranges for others.
.build_index_for_slab <- function(spec, i, dim_lengths) {
  if (!is.numeric(i) || length(i) != 1L) .stopf("i must be a single numeric index.")
  if (i < 1L) .stopf("i must be >= 1.")
  idx <- vector("list", length(spec$dims))
  names(idx) <- spec$dims
  idx[[1]] <- as.integer(i)
  if (length(spec$dims) > 1L) {
    for (k in 2:length(spec$dims)) {
      idx[[k]] <- 1:as.integer(dim_lengths[[spec$dims[[k]]]])
    }
  }
  idx
}

# Validate slab dimensions match the dataset (except the leading dim which is 1 for a single index).
.validate_slab_shape <- function(spec, slab, dim_lengths) {
  if (is.null(dim(slab))) .stopf("slab must be an array with dim().")
  expected <- as.integer(dim_lengths[spec$dims][-1])
  got <- as.integer(dim(slab))
  
  if (length(got) != length(expected)) {
    .stopf("Spec '%s': slab has %d dims; expected %d (all dims except leading '%s').",
           spec$series_id, length(got), length(expected), leading_dim(spec))
  }
  if (!identical(got, expected)) {
    .stopf("Spec '%s': slab dims mismatch. Got (%s) expected (%s).",
           spec$series_id,
           paste(got, collapse = "x"),
           paste(expected, collapse = "x"))
  }
  invisible(TRUE)
}

# Write one slab at leading-dimension index i.
# slab is an array with dims equal to all dimensions except the first dim.
h5_write_slab <- function(path, spec, dim_lengths, i, slab) {
  require_rhdf5()
  
  if (!h5_exists_dataset(path, spec$dataset)) {
    .stopf("Dataset not found. Create first with h5_create_cube(): %s [%s]", path, spec$dataset)
  }
  
  .validate_slab_shape(spec, slab, dim_lengths)
  idx <- .build_index_for_slab(spec, i, dim_lengths)
  
  if (inherits(slab, c("table", "xtabs"))) {
    slab <- unclass(slab)  # drops table class; keeps array + dimnames
  }
  
  # enforce storage mode expected by dataset
  if (identical(spec$storage_mode %||% "integer", "integer")) {
    slab <- as.integer(slab)
  } else if (identical(spec$storage_mode, "double")) {
    slab <- as.double(slab)
  }
  
  # h5write expects the in-memory array to align with the selection shape
  rhdf5::h5write(
    obj = slab,
    file = path,
    name = spec$dataset,
    index = idx
  )
  
  invisible(TRUE)
}


# append workflows (extend + write) ----------------------------
# Extend the leading dimension by one (or to an explicit new size).
h5_extend_leading <- function(path, spec, new_size, dim_lengths) {
  require_rhdf5()
  if (!isTRUE(spec$extendable_year)) {
    .stopf("Spec '%s' is not marked extendable_year=TRUE.", spec$series_id)
  }
  
  cur <- as.integer(dim_lengths[leading_dim(spec)])
  if (new_size < cur) .stopf("new_size (%d) is smaller than current size (%d).", new_size, cur)
  
  # rhdf5::h5set_extent expects full dims vector
  dims_new <- as.integer(dim_lengths[spec$dims])
  dims_new[1] <- as.integer(new_size)
  
  rhdf5::h5set_extent(file = path, dataset = spec$dataset, size = dims_new)
  invisible(TRUE)
}

# Append one slab at the end of the leading dimension.
# You must update your dim label vector elsewhere (labels_build.R) and/or pass label tracking
# in your build script; HDF5 file itself only stores numeric values.
h5_append_slab <- function(path, spec, dim_lengths, slab) {
  require_rhdf5()
  
  if (!isTRUE(spec$extendable_year)) {
    .stopf("Spec '%s' is not marked extendable_year=TRUE; cannot append.", spec$series_id)
  }
  
  # Current size in leading dim and compute new index
  cur <- as.integer(dim_lengths[leading_dim(spec)])
  new_i <- cur + 1L
  
  # Extend on disk
  dim_lengths2 <- dim_lengths
  dim_lengths2[leading_dim(spec)] <- new_i
  h5_extend_leading(path, spec, new_size = new_i, dim_lengths = dim_lengths2)
  
  # Write at new index
  h5_write_slab(path, spec, dim_lengths = dim_lengths2, i = new_i, slab = slab)
  
  invisible(new_i)
}

# convenience: rebuild from scratch----------------------------
h5_rebuild_cube <- function(path,
                            spec,
                            dim_lengths,
                            overwrite = TRUE,
                            attributes = NULL) {
  h5_create_cube(path, spec, dim_lengths = dim_lengths, overwrite = overwrite, attributes = attributes)
  invisible(TRUE)
}

# OPTIONAL: full write (in-memory cube) ----------------------------

# If you have the full cube array in memory (dims include all dims in spec$dims),
# this writes it in one shot.
h5_write_full <- function(path, spec, cube, overwrite = FALSE, attributes = NULL) {
  require_rhdf5()
  
  if (is.null(dim(cube))) .stopf("cube must be an array.")
  if (length(dim(cube)) != length(spec$dims)) {
    .stopf("Spec '%s': cube dims count mismatch.", spec$series_id)
  }
  
  dim_lengths <- setNames(as.integer(dim(cube)), spec$dims)
  
  if (!h5_exists_dataset(path, spec$dataset) || overwrite) {
    h5_create_cube(path, spec, dim_lengths = dim_lengths, overwrite = overwrite, attributes = attributes)
  }
  
  rhdf5::h5write(obj = cube, file = path, name = spec$dataset)
  invisible(TRUE)
}


