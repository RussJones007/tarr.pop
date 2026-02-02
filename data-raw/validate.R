# -------------------------------------------------------------------------------------->
# Script: validate.R
# Description:
#   Validation and QA checks for HDF5-backed population cubes built from series_specs.R
#   and label objects built/saved by labels_build.R.
#
# Design:
#   - Works for arbitrary dim counts (2D ZCTA, 5D TDC race.eth, 6D canonical).
#   - Validates that:
#       * spec structure is sane
#       * label objects exist and are consistent with spec
#       * HDF5 file + dataset exist
#       * HDF5 dataset rank/shape match label lengths
#       * key dataset attributes match the spec (series_id, dims, extendable_year)
#
# Notes:
#   - We do NOT validate numeric values here (totals, monotonicity, etc.) yet.
#     This script focuses on structural correctness and preventing dimname drift.
#   - Uses rhdf5 for inspecting datasets.
#   
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 4, 2026
# Revised:
# -------------------------------------------------------------------------------------->

#  Dependencies  ----------------------------

require_rhdf5 <- function() {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("Package 'rhdf5' is required for validation. Install via Bioconductor.", call. = FALSE)
  }
  invisible(TRUE)
}

.stopf <- function(...) stop(sprintf(...), call. = FALSE)
.warnf <- function(...) warning(sprintf(...), call. = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

#  Label resolution  ----------------------------

# In build-time scripts, label objects typically exist in the global env because you've
# sourced labels_build.R or loaded /data objects. We'll support both:
#   1) label exists in the global environment
#   2) label exists in installed package namespace (rare during development, but possible)
labels_get <- function(name, envir = parent.frame()) {
  # 0) Prefer build-time registry if it exists
  if (exists(".labels_env", inherits = TRUE)) {
    reg <- get(".labels_env", inherits = TRUE)
    if (is.environment(reg) && exists(name, envir = reg, inherits = FALSE)) {
      return(get(name, envir = reg, inherits = FALSE))
    }
  }
  
  # 1) Look in caller-supplied environment (your current behavior)
  if (exists(name, envir = envir, inherits = TRUE)) {
    return(get(name, envir = envir, inherits = TRUE))
  }
  
  # 2) Try package namespace if available (your current behavior)
  if (requireNamespace("tarr.pop", quietly = TRUE)) {
    ns <- asNamespace("tarr.pop")
    if (exists(name, envir = ns, inherits = FALSE)) {
      return(get(name, envir = ns, inherits = FALSE))
    }
  }
  
  .stopf("Label object '%s' not found (labels registry, caller env, nor package namespace).", name)
}

#  Spec validation helpers  ----------------------------

.validate_spec_minimal <- function(spec) {
  req <- c("series_id", "filename", "dataset", "dims", "label_keys", "storage_mode")
  missing <- setdiff(req, names(spec))
  if (length(missing)) {
    .stopf("Spec is missing required fields: %s", paste(missing, collapse = ", "))
  }
  if (!is.character(spec$series_id) || length(spec$series_id) != 1) .stopf("series_id must be scalar character.")
  if (!is.character(spec$filename)  || length(spec$filename)  != 1) .stopf("filename must be scalar character.")
  if (!is.character(spec$dataset)   || length(spec$dataset)   != 1) .stopf("dataset must be scalar character.")
  if (!is.character(spec$dims)      || length(spec$dims)      < 1)  .stopf("dims must be a non-empty character vector.")
  if (!is.list(spec$label_keys)     || length(spec$label_keys) < 1) .stopf("label_keys must be a non-empty list.")
  if (is.null(names(spec$label_keys))) .stopf("label_keys must be a *named* list keyed by dim names.")
  if (!all(spec$dims %in% names(spec$label_keys))) {
    .stopf("Spec '%s': label_keys missing entries for dims: %s",
           spec$series_id, paste(setdiff(spec$dims, names(spec$label_keys)), collapse = ", "))
  }
  if (!all(names(spec$label_keys) %in% spec$dims)) {
    .stopf("Spec '%s': label_keys contains names not in dims: %s",
           spec$series_id, paste(setdiff(names(spec$label_keys), spec$dims), collapse = ", "))
  }
  invisible(TRUE)
}

#  Label validation  ----------------------------

validate_labels_for_spec <- function(spec, envir = parent.frame()) {
  .validate_spec_minimal(spec)
  
  # Fetch all label vectors and confirm type + uniqueness
  dims <- spec$dims
  label_names <- spec$label_keys[dims]
  
  label_vecs <- lapply(label_names, labels_get, envir = envir)
  names(label_vecs) <- dims
  
  for (d in dims) {
    v <- label_vecs[[d]]
    if (!is.character(v)) .stopf("Spec '%s': label '%s' for dim '%s' must be character.", spec$series_id, label_names[[d]], d)
    if (anyNA(v)) .stopf("Spec '%s': label '%s' contains NA.", spec$series_id, label_names[[d]])
    if (anyDuplicated(v)) .stopf("Spec '%s': label '%s' contains duplicates.", spec$series_id, label_names[[d]])
    if (!length(v)) .stopf("Spec '%s': label '%s' is empty.", spec$series_id, label_names[[d]])
  }
  
  # Provide dim lengths as named integer vector in spec dim order
  dim_lengths <- setNames(as.integer(vapply(label_vecs, length, integer(1))), dims)
  
  list(label_names = label_names, labels = label_vecs, dim_lengths = dim_lengths)
}

#  HDF5 dataset validation  ----------------------------

# Read dataset dimensions without reading full data.
# We use h5dump with load=FALSE which returns a structure containing $dim.
h5_dataset_dim <- function(path, dataset) {
  require_rhdf5()
  if (!file.exists(path)) .stopf("HDF5 file not found: %s", path)
  
  out <- rhdf5::h5dump(file = path, load = FALSE, pattern = dataset)
  # h5dump returns a nested list; dataset might be under its name without leading slash.
  # We'll navigate by splitting the path.
  parts <- strsplit(sub("^/+", "", dataset), "/", fixed = TRUE)[[1]]
  node <- out
  for (p in parts) {
    if (!is.list(node) || !p %in% names(node)) .stopf("Dataset '%s' not found in %s.", dataset, path)
    node <- node[[p]]
  }
  #d <- attr(node, "dim")
  d <- node[["dim"]]
  if (is.null(d)) .stopf("Could not determine dimensions for dataset '%s' in %s.", dataset, path)
  # Make d a vector
  d <- strsplit(x = d, split = "x") |> unlist()
  as.integer(d)
}

h5_dataset_attrs <- function(path, dataset) {
  require_rhdf5()
  rhdf5::h5readAttributes(file = path, name = dataset)
}

validate_h5_for_spec <- function(spec,
                                 root = "inst/extdata",
                                 envir = parent.frame(),
                                 check_attributes = TRUE) {
  .validate_spec_minimal(spec)
  
  # Resolve labels to get expected dim lengths
  lbl <- validate_labels_for_spec(spec, envir = envir)
  expected_dims <- unname(lbl$dim_lengths)
  
  # Locate file
  path <- file.path(root, spec$filename)
  if (!file.exists(path)) .stopf("Spec '%s': HDF5 file missing: %s", spec$series_id, path)
  
  # Check dataset exists + rank/shape
  got_dims <- h5_dataset_dim(path, spec$dataset)
  
  if (length(got_dims) != length(expected_dims)) {
    .stopf(
      "Spec '%s': dataset rank mismatch. Expected %d dims (%s), got %d dims (%s).",
      spec$series_id,
      length(expected_dims), paste(spec$dims, collapse = ","),
      length(got_dims), paste(got_dims, collapse = "x")
    )
  }
  
  if (!identical(as.integer(got_dims), as.integer(expected_dims))) {
    .stopf(
      "Spec '%s': dataset shape mismatch.\n  Expected: %s\n  Got:      %s",
      spec$series_id,
      paste(expected_dims, collapse = "x"),
      paste(got_dims, collapse = "x")
    )
  }
  
  # Optional: attribute checks
  if (isTRUE(check_attributes)) {
    at <- h5_dataset_attrs(path, spec$dataset)
    
    # Only check if attribute exists (allows you to validate older files too)
    if (!is.null(at$series_id) && !identical(as.character(at$series_id), spec$series_id)) {
      .stopf("Spec '%s': attribute series_id mismatch (file has '%s').", spec$series_id, at$series_id)
    }
    if (!is.null(at$dims)) {
      # stored dims may come back as list/char; normalize
      stored <- as.character(unlist(at$dims))
      if (!identical(stored, spec$dims)) {
        .stopf("Spec '%s': attribute dims mismatch.\n  Expected: %s\n  Got:      %s",
               spec$series_id,
               paste(spec$dims, collapse = ","),
               paste(stored, collapse = ","))
      }
    }
    if (!is.null(at$extendable_year)) {
      if (!identical(as.logical(at$extendable_year), isTRUE(spec$extendable_year))) {
        .stopf("Spec '%s': attribute extendable_year mismatch.", spec$series_id)
      }
    }
  }
  
  invisible(list(
    series_id = spec$series_id,
    file = path,
    dataset = spec$dataset,
    dims = spec$dims,
    shape = got_dims,
    labels = lbl$label_names
  ))
}

#  Batch validation  ----------------------------

validate_all_specs <- function(specs) {
  ids <- vapply(specs, `[[`, character(1), "series_id")
  if (anyDuplicated(ids)) .stopf("Duplicate series_id in specs: %s", paste(unique(ids[duplicated(ids)]), collapse = ", "))
  invisible(TRUE)
}

validate_all_h5 <- function(specs,
                            root = "inst/extdata",
                            envir = parent.frame(),
                            check_attributes = TRUE) {
  validate_all_specs(specs)
  
  results <- vector("list", length(specs))
  names(results) <- names(specs)
  
  for (nm in names(specs)) {
    spec <- specs[[nm]]
    results[[nm]] <- validate_h5_for_spec(
      spec = spec,
      root = root,
      envir = envir,
      check_attributes = check_attributes
    )
  }
  
  invisible(results)
}

#  Optional: quick reporting  ----------------------------

print_validation_summary <- function(results) {
  # results from validate_all_h5()
  ok <- vapply(results, function(x) !is.null(x$file), logical(1))
  cat(sprintf("Validated %d series.\n", sum(ok)))
  invisible(TRUE)
}

