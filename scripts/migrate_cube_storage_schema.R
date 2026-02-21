#!/usr/bin/env Rscript

# Migration utility for Part 1 in inst/TOC.md:
# Move poparray metadata from package .rda objects into each cube HDF5 file.
#
# New schema per file:
#   /cube/population               (numeric cube dataset)
#   /cube/metadata/...             (registry/source/roles/dimnames metadata)
#
# Usage:
#   Rscript scripts/migrate_cube_storage_schema.R
#   Rscript scripts/migrate_cube_storage_schema.R --apply

args <- commandArgs(trailingOnly = TRUE)
apply_mode <- "--apply" %in% args

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(repo_root, "data")
extdata_dir <- file.path(repo_root, "inst", "extdata")
backup_dir <- file.path(
  extdata_dir,
  "backup_pre_cube_schema",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

if (!dir.exists(data_dir)) stop("Missing data directory: ", data_dir)
if (!dir.exists(extdata_dir)) stop("Missing extdata directory: ", extdata_dir)

if (!requireNamespace("rhdf5", quietly = TRUE)) {
  stop("Package 'rhdf5' is required for migration.")
}
if (!requireNamespace("HDF5Array", quietly = TRUE)) {
  stop("Package 'HDF5Array' is required for migration.")
}

load_rda_file <- function(path) {
  e <- new.env(parent = emptyenv())
  nms <- load(path, envir = e)
  if (length(nms) != 1L) {
    stop("Expected exactly one object in ", basename(path), "; got: ", paste(nms, collapse = ", "))
  }
  e[[nms[[1L]]]]
}

metadata_catalog <- function(data_dir) {
  rda_paths <- Sys.glob(file.path(data_dir, "*.rda"))
  out <- list()
  for (p in rda_paths) {
    nm <- sub("\\.rda$", "", basename(p))
    out[[nm]] <- load_rda_file(p)
  }
  out
}

source_name_for <- function(source_key) {
  switch(
    source_key,
    census = "US Census Bureau",
    seer = "National Cancer Institute; SEER Program",
    tdc = "Texas Demographic Center",
    "Unknown source"
  )
}

candidate_dimnames <- function(series_id, meta) {
  switch(
    series_id,
    census_decennial_county_1y = list(
      year = meta$years_census,
      area.name = meta$county_levels,
      sex = meta$sex_levels_census,
      age.char = meta$ages_census_1y,
      race = meta$race_levels_census,
      ethnicity = meta$ethnicity_levels_census
    ),
    census_estimates_county_5y = list(
      year = meta$years_census_estimates,
      area.name = meta$county_levels,
      sex = meta$sex_levels_census,
      age.char = meta$ages_census_5y,
      race = meta$race_levels_census_estimates,
      ethnicity = meta$ethnicity_levels_census_estimates
    ),
    census_zcta_estimates = list(
      end.year = meta$zcta_end_year_levels,
      zip.code = meta$zcta_levels
    ),
    seer_estimates_county_1y = list(
      year = meta$years_seer,
      area.name = meta$county_levels,
      sex = meta$sex_levels_seer,
      age.char = meta$ages_seer_1y,
      race = meta$race_levels_seer,
      ethnicity = meta$ethnicity_levels_seer
    ),
    seer_estimates_county_5y = list(
      year = meta$years_seer,
      area.name = meta$county_levels,
      sex = meta$sex_levels_seer,
      age.char = meta$ages_seer_5y,
      race = meta$race_levels_seer,
      ethnicity = meta$ethnicity_levels_seer
    ),
    tdc_estimates_county = list(
      year = meta$years_tdc_estimates,
      area.name = meta$county_levels,
      sex = meta$sex_levels_tdc,
      age.char = meta$ages_tdc_mixed,
      race.eth = meta$race_eth_levels_tdc_estimates
    ),
    tdc_projections_county = list(
      year = meta$years_tdc_projections,
      area.name = meta$county_levels,
      sex = meta$sex_levels_tdc,
      age.char = meta$ages_tdc_1y,
      race.eth = meta$race_eth_levels_tdc_projections
    ),
    stop("No dimname mapping defined for series_id: ", series_id)
  )
}

align_cube_to_dimnames <- function(cube, dimn, series_id) {
  cube_dim <- dim(cube)
  if (length(dimn) != length(cube_dim)) {
    stop(
      "Dim count mismatch for ", series_id, ": metadata has ",
      length(dimn), " dims, cube has ", length(cube_dim), "."
    )
  }

  for (i in seq_along(cube_dim)) {
    target_len <- length(dimn[[i]])
    current_len <- cube_dim[[i]]
    dim_name <- names(dimn)[[i]]

    if (current_len == target_len) next

    if (current_len == target_len + 1L) {
      # Legacy cubes may include one extra aggregate level that is not
      # represented in metadata vectors. Drop the terminal level.
      drop_idx <- current_len
      keep_idx <- setdiff(seq_len(current_len), drop_idx)
      subs <- rep(list(TRUE), length(cube_dim))
      subs[[i]] <- keep_idx
      cube <- do.call(`[`, c(list(cube), subs, list(drop = FALSE)))
      cube_dim <- dim(cube)
      message(
        "[INFO] ", series_id, ": removed terminal level index ", drop_idx,
        " from dim '", dim_name, "'."
      )
      next
    }

    stop(
      "Label length mismatch for ", series_id, " dim ", dim_name,
      ": labels=", target_len, ", cube=", current_len, "."
    )
  }

  list(cube = cube, dimn = dimn)
}

roles_for_dimnames <- function(dimn) {
  nms <- names(dimn)
  time_dim <- if ("year" %in% nms) "year" else if ("end.year" %in% nms) "end.year" else NA_character_
  area_dim <- if ("area.name" %in% nms) "area.name" else if ("zip.code" %in% nms) "zip.code" else NA_character_
  if (is.na(time_dim) || is.na(area_dim)) {
    stop("Unable to infer time/area roles from dims: ", paste(nms, collapse = ", "))
  }
  list(time = time_dim, area = area_dim, strata = setdiff(nms, c(time_dim, area_dim)))
}

h5_delete_if_exists <- function(file, name) {
  try(rhdf5::h5delete(file = file, name = name), silent = TRUE)
}

h5_write_dataset <- function(file, name, x) {
  h5_delete_if_exists(file, name)
  rhdf5::h5write(obj = x, file = file, name = name)
}

write_metadata_group <- function(path, row, dimn) {
  rhdf5::h5createGroup(path, "cube/metadata")
  rhdf5::h5createGroup(path, "cube/metadata/registry")
  rhdf5::h5createGroup(path, "cube/metadata/roles")
  rhdf5::h5createGroup(path, "cube/metadata/source")
  rhdf5::h5createGroup(path, "cube/metadata/dimnames")

  h5_write_dataset(path, "cube/metadata/schema_version", "1.0.0")
  h5_write_dataset(path, "cube/metadata/migrated_on", as.character(Sys.time()))

  row_list <- as.list(row)
  for (k in names(row_list)) {
    h5_write_dataset(path, paste0("cube/metadata/registry/", k), as.character(row_list[[k]]))
  }

  dim_order <- names(dimn)
  roles <- roles_for_dimnames(dimn)
  h5_write_dataset(path, "cube/metadata/roles/time", roles$time)
  h5_write_dataset(path, "cube/metadata/roles/area", roles$area)
  h5_write_dataset(path, "cube/metadata/roles/strata", as.character(roles$strata))
  h5_write_dataset(path, "cube/metadata/dim_order", dim_order)

  for (d in dim_order) {
    h5_write_dataset(path, paste0("cube/metadata/dimnames/", d), as.character(dimn[[d]]))
  }

  h5_write_dataset(path, "cube/metadata/source/note", as.character(row$series_id))
  h5_write_dataset(path, "cube/metadata/source/source", source_name_for(as.character(row$source_key)))
  h5_write_dataset(path, "cube/metadata/source/updated", as.character(Sys.Date()))
  h5_write_dataset(path, "cube/metadata/source/population_type", as.character(row$type_key))
}

validate_migrated_file <- function(path, expected_dim, expected_dimn) {
  pop_info <- rhdf5::h5ls(path)
  ok_pop <- any(pop_info$group == "/cube" & pop_info$name == "population")
  if (!ok_pop) stop("Missing /cube/population in migrated file: ", path)

  got_dim <- dim(HDF5Array::HDF5Array(path, "cube/population"))
  if (!identical(as.integer(got_dim), as.integer(expected_dim))) {
    stop("Dimension mismatch after migration for ", basename(path))
  }

  got_order <- rhdf5::h5read(path, "cube/metadata/dim_order")
  if (!identical(as.character(got_order), names(expected_dimn))) {
    stop("dim_order mismatch in migrated metadata for ", basename(path))
  }

  for (d in names(expected_dimn)) {
    lev <- rhdf5::h5read(path, paste0("cube/metadata/dimnames/", d))
    if (!identical(as.character(lev), as.character(expected_dimn[[d]]))) {
      stop("Dim labels mismatch for ", basename(path), " dim ", d)
    }
  }
  TRUE
}

migrate_one <- function(row, meta, extdata_dir, backup_dir, apply_mode = FALSE) {
  file_path <- file.path(extdata_dir, row$filename)
  if (!file.exists(file_path)) {
    stop("Missing file for series ", row$series_id, ": ", file_path)
  }

  old_dataset <- as.character(row$dataset)
  cube <- HDF5Array::HDF5Array(file_path, old_dataset)
  dimn <- candidate_dimnames(as.character(row$series_id), meta)
  aligned <- align_cube_to_dimnames(
    cube = cube,
    dimn = dimn,
    series_id = as.character(row$series_id)
  )
  cube <- aligned$cube
  dimn <- aligned$dimn
  cube_dim <- dim(cube)

  if (!apply_mode) {
    message("[DRY RUN] ", row$series_id, " validated: ", paste(cube_dim, collapse = " x "))
    return(invisible(TRUE))
  }

  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  backup_path <- file.path(backup_dir, row$filename)
  ok_backup <- file.copy(file_path, backup_path, overwrite = FALSE)
  if (!ok_backup) stop("Failed to backup file: ", file_path)

  tmp <- tempfile(pattern = "cube_migration_", fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  rhdf5::h5createFile(tmp)
  rhdf5::h5createGroup(tmp, "cube")
  HDF5Array::writeHDF5Array(cube, filepath = tmp, name = "cube/population")
  write_metadata_group(tmp, row = row, dimn = dimn)
  validate_migrated_file(tmp, expected_dim = cube_dim, expected_dimn = dimn)

  ok_replace <- file.copy(tmp, file_path, overwrite = TRUE)
  if (!ok_replace) stop("Failed to overwrite migrated file: ", file_path)

  message("[APPLY] migrated ", row$series_id, " -> ", row$filename)
  invisible(TRUE)
}

meta <- metadata_catalog(data_dir)
if (is.null(meta$series_registry)) {
  stop("Object 'series_registry' was not found in data/series_registry.rda")
}
registry <- meta$series_registry

required_cols <- c("series_id", "filename", "dataset", "geo", "source_key", "type_key", "dim_order", "extendable_year")
missing_cols <- setdiff(required_cols, names(registry))
if (length(missing_cols) > 0L) {
  stop("series_registry missing required columns: ", paste(missing_cols, collapse = ", "))
}

for (i in seq_len(nrow(registry))) {
  row <- registry[i, , drop = FALSE]
  migrate_one(
    row = row,
    meta = meta,
    extdata_dir = extdata_dir,
    backup_dir = backup_dir,
    apply_mode = apply_mode
  )
}

if (apply_mode) {
  message("Migration complete. Backups saved under: ", backup_dir)
} else {
  message("Dry run complete. Re-run with --apply to perform backup + migration.")
}
