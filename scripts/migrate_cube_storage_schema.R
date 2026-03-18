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
#   Rscript scripts/migrate_cube_storage_schema.R --to-1.1
#   Rscript scripts/migrate_cube_storage_schema.R --to-1.1 --apply

args <- commandArgs(trailingOnly = TRUE)
apply_mode <- "--apply" %in% args
upgrade_to_1_1 <- "--to-1.1" %in% args

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

h5_coalesce_chr <- function(x, default = "") {
  if (is.null(x) || !length(x)) return(default)
  out <- as.character(x[[1L]])
  if (!nzchar(out)) default else out
}

h5_read_scalar_if_present <- function(file, name, default = NULL) {
  out <- tryCatch(rhdf5::h5read(file, name), error = function(e) NULL)
  if (is.null(out) || !length(out)) return(default)
  as.character(out[[1L]])
}

h5_group_exists <- function(file, group) {
  grp <- if (startsWith(group, "/")) group else paste0("/", group)
  info <- tryCatch(rhdf5::h5ls(file), error = function(e) NULL)
  if (is.null(info)) return(FALSE)
  parent <- dirname(grp)
  if (identical(parent, ".")) parent <- "/"
  nm <- basename(grp)
  any(info$group == parent & info$name == nm)
}

h5_create_group <- function(file, group) {
  grp <- if (startsWith(group, "/")) sub("^/", "", group) else group
  if (!h5_group_exists(file, grp)) {
    try(rhdf5::h5createGroup(file = file, group = grp), silent = TRUE)
  }
  invisible(TRUE)
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

default_dim_domain <- function(dim_name, time_dim, area_dim) {
  if (identical(dim_name, time_dim)) return("time")
  if (identical(dim_name, area_dim)) return("area")
  dim_name
}

default_dim_scale_type <- function(dim_name, time_dim) {
  if (identical(dim_name, time_dim)) return("interval")
  "nominal"
}

read_current_dimnames <- function(path) {
  dim_order <- as.character(rhdf5::h5read(path, "cube/metadata/dim_order"))
  out <- lapply(dim_order, function(d) {
    as.character(rhdf5::h5read(path, paste0("cube/metadata/dimnames/", d)))
  })
  names(out) <- dim_order
  out
}

read_current_roles <- function(path) {
  list(
    time = as.character(rhdf5::h5read(path, "cube/metadata/roles/time"))[[1L]],
    area = as.character(rhdf5::h5read(path, "cube/metadata/roles/area"))[[1L]],
    strata = as.character(rhdf5::h5read(path, "cube/metadata/roles/strata"))
  )
}

read_current_source <- function(path) {
  list(
    note = h5_read_scalar_if_present(path, "cube/metadata/source/note", default = ""),
    source = h5_read_scalar_if_present(path, "cube/metadata/source/source", default = "Not given"),
    updated = h5_read_scalar_if_present(path, "cube/metadata/source/updated", default = as.character(Sys.Date())),
    population_type = h5_read_scalar_if_present(path, "cube/metadata/source/population_type", default = "Unknown")
  )
}

read_current_registry <- function(path) {
  info <- tryCatch(rhdf5::h5ls(path, recursive = TRUE), error = function(e) NULL)
  if (is.null(info)) return(list())
  reg_rows <- info[info$group == "/cube/metadata/registry", , drop = FALSE]
  if (nrow(reg_rows) == 0L) return(list())

  out <- vector("list", nrow(reg_rows))
  names(out) <- reg_rows$name
  for (nm in reg_rows$name) {
    out[[nm]] <- h5_read_scalar_if_present(path, paste0("cube/metadata/registry/", nm), default = "")
  }
  out
}

read_legacy_dim_semantics <- function(path, dim_order, time_dim, area_dim) {
  out <- lapply(dim_order, function(d) {
    base <- paste0("cube/metadata/dim_semantics/", d)
    cls <- h5_read_scalar_if_present(path, paste0(base, "/class"), default = "unknown")
    partition_type <- switch(cls, partition = "partition", set = "set", "unknown")
    validated <- tolower(h5_read_scalar_if_present(path, paste0(base, "/validated"), default = "false")) == "true"
    list(
      dim_name = d,
      domain = default_dim_domain(d, time_dim, area_dim),
      scale_type = default_dim_scale_type(d, time_dim),
      partition_type = partition_type,
      validated = validated,
      overlap_levels = character(),
      notes = character()
    )
  })
  names(out) <- dim_order
  out
}

write_dim_semantics_fieldwise <- function(path, dim_semantics, dim_order) {
  h5_create_group(path, "cube/metadata/dim_semantics")
  for (d in dim_order) {
    base <- paste0("cube/metadata/dim_semantics/", d)
    h5_create_group(path, base)
    ent <- dim_semantics[[d]]
    h5_write_dataset(path, paste0(base, "/dim_name"), as.character(ent$dim_name))
    h5_write_dataset(path, paste0(base, "/domain"), as.character(ent$domain))
    h5_write_dataset(path, paste0(base, "/scale_type"), as.character(ent$scale_type))
    h5_write_dataset(path, paste0(base, "/partition_type"), as.character(ent$partition_type))
    h5_write_dataset(path, paste0(base, "/validated"), as.character(ent$validated))
    h5_write_dataset(path, paste0(base, "/overlap_levels"), as.character(ent$overlap_levels))
    h5_write_dataset(path, paste0(base, "/notes"), as.character(ent$notes))
  }
  invisible(TRUE)
}

write_registry_group <- function(path, registry) {
  if (!length(registry)) return(invisible(TRUE))
  h5_create_group(path, "cube/metadata/registry")
  for (nm in names(registry)) {
    h5_write_dataset(path, paste0("cube/metadata/registry/", nm), as.character(registry[[nm]]))
  }
  invisible(TRUE)
}

write_metadata_group_v11 <- function(path,
                                     dimn,
                                     roles,
                                     source,
                                     series_id = NULL,
                                     geo = NULL,
                                     extendable_year = NULL,
                                     data_col = "population",
                                     dim_semantics) {
  h5_create_group(path, "cube/metadata")
  h5_create_group(path, "cube/metadata/roles")
  h5_create_group(path, "cube/metadata/source")
  h5_create_group(path, "cube/metadata/dimnames")

  dim_order <- names(dimn)

  h5_write_dataset(path, "cube/metadata/schema_version", "1.1.0")
  h5_write_dataset(path, "cube/metadata/migrated_on", as.character(Sys.time()))
  if (!is.null(data_col) && nzchar(as.character(data_col))) {
    h5_write_dataset(path, "cube/metadata/data_col", as.character(data_col))
  }
  if (!is.null(series_id) && nzchar(as.character(series_id))) {
    h5_write_dataset(path, "cube/metadata/series_id", as.character(series_id))
  }
  if (!is.null(geo) && nzchar(as.character(geo))) {
    h5_write_dataset(path, "cube/metadata/geo", as.character(geo))
  }
  if (!is.null(extendable_year) && nzchar(as.character(extendable_year))) {
    h5_write_dataset(path, "cube/metadata/extendable_year", as.character(extendable_year))
  }

  h5_write_dataset(path, "cube/metadata/roles/time", as.character(roles$time))
  h5_write_dataset(path, "cube/metadata/roles/area", as.character(roles$area))
  h5_write_dataset(path, "cube/metadata/roles/strata", as.character(roles$strata))
  h5_write_dataset(path, "cube/metadata/dim_order", as.character(dim_order))

  for (d in dim_order) {
    h5_write_dataset(path, paste0("cube/metadata/dimnames/", d), as.character(dimn[[d]]))
  }

  h5_write_dataset(path, "cube/metadata/source/note", as.character(source$note))
  h5_write_dataset(path, "cube/metadata/source/source", as.character(source$source))
  h5_write_dataset(path, "cube/metadata/source/updated", as.character(source$updated))
  h5_write_dataset(path, "cube/metadata/source/population_type", as.character(source$population_type))
  write_dim_semantics_fieldwise(path, dim_semantics, dim_order)
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

validate_upgraded_file <- function(path, expected_dim, expected_dimn, expected_roles) {
  validate_migrated_file(path, expected_dim = expected_dim, expected_dimn = expected_dimn)

  schema_version <- h5_read_scalar_if_present(path, "cube/metadata/schema_version", default = NA_character_)
  if (!identical(schema_version, "1.1.0")) {
    stop("schema_version mismatch after upgrade for ", basename(path))
  }

  got_time <- h5_read_scalar_if_present(path, "cube/metadata/roles/time", default = NA_character_)
  got_area <- h5_read_scalar_if_present(path, "cube/metadata/roles/area", default = NA_character_)
  got_strata <- as.character(rhdf5::h5read(path, "cube/metadata/roles/strata"))
  if (!identical(got_time, expected_roles$time)) stop("time role mismatch after upgrade for ", basename(path))
  if (!identical(got_area, expected_roles$area)) stop("area role mismatch after upgrade for ", basename(path))
  if (!identical(got_strata, expected_roles$strata)) stop("strata roles mismatch after upgrade for ", basename(path))

  info <- rhdf5::h5ls(path, recursive = TRUE)
  if (any(info$group == "/cube/metadata" & info$name == "registry")) {
    stop("Legacy registry group still present after upgrade for ", basename(path))
  }
  if (!any(info$group == "/cube/metadata" & info$name == "data_col")) {
    stop("Missing data_col after upgrade for ", basename(path))
  }
  for (d in names(expected_dimn)) {
    base <- paste0("/cube/metadata/dim_semantics/", d)
    required <- c("dim_name", "domain", "scale_type", "partition_type", "validated", "overlap_levels", "notes")
    present <- paste0(info$group, "/", info$name)
    missing <- required[!paste0(base, "/", required) %in% present]
    if (length(missing) > 0L) {
      stop("Missing 1.1.0 dim_semantics fields for ", basename(path), " dim ", d, ": ", paste(missing, collapse = ", "))
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

upgrade_one_to_1_1 <- function(file_path, backup_dir, apply_mode = FALSE) {
  schema_version <- h5_read_scalar_if_present(file_path, "cube/metadata/schema_version", default = NA_character_)
  if (!(schema_version %in% c("1.0.0", "1.1.0"))) {
    stop("Unsupported schema_version for upgrade: ", basename(file_path), " -> ", schema_version)
  }

  cube <- HDF5Array::HDF5Array(file_path, "cube/population")
  dimn <- read_current_dimnames(file_path)
  roles <- read_current_roles(file_path)
  source <- read_current_source(file_path)
  registry <- read_current_registry(file_path)
  series_id <- h5_read_scalar_if_present(file_path, "cube/metadata/series_id", default = h5_coalesce_chr(registry$series_id, ""))
  geo <- h5_read_scalar_if_present(file_path, "cube/metadata/geo", default = h5_coalesce_chr(registry$geo, ""))
  extendable_year <- h5_read_scalar_if_present(file_path, "cube/metadata/extendable_year", default = h5_coalesce_chr(registry$extendable_year, ""))
  data_col <- h5_read_scalar_if_present(file_path, "cube/metadata/data_col", default = "population")
  dim_semantics <- read_legacy_dim_semantics(
    path = file_path,
    dim_order = names(dimn),
    time_dim = roles$time,
    area_dim = roles$area
  )

  if (!apply_mode) {
    message(
      "[DRY RUN] normalize ", basename(file_path),
      " schema ", schema_version, " -> 1.1.0 (",
      paste(dim(cube), collapse = " x "), ")"
    )
    return(invisible(TRUE))
  }

  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  backup_path <- file.path(backup_dir, basename(file_path))
  ok_backup <- file.copy(file_path, backup_path, overwrite = FALSE)
  if (!ok_backup) stop("Failed to backup file: ", file_path)

  tmp <- tempfile(pattern = "cube_upgrade_", fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  rhdf5::h5createFile(tmp)
  rhdf5::h5createGroup(tmp, "cube")
  HDF5Array::writeHDF5Array(cube, filepath = tmp, name = "cube/population")
  write_metadata_group_v11(
        path = tmp,
        dimn = dimn,
        roles = roles,
        source = source,
        series_id = series_id,
        geo = geo,
        extendable_year = extendable_year,
    data_col = data_col,
    dim_semantics = dim_semantics
  )
  validate_upgraded_file(
    path = tmp,
    expected_dim = dim(cube),
    expected_dimn = dimn,
    expected_roles = roles
  )

  ok_replace <- file.copy(tmp, file_path, overwrite = TRUE)
  if (!ok_replace) stop("Failed to overwrite upgraded file: ", file_path)

  message("[APPLY] normalized ", basename(file_path), " schema ", schema_version, " -> 1.1.0")
  invisible(TRUE)
}

if (upgrade_to_1_1) {
  files <- sort(Sys.glob(file.path(extdata_dir, "*.h5")))
  for (file_path in files) {
    upgrade_one_to_1_1(
      file_path = file_path,
      backup_dir = backup_dir,
      apply_mode = apply_mode
    )
  }
  if (apply_mode) {
    message("Schema 1.1.0 upgrade complete. Backups saved under: ", backup_dir)
  } else {
    message("Dry run complete. Re-run with --to-1.1 --apply to perform backup + upgrade.")
  }
} else {
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
}
