#' Inspect cube schema and metadata revisions
#'
#' Reads metadata only, without reading population values. Schema versions
#' describe storage fields; metadata revisions describe provenance and semantic
#' changes. Legacy cubes report revision zero until their first tracked update.
#' History is stored inside the HDF5 file, without historical population arrays.
#' @param path HDF5 cube path.
#' @return `cube_version()` returns a list with schema version, cube ID, and
#'   metadata revision. `cube_history()` returns a tibble of committed revisions.
#' @export
cube_version <- function(path) {
  checkmate::assert_file_exists(path)
  if (!pa_h5_group_exists(path, "cube/metadata") &&
      pa_h5_group_exists(path, "cube/metadata_previous")) {
    cli::cli_abort("Metadata replacement was interrupted; run {.fn recover_cube_metadata} before opening the cube.")
  }
  read <- function(key, default) {
    name <- paste0("cube/metadata/", key)
    if (!pa_h5_group_exists(path, name)) return(default)
    rhdf5::h5read(path, name)[[1L]]
  }
  list(schema_version = as.character(read("schema_version", "legacy")),
       cube_id = as.character(read("cube_id", NA_character_)),
       metadata_revision = as.integer(read("metadata_revision", 0L)))
}

#' @rdname cube_version
#' @export
cube_history <- function(path) {
  state <- pa_cube_history_state(path)
  if (!length(state$records)) return(tibble::tibble(revision = integer(),
    timestamp = character(), reason = character(), action = character(),
    schema_version = character(), time_from = character(),
    time_through = character(), time_levels = integer()))
  purrr::map_dfr(state$records, function(record) {
    labels <- record$dimnames[[record$metadata$roles$time]]
    tibble::tibble(revision = record$revision, timestamp = record$timestamp,
      reason = record$reason, action = record$action,
      schema_version = record$schema_version,
      time_from = if (length(labels)) labels[[1L]] else NA_character_,
      time_through = if (length(labels)) tail(labels, 1L) else NA_character_,
      time_levels = length(labels))
  })
}

#' Read a historical metadata snapshot
#' @param path HDF5 cube path.
#' @param revision Positive integer metadata revision.
#' @return A list containing metadata, dimension labels, schema version,
#'   revision, timestamp, reason, and action. Historical labels describe coverage
#'   at that revision; they do not imply historical population values exist.
#' @export
cube_metadata_at <- function(path, revision) {
  checkmate::assert_int(revision, lower = 1L)
  state <- pa_cube_history_state(path)
  if (revision > length(state$records)) cli::cli_abort("Metadata revision {.val {revision}} is unavailable.")
  record <- state$records[[revision]]
  record$metadata$dim_semantics <- ensure_dim_semantics(
    record$metadata$dim_semantics, names(record$dimnames),
    record$metadata$roles$time, record$metadata$roles$area)
  record
}

#' Update cube metadata with a recorded reason
#'
#' Validates and stages a complete metadata bundle before replacing the current
#' metadata group. The previous group is retained during replacement for error
#' recovery. This is a single-writer operation; concurrent edits are unsupported.
#' A process interruption during group replacement may require
#' `recover_cube_metadata()` before reopening the cube.
#' @param path HDF5 cube path.
#' @param metadata Bundle accepted by `cube_metadata<-`.
#' @param reason Non-empty description, such as the estimate vintage and years
#'   revised. No population values are read or changed.
#' @return Invisibly, the normalized cube path.
#' @export
update_cube_metadata <- function(path, metadata, reason) {
  require_cube_metadata_admin("update versioned cube metadata")
  checkmate::assert_string(reason, min.chars = 1L)
  if (pa_h5_group_exists(path, "cube/metadata_previous")) {
    recover_cube_metadata(path)
  }
  pa_assert_supported_cube_schema(path)
  meta <- get_cube_metadata_cached(path, refresh = TRUE)
  bundled <- normalize_cube_metadata_bundle(metadata, meta$dim_order, meta$dimnames)
  state <- pa_cube_history_state(path)
  if (!length(state$records)) {
    state$records <- list(pa_cube_snapshot(path, "Legacy metadata baseline", "baseline", 1L))
  }
  stage <- tempfile(fileext = ".h5")
  on.exit(unlink(stage), add = TRUE)
  rhdf5::h5createFile(stage)
  pa_h5_create_group(stage, "cube")
  pa_h5_copy(path, "cube/metadata", stage, "cube/metadata")
  write_cube_roles_metadata(stage, bundled$roles)
  write_cube_source_metadata(stage, bundled$source)
  pa_h5_write_dataset(stage, "cube/metadata/data_col", bundled$data_col)
  if (pa_h5_group_exists(stage, "cube/metadata/dim_semantics")) {
    rhdf5::h5delete(stage, "cube/metadata/dim_semantics")
  }
  pa_write_dim_semantics_fieldwise(stage, bundled$dim_semantics, meta$dim_order)
  pa_h5_write_dataset(stage, "cube/metadata/schema_version", "1.2.0")
  record <- pa_cube_snapshot(stage, reason, "metadata", length(state$records) + 1L)
  state$records <- c(state$records, list(record))
  pa_install_cube_history(stage, state)
  pa_h5_delete_if_exists(path, "cube/metadata_pending")
  pa_h5_copy(stage, "cube/metadata", path, "cube/metadata_pending")
  # Uncommitted records are invisible until the current group is replaced.
  pa_write_history_records(path, state$records)
  tryCatch({
    pa_h5_move(path, "cube/metadata", "cube/metadata_previous")
    pa_h5_move(path, "cube/metadata_pending", "cube/metadata")
  }, error = function(e) {
    if (!pa_h5_group_exists(path, "cube/metadata") &&
        pa_h5_group_exists(path, "cube/metadata_previous")) {
      pa_h5_move(path, "cube/metadata_previous", "cube/metadata")
    }
    reset_poparray_cache()
    stop(e)
  })
  pa_h5_delete_if_exists(path, "cube/metadata_previous")
  reset_poparray_cache()
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Restore historical metadata as a new revision
#'
#' Validates the snapshot against current dimension labels. Snapshots from
#' different coverage or dimension layouts cannot be restored. Population
#' values are never restored, and schema migrations are not reversed.
#' @param path HDF5 cube path.
#' @param revision Historical metadata revision.
#' @param reason Reason for restoration.
#' @return Invisibly, the normalized cube path.
#' @export
restore_cube_metadata <- function(path, revision, reason) {
  checkmate::assert_string(reason, min.chars = 1L)
  record <- cube_metadata_at(path, revision)
  current <- get_cube_metadata_cached(path, refresh = TRUE)
  if (!identical(record$dimnames, current$dimnames)) {
    cli::cli_abort("Historical dimension labels differ from the current cube; metadata cannot be restored.")
  }
  update_cube_metadata(path, record$metadata,
    paste0(reason, " (restored metadata revision ", revision, ")"))
}

#' Recover an interrupted metadata replacement
#' @param path HDF5 cube path.
#' @return Invisibly, the path. If the current group is absent, restores the
#'   previous metadata group; otherwise discards obsolete staging groups.
#' @export
recover_cube_metadata <- function(path) {
  require_cube_metadata_admin("recover cube metadata")
  checkmate::assert_file_exists(path)
  if (!pa_h5_group_exists(path, "cube/metadata")) {
    if (!pa_h5_group_exists(path, "cube/metadata_previous")) cli::cli_abort("No recoverable metadata group exists.")
    pa_h5_move(path, "cube/metadata_previous", "cube/metadata")
  }
  pa_h5_delete_if_exists(path, "cube/metadata_previous")
  pa_h5_delete_if_exists(path, "cube/metadata_pending")
  reset_poparray_cache()
  invisible(path)
}

#' Capture a plain-list metadata and coverage snapshot
#' @param path Cube file.
#' @param reason Change reason.
#' @param action Type of write.
#' @param revision Revision number.
#' @return Snapshot list; no population values are read.
#' @keywords internal
#' @noRd
pa_cube_snapshot <- function(path, reason, action, revision) {
  meta <- get_cube_metadata_cached(path, refresh = TRUE)
  bundle <- read_cube_metadata_admin_impl(path)
  bundle$dim_semantics <- purrr::map(bundle$dim_semantics, function(sem) {
    props <- S7::prop_names(sem)
    stats::setNames(lapply(props, function(p) S7::prop(sem, p)), props)
  })
  list(revision = as.integer(revision),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    schema_version = cube_version(path)$schema_version,
    reason = reason, action = action, metadata = bundle, dimnames = meta$dimnames)
}

#' Read committed metadata history and identity
#' @param path Cube file.
#' @return List containing identity and plain-list snapshots.
#' @keywords internal
#' @noRd
pa_cube_history_state <- function(path) {
  version <- cube_version(path)
  records <- lapply(seq_len(version$metadata_revision), function(i) {
    bytes <- rhdf5::h5read(path, paste0("cube/metadata_history/", i, "/snapshot"))
    unserialize(as.raw(bytes))
  })
  list(cube_id = if (is.na(version$cube_id)) {
    paste0("cube-", format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"), "-", basename(tempfile()))
  } else version$cube_id, records = records)
}

#' Persist snapshots without changing committed records
#' @param path Cube file.
#' @param records Snapshot list.
#' @return Invisibly NULL.
#' @keywords internal
#' @noRd
pa_write_history_records <- function(path, records) {
  pa_h5_create_group(path, "cube/metadata_history")
  purrr::walk(records, function(record) {
    base <- paste0("cube/metadata_history/", record$revision)
    if (record$revision <= cube_version(path)$metadata_revision &&
        pa_h5_group_exists(path, paste0(base, "/snapshot"))) return(invisible(NULL))
    pa_h5_create_group(path, base)
    purrr::walk(c("revision", "timestamp", "reason", "action", "schema_version"),
      function(key) pa_h5_write_dataset(path, paste0(base, "/", key), record[[key]]))
    pa_h5_write_dataset(path, paste0(base, "/snapshot_format"), "R-serialize-v2-plain-list")
    pa_h5_write_dataset(path, paste0(base, "/snapshot"),
      as.integer(serialize(record, NULL, version = 2L)))
  })
}

#' Install history and its committed revision marker in a staged cube
#' @param path Cube file.
#' @param state History and identity list.
#' @keywords internal
#' @noRd
pa_install_cube_history <- function(path, state) {
  pa_write_history_records(path, state$records)
  pa_h5_write_dataset(path, "cube/metadata/cube_id", state$cube_id)
  pa_h5_write_dataset(path, "cube/metadata/metadata_revision", length(state$records))
}

#' Finish a creation or replacement revision
#' @param path New cube file.
#' @param previous Optional prior history.
#' @param reason Change reason.
#' @param action Type of write.
#' @keywords internal
#' @noRd
pa_finish_cube_revision <- function(path, previous = NULL, reason = "Cube created", action = "create") {
  if (is.null(previous)) previous <- pa_cube_history_state(path)
  if (!length(previous$records) && !is.null(previous$baseline)) {
    previous$records <- list(previous$baseline)
  }
  record <- pa_cube_snapshot(path, reason, action, length(previous$records) + 1L)
  previous$records <- c(previous$records, list(record))
  pa_install_cube_history(path, previous)
  reset_poparray_cache()
}

#' Capture history before replacing population values
#' @param path Existing cube file.
#' @keywords internal
#' @noRd
pa_capture_cube_history <- function(path) {
  pa_assert_supported_cube_schema(path)
  state <- pa_cube_history_state(path)
  if (!length(state$records)) state$baseline <- pa_cube_snapshot(path, "Legacy metadata baseline", "baseline", 1L)
  state
}

#' Reject writes to unknown storage formats
#' @param path Cube file.
#' @keywords internal
#' @noRd
pa_assert_supported_cube_schema <- function(path) {
  version <- cube_version(path)$schema_version
  if (!version %in% c("legacy", "1.0.0", "1.1.0", "1.2.0")) {
    cli::cli_abort("Schema version {.val {version}} is unsupported for metadata writes or cube replacement.")
  }
}

#' Copy a metadata group between files
#' @param from Source file.
#' @param source Source group.
#' @param to Destination file.
#' @param destination Destination group.
#' @keywords internal
#' @noRd
pa_h5_copy <- function(from, source, to, destination) {
  input <- rhdf5::H5Fopen(from)
  on.exit(rhdf5::H5Fclose(input), add = TRUE)
  output <- rhdf5::H5Fopen(to, flags = "H5F_ACC_RDWR")
  on.exit(rhdf5::H5Fclose(output), add = TRUE)
  rhdf5::H5Ocopy(input, source, output, destination)
}

#' Move an HDF5 metadata link within a file
#' @param path Cube file.
#' @param from Source link.
#' @param to Destination link.
#' @keywords internal
#' @noRd
pa_h5_move <- function(path, from, to) {
  file <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDWR")
  on.exit(rhdf5::H5Fclose(file))
  rhdf5::H5Lmove(file, from, file, to)
}
