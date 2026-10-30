#' List saved population views
#'
#' Reads and validates plain recipe definitions in `views/`, then refreshes
#' `cache/view_registry.rds`. The definitions are authoritative; missing or
#' stale registry caches are rebuilt without opening any population cube.
#' Registry paths are relative to the population root, allowing the whole root
#' to be moved. Explicit exports outside `views/` are not indexed.
#' @param root Population storage root, defaulting to the configured cube root.
#' @return A data frame with view ID, source series ID, relative filepath,
#'   name, description, creator, and modification time. Invalid definitions and
#'   duplicate IDs produce errors rather than ambiguous lookup results.
#' @export
list_poparray_views <- function(root = resolve_cube_dir()) {
  checkmate::assert_directory_exists(root)
  directory <- file.path(root, "views")
  files <- if (dir.exists(directory)) list.files(directory, pattern = "\\.rds$",
    full.names = TRUE) else character()
  registry <- data.frame(view_id = character(), source_series_id = character(),
    filepath = character(), name = character(), description = character(),
    created_by = character(), modified_at = character(), stringsAsFactors = FALSE)
  rows <- lapply(files, function(path) {
    tryCatch({
      recipe <- readRDS(path)
      pa_validate_view_recipe(recipe)
      data.frame(view_id = recipe$view_id, source_series_id = recipe$source_series_id,
        filepath = file.path("views", basename(path)), name = recipe$metadata$name,
        description = recipe$metadata$description, created_by = recipe$metadata$created_by,
        modified_at = recipe$metadata$modified_at, stringsAsFactors = FALSE)
    }, error = function(e) cli::cli_abort("Invalid view definition {path}: {conditionMessage(e)}", parent = e))
  })
  if (length(rows)) registry <- do.call(rbind, rows)
  if (anyDuplicated(registry$view_id)) cli::cli_abort("Duplicate view IDs in the views directory.")
  registry <- registry[order(registry$view_id), , drop = FALSE]
  rownames(registry) <- NULL
  # A failed cache refresh does not invalidate otherwise readable definitions.
  tryCatch({
    cache <- file.path(root, "cache")
    if (!dir.exists(cache) && !dir.create(cache, recursive = TRUE)) stop("Cannot create cache directory.")
    pa_write_view_rds(registry, file.path(cache, "view_registry.rds"))
  }, error = function(e) cli::cli_warn("Could not refresh the view registry cache: {conditionMessage(e)}"))
  registry
}

#' Edit view descriptions and notes
#'
#' Editing returns a new definition without replaying its operations or changing
#' the saved file. Save with `overwrite = TRUE` to persist edits. Creation
#' provenance, view ID, source association, and operations are preserved.
#' @param view A `poparray_view`, saved view ID, or path.
#' @param description,notes,name Optional replacement character scalars. `NULL`
#'   leaves a field unchanged; an empty string clears it. `name` is the display
#'   name and does not rename the view ID or file.
#' @param modified_by Modifier name, defaulting to the current OS user.
#' @return A validated S7 `poparray_view` with updated modification provenance.
#' @export
update_poparray_view_metadata <- function(view, description = NULL, notes = NULL,
                                          name = NULL, modified_by = Sys.info()[["user"]]) {
  if (is.character(view)) view <- read_poparray_view(view)
  pa_assert_view(view)
  edits <- list(description = description, notes = notes, name = name)
  for (field in names(edits)) if (!is.null(edits[[field]])) checkmate::assert_string(edits[[field]])
  checkmate::assert_string(modified_by)
  recipe <- pa_view_recipe(view)
  for (field in names(edits)) {
    if (!is.null(edits[[field]])) recipe$metadata[[field]] <- edits[[field]]
  }
  recipe$metadata$modified_by <- modified_by
  recipe$metadata$modified_at <- pa_view_time()
  do.call(poparray_view, recipe)
}

#' Print a population view without executing its recipe
#' @param x A `poparray_view`.
#' @param ... Unused.
#' @return The definition, invisibly.
#' @name print.poparray_view
#' @usage print(x, ...)
NULL
S7::method(print, poparray_view) <- function(x, ...) {
  cat("<poparray_view> ", x@view_id, "\n", sep = "")
  cat("Source: ", x@source_series_id, "\n", sep = "")
  cat("Name: ", x@metadata$name, "\n", sep = "")
  if (nzchar(x@metadata$description)) cat(x@metadata$description, "\n", sep = "")
  if (!length(x@operations)) cat("Transformations: none\n")
  for (i in seq_along(x@operations)) {
    step <- x@operations[[i]]
    target <- switch(step$op,
      group_ages = paste(step$args$age_groups, collapse = ", "),
      collapse_dim = step$args$dim,
      collapse_all = paste(step$args$dim, collapse = ", "),
      select = paste(step$args$dimensions, collapse = ", "),
      relocate = paste(step$args$dimensions, collapse = ", "),
      rename = paste(paste(names(step$args$mapping), step$args$mapping, sep = " = "), collapse = ", "),
      subset = paste(names(step$args$indices), collapse = ", "),
      filter = paste(unique(vapply(step$args$predicates, tp_pred_dim, character(1))), collapse = ", "))
    cat(sprintf("  %d. %s (%s)\n", i, step$op, target))
  }
  invisible(x)
}

#' Stage a plain RDS file beside its destination before installing it
#' @param object Plain data to serialize.
#' @param path Destination path.
#' @return The path, invisibly.
#' @noRd
pa_write_view_rds <- function(object, path) {
  staged <- tempfile(".view-", tmpdir = dirname(path))
  on.exit(unlink(staged), add = TRUE)
  saveRDS(object, staged, version = 2)
  if (!pa_install_view_file(staged, path)) cli::cli_abort("Could not install view file {path}.")
  invisible(path)
}

#' Install a staged view or registry file
#' @param from,to Staged and destination paths.
#' @return Whether the rename succeeded; never deletes an existing destination.
#' @noRd
pa_install_view_file <- function(from, to) file.rename(from, to)

#' Validate a view ID used as a default filename
#' @param id A view ID.
#' @return The ID, invisibly.
#' @noRd
pa_view_check_filename <- function(id) {
  checkmate::assert_string(id, min.chars = 1, pattern = "^[[:alnum:]][[:alnum:]_.-]*$")
  invisible(id)
}

#' Resolve an explicit definition path or a registered view ID
#' @param filepath An explicit path or positional view ID.
#' @param view_id An explicit registry identifier.
#' @return An existing absolute filepath.
#' @noRd
pa_resolve_view_path <- function(filepath = NULL, view_id = NULL) {
  if (is.null(filepath) == is.null(view_id)) cli::cli_abort("Supply exactly one of filepath or view_id.")
  if (!is.null(filepath)) {
    checkmate::assert_string(filepath, min.chars = 1)
    if (file.exists(filepath) || grepl("/", filepath, fixed = TRUE) ||
        grepl("\\", filepath, fixed = TRUE) || grepl("\\.rds$", filepath)) {
      checkmate::assert_file_exists(filepath)
      return(normalizePath(filepath, winslash = "/", mustWork = TRUE))
    }
    view_id <- filepath
  }
  checkmate::assert_string(view_id, min.chars = 1)
  root <- resolve_cube_dir()
  registry <- list_poparray_views(root)
  row <- registry[registry$view_id == view_id, , drop = FALSE]
  if (!nrow(row)) cli::cli_abort("Unknown view_id: {view_id}.")
  path <- file.path(root, row$filepath)
  checkmate::assert_file_exists(path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
