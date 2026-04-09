# Ingestion helpers for building population cubes from tabular source data.

#' Normalize common aggregate labels
#'
#' Converts a small set of common aggregate labels to the canonical `"All"`
#' marker used by ingestion normalization.
#'
#' @param x A vector of labels.
#'
#' @return A character vector.
#' @keywords internal
normalize_totals <- function(x) {
  total_aliases <- c("Total", "All", "All Ages")
  x <- as.character(x)
  x[data.table::`%chin%`(x, total_aliases)] <- "All"
  x
}

#' Prepare a population table for ingestion
#'
#' Applies schema-level normalization before validation and cube-building.
#' This helper is intentionally limited to column checks, aggregate-label
#' normalization, and optional dropping of `"All"` rows.
#'
#' @param df A source data.frame or data.table.
#' @param dims Character vector of dimension column names.
#' @param drop_all Logical; remove rows containing `"All"` in any dimension.
#' @param data_col Name of the value column.
#'
#' @return A data.table with normalized dimension columns.
#' @keywords internal
prepare_population_df <- function(df, dims, drop_all = TRUE, data_col = "population") {
  checkmate::assert_data_frame(df, min.rows = 1L)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(data_col, min.chars = 1L)

  required_cols <- unique(c(dims, data_col))
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Missing required columns: {.val {paste(missing, collapse = ', ')}}."
    )
  }

  out <- data.table::as.data.table(data.table::copy(df))
  cols <- intersect(dims, names(out))
  for (col in cols) {
    data.table::set(out, j = col, value = normalize_totals(out[[col]]))
  }

  if (isTRUE(drop_all)) {
    keep <- rowSums(as.matrix(out[, dims, with = FALSE]) == "All") == 0L
    out <- out[keep, ]
  }

  out
}

#' Find missing population cells against a valid support table
#'
#' Computes the valid but unobserved dimension combinations by comparing
#' observed rows to an explicit support table. When `support` is `NULL`, the
#' observed support is returned unchanged and no missing rows are inferred.
#'
#' @param df Observed data.frame or data.table.
#' @param dims Character vector of dimension column names.
#' @param support Optional support table listing valid dimension combinations.
#'
#' @return A data.table of missing dimension combinations.
#' @keywords internal
find_missing_population_cells <- function(df, dims, support = NULL) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)

  observed <- unique(data.table::as.data.table(data.table::copy(df))[, dims, with = FALSE])

  if (is.null(support)) {
    return(observed[0])
  }

  checkmate::assert_data_frame(support)
  missing <- setdiff(dims, names(support))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Missing required support columns: {.val {paste(missing, collapse = ', ')}}."
    )
  }

  skeleton <- unique(data.table::as.data.table(data.table::copy(support))[, dims, with = FALSE])
  data.table::fsetdiff(skeleton, observed)
}

#' Apply structural completion policy to an ingestion table
#'
#' Handles sparse source tables without assuming that the valid cell space is
#' the full Cartesian product of observed marginal levels. For `"zero"` and
#' `"na"` policies an explicit `support` table is required.
#'
#' @param df Observed data.frame or data.table.
#' @param dims Character vector of dimension column names.
#' @param policy Completion policy: `"error"`, `"zero"`, or `"na"`.
#' @param data_col Name of the value column.
#' @param support Optional support table listing valid dimension combinations.
#'
#' @return A data.table.
#' @keywords internal
apply_completion_policy <- function(df,
                                    dims,
                                    policy = c("error", "zero", "na"),
                                    data_col = "population",
                                    support = NULL) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(data_col, min.chars = 1L)

  policy <- match.arg(policy)
  observed <- unique(data.table::as.data.table(data.table::copy(df)))

  if (identical(policy, "error")) {
    missing <- find_missing_population_cells(observed, dims, support = support)
    if (nrow(missing) > 0L) {
      preview <- utils::capture.output(print(utils::head(missing, 5L), row.names = FALSE))
      cli::cli_abort(c(
        "Missing population cells under {.val completion_policy = 'error'}.",
        "x" = "{nrow(missing)} valid dimension combinations are absent from the source table.",
        "i" = "First missing combinations:",
        paste(preview, collapse = "\n")
      ))
    }
    return(observed)
  }

  if (is.null(support)) {
    cli::cli_abort(
      "{.arg support} is required for {.val completion_policy = '{policy}'} to avoid unsafe Cartesian expansion."
    )
  }

  checkmate::assert_data_frame(support)
  missing_support_cols <- setdiff(dims, names(support))
  if (length(missing_support_cols) > 0L) {
    cli::cli_abort(
      "Missing required support columns: {.val {paste(missing_support_cols, collapse = ', ')}}."
    )
  }

  skeleton <- unique(data.table::as.data.table(data.table::copy(support))[, dims, with = FALSE])
  full <- data.table::merge.data.table(
    skeleton,
    observed,
    by = dims,
    all.x = TRUE,
    sort = FALSE
  )

  if (identical(policy, "zero")) {
    idx <- which(is.na(full[[data_col]]))
    if (length(idx) > 0L) {
      data.table::set(full, i = idx, j = data_col, value = 0)
    }
  }

  full
}

#' Validate a prepared ingestion table
#'
#' Checks structural invariants before conversion to an array-backed cube.
#'
#' @param df A prepared data.frame or data.table.
#' @param dims Character vector of dimension column names.
#' @param allow_na Logical; permit `NA` in the value column.
#' @param data_col Name of the value column.
#'
#' @return Invisibly returns `TRUE` or errors.
#' @keywords internal
validate_population_df <- function(df,
                                   dims,
                                   allow_na = FALSE,
                                   data_col = "population") {
  checkmate::assert_data_frame(df, min.rows = 1L)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_flag(allow_na)
  checkmate::assert_string(data_col, min.chars = 1L)

  missing <- setdiff(unique(c(dims, data_col)), names(df))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Missing required columns: {.val {paste(missing, collapse = ', ')}}."
    )
  }

  bad_dim <- vapply(df[dims], function(x) any(is.na(x)), logical(1))
  if (any(bad_dim)) {
    cli::cli_abort(
      "Dimension columns cannot contain NA values: {.val {paste(names(bad_dim)[bad_dim], collapse = ', ')}}."
    )
  }

  dup <- duplicated(df[dims])
  if (any(dup)) {
    cli::cli_abort("{.arg df} contains duplicate rows for one or more dimension combinations.")
  }

  values <- df[[data_col]]
  if (any(values < 0, na.rm = TRUE)) {
    cli::cli_abort("Population contains negative values.")
  }

  if (!allow_na && any(is.na(values))) {
    cli::cli_abort("Population contains NA values.")
  }

  invisible(TRUE)
}

#' Build and persist a poparray cube from a table
#'
#' Converts a validated tabular population table to an array, validates
#' dimensional semantics against the resulting cube shape, and writes the HDF5
#' cube once through the canonical storage path.
#'
#' @param df A validated data.frame or data.table.
#' @param dims Character vector of dimension column names.
#' @param dim_semantics Named list of `DimSemantics` entries matching `dims`.
#' @param filepath Output HDF5 file path.
#' @param series_id Series identifier stored in cube metadata.
#' @param time_dim Name of the time dimension.
#' @param area_dim Name of the area dimension.
#' @param source Provenance metadata list.
#' @param data_col Name of the value column.
#' @param overwrite Logical; overwrite existing file.
#' @param chunkdim Chunk dimensions or `"auto"`.
#' @param level Compression level passed to HDF5 writer.
#' @param geo Optional geography tag.
#' @param extendable_year Optional extendable-year flag.
#' @param registry Optional registry metadata.
#' @param target_chunk_bytes Target bytes for auto chunk sizing.
#'
#' @return Invisibly returns the low-level write result from
#'   `pa_write_poparray_cube()`.
#' @keywords internal
build_poparray_from_df <- function(df,
                                   dims,
                                   dim_semantics,
                                   filepath,
                                   series_id,
                                   time_dim = "year",
                                   area_dim = "area.name",
                                   source = list(),
                                   data_col = "population",
                                   overwrite = FALSE,
                                   chunkdim = "auto",
                                   level = 6L,
                                   geo = NULL,
                                   extendable_year = NULL,
                                   registry = NULL,
                                   target_chunk_bytes = 1e6) {
  checkmate::assert_data_frame(df, min.rows = 1L)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(filepath, min.chars = 1L)
  checkmate::assert_string(series_id, min.chars = 1L)
  checkmate::assert_string(time_dim, min.chars = 1L)
  checkmate::assert_string(area_dim, min.chars = 1L)
  checkmate::assert_string(data_col, min.chars = 1L)

  fields <- unique(c(dims, data_col))
  missing <- setdiff(fields, names(df))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Missing required columns: {.val {paste(missing, collapse = ', ')}}."
    )
  }

  arr_df <- as.data.frame(data.table::as.data.table(data.table::copy(df))[, fields, with = FALSE])
  arr <- df_2_array(arr_df, data_col = data_col)
  dim_names <- names(dimnames(arr))

  validate_dim_semantics(
    dim_semantics = dim_semantics,
    dim_names = dim_names,
    time_dim = time_dim,
    area_dim = area_dim
  )

  pa_write_poparray_cube(
    x = DelayedArray::DelayedArray(arr),
    filepath = filepath,
    dimnames_list = dimnames(arr),
    overwrite = overwrite,
    chunkdim = chunkdim,
    level = level,
    time_dim = time_dim,
    area_dim = area_dim,
    dim_semantics = dim_semantics,
    source = source,
    data_col = data_col,
    series_id = series_id,
    geo = geo,
    extendable_year = extendable_year,
    registry = registry,
    target_chunk_bytes = target_chunk_bytes
  )
}

#' Ingest a population table into an HDF5-backed cube
#'
#' Runs the package-native ingestion pipeline: read, transform, normalize,
#' structurally complete according to policy, validate, and persist.
#'
#' @param reader Function returning a source table.
#' @param transformer Function transforming reader output into canonical columns.
#' @param dims Character vector of dimension column names.
#' @param dim_semantics Named list of `DimSemantics` entries matching `dims`.
#' @param filepath Output HDF5 file path.
#' @param series_id Series identifier stored in cube metadata.
#' @param completion_policy Completion policy: `"error"`, `"zero"`, or `"na"`.
#' @param drop_all Logical; remove rows containing `"All"` in any dimension.
#' @param source_meta Provenance metadata list with optional `nm`, `pop_type`,
#'   and `url` fields.
#' @param time_dim Name of the time dimension.
#' @param area_dim Name of the area dimension.
#' @param overwrite Logical; overwrite existing file.
#' @param chunkdim Chunk dimensions or `"auto"`.
#' @param level Compression level passed to HDF5 writer.
#' @param geo Optional geography tag.
#' @param extendable_year Optional extendable-year flag.
#' @param registry Optional registry metadata.
#' @param target_chunk_bytes Target bytes for auto chunk sizing.
#' @param data_col Name of the value column.
#' @param support Optional support table listing valid dimension combinations.
#' @param ... Additional arguments forwarded to `reader()` and `transformer()`.
#'
#' @return Invisibly returns the normalized output file path.
#' @keywords internal
ingest_population <- function(reader,
                              transformer = function(df, ...) df,
                              dims,
                              dim_semantics,
                              filepath,
                              series_id,
                              completion_policy = c("error", "zero", "na"),
                              drop_all = TRUE,
                              source_meta = list(),
                              time_dim = "year",
                              area_dim = "area.name",
                              overwrite = FALSE,
                              chunkdim = "auto",
                              level = 6L,
                              geo = NULL,
                              extendable_year = NULL,
                              registry = NULL,
                              target_chunk_bytes = 1e6,
                              data_col = "population",
                              support = NULL,
                              ...) {
  checkmate::assert_function(reader)
  checkmate::assert_function(transformer)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(filepath, min.chars = 1L)
  checkmate::assert_string(series_id, min.chars = 1L)
  checkmate::assert_string(time_dim, min.chars = 1L)
  checkmate::assert_string(area_dim, min.chars = 1L)
  checkmate::assert_string(data_col, min.chars = 1L)

  completion_policy <- match.arg(completion_policy)

  df <- reader(...)
  df <- transformer(df, ...)
  df <- prepare_population_df(df, dims = dims, drop_all = drop_all, data_col = data_col)
  df <- unique(df, by = c(dims, data_col))
  df <- apply_completion_policy(
    df,
    dims = dims,
    policy = completion_policy,
    data_col = data_col,
    support = support
  )
  validate_population_df(
    df,
    dims = dims,
    allow_na = identical(completion_policy, "na"),
    data_col = data_col
  )

  source <- list(
    note = source_meta$nm %||% "Unknown",
    population_type = source_meta$pop_type %||% "Unknown",
    source = source_meta$url %||% "",
    updated = as.character(Sys.Date())
  )

  build_poparray_from_df(
    df = df,
    dims = dims,
    dim_semantics = dim_semantics,
    filepath = filepath,
    series_id = series_id,
    time_dim = time_dim,
    area_dim = area_dim,
    source = source,
    data_col = data_col,
    overwrite = overwrite,
    chunkdim = chunkdim,
    level = level,
    geo = geo,
    extendable_year = extendable_year,
    registry = registry,
    target_chunk_bytes = target_chunk_bytes
  )

  invisible(normalizePath(filepath, winslash = "/", mustWork = FALSE))
}
