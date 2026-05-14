# Ingestion helpers for building population cubes from tabular source data.

#' Prepare a population table for ingestion
#'
#' Applies schema-level filtering before validation and cube-building.
#' Aggregate rows are invalid physical rows for `poparray` cubes and are removed
#' directly rather than normalized to a canonical label. When `df` is a plain
#' `data.frame`, it is converted to a `data.table` in place.
#'
#' @param df A source data.frame or data.table.
#' @param dims Character vector of dimension column names.
#' @param drop_all Logical; remove rows containing `"All"` in any dimension.
#' @param data_col Name of the value column.
#'
#' @return A data.table with aggregate rows removed when requested.
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

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  if (isTRUE(drop_all)) {
    aggregate_aliases <- c("All", "Total", "All Ages")
    keep <- rep(TRUE, nrow(df))
    for (col in dims) {
      keep <- keep & !data.table::`%chin%`(as.character(df[[col]]), aggregate_aliases)
    }
    df <- df[keep, ]
  }

 df
}

#' Find missing population cells against a valid support table
#'
#' Computes the valid but unobserved dimension combinations by comparing
#' observed rows to an explicit support table. When `support` is `NULL`, the
#' observed support is returned unchanged and no missing rows are inferred. The
#' input tables are used directly; no defensive copies are made.
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

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  observed <- df[, dims, with = FALSE]

  if (is.null(support)) {
    return(observed[0])
  }

  checkmate::assert_data_frame(support)
  if (!data.table::is.data.table(support)) {
    data.table::setDT(support)
  }
  missing <- setdiff(dims, names(support))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Missing required support columns: {.val {paste(missing, collapse = ', ')}}."
    )
  }

  support_dup <- duplicated(support[, dims, with = FALSE])
  if (any(support_dup)) {
    cli::cli_abort("{.arg support} contains duplicate rows for one or more dimension combinations.")
  }

  skeleton <- support[, dims, with = FALSE]
  data.table::fsetdiff(skeleton, observed)
}

#' Apply structural completion policy to an ingestion table
#'
#' Handles sparse source tables without assuming that the valid cell space is
#' the full Cartesian product of observed marginal levels. For `"zero"` and
#' `"na"` policies an explicit `support` table is required. The input table is
#' normalized to a `data.table` and then used directly.
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

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  observed <- df

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
  if (!data.table::is.data.table(support)) {
    data.table::setDT(support)
  }
  missing_support_cols <- setdiff(dims, names(support))
  if (length(missing_support_cols) > 0L) {
    cli::cli_abort(
      "Missing required support columns: {.val {paste(missing_support_cols, collapse = ', ')}}."
    )
  }

  support_dup <- anyDuplicated(support[, dims, with = FALSE])
  if (support_dup > 0L) {
    cli::cli_abort("{.arg support} contains duplicate rows for one or more dimension combinations.")
  }

  skeleton <- support[, dims, with = FALSE]
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

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  bad_dim <- vapply(dims, function(col) anyNA(df[[col]]), logical(1), USE.NAMES = TRUE)
  if (any(bad_dim)) {
    cli::cli_abort(
      "Dimension columns cannot contain NA values: {.val {paste(names(bad_dim)[bad_dim], collapse = ', ')}}."
    )
  }

  dup <- anyDuplicated(df[, dims, with = FALSE])
  if (dup > 0L) {
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

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  arr_df <- as.data.frame(df[, fields, with = FALSE])
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
#' `ingest_population()` is the supported high-level entry point for importing
#' tabular population data into the package's HDF5-backed cube format. The
#' function is designed for source-specific ingestion scripts that can supply a
#' reader and, when needed, a transformer that maps raw source columns to the
#' canonical dimension columns required for cube construction.
#'
#' @param reader Function returning a source table.
#' @param transformer Function transforming reader output into canonical columns.
#'   Defaults to the identity function.
#' @param dims Character vector of dimension column names. These columns,
#'   together with `data_col`, must be present after `transformer()` runs.
#' @param dim_semantics Named list of `DimSemantics` entries matching `dims` in
#'   the same order.
#' @param filepath Output HDF5 file path.
#' @param series_id Series identifier stored in cube metadata and used by
#'   [open_poparray()] to locate the cube later.
#' @param completion_policy Completion policy: `"error"`, `"zero"`, or `"na"`.
#' @param drop_all Logical; remove rows containing `"All"` in any dimension.
#'   This is usually the correct setting because aggregate rows are invalid
#'   physical rows for `poparray` storage.
#' @param source_meta Provenance metadata list with optional `note`,
#'   `population_type`, and `source` fields.
#' @param time_dim Name of the time dimension. Its semantics entry must have
#'   `partition_type = "partition"`.
#' @param area_dim Name of the area dimension. Its semantics entry must have
#'   `partition_type = "partition"`.
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
#'
#' @export
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
    note = source_meta$note %||% "Unknown",
    population_type = source_meta$population_type %||% "Unknown",
    source = source_meta$source %||% "",
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
