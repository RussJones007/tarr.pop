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
  
  # drop levles that no longer exist
  factor_cols <- names(df)[vapply(df, is.factor, logical(1))]
  if (length(factor_cols)) {
    df[, (factor_cols) := lapply(.SD, droplevels), .SDcols = factor_cols]
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

  if (!data.table::is.data.table(df)) data.table::setDT(df)

  # Align support dimension column types with the observed table before joins.
  if (!is.null(support)) {
    if (!data.table::is.data.table(support)) {
      data.table::setDT(support)
    }
    missing_support_cols <- setdiff(dims, names(support))
    if (length(missing_support_cols) > 0L) {
      cli::cli_abort(
        "Missing required support columns: {.val {paste(missing_support_cols, collapse = ', ')}}."
      )
    }
    for (col in dims) {
      support[, (col) := list(vctrs::vec_cast(support[[col]], df[[col]]))]
    }
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
    all = TRUE,
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
#' @name ingest_population
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

pa_expand_dim_grid <- function(dimnames_list) {
  out <- do.call(
    base::expand.grid,
    c(dimnames_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  )
  data.table::as.data.table(out)
}

pa_sort_append_labels <- function(labels) {
  labels <- unique(as.character(labels))
  if (length(labels) > 1L && all(grepl("^-?[0-9]+$", labels))) {
    labels <- labels[order(as.integer(labels))]
  }
  labels
}

pa_align_array_dimnames <- function(arr, target_dimnames) {
  current <- dimnames(arr)
  if (is.null(current) || is.null(names(current))) {
    cli::cli_abort("New data could not be converted to an array with named dimensions.")
  }

  target_names <- names(target_dimnames)
  perm <- match(target_names, names(current))
  if (anyNA(perm)) {
    missing <- target_names[is.na(perm)]
    cli::cli_abort("New array is missing dimensions: {.val {paste(missing, collapse = ', ')}}.")
  }
  if (!identical(perm, seq_along(perm))) {
    arr <- aperm(arr, perm)
    current <- dimnames(arr)
  }

  idx <- lapply(seq_along(target_dimnames), function(i) {
    match(as.character(target_dimnames[[i]]), as.character(current[[i]]))
  })
  names(idx) <- target_names

  bad <- vapply(idx, function(x) anyNA(x), logical(1))
  if (any(bad)) {
    cli::cli_abort(
      "New data is missing required dimension labels: {.val {paste(names(idx)[bad], collapse = ', ')}}."
    )
  }

  out <- do.call(`[`, c(list(arr), idx, list(drop = FALSE)))
  dimnames(out) <- target_dimnames
  out
}

pa_resolve_cube_update_target <- function(cube) {
  checkmate::assert_string(cube, min.chars = 1L)

  if (file.exists(cube)) {
    path <- normalizePath(cube, winslash = "/", mustWork = TRUE)
    meta <- get_cube_metadata_cached(path)
    h5 <- HDF5Array::HDF5Array(filepath = path, name = "cube/population")
    dimn <- read_dimnames_from_cube(path, meta = meta)
    roles <- read_roles_from_cube(path, meta = meta)
    dsem <- read_dim_semantics_from_cube(path, names(dimn), roles$time, roles$area, meta = meta)
    src <- read_source_from_cube(path, meta = meta)
    dimnames(h5) <- dimn
    return(list(
      path = path,
      object = new_poparray(
        x = h5,
        dimnames_list = dimn,
        data_col = read_data_col_from_cube(path, meta = meta),
        source = src,
        time_dim = roles$time,
        area_dim = roles$area,
        dim_semantics = dsem
      ),
      meta = meta
    ))
  }

  obj <- open_poparray(cube)
  sd <- tryCatch(DelayedArray::seed(obj), error = function(e) NULL)
  path <- tryCatch(sd@filepath, error = function(e) "")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Could not resolve the source HDF5 path for cube {.val {cube}}.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  list(path = path, object = obj, meta = get_cube_metadata_cached(path))
}

pa_write_delayed_blocks <- function(x, filepath, dataset, chunkdim, start_offset = NULL) {
  if (!is(x, "DelayedArray")) {
    x <- DelayedArray::DelayedArray(x)
  }
  x_dim <- as.integer(dim(x))
  blockdim <- pmin.int(as.integer(chunkdim), x_dim)
  idx_max <- ceiling(x_dim / blockdim)
  grid <- base::expand.grid(lapply(seq_along(x_dim), function(k) seq_len(idx_max[[k]])))
  if (is.null(start_offset)) {
    start_offset <- rep.int(0L, length(x_dim))
  }
  start_offset <- as.integer(start_offset)

  for (i in seq_len(nrow(grid))) {
    g <- as.integer(grid[i, ])
    start <- (g - 1L) * blockdim + 1L
    count <- pmin.int(blockdim, x_dim - start + 1L)
    index <- Map(seq.int, start, start + count - 1L)
    block <- DelayedArray::extract_array(x, index)
    rhdf5::h5write(
      block,
      file = filepath,
      name = dataset,
      start = start + start_offset,
      count = count
    )
  }

  invisible(TRUE)
}

pa_write_poparray_cube_append <- function(old_x,
                                          new_x,
                                          add_dim,
                                          filepath,
                                          dimnames_list,
                                          overwrite = FALSE,
                                          chunkdim = "auto",
                                          level = 6L,
                                          time_dim = "year",
                                          area_dim = "area.name",
                                          dim_semantics = NULL,
                                          source = NULL,
                                          data_col = "population",
                                          series_id = NULL,
                                          geo = NULL,
                                          extendable_year = NULL,
                                          target_chunk_bytes = 1e6) {
  checkmate::assert_string(filepath, min.chars = 1L)
  checkmate::assert_string(add_dim, min.chars = 1L)

  if (!is(old_x, "DelayedArray")) old_x <- DelayedArray::DelayedArray(old_x)
  if (!is(new_x, "DelayedArray")) new_x <- DelayedArray::DelayedArray(new_x)

  dim_order <- names(dimnames_list)
  add_k <- match(add_dim, dim_order)
  if (is.na(add_k)) {
    cli::cli_abort("{.arg add_dim} must be present in {.arg dimnames_list}.")
  }

  out_dim <- as.integer(lengths(dimnames_list))
  old_dim <- as.integer(dim(old_x))
  new_dim <- as.integer(dim(new_x))
  expected_old_dim <- out_dim
  expected_old_dim[[add_k]] <- old_dim[[add_k]]
  expected_new_dim <- out_dim
  expected_new_dim[[add_k]] <- new_dim[[add_k]]
  if (!identical(old_dim, expected_old_dim)) {
    cli::cli_abort("Existing cube dimensions do not align with the requested append.")
  }
  if (!identical(new_dim, expected_new_dim)) {
    cli::cli_abort("New data dimensions do not align with the requested append.")
  }

  out_dir <- dirname(filepath)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(filepath)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort("File already exists: {.file {filepath}}. Set {.arg overwrite = TRUE} to replace.")
    }
    if (!file.remove(filepath)) {
      cli::cli_abort("Could not remove existing file: {.file {filepath}}.")
    }
  }

  if (is.null(chunkdim) || (is.character(chunkdim) && length(chunkdim) == 1L && identical(chunkdim, "auto"))) {
    chunkdim <- pa_guess_chunkdim(
      dim = out_dim,
      dimnames_list = dimnames_list,
      time_dim = time_dim,
      area_dim = area_dim,
      target_chunk_bytes = target_chunk_bytes,
      type = DelayedArray::type(old_x)
    )
  } else {
    chunkdim <- as.integer(chunkdim)
    if (length(chunkdim) != length(out_dim) || any(is.na(chunkdim)) || any(chunkdim < 1L)) {
      cli::cli_abort("{.arg chunkdim} must be positive integers with length equal to number of dimensions.")
    }
  }

  rhdf5::h5createFile(filepath)
  pa_h5_create_group(filepath, "cube")
  rhdf5::h5createDataset(
    file = filepath,
    dataset = "cube/population",
    dims = out_dim,
    chunk = as.integer(chunkdim),
    level = as.integer(level)
  )

  pa_write_delayed_blocks(
    x = old_x,
    filepath = filepath,
    dataset = "cube/population",
    chunkdim = chunkdim
  )
  offset <- rep.int(0L, length(out_dim))
  offset[[add_k]] <- old_dim[[add_k]]
  pa_write_delayed_blocks(
    x = new_x,
    filepath = filepath,
    dataset = "cube/population",
    chunkdim = chunkdim,
    start_offset = offset
  )

  pa_write_poparray_metadata(
    filepath = filepath,
    dimnames_list = dimnames_list,
    time_dim = time_dim,
    area_dim = area_dim,
    dim_semantics = dim_semantics,
    source = source,
    data_col = data_col,
    series_id = series_id,
    geo = geo,
    extendable_year = extendable_year
  )

  list(
    filepath = normalizePath(filepath, winslash = "/", mustWork = TRUE),
    dataset = "cube/population",
    chunkdim = as.integer(chunkdim)
  )
}

#' Add population records to an existing cube
#'
#' Reads and transforms new tabular population data, validates it against an
#' existing cube's dimensional contract, appends new labels on one dimension,
#' and writes a replacement cube. The existing HDF5 data are kept lazy while the
#' combined cube is written to disk.
#'
#' This is intended for updates such as adding a newly released estimate year to
#' a county-by-age-by-sex-by-race cube. It does not modify the existing HDF5
#' dataset in place; when `output_filepath` is the same as `cube`, a temporary
#' file is written first and then moved into place.
#'
#' @param cube Existing cube filepath or `series_id`.
#' @param reader Function returning the new source table.
#' @param transformer Function transforming reader output into canonical columns.
#' @param dims Character vector of dimension column names. Defaults to the
#'   existing cube dimension order.
#' @param add_dim Dimension that receives new labels. Defaults to `"year"`.
#' @param output_filepath Destination HDF5 filepath. Defaults to replacing
#'   `cube` when `cube` is a filepath.
#' @param overwrite Logical; overwrite `output_filepath` when writing to a
#'   different existing file. Replacing the source file is always done through a
#'   temporary file.
#' @param completion_policy Completion policy for missing cells in the new
#'   slice: `"error"`, `"zero"`, or `"na"`.
#' @param drop_all Logical; remove rows containing aggregate aliases.
#' @param source_meta Optional source metadata overrides for the rewritten cube.
#' @param chunkdim Chunk dimensions or `"auto"`.
#' @param level Compression level passed to HDF5 writer.
#' @param target_chunk_bytes Target bytes for auto chunk sizing.
#' @param data_col Name of the value column. Defaults to the existing cube's
#'   stored data column.
#' @param support Optional support table for the new data. Defaults to the
#'   existing cube support crossed with the new `add_dim` labels.
#' @param ... Additional arguments forwarded to `reader()` and `transformer()`.
#'
#' @return Invisibly returns the normalized output filepath.
#' @export
add_population_data <- function(cube,
                                reader,
                                transformer = function(df, ...) df,
                                dims = NULL,
                                add_dim = "year",
                                output_filepath = NULL,
                                overwrite = FALSE,
                                completion_policy = c("error", "zero", "na"),
                                drop_all = TRUE,
                                source_meta = list(),
                                chunkdim = "auto",
                                level = 6L,
                                target_chunk_bytes = 1e6,
                                data_col = NULL,
                                support = NULL,
                                ...) {
  checkmate::assert_function(reader)
  checkmate::assert_function(transformer)
  checkmate::assert_string(add_dim, min.chars = 1L)
  completion_policy <- match.arg(completion_policy)

  target <- pa_resolve_cube_update_target(cube)
  existing <- target$object
  existing_path <- target$path
  existing_dimnames <- dimnames(existing)
  dim_order <- names(existing_dimnames)

  if (is.null(dims)) {
    dims <- dim_order
  }
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  if (!identical(dims, dim_order)) {
    cli::cli_abort("{.arg dims} must match the existing cube dimension order.")
  }
  if (!add_dim %in% dim_order) {
    cli::cli_abort("{.arg add_dim} must be one of the existing cube dimensions.")
  }

  data_col <- data_col %||% existing@data_col

  df <- reader(...)
  df <- transformer(df, ...)
  df <- prepare_population_df(df, dims = dims, drop_all = drop_all, data_col = data_col)
  for (col in dims) {
    df[, (col) := as.character(df[[col]])]
  }

  incoming_labels <- lapply(dims, function(col) unique(as.character(df[[col]])))
  names(incoming_labels) <- dims

  add_labels <- pa_sort_append_labels(incoming_labels[[add_dim]])
  if (!length(add_labels)) {
    cli::cli_abort("No labels were found in {.arg add_dim}.")
  }
  overlap <- intersect(add_labels, as.character(existing_dimnames[[add_dim]]))
  if (length(overlap) > 0L) {
    cli::cli_abort(
      "New data overlaps existing {.arg add_dim} labels: {.val {paste(overlap, collapse = ', ')}}."
    )
  }

  for (col in setdiff(dims, add_dim)) {
    extra <- setdiff(incoming_labels[[col]], as.character(existing_dimnames[[col]]))
    if (length(extra) > 0L) {
      cli::cli_abort(
        "New data contains labels not present in existing dimension {.val {col}}: {.val {paste(extra, collapse = ', ')}}."
      )
    }
  }

  new_dimnames <- existing_dimnames
  new_dimnames[[add_dim]] <- add_labels

  if (is.null(support)) {
    support <- pa_expand_dim_grid(new_dimnames)
  }
  for (col in dims) {
    support[[col]] <- as.character(support[[col]])
  }

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

  arr_df <- as.data.frame(df[, unique(c(dims, data_col)), with = FALSE])
  new_arr <- df_2_array(arr_df, data_col = data_col)
  new_arr <- pa_align_array_dimnames(new_arr, new_dimnames)

  old_x <- methods::as(existing, "DelayedArray")
  new_x <- DelayedArray::DelayedArray(new_arr)

  combined_dimnames <- existing_dimnames
  combined_dimnames[[add_dim]] <- c(
    as.character(existing_dimnames[[add_dim]]),
    add_labels
  )

  source <- modifyList(
    as.list(get_source(existing)),
    as.list(source_meta %||% list())
  )
  source$updated <- source_meta$updated %||% as.character(Sys.Date())

  series_id <- h5_read_scalar_chr_if_present(existing_path, "cube/metadata/series_id", info = target$meta$info)
  geo <- h5_read_scalar_chr_if_present(existing_path, "cube/metadata/geo", info = target$meta$info)
  extendable_year <- h5_read_scalar_chr_if_present(
    existing_path,
    "cube/metadata/extendable_year",
    info = target$meta$info
  )

  if (is.null(output_filepath)) {
    output_filepath <- existing_path
  }
  checkmate::assert_string(output_filepath, min.chars = 1L)
  output_filepath <- normalizePath(output_filepath, winslash = "/", mustWork = FALSE)

  same_file <- file.exists(output_filepath) &&
    identical(normalizePath(output_filepath, winslash = "/", mustWork = TRUE), existing_path)
  write_path <- if (same_file) {
    tempfile(
      pattern = paste0(tools::file_path_sans_ext(basename(output_filepath)), "_"),
      tmpdir = dirname(output_filepath),
      fileext = ".h5"
    )
  } else {
    output_filepath
  }

  pa_write_poparray_cube_append(
    old_x = old_x,
    new_x = new_x,
    add_dim = add_dim,
    filepath = write_path,
    dimnames_list = combined_dimnames,
    overwrite = !same_file && isTRUE(overwrite),
    chunkdim = chunkdim,
    level = level,
    time_dim = time_role(existing),
    area_dim = area_role(existing),
    dim_semantics = dim_semantics(existing),
    source = source,
    data_col = data_col,
    series_id = series_id,
    geo = geo,
    extendable_year = extendable_year,
    target_chunk_bytes = target_chunk_bytes
  )

  if (same_file) {
    rm(existing, old_x, new_x)
    gc()
    if (!file.remove(output_filepath)) {
      unlink(write_path)
      cli::cli_abort("Could not replace existing cube file: {.file {output_filepath}}.")
    }
    if (!file.rename(write_path, output_filepath)) {
      cli::cli_abort("Could not move updated cube into place: {.file {output_filepath}}.")
    }
  }

  reset_poparray_cache()
  invisible(normalizePath(output_filepath, winslash = "/", mustWork = FALSE))
}
