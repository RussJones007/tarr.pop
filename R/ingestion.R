# Ingestion helpers for building population cubes from tabular source data.

normalize_totals <- function(x) {
  x <- as.character(x)
  x[x %in% c("Total", "All", "All Ages")] <- "All"
  x
}

prepare_population_df <- function(df, dims, drop_all = TRUE, data_col = "population") {
  checkmate::assert_data_frame(df, min.rows = 1L)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(data_col, min.chars = 1L)

  required_cols <- unique(c(dims, data_col))
  missing <- setdiff(required_cols, names(df))
  if (length(missing)) {
    cli::cli_abort(
      "Missing required columns: {.val {paste(missing, collapse = ', ')}}."
    )
  }

  out <- df
  for (nm in intersect(dims, names(out))) {
    out[[nm]] <- normalize_totals(out[[nm]])
  }

  if (isTRUE(drop_all)) {
    for (nm in dims) {
      out <- out[out[[nm]] != "All", , drop = FALSE]
    }
  }

  out
}

find_missing_population_cells <- function(df, dims) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)

  observed <- unique(df[dims])
  observed[[".present"]] <- TRUE
  full <- tidyr::complete(observed, !!!rlang::syms(dims))

  full[is.na(full$.present), dims, drop = FALSE]
}

apply_completion_policy <- function(df,
                                    dims,
                                    policy = c("error", "zero", "na"),
                                    data_col = "population") {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(dims, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(data_col, min.chars = 1L)

  policy <- match.arg(policy)

  if (identical(policy, "error")) {
    missing <- find_missing_population_cells(df, dims)
    if (nrow(missing) > 0L) {
      preview <- utils::capture.output(print(utils::head(missing, 5L), row.names = FALSE))
      cli::cli_abort(c(
        "Missing population cells under {.val completion_policy = 'error'}.",
        "x" = "{nrow(missing)} dimension combinations are absent from the source table.",
        "i" = "First missing combinations:",
        paste(preview, collapse = "\n")
      ))
    }
    return(df)
  }

  full <- tidyr::complete(df, !!!rlang::syms(dims))

  if (identical(policy, "zero")) {
    full[[data_col]][is.na(full[[data_col]])] <- 0
  }

  full
}

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

  dup <- duplicated(df[c(dims)])
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

  arr_df <- df[fields]
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
  df <- df[, unique(c(dims, data_col)), drop = FALSE]
  df <- apply_completion_policy(
    df,
    dims = dims,
    policy = completion_policy,
    data_col = data_col
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
