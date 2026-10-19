#' Aligned numeric measures with explicit statistical contracts
#'
#' Coordinates, roles and dimension semantics must match exactly. Compatibility
#' fields must explicitly describe source, product, period, geography/vintage and
#' universe; equal shapes alone never establish alignment. Unknown measures may
#' be stored but cannot be reduced. Existing single-measure cubes are unchanged.
#' @param measures Named list of poparrays.
#' @param metadata Named list of measure contracts. Each includes statistic,
#'   unit, method, compatibility, and annotations. MOEs additionally require
#'   estimate, uncertainty_kind, confidence_level, derivation and status.
#' @return An aligned `poparray_measures` container with delayed measure arrays.
#' @aliases collapse_dim,poparray_measures-method poparray_measures-class
#' @details Use `collapse_dim()` to reduce all linked measures over one validated
#'   grouping. Counts use sums; ACS MOEs use root-sum-of-squares, omitting covariance.
#'   Missing contributors invalidate both paired outputs. Time reductions are blocked.
#' @export
poparray_measures <- function(measures, metadata) {
  checkmate::assert_list(measures, min.len = 1L, names = "unique")
  if (!identical(names(measures), names(metadata))) cli::cli_abort("Measure metadata names/order must match measures.")
  ref <- measures[[1L]]
  if (!all(vapply(measures, is, logical(1), "poparray"))) cli::cli_abort("Measures must be poparrays.")
  for (id in names(measures)) {
    a <- measures[[id]]
    if (!identical(dimnames(a), dimnames(ref)) ||
        !identical(time_role(a), time_role(ref)) || !identical(area_role(a), area_role(ref)) ||
        !identical(dim_semantics(a), dim_semantics(ref))) cli::cli_abort("Measure coordinates, roles and semantics must align exactly.")
    m <- metadata[[id]]
    for (field in c("statistic", "unit", "method")) checkmate::assert_string(m[[field]], min.chars = 1L)
    for (field in c("source", "product", "period", "geography", "vintage", "universe")) {
      if (is.null(m$compatibility[[field]]) || anyNA(m$compatibility[[field]])) cli::cli_abort("Explicit compatibility field {.val {field}} is required.")
    }
    if (is.null(m$annotations)) m$annotations <- character()
    m$id <- id
    m$dataset <- paste0("cube/measures/", id)
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", id)) cli::cli_abort("Measure IDs must be safe dataset names.")
    if (identical(m$statistic, "moe")) {
      checkmate::assert_number(m$confidence_level, lower = 0, upper = 1)
      if (m$confidence_level == 0 || m$confidence_level == 1) cli::cli_abort("Confidence level must be between zero and one.")
      if (!identical(m$uncertainty_kind, "margin_of_error") ||
          !m$status %in% c("published", "derived")) cli::cli_abort("MOE uncertainty kind and status are required.")
      checkmate::assert_string(m$derivation, min.chars = 1L)
      if (is.null(m$estimate) || !m$estimate %in% names(measures)) cli::cli_abort("Required companion estimate is absent.")
      est <- metadata[[m$estimate]]
      if (!identical(est$statistic, "count") || !identical(est$unit, m$unit) ||
          !identical(est$compatibility, m$compatibility)) cli::cli_abort("Estimate/MOE units, periods, universe and source compatibility must match.")
    }
    metadata[[id]] <- m
  }
  methods::new("poparray_measures", measures = measures, metadata = metadata)
}

setClass("poparray_measures", slots = c(measures = "list", metadata = "list"))

# Registered reducers operate around the existing bounded-memory sum kernel.
pa_measure_reducers <- list(
  sum = list(before = identity, after = identity),
  acs_moe = list(before = function(x) x^2, after = sqrt, type = "double")
)

#' Select aligned measures and coordinates lazily
#' @param x A measure container.
#' @param measures Measure IDs to retain.
#' @param indices Named list of dimension indices, applied to every measure.
#' @return A measure container. Selecting an MOE requires its companion estimate.
#' @export
select_measures <- function(x, measures = names(x@measures), indices = list()) {
  if (!all(measures %in% names(x@measures)) || !length(measures) || anyDuplicated(measures)) cli::cli_abort("Unknown or duplicate measures.")
  dn <- dimnames(x@measures[[1L]])
  if (length(indices) && (is.null(names(indices)) || anyDuplicated(names(indices)) || !all(names(indices) %in% names(dn)))) cli::cli_abort("Indices must name unique dimensions.")
  idx <- rep(list(TRUE), length(dn)); names(idx) <- names(dn)
  idx[names(indices)] <- indices
  arrays <- lapply(x@measures[measures], function(a) do.call(`[`, c(list(a), unname(idx), list(drop = FALSE))))
  poparray_measures(arrays, x@metadata[measures])
}

setMethod("collapse_dim", "poparray_measures", function(x, dim, groups,
  keep_empty = FALSE, name = NULL, strict = TRUE, allow_overlap = FALSE, drop = FALSE) {
  x <- poparray_measures(x@measures, x@metadata)
  ref <- x@measures[[1L]]
  dim_name <- if (is.character(dim)) dim else names(dimnames(ref))[dim]
  if (length(dim_name) != 1L || !dim_name %in% names(dimnames(ref))) cli::cli_abort("Unknown collapse dimension.")
  if (identical(dim_name, time_role(ref))) cli::cli_abort("Linked measure reductions across reference periods are unsupported.")
  arrays <- x@measures
  # A shared missingness mask guarantees identical contributors for each pair.
  for (id in names(arrays)) {
    m <- x@metadata[[id]]
    expected <- switch(m$statistic, count = "sum", moe = "acs_moe", "unsupported")
    if (!identical(m$method, expected) || !m$method %in% names(pa_measure_reducers)) cli::cli_abort("Unsupported measure reduction for {.val {id}}.")
  }
  uncertainty <- names(arrays)[vapply(x@metadata, function(m) m$statistic == "moe", logical(1))]
  estimates <- unique(vapply(x@metadata[uncertainty], `[[`, character(1), "estimate"))
  for (est in estimates) {
    ids <- uncertainty[vapply(x@metadata[uncertainty], function(m) identical(m$estimate, est), logical(1))]
    confidence <- vapply(x@metadata[ids], `[[`, numeric(1), "confidence_level")
    if (length(unique(confidence)) != 1L) cli::cli_abort("Linked MOEs must have matching confidence levels.")
    bad <- Reduce(`|`, c(list(is.na(arrays[[est]])), lapply(ids, function(id) {
      is.na(arrays[[id]]) | arrays[[id]] < 0
    })))
    for (id in c(est, ids)) {
      a <- arrays[[id]]; a[bad] <- NA_real_
      arrays[[id]] <- pa_measure_wrap(a, x@measures[[id]])
    }
  }
  plan <- collapse_dim_poparray_impl(ref, dim, groups, keep_empty, name,
    strict, allow_overlap, drop, prepare_only = TRUE)
  out <- lapply(names(arrays), function(id) {
    collapse_dim_poparray_impl(arrays[[id]], dim, groups, keep_empty, name,
      strict, allow_overlap, drop, reducer = pa_measure_reducers[[x@metadata[[id]]$method]], plan = plan)
  })
  names(out) <- names(arrays)
  meta <- x@metadata
  for (id in names(meta)) {
    meta[[id]]$aggregation <- list(dimension = dim_name,
      source_labels = dimnames(ref)[[dim_name]][plan$keep],
      groups = plan$new_levels[plan$g],
      missingness = "strict shared contributors")
    if (identical(dim_name, area_role(ref))) {
      meta[[id]]$compatibility$geography <- paste("user-defined groups of", meta[[id]]$compatibility$geography)
    }
  }
  for (id in names(meta)) if (meta[[id]]$statistic == "moe") {
    meta[[id]]$status <- "derived"
    meta[[id]]$derivation <- "sqrt(sum(component_moe^2)); ACS approximation omitting covariance"
  }
  poparray_measures(out, meta)
})

#' Save linked measures using shared coordinate metadata
#' @param x A measure container.
#' @param filepath Destination HDF5 file; must not exist.
#' @return Invisibly the destination path. Writes blockwise through HDF5Array.
#' @details This additive format has its own version marker. It does not migrate
#'   existing files. Metadata history covers shared metadata, not measure values.
#' @export
save_measures <- function(x, filepath) {
  x <- poparray_measures(x@measures, x@metadata)
  checkmate::assert_string(filepath, min.chars = 1L)
  if (file.exists(filepath)) cli::cli_abort("Destination exists; linked measure saves never overwrite files.")
  stage <- tempfile("measures_", tmpdir = dirname(filepath), fileext = ".h5")
  on.exit(unlink(stage), add = TRUE)
  save_poparray(x@measures[[1L]], stage)
  pa_h5_create_group(stage, "cube/measures")
  for (id in names(x@measures)) HDF5Array::writeHDF5Array(
    methods::as(x@measures[[id]], "DelayedArray"), filepath = stage,
    name = x@metadata[[id]]$dataset)
  rhdf5::h5delete(stage, "cube/population")
  pa_h5_write_dataset(stage, "cube/measure_format", "1.0.0")
  pa_h5_write_dataset(stage, "cube/measure_contracts", as.integer(serialize(x@metadata, NULL, version = 2)))
  if (!file.rename(stage, filepath)) cli::cli_abort("Could not install linked measure file.")
  invisible(filepath)
}

#' Open linked measures without realizing values
#' @param filepath HDF5 file written by `save_measures()`.
#' @return An HDF5-backed measure container. Legacy files must use `open_poparray()`;
#'   their statistical meaning is never inferred from a dataset or filename.
#' @export
open_measures <- function(filepath) {
  info <- h5_inventory(filepath)
  if (!h5_dataset_exists(info, "cube/measure_format")) cli::cli_abort("No measure contract; use open_poparray() for legacy single-measure files.")
  if (!identical(h5_read_scalar_chr(filepath, "cube/measure_format"), "1.0.0")) cli::cli_abort("Unsupported measure format.")
  meta <- unserialize(as.raw(rhdf5::h5read(filepath, "cube/measure_contracts")))
  arrays <- lapply(meta, function(m) open_poparray(filepath = filepath, dataset = m$dataset, data_col = m$id))
  poparray_measures(arrays, meta)
}

#' Restore the poparray contract after a delayed numeric transformation
#' @param values Delayed numeric result.
#' @param template Original poparray.
#' @return A poparray retaining the template metadata.
#' @noRd
pa_measure_wrap <- function(values, template) {
  new_poparray(values, dimnames_list = dimnames(template),
    time_dim = time_role(template), area_dim = area_role(template),
    dim_semantics = dim_semantics(template), source = get_source(template),
    data_col = data_col(template), group_vars = template@group_vars)
}
