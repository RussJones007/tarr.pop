# Internal schema-specific reduction plans. All plans contain labels/indices,
# never population values; numerical extraction remains in collapse's blocks.
#' Build schema-specific grouping membership from current metadata
#' @param x A poparray.
#' @param dim_nm Target dimension name.
#' @param old_labels Retained current source labels.
#' @param new_levels Requested output labels.
#' @param group_index Output index for each source label.
#' @param schema_groups Optional internal interval-derived contributor plan.
#' @param strict Whether unsafe overlap raises an error.
#' @param allow_overlap Explicit overlap override.
#' @return Small schema mapping list, or NULL for legacy grouping.
#' @keywords internal
#' @noRd
pa_collapse_schema_plan <- function(x, dim_nm, old_labels, new_levels,
                                    group_index, schema_groups = NULL,
                                    strict = TRUE, allow_overlap = FALSE) {
  sem <- dim_semantics(x)[[dim_nm]]
  app <- pa_dim_applicability(sem)
  if (is.null(app)) return(NULL)
  dn <- dimnames(x)
  indices <- pa_applicability_indices(app, dim_nm, dn)
  contributors <- lapply(seq_along(app$schemas), function(i) {
    schema <- app$schemas[[i]]
    lapply(seq_along(new_levels), function(j) {
      labels <- if (is.null(schema_groups)) old_labels[group_index == j] else
        schema_groups$contributors[[i]][[j]]
      match(intersect(intersect(labels, schema$levels), old_labels), old_labels)
    })
  })
  unsafe <- vapply(seq_along(contributors), function(i) {
    any(vapply(contributors[[i]], function(cols) {
      labels <- old_labels[cols]
      if (pa_is_interval(sem)) pa_has_interval_overlap(labels) else
        pa_labels_have_overlap_risk(sem, labels)
    }, logical(1)))
  }, logical(1))
  if (any(unsafe) && !isTRUE(allow_overlap)) {
    msg <- c(
      "Unsafe collapse blocked: overlapping contributors within an applicability schema.",
      "i" = "Dimension: {.val {dim_nm}}.",
      "i" = "Use {.code overlaps(x)} to inspect overlap risk and declared overlap levels.",
      "i" = "Use {.code drop_overlap_levels(x, dim = {encodeString(dim_nm, quote = '\"')})} to remove declared overlap levels, or filter to non-overlapping levels, then retry {.fn collapse_dim}.",
      "i" = "Set {.arg allow_overlap = TRUE} to bypass, or {.arg strict = FALSE} to warn and continue."
    )
    if (isTRUE(strict)) cli::cli_abort(msg) else cli::cli_warn(msg)
  }
  output_levels <- lapply(contributors, function(groups) new_levels[lengths(groups) > 0L])
  missing <- lapply(output_levels, function(levels) setdiff(new_levels, levels))
  affected <- which(lengths(missing) > 0L & lengths(indices) > 0L)
  if (length(affected)) {
    summaries <- vapply(affected, function(i) {
      periods <- dn[[app$by]][indices[[i]]]
      paste0(paste(missing[[i]], collapse = ", "), " [", app$by, ": ",
        periods[[1L]], if (length(periods) > 1L) paste0(" to ", utils::tail(periods, 1L)) else "", "]")
    }, character(1))
    warning("Non-derivable output groups remain NA: ", paste(summaries, collapse = "; "), call. = FALSE)
  }
  result_app <- app
  result_app$schemas <- lapply(seq_along(app$schemas), function(i) {
    schema <- app$schemas[[i]]
    schema$levels <- output_levels[[i]]
    schema
  })
  # Retain a uniform operational schema so subsequent age grouping still checks
  # exact coverage instead of falling back to the legacy overlap-only mapping.
  if (length(output_levels) &&
      all(vapply(output_levels, identical, logical(1), output_levels[[1L]]))) {
    result_app$schemas <- list(list(from = NULL, through = NULL,
      levels = output_levels[[1L]]))
  }
  schema_ids <- integer(length(dn[[app$by]]))
  for (i in seq_along(indices)) schema_ids[indices[[i]]] <- i
  list(contributors = contributors, controller = match(app$by, names(dn)),
       schema_ids = schema_ids, n_new = length(new_levels),
       source_schemas_differed = length(unique(lapply(app$schemas,
         function(schema) sort(schema$levels)))) > 1L,
       result_applicability = result_app)
}

#' Reduce one bounded source block using schema-specific membership
#' @param mat_old Matrix of bounded block values with source labels in columns.
#' @param block_dim Block dimensions after target-last permutation.
#' @param block_idx Indices of non-target dimensions in the source cube.
#' @param perm Target-last dimension permutation.
#' @param plan Metadata-only schema mapping.
#' @return Matrix of reduced values, preserving applicable missingness.
#' @keywords internal
#' @noRd
pa_collapse_schema_block <- function(mat_old, block_dim, block_idx, perm, plan) {
  # Matrix rows enumerate all non-target dimensions in R's column-major order.
  controller_pos <- match(plan$controller, perm[-length(perm)])
  before <- if (controller_pos == 1L) 1L else prod(block_dim[seq_len(controller_pos - 1L)])
  after <- if (controller_pos == length(block_idx)) 1L else
    prod(block_dim[seq.int(controller_pos + 1L, length(block_idx))])
  row_schemas <- rep(rep(plan$schema_ids[block_idx[[controller_pos]]], each = before), times = after)
  result <- matrix(NA_real_, nrow = nrow(mat_old), ncol = plan$n_new)
  for (i in unique(row_schemas)) {
    rows <- which(row_schemas == i)
    for (j in seq_len(plan$n_new)) {
      cols <- plan$contributors[[i]][[j]]
      if (length(cols)) {
        # Exclusion happens before summation; applicable NA remains a summand.
        result[rows, j] <- rowSums(mat_old[rows, cols, drop = FALSE], na.rm = FALSE)
      }
    }
  }
  result
}
