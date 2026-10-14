#' Summarise a population cube over ungrouped dimensions
#'
#' Reduce dimensions not selected by `group_by()` or `.by`, retaining every
#' coordinate combination of the grouping dimensions. This first implementation
#' accepts exactly one named population sum: `total = sum(population)`, where
#' `population` is the current [data_col()]. `sum(..., na.rm = TRUE)` excludes
#' applicable missing values; structural exclusions still use applicability
#' metadata. Other expressions and multiple measures are not supported.
#'
#' @param .data A `poparray`.
#' @param ... Exactly one named expression, `name = sum(value, na.rm = FALSE)`.
#'   The value can be its bare column name, `.data$value`, or `.data[["value"]]`.
#'   `base::sum()` is also supported. The output name must be non-blank and must
#'   not collide with a retained dimension name.
#' @param .by Optional tidy-select expression selecting dimensions for this
#'   operation only. Cannot be combined with existing grouping or `.groups`.
#' @param .groups Output grouping: `"drop_last"` (the default), `"drop"`, or
#'   `"keep"`. Dropping grouping metadata does not drop retained cube dimensions.
#'   `.by` results are always ungrouped; rowwise results are not supported.
#' @return An HDF5-backed `poparray`, with a single renamed
#'   value column and retained dimensions in their original axis order.
#' @details This is a computation boundary: population values are read in bounded
#'   blocks and the result is written to temporary HDF5 storage. The full source
#'   cube is not realized in memory; subsequent operations remain delayed. Sum
#'   results use double storage to avoid integer overflow. Grouping itself is lazy.
#'   With no reduced dimensions and `na.rm = FALSE`, or with no output coordinate
#'   combinations, only metadata and delayed views are changed.
#'
#'   Time and area dimensions, and dimensions controlling applicability, must be
#'   included in grouping. Ungrouped whole-cube summaries are rejected because
#'   they cannot preserve the `poparray` role contract. Overlap checks run before
#'   any population reads, for each reduced dimension and applicability schema.
#'   There is no overlap override in this method. Filter overlapping categories
#'   or use [group_ages()] to construct suitable age bands before summarising.
#'
#'   Structural exclusions are omitted before summation. A group with no
#'   applicable contributors remains `NA` and issues the existing reducer's
#'   warning; applicable missing values propagate unless `na.rm = TRUE`.
#'   Empty retained dimensions produce an empty cube. Empty reduced dimensions
#'   are rejected. Reduction provenance is appended to the source note. Grouping
#'   metadata is an in-memory setting and is not saved in HDF5 cube metadata.
#' @seealso [group_by.poparray()], [collapse_dim()], [collapse_all()]
#' @importFrom dplyr summarise summarize
#' @exportS3Method summarise poparray
#' @examples
#' path <- system.file("extdata", "tdc_example.h5", package = "tarr.pop")
#' pop <- open_poparray(filepath = path)
#' # Sum explicit, disjoint single ages 1-84; retain sex and race/ethnicity.
#' single_ages <- pop[age.char = as.character(1:84), drop = FALSE]
#' totals <- single_ages |>
#'   dplyr::group_by(year, area.name, sex, race.eth) |>
#'   dplyr::summarise(total = sum(population), .groups = "drop")
#' names(dimnames(totals))
#' data_col(totals)
summarise.poparray <- function(.data, ..., .by = NULL, .groups = NULL) {
  validate_poparray(.data)
  expressions <- rlang::enquos(...)
  if (length(expressions) != 1L || is.null(names(expressions)) ||
      !nzchar(trimws(names(expressions)[[1L]]))) {
    cli::cli_abort("Supply exactly one named population sum, such as {.code total = sum(population)}.")
  }
  specification <- pa_parse_population_sum(expressions[[1L]], data_col(.data))
  by <- rlang::enquo(.by)
  has_by <- !rlang::quo_is_null(by)
  if (has_by && (length(.data@group_vars) || !is.null(.groups))) {
    cli::cli_abort("{.arg .by} cannot be combined with existing grouping or {.arg .groups}.")
  }
  grouping <- if (has_by) pa_select_group_dimensions(.data, by) else .data@group_vars
  if (!is.null(.groups)) {
    checkmate::assert_choice(.groups, c("drop_last", "drop", "keep"))
  }
  output_grouping <- if (has_by || identical(.groups, "drop")) character() else
    if (identical(.groups, "keep")) grouping else utils::head(grouping, -1L)
  dn <- dimnames(.data)
  controllers <- unique(unlist(lapply(dim_semantics(.data), function(sem) {
    app <- pa_dim_applicability(sem)
    if (is.null(app)) character() else app$by
  }), use.names = FALSE))
  required <- unique(c(time_role(.data), area_role(.data), controllers))
  absent <- setdiff(required, grouping)
  if (length(absent)) {
    cli::cli_abort("Grouping must retain required role and applicability-controller dimensions: {.val {absent}}.")
  }
  reduced <- setdiff(names(dn), grouping)
  output_name <- names(expressions)[[1L]]
  if (output_name %in% grouping) {
    cli::cli_abort("The summary name must not collide with a retained dimension name.")
  }
  if (any(lengths(dn[reduced]) == 0L)) {
    cli::cli_abort("Cannot summarise an empty reduction dimension; select non-empty coordinates first.")
  }
  # Preflight all axes before even the first block is read.
  for (dimension in reduced) {
    labels <- dn[[dimension]]
    # The executing reducer emits non-derivable-group warnings once per axis.
    plan <- suppressWarnings(pa_collapse_schema_plan(.data, dimension, labels,
      "Total", rep(1L, length(labels))))
    if (is.null(plan)) {
      pa_check_collapse_semantics(.data, dimension, labels, "Total",
                                  rep(1L, length(labels)), TRUE, FALSE)
    }
  }
  dx <- methods::as(.data, "DelayedArray") * 1.0
  out <- new_poparray(dx, dimnames_list = dn, data_col = data_col(.data),
    source = get_source(.data), time_dim = time_role(.data),
    area_dim = area_role(.data), dim_semantics = dim_semantics(.data))
  if (any(lengths(dn[grouping]) == 0L)) {
    # No output coordinates exist; remove the unused axes through a delayed reshape.
    indices <- lapply(names(dn), function(nm) {
      if (nm %in% reduced) 1L else seq_len(length(dn[[nm]]))
    })
    dx <- do.call(`[`, c(list(methods::as(out, "DelayedArray")), indices,
                         list(drop = FALSE)))
    kept <- dn[names(dn) %in% grouping]
    dim(dx) <- unname(lengths(kept))
    dimnames(dx) <- kept
    out <- new_poparray(dx, dimnames_list = kept, data_col = data_col(.data),
      source = get_source(.data), time_dim = time_role(.data),
      area_dim = area_role(.data), dim_semantics = dim_semantics(.data)[names(kept)])
  } else {
    for (dimension in reduced) {
      labels <- dimnames(out)[[dimension]]
      out <- collapse_dim_poparray_impl(out, dimension,
        stats::setNames(rep("Total", length(labels)), labels), drop = TRUE,
        na.rm = specification$na.rm)
    }
    if (!length(reduced) && specification$na.rm) {
      # One value per group still needs sum(NA, na.rm = TRUE) == 0.
      axis <- tail(make.unique(c(names(dn), ".summary_cell")), 1L)
      augmented <- c(dn, stats::setNames(list("cell"), axis))
      dx <- methods::as(out, "DelayedArray")
      dim(dx) <- unname(lengths(augmented))
      dimnames(dx) <- augmented
      sem <- c(dim_semantics(out), stats::setNames(list(new_dim_semantics(
        axis, axis, "nominal", partition_type = "partition", validated = TRUE)), axis))
      out <- new_poparray(dx, dimnames_list = augmented, data_col = data_col(out),
        source = get_source(out), time_dim = time_role(out), area_dim = area_role(out),
        dim_semantics = sem)
      out <- collapse_dim_poparray_impl(out, axis, c(cell = "Total"),
                                        drop = TRUE, na.rm = TRUE)
    }
    out <- pa_mask_summary_applicability(out, .data, reduced)
  }
  out@data_col <- output_name
  attr(out, "data_col") <- output_name
  source <- get_source(out)
  source$note <- paste(c(source$note[nzchar(source$note)], paste0(
    "summarise: ", output_name, " = sum(", data_col(.data),
    ", na.rm = ", specification$na.rm, "); retained dimensions: ",
    paste(grouping, collapse = ", "), "; reduced dimensions: ",
    if (length(reduced)) paste(reduced, collapse = ", ") else "none", ".")), collapse = "\n")
  source_meta(out) <- source
  pa_set_group_vars(out, output_grouping)
}

#' @rdname summarise.poparray
#' @exportS3Method summarize poparray
summarize.poparray <- summarise.poparray

#' Validate a supported population sum without evaluating population values
#' @param expression A summary quosure.
#' @param value_name Current value-column name.
#' @return A list with the validated na.rm flag.
#' @noRd
pa_parse_population_sum <- function(expression, value_name) {
  call <- rlang::quo_get_expr(expression)
  env <- rlang::quo_get_env(expression)
  supported <- rlang::is_call(call, "sum") || rlang::is_call(call, "sum", ns = "base")
  if (!supported || !identical(rlang::eval_bare(call[[1L]], env), base::sum)) {
    cli::cli_abort("Only a population sum with {.fn sum} or {.code base::sum} is supported.")
  }
  args <- as.list(call)[-1L]
  arg_names <- names(args) %||% rep("", length(args))
  if (length(args) < 1L || length(args) > 2L || arg_names[[1L]] != "" ||
      (length(args) == 2L && arg_names[[2L]] != "na.rm")) {
    cli::cli_abort("Use {.code sum(value)} or {.code sum(value, na.rm = TRUE)} with one population measure.")
  }
  value <- args[[1L]]
  name <- if (rlang::is_symbol(value)) rlang::as_string(value) else
    if (rlang::is_call(value, "$", n = 2L) && rlang::is_symbol(value[[2L]], ".data")) {
      rlang::as_string(value[[3L]])
    } else if (rlang::is_call(value, "[[", n = 2L) && rlang::is_symbol(value[[2L]], ".data")) {
      rlang::eval_bare(value[[3L]], env)
    } else NULL
  if (!identical(name, value_name)) {
    cli::cli_abort("The sum must reference the current population value column {.val {value_name}}.")
  }
  remove_na <- if (length(args) == 2L) rlang::eval_bare(args[[2L]], env) else FALSE
  checkmate::assert_flag(remove_na, .var.name = "na.rm")
  list(na.rm = remove_na)
}

#' Keep structural exclusions distinct from applicable missing values in summaries
#' @param out Summarised poparray.
#' @param source Original poparray.
#' @param reduced Removed dimensions.
#' @return A delayed view with structurally unavailable output cells set to NA.
#' @noRd
pa_mask_summary_applicability <- function(out, source, reduced) {
  dn <- dimnames(source)
  result_dn <- dimnames(out)
  dx <- methods::as(out, "DelayedArray")
  changed <- FALSE
  for (dimension in names(dn)) {
    app <- pa_dim_applicability(dim_semantics(source)[[dimension]])
    if (is.null(app)) next
    periods <- pa_applicability_indices(app, dimension, dn)
    for (i in seq_along(app$schemas)) {
      applicable <- intersect(app$schemas[[i]]$levels, dn[[dimension]])
      if (!length(periods[[i]])) next
      indices <- stats::setNames(rep(list(TRUE), length(result_dn)), names(result_dn))
      indices[[app$by]] <- periods[[i]]
      if (dimension %in% reduced) {
        if (length(applicable)) next
      } else {
        excluded <- which(!dn[[dimension]] %in% applicable)
        if (!length(excluded)) next
        indices[[dimension]] <- excluded
      }
      # Scalar subassignment builds a delayed overlay, using only metadata indices.
      dx <- do.call(`[<-`, c(list(dx), unname(indices), list(value = NA_real_)))
      changed <- TRUE
    }
  }
  if (!changed) return(out)
  new_poparray(dx, dimnames_list = result_dn, data_col = data_col(out),
    source = get_source(out), time_dim = time_role(out), area_dim = area_role(out),
    dim_semantics = dim_semantics(out))
}
