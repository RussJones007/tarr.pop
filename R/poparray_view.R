#' Persistent population view definitions (proof of concept)
#'
#' A view records transformations of one registered source series. It contains
#' no population values and does not inherit from `poparray`. The S7 class has
#' typed properties; saved definitions are plain lists, not serialized S7 objects.
#'
#' @param source_series_id Registered source cube series ID.
#' @param view_id Non-empty name for the view.
#' @param description,notes Descriptive character scalars.
#' @param created_by Creator name; defaults to the current OS user.
#' @param format_version Integer recipe version; currently `1L`.
#' @param source_contract Plain list of source dimensions, labels, roles, and
#'   semantics, normally captured by `new_poparray_view()`.
#' @param operations Ordered list of validated operation records.
#' @param metadata Plain list containing name, description, notes, and creation
#'   and modification provenance.
#' @return `new_poparray_view()` returns a validated S7 `poparray_view`.
#' @details
#' Definitions are saved under `views/` in the configured population root and
#' indexed separately from population cubes. Source compatibility compares dimension names/order, labels, roles,
#' and relevant semantics, excluding descriptive semantic notes. Current source
#' values and provenance are used on replay; a view is not a numeric snapshot.
#' Construction reads source metadata but not population values. Building and
#' saving do not execute transformations. Opening can perform bounded-memory
#' aggregation into session-temporary HDF5 files, which must remain available
#' while the returned array is in use. Do not serialize that returned array as
#' a substitute for saving its recipe.
#' @name poparray_view
#' @export
poparray_view <- S7::new_class(
  "poparray_view",
  properties = list(
    format_version = S7::class_integer,
    view_id = S7::class_character,
    source_series_id = S7::class_character,
    source_contract = S7::class_list,
    operations = S7::class_list,
    metadata = S7::class_list
  ),
  validator = function(self) {
    tryCatch({
      pa_validate_view_recipe(pa_view_recipe(self))
      NULL
    }, error = function(e) conditionMessage(e))
  }
)

#' @rdname poparray_view
#' @export
new_poparray_view <- function(source_series_id, view_id, description = "",
                              notes = "", created_by = Sys.info()[["user"]]) {
  checkmate::assert_string(source_series_id, min.chars = 1)
  source <- open_poparray(series_id = source_series_id)
  now <- pa_view_time()
  poparray_view(format_version = 1L, view_id = view_id,
    source_series_id = source_series_id,
    source_contract = pa_view_source_contract(source), operations = list(),
    metadata = list(name = view_id, description = description, notes = notes,
      created_by = created_by, created_at = now,
      modified_by = created_by, modified_at = now))
}

#' Add a population view operation
#'
#' Records arguments without executing population transformations. Only the
#' fixed operations listed below are accepted. Existing manipulation APIs are
#' unchanged. Arguments must be plain data; functions and expressions cannot be
#' stored. Use [view_filter()] for supported filter expressions.
#'
#' @param view A `poparray_view`.
#' @param op One of `subset`, `select`, `rename`, `relocate`, `collapse_dim`,
#'   `collapse_all`, or `group_ages`.
#' @param ... Named arguments. `subset` takes `indices`, a named list of label
#'   vectors (not numeric positions), and always retains dimensions. `select`
#'   takes `dimensions`, a character vector of existing names, optionally named
#'   with output names. `rename` takes `mapping`, a character vector with new
#'   names as names and old names as values. `relocate` takes `dimensions`, a
#'   complete character vector in desired order. Other operations take their
#'   existing exported function arguments, excluding the input population array.
#'   `group_ages` requires character age labels; closures are not supported.
#' @return A new validated view definition. Invalid dimension references or
#'   aggregation semantics are checked when replayed, not when recorded.
#' @export
view_step <- function(view, op, ...) {
  pa_assert_view(view)
  args <- list(...)
  pa_validate_view_step(list(op = op, args = args))
  defaults <- switch(op,
    collapse_dim = list(keep_empty = FALSE, name = NULL, strict = TRUE,
      allow_overlap = FALSE, drop = FALSE),
    collapse_all = list(label = "all", strict = TRUE, allow_overlap = FALSE, drop = FALSE),
    group_ages = list(dimension_name = "age.char", keep_empty = FALSE,
      strict = TRUE, allow_overlap = FALSE), list())
  defaults[names(args)] <- args
  pa_append_view_step(view, list(op = op, args = defaults))
}

#' Record filter predicates using the existing population filter parser
#'
#' External right-hand-side values and text patterns are evaluated now. Only
#' plain predicate data are retained; caller environments are discarded. Uses
#' the same predicate grammar and age semantics as [filter.poparray()].
#' @param view A `poparray_view`.
#' @param ... Unnamed predicates supported by `filter.poparray()`.
#' @param .strict Reject unknown labels during replay (default `TRUE`).
#' @return A new view definition; no population values are read.
#' @export
view_filter <- function(view, ..., .strict = TRUE) {
  pa_assert_view(view)
  checkmate::assert_flag(.strict)
  quos <- rlang::enquos(...)
  if (any(nzchar(names(quos)))) cli::cli_abort("Filter predicates must be unnamed.")
  predicates <- lapply(quos, function(q) {
    pred <- tp_parse_predicate(q, env = rlang::caller_env())
    pa_view_strip_env(pred)
  })
  pa_append_view_step(view, list(op = "filter",
    args = list(predicates = predicates, .strict = .strict)))
}

#' Record age grouping in a population view
#'
#' Convenience wrapper for `view_step(view, "group_ages", ...)`. Records target
#' age labels without reading population values. Replay delegates to
#' [group_ages()], retaining its interval, applicability, and overlap checks.
#' @param view A `poparray_view`.
#' @param age_groups Character vector of target age interval labels.
#' @param dimension_name Name of the age dimension at this point in the recipe.
#' @param keep_empty,strict,allow_overlap Passed to [group_ages()] during replay.
#' @return A validated `poparray_view` definition with an added age-grouping step.
#' @export
view_group_ages <- function(view, age_groups, dimension_name = "age.char",
                            keep_empty = FALSE, strict = TRUE,
                            allow_overlap = FALSE) {
  view_step(view, "group_ages", age_groups = age_groups,
    dimension_name = dimension_name, keep_empty = keep_empty,
    strict = strict, allow_overlap = allow_overlap)
}

#' Save, read, and replay population view recipes
#'
#' @param view A `poparray_view`; for `open_poparray_view()`, alternatively a
#'   saved view ID or path. Supply exactly one of `view`, `view_id`, or `filepath`.
#' @param view_id Saved view identifier for registry lookup.
#' @param filepath Path to a trusted RDS view definition. Saving defaults to
#'   `views/<view_id>.rds` in the configured population root. As with ordinary RDS
#'   files, do not load files from untrusted sources.
#' @param overwrite Replace an existing definition? Defaults to `FALSE`.
#' @param checkout,checkout_dir Forwarded to [open_poparray()] when opening the
#'   original source. There is no alternate-source argument.
#' @return `save_poparray_view()` invisibly returns the path;
#'   `read_poparray_view()` returns an S7 definition without opening its source;
#'   `open_poparray_view()` returns a valid, HDF5-backed `poparray`.
#' @details Reading or opening does not change modification timestamps. Save
#' updates modification provenance in the saved definition, preserves creation
#' provenance, and serializes only a versioned plain list without replaying it.
#' @name save_poparray_view
#' @export
save_poparray_view <- function(view, filepath = NULL, overwrite = FALSE) {
  pa_assert_view(view)
  managed <- is.null(filepath)
  if (managed) {
    root <- resolve_cube_dir()
    pa_view_check_filename(view@view_id)
    directory <- file.path(root, "views")
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
      cli::cli_abort("Could not create the views directory.")
    }
    filepath <- file.path(directory, paste0(view@view_id, ".rds"))
    registry <- list_poparray_views(root)
    existing <- registry[registry$view_id == view@view_id, , drop = FALSE]
    if (nrow(existing) && !identical(existing$filepath, file.path("views", basename(filepath)))) {
      cli::cli_abort("View ID already exists at another path: {view@view_id}.")
    }
  }
  checkmate::assert_string(filepath, min.chars = 1)
  checkmate::assert_flag(overwrite)
  if (file.exists(filepath) && !overwrite) cli::cli_abort("View file already exists.")
  checkmate::assert_directory_exists(dirname(filepath))
  recipe <- pa_view_recipe(view)
  recipe$metadata$modified_at <- pa_view_time()
  pa_write_view_rds(recipe, filepath)
  if (managed) list_poparray_views(root)
  invisible(filepath)
}

#' @rdname save_poparray_view
#' @export
read_poparray_view <- function(filepath = NULL, view_id = NULL) {
  filepath <- pa_resolve_view_path(filepath, view_id)
  recipe <- readRDS(filepath)
  pa_validate_view_recipe(recipe)
  do.call(poparray_view, recipe)
}

#' @rdname save_poparray_view
#' @export
open_poparray_view <- function(view = NULL, checkout = FALSE, checkout_dir = tempdir(),
                               view_id = NULL, filepath = NULL) {
  if (sum(!vapply(list(view, view_id, filepath), is.null, logical(1))) != 1L) {
    cli::cli_abort("Supply exactly one of view, view_id, or filepath.")
  }
  if (!is.null(view_id) || !is.null(filepath)) view <- read_poparray_view(filepath, view_id)
  if (is.character(view)) view <- read_poparray_view(view)
  pa_assert_view(view)
  source <- open_poparray(series_id = view@source_series_id,
    checkout = checkout, checkout_dir = checkout_dir)
  if (!identical(pa_view_source_contract(source), view@source_contract)) {
    cli::cli_abort("Source cube is incompatible with this view: dimensions, labels, roles, or semantics changed.")
  }
  purrr::reduce(seq_along(view@operations), function(x, i) {
    step <- view@operations[[i]]
    tryCatch({
      out <- pa_replay_view_step(x, step)
      if (!methods::is(out, "poparray")) cli::cli_abort("Operation must return a poparray.")
      validate_poparray(out)
      out
    }, error = function(e) cli::cli_abort(
      "View operation {i} ({step$op}) failed: {conditionMessage(e)}", parent = e))
  }, .init = source)
}

# Internal helpers deliberately operate only on data, never on saved R calls.
pa_view_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
pa_view_recipe <- function(view) {
  props <- c("format_version", "view_id", "source_series_id", "source_contract",
             "operations", "metadata")
  stats::setNames(lapply(props, function(p) S7::prop(view, p)), props)
}
pa_assert_view <- function(view) {
  if (!S7::S7_inherits(view, poparray_view)) cli::cli_abort("Expected a poparray_view definition.")
  S7::validate(view)
  invisible(view)
}
pa_view_plain <- function(x) {
  if (is.null(x)) return(TRUE)
  if (!is.null(attributes(x)) && !identical(names(attributes(x)), "names")) return(FALSE)
  if (is.list(x)) return(all(vapply(x, pa_view_plain, logical(1))))
  typeof(x) %in% c("character", "logical", "integer", "double")
}
pa_view_source_contract <- function(x) {
  fields <- c("dim_name", "domain", "scale_type", "partition_type", "validated",
              "overlap_levels", "applicability")
  list(dimnames = dimnames(x), time_role = time_role(x), area_role = area_role(x),
    semantics = lapply(dim_semantics(x), function(s) {
      stats::setNames(lapply(fields, function(p) S7::prop(s, p)), fields)
    }))
}
pa_append_view_step <- function(view, step) {
  recipe <- pa_view_recipe(view)
  recipe$operations <- c(recipe$operations, list(step))
  recipe$metadata$modified_at <- pa_view_time()
  recipe$metadata$modified_by <- Sys.info()[["user"]]
  do.call(poparray_view, recipe)
}
pa_view_strip_env <- function(pred) {
  pred$env <- NULL
  for (p in intersect(names(pred), c("child", "lhs", "rhs"))) {
    if (is.list(pred[[p]])) pred[[p]] <- pa_view_strip_env(pred[[p]])
  }
  pred
}
pa_validate_view_recipe <- function(r) {
  required <- c("format_version", "view_id", "source_series_id", "source_contract",
                "operations", "metadata")
  if (!pa_view_plain(r) || !is.list(r) || !identical(names(r), required)) {
    cli::cli_abort("A view recipe must contain only the documented plain-data fields.")
  }
  if (!identical(r$format_version, 1L)) cli::cli_abort("Unsupported view recipe format_version.")
  for (p in c("view_id", "source_series_id")) {
    checkmate::assert_string(r[[p]], min.chars = 1)
    if (!nzchar(trimws(r[[p]]))) cli::cli_abort("View and source identifiers cannot be blank.")
  }
  checkmate::assert_list(r$source_contract)
  if (!identical(names(r$source_contract), c("dimnames", "time_role", "area_role", "semantics"))) {
    cli::cli_abort("Invalid source compatibility contract.")
  }
  contract <- r$source_contract
  checkmate::assert_list(contract$dimnames, names = "unique", min.len = 2)
  invisible(lapply(contract$dimnames, checkmate::assert_character, any.missing = FALSE))
  for (p in c("time_role", "area_role")) checkmate::assert_choice(contract[[p]], names(contract$dimnames))
  if (identical(contract$time_role, contract$area_role)) cli::cli_abort("Source time and area roles must differ.")
  if (!is.list(contract$semantics) || !identical(names(contract$semantics), names(contract$dimnames))) {
    cli::cli_abort("Source semantics must match its dimensions.")
  }
  invisible(lapply(seq_along(contract$semantics), function(i) {
    sem <- contract$semantics[[i]]
    if (!is.list(sem) || !identical(names(sem), c("dim_name", "domain", "scale_type",
        "partition_type", "validated", "overlap_levels", "applicability"))) cli::cli_abort("Invalid source semantics.")
    if (!identical(sem$dim_name, names(contract$dimnames)[[i]])) cli::cli_abort("Source semantic dimension name mismatch.")
    do.call(new_dim_semantics, sem)
  }))
  checkmate::assert_list(r$operations)
  invisible(lapply(r$operations, pa_validate_view_step))
  if (!is.list(r$metadata) || !identical(names(r$metadata), c("name", "description", "notes",
      "created_by", "created_at", "modified_by", "modified_at"))) cli::cli_abort("Invalid view metadata.")
  for (p in names(r$metadata)) checkmate::assert_string(r$metadata[[p]])
  invisible(r)
}
pa_validate_view_predicate <- function(p) {
  if (!is.list(p) || !is.character(p$type) || length(p$type) != 1L || is.na(p$type)) {
    cli::cli_abort("Invalid filter predicate.")
  }
  fields <- switch(p$type, atom = c("type", "dim", "op", "rhs"),
    text = c("type", "dim", "op", "pattern", "negate"),
    not = c("type", "child"), bool = c("type", "op", "lhs", "rhs"), NULL)
  if (is.null(fields) || !identical(names(p), fields)) cli::cli_abort("Invalid filter predicate fields.")
  if (p$type == "not") return(pa_validate_view_predicate(p$child))
  if (p$type == "bool") {
    checkmate::assert_choice(p$op, c("&", "|"))
    pa_validate_view_predicate(p$lhs); pa_validate_view_predicate(p$rhs)
    tp_pred_dim(p) # Retain the existing same-dimension boolean restriction.
  } else {
    checkmate::assert_string(p$dim, min.chars = 1)
    if (p$type == "atom") {
      checkmate::assert_choice(p$op, c("==", "!=", "%in%", "<", "<=", ">", ">=", "%between%"))
      if (!is.atomic(p$rhs)) cli::cli_abort("Filter values must be atomic.")
    } else {
      checkmate::assert_choice(p$op, c("str_detect", "startsWith", "endsWith"))
      checkmate::assert_string(p$pattern)
      checkmate::assert_flag(p$negate)
    }
  }
  invisible(p)
}
pa_validate_view_step <- function(step) {
  if (!pa_view_plain(step) || !is.list(step) || !identical(names(step), c("op", "args"))) {
    cli::cli_abort("View operations must contain plain op and args fields.")
  }
  allowed <- list(filter = c("predicates", ".strict"), subset = "indices",
    select = "dimensions", rename = "mapping", relocate = "dimensions",
    collapse_dim = c("dim", "groups", "keep_empty", "name", "strict", "allow_overlap", "drop"),
    collapse_all = c("dim", "label", "strict", "allow_overlap", "drop"),
    group_ages = c("age_groups", "dimension_name", "keep_empty", "strict", "allow_overlap"))
  checkmate::assert_choice(step$op, names(allowed))
  a <- step$args
  if (!is.list(a) || (length(a) && (is.null(names(a)) || anyNA(names(a)) ||
      any(!nzchar(names(a))) || anyDuplicated(names(a))))) cli::cli_abort("Operation arguments must be uniquely named.")
  if (any(!names(a) %in% allowed[[step$op]])) cli::cli_abort("Unknown arguments for view operation {step$op}.")
  required <- switch(step$op, filter = c("predicates", ".strict"), subset = "indices",
    select = "dimensions", rename = "mapping", relocate = "dimensions",
    collapse_dim = c("dim", "groups"), collapse_all = "dim", group_ages = "age_groups")
  if (!all(required %in% names(a))) cli::cli_abort("Missing required arguments for view operation {step$op}.")
  for (p in intersect(names(a), c("keep_empty", "strict", "allow_overlap", "drop", ".strict"))) checkmate::assert_flag(a[[p]])
  for (p in intersect(names(a), c("dim", "dimension_name", "label", "name"))) {
    if (!(p == "name" && is.null(a[[p]]))) checkmate::assert_character(a[[p]], any.missing = FALSE, min.len = 1)
  }
  for (p in intersect(names(a), c("dimension_name", "label", "name"))) {
    if (!is.null(a[[p]])) checkmate::assert_string(a[[p]], min.chars = 1)
  }
  if (step$op == "collapse_dim") checkmate::assert_string(a$dim, min.chars = 1)
  if (step$op %in% c("select", "relocate")) checkmate::assert_character(a$dimensions, any.missing = FALSE)
  if (step$op == "rename") {
    checkmate::assert_character(a$mapping, any.missing = FALSE, min.len = 1)
    if (is.null(names(a$mapping)) || any(!nzchar(names(a$mapping))) || anyDuplicated(names(a$mapping))) cli::cli_abort("Rename mapping needs unique new names.")
  }
  if (step$op == "subset") {
    checkmate::assert_list(a$indices, names = "unique")
    invisible(lapply(a$indices, checkmate::assert_character, any.missing = FALSE))
  }
  if (step$op == "group_ages") checkmate::assert_character(a$age_groups, any.missing = FALSE, min.len = 1)
  if (step$op == "collapse_dim") {
    if (is.list(a$groups)) {
      checkmate::assert_list(a$groups, names = "unique", min.len = 1)
      invisible(lapply(a$groups, checkmate::assert_character, any.missing = FALSE))
    } else {
      checkmate::assert_character(a$groups, any.missing = FALSE, min.len = 1)
      if (is.null(names(a$groups))) cli::cli_abort("Groups must be a named character mapping or named list.")
    }
  }
  if (step$op == "filter") {
    checkmate::assert_list(a$predicates)
    invisible(lapply(a$predicates, pa_validate_view_predicate))
  }
  invisible(step)
}
pa_replay_view_step <- function(x, step) {
  a <- step$args
  switch(step$op,
    filter = {
      idx <- stats::setNames(rep(list(TRUE), length(dim(x))), names(dimnames(x)))
      for (pred in a$predicates) {
        d <- tp_pred_dim(pred)
        keep <- tp_eval_pred_tree(x, pred, .strict = a$.strict)
        idx[[d]] <- idx[[d]] & keep
      }
      do.call(`[`, c(list(x), unname(idx), list(drop = FALSE)))
    },
    subset = {
      if (any(!names(a$indices) %in% names(dimnames(x)))) cli::cli_abort("Unknown subset dimension.")
      idx <- lapply(names(dimnames(x)), function(d) {
        if (!d %in% names(a$indices)) return(TRUE)
        tp_check_known_labels(a$indices[[d]], dimnames(x)[[d]], d, TRUE)
        a$indices[[d]]
      })
      do.call(`[`, c(list(x), idx, list(drop = FALSE)))
    },
    select = dplyr::select(x, tidyselect::all_of(a$dimensions)),
    rename = dplyr::rename(x, tidyselect::all_of(a$mapping)),
    relocate = {
      if (!setequal(a$dimensions, names(dimnames(x))) || anyDuplicated(a$dimensions)) cli::cli_abort("Relocate requires every dimension exactly once.")
      dplyr::relocate(x, tidyselect::all_of(a$dimensions))
    },
    collapse_dim = do.call(collapse_dim, c(list(x = x), a)),
    collapse_all = do.call(collapse_all, c(list(x = x), a)),
    group_ages = do.call(group_ages, c(list(pop = x), a)),
    cli::cli_abort("Unsupported view operation."))
}
