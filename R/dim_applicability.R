#' Read optional dimension applicability, including older semantic objects
#' @param sem A DimSemantics object.
#' @return An applicability list or NULL.
#' @keywords internal
#' @noRd
pa_dim_applicability <- function(sem) {
  if (!"applicability" %in% S7::prop_names(sem)) return(NULL)
  sem@applicability
}

#' Validate the small applicability list independently of a population cube
#' @param applicability Optional applicability list.
#' @return Character vector of structural problems (empty when valid).
#' @keywords internal
#' @noRd
pa_applicability_structure_problems <- function(applicability) {
  if (is.null(applicability)) return(character())
  named_fields <- function(x, required, allowed) {
    is.list(x) && !is.null(names(x)) && !anyNA(names(x)) &&
      !anyDuplicated(names(x)) && all(required %in% names(x)) &&
      all(names(x) %in% allowed)
  }
  label_scalar <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  if (!named_fields(applicability, c("by", "schemas"), c("by", "schemas")) ||
      !label_scalar(applicability$by) || !is.list(applicability$schemas)) {
    return("@applicability must be NULL or a list with a non-empty character(1) 'by' and a list 'schemas'.")
  }
  problems <- lapply(seq_along(applicability$schemas), function(i) {
    schema <- applicability$schemas[[i]]
    if (!named_fields(schema, "levels", c("from", "through", "levels")) ||
        (!is.null(schema$from) && !label_scalar(schema$from)) ||
        (!is.null(schema$through) && !label_scalar(schema$through)) ||
        !is.character(schema$levels) || anyNA(schema$levels) ||
        any(!nzchar(schema$levels)) || anyDuplicated(schema$levels)) {
      return(sprintf("@applicability schema %d must have optional scalar 'from'/'through' labels and unique, non-missing character 'levels'.", i))
    }
    character()
  })
  unlist(problems, use.names = FALSE)
}

#' Resolve applicability ranges against the current ordered labels
#' @param applicability Applicability list.
#' @param dim_name Target dimension name.
#' @param dimnames_list Current named dimension labels.
#' @return List of controlling-label index vectors, one per schema.
#' @keywords internal
#' @noRd
pa_applicability_indices <- function(applicability, dim_name, dimnames_list) {
  problems <- pa_applicability_structure_problems(applicability)
  if (length(problems)) cli::cli_abort(problems)
  by <- applicability$by
  if (identical(by, dim_name) || !by %in% names(dimnames_list)) {
    cli::cli_abort("Applicability for {.val {dim_name}} must name another existing dimension in {.field by}.")
  }
  controlling <- as.character(dimnames_list[[by]])
  if (anyNA(controlling) || any(!nzchar(controlling)) || anyDuplicated(controlling)) {
    cli::cli_abort("Applicability controlling dimension {.val {by}} must have unique, non-missing labels.")
  }
  indices <- lapply(applicability$schemas, function(schema) {
    if (!all(schema$levels %in% dimnames_list[[dim_name]])) {
      cli::cli_abort("Applicability for {.val {dim_name}} references unknown target levels.")
    }
    from <- if (is.null(schema$from)) 1L else match(schema$from, controlling)
    through <- if (is.null(schema$through)) length(controlling) else match(schema$through, controlling)
    if (is.na(from) || is.na(through)) {
      cli::cli_abort("Applicability for {.val {dim_name}} references an unknown boundary in {.val {by}}.")
    }
    if (!length(controlling) && is.null(schema$from) && is.null(schema$through)) return(integer())
    if (from > through) {
      cli::cli_abort("Applicability range for {.val {dim_name}} is reversed: from occurs after through.")
    }
    seq.int(from, through)
  })
  covered <- tabulate(as.integer(unlist(indices, use.names = FALSE)), nbins = length(controlling))
  if (any(covered > 1L)) {
    cli::cli_abort("Applicability schema ranges for {.val {dim_name}} overlap in {.val {by}}.")
  }
  if (any(covered == 0L)) {
    cli::cli_abort("Applicability for {.val {dim_name}} leaves current {.val {by}} labels uncovered.")
  }
  indices
}

#' Check all applicability references without inspecting population values
#' @param dim_semantics Named semantic entries.
#' @param dimnames_list Current named dimension labels.
#' @return Invisibly TRUE; errors on inconsistent metadata.
#' @keywords internal
#' @noRd
pa_validate_applicability <- function(dim_semantics, dimnames_list) {
  invisible(lapply(names(dim_semantics), function(nm) {
    applicability <- pa_dim_applicability(dim_semantics[[nm]])
    if (!is.null(applicability)) {
      indices <- pa_applicability_indices(applicability, nm, dimnames_list)
      sem <- dim_semantics[[nm]]
      if (pa_is_partition(sem) && pa_is_interval(sem) &&
          any(vapply(applicability$schemas[lengths(indices) > 0L], function(schema) {
            pa_has_interval_overlap(schema$levels)
          }, logical(1)))) {
        cli::cli_abort("Applicability for {.val {nm}} declares overlapping or unrecognized intervals within a partition schema.")
      }
    }
  }))
  invisible(TRUE)
}

#' Trim applicability to a lazy subset's controlling and target labels
#' @param sem Target DimSemantics object.
#' @param before_dimnames Original labels.
#' @param after_dimnames Subset labels.
#' @return Updated semantic entry; intrinsic overlap descriptors are preserved.
#' @keywords internal
#' @noRd
pa_subset_applicability <- function(sem, before_dimnames, after_dimnames) {
  applicability <- pa_dim_applicability(sem)
  if (is.null(applicability)) return(sem)
  nm <- sem@dim_name
  by <- applicability$by
  if (!by %in% names(after_dimnames)) {
    cli::cli_abort("Cannot retain applicability after its controlling dimension {.val {by}} was dropped.")
  }
  indices <- pa_applicability_indices(applicability, nm, before_dimnames)
  if (identical(before_dimnames[[by]], after_dimnames[[by]])) {
    applicability$schemas <- lapply(applicability$schemas, function(schema) {
      schema$levels <- intersect(schema$levels, after_dimnames[[nm]])
      schema
    })
  } else {
    assignments <- integer(length(before_dimnames[[by]]))
    for (i in seq_along(indices)) assignments[indices[[i]]] <- i
    current <- after_dimnames[[by]]
    selected <- assignments[match(current, before_dimnames[[by]])]
    runs <- rle(selected)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    # Split interleaved contexts into contiguous ranges in the current order.
    applicability$schemas <- lapply(seq_along(runs$values), function(i) {
      schema <- applicability$schemas[[runs$values[[i]]]]
      list(
        from = if (starts[[i]] == 1L) NULL else current[[starts[[i]]]],
        through = if (ends[[i]] == length(current)) NULL else current[[ends[[i]]]],
        levels = intersect(schema$levels, after_dimnames[[nm]])
      )
    })
  }
  pa_update_dim_semantics(sem, applicability = applicability)
}
