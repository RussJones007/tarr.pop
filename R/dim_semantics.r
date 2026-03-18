# DimSemantics stores intrinsic semantic descriptors only. Current overlap presence is computed by poparray methods from
# current labels. overlap_levels is a list of known overlap-causing levels (may be empty if unknown). Interval overlap
# is determined from label parsing (outside S7 object), not by overlap_levels.

#' DimSemantics S7 Class
#'
#' Declarative semantic contract for a single array dimension.
#'
#' `DimSemantics` stores intrinsic semantics only and must not store current-state
#' facts such as whether overlaps are currently present in a filtered cube.
#' The object describes the dimension contract and its known overlap-causing
#' levels, while overlap status for any realized or filtered cube is computed
#' from the active labels at that point in the workflow.
#'
#' @section Fields:
#' - `dim_name`: Character scalar. Dimension name in the array (e.g., `"race"`).
#' - `domain`: Character scalar semantic key (e.g., `"age"`, `"race"`).
#' - `scale_type`: Character scalar in `c("nominal", "ordinal", "interval")`.
#' - `partition_type`: Character scalar in `c("partition", "set", "unknown")`.
#' - `validated`: Non-missing logical scalar.
#' - `overlap_levels`: Character vector of known overlap-causing levels.
#' - `notes`: Free-text character vector.
#'
#' @section Validation Rules:
#' - `dim_name` must be non-missing, length 1, and non-empty.
#' - `domain` must be non-missing, length 1, and non-empty.
#' - `scale_type` must be one of `nominal`, `ordinal`, `interval`.
#' - `partition_type` must be one of `partition`, `set`, `unknown`.
#' - `validated` must be non-missing logical(1).
#' - If `partition_type == "partition"`, then `overlap_levels` must be empty.
#'
#' `overlap_levels = character()` does not imply unconditional safety. Poparray
#' guards may still consider context such as level count or interval overlap tests.
#'
#' @name DimSemantics
#' @docType class
#' @keywords internal
NULL

.allowed_scale_types <- c("nominal", "ordinal", "interval")
.allowed_partition_types <- c("partition", "set", "unknown")

DimSemantics <- S7::new_class(
  "DimSemantics",
  properties = list(
    dim_name = S7::class_character,
    domain = S7::class_character,
    scale_type = S7::class_character,
    partition_type = S7::class_character,
    validated = S7::class_logical,
    overlap_levels = S7::class_character,
    notes = S7::class_character
  ),
  validator = function(self) {
    probs <- character()

    if (length(self@dim_name) != 1L || is.na(self@dim_name) || !nzchar(self@dim_name)) {
      probs <- c(probs, "@dim_name must be a non-empty character(1).")
    }

    if (length(self@domain) != 1L || is.na(self@domain) || !nzchar(self@domain)) {
      probs <- c(probs, "@domain must be a non-empty character(1).")
    }

    if (
      length(self@scale_type) != 1L ||
      is.na(self@scale_type) ||
      !(self@scale_type %in% .allowed_scale_types)
    ) {
      probs <- c(
        probs,
        sprintf("@scale_type must be one of: %s.", paste(.allowed_scale_types, collapse = ", "))
      )
    }

    if (
      length(self@partition_type) != 1L ||
      is.na(self@partition_type) ||
      !(self@partition_type %in% .allowed_partition_types)
    ) {
      probs <- c(
        probs,
        sprintf("@partition_type must be one of: %s.", paste(.allowed_partition_types, collapse = ", "))
      )
    }

    if (length(self@validated) != 1L || is.na(self@validated)) {
      probs <- c(probs, "@validated must be logical(1) and not NA.")
    }

    if (identical(self@partition_type, "partition") && length(self@overlap_levels) != 0L) {
      probs <- c(probs, "@overlap_levels must be empty when @partition_type == 'partition'.")
    }

    if (length(probs) == 0L) NULL else probs
  }
)

#' Construct a DimSemantics object
#'
#' @param dim_name Character scalar dimension name.
#' @param domain Character scalar domain key.
#' @param scale_type Character scalar in `c("nominal", "ordinal", "interval")`.
#' @param partition_type Character scalar in
#'   `c("partition", "set", "unknown")`.
#' @param validated Logical scalar indicating whether semantics have been
#'   externally validated.
#' @param overlap_levels Character vector of known overlap-causing levels.
#' @param notes Free-text character vector.
#'
#' @return A validated `DimSemantics` object.
#' @keywords internal
new_dim_semantics <- function(dim_name,
                              domain,
                              scale_type,
                              partition_type = "unknown",
                              validated = FALSE,
                              overlap_levels = character(),
                              notes = character()) {
  DimSemantics(
    dim_name = dim_name,
    domain = domain,
    scale_type = scale_type,
    partition_type = partition_type,
    validated = validated,
    overlap_levels = overlap_levels,
    notes = notes
  )
}

#' Predicate: interval scale
#'
#' @param sem Object to check.
#'
#' @return `TRUE` when `sem` is `DimSemantics` with `scale_type == "interval"`.
#' @keywords internal
pa_is_interval <- function(sem) {
  S7::S7_inherits(sem, DimSemantics) && identical(sem@scale_type, "interval")
}

#' Predicate: set partition type
#'
#' @param sem Object to check.
#'
#' @return `TRUE` when `sem` is `DimSemantics` with `partition_type == "set"`.
#' @keywords internal
pa_is_set <- function(sem) {
  S7::S7_inherits(sem, DimSemantics) && identical(sem@partition_type, "set")
}

#' Predicate: partition partition type
#'
#' @param sem Object to check.
#'
#' @return `TRUE` when `sem` is `DimSemantics` with
#'   `partition_type == "partition"`.
#' @keywords internal
pa_is_partition <- function(sem) {
  S7::S7_inherits(sem, DimSemantics) && identical(sem@partition_type, "partition")
}

#' Update a DimSemantics object
#'
#' Internal controlled updater for `DimSemantics` properties.
#'
#' @param sem A `DimSemantics` object.
#' @param ... Named properties to update.
#'
#' @return Updated `DimSemantics` object.
#' @keywords internal
pa_update_dim_semantics <- function(sem, ...) {
  if (!S7::S7_inherits(sem, DimSemantics)) {
    cli::cli_abort("{.arg sem} must inherit from {.cls DimSemantics}.")
  }

  dots <- list(...)
  if (!length(dots)) {
    return(sem)
  }

  if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
    cli::cli_abort("{.arg ...} must be named with DimSemantics property names.")
  }

  allowed_props <- S7::prop_names(sem)
  unknown <- setdiff(names(dots), allowed_props)
  if (length(unknown) > 0L) {
    cli::cli_abort("Unknown property name(s): {.val {paste(unknown, collapse = ', ')}}.")
  }

  for (nm in names(dots)) {
    S7::prop(sem, nm) <- dots[[nm]]
  }

  sem
}
