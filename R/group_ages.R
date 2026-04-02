# ------------------------------------------------------------------------------------------------------------------->
# Script: group_ages.R
# Description:
#   Age-group helpers built on top of collapse_dim().
# ------------------------------------------------------------------------------------------------------------------->

#' Collapse age labels into coarser groups
#'
#' `group_ages()` is a specialization of [collapse_dim()] for age dimensions. It
#' maps existing age labels to `age_groups`, then performs the grouped reduction
#' with the same semantic safety checks and blockwise backend processing used by
#' [collapse_dim()].
#'
#' @param pop A `poparray`.
#' @param age_groups Target age groups accepted by [rage::as.age_group()].
#' @param dimension_name Name of the age dimension. Defaults to `"age.char"`.
#' @param keep_empty Logical; keep declared `age_groups` levels even when no
#'   source labels map to them?
#' @param strict Logical; when `TRUE` (default), unsafe grouped reductions are
#'   blocked. When `FALSE`, a warning is emitted and the grouping proceeds.
#' @param allow_overlap Logical; default `FALSE`. Set `TRUE` to explicitly allow
#'   grouping overlapping age categories.
#'
#' @returns A `poparray` with `dimension_name` collapsed to the requested age
#'   groups.
#' @export
#'
#' @examples
#' # Load a poparray, subset lazily, then group ages.
#' # census <- open_poparray(population$census.bureau$census)
#' # census <- dplyr::filter(census, year %in% c("2010", "2020"))
#' # census_age_grouped <- group_ages(census, rage::age_groups$ILI)
group_ages <- function(pop,
                       age_groups,
                       dimension_name = "age.char",
                       keep_empty = FALSE,
                       strict = TRUE,
                       allow_overlap = FALSE) {
  validate_poparray(pop)

  dn <- dimnames(pop)
  if (!dimension_name %in% names(dn)) {
    cli::cli_abort("Dimension {.val {dimension_name}} was not found in {.cls poparray}.")
  }

  current_age <- rage::as.age_group(as.character(dn[[dimension_name]]))
  target_age <- rage::as.age_group(age_groups)

  al <- ivs::iv_locate_overlaps(
    needles = current_age,
    haystack = target_age,
    relationship = "many-to-one"
  ) |>
    ivs::iv_align(current_age, target_age, locations = _)

  target_age_unique <- unique(target_age)
  grouping <- purrr::map(target_age_unique, function(age_group) {
    as.character(al[al$haystack == age_group, "needles"])
  })
  names(grouping) <- as.character(target_age_unique)

  collapse_dim(
    x = pop,
    dim = dimension_name,
    groups = grouping,
    keep_empty = keep_empty,
    strict = strict,
    allow_overlap = allow_overlap
  )
}

#' Group a poparray dimension by level mappings
#'
#' Internal wrapper around [collapse_dim()] that preserves the older helper API
#' while routing all grouped reductions through the current `poparray`
#' implementation.
#'
#' @param arr A `poparray`.
#' @param dim_name Name of the dimension to group.
#' @param groups Group specification accepted by [collapse_dim()].
#' @param keep_unmapped What to do with source levels not present in `groups`:
#'   `"error"`, `"drop"`, `"keep"`, or `"other"`.
#' @param other_label Label used when `keep_unmapped = "other"`.
#' @param strict Logical; when `TRUE` (default), unsafe grouped reductions are
#'   blocked. When `FALSE`, a warning is emitted and the grouping proceeds.
#' @param allow_overlap Logical; default `FALSE`. Set `TRUE` to explicitly allow
#'   grouping overlapping categories.
#'
#' @returns A `poparray`.
group_array_by_levels <- function(arr,
                                  dim_name,
                                  groups,
                                  keep_unmapped = c("error", "drop", "keep", "other"),
                                  other_label = "Other",
                                  strict = TRUE,
                                  allow_overlap = FALSE) {
  validate_poparray(arr)
  keep_unmapped <- match.arg(keep_unmapped)

  dn <- dimnames(arr)
  if (!dim_name %in% names(dn)) {
    cli::cli_abort("Dimension {.val {dim_name}} was not found in {.cls poparray}.")
  }
  old_levels <- dn[[dim_name]]
  mapped <- normalize_groups(groups, old_levels)
  unmapped <- is.na(mapped)

  if (any(unmapped)) {
    if (identical(keep_unmapped, "error")) {
      cli::cli_abort(c(
        "Unmapped levels remain in {.val {dim_name}}.",
        "i" = "Examples: {.val {paste(utils::head(old_levels[unmapped], 10L), collapse = ', ')}}."
      ))
    }
    if (identical(keep_unmapped, "keep")) {
      mapped[unmapped] <- old_levels[unmapped]
    }
    if (identical(keep_unmapped, "other")) {
      mapped[unmapped] <- other_label
    }
  }

  collapse_dim(
    x = arr,
    dim = dim_name,
    groups = mapped,
    strict = strict,
    allow_overlap = allow_overlap
  )
}
