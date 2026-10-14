#' Rename poparray dimensions
#'
#' Rename dimensions using `dplyr::rename()` syntax, `new_name = old_name`.
#' Category labels, dimension order, population values, and provenance are
#' preserved. This is a lazy metadata operation: no population values are read
#' and the backing HDF5 file is not changed. Time and area roles, semantic
#' dimension names, and applicability controller references follow the rename.
#' Semantic domains retain their meaning, including age interval filtering.
#'
#' @param .data A `poparray`.
#' @param ... Named tidy-select expressions specifying dimension renames.
#' @return A `poparray` with renamed dimensions and consistent metadata.
#' @importFrom dplyr rename
#' @exportS3Method rename poparray
#' @examples
#' \dontrun{
#' pop |> dplyr::rename(age = age.char, county = area.name)
#' }
rename.poparray <- function(.data, ...) {
  dn <- dimnames(.data)
  old_names <- names(dn)
  selection <- tidyselect::eval_rename(
    rlang::expr(c(...)), data = stats::setNames(seq_along(dn), old_names)
  )
  if (!length(selection)) return(.data)
  new_names <- old_names
  new_names[unname(selection)] <- names(selection)
  pa_rename_dimensions(.data, new_names)
}

#' Rename poparray dimensions with a function
#'
#' Apply a function to selected dimension names, preserving coordinate labels,
#' dimension order, semantic domains, and provenance. This is a lazy metadata
#' operation: no population values are read or backing files modified. Time and
#' area roles and applicability controller references follow the new names.
#'
#' @param .data A `poparray`.
#' @param .fn A function, function name, or formula transforming selected names.
#'   It must return a character vector of the same length as its input, including
#'   length zero for an empty selection.
#' @param .cols Tidy-select expression selecting dimension names; defaults to all
#'   dimensions. Selectors operate on dimensions, not population values.
#' @param ... Additional arguments passed to `.fn`.
#' @return A delayed, HDF5-backed `poparray` with consistent renamed metadata.
#'   Missing, blank, and duplicated resulting names are rejected.
#' @seealso [rename.poparray()], [relocate.poparray()]
#' @importFrom dplyr rename_with
#' @exportS3Method rename_with poparray
#' @examples
#' path <- system.file("extdata", "tdc_example.h5", package = "tarr.pop")
#' pop <- open_poparray(filepath = path)
#' renamed <- dplyr::rename_with(pop, toupper)
#' names(dimnames(renamed))
#' time_role(renamed)
rename_with.poparray <- function(.data, .fn, .cols = dplyr::everything(), ...) {
  validate_poparray(.data)
  old_names <- names(dimnames(.data))
  selection <- tidyselect::eval_select(
    rlang::enquo(.cols),
    data = stats::setNames(seq_along(old_names), old_names),
    allow_rename = FALSE
  )
  .fn <- rlang::as_function(.fn)
  renamed <- .fn(old_names[unname(selection)], ...)
  if (!is.character(renamed) || length(renamed) != length(selection)) {
    cli::cli_abort("{.arg .fn} must return a character vector of length {length(selection)}.")
  }
  if (!length(selection)) return(.data)
  new_names <- old_names
  new_names[unname(selection)] <- renamed
  if (identical(new_names, old_names)) return(.data)
  pa_rename_dimensions(.data, new_names)
}

#' Apply dimension names and update dependent metadata together
#' @param .data A `poparray`.
#' @param new_names Complete dimension names in the original axis order.
#' @return A lazy renamed `poparray`.
#' @noRd
pa_rename_dimensions <- function(.data, new_names) {
  dn <- dimnames(.data)
  old_names <- names(dn)
  if (anyNA(new_names) || any(!nzchar(trimws(new_names)))) {
    cli::cli_abort("Dimension names must be non-missing and non-empty.")
  }
  if (anyDuplicated(new_names)) {
    cli::cli_abort("Renaming must leave unique dimension names.")
  }
  lookup <- stats::setNames(new_names, old_names)
  semantics <- dim_semantics(.data) |>
    purrr::imap(function(sem, name) {
      app <- pa_dim_applicability(sem)
      if (!is.null(app)) app$by <- unname(lookup[app$by])
      pa_update_dim_semantics(sem, dim_name = unname(lookup[name]),
                              applicability = app)
    })
  names(semantics) <- new_names
  names(dn) <- new_names
  dx <- methods::as(.data, "DelayedArray")
  dimnames(dx) <- dn
  new_poparray(dx, dimnames_list = dn, data_col = data_col(.data),
               source = get_source(.data),
               time_dim = unname(lookup[time_role(.data)]),
               area_dim = unname(lookup[area_role(.data)]),
               dim_semantics = semantics,
               group_vars = unname(lookup[.data@group_vars]))
}
