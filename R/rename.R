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
               dim_semantics = semantics)
}
