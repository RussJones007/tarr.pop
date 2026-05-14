#' tarr.pop-package.R
#'
#' @description
#' 
#' The package provides a flexible framework for storing, managing, and analyzing population data using multidimensional
#' arrays. It is designed to support demographic and epidemiologic workflows where population counts are organized
#' across dimensions like time, geography, age, sex, race, ethnicity, or other descriptors like income groups, home
#' values, household size, et cetera.
#'
#' The package enables users to work with population data from multiple sources in a consistent structure, preserving
#' important semantic information about each dimension (e.g., whether categories are mutually exclusive or overlapping).
#' This allows for safe aggregation, filtering, and transformation of population data while maintaining reproducibility
#' and interpretability.
#'
#' Population data are stored using  [DelayedArray] and [HDF5Array] disk-backed arrays, allowing users to work with
#' large data sets that exceed available memory. Operations such as filtering, sub-setting, and projection are performed
#' lazily, minimizing memory usage and improving performance by avoiding unnecessary data realization.
#'
#' The package also provides tools for extending population cubes, including time-series projection methods and a
#' standardized HDF5-based storage format with embedded metadata. Refer to the [population] help topic for available
#' example data sets and their formats.
#'
#'
#' @author Russ Jones <RussJones007@gmail.com>
#' @references Population figure sources are:
#' * Decennial censuses and Estimates [U.S. Census Bureau](https://www.census.gov/)
#' * [Texas Demographic Center Estimates program](https://demographics.texas.gov/Data/TPEPP/Estimates/)
#' * [Texas Demographic Center Projections program](https://demographics.texas.gov/Data/TPEPP/Projections/)
#' * [National Cancer Institute SEER Program](https://seer.cancer.gov/data-software/uspopulations.html)
#' @keywords package
#' @name tarr.pop
"_PACKAGE"

tarr_pop_skip_cube_setup <- function() {
  flag <- tolower(Sys.getenv("TARR_POP_SKIP_CUBE_SETUP", unset = ""))
  flag %in% c("1", "true", "yes")
}

tarr_pop_startup_setup <- function(interactive_session = interactive()) {
  if (tarr_pop_skip_cube_setup()) {
    return(invisible(FALSE))
  }

  path <- configured_cube_path()
  if (is.null(path) || !nzchar(path)) {
    if (!isTRUE(interactive_session)) {
      cli::cli_abort(c(
        "Cube folder is not configured.",
        "i" = "Load {.pkg tarr.pop} interactively for initial cube setup."
      ))
    }
    path <- prompt_for_cube_path()
  }

  init_cubes(path)
  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  tarr_pop_startup_setup(interactive_session = interactive())
}
