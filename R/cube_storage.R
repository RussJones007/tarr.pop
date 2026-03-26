# -------------------------------------------------------------------------------------->
# Script: cube_storage.r
# Description:
#   Persistent storage helpers for HDF5-backed population cubes.
# -------------------------------------------------------------------------------------->

tarr_pop_config_dir <- function() {
  tools::R_user_dir("tarr.pop", which = "config")
}

tarr_pop_data_dir <- function() {
  tools::R_user_dir("tarr.pop", which = "data")
}

tarr_pop_config_file <- function() {
  file.path(tarr_pop_config_dir(), "cube_path.yml")
}

tarr_pop_default_cube_path <- function() {
  file.path(tarr_pop_data_dir(), "cubes")
}

read_cube_path_config <- function(path = tarr_pop_config_file()) {
  if (!file.exists(path)) {
    return(NULL)
  }

  cfg <- yaml::read_yaml(path)
  if (!is.list(cfg) || is.null(cfg$cube_path) || !nzchar(cfg$cube_path)) {
    return(NULL)
  }

  as.character(cfg$cube_path)
}

write_cube_path_config <- function(path, config_file = tarr_pop_config_file()) {
  dir.create(dirname(config_file), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(cube_path = path), config_file)
  invisible(normalizePath(config_file, winslash = "/", mustWork = TRUE))
}

prompt_for_cube_path <- function(default = tarr_pop_default_cube_path()) {
  if (!interactive()) {
    return(NULL)
  }

  ans <- readline(
    sprintf(
      "Enter a directory for tarr.pop cubes [%s]: ",
      default
    )
  )
  ans <- trimws(ans)
  if (!nzchar(ans)) default else ans
}

#' Return the active cube storage root
#'
#' Reads the configured cube path from, in order:
#' 1. `getOption("tarr.pop.cube_path")`
#' 2. a YAML config file in the user config directory
#' 3. a package-managed default under `tools::R_user_dir()`
#'
#' In interactive sessions, when no configuration exists yet, the user is
#' prompted for a path and the choice is persisted. In non-interactive sessions,
#' the default is used unless `legacy_ok = TRUE` and package `inst/extdata`
#' exists, in which case that directory is returned as a compatibility fallback.
#'
#' @param create Logical; create the resolved directory if needed.
#' @param legacy_ok Logical; allow fallback to bundled `inst/extdata`.
#'
#' @return Absolute path to the active cube directory.
#' @export
cube_path <- function(create = FALSE, legacy_ok = TRUE) {
  opt <- getOption("tarr.pop.cube_path")
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    path <- opt
  } else {
    path <- read_cube_path_config()
  }

  if (is.null(path) || !nzchar(path)) {
    path <- prompt_for_cube_path()
    if (is.null(path) || !nzchar(path)) {
      if (isTRUE(legacy_ok)) {
        legacy <- resolve_extdata_dir(strict = FALSE)
        if (!is.null(legacy)) {
          return(legacy)
        }
      }
      path <- tarr_pop_default_cube_path()
    }
    write_cube_path_config(path)
  }

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Persist the active cube storage root
#'
#' @param path Root directory that will contain `base/` and `derived/` cubes.
#' @param create Logical; create the directory if needed.
#' @param persist Logical; write the path to the user YAML config file.
#'
#' @return Invisibly returns the normalized path.
#' @export
set_cube_path <- function(path, create = TRUE, persist = TRUE) {
  checkmate::assert_string(path, min.chars = 1)

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  options(tarr.pop.cube_path = path)

  if (isTRUE(persist)) {
    write_cube_path_config(path)
  }

  invisible(path)
}

#' Initialize the cube storage directory structure
#'
#' @param path Optional storage root. Defaults to [cube_path()].
#'
#' @return Invisibly returns the normalized root path.
#' @export
init_cubes <- function(path = cube_path()) {
  root <- set_cube_path(path, create = TRUE, persist = TRUE)

  dirs <- file.path(
    root,
    c("base", "derived", "derived/projections", "derived/filtered", "derived/custom")
  )
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))

  invisible(root)
}

#' Build a cube only when it does not already exist
#'
#' @param name Series identifier or file stem.
#' @param builder_fun Function called as `builder_fun(target_dir, filepath)`.
#' @param subdir Relative subdirectory under [cube_path()].
#' @param ext File extension, defaults to `".h5"`.
#'
#' @return Invisibly returns the cube filepath.
#' @export
build_cube_if_missing <- function(name,
                                  builder_fun,
                                  subdir = "base",
                                  ext = ".h5") {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_function(builder_fun)
  checkmate::assert_string(subdir, min.chars = 1)
  checkmate::assert_string(ext, min.chars = 1)

  root <- init_cubes()
  target_dir <- file.path(root, subdir)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  filepath <- file.path(target_dir, paste0(name, ext))

  if (!file.exists(filepath)) {
    builder_fun(target_dir, filepath)
  }

  invisible(normalizePath(filepath, winslash = "/", mustWork = FALSE))
}

#' Resolve the active cube directory
#'
#' @param strict Logical; error when the path does not exist.
#' @param legacy_ok Logical; allow fallback to bundled `inst/extdata`.
#'
#' @return Absolute path to a cube directory.
#' @keywords internal
resolve_cube_dir <- function(strict = TRUE, legacy_ok = TRUE) {
  path <- cube_path(create = FALSE, legacy_ok = legacy_ok)

  if (!dir.exists(path)) {
    if (isTRUE(strict)) {
      stop("Cube directory does not exist: ", path)
    }
    return(path)
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}
