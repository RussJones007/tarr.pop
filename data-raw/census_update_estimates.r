# Census annual-estimate vintage replacement example
#
# Unlike a simple annual append, a Census vintage revises ALL July estimates
# from its base year onward. This example preserves 2010-2019 exactly, replaces
# 2020 through the incoming vintage, and retains the original non-time axes.
# It does not update decennial or ACS cubes.
#
# Source this file after data-raw/control_def.r. Sourcing defines functions only:
# it does not download files, write cubes, or replace production data.
# See data-raw/census_update_estimates.md for a complete runnable workflow.
#
# Source CSV parsing and incoming-table ingestion are EAGER. Existing historical
# values remain HDF5-backed and are copied in blocks; the entire old cube is
# never converted to a data frame or an in-memory array. The writer below is an
# internal tarr.pop helper, so this example should run against this checkout.

#' Load Census build helpers without triggering their automatic builds
#' @param script Path to the existing Census builder, relative to package root.
#' @return Private environment containing reader, transformer and support helpers.
load_census_update_helpers <- function(script = "data-raw/census_data_2.r") {
  checkmate::assert_file_exists(script)
  previous <- options(tarr.pop.census_build = FALSE)
  on.exit(options(previous))
  env <- new.env(parent = asNamespace("tarr.pop"))
  sys.source(script, envir = env)
  env
}

#' Obtain only the incoming vintage CSV in the normal source directory
#' @param input_dir Census source directory.
#' @param vintage End year of the 2020-based annual vintage.
#' @param cached_dir Optional previously downloaded-file directory.
#' @param download_missing Download from Census when neither copy is available.
#' @return Path to the current-vintage CSV; existing files are not overwritten.
#' @details File presence is not proof of its vintage: the reader and coverage
#'   checks below also require the expected YEAR codes. Future Census layouts
#'   must be checked before using this example for a new vintage.
prepare_census_vintage_file <- function(input_dir, vintage = 2025L,
    cached_dir = NULL, download_missing = TRUE) {
  checkmate::assert_integerish(vintage, lower = 2020, upper = 2029, len = 1, any.missing = FALSE)
  input_dir <- path.expand(input_dir)
  filename <- sprintf("cc-est%d-alldata-48.csv", vintage)
  target <- file.path(input_dir, filename)
  if (file.exists(target)) return(normalizePath(target))
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(tmpdir = input_dir)
  on.exit(unlink(temporary))
  cached <- if (is.null(cached_dir)) "" else file.path(path.expand(cached_dir), filename)
  if (nzchar(cached) && file.exists(cached)) {
    if (!file.copy(cached, temporary)) cli::cli_abort("Could not copy the cached source CSV.")
  } else {
    if (!isTRUE(download_missing)) cli::cli_abort("Missing current-vintage CSV {.file {target}}.")
    url <- sprintf(paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
      "2020-%d/counties/asrh/%s"), vintage, filename)
    utils::download.file(url, temporary, mode = "wb", quiet = TRUE)
  }
  if (!file.rename(temporary, target)) cli::cli_abort("Could not install source CSV {.file {target}}.")
  normalizePath(target)
}

#' Preserve historical applicability and add the incoming source period
#' @param existing Existing poparray with the complete historical year axis.
#' @param historical Lazy subset containing 2010-2019 only.
#' @param incoming Incoming poparray containing 2020 through the vintage.
#' @param combined_dimnames Named labels of the output cube.
#' @return Semantic entries preserving intrinsic fields and expanding applicability.
#' @details Historical open upper endpoints must be bounded before the incoming
#'   period is added. Identical adjacent schemas are then compressed. Domains,
#'   partition declarations, overlap labels, notes and validated flags are kept.
census_vintage_semantics <- function(existing, historical, incoming, combined_dimnames) {
  sem <- tarr.pop::dim_semantics(existing)
  hist_dn <- dimnames(historical)
  incoming_dn <- dimnames(incoming)
  result <- lapply(names(sem), function(nm) {
    entry <- sem[[nm]]
    app <- tarr.pop:::pa_dim_applicability(entry)
    if (is.null(app) || app$by != "year") return(entry)
    hist_app <- tarr.pop:::pa_dim_applicability(tarr.pop::dim_semantics(historical)[[nm]])
    indices <- tarr.pop:::pa_applicability_indices(hist_app, nm, hist_dn)
    schemas <- lapply(seq_along(indices), function(i) {
      idx <- indices[[i]]
      list(from = if (is.null(hist_app$schemas[[i]]$from)) NULL else hist_dn$year[min(idx)],
        through = hist_dn$year[max(idx)], levels = hist_app$schemas[[i]]$levels)
    })
    schemas <- c(schemas, list(list(from = incoming_dn$year[1L],
      through = tail(incoming_dn$year, 1L), levels = incoming_dn[[nm]])))
    tarr.pop:::pa_compress_applicability(tarr.pop:::pa_update_dim_semantics(entry,
      applicability = list(by = "year", schemas = schemas)), combined_dimnames)
  })
  names(result) <- names(sem)
  result
}

#' Replace the current decade in a Census estimate cube into a new candidate file
#' @param cube Existing canonical HDF5 path or registered series identifier.
#' @param df Canonical incoming long table from transform_census_estimates().
#' @param output_filepath New candidate path; must not already exist.
#' @param vintage Last incoming July estimate year.
#' @param source_file Source CSV name/path for provenance.
#' @param helpers Environment returned by load_census_update_helpers().
#' @return Invisible candidate path. No existing cube is overwritten.
#' @details Requires the current six-axis county estimate schema and consecutive
#'   years beginning in 2010. Category changes, missing expected source keys,
#'   or an older incoming vintage fail before a candidate is installed. Existing
#'   area selections are retained; changing demographic axes requires a rebuild.
revise_census_estimate_cube <- function(cube, df, output_filepath, vintage = 2025L,
    source_file = sprintf("cc-est%d-alldata-48.csv", vintage),
    helpers = load_census_update_helpers()) {
  checkmate::assert_integerish(vintage, lower = 2020, upper = 2029, len = 1, any.missing = FALSE)
  target <- tarr.pop:::pa_resolve_cube_update_target(cube)
  existing <- target$object
  dn <- dimnames(existing)
  dims <- c("year", "area.name", "sex", "age.char", "race", "ethnicity")
  if (!identical(names(dn), dims) || tarr.pop::time_role(existing) != "year" ||
      tarr.pop::area_role(existing) != "area.name" || tarr.pop::data_col(existing) != "population") {
    cli::cli_abort("Expected the canonical six-dimension Census county estimates cube.")
  }
  old_years <- suppressWarnings(as.integer(dn$year))
  if (anyNA(old_years) || !identical(old_years, seq.int(2010L, max(old_years))) ||
      max(old_years) < 2019L || max(old_years) > vintage) {
    cli::cli_abort("Existing years must be consecutive from 2010, include 2019, and not exceed the incoming vintage.")
  }
  checkmate::assert_string(output_filepath, min.chars = 1L)
  output_filepath <- path.expand(output_filepath)
  if (file.exists(output_filepath)) cli::cli_abort("Candidate already exists: {.file {output_filepath}}.")
  df <- helpers$canonical_census_table(df)
  if (!setequal(unique(df$year), seq.int(2020L, vintage))) {
    cli::cli_abort("Incoming source must contain every July estimate year from 2020 through the vintage, and no other years.")
  }
  df <- df[as.character(df$area.name) %in% dn$area.name, , drop = FALSE]
  # Ingestion derives the year axis from row order for integer columns. Ensure
  # it follows calendar order even when an incoming file/table is shuffled.
  df <- df[order(df$year), , drop = FALSE]
  mismatch <- setdiff(dims, "year")[!vapply(setdiff(dims, "year"), function(nm)
    setequal(unique(as.character(df[[nm]])), dn[[nm]]), logical(1))]
  if (length(mismatch)) cli::cli_abort("Incoming category schema differs in {.val {mismatch}}; rebuild or explicitly harmonize the cube.")
  support <- helpers$census_estimate_support(seq.int(2020L, vintage))
  support <- support[as.character(support$area.name) %in% dn$area.name, , drop = FALSE]
  dir.create(dirname(output_filepath), recursive = TRUE, showWarnings = FALSE)
  stage_root <- tempfile("census-vintage-", tmpdir = dirname(output_filepath))
  dir.create(stage_root)
  on.exit(unlink(stage_root, recursive = TRUE))
  source <- utils::modifyList(as.list(tarr.pop::get_source(existing)), list(
    source = paste("U.S. Census Bureau county characteristics estimates;",
      "retained 2010-2019 history; 2020 onward", basename(source_file)),
    population_type = "Estimate", updated = as.character(Sys.Date())))
  source$note <- paste(source$note, sprintf(
    "Vintage %d replaces every July estimate for 2020-%d; 2010-2019 values retained unchanged.", vintage, vintage))
  replacement_path <- helpers$ingest_census_table(df, support,
    helpers$census_dimension_semantics(support, "July 1 annual population estimates."),
    "replacement", stage_root, source)
  incoming <- tarr.pop:::pa_resolve_cube_update_target(replacement_path)$object
  historical <- existing[as.character(2010:2019), , , , , , drop = FALSE]
  # Align by LABELS, not positional assumptions about CSV/factor ordering.
  incoming_indices <- lapply(dims, function(nm) if (nm == "year")
    as.character(seq.int(2020L, vintage)) else dn[[nm]])
  incoming <- do.call(`[`, c(list(incoming), incoming_indices, list(drop = FALSE)))
  combined_dn <- dn
  combined_dn$year <- as.character(seq.int(2010L, vintage))
  semantics <- census_vintage_semantics(existing, historical, incoming, combined_dn)
  tarr.pop:::validate_dim_semantics(semantics, dims, "year", "area.name", combined_dn)
  staged <- file.path(stage_root, "candidate.h5")
  field <- function(nm) tarr.pop:::h5_read_scalar_chr_if_present(target$path,
    paste0("cube/metadata/", nm), info = target$meta$info)
  # Reuse the same blockwise writer as add_population_data, after explicitly
  # discarding the years that are being replaced. This is a full new HDF5 file,
  # not an in-place patch; the existing HDF5 file remains readable throughout.
  tarr.pop:::pa_write_poparray_cube_append(
    old_x = methods::as(historical, "DelayedArray"),
    new_x = methods::as(incoming, "DelayedArray"), add_dim = "year",
    filepath = staged, dimnames_list = combined_dn, dim_semantics = semantics,
    time_dim = "year", area_dim = "area.name", source = source,
    data_col = "population", series_id = field("series_id"), geo = field("geo"),
    extendable_year = field("extendable_year"))
  reopened <- tarr.pop:::pa_resolve_cube_update_target(staged)$object
  methods::validObject(reopened)
  if (!identical(dimnames(reopened), combined_dn) ||
      !isTRUE(all.equal(tarr.pop::dim_semantics(reopened), semantics))) {
    cli::cli_abort("Candidate metadata failed round-trip validation.")
  }
  if (!file.rename(staged, output_filepath)) cli::cli_abort("Could not save the validated candidate.")
  tarr.pop:::reset_poparray_cache()
  invisible(normalizePath(output_filepath))
}

#' Build a replacement candidate from the normal Census source folder
#' @param cube_root Cube root directory.
#' @param input_dir Usual downloaded Census source directory.
#' @param vintage Incoming 2020-based vintage; verify future file layouts first.
#' @param cube Existing path or series ID; defaults to the base estimate file.
#' @param output_filepath Candidate path, outside base/ to avoid registry ambiguity.
#' @param download_missing Whether a missing source can be downloaded.
#' @return Invisible candidate path, leaving the base cube unchanged.
update_census_estimate_vintage <- function(cube_root,
    input_dir = file.path(tarr::paths$population, "Estimates", "Census"),
    vintage = 2025L,
    cube = file.path(cube_root, "base", "census_estimates_county_5y.h5"),
    output_filepath = file.path(cube_root, "staging", sprintf("census_estimates_vintage_%d.h5", vintage)),
    download_missing = TRUE) {
  helpers <- load_census_update_helpers()
  file <- prepare_census_vintage_file(input_dir, vintage,
    cached_dir = file.path(cube_root, "source-data", "census"),
    download_missing = download_missing)
  df <- helpers$transform_census_estimates(helpers$read_census_estimate_file(file, "current", vintage))
  revise_census_estimate_cube(cube, df, output_filepath, vintage, file, helpers)
}

#' Install a reviewed candidate while retaining a recoverable original
#' @param candidate Validated candidate path returned by the update function.
#' @param cube_path Existing base cube path.
#' @param backup_path New backup path outside base/; never overwritten.
#' @param cube_root Root whose registry should be refreshed after installation.
#' @return Invisible installed path. Candidate is moved, original remains at backup.
#' @details Run only after reviewing the candidate. Close objects opened from the
#'   original cube before installation. Renames should be on one filesystem. If
#'   candidate installation fails, restoring the original is attempted; a failed
#'   restoration reports the backup location. This is not an atomic transaction
#'   across both renames. A registry failure leaves the installed cube and backup
#'   intact; retry rebuild_poparray_registry().
install_census_vintage_candidate <- function(candidate, cube_path, backup_path, cube_root) {
  checkmate::assert_file_exists(candidate)
  checkmate::assert_file_exists(cube_path)
  if (identical(normalizePath(candidate), normalizePath(cube_path))) {
    cli::cli_abort("Candidate and original must be different files.")
  }
  if (file.exists(backup_path)) cli::cli_abort("Backup already exists; choose a new path.")
  old <- tarr.pop:::pa_resolve_cube_update_target(cube_path)$object
  new <- tarr.pop:::pa_resolve_cube_update_target(candidate)$object
  if (!identical(names(dimnames(old)), names(dimnames(new))) ||
      !identical(dimnames(old)[-1L], dimnames(new)[-1L]) ||
      !identical(tarr.pop::time_role(old), tarr.pop::time_role(new)) ||
      !identical(tarr.pop::area_role(old), tarr.pop::area_role(new)) ||
      !identical(tarr.pop::data_col(old), tarr.pop::data_col(new))) {
    cli::cli_abort("Candidate dimensions, roles or value column do not match the original.")
  }
  old_years <- as.integer(dimnames(old)$year)
  new_years <- as.integer(dimnames(new)$year)
  if (anyNA(new_years) || !identical(new_years, seq.int(2010L, max(new_years))) ||
      !all(old_years %in% new_years)) cli::cli_abort("Candidate does not preserve historical year coverage.")
  rm(old, new)
  tarr.pop:::reset_poparray_cache()
  dir.create(dirname(backup_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(cube_path, backup_path)) cli::cli_abort("Could not move the original to its backup.")
  if (!file.rename(candidate, cube_path)) {
    restored <- file.rename(backup_path, cube_path)
    if (!restored) cli::cli_abort("Installation and rollback failed; original is at {.file {backup_path}}.")
    cli::cli_abort("Installation failed; original restored.")
  }
  tarr.pop:::reset_poparray_cache()
  tarr.pop::rebuild_poparray_registry(cube_root)
  invisible(normalizePath(cube_path))
}
