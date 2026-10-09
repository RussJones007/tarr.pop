# Census Bureau population cube builds. Run after data-raw/control_def.r.
# Source tables are EAGER; ingest_population writes HDF5-backed poparrays.
# Set options(tarr.pop.census_build = FALSE) to load helpers without building.
# Original census_data.r is retained for reference.
# Usage: source("data-raw/control_def.r"); source("data-raw/census_data_2.r")
# County census uses the original parquet when present; pass cache_file = NULL
# to build_census_decennial() to force a fresh API read. Estimate CSVs default
# to <cube_root>/source-data/census; input_dir may instead point to the original
# <population>/Estimates/Census directory. Missing CSVs are downloaded there.
# Full source tables AND ingestion's dense array construction are EAGER.
# HDF5-backed access after ingestion is lazy. ACS MOEs must not be summed.
# Potential shared utilities: census_county_names(), census_age_factor(),
# census_support_table(), census_dimension_semantics(), ingest_census_table().

#' Resolve Texas county identifiers using the package reference
#' @param codes Five-character county FIPS identifiers.
#' @return Canonical county names; unknown identifiers fail.
census_county_names <- function(codes) {
  ref <- tarr.pop::county_fips
  index <- match(as.character(codes), as.character(ref))
  if (anyNA(index)) cli::cli_abort("Unknown county FIPS: {.val {unique(codes[is.na(index)])}}.")
  names(ref)[index]
}

#' Normalize and order interval-valued ages
#' @param x Age labels, excluding aggregate labels.
#' @return Ordered factor using rage interval ordering.
census_age_factor <- function(x) {
  labels <- unique(as.character(x))
  ages <- rage::as.age_group(labels)
  ordered(as.character(ages)[match(as.character(x), labels)],
    levels = as.character(sort(unique(ages))))
}

#' Validate and canonicalize county population records
#' @param df Long data frame with year, area.name, sex, age.char, race,
#'   ethnicity and population columns.
#' @return Canonical data frame without total rows, with ordered ages.
canonical_census_table <- function(df) {
  dims <- c("year", "area.name", "sex", "age.char", "race", "ethnicity")
  checkmate::assert_subset(c(dims, "population"), names(df))
  df <- as.data.frame(df)
  if ("fips" %in% names(df)) df$area.name <- census_county_names(df$fips)
  df <- df[as.character(df$area.name) != "Texas", c(dims, "population")]
  keep <- !Reduce(`|`, lapply(df[dims], function(x) as.character(x) == "All"))
  df <- df[keep, , drop = FALSE]
  df$year <- as.integer(df$year)
  df$population <- as.numeric(df$population)
  if (!nrow(df) || anyNA(df[dims]) || any(!is.finite(df$population)) || any(df$population < 0)) {
    cli::cli_abort("County data contain missing labels, invalid counts, or no detail records.")
  }
  if (anyDuplicated(df[dims])) cli::cli_abort("Duplicate county demographic keys in source data.")
  df$age.char <- census_age_factor(df$age.char)
  factor_dims <- setdiff(dims, c("year", "age.char"))
  df[factor_dims] <- lapply(df[factor_dims], function(x) factor(as.character(x), levels = sort(unique(as.character(x)))))
  df
}

#' Construct valid source support without inventing cross-year categories
#' @param df Canonical source data.
#' @param counties Expected county names (all Texas counties by default).
#' @return Expected keys: each year's reported strata crossed with counties.
#' @details Missing county observations fail during ingestion, rather than
#'   becoming zeros. Categories not reported in a year are not fabricated.
census_support_table <- function(df, counties = setdiff(names(tarr.pop::county_fips), "Texas")) {
  strata <- unique(df[c("year", "sex", "age.char", "race", "ethnicity")])
  tidyr::crossing(strata, area.name = factor(counties, levels = sort(unique(counties)))) |>
    dplyr::select(year, area.name, sex, age.char, race, ethnicity)
}

#' Declare source-specific dimensions and time-varying applicability
#' @param support Valid source support.
#' @param time_note Description of the observation date.
#' @return Named list of DimSemantics objects.
census_dimension_semantics <- function(support, time_note) {
  dims <- names(support)
  result <- lapply(dims, function(dim) {
    scale <- if (dim %in% c("year", "age.char")) "interval" else "nominal"
    overlap <- if (dim == "race") grep("in combination", unique(as.character(support$race)), value = TRUE) else character()
    tarr.pop:::new_dim_semantics(dim, dim, scale,
      partition_type = if (length(overlap)) "set" else "partition",
      validated = TRUE, overlap_levels = overlap,
      notes = if (dim == "year") time_note else if (dim == "race" && length(overlap))
        "Race alone-or-in-combination categories overlap each other and race-alone categories." else character())
  })
  names(result) <- dims
  years <- sort(unique(support$year))
  for (dim in intersect(c("age.char", "race"), dims)) {
    schemas <- lapply(years, function(yr) list(from = as.character(yr), through = as.character(yr),
      levels = unique(as.character(support[[dim]][support$year == yr]))))
    result[[dim]] <- tarr.pop:::pa_update_dim_semantics(result[[dim]], applicability = list(by = "year", schemas = schemas))
  }
  result
}

#' Ingest a prepared table through the standard population contract
#' @param df Canonical records.
#' @param support Expected keys.
#' @param semantics Named dimension semantics.
#' @param series_id Registry series identifier and file stem.
#' @param cube_root Cube storage root.
#' @param source_meta Provenance fields.
#' @param data_col Numeric value column.
#' @param area_dim Geographic dimension.
#' @param completion_policy Missing-key policy.
#' @return Invisible path returned by ingest_population.
ingest_census_table <- function(df, support, semantics, series_id, cube_root,
                                source_meta, data_col = "population", area_dim = "area.name",
                                completion_policy = "error") {
  tarr.pop::ingest_population(reader = function(...) df,
    transformer = function(df, ...) df, dims = names(support),
    dim_semantics = semantics, support = support,
    filepath = file.path(cube_root, "base", paste0(series_id, ".h5")),
    series_id = series_id, time_dim = "year", area_dim = area_dim,
    completion_policy = completion_policy, drop_all = TRUE,
    source_meta = source_meta, data_col = data_col, overwrite = TRUE)
}

# 1. Decennial census: 2000, 2010, 2020 -------------------------------------

#' Retrieve one census year's single-year age tables
#' @param year Census year: 2000, 2010 or 2020.
#' @return Raw county observations joined to Census variable labels/concepts.
read_decennial_year <- function(year) {
  checkmate::assert_choice(year, c(2000, 2010, 2020))
  dataset <- if (year == 2020) "dhc" else "sf1"
  vars <- tidycensus::load_variables(year, dataset)
  vars <- vars[grepl("^PCT0?12[A-O]?(_|[0-9])", vars$name), ]
  if (!nrow(vars)) cli::cli_abort("No single-year age variables found for {year}.")
  tidycensus::get_decennial(geography = "county", state = "TX", year = year,
    sumfile = dataset, variables = vars$name, geometry = FALSE) |>
    dplyr::left_join(vars, by = c("variable" = "name")) |>
    dplyr::mutate(year = year)
}

#' Read an existing decennial parquet or retrieve Census API data
#' @param cache_file Original processed census parquet; NULL forces API reads.
#' @param years Census years requested.
#' @param ... Reserved for ingestion arguments.
#' @return Processed cached table or raw API observations.
read_census_decennial <- function(cache_file = file.path(tarr::paths$population,
    "Census", "Census_2000_thru_2020.parquet"), years = c(2000L, 2010L, 2020L), ...) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    df <- nanoparquet::read_parquet(cache_file)
    if (!all(years %in% df$year)) cli::cli_abort("Decennial cache does not contain all requested years.")
    return(df[df$year %in% years, ])
  }
  dplyr::bind_rows(lapply(years, read_decennial_year))
}

#' Decode decennial labels into detailed age/sex/race records
#' @param raw Raw tidycensus observations with labels and concepts.
#' @return Long detailed counts; Hispanic counts derived by subtraction.
#' @details Only male/female age-detail rows are retained. Race totals are used
#'   solely to subtract non-Hispanic counts; no All level is stored.
decode_decennial_records <- function(raw) {
  text <- tolower(raw$label)
  sex <- ifelse(grepl("!!male:?(!!|$)", text), "Male",
    ifelse(grepl("!!female:?(!!|$)", text), "Female", NA_character_))
  age <- trimws(sub(".*!!", "", text))
  age <- sub(":$", "", age)
  detail <- !is.na(sex) & grepl("year", age)
  raw <- raw[detail, ]; sex <- sex[detail]; age <- age[detail]
  if (!nrow(raw)) cli::cli_abort("No detailed age/sex observations found in decennial labels.")
  age <- gsub(" years?| years? old", "", age)
  age <- sub("^under 1$", "< 1", age)
  age <- sub(" and over$", " +", age)
  age <- gsub(" to | and ", "-", age)
  concept <- tolower(raw$concept)
  race <- dplyr::case_when(
    grepl("white", concept) ~ "White",
    grepl("black", concept) ~ "Black",
    grepl("american indian", concept) ~ "American Indian and Alaska Native",
    grepl("hawaiian", concept) ~ "Hawaiian or Pacific Islander",
    grepl("asian", concept) ~ "Asian",
    grepl("some other", concept) ~ "Other",
    grepl("two or more", concept) ~ "Two or more",
    TRUE ~ NA_character_)
  df <- data.frame(year = raw$year, area.name = census_county_names(raw$GEOID), sex,
    age.char = age, race, ethnicity = ifelse(grepl("not hispanic", concept), "Non-Hispanic", "Total"),
    population = raw$value)
  df <- df[!is.na(df$race), ]
  wide <- tidyr::pivot_wider(df, names_from = ethnicity, values_from = population)
  if (!all(c("Total", "Non-Hispanic") %in% names(wide)) || anyNA(wide[c("Total", "Non-Hispanic")]))
    cli::cli_abort("Incomplete race/ethnicity pairs in decennial tables.")
  wide$Hispanic <- wide$Total - wide$`Non-Hispanic`
  tidyr::pivot_longer(dplyr::select(wide, -Total), c("Hispanic", "Non-Hispanic"),
    names_to = "ethnicity", values_to = "population")
}

#' Transform cached or API census records
#' @param df Reader output.
#' @param ... Reserved arguments.
#' @return Canonical county demographic counts.
transform_census_decennial <- function(df, ...) {
  if (!"population" %in% names(df)) df <- decode_decennial_records(df)
  canonical_census_table(df)
}

#' Build the decennial county cube
#' @param cube_root Cube directory.
#' @param ... Arguments forwarded to the reader.
#' @return Cube path.
build_census_decennial <- function(cube_root, ...) {
  df <- transform_census_decennial(read_census_decennial(...))
  support <- census_support_table(df)
  ingest_census_table(df, support, census_dimension_semantics(support, "April 1 decennial census counts."),
    "census_decennial_county_1y", cube_root,
    list(note = "Decennial census 2000, 2010 and 2020; separate race and ethnicity.",
      population_type = "Census", source = "Census Bureau SF1 (2000/2010), DHC (2020)."))
}

# 2. Annual county estimates: final intercensal plus current vintage --------
# Layouts: www2.census.gov/programs-surveys/popest/technical-documentation/
# file-layouts/2010-2020/intercensal/county/CC-EST2020INT-ALLDATA.pdf
# file-layouts/2020-2025/CC-EST2025-ALLDATA.pdf

#' Identify and obtain the two estimate input files
#' @param input_dir Downloaded Census source directory.
#' @param vintage Latest 2020-based vintage.
#' @param download_missing Download missing files from Census Bureau.
#' @return Named file paths.
census_estimate_files <- function(input_dir, vintage = 2025L, download_missing = TRUE) {
  checkmate::assert_integerish(vintage, lower = 2020, upper = 2099, len = 1)
  stems <- c(intercensal = "cc-est2020int-alldata-48.csv",
    current = sprintf("cc-est%d-alldata-48.csv", vintage))
  files <- file.path(input_dir, stems); names(files) <- names(stems)
  urls <- c(intercensal = paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
    "2010-2020/intercensal/county/asrh/", stems[[1]]),
    current = paste0("https://www2.census.gov/programs-surveys/popest/datasets/2020-",
      vintage, "/counties/asrh/", stems[[2]]))
  if (download_missing) {
    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    invisible(lapply(which(!file.exists(files)), function(i) {
      temp <- tempfile(tmpdir = input_dir)
      on.exit(unlink(temp))
      utils::download.file(urls[[i]], temp, mode = "wb", quiet = TRUE)
      if (!file.rename(temp, files[[i]])) cli::cli_abort("Could not save {.file {files[[i]]}}.")
    }))
  }
  if (!all(file.exists(files))) cli::cli_abort("Missing Census estimate files: {.file {files[!file.exists(files)]}}.")
  files
}

#' Decode official YEAR codes for the selected file layout
#' @param codes Numeric YEAR codes.
#' @param series intercensal or current.
#' @param vintage Current vintage end year.
#' @return Calendar July 1 years; base/census rows become NA.
census_estimate_years <- function(codes, series, vintage = 2025L) {
  codes <- as.integer(codes)
  if (series == "intercensal") {
    if (any(!codes %in% 1:12)) cli::cli_abort("Unexpected final intercensal YEAR code.")
    return(ifelse(codes %in% 2:11, codes + 2008L, NA_integer_))
  }
  checkmate::assert_choice(series, "current")
  if (any(!codes %in% seq_len(vintage - 2018L))) cli::cli_abort("Unexpected current-vintage YEAR code.")
  ifelse(codes >= 2L, codes + 2018L, NA_integer_)
}

#' Read one county estimates CSV with explicit year decoding
#' @param file Source file.
#' @param series File layout name.
#' @param vintage Current vintage.
#' @return Wide county records containing July 1 observations only.
read_census_estimate_file <- function(file, series, vintage = 2025L) {
  df <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
    janitor::clean_names()
  df$year <- census_estimate_years(df$year, series, vintage)
  df <- df[!is.na(df$year) & df$sumlev == "050" & df$state == "48" & df$county != "000", ]
  df$area.name <- census_county_names(paste0(df$state, df$county))
  df
}

#' Read final 2010s estimates and the configurable 2020s vintage
#' @param input_dir Source directory.
#' @param vintage End year of current vintage.
#' @param download_missing Whether to retrieve missing CSVs.
#' @param ... Reserved arguments.
#' @return Wide county estimate records.
read_census_estimates <- function(input_dir, vintage = 2025L, download_missing = TRUE, ...) {
  files <- census_estimate_files(input_dir, vintage, download_missing)
  dplyr::bind_rows(lapply(names(files), function(series)
    read_census_estimate_file(files[[series]], series, vintage)))
}

#' Map published race codes to explicit category labels
#' @return Named labels, including overlapping alone-or-in-combination races.
census_estimate_races <- function() {
  alone <- c(wa = "White", ba = "Black", ia = "American Indian and Alaska Native",
    aa = "Asian", na = "Hawaiian or Pacific Islander")
  c(alone, tom = "Two or more", stats::setNames(paste(alone, "or in combination"), paste0(names(alone), "c")))
}

#' Transform wide estimates into separate race and ethnicity dimensions
#' @param df Wide county CSV records.
#' @param ... Reserved arguments.
#' @return Canonical long counts, retaining all detailed race categories.
transform_census_estimates <- function(df, ...) {
  races <- census_estimate_races()
  expected <- as.vector(outer(paste0(rep(c("nh", "h"), each = length(races)), rep(names(races), 2)),
    c("male", "female"), paste, sep = "_"))
  checkmate::assert_subset(expected, names(df))
  df <- df[as.integer(df$agegrp) %in% 1:18, ]
  long <- tidyr::pivot_longer(dplyr::select(df, year, area.name, agegrp, dplyr::all_of(expected)),
    dplyr::all_of(expected), names_to = "variable", values_to = "population")
  code <- sub("_(male|female)$", "", long$variable)
  long$ethnicity <- ifelse(startsWith(code, "nh"), "Non-Hispanic", "Hispanic")
  long$race <- unname(races[sub("^(nh|h)", "", code)])
  long$sex <- ifelse(endsWith(long$variable, "_male"), "Male", "Female")
  ages <- c(paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), "85 +")
  long$age.char <- ages[as.integer(long$agegrp)]
  canonical_census_table(long)
}

#' Build the annual county estimates cube
#' @param cube_root Cube directory.
#' @param input_dir Download directory; stored under cubes by default.
#' @param vintage Current estimate vintage (default 2025).
#' @param ... Reader arguments.
#' @return Cube path.
build_census_estimates <- function(cube_root, input_dir = file.path(cube_root, "source-data", "census"), vintage = 2025L, ...) {
  df <- transform_census_estimates(read_census_estimates(input_dir, vintage = vintage, ...))
  support <- census_estimate_support(seq.int(2010L, vintage))
  ingest_census_table(df, support, census_dimension_semantics(support, "July 1 annual population estimates."),
    "census_estimates_county_5y", cube_root,
    list(note = "2010–2019 final intercensal; 2020 onward current vintage; five-year age groups, not ACS periods.",
      population_type = "Estimate", source = paste("U.S. Census Bureau county characteristics estimates;",
        "cc-est2020int-alldata-48.csv and", sprintf("cc-est%d-alldata-48.csv", vintage))))
}

#' Define annual estimate support from the published schema
#' @param years July 1 calendar years.
#' @return Complete county/sex/age/race/ethnicity keys, independently of observed rows.
census_estimate_support <- function(years) {
  ages <- c(paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), "85 +")
  tidyr::expand_grid(year = sort(unique(as.integer(years))),
    area.name = sort(setdiff(names(tarr.pop::county_fips), "Texas")),
    sex = c("Female", "Male"), age.char = census_age_factor(ages),
    race = sort(unname(census_estimate_races())), ethnicity = c("Hispanic", "Non-Hispanic"))
}

# 3. ZCTA: ACS five-year population and companion margin-of-error cube ------

#' Preserve the original comprehensive Tarrant-area ZCTA selection
#' @param spatial_file Original spatial RData file, loaded into a private environment.
#' @return Unique five-character ZCTA identifiers.
tarrant_census_zctas <- function(spatial_file = file.path(tarr::paths$spatial,
    "Tarrant", "Zip Code Tabluation Areas", "Tarrant Zip Codes.rdata")) {
  if (!file.exists(spatial_file)) cli::cli_abort("Missing Tarrant ZCTA spatial file {.file {spatial_file}}.")
  env <- new.env(parent = baseenv()); load(spatial_file, envir = env)
  if (!exists("zips.tarrant", envir = env, inherits = FALSE)) cli::cli_abort("Spatial file must contain zips.tarrant.")
  spatial <- methods::slot(env$zips.tarrant, "data")$ZCTA5CE10
  michelle <- c(75019,75022,75028,75038,75050,75051,75052,75054,75061,75062,75063,75067,75104,75261,
    76001,76002,76005,76006,76008,76009,76010,76011,76012,76013,76014,76015,76016,76017,76018,76019,
    76020,76021,76022,76023,76028,76034,76036,76039,76040,76044,76051,76052,76053,76054,76060,76063,
    76065,76071,76084,76092,76102,76103,76104,76105,76106,76107,76108,76109,76110,76111,76112,76114,
    76115,76116,76117,76118,76119,76120,76122,76123,76126,76127,76129,76131,76132,76133,76134,76135,
    76137,76140,76148,76155,76164,76177,76179,76180,76182,76244,76247,76248,76262)
  additional <- c(75061,75062,75067,75261,76005,76009,76019,76023,76044,76084,76122,76247)
  sort(unique(c(as.character(spatial), as.character(michelle), as.character(additional))))
}

#' Discover published ACS five-year years from the Census API catalog
#' @param first_year First end year requested.
#' @return Consecutive end years through the latest published ACS5 release.
#' @details Catalog failures are errors, not silently mistaken for unavailable years.
census_acs_years <- function(first_year = 2011L) {
  catalog <- jsonlite::fromJSON("https://api.census.gov/data.json", simplifyVector = FALSE)$dataset
  years <- vapply(Filter(function(x) identical(unlist(x$c_dataset), c("acs", "acs5")), catalog),
    function(x) as.integer(x$c_vintage), integer(1))
  years <- sort(unique(years[years >= first_year]))
  if (!length(years) || !identical(years, seq.int(first_year, max(years))))
    cli::cli_abort("Census catalog does not provide a consecutive ACS5 series from {first_year}.")
  years
}

#' Read one ACS end year's total ZCTA population and MOE
#' @param year ACS five-year period end year.
#' @param zctas Requested ZCTA identifiers.
#' @return Raw requested observations; unavailable ZCTAs remain absent.
read_census_zcta_year <- function(year, zctas) {
  df <- tidycensus::get_acs(geography = "zcta", table = "B01003", year = year,
    survey = "acs5", geometry = FALSE)
  df$GEOID <- substr(df$GEOID, nchar(df$GEOID) - 4L, nchar(df$GEOID))
  df <- df[df$GEOID %in% zctas, ]
  df$year <- year
  df
}

#' Obtain ACS population and MOE observations for selected ZCTAs
#' @param years Published ACS end years.
#' @param zctas Comprehensive Tarrant ZCTA selection.
#' @param ... Reserved arguments.
#' @return Raw ACS data frame.
read_census_zcta <- function(years, zctas, ...) {
  dplyr::bind_rows(lapply(years, read_census_zcta_year, zctas = zctas))
}

#' Select a numeric ACS measure without dropping unavailable cells
#' @param df Raw ACS data.
#' @param measure estimate or moe.
#' @param ... Reserved arguments.
#' @return Long year/zip.code/value data frame.
transform_census_zcta <- function(df, measure = "estimate", ...) {
  checkmate::assert_choice(measure, c("estimate", "moe"))
  out <- data.frame(year = as.integer(df$year), zip.code = as.character(df$GEOID))
  out[[measure]] <- as.numeric(df[[measure]])
  # Census negative sentinel values are unavailable, not negative populations/MOEs.
  out[[measure]][out[[measure]] < 0] <- NA_real_
  if (anyDuplicated(out[c("year", "zip.code")])) cli::cli_abort("Duplicate ZCTA/year keys.")
  out
}

#' Construct ZCTA support including codes absent from particular releases
#' @param years ACS period end years.
#' @param zctas Requested identifiers.
#' @return Complete year/ZCTA keys; absent observations will be NA, not zero.
census_zcta_support <- function(years, zctas) {
  tidyr::expand_grid(year = sort(unique(as.integer(years))), zip.code = sort(unique(as.character(zctas))))
}

#' Define ACS period and ZCTA semantics
#' @return Named dimension semantics with ACS period notes.
#' @details Role dimensions must be partitions in the current class contract.
#'   Period overlap is recorded in notes but cannot activate reduction guards.
census_zcta_semantics <- function() {
  list(year = tarr.pop:::new_dim_semantics("year", "time", "ordinal", "partition", TRUE,
      notes = "Label is the end year of a five-year ACS period. Adjacent periods overlap; do not sum across years."),
    zip.code = tarr.pop:::new_dim_semantics("zip.code", "area", "nominal", "partition", TRUE,
      notes = "Census ZCTAs, not USPS ZIP delivery areas; boundaries may change between releases."))
}

#' Build matching ACS estimate and MOE cubes from one download
#' @param cube_root Cube directory.
#' @param years End years; NULL discovers latest published release.
#' @param zctas Selection; NULL reads original spatial selection plus fixed lists.
#' @return Invisible vector of estimate and MOE file paths.
#' @details MOE is the published 90% margin of error. Its values are not additive;
#'   aggregating uncertainty requires a statistical method, not sum(moe).
build_census_zcta <- function(cube_root, years = NULL, zctas = NULL) {
  if (is.null(years)) years <- census_acs_years()
  if (is.null(zctas)) zctas <- tarrant_census_zctas()
  raw <- read_census_zcta(years, zctas)
  support <- census_zcta_support(years, zctas)
  semantics <- census_zcta_semantics()
  paths <- lapply(c("estimate", "moe"), function(measure) {
    sem <- semantics
    if (measure == "moe") {
      # The area role must remain a partition. Notes document non-additivity;
      # the current class cannot enforce an MOE-specific reduction rule.
      sem$zip.code <- tarr.pop:::pa_update_dim_semantics(sem$zip.code,
        notes = c(sem$zip.code@notes, "MOEs are not additive; use an explicit uncertainty aggregation method."))
    }
    ingest_census_table(transform_census_zcta(raw, measure), support, sem,
      if (measure == "estimate") "census_zcta_estimates" else "census_zcta_estimates_moe",
      cube_root, list(note = paste("ACS five-year B01003", measure, "for Tarrant-area ZCTAs; missing codes retained as NA."),
        population_type = if (measure == "estimate") "Estimate" else "Margin of error",
        source = "U.S. Census Bureau ACS5 B01003; MOE confidence level 90%."),
      data_col = measure, area_dim = "zip.code", completion_policy = "na")
  })
  invisible(unlist(paths))
}

# Recreate all four cubes when sourced after control_def.r.
if (isTRUE(getOption("tarr.pop.census_build", TRUE))) {
  census_cube_root <- tarr.pop::init_cubes()
  build_census_decennial(census_cube_root)
  build_census_estimates(census_cube_root)
  build_census_zcta(census_cube_root)
  tarr.pop::rebuild_poparray_registry(census_cube_root)
}
