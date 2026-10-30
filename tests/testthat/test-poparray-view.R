make_view_fixture <- function(root = tempfile("view-root-")) {
  dir.create(file.path(root, "base"), recursive = TRUE)
  dn <- list(year = c("2020", "2021"), area.name = c("A", "B"),
    age.char = c("0-4", "5-9", "10-14", "15+"), sex = c("female", "male"))
  x <- as.poparray(array(as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)), dimnames = dn), filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(x)
  sem$sex <- new_dim_semantics("sex", "sex", "nominal", partition_type = "partition", validated = TRUE)
  dim_semantics(x) <- sem
  save_poparray(x, file.path(root, "base", "view_source.h5"), series_id = "view_source")
  rebuild_poparray_registry(root)
  root
}

local_view_fixture <- function() {
  root <- make_view_fixture()
  withr::local_options(list(tarr.pop.cube_path = root), .local_envir = parent.frame())
  root
}

test_that("S7 definitions and saved recipes contain plain data only", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "demo", created_by = "Analyst")
  expect_true(S7::S7_inherits(view, poparray_view))
  expect_false(methods::is(view, "poparray"))
  counties <- "A"
  view <- view_filter(view, area.name %in% counties, year >= 2020)
  counties <- "B"
  expect_identical(view@operations[[1]]$args$predicates[[1]]$rhs, "A")
  path <- tempfile(fileext = ".rds")
  save_poparray_view(view, path)
  recipe <- readRDS(path)
  expect_true(pa_view_plain(recipe))
  expect_type(recipe, "list")
  expect_equal(pa_view_recipe(read_poparray_view(path)), recipe)
  expect_identical(recipe$metadata$created_by, "Analyst")
  expect_identical(recipe$metadata$created_at, view@metadata$created_at)
  expect_identical(recipe$metadata$modified_by, Sys.info()[["user"]])
  expect_error(save_poparray_view(view, path), "already exists")
  expect_error(view@format_version <- 2L, "format_version")
  expect_error(view@source_series_id <- 1L, "character")
  expect_error(view_step(view, "filter", predicates = list(), .strict = TRUE, env = globalenv()), "plain")
  expect_error(view_step(view, "unknown"), "op|element|choice")
  expect_error(view_step(view, "select", dimensions = function() NULL), "plain")
  recipe$operations <- list(list(op = "system", args = list()))
  saveRDS(recipe, path)
  expect_error(read_poparray_view(path), "op|element|choice")
})

test_that("replay matches manual filtering grouping selection and metadata", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "demo") |>
    view_filter(area.name == "A", !(year < 2020)) |>
    view_step("group_ages", age_groups = c("0-9", "10+")) |>
    view_step("select", dimensions = c("year", county = "area.name", "age.char"))
  x <- open_poparray("view_source")
  manual <- x |>
    dplyr::filter(area.name == "A", !(year < 2020)) |>
    group_ages(c("0-9", "10+")) |>
    dplyr::select(year, county = area.name, age.char)
  stamp <- view@metadata$modified_at
  out <- open_poparray_view(view)
  expect_equal(as.array(out), as.array(manual))
  expect_identical(dimnames(out), dimnames(manual))
  expect_equal(dim_semantics(out), dim_semantics(manual))
  expect_identical(time_role(out), time_role(manual))
  expect_identical(area_role(out), area_role(manual))
  expect_identical(get_source(out), get_source(manual))
  expect_identical(data_col(out), data_col(manual))
  expect_identical(view@metadata$modified_at, stamp)
  expect_true(is_hdf5_backed_delayed(out))
  expect_true(methods::validObject(out))
  checkout <- open_poparray_view(view, checkout = TRUE, checkout_dir = tempfile("view-checkout-"))
  expect_equal(as.array(checkout), as.array(manual))
})

test_that("subset rename relocate and explicit collapse use existing semantics", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "demo") |>
    view_step("subset", indices = list(area.name = "B")) |>
    view_step("collapse_dim", dim = "sex", groups = list(Total = c("female", "male")), drop = TRUE) |>
    view_step("rename", mapping = c(age = "age.char")) |>
    view_step("relocate", dimensions = c("age", "year", "area.name"))
  x <- open_poparray("view_source")
  manual <- x[, "B", , , drop = FALSE] |>
    collapse_dim("sex", list(Total = c("female", "male")), drop = TRUE) |>
    dplyr::rename(age = age.char) |>
    dplyr::relocate(age, year, area.name)
  expect_equal(as.array(open_poparray_view(view)), as.array(manual))
  expect_error(open_poparray_view(view_step(view, "subset", indices = list(age = "missing"))), "operation 5|Unknown")
  text <- new_poparray_view("view_source", "text") |>
    view_filter(startsWith(area.name, "A") | endsWith(area.name, "B"))
  expect_equal(as.array(open_poparray_view(text)), as.array(x))
})

test_that("changed labels and relevant semantics fail but source notes may change", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "demo")
  source <- open_poparray("view_source")
  changed <- source["2020", , , , drop = FALSE]
  testthat::local_mocked_bindings(open_poparray = function(...) changed, .package = "tarr.pop")
  expect_error(open_poparray_view(view), "incompatible")
  changed <- source
  sem <- dim_semantics(changed)
  sem$sex <- new_dim_semantics("sex", "sex", "nominal", partition_type = "set", validated = TRUE)
  dim_semantics(changed) <- sem
  expect_error(open_poparray_view(view), "incompatible")
  changed <- source
  sem <- dim_semantics(changed)
  sem$sex <- pa_update_dim_semantics(sem$sex, notes = "New description")
  dim_semantics(changed) <- sem
  expect_s4_class(open_poparray_view(view), "poparray")
})

test_that("overlap and applicability guards remain active during replay", {
  local_view_fixture()
  source <- open_poparray("view_source")
  sem <- dim_semantics(source)
  sem$sex <- new_dim_semantics("sex", "sex", "nominal", partition_type = "set",
    overlap_levels = "male", validated = TRUE)
  dim_semantics(source) <- sem
  testthat::local_mocked_bindings(open_poparray = function(...) source, .package = "tarr.pop")
  view <- new_poparray_view("view_source", "unsafe") |>
    view_step("collapse_all", dim = "sex")
  expect_error(open_poparray_view(view), "Unsafe collapse")
  view2 <- new_poparray_view("view_source", "unsafe-select") |>
    view_step("select", dimensions = c("year", "area.name", "age.char"))
  expect_error(open_poparray_view(view2), "Unsafe collapse")
})

test_that("recording saving and lazy replay do not extract population values", {
  local_view_fixture()
  testthat::local_mocked_bindings(collapse_dim_poparray_impl = function(...) stop("Unexpected aggregation"), .package = "tarr.pop")
  tracker <- new.env(parent = emptyenv()); tracker$cells <- numeric()
  invisible(suppressMessages(trace("extract_array", signature = "HDF5ArraySeed", exit = substitute({
    assign("cells", c(tracker$cells, length(returnValue())), envir = tracker)
  }, list(tracker = tracker)), where = asNamespace("HDF5Array"), print = FALSE)))
  withr::defer(invisible(suppressMessages(untrace("extract_array", signature = "HDF5ArraySeed", where = asNamespace("HDF5Array")))))
  view <- new_poparray_view("view_source", "lazy") |>
    view_filter(area.name == "A") |>
    view_step("select", dimensions = c("year", "area.name", "age.char", "sex"))
  path <- tempfile(fileext = ".rds")
  save_poparray_view(view, path)
  out <- open_poparray_view(path)
  expect_true(is_hdf5_backed_delayed(out))
  expect_true(all(tracker$cells == 0))
})

test_that("view aggregation reads bounded blocks", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "blocks") |>
    view_step("collapse_all", dim = "sex")
  withr::local_options(list(poparray.collapse_block_bytes = 4 * 8))
  tracker <- new.env(parent = emptyenv()); tracker$cells <- numeric()
  invisible(suppressMessages(trace("extract_array", signature = "HDF5ArraySeed", exit = substitute({
    assign("cells", c(tracker$cells, length(returnValue())), envir = tracker)
  }, list(tracker = tracker)), where = asNamespace("HDF5Array"), print = FALSE)))
  withr::defer(invisible(suppressMessages(untrace("extract_array", signature = "HDF5ArraySeed", where = asNamespace("HDF5Array")))))
  out <- open_poparray_view(view)
  reads <- tracker$cells[tracker$cells > 0]
  expect_gt(length(reads), 1)
  expect_lte(max(reads), 4)
})

test_that("plain recipes reopen across sessions without temporary aggregation files", {
  root <- local_view_fixture()
  view <- new_poparray_view("view_source", "sessions") |>
    view_step("group_ages", age_groups = c("0-9", "10+")) |>
    view_step("select", dimensions = c("year", "area.name", "age.char"))
  path <- save_poparray_view(view)
  expected <- as.array(open_poparray_view(view))
  # A fresh child computes a view; its temporary HDF5 files disappear on exit.
  script <- tempfile(fileext = ".R")
  package_path <- getNamespaceInfo(asNamespace("tarr.pop"), "path")
  load_package <- if (file.exists(file.path(package_path, "Meta", "package.rds"))) {
    sprintf("library(tarr.pop, lib.loc=%s)", deparse(dirname(package_path)))
  } else {
    sprintf("pkgload::load_all(%s, quiet=TRUE)", deparse(package_path))
  }
  writeLines(c("Sys.setenv(TARR_POP_SKIP_CUBE_SETUP='true')",
    paste0(".libPaths(", paste(deparse(.libPaths()), collapse = ""), ")"),
    load_package,
    sprintf("options(tarr.pop.cube_path=%s)", deparse(root)),
    "v <- read_poparray_view(view_id = 'sessions')",
    "out <- open_poparray_view(view_id = 'sessions')",
    sprintf("saveRDS(as.array(out), %s)", deparse(file.path(root, "values.rds"))),
    sprintf(paste0("saveRDS(unlist(DelayedArray::seedApply(methods::as(out, 'DelayedArray'), ",
      "HDF5Array::path), use.names=FALSE), %s)"), deparse(file.path(root, "session-paths.rds"))),
    "save_poparray_view(v, overwrite=TRUE)"), script)
  log <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = TRUE, stderr = TRUE)
  expect_null(attr(log, "status"), info = paste(log, collapse = "\n"))
  session_paths <- readRDS(file.path(root, "session-paths.rds"))
  expect_gt(length(session_paths), 0L)
  expect_false(any(file.exists(session_paths)))
  expect_equal(readRDS(file.path(root, "values.rds")), expected)
  expect_equal(as.array(open_poparray_view(path)), expected)
  expect_equal(as.array(open_poparray_view(view_id = "sessions")), expected)
  expect_true(pa_view_plain(readRDS(path)))
})

test_that("applicability and source errors are validated during replay", {
  local_view_fixture()
  source <- open_poparray("view_source")
  sem <- dim_semantics(source)
  sem$sex <- pa_update_dim_semantics(sem$sex, applicability = list(by = "year", schemas = list(
    list(from = NULL, through = "2020", levels = "female"),
    list(from = "2021", through = NULL, levels = c("female", "male")))))
  dim_semantics(source) <- sem
  testthat::local_mocked_bindings(open_poparray = function(...) source, .package = "tarr.pop")
  view <- new_poparray_view("view_source", "applicability") |>
    view_filter(year == 2020) |>
    view_step("collapse_all", dim = "sex")
  manual <- source |> dplyr::filter(year == 2020) |> collapse_all("sex")
  out <- open_poparray_view(view)
  expect_equal(as.array(out), as.array(manual))
  expect_equal(dim_semantics(out), dim_semantics(manual))
  changed <- pa_view_recipe(view)
  changed$source_contract$semantics$sex["applicability"] <- list(NULL)
  incompatible <- do.call(poparray_view, changed)
  expect_error(open_poparray_view(incompatible), "incompatible")
})

test_that("missing sources and malformed recipe data fail clearly", {
  root <- local_view_fixture()
  view <- new_poparray_view("view_source", "missing")
  expect_error(new_poparray_view("not_registered", "missing"), "Unknown.*series_id")
  unlink(file.path(root, "base", "view_source.h5"))
  expect_error(open_poparray_view(view))
  bad <- pa_view_recipe(view)
  bad$format_version <- 99L
  path <- tempfile(fileext = ".rds"); saveRDS(bad, path)
  expect_error(read_poparray_view(path), "format_version")
  bad$format_version <- 1L
  bad$metadata$notes <- globalenv(); saveRDS(bad, path)
  expect_error(read_poparray_view(path), "plain")
  expect_error(view_step(view, "collapse_dim", dim = "sex", groups = list(Total = 1:32)), "character")
  expect_error(view_step(view, "subset", indices = list(area.name = 1:2)), "character")
  bad <- pa_view_recipe(view)
  bad$operations <- list(list(op = "filter", args = list(predicates = list(
    list(type = "atom", dim = "year", op = "system", rhs = "command")), .strict = TRUE)))
  saveRDS(bad, path)
  expect_error(read_poparray_view(path), "op|element|choice")
})

expect_view_matches <- function(actual, expected) {
  expect_equal(as.array(actual), as.array(expected))
  expect_identical(dimnames(actual), dimnames(expected))
  expect_equal(dim_semantics(actual), dim_semantics(expected))
  expect_identical(roles(actual), roles(expected))
  expect_identical(data_col(actual), data_col(expected))
  expect_identical(get_source(actual), get_source(expected))
  expect_identical(actual@group_vars, expected@group_vars)
  expect_true(methods::validObject(actual))
}

view_test_seed_paths <- function(x) {
  unique(unlist(DelayedArray::seedApply(methods::as(x, "DelayedArray"),
    HDF5Array::path), use.names = FALSE))
}

test_that("strict source policy currently rejects appended years", {
  root <- local_view_fixture()
  view <- new_poparray_view("view_source", "fixed-years") |> view_filter(year == 2020)
  source <- open_poparray("view_source")
  dn <- dimnames(source); dn$year <- c(dn$year, "2022")
  values <- array(0, unname(lengths(dn)), dn)
  values[1:2,,,] <- as.array(source)
  extended <- new_poparray(HDF5Array::writeHDF5Array(values), dimnames_list = dn,
    dim_semantics = dim_semantics(source), source = get_source(source),
    data_col = data_col(source), time_dim = time_role(source), area_dim = area_role(source))
  save_poparray(extended, file.path(root, "base", "view_source.h5"),
    series_id = "view_source", overwrite = TRUE)
  rebuild_poparray_registry(root)
  expect_identical(dimnames(open_poparray("view_source"))$year, c("2020", "2021", "2022"))
  expect_error(open_poparray_view(view), "incompatible")
})

test_that("age convenience builder records exactly the existing operation", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "ages")
  via_helper <- view_group_ages(view, c("0-9", "10+"))
  via_step <- view_step(view, "group_ages", age_groups = c("0-9", "10+"))
  expect_identical(via_helper@operations, via_step@operations)
  expect_identical(view@operations, list())
  expect_view_matches(open_poparray_view(via_helper), open_poparray_view(via_step))
  expect_error(view_group_ages(view, 1:5), "character")
})

test_that("renaming and relocating before grouping preserve metadata at each step", {
  local_view_fixture()
  source <- open_poparray("view_source")
  # Add applicability so rename must also update its controller references.
  sem <- dim_semantics(source)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, applicability = list(
    by = "year", schemas = list(list(from = NULL, through = NULL,
      levels = dimnames(source)$age.char))))
  dim_semantics(source) <- sem
  testthat::local_mocked_bindings(open_poparray = function(...) source, .package = "tarr.pop")
  view <- new_poparray_view("view_source", "combined")
  manual <- source
  view <- view_filter(view, area.name == "A")
  manual <- dplyr::filter(manual, area.name == "A")
  expect_view_matches(open_poparray_view(view), manual)
  view <- view_step(view, "rename", mapping = c(age = "age.char", period = "year", county = "area.name"))
  manual <- dplyr::rename(manual, age = age.char, period = year, county = area.name)
  expect_view_matches(open_poparray_view(view), manual)
  view <- view_step(view, "relocate", dimensions = c("age", "county", "sex", "period"))
  manual <- dplyr::relocate(manual, age, county, sex, period)
  expect_view_matches(open_poparray_view(view), manual)
  view <- view_group_ages(view, c("0-9", "10+"), dimension_name = "age")
  manual <- group_ages(manual, c("0-9", "10+"), dimension_name = "age")
  expect_view_matches(open_poparray_view(view), manual)
  expect_identical(dim_semantics(manual)$age@applicability$by, "period")
  view <- view_step(view, "select", dimensions = c("period", "county", "age"))
  manual <- dplyr::select(manual, period, county, age)
  expect_view_matches(open_poparray_view(view), manual)
  # Applicability controller aggregation remains explicitly prohibited.
  expect_error(open_poparray_view(view_step(view, "collapse_all", dim = "period")), "applicability controller")
  expect_error(open_poparray_view(view_step(view, "select", dimensions = c("county", "age"))), "applicability controller")
})

test_that("overlapping race groups are rejected but valid disjoint groups work", {
  local_view_fixture()
  dn <- list(year = c("2020", "2021"), area.name = c("A", "B"),
    race = c("White", "Black", "All races"))
  values <- array(rep(c(10, 20, 30), each = 4), c(2, 2, 3), dn)
  source <- as.poparray(values, filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(source)
  sem$race <- new_dim_semantics("race", "race", "nominal", partition_type = "set",
    overlap_levels = "All races", validated = TRUE)
  dim_semantics(source) <- sem
  testthat::local_mocked_bindings(open_poparray = function(...) source, .package = "tarr.pop")
  view <- new_poparray_view("view_source", "race")
  expect_error(open_poparray_view(view_step(view, "collapse_all", dim = "race")), "Unsafe collapse")
  expect_error(open_poparray_view(view_step(view, "select", dimensions = c("year", "area.name"))), "Unsafe collapse")
  valid <- view_filter(view, race %in% c("White", "Black")) |>
    view_step("collapse_all", dim = "race")
  manual <- source |> dplyr::filter(race %in% c("White", "Black")) |> collapse_all("race")
  expect_view_matches(open_poparray_view(valid), manual)
  expect_true(all(as.array(open_poparray_view(valid)) == 30))
})

test_that("temporary HDF5 results survive GC and multiple independent openings", {
  root <- local_view_fixture()
  definition <- new_poparray_view("view_source", "lifetime") |>
    view_group_ages(c("0-9", "10+")) |>
    view_step("select", dimensions = c("year", "area.name", "age.char"))
  first <- open_poparray_view(view_filter(definition, area.name == "A"))
  first_values <- as.array(first)
  first_paths <- view_test_seed_paths(first)
  second <- open_poparray_view(view_filter(definition, area.name == "B"))
  second_values <- as.array(second)
  second_paths <- view_test_seed_paths(second)
  third <- open_poparray_view(definition)
  third_values <- as.array(third)
  third_paths <- view_test_seed_paths(third)
  expect_length(intersect(first_paths, second_paths), 0)
  expect_length(intersect(first_paths, third_paths), 0)
  expect_length(intersect(second_paths, third_paths), 0)
  expect_true(all(startsWith(c(first_paths, second_paths, third_paths), paste0(tempdir(), "/"))))
  invisible(gc())
  expect_true(all(file.exists(c(first_paths, second_paths, third_paths))))
  expect_identical(as.array(first), first_values)
  expect_identical(as.array(second), second_values)
  expect_identical(as.array(third), third_values)
  rm(second)
  invisible(gc())
  expect_true(all(file.exists(c(first_paths, third_paths))))
  expect_identical(as.array(first), first_values)
  expect_identical(as.array(third), third_values)
  # Neither another opening nor a failed replay may damage live results.
  expect_error(open_poparray_view(view_step(definition, "collapse_all", dim = "missing")), "Unknown|unknown")
  fourth <- open_poparray_view(definition)
  expect_identical(as.array(fourth), third_values)
  expect_identical(as.array(first), first_values)
  expect_identical(as.array(third), third_values)
  expect_identical(list.files(file.path(root, "base")), "view_source.h5")
})

test_that("default storage registers views and opens them by ID", {
  root <- local_view_fixture()
  view <- new_poparray_view("view_source", "by_name", description = "County A") |>
    view_filter(area.name == "A") |> view_group_ages(c("0-9", "10+"))
  path <- save_poparray_view(view)
  expect_identical(path, file.path(root, "views", "by_name.rds"))
  expect_true(file.exists(file.path(root, "cache", "view_registry.rds")))
  registry <- list_poparray_views()
  expect_identical(registry$view_id, "by_name")
  expect_identical(registry$source_series_id, "view_source")
  expect_identical(registry$filepath, file.path("views", "by_name.rds"))
  expect_identical(registry$description, "County A")
  expect_equal(readRDS(file.path(root, "cache", "view_registry.rds")), registry)
  expect_identical(read_poparray_view(view_id = "by_name")@operations, view@operations)
  expected <- open_poparray_view(view)
  expect_view_matches(open_poparray_view("by_name"), expected)
  expect_view_matches(open_poparray_view(view_id = "by_name"), expected)
  expect_view_matches(open_poparray_view(filepath = path), expected)
  stamp <- read_poparray_view(view_id = "by_name")@metadata$modified_at
  invisible(open_poparray_view(view_id = "by_name"))
  expect_identical(read_poparray_view(view_id = "by_name")@metadata$modified_at, stamp)
  expect_error(save_poparray_view(view), "already exists")
  expect_error(open_poparray_view(view, view_id = "by_name"), "exactly one")
  expect_error(open_poparray_view(view_id = "missing"), "Unknown view_id")
  expect_error(read_poparray_view(filepath = "missing.rds"))
  unsafe_id <- new_poparray_view("view_source", "../outside")
  expect_error(save_poparray_view(unsafe_id), "pattern")
})

test_that("registry rebuilds from definitions and detects duplicate or corrupt files", {
  root <- local_view_fixture()
  expect_equal(nrow(list_poparray_views()), 0L)
  view <- new_poparray_view("view_source", "registry")
  path <- save_poparray_view(view)
  cache <- file.path(root, "cache", "view_registry.rds")
  saveRDS("corrupt cache", cache)
  expect_identical(list_poparray_views()$view_id, "registry")
  expect_identical(readRDS(cache)$view_id, "registry")
  # A definition updated independently of the cache remains authoritative.
  recipe <- readRDS(path); recipe$metadata$description <- "Edited externally"
  saveRDS(recipe, path)
  expect_identical(list_poparray_views()$description, "Edited externally")
  copied <- file.path(root, "views", "duplicate.rds")
  expect_true(file.copy(path, copied))
  expect_error(list_poparray_views(), "Duplicate view IDs")
  unlink(copied)
  saveRDS(list(invalid = TRUE), copied)
  expect_error(list_poparray_views(), "Invalid view definition")
  unlink(copied)
  unlink(path)
  expect_equal(nrow(list_poparray_views()), 0L)
  expect_error(read_poparray_view(view_id = "registry"), "Unknown view_id")
})

test_that("metadata editing preserves creation provenance and does not replay", {
  local_view_fixture()
  original <- new_poparray_view("view_source", "edit", created_by = "Creator") |>
    view_group_ages(c("0-9", "10+"))
  save_poparray_view(original)
  testthat::local_mocked_bindings(open_poparray = function(...) stop("Must not open population data"),
    pa_replay_view_step = function(...) stop("Must not replay"), .package = "tarr.pop")
  edited <- update_poparray_view_metadata("edit", description = "New description",
    notes = "Reviewed", name = "Display name", modified_by = "Editor")
  expect_identical(edited@view_id, original@view_id)
  expect_identical(edited@source_series_id, original@source_series_id)
  expect_identical(edited@source_contract, original@source_contract)
  expect_identical(edited@operations, original@operations)
  expect_identical(edited@metadata$created_by, original@metadata$created_by)
  expect_identical(edited@metadata$created_at, original@metadata$created_at)
  expect_identical(edited@metadata$modified_by, "Editor")
  expect_identical(original@metadata$notes, "")
  expect_identical(read_poparray_view(view_id = "edit")@metadata$description, "")
  save_poparray_view(edited, overwrite = TRUE)
  restored <- read_poparray_view(view_id = "edit")
  expect_identical(restored@metadata$created_by, "Creator")
  expect_identical(restored@metadata$created_at, original@metadata$created_at)
  expect_identical(restored@metadata$modified_by, "Editor")
  expect_identical(restored@metadata$name, "Display name")
  expect_identical(restored@metadata$notes, "Reviewed")
  expect_identical(list_poparray_views()$description, "New description")
  expect_identical(update_poparray_view_metadata(restored, notes = "")@metadata$notes, "")
  expect_error(update_poparray_view_metadata(restored, notes = list("bad")), "string|character")
})

test_that("printing is ordered and never opens or executes population operations", {
  local_view_fixture()
  view <- new_poparray_view("view_source", "print") |>
    view_filter(area.name == "A") |> view_group_ages(c("0-9", "10+")) |>
    view_step("select", dimensions = c("year", "area.name", "age.char"))
  testthat::local_mocked_bindings(open_poparray = function(...) stop("Must not open"),
    pa_replay_view_step = function(...) stop("Must not replay"), .package = "tarr.pop")
  printed <- capture.output(result <- withVisible(print(view)))
  expect_match(printed[[1]], "poparray_view.*print")
  expect_true(any(grepl("Source: view_source", printed, fixed = TRUE)))
  steps <- printed[grepl("^  [123]\\.", printed)]
  expect_length(steps, 3L)
  expect_match(steps[[1]], "1. filter", fixed = TRUE)
  expect_match(steps[[2]], "2. group_ages", fixed = TRUE)
  expect_match(steps[[3]], "3. select", fixed = TRUE)
  expect_false(result$visible)
  expect_identical(result$value, view)
})

test_that("failed staged save preserves the previous recipe and cache", {
  root <- local_view_fixture()
  original <- new_poparray_view("view_source", "atomic", description = "Original")
  path <- save_poparray_view(original)
  before <- readRDS(path)
  cache_path <- file.path(root, "cache", "view_registry.rds")
  before_cache <- readRDS(cache_path)
  edited <- update_poparray_view_metadata(original, description = "Replacement")
  testthat::local_mocked_bindings(pa_install_view_file = function(from, to) {
    if (identical(to, path)) FALSE else base::file.rename(from, to)
  }, .package = "tarr.pop")
  expect_error(save_poparray_view(edited, overwrite = TRUE), "Could not install")
  expect_identical(readRDS(path), before)
  expect_identical(readRDS(cache_path), before_cache)
  expect_identical(list.files(file.path(root, "views"), all.files = TRUE, no.. = TRUE), "atomic.rds")
})

test_that("registry paths remain relative after moving definitions", {
  root <- local_view_fixture()
  path <- save_poparray_view(new_poparray_view("view_source", "portable"))
  moved <- tempfile("moved-views-")
  dir.create(file.path(moved, "views"), recursive = TRUE)
  expect_true(file.copy(path, file.path(moved, "views", basename(path))))
  registry <- list_poparray_views(moved)
  expect_identical(registry$filepath, file.path("views", "portable.rds"))
  expect_true(file.exists(file.path(moved, registry$filepath)))
  withr::local_options(list(tarr.pop.cube_path = moved))
  expect_identical(read_poparray_view(view_id = "portable")@source_series_id, "view_source")
})
