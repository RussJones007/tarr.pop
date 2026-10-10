load_census_vintage_functions <- function() {
  script <- test_path("..", "..", "data-raw", "census_update_estimates.r")
  builder <- test_path("..", "..", "data-raw", "census_data_2.r")
  skip_if_not(file.exists(script) && file.exists(builder), "Source build scripts are excluded from installed packages")
  env <- new.env(parent = asNamespace("tarr.pop"))
  sys.source(script, env)
  helpers <- env$load_census_update_helpers(builder)
  list(functions = env, helpers = helpers)
}

make_census_vintage_fixture <- function(h, root) {
  support <- h$census_estimate_support(2010:2024)
  support <- support[as.character(support$area.name) == "Tarrant", , drop = FALSE]
  df <- as.data.frame(support)
  df$population <- seq_len(nrow(df))
  old_path <- h$ingest_census_table(df, support,
    h$census_dimension_semantics(support, "July 1"), "census_estimates_county_5y", root,
    list(note = "original fixture", population_type = "Estimate", source = "Census fixture"))
  new_support <- h$census_estimate_support(2020:2025)
  new_support <- new_support[as.character(new_support$area.name) == "Tarrant", , drop = FALSE]
  revised <- as.data.frame(new_support)
  revised$population <- seq_len(nrow(revised)) + 50000
  list(path = old_path, revised = revised, original = df)
}

test_that("vintage replacement preserves history, replaces the decade, and appends the new year", {
  h <- load_census_vintage_functions()
  root <- tempfile("vintage-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  fx <- make_census_vintage_fixture(h$helpers, root)
  old <- pa_resolve_cube_update_target(fx$path)$object
  candidate <- file.path(root, "staging", "candidate.h5")
  shuffled <- fx$revised[rev(seq_len(nrow(fx$revised))), ]
  expect_silent(h$functions$revise_census_estimate_cube(fx$path, shuffled, candidate,
    helpers = h$helpers))
  out <- pa_resolve_cube_update_target(candidate)$object
  expect_equal(dimnames(out)$year, as.character(2010:2025))
  expect_equal(as.array(out[as.character(2010:2019), , , , , , drop = FALSE]),
    as.array(old[as.character(2010:2019), , , , , , drop = FALSE]))
  expected <- df_2_array(fx$revised, data_col = "population")
  idx <- lapply(names(dimnames(out)), function(nm) if (nm == "year") as.character(2020:2025) else dimnames(out)[[nm]])
  expected <- do.call(`[`, c(list(expected), idx, list(drop = FALSE)))
  expect_equal(as.array(out[as.character(2020:2025), , , , , , drop = FALSE]), expected)
  expect_equal(dimnames(out)[-1], dimnames(old)[-1])
  expect_equal(dim_semantics(out)$race@overlap_levels, dim_semantics(old)$race@overlap_levels)
  expect_length(dim_semantics(out)$race@applicability$schemas, 1L)
  expect_equal(dim_semantics(out)$race@applicability$schemas[[1]]$through, "2025")
  expect_true(is_hdf5_backed_delayed(out))
  expect_error(sum(out), "Unsafe reduction")
  expect_match(paste(get_source(out)$note, collapse = " "), "Vintage 2025")
  expect_match(paste(get_source(out)$note, collapse = " "), "original fixture")
  expect_match(get_source(out)$source, "cc-est2025-alldata-48.csv")
  unchanged <- pa_resolve_cube_update_target(fx$path)$object
  expect_equal(as.array(unchanged), as.array(old))
  expect_error(h$functions$revise_census_estimate_cube(fx$path, fx$revised, candidate,
    helpers = h$helpers), "already exists")
  old_values <- as.array(old)
  rm(old, out, unchanged)
  backup <- file.path(root, "backups", "original.h5")
  expect_silent(h$functions$install_census_vintage_candidate(candidate, fx$path, backup, root))
  expect_true(file.exists(backup))
  expect_false(file.exists(candidate))
  expect_equal(as.array(pa_resolve_cube_update_target(backup)$object), old_values)
  expect_equal(dimnames(pa_resolve_cube_update_target(fx$path)$object)$year, as.character(2010:2025))
})

test_that("vintage replacement rejects incomplete or incompatible source data", {
  h <- load_census_vintage_functions()
  root <- tempfile("vintage-invalid-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  fx <- make_census_vintage_fixture(h$helpers, root)
  revise <- function(df, vintage = 2025L) h$functions$revise_census_estimate_cube(
    fx$path, df, file.path(root, "candidate.h5"), vintage = vintage, helpers = h$helpers)
  expect_error(revise(fx$revised[fx$revised$year != 2020, ]), "every July")
  expect_error(revise(fx$revised[-1, ]), "Missing population")
  expect_error(revise(rbind(fx$revised, fx$revised[1, ])), "Duplicate")
  bad <- fx$revised; bad$population[1] <- -1
  expect_error(revise(bad), "invalid counts")
  bad <- fx$revised; bad$race <- as.character(bad$race); bad$race[1] <- "new category"
  expect_error(revise(bad), "schema differs")
  expect_error(revise(fx$revised, vintage = 2023L), "not exceed")
  expect_false(file.exists(file.path(root, "candidate.h5")))
})

test_that("source-file staging uses a cached copy and does not overwrite existing files", {
  h <- load_census_vintage_functions()
  root <- tempfile("vintage-source-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  cached <- file.path(root, "cached"); dir.create(cached)
  filename <- "cc-est2025-alldata-48.csv"
  writeLines("cached fixture", file.path(cached, filename))
  target <- h$functions$prepare_census_vintage_file(file.path(root, "sources"),
    cached_dir = cached, download_missing = FALSE)
  expect_equal(readLines(target), "cached fixture")
  writeLines("existing fixture", target)
  expect_equal(readLines(h$functions$prepare_census_vintage_file(dirname(target),
    cached_dir = cached, download_missing = FALSE)), "existing fixture")
  expect_error(h$functions$prepare_census_vintage_file(file.path(root, "missing"),
    download_missing = FALSE), "Missing current")
})
