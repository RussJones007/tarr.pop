make_overlap_drop_fixture <- function() {
  dn <- list(year = c("2020", "2021"), area.name = "A",
    race = c("White", "Black", "White in combination"),
    age.char = c("0-4", "5-9", "0-9"))
  x <- as.poparray(array(1, dim = unname(lengths(dn)), dimnames = dn),
    filepath = tempfile(fileext = ".h5"))
  sem <- dim_semantics(x)
  sem$race <- pa_update_dim_semantics(sem$race,
    overlap_levels = "White in combination", validated = TRUE)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, overlap_levels = "0-9")
  dim_semantics(x) <- sem
  x
}

test_that("declared overlap removal is lazy and preserves semantic contracts", {
  x <- make_overlap_drop_fixture()
  expect_error(sum(x), "Unsafe reduction")
  out <- drop_overlap_levels(x, c("race", "age.char"))
  expect_equal(dimnames(out)$race, c("White", "Black"))
  expect_equal(dimnames(out)$age.char, c("0-4", "5-9"))
  expect_equal(dim(out), c(2L, 1L, 2L, 2L))
  expect_true(is_hdf5_backed_delayed(out))
  expect_equal(dim_semantics(out), dim_semantics(x))
  expect_equal(get_source(out), get_source(x))
  expect_equal(time_role(out), time_role(x))
  expect_equal(area_role(out), area_role(x))
  expect_equal(sum(out), 8)
  expect_equal(dim(drop_overlap_levels(x, names(dimnames(x)))), dim(out))
  expect_identical(drop_overlap_levels(out, "race"), out)
  expect_equal(length(dimnames(x)$race), 3L)
})

test_that("overlap removal validates selectors and retains unresolved guards", {
  x <- make_overlap_drop_fixture()
  expect_error(drop_overlap_levels(x, "missing"))
  expect_error(drop_overlap_levels(x, character()))
  expect_error(drop_overlap_levels(x, c("race", "race")))
  expect_error(drop_overlap_levels(x, NA_character_))
  expect_error(drop_overlap_levels(x, 3))
  expect_error(drop_overlap_levels(list(), "race"), "poparray")
  sem <- dim_semantics(x)
  sem$race <- pa_update_dim_semantics(sem$race, overlap_levels = dimnames(x)$race)
  dim_semantics(x) <- sem
  expect_error(drop_overlap_levels(x, "race"), "empty dimension")
  sem$race <- pa_update_dim_semantics(sem$race, overlap_levels = character())
  dim_semantics(x) <- sem
  expect_error(sum(drop_overlap_levels(x, names(dimnames(x)))), "Unsafe reduction")
})

test_that("applicability compression merges only consecutive equal sets", {
  dn <- list(year = c("late", "early", "middle", "last"), race = c("A", "B", "C"))
  schemas <- list(
    list(from = NULL, through = "late", levels = c("A", "B")),
    list(from = "early", through = "early", levels = c("B", "A")),
    list(from = "middle", through = "middle", levels = "C"),
    list(from = "last", through = NULL, levels = c("A", "B")))
  sem <- new_dim_semantics("race", "race", "nominal", "set", TRUE,
    notes = "canonical", applicability = list(by = "year", schemas = schemas[c(4, 2, 3, 1)]))
  out <- pa_compress_applicability(sem, dn)
  expect_length(out@applicability$schemas, 3L)
  expect_null(out@applicability$schemas[[1]]$from)
  expect_equal(out@applicability$schemas[[1]]$through, "early")
  expect_null(out@applicability$schemas[[3]]$through)
  expect_equal(out@notes, sem@notes)
  expect_true(out@validated)
  levels_by_label <- function(s) {
    app <- s@applicability
    indices <- pa_applicability_indices(app, "race", dn)
    lapply(seq_along(dn$year), function(i) sort(app$schemas[[which(vapply(indices,
      function(idx) i %in% idx, logical(1)))]]$levels))
  }
  expect_equal(levels_by_label(out), levels_by_label(sem))
  expect_equal(pa_compress_applicability(out, dn), out)
  expect_identical(pa_compress_applicability(new_dim_semantics("race", "race", "nominal"), dn)@applicability, NULL)
  bad <- pa_update_dim_semantics(sem, applicability = list(by = "year", schemas = schemas[-2]))
  expect_error(pa_compress_applicability(bad, dn), "uncovered")
})

test_that("overlap removal trims applicability and compressed metadata persists", {
  x <- make_overlap_drop_fixture()
  sem <- dim_semantics(x)
  sem$race <- pa_update_dim_semantics(sem$race, applicability = list(by = "year",
    schemas = lapply(dimnames(x)$year, function(yr) list(from = yr, through = yr,
      levels = dimnames(x)$race))))
  sem$race <- pa_compress_applicability(sem$race, dimnames(x))
  expect_length(sem$race@applicability$schemas, 1L)
  expect_equal(sem$race@applicability$schemas[[1]]$from, "2020")
  expect_equal(sem$race@applicability$schemas[[1]]$through, "2021")
  dim_semantics(x) <- sem
  out <- drop_overlap_levels(x, "race")
  expect_equal(out@dim_semantics$race@applicability$schemas[[1]]$levels, c("White", "Black"))
  expect_equal(out@dim_semantics$race@overlap_levels, "White in combination")
  path <- tempfile(fileext = ".h5")
  on.exit(unlink(path))
  save_poparray(out, filepath = path, series_id = "compressed-fixture")
  expect_equal(dim_semantics(path), dim_semantics(out))
})
