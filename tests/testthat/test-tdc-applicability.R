load_tdc_applicability_functions <- function() {
  paths <- test_path(paste0("../../data-raw/", c("tdc_estimates_support.r", "tdc_estimates.r")))
  if (!all(file.exists(paths))) skip("TDC metadata rules are available in the source repository only")
  env <- new.env(parent = asNamespace("tarr.pop"))
  for (path in paths) for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) eval(expr, env)
  }
  env
}

test_that("TDC ingestion attaches repository-established age and race transitions", {
  env <- load_tdc_applicability_functions()
  for (years in list(2016:2017, 2011:2016, 2017:2020)) {
    support <- env$tdc_estimate_support_table(years, "De Witt")
    sem <- env$tdc_estimate_semantics(support)
    expect_true(all(vapply(sem, function(entry) isTRUE(entry@validated), logical(1))))
    dn <- lapply(support[c("year", "area.name", "sex", "age.char", "race.eth")], function(x) unique(as.character(x)))
    expect_silent(pa_validate_applicability(sem, dn))
    expect_false(pa_dim_has_overlap_risk(sem$age.char, dn$age.char, dn))
    age <- sem$age.char@applicability
    race <- sem$race.eth@applicability
    expect_length(age$schemas, if (min(years) < 2017 && max(years) >= 2017) 2L else 1L)
    for (i in seq_along(age$schemas)) {
      indices <- pa_applicability_indices(age, "age.char", dn)[[i]]
      if (as.integer(dn$year[indices[1]]) < 2017) {
        expect_true("85 +" %in% age$schemas[[i]]$levels)
        expect_false("95 +" %in% age$schemas[[i]]$levels)
        expect_false("asian" %in% race$schemas[[i]]$levels)
      } else {
        expect_false("85 +" %in% age$schemas[[i]]$levels)
        expect_true("95 +" %in% age$schemas[[i]]$levels)
        expect_true("asian" %in% race$schemas[[i]]$levels)
      }
    }
    expect_match(paste(sem$race.eth@notes, collapse = " "), "no numerical decomposition")
  }
})

test_that("TDC canonical infant and open age labels support exact harmonization", {
  expect_identical(pa_age_contributors(c("< 1", as.character(1:4)), "0-4"),
    c("< 1", as.character(1:4)))
  expect_identical(pa_age_contributors(c(as.character(85:94), "95 +"), "85 +"),
    c(as.character(85:94), "95 +"))
})
