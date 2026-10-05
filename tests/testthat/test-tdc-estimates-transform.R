# Load the data-raw functions without running the production build script.
load_tdc_estimate_transform <- function() {
  path <- test_path("../../data-raw/tdc_estimates.r")
  if (!file.exists(path)) skip("TDC ingestion script is available in the source repository only")
  env <- new.env(parent = asNamespace("tarr.pop"))
  functions <- c("process_age_char", "ordered_age_levels", "transform_tdc_estimates")
  for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        as.character(expr[[2L]]) %in% functions) {
      eval(expr, env)
    }
  }
  env
}

# Fixtures represent read_tdc_estimates_raw() output: County/Area Name becomes
# county, FIPS/Area Code becomes fips, and race/sex components are integers.
tdc_geography_fixture <- function(county = "DeWitt", fips = "123", year = 2020L) {
  data.table::data.table(
    year = year,
    county = factor(county),
    fips = factor(fips),
    age = ordered("< 01 Year"),
    white_female = 40L,
    white_male = 0L
  )
}

test_that("older De Witt source schema retains its coordinates and values", {
  env <- load_tdc_estimate_transform()
  raw <- data.table::data.table(
    year = 2016L, county = factor("DE WITT COUNTY"), fips = factor("123"),
    age = ordered("85 Years +"), white_female = 0L, white_male = 17L
  )
  out <- env$transform_tdc_estimates(raw, counties = "De Witt")
  expect_identical(names(out), c("year", "area.name", "sex", "age.char", "race.eth", "population"))
  expect_equal(nrow(out), 2L)
  expect_identical(as.character(out$area.name), rep("De Witt", 2L))
  expect_identical(as.character(out$age.char), rep("85 +", 2L))
  expect_identical(out$population[out$sex == "female"], 0L)
  expect_identical(out$population[out$sex == "male"], 17L)
  expect_identical(out$year, rep(2016L, 2L))
})

test_that("new DeWitt source schema preserves all 960 age sex race cells", {
  env <- load_tdc_estimate_transform()
  ages <- c("< 01 Year", paste(sprintf("%02d", 1:94), "Years"), "95+ Years")
  fixture <- data.table::data.table(
    year = 2024L, county = factor("DeWitt"), fips = factor("123"),
    age = ordered(ages, levels = ages)
  )
  races <- c("white", "black", "hispanic", "asian", "other")
  expected <- list()
  k <- 0L
  for (race in races) {
    for (sex in c("female", "male")) {
      values <- seq_len(96L) + k * 100L
      if (race == "white" && sex == "female") values[96L] <- 0L
      fixture[[paste(race, sex, sep = "_")]] <- values
      expected[[paste(race, sex)]] <- values
      k <- k + 1L
    }
  }
  out <- env$transform_tdc_estimates(fixture, counties = "De Witt")
  age_labels <- c("< 1", as.character(1:94), "95 +")
  expect_equal(nrow(out), 960L)
  expect_equal(anyDuplicated(out[, 1:5, with = FALSE]), 0L)
  expect_identical(unique(as.character(out$area.name)), "De Witt")
  expect_setequal(levels(out$age.char), age_labels)
  expect_true(is.ordered(out$age.char))
  for (race in races) {
    for (sex in c("female", "male")) {
      indices <- which(as.character(out$race.eth) == race & as.character(out$sex) == sex)
      cells <- out[indices]
      expect_identical(cells$population[match(age_labels, as.character(cells$age.char))],
                       expected[[paste(race, sex)]])
    }
  }
  zero <- out[out$sex == "female" & out$race.eth == "white" & out$age.char == "95 +"]
  expect_equal(nrow(zero), 1L)
  expect_identical(zero$population, 0L)
  expect_equal(sum(out$population), sum(unlist(expected)))
})

test_that("FIPS identity overrides display spelling and normalizes short codes", {
  env <- load_tdc_estimate_transform()
  for (code in c("123", "48123")) {
    out <- env$transform_tdc_estimates(
      tdc_geography_fixture("A different display label", code), counties = "De Witt"
    )
    expect_equal(nrow(out), 2L)
    expect_identical(as.character(out$area.name), rep("De Witt", 2L))
    expect_identical(out$population, c(40L, 0L))
  }
  for (code in c("1", "001", "48001")) {
    out <- env$transform_tdc_estimates(tdc_geography_fixture("ANDERSON COUNTY", code))
    expect_identical(unique(as.character(out$area.name)), "Anderson")
  }
})

test_that("Texas state flag controls old schema totals with and without a county whitelist", {
  env <- load_tdc_estimate_transform()
  raw <- data.table::data.table(
    year = 2016L, county = factor(c("STATE OF TEXAS", "DE WITT COUNTY")),
    fips = factor(c("000", "123")), age = ordered("< 1 Year"),
    white_female = c(100L, 40L), white_male = c(101L, 0L)
  )
  for (counties in list(NULL, "De Witt", c("De Witt", "Texas"))) {
    excluded <- env$transform_tdc_estimates(data.table::copy(raw), counties, FALSE)
    expect_identical(unique(as.character(excluded$area.name)), "De Witt")
    expect_equal(sum(excluded$population), 40L)
    included <- env$transform_tdc_estimates(data.table::copy(raw), counties, TRUE)
    expect_setequal(as.character(included$area.name), c("De Witt", "Texas"))
    expect_equal(sum(included$population[included$area.name == "Texas"]), 201L)
    expect_equal(nrow(included), 4L)
  }
  for (label in c("state of texas", "State Of Texas", "Texas")) {
    out <- env$transform_tdc_estimates(tdc_geography_fixture(label, "48000"),
                                     counties = "De Witt", include_texas_total = TRUE)
    expect_identical(unique(as.character(out$area.name)), "Texas")
  }
})

test_that("unresolved geography fails before a county whitelist can discard it", {
  env <- load_tdc_estimate_transform()
  for (code in c("999", "123x", "47123", NA_character_)) {
    expect_error(
      env$transform_tdc_estimates(tdc_geography_fixture("Unknown county", code), counties = "De Witt"),
      "Unresolved TDC geography identifiers.*Unknown county"
    )
  }
  # A plausible display name must not conceal an invalid identifier.
  expect_error(env$transform_tdc_estimates(tdc_geography_fixture("De Witt", "999")),
               "Unresolved TDC geography identifiers")
  missing_id <- tdc_geography_fixture()
  missing_id[, fips := NULL]
  expect_error(env$transform_tdc_estimates(missing_id), "require FIPS/Area Code")
})

test_that("new schema state code 48 canonicalizes Texas independently of the whitelist", {
  env <- load_tdc_estimate_transform()
  raw <- data.table::data.table(
    year = 2024L, county = factor(c("Texas", "DeWitt")),
    fips = factor(c("48", "123")), age = ordered("< 01 Year"),
    white_female = c(100L, 40L)
  )
  excluded <- env$transform_tdc_estimates(data.table::copy(raw), counties = NULL,
                                         include_texas_total = FALSE)
  expect_identical(as.character(excluded$area.name), "De Witt")
  expect_identical(excluded$population, 40L)
  included <- env$transform_tdc_estimates(data.table::copy(raw), counties = "De Witt",
                                         include_texas_total = TRUE)
  expect_setequal(as.character(included$area.name), c("Texas", "De Witt"))
  expect_identical(included$population[included$area.name == "Texas"], 100L)
})

test_that("transformation preserves supported source NA and observed zero separately", {
  env <- load_tdc_estimate_transform()
  raw <- tdc_geography_fixture("DE WITT COUNTY", "123", 2016L)
  raw[, asian_female := NA_integer_]
  raw[, asian_male := NA_integer_]
  out <- env$transform_tdc_estimates(raw, counties = "De Witt")
  expect_equal(nrow(out), 4L)
  expect_identical(out$population[out$race.eth == "asian"], c(NA_integer_, NA_integer_))
  expect_identical(out$population[out$sex == "male" & out$race.eth == "white"], 0L)
  expect_identical(out$population[out$sex == "female" & out$race.eth == "white"], 40L)
})

test_that("the authoritative county identity reference is one-to-one", {
  reference <- tarr.pop::county_fips
  expect_equal(anyDuplicated(as.character(reference)), 0L)
  expect_equal(anyDuplicated(names(reference)), 0L)
  expect_identical(as.character(reference["De Witt"]), "48123")
  expect_identical(as.character(reference["Texas"]), "48000")
})
