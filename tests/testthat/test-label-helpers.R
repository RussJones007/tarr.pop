make_label_helper_fixture <- function(dn = list(year = c("2020", "2021"),
  area.name = c("A", "B"), age.char = c("0-4", "5-9"),
  sex = c("Female", "Male"), race = c("Asian", "White"),
  ethnicity = c("Hispanic", "Not Hispanic"))) {
  as.poparray(array(seq_len(prod(lengths(dn))), unname(lengths(dn)), dn),
              filepath = tempfile(fileext = ".h5"))
}

test_that("label helpers follow renamed roles and semantic domains", {
  x <- make_label_helper_fixture()
  out <- dplyr::rename(x, time = year, zip = area.name, age = age.char,
                       gender = sex, ancestry = race, origin = ethnicity)
  expect_identical(times(out), dimnames(x)$year)
  expect_identical(years(out), times(out))
  expect_identical(areas(out), dimnames(x)$area.name)
  expect_identical(ages(out), dimnames(x)$age.char)
  expect_identical(sexes(out), dimnames(x)$sex)
  expect_identical(races(out), dimnames(x)$race)
  expect_identical(ethnicities(out), dimnames(x)$ethnicity)
  expect_identical(ages(out, remove = "5-9"), "0-4")
  expect_identical(sexes(out, remove = stringr::regex("^F")), "Male")
})

test_that("missing dimensions are explicit and differ from empty selections", {
  x <- make_label_helper_fixture(list(year = "2020", area.name = "76101"))
  expect_identical(areas(x), "76101")
  expect_identical(times(x), "2020")
  expect_error(ages(x), "absent")
  expect_error(sexes(x), "absent")
  expect_error(races(x), "absent")
  expect_null(ages(x, missing = "null"))
  expect_null(sexes(x, missing = "null"))
  expect_null(races(x, missing = "null"))
  expect_null(ethnicities(x, missing = "null"))
  expect_null(dim_labels(x, "age.char", missing = "null"))
  expect_identical(areas(x, remove = "76101", missing = "null"), character())
  expect_error(ages(x, missing = "ignore"))
})

test_that("time helpers return date labels without numeric conversion", {
  x <- make_label_helper_fixture(list(year = c("2020-01-01", "2020-01-02"), area.name = "A")) |>
    dplyr::rename(date = year)
  expect_identical(times(x), c("2020-01-01", "2020-01-02"))
  expect_identical(years(x), times(x))
})

test_that("domain aliases, ambiguity and legacy fallback are handled", {
  x <- make_label_helper_fixture()
  sem <- dim_semantics(x)
  sem$age.char <- pa_update_dim_semantics(sem$age.char, domain = "age interval")
  dim_semantics(x) <- sem
  out <- dplyr::rename(x, age = age.char)
  expect_identical(ages(out), ages(x))
  sem$sex <- pa_update_dim_semantics(sem$sex, domain = "age")
  dim_semantics(x) <- sem
  expect_error(ages(x), "Multiple dimensions")
  expect_error(ages(x, missing = "null"), "Multiple dimensions")
  expect_identical(dim_labels(x, "age.char"), c("0-4", "5-9"))
  sem$sex <- pa_update_dim_semantics(sem$sex, domain = "custom sex")
  dim_semantics(x) <- sem
  expect_identical(sexes(x), c("Female", "Male"))
})
