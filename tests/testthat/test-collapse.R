make_collapse_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    age.char = c("0-4", "5-9")
  )
  arr <- array(
    as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  new_poparray(arr)
}

make_collapse_time_role_fixture <- function() {
  dn <- list(
    time = c("2020", "2021"),
    area.name = "A",
    age.char = c("0-4", "5-9")
  )
  arr <- array(
    as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )
  new_poparray(arr, time_dim = "time", area_dim = "area.name")
}

test_that("collapse_dim generic works with positional args", {
  pa <- make_collapse_fixture()
  groups <- c("0-4" = "0-9", "5-9" = "0-9")

  out <- collapse_dim(pa, "age.char", groups)

  expect_s3_class(out, "poparray")
  expect_equal(dimnames(out)$age.char, "0-9")
})

test_that("collapse_dim preserves non-default time role metadata", {
  pa <- make_collapse_time_role_fixture()
  groups <- c("0-4" = "0-9", "5-9" = "0-9")

  out <- collapse_dim(pa, "age.char", groups)

  expect_s3_class(out, "poparray")
  expect_equal(time_role(out), "time")
  expect_equal(area_role(out), "area.name")
  expect_true("time" %in% names(dimnames(out)))
})

test_that("renaming collapsed role dimension updates dimroles", {
  pa <- make_collapse_fixture()
  groups <- c("2020" = "p0", "2021" = "p1")

  out <- collapse_dim(pa, "year", groups, name = "period")

  expect_s3_class(out, "poparray")
  expect_true("period" %in% names(dimnames(out)))
  expect_false("year" %in% names(dimnames(out)))
  expect_equal(time_role(out), "period")
  expect_equal(area_role(out), "area.name")
})

test_that("keep_empty retains declared unused factor levels", {
  pa <- new_poparray(
    array(
      c(5, 7),
      dim = c(1, 1, 2),
      dimnames = list(
        year = "2020",
        area.name = "A",
        age.char = c("0-4", "5-9")
      )
    )
  )

  groups <- factor(c("A", "A"), levels = c("A", "B"))

  out <- collapse_dim(pa, "age.char", groups, keep_empty = TRUE)
  arr_out <- as.array(out$handle)

  expect_equal(dimnames(out)$age.char, c("A", "B"))
  expect_equal(as.numeric(arr_out[1, 1, 1]), 12)
  expect_equal(as.numeric(arr_out[1, 1, 2]), 0)
})
