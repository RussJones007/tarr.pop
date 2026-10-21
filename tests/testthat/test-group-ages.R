make_group_ages_fixture <- function() {
  dn <- list(
    year = c("2020", "2021"),
    area.name = c("A", "B"),
    age.char = c("0-4", "5-9", "10-14")
  )
  arr <- array(
    as.numeric(seq_len(prod(unname(lengths(dn))))),
    dim = unname(lengths(dn)),
    dimnames = dn
  )

  fp <- tempfile("group_ages_fixture_", fileext = ".h5")
  pa_write_poparray_cube(
    x = arr,
    filepath = fp,
    dimnames_list = dn,
    overwrite = TRUE,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = default_dim_semantics(names(dn), "year", "area.name")
  )
  dx <- HDF5Array::HDF5Array(filepath = fp, name = "cube/population")
  dimnames(dx) <- dn
  new_poparray(
    dx,
    dimnames_list = dn,
    time_dim = "year",
    area_dim = "area.name",
    dim_semantics = default_dim_semantics(names(dn), "year", "area.name")
  )
}

test_that("group_ages returns a valid poparray and preserves metadata", {
  pa <- make_group_ages_fixture()

  out <- group_ages(pa, c("0-9", "10-14"))

  expect_s4_class(out, "poparray")
  expect_true(tarr.pop:::is_hdf5_backed_delayed(out))
  expect_silent(validate_poparray(out))
  expect_equal(time_role(out), time_role(pa))
  expect_equal(area_role(out), area_role(pa))
  expect_identical(names(dim_semantics(out)), names(dim_semantics(pa)))
  expect_equal(dimnames(out)$age.char, c("0-9", "10-14"))
})

test_that("group_ages matches collapse_dim age specialization", {
  pa <- make_group_ages_fixture()

  grouped <- group_ages(pa, c("0-9", "10-14"))
  collapsed <- collapse_dim(
    pa,
    "age.char",
    list("0-9" = c("0-4", "5-9"), "10-14" = "10-14")
  )

  expect_equal(as.array(grouped), as.array(collapsed))
  expect_equal(dimnames(grouped), dimnames(collapsed))
})

test_that("group_ages accepts agecraft preset breaks and preserves totals", {
  pa <- make_group_ages_fixture()
  out <- group_ages(pa, agecraft::age_group_presets$ILI)

  expect_identical(dimnames(out)$age.char, c("0-4", "5-24"))
  expect_true(tarr.pop:::is_hdf5_backed_delayed(out))
  expect_equal(time_role(out), time_role(pa))
  expect_equal(area_role(out), area_role(pa))
  expect_equal(as.array(out)[, , 1], as.array(pa)[, , 1])
  expect_equal(as.array(out)[, , 2], as.array(pa)[, , 2] + as.array(pa)[, , 3])
  expect_silent(validate_poparray(out))
})

test_that("agecraft canonical labels retain interval safety", {
  expect_false(pa_has_interval_overlap(c("< 1", "1-4", "5-9", "85 +")))
  expect_true(pa_has_interval_overlap(c("< 1", "0-4")))
  expect_true(pa_has_interval_overlap(c("85 +", "90-94")))
  expect_identical(pa_age_contributors(c("85-89", "90-94", "95 +"), "85+"),
    c("85-89", "90-94", "95 +"))
  expect_identical(pa_age_contributors("85 +", "90+"), character())
})
