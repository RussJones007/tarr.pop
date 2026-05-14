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
