make_demographic_fixture <- function(projection = FALSE) {
  dn <- list(year = c("2030", "2031"), area.name = c("A", "B"),
    race = c("White", "Black", "Total"))
  if (projection) dn$stat <- c("projection", "std_error")
  backend <- HDF5Array::writeHDF5Array(array(as.numeric(seq_len(prod(lengths(dn)))),
    dim = unname(lengths(dn)), dimnames = dn), filepath = tempfile(fileext = ".h5"))
  dimnames(backend) <- dn
  sem <- default_dim_semantics(setdiff(names(dn), "stat"), "year", "area.name")
  sem$race <- pa_update_dim_semantics(sem$race, overlap_levels = "Total",
    applicability = list(by = "year", schemas = list(
      list(from = NULL, through = NULL, levels = dn$race))))
  if (projection) return(new_poparray_projection_s4(backend, 0.95, "ETS",
    list(note = "Fixture", source = "example", updated = "2026-10-31"),
    c("2028", "2029"), "year", "area.name", dim_semantics = sem))
  new_poparray(backend, dim_semantics = sem)
}

test_that("demographic_array is virtual and concrete classes are siblings", {
  expect_true(methods::isVirtualClass("demographic_array"))
  expect_error(methods::new("demographic_array"), "virtual")
  for (projected in c(FALSE, TRUE)) {
    x <- make_demographic_fixture(projected)
    expect_s4_class(x, "demographic_array")
    expect_s4_class(x, "DelayedArray")
    expect_true(methods::validObject(x))
    expect_identical(is(x, "poparray"), !projected)
    expect_identical(is(x, "poparray_projection"), projected)
    bad <- x
    bad@area_role <- bad@time_role
    expect_match(paste(methods::validObject(bad, test = TRUE), collapse = " "), "different")
    bad <- x
    bad@dim_semantics <- rev(bad@dim_semantics)
    expect_match(paste(methods::validObject(bad, test = TRUE), collapse = " "), "same order")
  }
})
