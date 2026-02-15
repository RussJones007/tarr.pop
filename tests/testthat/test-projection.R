

test_that("writer creates a single dataset with stat dim", {
  out_dim <- c(year = 5, a = 2, stat = 2)
  out_dn  <- list(year = as.character(1:5), a = c("x","y"), stat = c("projection","std_error"))
  
  w <- tp_projection_hdf5_writer(out_dim, out_dn, year_k = 1, dir = tempdir())
  h <- w$as_handles()
  
  expect_true(inherits(h$data, "DelayedArray"))
  expect_true("stat" %in% names(dimnames(h$data)))
  expect_equal(dimnames(h$data)$stat, c("projection","std_error"))
})

test_that("write_year_slice writes into chosen stat slice", {
  out_dim <- c(year = 3, a = 1, stat = 2)
  out_dn  <- list(year = c("1","2","3"), a = "x", stat = c("projection","std_error"))
  
  w <- tp_projection_hdf5_writer(out_dim, out_dn, year_k = 1, dir = tempdir())
  h <- w$as_handles()
  
  w$write_year_slice("data", fixed_k_list = list("2" = 1), stat = "projection", values = c(10, 11, 12))
  w$write_year_slice("data", fixed_k_list = list("2" = 1), stat = "std_error",  values = c(1,  2,  3))
  
  # EAGER read, but tiny
  proj <- as.array(h$data)[,1,1]
  se   <- as.array(h$data)[,1,2]
  
  expect_equal(proj, c(10,11,12))
  expect_equal(se, c(1,2,3))
})
