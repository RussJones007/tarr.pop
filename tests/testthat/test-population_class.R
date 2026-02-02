# Testing the tarr_pop class

test_that("Creating data frame and tibbl from tarr_pop", {
  census <- open_tarr_pop(population$census.bureau$census)
  arr <- census["2020", c("Dallas", "Tarrant") , sexes(census) , drop = FALSE]
  df <- as.data.frame(arr)
  expect_s3_class(object = df, class = "data.frame")
  expect_setequal(names(df), c(names(dimnames(arr)), "population"))
  expect_setequal(unique(df$area.name), areas(arr))
  expect_setequal(unique(df$sex), sexes(arr))
  expect_s3_class(df$year, "ordered")
  expect_s3_class(df$age.char, "ordered")
  expect_s3_class(df$race, "factor")

  tib <- as_tibble(arr)
  expect_s3_class(object = tib, class = "data.frame")
  expect_s3_class(object = tib, class = "tbl_df")
  expect_setequal(names(tib), c(names(dimnames(arr)), "population"))
  expect_setequal(unique(tib$area.name), areas(arr))
  expect_setequal(unique(tib$sex), sexes(arr))
  expect_s3_class(tib$year, "ordered")
  expect_s3_class(tib$age.char, "ordered")
  expect_s3_class(tib$race, "factor")

})

test_that("Testing selection of tarr_pop by the indexing operator", {
 census <- open_tarr_pop(population$census.bureau$census)
 arr <- census["2020", "Tarrant"]
 expect_s3_class(arr, "tarr_pop")
 expect_equal(sum(arr), 2110640)
 expect_length(ages(arr), 103)
 expect_length(sexes(arr), 2)

 # dropping dimensions should return a HDF5Array, but not a tarr_pop
 arr <- census["2020", "Tarrant" , sexes(census),  drop = TRUE]
 expect_s4_class(arr, "DelayedArray")
 expect_false(class(arr) %in% "tarr_pop")

})

test_that("Filtering and operator variation testing", {
  census <- open_tarr_pop(population$census.bureau$census)
  expect_equal(class(census), "tarr_pop")
  cen_a <- census["2020", c("Dallas", "Tarrant")]
  expect_equal(sum(cen_a), 4724179)
  expect_equal(class(cen_a), "tarr_pop")
  expect_s3_class(cen_a, "tarr_pop")
  cen_b <- census[area.name =c("Dallas", "Tarrant"), year = "2020"]
  expect_s3_class(cen_b, "tarr_pop")
  cen_c <- census["2020", c("Dallas", "Tarrant"),,,,]
  expect_s3_class(cen_c, "tarr_pop")
  cen_d <- census |> 
    filter(year == "2020",
           area.name == c("Dallas", "Tarrant"))
  expect_s3_class(cen_d, "tarr_pop")
  expect_equal(sum(cen_a), sum(cen_b))
  expect_equal(sum(cen_a), sum(cen_c))
  expect_equal(sum(cen_a), sum(cen_d))
})





