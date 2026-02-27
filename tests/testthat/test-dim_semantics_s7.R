test_that("valid construction works for all scale_type + partition_type combos", {
  scales <- c("nominal", "ordinal", "interval")
  partitions <- c("partition", "set", "unknown")

  combos <- expand.grid(
    scale_type = scales,
    partition_type = partitions,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(combos))) {
    sc <- combos$scale_type[[i]]
    pt <- combos$partition_type[[i]]

    sem <- new_dim_semantics(
      dim_name = "age.char",
      domain = "age",
      scale_type = sc,
      partition_type = pt,
      validated = FALSE,
      overlap_levels = character(),
      notes = character()
    )

    expect_true(S7::S7_inherits(sem, DimSemantics))
    expect_identical(sem@scale_type, sc)
    expect_identical(sem@partition_type, pt)
  }
})

test_that("invalid scale_type errors", {
  expect_error(
    new_dim_semantics(
      dim_name = "age.char",
      domain = "age",
      scale_type = "ratio",
      partition_type = "unknown"
    ),
    "@scale_type must be one of"
  )
})

test_that("invalid partition_type errors", {
  expect_error(
    new_dim_semantics(
      dim_name = "race",
      domain = "race",
      scale_type = "nominal",
      partition_type = "overlap"
    ),
    "@partition_type must be one of"
  )
})

test_that("partition_type='partition' with non-empty overlap_levels errors", {
  expect_error(
    new_dim_semantics(
      dim_name = "sex",
      domain = "sex",
      scale_type = "nominal",
      partition_type = "partition",
      overlap_levels = c("Female", "Male")
    ),
    "@overlap_levels must be empty"
  )
})
