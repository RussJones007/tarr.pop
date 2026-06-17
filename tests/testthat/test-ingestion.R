make_ingestion_semantics <- function(dims, time_dim = "year", area_dim = "area.name") {
  out <- lapply(dims, function(d) {
    tarr.pop:::new_dim_semantics(
      dim_name = d,
      domain = if (identical(d, time_dim)) "time" else if (identical(d, area_dim)) "area" else d,
      scale_type = if (d %in% c(time_dim, "age.char")) "interval" else "nominal",
      partition_type = if (d %in% c(time_dim, area_dim)) "partition" else "set"
    )
  })
  names(out) <- dims
  out
}

test_that("apply_completion_policy errors when source table is incomplete", {
  df <- data.frame(
    year = c("2020", "2020", "2020"),
    area.name = c("A", "A", "B"),
    sex = c("Female", "Male", "Female"),
    population = c(10, 11, 12),
    stringsAsFactors = FALSE
  )

  support <- data.frame(
    year = c("2020", "2020", "2020", "2020"),
    area.name = c("A", "A", "B", "B"),
    sex = c("Female", "Male", "Female", "Male"),
    stringsAsFactors = FALSE
  )

  expect_error(
    tarr.pop:::apply_completion_policy(
      df,
      dims = c("year", "area.name", "sex"),
      policy = "error",
      support = support
    ),
    "Missing population cells"
  )
})

test_that("apply_completion_policy requires support for zero completion", {
  df <- data.frame(
    year = c("2020", "2020"),
    area.name = c("A", "B"),
    population = c(10, 11),
    stringsAsFactors = FALSE
  )

  expect_error(
    tarr.pop:::apply_completion_policy(
      df,
      dims = c("year", "area.name"),
      policy = "zero"
    ),
    "support"
  )
})

test_that("apply_completion_policy fills only supported missing cells", {
  df <- data.frame(
    year = c("2020", "2020", "2021"),
    area.name = c("A", "B", "A"),
    population = c(10, 11, 12),
    stringsAsFactors = FALSE
  )

  support <- data.frame(
    year = c("2020", "2020", "2021", "2021"),
    area.name = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  out <- tarr.pop:::apply_completion_policy(
    df,
    dims = c("year", "area.name"),
    policy = "zero",
    support = support
  )

  expect_equal(nrow(out), 4L)
  filled <- out[year == "2021" & area.name == "B"]
  expect_equal(filled$population, 0)
})

test_that("apply_completion_policy errors when support has duplicate cells", {
  df <- data.frame(
    year = c("2020", "2020"),
    area.name = c("A", "B"),
    population = c(10, 11),
    stringsAsFactors = FALSE
  )

  support <- data.frame(
    year = c("2020", "2020", "2020"),
    area.name = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )

  expect_error(
    tarr.pop:::apply_completion_policy(
      df,
      dims = c("year", "area.name"),
      policy = "zero",
      support = support
    ),
    "support.*duplicate rows"
  )
})

test_that("prepare_population_df filters aggregate aliases directly", {
  df <- data.frame(
    year = c("2020", "2020", "2020"),
    area.name = c("A", "A", "A"),
    sex = c("Total", "All Ages", "Female"),
    population = c(99, 98, 10),
    stringsAsFactors = FALSE
  )

  out_drop <- tarr.pop:::prepare_population_df(
    df,
    dims = c("year", "area.name", "sex"),
    drop_all = TRUE
  )
  expect_equal(nrow(out_drop), 1L)
  expect_identical(out_drop$sex, "Female")
})

test_that("prepare_population_df mutates the incoming table by reference", {
  df <- data.table::data.table(
    year = c("2020", "2020"),
    area.name = c("A", "A"),
    sex = c("Total", "Female"),
    population = c(99, 10)
  )

  out <- tarr.pop:::prepare_population_df(
    df,
    dims = c("year", "area.name", "sex"),
    drop_all = FALSE
  )

  expect_true(data.table::address(df) == data.table::address(out))
  expect_identical(df$sex, c("Total", "Female"))
})

test_that("apply_completion_policy converts plain data.frame inputs to data.table in place", {
  df <- data.frame(
    year = c("2020", "2020"),
    area.name = c("A", "B"),
    population = c(10, 11),
    stringsAsFactors = FALSE
  )

  tarr.pop:::apply_completion_policy(
    df,
    dims = c("year", "area.name"),
    policy = "error"
  )

  expect_true(data.table::is.data.table(df))
})

test_that("build_poparray_from_df validates dim_semantics against cube dimensions", {
  fp <- tempfile("ingestion-bad-semantics-", fileext = ".h5")

  df <- data.frame(
    year = c("2020", "2021"),
    area.name = c("A", "A"),
    population = c(10, 11),
    stringsAsFactors = FALSE
  )

  bad_semantics <- make_ingestion_semantics(c("year", "sex"))

  expect_error(
    tarr.pop:::build_poparray_from_df(
      df = df,
      dims = c("year", "area.name"),
      dim_semantics = bad_semantics,
      filepath = fp,
      series_id = "ingestion_bad_semantics"
    ),
    "dim_semantics"
  )
})

test_that("ingest_population writes reduced-dimension cubes with explicit semantics", {
  fp <- tempfile("ingestion-reduced-", fileext = ".h5")
  dims <- c("year", "area.name")
  sem <- make_ingestion_semantics(dims)

  reader <- function(...) {
    data.frame(
      year = c("2020", "2020", "2021", "2021"),
      area.name = c("A", "B", "A", "B"),
      population = c(100, 120, 101, 121),
      scenario = "baseline",
      stringsAsFactors = FALSE
    )
  }

  out <- tarr.pop:::ingest_population(
    reader = reader,
    dims = dims,
    dim_semantics = sem,
    filepath = fp,
    series_id = "ingestion_reduced",
    source_meta = list(
      note = "Reduced Dimension Example",
      population_type = "Synthetic",
      source = "https://example.test/reduced"
    )
  )

  expect_true(file.exists(fp))
  expect_equal(out, normalizePath(fp, winslash = "/", mustWork = FALSE))

  meta <- tarr.pop:::get_cube_metadata_cached(fp)
  dimn <- tarr.pop:::read_dimnames_from_cube(fp, meta = meta)
  roles <- tarr.pop:::read_roles_from_cube(fp, meta = meta)
  dsem <- tarr.pop:::read_dim_semantics_from_cube(fp, names(dimn), roles$time, roles$area, meta = meta)
  src <- tarr.pop:::read_source_from_cube(fp, meta = meta)

  expect_identical(names(dimn), dims)
  expect_identical(dimn$year, c("2020", "2021"))
  expect_identical(dimn$area.name, c("A", "B"))
  expect_identical(roles$time, "year")
  expect_identical(roles$area, "area.name")
  expect_identical(names(dsem), dims)
  expect_true(tarr.pop:::pa_is_partition(dsem$year))
  expect_true(tarr.pop:::pa_is_partition(dsem$area.name))
  expect_identical(src[["note"]], "Reduced Dimension Example")
  expect_identical(src[["population_type"]], "Synthetic")
  expect_identical(src[["source"]], "https://example.test/reduced")
})

test_that("ingest_population can use support for zero completion", {
  fp <- tempfile("ingestion-zero-support-", fileext = ".h5")
  dims <- c("year", "area.name")
  sem <- make_ingestion_semantics(dims)
  support <- data.frame(
    year = c("2020", "2020", "2021", "2021"),
    area.name = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  tarr.pop:::ingest_population(
    reader = function(...) {
      data.frame(
        year = c("2020", "2020", "2021"),
        area.name = c("A", "B", "A"),
        population = c(10, 11, 12),
        stringsAsFactors = FALSE
      )
    },
    dims = dims,
    dim_semantics = sem,
    filepath = fp,
    series_id = "ingestion_zero_support",
    completion_policy = "zero",
    support = support
  )

  meta <- tarr.pop:::get_cube_metadata_cached(fp)
  dimn <- tarr.pop:::read_dimnames_from_cube(fp, meta = meta)
  arr <- as.array(HDF5Array::HDF5Array(filepath = fp, name = "cube/population"))
  dimnames(arr) <- dimn

  expect_equal(arr["2021", "B"], 0)
})

test_that("ingest_population errors when source has duplicate cells", {
  fp <- tempfile("ingestion-dup-cells-", fileext = ".h5")
  dims <- c("year", "area.name")
  sem <- make_ingestion_semantics(dims)

  expect_error(
    tarr.pop:::ingest_population(
      reader = function(...) {
        data.frame(
          year = c("2020", "2020"),
          area.name = c("A", "A"),
          population = c(10, 11),
          stringsAsFactors = FALSE
        )
      },
      dims = dims,
      dim_semantics = sem,
      filepath = fp,
      series_id = "ingestion_dup_cells"
    ),
    "duplicate rows"
  )
})

test_that("ingest_population persists through a single cube write path", {
  fp <- tempfile("ingestion-single-write-", fileext = ".h5")
  dims <- c("year", "area.name")
  sem <- make_ingestion_semantics(dims)
  writes <- 0L
  orig_write <- tarr.pop:::pa_write_poparray_cube

  testthat::local_mocked_bindings(
    pa_write_poparray_cube = function(...) {
      writes <<- writes + 1L
      orig_write(...)
    },
    save_poparray = function(...) {
      stop("save_poparray() should not be called by ingest_population()")
    },
    .package = "tarr.pop"
  )

  tarr.pop:::ingest_population(
    reader = function(...) {
      data.frame(
        year = c("2020", "2020", "2021", "2021"),
        area.name = c("A", "B", "A", "B"),
        population = c(10, 11, 12, 13),
        stringsAsFactors = FALSE
      )
    },
    dims = dims,
    dim_semantics = sem,
    filepath = fp,
    series_id = "ingestion_single_write"
  )

  expect_equal(writes, 1L)
  expect_true(file.exists(fp))
})

test_that("add_population_data appends a new year to an existing cube", {
  fp <- tempfile("population-add-year-", fileext = ".h5")
  dims <- c("year", "area.name", "sex")
  sem <- make_ingestion_semantics(dims)

  base_df <- data.frame(
    year = rep("2023", 4),
    area.name = rep(c("A", "B"), each = 2),
    sex = rep(c("Female", "Male"), 2),
    population = c(10, 11, 12, 13),
    stringsAsFactors = FALSE
  )

  tarr.pop:::build_poparray_from_df(
    df = base_df,
    dims = dims,
    dim_semantics = sem,
    filepath = fp,
    series_id = "population_add_year",
    source = list(note = "base", source = "test", population_type = "estimate"),
    overwrite = TRUE
  )

  new_df <- data.frame(
    year = rep("2024", 4),
    area.name = rep(c("A", "B"), each = 2),
    sex = rep(c("Female", "Male"), 2),
    population = c(20, 21, 22, 23),
    stringsAsFactors = FALSE
  )

  out <- add_population_data(
    cube = fp,
    reader = function(...) new_df,
    dims = dims,
    source_meta = list(note = "updated")
  )

  expect_equal(out, normalizePath(fp, winslash = "/", mustWork = FALSE))

  meta <- tarr.pop:::get_cube_metadata_cached(fp)
  dimn <- tarr.pop:::read_dimnames_from_cube(fp, meta = meta)
  arr <- as.array(HDF5Array::HDF5Array(filepath = fp, name = "cube/population"))
  dimnames(arr) <- dimn

  expect_identical(dimn$year, c("2023", "2024"))
  expect_equal(arr["2023", "A", "Female"], 10)
  expect_equal(arr["2024", "B", "Male"], 23)
  expect_identical(tarr.pop:::read_source_from_cube(fp, meta = meta)[["note"]], "updated")
})

test_that("add_population_data errors on overlapping append labels", {
  fp <- tempfile("population-add-overlap-", fileext = ".h5")
  dims <- c("year", "area.name")
  sem <- make_ingestion_semantics(dims)

  tarr.pop:::build_poparray_from_df(
    df = data.frame(
      year = c("2024", "2024"),
      area.name = c("A", "B"),
      population = c(10, 11),
      stringsAsFactors = FALSE
    ),
    dims = dims,
    dim_semantics = sem,
    filepath = fp,
    series_id = "population_add_overlap",
    overwrite = TRUE
  )

  expect_error(
    add_population_data(
      cube = fp,
      reader = function(...) {
        data.frame(
          year = c("2024", "2024"),
          area.name = c("A", "B"),
          population = c(20, 21),
          stringsAsFactors = FALSE
        )
      },
      dims = dims
    ),
    "overlaps existing"
  )
})
