# INGESTION_LAYER.md

## Purpose

This document defines the **population data ingestion framework** for the `tarr.pop` package.

The ingestion layer is implemented **inside the package (not data-raw)** and provides a standardized, testable pipeline for converting heterogeneous source data (Census, TDC, ACS, etc.) into **HDF5-backed poparray cubes**.

---

## Key Clarification

⚠️ Ingestion is **NOT lazy**

* Ingestion is an **eager ETL step**
* It produces an HDF5-backed cube
* Laziness begins **after ingestion** via `DelayedArray`

---

## Architecture Overview

```
Reader → Transformer → Normalizer → Validator → Builder → Storage
```

Each stage is explicit and testable.

---

## Core Design Principles

### 1. Separate Schema from Semantics

* Schema = column names (`year`, `sex`, etc.)
* Semantics = meaning (`partition`, `interval`, etc.)

👉 These MUST NOT be conflated.

---

### 2. No Aggregate Levels Stored (Default)

Values like:

```
"All"
"Total"
"All Ages"
```

represent derived aggregates and are **removed by default**.

Retention is allowed but requires:

* explicit opt-in
* explicit semantic override (non-partition)

---

### 3. Explicit Dimensional Semantics (NEW REQUIREMENT)

Semantics are **NOT inferred automatically**.

They MUST be provided explicitly during ingestion.

This avoids reliance on:

```
default_dim_semantics()
```

which is currently used internally during construction.

---

### 4. Explicit Completion Policy (CRITICAL)

Before calling `df_2_array()`, ingestion MUST define:

| Policy    | Meaning                     |
| --------- | --------------------------- |
| `"error"` | missing cells are invalid   |
| `"zero"`  | missing cells filled with 0 |
| `"na"`    | allow NA structural cells   |

👉 This is required because `df_2_array()` uses `tidyr::complete()` and may expand sparse data.

---

### 5. Package-Native Builders

Ingestion MUST use:

* `df_2_array()`
* `as.poparray.array()`
* `create_poparray()`

No manual array construction.

---

## Canonical Schema (Flexible Contract)

Required columns (names only):

```
year
area.name
population
```

Optional but typical:

```
sex
age.char
race
ethnicity
```

👉 Not all sources must have all dimensions (e.g., ZCTA)

---

## Core Functions

---

### normalize_totals()

⚠️ Minimal normalization only — not exhaustive.

```r
normalize_totals <- function(x) {
  x <- as.character(x)
  x[x %in% c("Total", "All", "All Ages")] <- "All"
  x
}
```

---

### prepare_population_df()

Schema normalization only (no semantics).

```r
prepare_population_df <- function(df, dims, drop_all = TRUE) {

  missing <- setdiff(c(dims, "population"), names(df))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  df <- df |>
    dplyr::mutate(
      across(any_of(dims), normalize_totals)
    )

  if (isTRUE(drop_all)) {
    for (d in dims) {
      df <- df[df[[d]] != "All", , drop = FALSE]
    }
  }

  df
}
```

---

### apply_completion_policy()

NEW: explicitly handle sparsity.

```r
apply_completion_policy <- function(df, dims, policy = c("error", "zero", "na")) {

  policy <- match.arg(policy)

  if (policy == "error") {
    return(df)
  }

  df_full <- tidyr::complete(df, !!!rlang::syms(dims))

  if (policy == "zero") {
    df_full$population[is.na(df_full$population)] <- 0
  }

  df_full
}
```

---

### validate_population_df()

```r
validate_population_df <- function(df, allow_na = FALSE) {

  if (any(df$population < 0, na.rm = TRUE)) {
    stop("Population contains negative values")
  }

  if (!allow_na && any(is.na(df$population))) {
    stop("Population contains NA values")
  }

  invisible(TRUE)
}
```

---

### build_poparray_from_df()

⚠️ Semantics MUST be passed explicitly.

```r
build_poparray_from_df <- function(df, dim_semantics) {

  arr <- df_2_array(df, data_col = "population")

  pa <- as.poparray.array(arr)

  dim_semantics(pa) <- dim_semantics

  pa
}
```

---

### ingest_population()

Expanded interface (more realistic for package use).

```r
ingest_population <- function(
  reader,
  transformer,
  dims,
  dim_semantics,
  filepath,
  series_id,
  completion_policy = "error",
  drop_all = TRUE,
  source_meta = list(),
  ...
) {

  df <- reader(...)

  df <- transformer(df, ...)

  df <- prepare_population_df(df, dims = dims, drop_all = drop_all)

  df <- apply_completion_policy(df, dims = dims, policy = completion_policy)

  validate_population_df(df, allow_na = completion_policy == "na")

  pa <- build_poparray_from_df(df, dim_semantics)

  pa <- set_source_url(
    pa,
    nm = source_meta$nm %||% "Unknown",
    pop_type = source_meta$pop_type %||% "Unknown",
    url = source_meta$url %||% ""
  )

  create_poparray(
    x = pa,
    filepath = filepath,
    series_id = series_id
  )

  invisible(filepath)
}
```

---

## Semantics Are Source-Specific

⚠️ DO NOT hardcode semantics globally

Example:

```r
tdc_semantics <- list(
  year = new_dim_semantics("year", "time", "interval", "partition"),
  area.name = new_dim_semantics("area.name", "geo", "nominal", "partition"),
  sex = new_dim_semantics("sex", "sex", "nominal", "partition"),
  age.char = new_dim_semantics("age.char", "age", "interval", "partition"),
  race = new_dim_semantics("race", "race", "nominal", "partition"),
  ethnicity = new_dim_semantics("ethnicity", "ethnicity", "nominal", "partition")
)
```

👉 Another source MAY differ.

---

## Integration with Cube Storage

All ingestion MUST be called through:

```r
build_cube_if_missing()
```

Example:

```r
build_cube_if_missing(
  name = "census_decennial",
  builder_fun = function(target_dir, filepath) {

    ingest_population(
      reader = read_census_decennial,
      transformer = transform_census_decennial,
      dims = c("year", "area.name", "sex", "age.char", "race", "ethnicity"),
      dim_semantics = census_semantics,
      filepath = filepath,
      series_id = "census_decennial",
      completion_policy = "error"
    )
  }
)
```

---

## Idempotence and Rebuild Policy

* `build_cube_if_missing()` ensures **no overwrite**
* To rebuild:

  * delete file manually OR
  * implement future `rebuild = TRUE`

---

## What NOT to Do

❌ Assume semantics
❌ Store aggregate levels by default
❌ Rely on df completeness implicitly
❌ Build arrays manually
❌ Mix ingestion with analysis

---

## Testing Requirements

```r
test_that("ingestion creates cube", {
  fp <- tempfile(fileext = ".h5")

  ingest_population(...)

  expect_true(file.exists(fp))
})

test_that("completion policy enforced", {
  expect_error(
    apply_completion_policy(df, dims, "error")
  )
})
```

---

## Summary

This ingestion layer:

* Formalizes ETL inside the package
* Makes semantics explicit (not implicit)
* Handles sparsity correctly
* Scales across multiple sources
* Integrates cleanly with cube storage

It replaces legacy scripts with a **contract-driven ingestion system** aligned with `tarr.pop` architecture.
