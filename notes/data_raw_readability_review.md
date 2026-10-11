# Readability review of newer data-raw scripts

Date: 2026-10-10

## Scope and preferences

Reviewed the recent Census build/update scripts, TDC build/update scripts,
and their support helpers. Excluded `data-raw/Archived/` and the older Census
implementation (`census_data.r`). This review proposes changes only; no
source files were changed.

Prefer descriptive helper functions called by `purrr::map()`, `walk()`, or
`reduce()` over explicit loops when this improves readability. Prefer native
pipes for sequential transformations. Retain concise nesting in conditions
and other places where it communicates intent more clearly.

There was only one explicit `for` loop in the newer scripts reviewed. Most
opportunities involve substantial anonymous functions and nested operations.

## Findings

### 1. Replace metadata mutation with a helper and imap

Location: `data-raw/census_data_2.r`, `census_dimension_semantics()` (line 97).

The function builds semantics, loops over age/race dimensions, and mutates
`result`. Extract `add_year_applicability(entry, dimension, support)` and use
`purrr::imap()` to return updated entries while preserving list names.
Avoid using `walk()` to mutate an outer variable. A separate
`schema_for_year()` helper would clarify the inner mapping.

### 2. Use walk for download side effects

Location: `data-raw/census_data_2.r`, `census_estimate_files()` (line 247).

The function uses `invisible(lapply(...))` for download side effects.
Extract `download_census_file(url, destination)` and use `purrr::walk2()`
over missing URLs and paths. Keep temporary-file creation, `on.exit()`
cleanup, and rename validation inside the helper.

### 3. Express readers as map-to-combine pipelines

Locations in `data-raw/census_data_2.r`:

- `read_census_decennial()` (line 165).
- `read_census_estimates()` (line 305).
- `read_census_zcta()` (line 420).

These readers nest `lapply()` inside `bind_rows()`. Use a native pipeline:

```r
years |>
  purrr::map(read_decennial_year) |>
  dplyr::bind_rows()
```

The estimates reader could use `imap()` because each file's name identifies
its series. Preserve file/year ordering and existing reader arguments.

### 4. Extract ACS measure construction

Location: `data-raw/census_data_2.r`, `build_census_zcta()` (line 468).

The function maps over estimates and margins of error using a substantial
anonymous function. Extract `build_census_zcta_measure(measure, ...)` to give
MOE-specific metadata handling a descriptive home. Use `map()` because the
resulting paths are needed. Preserve the distinct measure metadata and
the warning that MOEs are not additive.

### 5. Clarify Census vintage semantics

Location: `data-raw/census_update_estimates.r`,
`census_vintage_semantics()` (line 70).

The function contains nested mappings and nested update/compression calls.
Extract helpers such as `historical_schema_bounds()` and
`extend_dimension_applicability()`. Use `imap()` for named semantics and
pipe the final update:

```r
entry |>
  tarr.pop:::pa_update_dim_semantics(
    applicability = list(by = "year", schemas = schemas)
  ) |>
  tarr.pop:::pa_compress_applicability(combined_dimnames)
```

Preserve open-boundary handling and historical schema levels.

### 6. Pipe sequential reading and transformation

Locations: Census build functions in `data-raw/census_data_2.r` and
`update_census_estimate_vintage()` in
`data-raw/census_update_estimates.r` (line 188).

Replace nested reader/transformer calls with native pipelines:

```r
df <- read_census_estimates(input_dir, vintage = vintage, ...) |>
  transform_census_estimates()
```

Repeated conversions in the Census and TDC scripts, such as
`sort(unique(as.integer(...)))` and
`as.character(rage::as.age_group(...))`, are also candidates for short
pipelines when used as sequential transformations.

## Priorities and boundaries

Prioritize findings **1, 2, and 4**: these clarify responsibilities rather
than merely changing syntax.

Retain concise nested checks in `if` statements and idiomatic `data.table`
`.SD` operations. The three explicit build calls in `build_census_cubes.r`
are readable enough to keep; introducing mapping there is optional.

Do not change aggregation semantics, source counts, factor ordering,
applicability boundaries, or delayed cube behavior as part of these
readability refactors. Native pipes do not alter data.table's reference
semantics, so helper extraction must preserve the intended mutation behavior.

## Validation if implemented

Run existing source-transformation and metadata/applicability tests, checking
that counts, factor levels, label order, and historical/incoming schemas are
unchanged. For the download helper, verify temporary-file cleanup and failure
handling. No exported API changes or new exported-function documentation
are needed for the proposed internal helpers.
