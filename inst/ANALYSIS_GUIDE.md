# tarr.pop Analysis Guide

This guide is for analysts using population cubes for filtering, grouping,
projection, and tabular extraction.

## Main Objects

- `poparray`: the main population cube class
- `poparray_projection`: a projected population cube with `projection` and `std_error`

## Typical Analysis Workflow

1. Open a cube lazily with `open_poparray()`.
2. Inspect dimensions and metadata with `dimnames()`, `time_role()`, `area_role()`, `dim_semantics()`, and `get_source()`.
3. Restrict to the relevant slice using `filter.poparray()` or `[` subsetting.
4. Group safely with `collapse_dim()` or `group_ages()` if needed.
5. Summarize with `summary()`, `sum()`, `sd()`, or `by.poparray()`.
6. Convert to tabular form only after slicing to a manageable subset.

## Most Useful Functions for Analysis

### Open and Inspect

- `open_poparray()`
- `time_role()`
- `area_role()`
- `dim_semantics()`
- `get_source()`
- `dim_labels()`, `years()`, `areas()`, `ages()`, `sexes()`, `races()`, `ethnicities()`

### Filter and Slice

- `filter.poparray()`
- `%between%`
- `[` on `poparray`
- `split.poparray()`
- `by.poparray()`

### Group and Aggregate

- `collapse_dim()`
- `group_ages()`
- `sum()`
- `sd()`
- `summary.poparray()`

### Projection

- `project()`
- `projection()`
- `std_error()`
- `plot.poparray_projection()`
- `as.poparray.poparray_projection()`

### Tabular Output

- `as.data.frame.poparray()`
- `as_tibble.poparray()`
- `as.data.frame.poparray_projection()`
- `as_tibble.poparray_projection()`

These tabular coercions are **EAGER**. Subset first.

## Analysis Rules

- Prefer lazy slicing before any tabular coercion.
- Treat age labels as intervals, not plain strings.
- Do not assume all stratification dimensions are safe to sum across.
- Use `dim_semantics()` to understand whether a dimension is a partition or an overlapping set.
- Use guarded `sum()` rather than manually coercing to arrays for totals.

## Minimal Example

```r
x <- open_poparray("seer_estimates_county_5y")

x_small <- dplyr::filter(
  x,
  year %between% c(2015, 2020),
  area.name %in% c("Tarrant", "Dallas"),
  sex == "Female"
)

age_grp <- group_ages(x_small, c("0-19", "20-64", "65+"))

summary(age_grp)

df <- as.data.frame(age_grp)
```

## When to Use Projection Objects

Use `project()` when you want future values with uncertainty. Use:

- `projection()` to access the projected counts
- `std_error()` to access uncertainty
- `plot.poparray_projection()` for standard visualization

Convert to tabular form only after reducing to a focused slice.
