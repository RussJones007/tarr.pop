# poparray_projection: Class Purpose and Design Decisions

## Overview

`poparray_projection` is an S3 class for projected population outputs generated from a `poparray`. It is designed for large, multi-dimensional projection cubes and follows a lazy-first model using `DelayedArray` / `HDF5Array` backends.

The class represents projection results as a **single cube** with a `stat` dimension, rather than multiple separate cubes.

------------------------------------------------------------------------

## Purpose

The class exists to:

1.  Store projected values and uncertainty in one coherent delayed array.
2.  Preserve scalability for high-dimensional population projections.
3.  Keep projection metadata (`method`, `level`, `base_years`, `source`) attached to the result.
4.  Support explicit, role-aware interoperability with `poparray`.

------------------------------------------------------------------------

## Core Data Model

A `poparray_projection` object is a list with:

-   `handle`: a `DelayedArray` (often `HDF5Array`) containing projected data.

Attributes include:

-   `level`: confidence level used for uncertainty.
-   `method`: projection method (`ARIMA`, `ETS`, `CAGR`).
-   `source`: provenance string.
-   `base_years`: base years used for model fitting.
-   `dimroles`: list with `time`, `area`, and `strata`.
-   `data_col`: default value-column label used in tabular coercions.
-   `created`: timestamp.

------------------------------------------------------------------------

## Structural Invariants

The wrapped `handle` must:

1.  Be a `DelayedArray`.
2.  Have named dimensions.
3.  Include a named `stat` dimension.
4.  Have `stat` labels that are a non-empty subset of:
    -   `projection`
    -   `std_error`
5.  Retain valid role metadata where `dimroles$time` and `dimroles$area` are present in dimension names.

Note: Full objects returned by `project()` are expected to include both `projection` and `std_error`. Subsetted projection objects may legitimately contain only one `stat` level.

------------------------------------------------------------------------

## Key Design Decisions

### 1. One-cube design (selected)

Decision:

-   Use one delayed cube with `stat` dimension (`projection`, `std_error`).

Why:

-   Avoids synchronizing separate `projected/lower/upper` cubes.
-   Keeps all projection outputs aligned on the same dimensional index space.
-   Simplifies storage and I/O for HDF5-backed outputs.

### 2. `handle` naming aligned with `poparray` (selected)

Decision:

-   Use `handle` as the wrapped delayed-array member (same naming pattern as `poparray`).

Why:

-   Reduces cognitive overhead and implementation divergence between classes.
-   Makes internal method logic and object inspection more consistent.

### 3. Uncertainty representation via standard error (selected)

Decision:

-   Store `std_error` in `stat` and derive lower/upper bounds on demand.

Why:

-   Keeps storage compact and avoids redundant data.
-   Supports confidence interval recalculation for alternate levels in methods like `confint()`.

### 4. Keep `handle` as delayed array, not embedded poparray (selected)

Decision:

-   `poparray_projection` directly wraps `DelayedArray` in `handle`.
-   Provide `as.poparray.poparray_projection()` for explicit coercion when `poparray` behavior is needed.

Why:

-   Keeps projection semantics explicit.
-   Avoids over-coupling projection class behavior to all `poparray` method semantics.
-   Still enables interoperability and method reuse through explicit conversion.

### 5. Lazy-first operations (selected)

Decision:

-   Preserve delayed behavior for indexing and most transformations.
-   Mark tabular coercion (`as.data.frame`, `as_tibble`) as eager operations.

Why:

-   Projection cubes can be large and multi-dimensional.
-   Eager realization is expensive and should remain explicit.

------------------------------------------------------------------------

## Method Behavior Summary

### Accessors

-   `projection(x)`: returns `stat = "projection"` slice (position-agnostic by named `stat` lookup).
-   `std_error(x)`: returns `stat = "std_error"` slice (position-agnostic).

### Uncertainty

-   `confint(x, level=...)`: computes lower/upper from `projection` and `std_error`.

### Subsetting

-   `[.poparray_projection` supports named/positional indexing.
-   If `stat` remains after subsetting, return `poparray_projection`.
-   If `stat` is removed and role requirements are still met, return `poparray`.
-   If role requirements are no longer met, return underlying delayed subset.

### Coercion

-   `as.poparray.poparray_projection(x)`: returns `poparray` preserving `time`/`area` roles and keeping `stat`.
-   `as.data.frame.poparray_projection(x)`: eager long-to-wide coercion with `projection` and `std_error` columns.
-   `as_tibble.poparray_projection(x)`: tibble variant of tabular coercion.

### Plotting

-   `plot.poparray_projection(type = "ts" | "pyramid")`:
    -   `ts`: totals over time with uncertainty ribbon derived from standard error.
    -   `pyramid`: uses projection slice and delegates to `plot.poparray` style logic.

------------------------------------------------------------------------

## HDF5 Writer Contract

Projection writer output is expected to:

1.  Create a single dataset with `stat` as the last dimension.
2.  Preserve named dimnames on the delayed handle returned to class constructor.
3.  Write projection and standard error by hyperslab without realizing full cube in memory.

This contract is required for `new_poparray_projection()` validation to succeed.

------------------------------------------------------------------------

## Relationship to POPARRAY_CLASS.md

`poparray_projection` follows the same storage and laziness philosophy as `poparray`, with one important extension:

-   `poparray_projection` adds a `stat` axis to represent projection outputs and uncertainty in one cube.

`poparray` remains the base semantic population cube class; `poparray_projection` is a specialized result class for forecasted outputs with projection-specific metadata and methods.

------------------------------------------------------------------------

## Source Mapping (Design -\> Code)

Use this section to trace design decisions to implementation points.

### Class construction and validation

-   File: `R/projection_class.r`
-   Functions:
    -   `new_poparray_projection()`
    -   `validate_poparray_projection()`
    -   `poparray_projection()`

### Accessors and uncertainty semantics

-   File: `R/projection_class.r`
-   Functions:
    -   `projection()`
    -   `std_error()`
    -   `confint.poparray_projection()`

### Subsetting and coercion behavior

-   File: `R/projection_class.r`
-   Functions:
    -   `[.poparray_projection`
    -   `as.poparray.poparray_projection()`
    -   `as.data.frame.poparray_projection()`
    -   `as_tibble.poparray_projection()`

### Projection engine orchestration and writer integration

-   File: `R/projection.r`
-   Functions:
    -   `project()`
    -   `project_cube()`
    -   `run_projection_engine()`
    -   `tp_projection_hdf5_writer()`

### Plotting behavior

-   File: `R/projection_plot.r`
-   Functions:
    -   `plot.poparray_projection()`
    -   `plot_ts_pop_projection()`
    -   `plot_pyramid_pop_projection()`

### Poparray interoperability

-   File: `R/poparray_class.r`
-   Relevant APIs:
    -   `new_poparray()`
    -   `[.poparray`
    -   role access patterns used by projection workflows (`dimroles`, time/area semantics)
