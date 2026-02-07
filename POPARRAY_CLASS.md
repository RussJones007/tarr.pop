---
editor_options: 
  markdown: 
    wrap: 120
---

# poparray: A Lazy, Role-Aware Population Array

## Overview

A **`poparray`** object represents population counts indexed over **time** and **area**, optionally stratified by
additional dimensions such as age, sex, race, household size, or economic characteristics.

The class is designed for **large, multi-dimensional population data**, with an emphasis on:

-   scalability (HDF5-backed storage),
-   delayed computation,
-   explicit dimensional contracts, and
-   clear separation between numeric data and semantic metadata.

`poparray` is intended for epidemiologic analysis, demographic reporting, and population projection workflows where
correctness, traceability, and memory safety are critical.

------------------------------------------------------------------------------------------------------------------------

## Core design principles

The design of `poparray` is governed by the following principles:

1.  **Time and area are structural invariants**\
    Every `poparray` must include:

    -   exactly one **time dimension** (e.g. `year`), and
    -   exactly one **area dimension** (e.g. `area`, `county`, `tract`).

2.  **All other dimensions are optional stratifications**\
    Additional dimensions represent orthogonal (i.e., independent or non-overlapping) population stratifications and may
    include:

    -   demographic variables (age, sex, race, ethnicity),
    -   socioeconomic variables (income bracket, education level),
    -   household characteristics (household size, family type),
    -   or other analyst-defined groupings.

3.  **Only independent indexing axes are dimensions**\
    Hierarchical geography (e.g. tract → county → state) is **not** encoded as multiple array dimensions.\
    Instead, only one geographic resolution is represented in the array at any time.

4.  **Hierarchy and meaning live in metadata, not in the array**\
    Provenance and geographical hierarchies are stored as attributes, not as dimensions or additional array axes.
    Geographic hierarchic relationships, if available, are supported via a look-up table. Fine (smaller) resolution can
    be collapsed into coarser (larger) geographies, e.g., block -\> tract -\> county -\> state.

5.  **Delayed computation by default**\
    Operations preserve 'laziness' whenever possible and avoid realizing large arrays in memory.

-----

## What a poparray object contains

A `poparray` object is an S3 wrapper around a multi-dimensional array (often HDF5-backed) plus semantic metadata.

### Numeric data

-   Stores its numeric data as a DelayedArray, typically backed by an HDF5 file via HDF5Array.
-   Dimension names (`dimnames`) are always present and meaningful.
-   Chunking and on-disk storage are handled by the HDF5Array infrastructure.

### Required dimensions

Every `poparray` includes:

| Role | Description                                                              |
|------|--------------------------------------------------------------------------|
| Time | An ordered time dimension (e.g. `year`)                                  |
| Area | A single geographic indexing dimension (e.g. `block`, `tract`, `county`) |

Dimension order is not significant; semantic roles are determined by metadata.

### Optional stratification dimensions

Any number of additional dimensions may be present, representing orthogonal population stratifications.\
These dimensions:

-   are independent of time and area,
-   are not hierarchical with each other,
-   may be collapsed or retained depending on analysis needs.

------------------------------------------------------------------------------------------------------------------------

## Attribute usage and semantic metadata

`poparray` relies heavily on attributes to store meaning that should not be encoded structurally in the array. 
### dim_roles

A named list describing the semantic role of each dimension:

\`\`\`r attr(x, "dim_roles") \<- list( time = "year", area = "block", strata = c("age", "sex") )

\`attr(x, "dim_roles") \<- list(\
time = "year",\
area = "block",\
strata = c("age", "sex") )'

This allows methods to operate based on role, rather than hard-coded dimension names or positions.

### area_lookup

A data frame describing hierarchical geographic relationships for the area dimension. Example:

attr(x, "area_lookup") \<- data.frame(\
block = "...",\
tract = "...",\
county = "...", state = "TX" )

This table:

-   has one row per area level in the array,
-   encodes many-to-one relationships,
-   is used for aggregation and reconciliation,
-   is never treated as an array dimension.

### Source and provenance metadata

Attributes such as:

-   source
-   series_spec
-   release year
-   Census vintage
-   differential privacy notes

These document where the data came from and how it should be interpreted, without affecting array mechanics.

## Storage and persistence model

A poparray object typically consists of two files:

### HDF5 file

-   Stores the numeric array data.
-   Handles chunking, compression, and on-disk access.
-   Contains no semantic metadata beyond dimnames.

### RDS file

-   Stores the S3 poparray wrapper.
-   Contains all attributes, including lookup tables and provenance.
-   References (but does not embed) the HDF5 file.

This separation allows:

-   multiple derived views of the same numeric data,
-   safe sharing across users,
-   metadata evolution without rewriting large arrays.

## Supported operations

Operations may return realized R objects only when explicitly documented (e.g., summaries, plots). Functions that
realize the array are tagged with **Eager**.

### Basic array interface

These operations behave like the corresponding base R generics and are expected to be cheap (metadata-only)
wherepossible.

-   `length(x)` Returns the total number of cells (`prod(dim(x))`).

-   `dim(x)`Returns the array dimensions (delegates to the delayed backend).

-   `dimnames(x)` and `names(x)`Returns dimension names and per-dimension labels (stored/maintained explicitly).

### Printing

-   `print(x)` Prints a compact summary including class, source metadata (if present), total length, dimensions, and the
    configured value-column name used by tabular coercions.

### Subsetting ([) or filter()

-   Preserves laziness.
-   Maintains dimensional integrity.
-   Updates metadata consistently.
-   Defaults to drop = FALSE.
-   sulting object is a poparray.

### Collapsing dimensions

-   Stratification dimensions may be collapsed freely.
-   Area and time dimensions may be collapsed only explicitly, with clear intent.
-   Area collapsing uses lookup tables and replaces (not adds) the area dimension.
-   Geographic aggregation Fine-to-coarse aggregation (e.g. block → county) is supported via lookup-driven collapsing.
-   The resulting object remains a valid poparray.

### Coercion to tabular formats (EAGER)

These operations typically **realize** the selected data to construct a `data.frame`/`tibble`. For large cubes, slice
first (e.g., subset years/areas) before converting.

-   `as.data.frame(x)` (**EAGER**) Produces a long data frame via `as.table()` semantics with one row per cell.

-   `tibble::as_tibble(x)` (**EAGER**) Produces a tibble, typically by delegating through `as.data.frame()` for
    consistency.

### Plotting

-   plot(x, ...) A base R plot method may be provided (e.g., population pyramid panels). Plotting typically requires
    tabular data and may realize the subset being plotted.
-   ggplot2::autoplot(x, ...) Where implemented, returns a ggplot object (often faceted) and commonly relies on
    converting to a data frame first.

### Splitting

-   split(x, f, drop = FALSE) Splits a poparray by the values of a single dimension f (e.g., split by year or area).
    Returns a named list of poparray objects (or delayed arrays if drop = TRUE causes dimension dropping).

### Summary and numeric operations (EAGER by default)

-   summary(x) / sum(x) / min(x) / max(x) / range(x) / etc. Summary-group generics are supported in some by coercing
    values to numeric, which can be EAGER for large arrays.

Recommendation: summary operations should prefer delayed reductions when feasible (e.g.,DelayedArray::DelayedReduce) and
only realize small results. If a method realizes, it must be explicit in documentation and (ideally) warn for large
objects.

### Benchmark reconciliation (optional)

When fine-level data are subject to differential privacy noise:

-   Aggregated totals may differ from authoritative higher-level totals.
-   Reconciliation methods (e.g. benchmarking or raking) may be applied explicitly.
-   Provenance of reconciliation is recorded in metadata.

Plotting and summaries - Methods may impose constraints on remaining stratification dimensions. - All plotting and
summaries respect dimension roles and metadata.

### What poparray deliberately does not do

-   It does not encode hierarchical geography as multiple dimensions.
-   It does not assume a fixed set of demographic variables.
-   It does not silently reconcile or adjust data.
-   It does not force realization of large arrays without warning.

### Intended use cases

-   Census and intercensal population analysis
-   Epidemiologic rate calculations
-   Limited population projections
-   Small-area estimation workflows
-   Shared, reproducible population cubes across teams

### Conceptual summary

A poparray is a lazy, role-aware population cube\
where arrays store values,\
metadata stores meaning,\
and hierarchy is expressed through lookup tables, not dimensions
