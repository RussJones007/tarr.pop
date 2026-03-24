---
name: tarr-pop-projection
description: Implement and manage population projections using poparray_projection with delayed evaluation and scalable forecasting methods.
---

# Purpose

Implement and review projection workflows for `poparray_projection` while preserving:

-   delayed evaluation where feasible
-   dimensional integrity
-   metadata consistency
-   scalability across many series

This skill governs how forecasts are built, stored, and exposed in the package.

------------------------------------------------------------------------

# Use this skill when

-   Working with:
    -   `project()`
    -   `project_cube()`
    -   `poparray_projection`
-   Adding or reviewing forecasting models
-   Designing projection output structures
-   Managing uncertainty (`std_error`, intervals, or related statistics)
-   Auditing projection performance and storage

------------------------------------------------------------------------

# Core rules (ALWAYS APPLY)

-   Prefer lazy operations (DelayedArray / HDF5Array)
-   NEVER recommend realization unless explicitly labeled **EAGER**
-   Use dimension names (not positional indexing)
-   Preserve:
    -   `dimnames`
    -   roles (`time`, `area`)
    -   `dim_semantics`
-   Treat `"All"` as virtual (never physically stored)
-   Age must be treated as intervals, not strings
-   Do not allow unsafe aggregation across overlapping categories
-   Do not bypass semantic guards for convenience

------------------------------------------------------------------------

# Response requirements

Every response MUST include:

## 1. Interpretation

-   What projection task is being requested
-   Whether the problem is model choice, output design, performance, or storage

## 2. Recommended lazy approach

-   Keep the overall cube backed by delayed / HDF5 storage
-   Process series-wise rather than realizing the full cube
-   Preserve metadata and output shape

## 3. Base R alternative

-   Mark clearly as **EAGER** if it realizes data
-   Explain why full-cube realization is usually not scalable

## 4. Tidyverse alternative

-   Usually not recommended for the core projection engine
-   If mentioned, clarify that tidyverse workflows are mainly for small extracted subsets and are often **EAGER**

## 5. Tests to add (testthat)

-   Cover:
    -   projection preserves dimensions
    -   uncertainty/stat dimension is correct
    -   no unintended full realization
    -   short-series edge cases
    -   NA handling
    -   output class and metadata preservation

## 6. Roxygen notes (if exported)

-   Document:
    -   projection assumptions
    -   meaning of any `stat` dimension
    -   lazy vs EAGER boundaries
    -   model limitations
-   Include small, safe examples

------------------------------------------------------------------------

# Anti-patterns to flag

-   modeling the full cube at once
-   dropping dimensions during projection
-   mixing projections with historical data silently
-   missing or implicit uncertainty handling
-   storing projections without clear metadata
-   eager realization before per-series processing

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("DelayedArray")
getNamespaceVersion("HDF5Array")

exists("abind", where = asNamespace("DelayedArray"))
```

------------------------------------------------------------------------

# Tone and behavior

-   Be practical, scalable, and conservative
-   Prefer simple model pipelines that fit the package architecture
-   Focus on preserving class contracts and performance
-   Explain when a projection method is appropriate or not

# Notes for this skill

-   Projection should generally operate per time series, not on a fully realized cube
-   If uncertainty is stored, make it explicit in the output structure
-   Preserve input roles and dimension semantics in the projected object
-   Distinguish clearly between observed/historical values and projected values
