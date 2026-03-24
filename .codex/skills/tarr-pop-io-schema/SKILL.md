---
name: tarr-pop-io-schema
description: Design and manage HDF5-backed storage, metadata schema, and cube persistence for poparray objects using HDF5Array.
---

# Purpose

Design, validate, and maintain the HDF5-backed storage contract for `poparray` objects so cubes remain:

-   scalable
-   reproducible
-   metadata-complete
-   lazily loadable

This skill governs schema design, metadata layout, persistence, and shared cube storage strategies.

------------------------------------------------------------------------

# Use this skill when

-   Working with:
    -   `open_poparray()`
    -   `save_poparray()`
    -   `create_poparray()`
-   Designing HDF5 schema
-   Storing one or more related cubes in a file
-   Managing metadata consistency and round-tripping
-   Implementing user-configurable or shared storage paths
-   Reviewing on-disk compatibility and migration

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

-   What storage or schema problem the user is solving
-   Whether the issue is file layout, metadata, loading, saving, or shared storage

## 2. Recommended lazy approach

-   Use HDF5Array-backed storage
-   Load cubes as delayed objects
-   Validate metadata before constructing objects
-   Preserve round-trip consistency

## 3. Base R alternative

-   Mark clearly as **EAGER** if it reads the full dataset into memory
-   Explain that this is only suitable for small arrays or debugging

## 4. Tidyverse alternative

-   Usually not applicable at the storage layer
-   If mentioned, clarify that tidyverse tools generally apply only after safe eager coercion

## 5. Tests to add (testthat)

-   Cover:
-   metadata round-trip integrity
-   dimension / dimname alignment
-   role preservation
-   dim_semantics preservation
-   delayed loading
-   multi-cube storage behavior if supported

## 6. Roxygen notes (if exported)

-   Document:
-   canonical schema
-   required metadata fields
-   lazy vs EAGER loading behavior
-   shared storage assumptions
-   Include small examples of opening and saving cubes

------------------------------------------------------------------------

# Anti-patterns to flag

-   eager reads during open
-   schema drift across versions
-   missing metadata fields
-   inconsistent `dim_order`
-   hardcoded paths
-   storing derived aggregates like `"All"` as physical data
-   constructing objects before metadata validation

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("HDF5Array")
getNamespaceVersion("DelayedArray")

exists("writeHDF5Array", where = asNamespace("HDF5Array"))
exists("DelayedArray", where = asNamespace("DelayedArray"))
```

------------------------------------------------------------------------

# Tone and behavior

-   Be precise, structured, and reproducibility-focused
-   Prefer stable schemas over clever ad hoc layouts
-   Keep file contracts explicit
-   Prioritize metadata correctness and delayed access

------------------------------------------------------------------------

# Notes for this skill

-   Canonical schema should clearly separate population data from metadata
-   Metadata should include at least:
-   dimension names
-   dimension order
-   roles
-   dim semantics
-   provenance/source as appropriate
-   Support shared storage patterns where multiple users can access the same cube files without unnecessary duplication
-   If multiple related cubes are stored together, explain the tradeoffs and maintain a clear contract
