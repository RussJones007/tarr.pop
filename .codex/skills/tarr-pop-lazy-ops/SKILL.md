---
name: tarr-pop-lazy-ops
description: Implement or refactor operations on poparray objects while preserving DelayedArray laziness, metadata integrity, and dimensional semantics.
---

# Purpose

Implement and refactor operations on `poparray` and `poparray_projection` so they remain:

-   lazy
-   memory-efficient
-   semantically correct
-   metadata-preserving

This skill is responsible for array operations that should stay within the DelayedArray / HDF5Array model as long as possible.

Apply `tarr-pop-repo-guard` first for shared repository constraints. Use
`tarr-pop-repo-guard/references/repo-guidelines.md` as the source of truth for
repo-wide laziness, dimensional, style, testing, and documentation rules.

------------------------------------------------------------------------

# Use this skill when

-   Implementing or modifying:
    -   `filter()`
    -   `select()`
    -   `collapse_dim()`
    -   `group_dim()`
    -   subsetting with `[`
    -   delayed reductions
-   Optimizing pipelines so filtering and selection happen before coercion
-   Avoiding accidental eager realization
-   Reviewing whether an operation can remain delayed

------------------------------------------------------------------------

# Core rules (in addition to tarr-pop-repo-guard)

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

-   What operation the user wants
-   Which dimensions, classes, or verbs are involved
-   Whether the task should remain lazy

## 2. Recommended lazy approach

-   Use DelayedArray-safe operations
-   Prefer slicing, delayed transforms, and metadata-preserving methods
-   Explain how laziness is preserved

## 3. Base R alternative

-   Mark clearly as **EAGER** if it realizes data
-   Explain memory implications
-   Use only as a fallback or for small arrays

## 4. Tidyverse alternative

-   Warn about NSE and row-based assumptions
-   Make clear that `filter()` here means dimension filtering, not row filtering
-   Mark **EAGER** if coercion is required

## 5. Tests to add (testthat)

-   Cover:
    -   lazy vs EAGER behavior
    -   metadata preservation
    -   semantic safety
    -   edge cases
    -   `drop = FALSE` behavior when relevant

## 6. Roxygen notes (if exported)

-   Document:
    -   lazy vs EAGER behavior
    -   required dimensions and roles
    -   semantic assumptions
-   Include safe examples that avoid large realization

------------------------------------------------------------------------

# Anti-patterns to flag

-   early `as.data.frame()`, `as.array()`, or `as.matrix()`
-   `apply()` on large arrays
-   silent dimension dropping
-   position-based indexing instead of names
-   coercing before filtering or collapsing
-   returning a base type when a `poparray` should be preserved
-   treating array-like cubes as row-wise tibbles too early

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("DelayedArray")
getNamespaceVersion("HDF5Array")

exists("DelayedArray", where = asNamespace("DelayedArray"))
exists("abind", where = asNamespace("DelayedArray"))
```

------------------------------------------------------------------------

# Tone and behavior

-   Be precise, structured, and actionable
-   Prefer minimal, high-impact changes
-   Do not rewrite entire files unless asked
-   Prioritize laziness, metadata integrity, and semantic correctness

------------------------------------------------------------------------

# Notes for this skill

-   filter() means dimension slicing, not row filtering
-   select() means selecting dimensions to keep, not selecting columns from a data frame
-   Always prefer filtering, selecting, and collapsing before any eager coercion
-   If an invariant dimension would be removed, explain whether the object should degrade to a more general type
