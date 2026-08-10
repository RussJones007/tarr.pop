---
name: tarr-pop-class-methods
description: Implement and maintain S4/S3 methods, constructors, and class behavior for poparray and poparray_projection.
---

# Purpose

Define and maintain the class contracts for `poparray` and `poparray_projection`, including:

-   constructors
-   validators
-   method implementations
-   coercions
-   consistent return behavior

This skill ensures the object system remains robust and predictable.

Apply `tarr-pop-repo-guard` first for shared repository constraints. Use
`tarr-pop-repo-guard/references/repo-guidelines.md` as the source of truth for
repo-wide laziness, dimensional, style, testing, and documentation rules.

------------------------------------------------------------------------

# Use this skill when

-   Creating or modifying:
    -   constructors
    -   validators
    -   S3 or S4 methods
    -   coercions such as `as.data.frame()` or `as_tibble()`
    -   accessors, summaries, or display methods
-   Auditing method completeness
-   Reviewing class-return behavior after transformations

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

-   What class or method behavior is being designed or repaired
-   Whether the issue is constructor logic, validation, dispatch, coercion, or return type

## 2. Recommended lazy approach

-   Keep core methods operating on delayed objects where possible
-   Preserve class identity and metadata
-   Use generics where appropriate

## 3. Base R alternative

-   Mark clearly as **EAGER** if coercion realizes data
-   Explain memory implications and when this is acceptable

## 4. Tidyverse alternative

-   Clarify where tidyverse-facing APIs are acceptable
-   Warn that user-facing convenience methods may still need delayed internals
-   Mark **EAGER** if tabular coercion is required

## 5. Tests to add (testthat)

-   Cover:
    -   constructor validation
    -   role enforcement
    -   metadata preservation
    -   correct return class
    -   coercion behavior
    -   method dispatch behavior

## 6. Roxygen notes (if exported)

-   Document:
    -   required roles and dimensions
    -   return class
    -   lazy vs EAGER behavior
    -   any downgrade behavior when invariants are removed
-   Include safe examples

------------------------------------------------------------------------

# Anti-patterns to flag

-   methods returning base arrays instead of `poparray`
-   missing validation
-   inconsistent metadata updates
-   silent coercion to data frame
-   relying on positional assumptions for core invariants
-   dispatch that breaks because of mixed S3/S4 expectations

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("DelayedArray")
getNamespaceVersion("HDF5Array")
```

------------------------------------------------------------------------

# Tone and behavior

-   Be precise, object-contract-focused, and practical
-   Prefer small, consistent method definitions over special-case sprawl
-   Favor explicit validation and informative errors
-   Keep exported API behavior predictable

------------------------------------------------------------------------

# Notes for this skill

-   Required roles like time and area should remain explicit
-   Coercions to tabular forms are typically EAGER and should be documented that way
-   If an operation cannot preserve the class safely, explain the downgrade path clearly
