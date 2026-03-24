---
name: tarr-pop-tests-docs
description: Create and maintain testthat tests, roxygen2 documentation, and vignettes for tarr_pop ensuring correctness and reproducibility.
---

# Purpose

Support package reliability and usability through:

-   strong `testthat` coverage
-   clear `roxygen2` documentation
-   reproducible examples
-   vignettes that explain core concepts safely

This skill is responsible for turning package behavior into well-tested and well-documented user-facing quality.

------------------------------------------------------------------------

# Use this skill when

-   Writing or reviewing:
    -   `testthat` tests
    -   `roxygen2` docs
    -   vignettes
    -   examples
-   Auditing documentation gaps
-   Adding coverage for new or changed features
-   Clarifying lazy vs EAGER behavior in docs

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

-   What is being documented or tested
-   Which package behaviors need coverage or clarification

## 2. Recommended lazy approach

-   Ensure examples and tests respect delayed behavior where intended
-   Avoid examples that accidentally realize large cubes unless that is the point

## 3. Base R alternative

-   If documented, clearly mark as **EAGER** when it realizes data
-   Explain why it is included

## 4. Tidyverse alternative

-   If documented, explain row-wise assumptions and potential eagerness
-   Mark **EAGER** where applicable

## 5. Tests to add (testthat)

-   Recommend concrete tests for:
    -   lazy vs EAGER behavior
    -   metadata preservation
    -   semantic safety
    -   edge cases
    -   class / return-type stability

## 6. Roxygen notes (if exported)

-   Provide:
    -   `@param`
    -   `@return`
    -   lazy vs EAGER note
    -   semantic assumptions
    -   safe examples
-   Mention vignettes or related functions when useful

------------------------------------------------------------------------

# Anti-patterns to flag

-   missing tests for non-trivial behavior
-   examples that realize large data without warning
-   undocumented exported functions
-   docs that fail to mention lazy vs EAGER behavior
-   tests that only confirm implementation details rather than behavior

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
devtools::test()
devtools::document()
```

------------------------------------------------------------------------

# Tone and behavior

-   Be clear, practical, and user-facing
-   Prefer small, behavior-focused examples
-   Encourage tests that protect package contracts
-   Keep docs explicit about laziness, semantics, and invariants

------------------------------------------------------------------------

# Notes for this skill

-   Prioritize tests around:

    -   semantic safety
    -   metadata preservation
    -   delayed behavior
    -   invariant dimensions

-   Good vignette topics include:

    -   getting started
    -   dimensional semantics
    -   HDF5-backed cubes
    -   safe aggregation
    -   Examples should be small enough to run safely during package checks
