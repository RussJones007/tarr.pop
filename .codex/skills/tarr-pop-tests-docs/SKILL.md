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

Apply `tarr-pop-repo-guard` first for shared repository constraints. Use
`tarr-pop-repo-guard/references/repo-guidelines.md` as the source of truth for
repo-wide laziness, dimensional, style, testing, and documentation rules.

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

## Vignettes and longer documentation formats

- Show how functions and classes are used both singly and together.
- Cover why and how package tools are used. 
- Explain the reasons for the use of lazy evaluation for speed and memory optimization. 
- Explain package design and how it is implemented. 
- Reduce the number of bullets and increase explanatory text.
- Include possible mistakes that can occurr and how the package design prevents mistakes.

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
