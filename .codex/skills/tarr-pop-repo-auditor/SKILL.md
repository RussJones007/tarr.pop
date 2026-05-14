---
name: tarr-pop-repo-auditor
description: Audit tarr_pop source code for lazy violations, semantic errors, missing methods, and incomplete migration to DelayedArray/S4 architecture.
---

# Purpose

Audit the `tarr_pop` codebase to identify:

-   violations of lazy DelayedArray principles
-   dimensional semantic errors
-   metadata inconsistencies
-   missing or incomplete methods
-   legacy patterns from earlier architectures
-   high-impact refactoring opportunities

This skill acts as a code-quality gate for the package.

Apply `tarr-pop-repo-guard` first for shared repository constraints. Use
`tarr-pop-repo-guard/references/repo-guidelines.md` as the source of truth for
repo-wide laziness, dimensional, style, testing, and documentation rules.

------------------------------------------------------------------------

# Use this skill when

-   The user asks:
    -   “audit this file”
    -   “review these scripts”
    -   “what still needs refactoring?”
    -   “what is missing?”
    -   “find problems in my code”
-   Reviewing uploaded `.R` files
-   Migrating toward DelayedArray / HDF5Array / S4 architecture
-   Preparing for release or a major refactor

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

Every audit MUST include:

## 1. Interpretation

-   What part of the codebase is being audited
-   Whether the focus is file-specific or repo-wide

## 2. Recommended lazy approach

-   Explain where delayed operations should be preserved
-   Identify opportunities to replace eager logic with delayed logic

## 3. Base R alternative

-   Mark clearly as **EAGER** where relevant
-   Use mainly to explain why current code is risky, not as the preferred design

## 4. Tidyverse alternative

-   Identify where tidyverse idioms might introduce row-wise or eager assumptions
-   Mark **EAGER** where applicable

## 5. Tests to add (testthat)

-   Recommend tests for:
    -   lazy vs EAGER behavior
    -   metadata preservation
    -   semantic safety
    -   edge cases
    -   missing method coverage

## 6. Roxygen notes (if exported)

-   Identify missing or unclear docs
-   Call out where lazy vs EAGER behavior should be documented

## 7. Findings

Group findings by category: - Lazy violations - Semantic issues - Metadata issues - Method gaps - Performance issues

For each finding include: - file + function name - problem - why it matters - suggested fix

## 8. Recommended fixes

Prioritize: - Critical - High - Medium - Low

------------------------------------------------------------------------

# Anti-patterns to flag

-   early `as.data.frame()` or `as.array()`
-   `apply()` on large arrays
-   silent dimension dropping
-   summing overlapping categories
-   ignoring `dim_semantics`
-   inconsistent metadata after operations
-   methods returning base types instead of `poparray`
-   broken or incomplete method dispatch

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("DelayedArray")
getNamespaceVersion("HDF5Array")

exists("DelayedArray", where = asNamespace("DelayedArray"))
exists("writeHDF5Array", where = asNamespace("HDF5Array"))
```

------------------------------------------------------------------------

# Tone and behavior

-   Be precise and actionable
-   Prefer minimal, high-impact fixes over large rewrites
-   Do not rewrite entire files unless asked
-   Focus on correctness, safety, and performance
-   Make the audit easy to act on

------------------------------------------------------------------------

# Notes for this skill

-   This skill should identify issues, prioritize them, and suggest fixes
-   It should not default to rewriting code unless the user explicitly asks for patches or refactors
-   It should pay special attention to incomplete migration from legacy wrapper patterns to current delayed and semantic contracts
