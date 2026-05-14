---
name: tarr-pop-semantics-guard
description: Enforce dimensional semantics, overlap safety, and epidemiologic correctness for poparray operations and aggregations.
---

# Purpose

Protect the package from incorrect results by enforcing:

-   dimensional semantics
-   overlap safety
-   valid aggregation rules
-   correct age interval handling

This skill is responsible for guarding reductions and other operations that depend on semantic correctness.

Apply `tarr-pop-repo-guard` first for shared repository constraints. Use
`tarr-pop-repo-guard/references/repo-guidelines.md` as the source of truth for
repo-wide laziness, dimensional, style, testing, and documentation rules.

------------------------------------------------------------------------

# Use this skill when

-   The task involves:
    -   overlap or double-counting risk
    -   aggregation safety
    -   `sum()`, `summary()`, or semantic reductions
    -   age interval logic
    -   `DimSemantics`
    -   set, partition, or interval semantics
-   The user asks:
    -   “Can this be safely summed?”
    -   “Why is this still unsafe after filtering?”
    -   “How should overlapping categories behave?”

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

-   Which dimension semantics are involved
-   Whether the issue is intrinsic semantics or current active-label safety

## 2. Recommended lazy approach

-   Explain how to validate semantics without realizing the full cube
-   Recompute overlap safety from the active state where needed

## 3. Base R alternative

-   Mark clearly as **EAGER** if it realizes data
-   Explain that semantic checks still apply even in base R workflows

## 4. Tidyverse alternative

-   Warn against row-based summarization that skips semantic validation
-   Mark **EAGER** if coercion is required

## 5. Tests to add (testthat)

-   Cover:
    -   unsafe sums fail or warn
    -   safe sums succeed after filtering
    -   age intervals are interpreted correctly
    -   stale overlap flags are not blindly trusted
    -   mixed-dimension cases behave correctly

## 6. Roxygen notes (if exported)

-   Document:
    -   overlap safety behavior
    -   when errors or warnings occur
    -   how semantics are determined
-   Include examples of unsafe and safe aggregation

------------------------------------------------------------------------

# Anti-patterns to flag

-   summing overlapping categories without checks
-   treating age labels as plain strings
-   relying only on stored overlap flags when active labels have changed
-   assuming a dimension remains unsafe after overlap categories are removed
-   bypassing semantic validation in summary methods

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("DelayedArray")
getNamespaceVersion("HDF5Array")
```

------------------------------------------------------------------------

# Tone and behavior

-   Be precise, conservative, and safety-first
-   Prioritize correctness over convenience
-   Explain why an operation is unsafe, not just that it fails
-   Prefer minimal semantic guard logic that is easy to test

------------------------------------------------------------------------

# Notes for this skill

-   Distinguish intrinsic semantics from currently safe active labels
-   For age, use interval semantics such as:
-   "0-4" → [0, 4]
-   "85+" → [85, Inf)
-   Remaining categories after filtering may become safe even if the original dimension contained overlaps
