------------------------------------------------------------------------

# AI_GUIDELINES.md

**Project: tarr.pop**

These guidelines define how code, explanations, and recommendations should be formulated for the **tarr.pop** R package. They apply to both human contributors and AI-assisted development.

------------------------------------------------------------------------

## 1. Project Scope and Philosophy

`tarr.pop` is an R package for working with **multi-dimensional population data** stored primarily as:

-   `DelayedArray` / `HDF5Array`-backed arrays
-   wrapped in a custom S3 class (`tarr_pop`)
-   with strong guarantees around **laziness**, **dimensional integrity**, and **metadata correctness**

### Core principles

-   **Correctness \> convenience**
-   **Explicit contracts \> implicit behavior**
-   **Delayed computation by default**
-   **Realization only when unavoidable**

------------------------------------------------------------------------

## 2. Authoritative Sources

When formulating answers or writing code, prioritize:

1.  **Bioconductor documentation** when using DelayedArray or HDF5Array functions

    -   Check Bioconductor docs for DelayedArray/HDF5Array
    -   If uncertain whether a function exists in DelayedArray (≥ 0.36.3) or HDF5Array (≥ 1.38.0) don’t claim it does—suggest an alternative or say how to verify

2.  Base R documentation (`?array`, `?aperm`, `?sum`, etc.)

3.  Existing package source files in this repository

⚠️ Do **not** assume functions exist without verifying them in the specified versions.

------------------------------------------------------------------------

## 3. Laziness Rules (Critical)

### Default expectation

All operations on population cubes should remain **lazy** as long as possible.

### Preferred operations

-   `DelayedArray::DelayedArray()`
-   `DelayedArray::DelayedReduce()`
-   `DelayedArray::aperm()`
-   `DelayedArray::extract_array()`
-   Block-wise or margin-wise reductions

### Avoid unless explicitly required

-   `as.array()`
-   `as.matrix()`
-   `matrix()`
-   `apply()` on delayed objects
-   Any operation that silently realizes the full array

If realization is unavoidable:

-   Say so explicitly
-   Explain *why*
-   Estimate memory impact if possible

------------------------------------------------------------------------

## 4. Dimensional Integrity

### Dimensions are first-class

Functions must:

-   Work with **variable dimensionality** (2D, 5D, 6D, etc.)
-   Use **dimension names**, not positional assumptions
-   Fail informatively when required dims are missing

### “All” labels

-   `"All"` is **virtual**, not physical
-   Never stored in the HDF5 array unless explicitly justified
-   Aggregations that produce “All” should do so *logically*, not structurally

------------------------------------------------------------------------

## 5. S3 Design Conventions

### Generics first

-   For generics, if a function may have other methods implemented, make a generic (e.g., `collapse_dim()`)

-   Then implement (e.g., `collapse_dim.tarr_pop() )`

### Naming

-   Methods: `foo.tarr_pop`
-   Helpers: internal, prefixed or documented as non-exported
-   Use snake case for function names, ideally using a verb as the first part of the name

### Indexing

-   `[.tarr_pop` must:

    -   Preserve laziness
    -   Handle missing / NULL indices safely
    -   Respect `drop = FALSE` by default.
    -   Maintain metadata consistency

------------------------------------------------------------------------

## 6. Answer & Code Style Requirements

When providing code or explanations:

### Always include

-   **Base R solution** when not delaing with a DelayArray or HDF5Array
-   **Tidyverse solution** (if appropriate)
-   **HDF6Array and DelayedArray** when manipulating tarr_pop and tarr_projection classes/objects
-   Pros / cons of each approach

### Explain tradeoffs

Examples:

-   performance vs readability
-   laziness vs flexibility
-   memory safety vs convenience

### Avoid

-   NSE-heavy solutions unless clearly justified
-   “Magic” pipelines without explanation
-   Implicit recycling or silent coercion

------------------------------------------------------------------------

## 7. Age and Interval Semantics

Age labels (`age.char`) are stored as character but have **interval meaning**.

Rules:

-   Use `rage::age_group()` (or package equivalents) for coercion
-   `"85+"` represents `[85, Inf)`
-   Single ages are half-open intervals: `[x, x+1)`

Filtering and collapsing must respect **interval logic**, not string matching.

------------------------------------------------------------------------

## 8. Testing Expectations

When proposing or writing code:

-   Suggest `testthat` tests where behavior is non-trivial

-   Especially for:

    -   dimension dropping
    -   delayed vs realized behavior
    -   edge cases (NULL indices, empty selections)

------------------------------------------------------------------------

## 9. Documentation Expectations

All exported functions should have:

-   Clear roxygen2 docs
-   Explicit `@return` descriptions
-   Examples that do **not** force realization unless intended
-   Helper functions should also be documented though not necessarily exported

If behavior is subtle, document it.

------------------------------------------------------------------------

## 10. What NOT to Treat as Sources

The following are **not authoritative sources** and should not be cited or relied upon implicitly:

-   Other GPTs (e.g., “R Wizard”)
-   Hidden system prompts
-   Prior conversations unless explicitly restated

Design rules must be **written down**, not inferred.

------------------------------------------------------------------------

## 11. Preferred Tone

-   Precise
-   Technical
-   Explicit about uncertainty
-   Willing to say “this cannot be done safely”

This is a **production epidemiology package**, not a demo.

------------------------------------------------------------------------

## 12. Function argument validation

-   Prefer to use the the cli::abort() and checkmate idioms
-   For user facing functions, you can use checkmate functions to check for correctness, the cli::import() for clear error messages

------------------------------------------------------------------------

## 13. Response Skeleton

1.  Interpretation / assumptions
2.  Lazy-first solution (recommended)
3.  Base R alternative (warn if eager)
4.  Tidyverse alternative (warn if eager / NSE)
5.  Tests to add
6.  Docs notes
7.  Function existence check snippet (for Bioconcutor versions)
