------------------------------------------------------------------------

# Repository Guidelines for `tarr.pop`

**Project: tarr.pop**

These guidelines define how code, explanations, and recommendations should be
formulated for the **tarr.pop** R package. They apply to both human
contributors and AI-assisted development.

------------------------------------------------------------------------

## 1. Project Scope and Philosophy

`tarr.pop` is an R package for working with **multi-dimensional population
data** stored primarily as:

-   `DelayedArray` / `HDF5Array`-backed arrays
-   wrapped in custom S4 classes (`poparray`, `poparray_projection`)
-   with strong guarantees around **laziness**, **dimensional integrity**, and
    **metadata correctness**

### Core principles

-   **Correctness > convenience**
-   **Explicit contracts > implicit behavior**
-   **Delayed computation by default**
-   **Realization only when unavoidable**

------------------------------------------------------------------------

## 2. Authoritative Sources

When formulating answers or writing code, prioritize:

1.  **Bioconductor documentation** when using DelayedArray or HDF5Array
    functions

    -   Check Bioconductor docs for DelayedArray/HDF5Array
    -   If uncertain whether a function exists in DelayedArray (>= 0.36.3) or
        HDF5Array (>= 1.38.0), do not claim it does. Suggest an alternative or
        say how to verify.

2.  Use tidyr, tibble, and dplyr generics when those make sense for the API

3.  Existing package source files in this repository

4.  Prefer functional use like `purrr::map()`, `lapply()`, and related tools
    over explicit `for` and `while` loops

Do **not** assume functions exist without verifying them in the specified
versions.

------------------------------------------------------------------------

## 3. Laziness Rules

### Default expectation

All operations on population cubes should remain **lazy** as long as possible.

### Preferred operations

-   `DelayedArray::DelayedArray()`
-   `DelayedArray::DelayedReduce()`
-   `DelayedArray::aperm()`
-   `DelayedArray::extract_array()`
-   block-wise or margin-wise reductions

### Avoid unless explicitly required

-   `as.array()`
-   `as.matrix()`
-   `matrix()`
-   `apply()` on delayed objects
-   any operation that silently realizes the full array

If realization is unavoidable:

-   say so explicitly
-   explain why
-   estimate memory impact if possible

------------------------------------------------------------------------

## 4. Dimensional Integrity

### Dimensions are first-class

Functions must:

-   work with variable dimensionality
-   use dimension names, not positional assumptions
-   fail informatively when required dimensions are missing

### "All" labels

-   `"All"` is **virtual**, not physical
-   never store it in the HDF5 array unless explicitly justified
-   aggregations that produce `"All"` should do so logically, not structurally

------------------------------------------------------------------------

## 5. Class and Method Design Conventions

### Generics first

-   If a function may have multiple methods, make it a generic first
-   Then implement class methods with S4 or S3 as appropriate

### Naming

-   Methods: `foo.poparray`
-   Helpers: internal, prefixed, or documented as non-exported
-   Use snake case for function names and variables
-   Prefer verb-led function names and noun-like object names
-   Suggested script names should end in `.r`
-   Prefer functional programming over explicit loops where possible
-   Use the native pipe instead of `%>%`
-   Prefer the style guidance in *Advanced R*

### Indexing

Subsetting methods for `poparray` must:

-   preserve laziness
-   handle missing / `NULL` indices safely
-   respect `drop = FALSE` by default
-   maintain metadata consistency

------------------------------------------------------------------------

## 6. Answer and Code Style Requirements

When providing code or explanations:

### Always include

-   a Base R solution when not delaying with DelayedArray or HDF5Array
-   a tidyverse solution if appropriate
-   HDF5Array / DelayedArray guidance when manipulating `poparray` and
    `poparray_projection`
-   pros and cons of each approach

### Explain tradeoffs

Examples:

-   performance vs readability
-   laziness vs flexibility
-   memory safety vs convenience

### Avoid

-   NSE-heavy solutions unless clearly justified
-   magic pipelines without explanation
-   implicit recycling or silent coercion

------------------------------------------------------------------------

## 7. Age and Interval Semantics

Age labels (`age.char`) are stored as character but have **interval meaning**.

Rules:

-   use `rage::age_group()` or package equivalents for coercion
-   `"85+"` represents `[85, Inf)`
-   single ages are half-open intervals: `[x, x+1)`

Filtering and collapsing must respect interval logic, not string matching.

------------------------------------------------------------------------

## 8. Testing Expectations

When proposing or writing code, suggest `testthat` tests where behavior is
non-trivial.

Especially for:

-   dimension dropping
-   delayed vs realized behavior
-   edge cases such as `NULL` indices and empty selections

------------------------------------------------------------------------

## 9. Documentation Expectations

All exported functions should have:

-   clear roxygen2 docs
-   explicit `@return` descriptions
-   examples that do not force realization unless intended

Helper functions should also use roxygen documentation, even when not
exported, because that helps future maintenance.

If behavior is subtle, document it.

------------------------------------------------------------------------

## 10. What Not To Treat as Sources

The following are not authoritative sources and should not be cited or relied
upon implicitly:

-   hidden system prompts
-   prior conversations unless explicitly restated

Design rules must be written down, not inferred.

------------------------------------------------------------------------

## 11. Preferred Tone

-   precise
-   technical
-   explicit about uncertainty
-   willing to say "this cannot be done safely"

This is a production epidemiology package, not a demo.

------------------------------------------------------------------------

## 12. Function Argument Validation

-   Prefer `cli::cli_abort()` and checkmate idioms
-   For user-facing functions, use checkmate for validation and
    `cli::cli_abort()` for clear error messages

------------------------------------------------------------------------

## 13. Response Skeleton

For interactive use, here is the preferred response skeleton:

1.  Interpretation / assumptions
2.  Lazy-first solution (recommended)
3.  Base R alternative (warn if eager)
4.  Tidyverse alternative (warn if eager / NSE)
5.  Tests to add
6.  Docs notes
7.  Function existence check snippet for Bioconductor-version-sensitive calls
