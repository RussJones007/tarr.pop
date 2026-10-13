---
editor_options:
  markdown:
    wrap: 110
---

# dplyr verbs for arrays: precedents and recommendations

Reviewed 2026-10-11. This is a design review, not a performance benchmark.

Implementation update, 2026-10-13: `rename()` was already implemented;
`slice()` and `relocate()` now have poparray methods. `slice()` requires
`.dimension`, preserves all dimensions, rejects duplicate retained positions,
and keeps time order. `relocate()` permutes the delayed view with tidy-select
selectors and `.before`/`.after`. Both preserve metadata without extracting
population values. The remaining recommendations below are retained as design
context; `group_by()`/`summarise()` and the deferred verbs remain proposals.

## Findings from primary sources

| Package | Array interface | Evidence about laziness | Relevance to poparray |
|----|----|----|----|
| cubelyr | General data cubes (`tbl_cube`), with dimensions and measured arrays; `filter`, `select`, `group_by`, and `summarise` methods | No claim of HDF5-backed delayed execution established in this review | Closest general-array API precedent; useful for distinguishing dimension filtering from measure selection |
| stars | Spatial and temporal arrays; `filter`, `slice`, `rename`, `select`, `mutate`, `transmute`, and `pull`, including proxy methods | `stars_proxy` defers reads; ordinary `stars` objects can already hold values in memory. A proxy method alone does not establish bounded-memory execution for every expression | Useful precedent for dimension-aware slicing and an explicit deferred representation |
| tidync | NetCDF dimension filtering through `hyper_filter`, followed by explicit extraction | `hyper_filter` records a subset without reading variable values; `hyper_array` and `hyper_tibble` are extraction boundaries | Strong model for evaluating coordinate predicates first and postponing population reads |
| DelayedArray | General array operations rather than a dplyr backend | Operations are delayed or executed with block processing | Keep this as poparray's execution backend; borrow API ideas without converting cubes into another representation |

Sources:

-   [cubelyr author repository](https://github.com/hadley/cubelyr): extracted the experimental `tbl_cube`
    backend from dplyr.
-   [cubelyr CRAN reference index](https://rdrr.io/cran/cubelyr/api/): method inventory; generated from the
    package.
-   [stars verb reference](https://r-spatial.github.io/stars/reference/dplyr.html): ordinary and proxy method
    signatures.
-   [stars tidyverse vignette](https://r-spatial.github.io/stars/articles/stars3.html): `filter` selects
    dimension coordinates, whereas `select` selects measured attributes; `mutate` adds measured arrays.
-   [stars proxy vignette](https://r-spatial.github.io/stars/articles/stars2.html): deferred reads, subset
    reads, and eventual materialization.
-   [tidync hyper_filter documentation](https://docs.ropensci.org/tidync/reference/hyper_filter.html): lazy
    coordinate filtering and index-based predicates.
-   [tidync hyper_array documentation](https://docs.ropensci.org/tidync/reference/hyper_array.html): explicit
    data extraction and large-extraction checks.
-   [DelayedArray Bioconductor
    documentation](https://bioconductor.org/packages/release/bioc/html/DelayedArray.html): delayed operations
    and block processing.

## Recommendations for poparray

The following are design recommendations inferred from these sources and the package's existing role,
applicability, and overlap contracts.

1.  **Finish `rename` first.** Rename dimension metadata and update time/area roles, semantic names, and
    applicability controllers together. Keep domains stable so age remains an interval after renaming. No
    population reads needed.
2.  **Add `slice` next.** Require an explicit dimension, select its integer positions, and delegate to
    existing lazy `[` subsetting. Preserve singleton dimensions by default. Any optional dropping must enforce
    required roles and applicability controller dependencies, as `collapse_dim` does.
3.  **Consider `relocate`.** Reorder dimensions through delayed permutation; preserve labels, roles,
    semantics, and applicability. It offers a clearer axis-order operation than adapting row-oriented
    `arrange`.
4.  **Design `group_by` and `summarise` together later.** State which dimensions remain and which reduce, with
    applicability masking and overlap checks shared with current reducers. Scalars may read blocks
    immediately; an array-shaped result should remain deferred where the backend supports it. Block processing
    is bounded-memory computation, not the same thing as deferring computation.
5.  **Defer `select`, `mutate`, and `transmute` until their meaning is explicit.** In stars/cubelyr, selection
    concerns measures. Poparray currently has one population measure; selecting dimensions would instead
    require slicing, reduction, or safe singleton removal. Mutated population values also need a contract for
    applicability, count validity, and provenance. A wrapper that returns poparray alone does not prove a
    transformation remains lazy.
6.  **Lower priority: `arrange`, joins, and row-based helpers.** A rectangular cube has axes rather than
    independent rows. Time ordering is a validity constraint, age has interval meaning, and joins require
    aligned coordinates and applicability schemas. These need separate designs.

## What to reuse

Reuse design patterns: coordinate-only predicate evaluation from tidync, dimension/measure distinctions from
cubelyr and stars, and a visible computation boundary from proxy workflows. Implement them directly on the
existing DelayedArray/HDF5Array backend rather than adding a conversion dependency on these packages. No new
backend package is recommended by this review.

Use tidyselect for selecting or renaming dimension **names**, with controlled tidy evaluation. Avoid
converting the full cube to a tibble just to use a verb: that conversion is **EAGER** and scales with the
number of cells. For small base R arrays, `[` and `aperm` provide familiar equivalents but operate on
in-memory values and do not enforce poparray metadata contracts.

## Verification for future verbs

-   Test that metadata-only operations trigger no HDF5 value reads and leave the original cube unchanged.
-   Test lazy slicing/permutation preserves backing seeds and delayed transforms.
-   Exercise required roles, renamed age intervals, applicability controllers, empty/singleton dimensions, and
    overlap guards.
-   For reductions, test block-size independence, missing values, structural exclusions, and agreement with
    small reference arrays.
-   Document whether an operation only builds a plan, reads blocks, or realizes a whole result. Benchmark
    representative pipelines before promising speed.

## Scope and limitations

The reviewed documentation establishes API precedents and deferred-read designs. It does not establish that
all verbs are lazy on all backends. No package was installed or benchmarked, and source-level licensing review
would be needed before copying code. The recommendations concern poparray; projection-specific methods would
need their own contract.
