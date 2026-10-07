# Phase 1 report

## Root cause

The reported stale-overlap error is **not reproducible in the current checkout**.
Before changes, strict summation already succeeded after removing `85+` with
exact membership filtering or `[` while keeping multiple detailed groups. It
also succeeded when retaining `80-84` and `85+` without the detailed groups.
The exact requested expression, `dplyr::filter(x, age.char != "85+")`, instead
failed at predicate parsing because `!=` was unsupported. Baseline regression
results recorded four such failures before the implementation change.

No mutable overlap flag exists in the present class. `overlap_levels` stores
intrinsic known overlap-causing labels and is intentionally preserved through
subsetting. It is not a cache of current overlap presence. This investigation
cannot establish why a different installed version or data object might produce
the reported post-filter reduction error.

### Existing safety path inspected

- `R/dim_semantics.r`: S7 `DimSemantics`, `new_dim_semantics()`, predicates, and
  controlled updater. `partition_type` is `partition`/`set`/`unknown`;
  `scale_type` is `nominal`/`ordinal`/`interval`. True partitions cannot declare
  `overlap_levels`.
- `R/poparray_semantic_reductions.r`: `sum()` obtains current `dimnames()` and
  calls `pa_dim_has_overlap_risk()`. Partitions are trusted; zero/single-level
  dimensions are safe. Interval dimensions use `pa_has_interval_overlap()` and
  the existing `tp_age_bounds()` parser. Other set/unknown dimensions intersect
  current labels with declared `overlap_levels`, or remain conservative when
  no overlap-causing levels are known. Strict failures precede value reduction;
  `strict = FALSE` warns, and `allow_overlap = TRUE` explicitly bypasses the guard.
- `R/filter.R`: predicate parser/evaluator creates label indices, then delegates
  to `[`. `tp_age_bounds()` represents `85+` as `[85, Inf)`, `85-89` as `[85, 90)`,
  and single ages as half-open intervals. Overlap detection compares interval
  bounds, not label substrings.
- `R/poparray_class.r`: `[` validates metadata, slices the delayed backend, and
  computes new labels from indices. `wrap_subset_poparray()` calls
  `subset_dim_semantics()` to retain semantic entries for remaining dimensions.
  Roles, provenance, data column, and the dimension-name cache are preserved.
  `validate_poparray()` and S4 validity check class/metadata/role consistency,
  HDF5 metadata shape, and time ordering; they do not store overlap state.
- `R/collapse.R`: `pa_check_collapse_semantics()` calls the same overlap helper
  on current labels within each output group. Grouped reductions subsequently
  run blockwise into an HDF5 sink. `group_ages()` and
  `group_array_by_levels()` in `R/group_ages.R` delegate to this path.
- `summary.poparray()` uses delayed summaries; `sd()` uses block reduction.
  Neither declares a separate `strict`/`allow_overlap` contract. These APIs were
  inspected and their behavior was not changed.
- Existing overlap, nominal-set, interval, subsetting, metadata, collapse,
  filtering, and age-grouping tests were inspected before implementation.

## Implementation

Added `!=` predicates for categorical labels, exact age labels, and numeric time
labels. Categorical and age exclusions retain existing strict unknown-label
validation. Boolean composition and delayed slicing use the existing paths.
The shared overlap guard, intrinsic metadata contract, constructors, and public
argument names were unchanged. No architectural change was necessary.

Roxygen documentation for filtering and `sum(poparray)` now states:

> Overlap safety is evaluated from the dimension levels currently present in the
> `poparray`. Removing overlapping levels by subsetting can therefore remove
> overlap risk.

Documentation was regenerated. Roxygen skipped three existing manually maintained
pages (`roles.Rd`, `source_meta.Rd`, `cube_metadata.Rd`); the two changed generated
pages were updated successfully.

## Regression tests added

`test-current-overlap.R` covers genuine older-age overlap; strict errors, warning
and explicit override behavior; removing the broad category; retaining the broad
category without details; partial removal that remains unsafe; unrelated year
and area filtering; named and positional `[`; semantic and role/provenance
preservation; existing nominal race-overlap semantics; safe and unsafe collapse
and age grouping; and HDF5 extraction instrumentation. `test-filter.R` adds
exclusion composition and strict/lenient unknown-label checks.

## Full test results

Focused overlap/semantics/filter/collapse/age-grouping/class tests passed.
The full testthat suite passed: **163 tests, 625 expectations; zero failures,
errors, warnings, or skipped tests**. Commands used:

```r
devtools::test(
  filter = "current-overlap|dim-semantics|dim_semantics_s7|filter|collapse|group-ages|poparray_classr",
  reporter = "summary", stop_on_failure = FALSE
)
devtools::test(reporter = "summary", stop_on_failure = FALSE)
```

`TZ=UTC` and `TARR_POP_SKIP_CUBE_SETUP=true` isolated package startup during
verification. No population cube in the user's storage was changed.

## R CMD check

Ran `devtools::check(document = FALSE, manual = FALSE, error_on = "never")`
with vignettes enabled and `--as-cran` checks. The first build attempt failed
because Quarto could not open its Sass cache database in the sandbox. Retrying
with `XDG_CACHE_HOME` and `DENO_DIR` under `/tmp` resolved that build failure.
The retry built vignettes and installed the package successfully.

**Final check status: 3 errors, 9 warnings, 5 notes.** The package check did
not pass; these findings are not masked by the passing source test suite.

Errors found in unchanged code/documentation:

- `dim_labels` examples refer to an undefined `census` object.
- Packaged tests unconditionally source `../../data-raw/tdc_estimates_support.r`;
  `data-raw` is excluded by the existing `.Rbuildignore`. Packaged results:
  **546 passes, one failure, one warning, seven skipped tests**. The warning is
  the missing source file; the seven skips are TDC transformation tests that
  explicitly require a source-repository script.
- Rebuilding `Introduction-to-tarr_pop.qmd` failed because `codetools` was not
  available in the vignette check environment during the initial knitr setup
  chunk. Other vignette rebuilds completed.

Warning categories observed:

- Nonportable existing filenames and long cached-vignette paths.
- Non-ASCII characters in unchanged projection source.
- Undeclared `IRanges`/`S4Arrays` imports, missing/unexported `cli`/`DelayedArray`
  references, and use of internal DelayedArray calls.
- Existing `as.data.frame.poparray` and `confint.poparray_projection` signature
  mismatches.
- Replacement `data_col<-` uses `values` rather than `value`.
- Missing documentation for existing objects/data/S4 methods.
- `data_col<-` code/documentation disagreement.
- Undocumented or mismatched arguments in existing Rd usage sections.
- `withr` is used by existing tests and the new laziness test but is not declared
  in Suggests. Restricted-network repository-index lookups also emitted warnings.

Notes observed: inclusion of `.codex`, unverifiable remote clock, nonstandard
top-level files (including the supplied instruction file and existing `notes`
directory), unresolved globals/imports, and undeclared vignette dependencies
(`devtools`, `forcats`, `janitor`, `readr`, `tarr`, `tidyverse`). Manual/PDF checking
was disabled. These package-wide issues were reported rather than folded into
Phase 1. Full logs are in `/tmp/tarr-pop-phase1-check-retry.log` and
`/tmp/tarr-pop-phase1-check/tarr.pop.Rcheck/00check.log`.

## Laziness

The public HDF5 seed `extract_array()` method was instrumented. Filtering,
subsetting, overlap queries, and blocked strict reductions returned **zero
population values** from the backend. Validity can make zero-length extraction
probes; those are not population reads. The test also confirms that a successful
sum does extract values, proving the instrumentation is active. Subsets retain
HDF5-backed DelayedArray storage. Summation still scans the selected data to
produce its scalar; collapse/grouping retain their existing blockwise HDF5
processing rather than realizing the entire cube in memory.

Verified locally with DelayedArray 0.36.0 and HDF5Array 1.38.0.

## API argument audit

See [the full inventory](phase_1_api_argument_audit.md). Established generic
formals are retained (`x`, `.data`, `object`, `data`). Future population-specific
verb/accessor candidates prefer `pop`; polymorphic object/path and array helpers
keep descriptive or generic names. All proposed renames identify named-call
breakage. `collapse_dim` would require a generic-wide compatibility migration.
No names were changed, and there is no recommendation to standardize on `pa`.

## Files changed

- `R/filter.R`
- `R/poparray_semantic_reductions.r` (documentation only)
- `man/filter.poparray.Rd` (regenerated)
- `man/sum-poparray-method.Rd` (regenerated)
- `tests/testthat/test-current-overlap.R` (new)
- `tests/testthat/test-filter.R`
- `notes/phase_1_api_argument_audit.md` (new)
- `notes/phase_1_report.md` (new)

The supplied untracked `instruction_phase_1.md` was read and left unchanged.

## Items deferred to Phase 2

Time-varying applicability/schema rules, the 2017 TDC transition, Asian/Other
applicability, deriving `85+`/`90+`, and structural-NA policy remain outside this
phase. Unknown non-age sets remain conservative without usable intrinsic overlap
information. An actual cube/version showing a post-filter false positive is still
needed to diagnose the originally reported stale-state behavior. Compatibility
migrations, the audit's unrelated documentation mismatches, and the package-check
example/packaging/dependency issues are separate follow-up work. Phase 2 was not started.
