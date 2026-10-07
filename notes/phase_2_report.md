# Phase 2 report

## Phase 2 representation

`DimSemantics@applicability` is optional, with an S7 `NULL`/list union whose
default is `NULL`. The representation is the requested small list, for example:

```r
list(
  by = "year",
  schemas = list(
    list(from = NULL, through = "2001", levels = c("80-84", "85+")),
    list(from = "2002", through = NULL,
         levels = c("80-84", "85-89", "90-94", "95+"))
  )
)
```

These are synthetic test periods, not a source-specific transition. Ranges are
inclusive, matched against the controlling dimension's label **positions**.
NULL endpoints mean the beginning/end of its current domain. `dimnames()` can
retain the union, while applicability records simultaneous levels. Source
narratives remain provenance. No exported accessor or argument rename was added;
inspect it through `dim_semantics(pop)[[dimension]]@applicability`.

## Files inspected

Implementation paths inspected before editing:

- `R/dim_semantics.r`: S7 class, constructor, predicates, controlled updater.
- `R/poparray_class.r`: constructors, S4 validity, `validate_poparray()`, semantic
  accessors/replacement, `subset_dim_semantics()`, subset wrapping, and `[`.
- `R/filter.R`: predicate evaluation and delegation to lazy `[`.
- `R/poparray_semantic_reductions.r`: interval overlap and strict `sum()` guard.
- `R/collapse.R`: grouped overlap guard, numerical reduction, output metadata.
- `R/group_ages.R`: age/group wrappers around collapse.
- `R/cube_io.R`: fieldwise semantic metadata, canonical writing, `save_poparray()`.
- `R/open_pop_array.r`: metadata inventory/cache, semantic reconstruction,
  registered-series opening, and backward compatibility.
- `R/cube_metadata_admin.R`: path-based semantic and bundled metadata updates.
- `R/ingestion.R`: constructor validation that also needs current label context.
- S7, semantics, current-overlap, filtering, collapse, group-age, class, opening,
  and metadata-admin tests; existing manual accessor documentation and Phase 1
  reports. Local S7 class-union/property behavior was verified before choosing
  the nullable field.

The existing fieldwise HDF5 design accommodates this additive representation
without a schema engine, population rewrite, or new public API.

## Implementation

Added internal metadata helpers in `R/dim_applicability.R` for structural checks,
range resolution, contextual validity, and lazy-subset trimming. Constructors,
S4 validity, controlled semantic replacement, ingestion validation, and bundled
metadata administration pass current dimension labels to these helpers.

`sum()` and grouped overlap guards use schema contexts. Applicability-free
objects still call the original intrinsic/current-label overlap logic.

Roxygen docs describe the union, per-period levels, provenance separation,
inspection, legacy behavior, and the lack of numerical harmonization. Generated
pages were rebuilt. Roxygen still skips the three existing manual accessor pages;
`cube_metadata.Rd` was updated manually to match its source documentation and
verified with `tools::checkRd()`.

## Validity rules

Validation uses only small metadata lists/vectors:

- `by` must be one non-empty name of another existing dimension.
- Controlling labels must be unique, non-missing, and non-empty.
- Applicability and schema fields must have the specified structure; arbitrary
  expression/relationship fields are rejected.
- Schema levels must be unique, non-missing character labels in the current
  target dimension. Empty levels are valid after subsetting.
- Non-NULL boundaries must exist in the controlling dimension; ranges must be
  ordered and disjoint, with every current controlling label covered once.
- Within an explicitly declared interval partition, each current schema must
  have no interval overlap; unresolved multi-label intervals are rejected.
  Set/unknown schemas can
  represent overlap, but strict reductions reject it. This retains the existing
  distinction between valid metadata and an unsafe reduction.

Empty controlling subsets retain applicability with an empty schema list; they
have no current periods requiring coverage. Empty target subsets retain covered
periods with empty applicable levels. No population-value validity scan occurs.

## Overlap behavior

Union labels in mutually exclusive periods no longer trigger false overlap.
The guard intersects current reduction labels with each schema's levels and uses
the existing half-open age interval parser. Genuine within-schema overlap still
errors by default; `strict = FALSE` warns, and `allow_overlap = TRUE` retains its
explicit override. Unknown nominal set overlap remains conservative. Intrinsic
`overlap_levels` is preserved rather than rewritten as a current-state flag.

Safety changes do **not** mask cells, sum only applicable values, alter `na.rm`,
or implement structural-NA handling. A successful scalar sum still scans its
selected numeric data using the existing delayed reduction.

## Subsetting behavior

Both `[` and `filter()` retain the existing delayed backend. Target filtering
intersects schema levels. Controlling filtering removes irrelevant schemas and
clips boundaries into the current domain. Controlling-label reordering splits
interleaved contexts into contiguous runs so ranges remain correct in current
label order. Unrelated subsetting preserves applicability exactly.

Dropping a controller with `drop = TRUE` returns the raw delayed backend if its
remaining target cannot be interpreted as a poparray. No applicability is
silently discarded to make a subset appear safe.

`collapse_dim()` checks simultaneous schema overlap. Identity mappings can trim
metadata and rename dimensions/controllers consistently. Changing labels of an
applicability target/controller stops before value reduction with a clear
schema-aware-grouping error. That protection prevents accidentally implementing
Phase 3 numerical harmonization or leaving dangling schema references.

## Persistence / backward compatibility

Optional HDF5 fields live under
`cube/metadata/dim_semantics/<dimension>/applicability/`:

- `by` and `n_schemas` datasets;
- `schemas/<index>/from`, `through`, and `levels` datasets in list order.

NULL endpoints are zero-length character datasets, following the existing
fieldwise metadata convention. A missing applicability group normalizes to NULL.
Existing semantic fields and population datasets do not need migration or
rewriting. `save_poparray()` and the existing registry-based `open_poparray()`
round-trip applicability; no file-path opening API was invented.

Path-based `dim_semantics<-` and `cube_metadata<-` validate references before
writing metadata, retain metadata-admin permission checks, and invalidate the
existing metadata cache. Failed applicability validation leaves existing
metadata intact. Removing applicability restores the absent-state representation.

## Laziness

Instrumented the public HDF5 seed `extract_array()` method using the Phase 1
pattern, counting returned population values rather than empty validity probes.
Inspection, validity, overlap checks, filtering, indexing, metadata-only path
updates, and rejected schema grouping extracted **zero population values**.
Subsets retain HDF5-backed DelayedArray storage. A test blocks the canonical cube
writer during metadata updates and verifies population values are unchanged.

Metadata helpers use base R lists/vectors; that is not eager with respect to the
population cube. Base `[` and dplyr filtering share the same lazy path. No
population conversion to arrays, matrices, data frames, or tibbles was added.

## Regression tests added

`tests/testthat/test-dim-applicability.R` adds 19 tests and 128 expectations:
optional/default S7 construction; malformed/duplicate levels; unknown dimensions,
levels, and boundaries; reversed/overlapping/uncovered ranges; constructor,
replacement, S4 validity, and reduction validation; safe cross-period intervals;
unsafe within-schema intervals and overrides; unchanged NA propagation;
early/late/transition subsets; target and unrelated filtering; nonnumeric label
positions and interleaved ordering; dropped controllers; conservative nominal
schemas; canonical HDF5 round-trip; legacy files; metadata-update validation and
unchanged values; zero-value extraction; interval partition validity; empty
subsets; identity collapse/controller renaming; and removal of unsafe periods.

All Phase 1 regressions remain in the suites.

## Focused test results

**103 tests, 422 expectations passed; zero failures, errors, warnings, or skips.**

```r
devtools::test(
  filter = "dim-applicability|current-overlap|dim-semantics|dim_semantics_s7|filter|collapse|group-ages|poparray_classr|cube-metadata-admin|open_pop_array",
  reporter = "summary", stop_on_failure = FALSE
)
```

## Full source test results

**182 tests, 753 expectations passed; zero failures, errors, warnings, or skips.**

```r
devtools::test(reporter = "summary", stop_on_failure = FALSE)
```

Both suites used `TZ=UTC` and `TARR_POP_SKIP_CUBE_SETUP=true` to isolate startup.

## R CMD check

**3 errors, 9 warnings, 5 notes**, matching Phase 1. The package check does
**not** pass. Comparing the actual diagnostic sections found no new Phase 2
findings: only the additional passing tests and removal of the `dim_semantics<-`
undocumented-`value` complaint differ from the Phase 1 diagnostics.

Errors are the same undefined `census` example, excluded `data-raw` support-script
source in packaged tests, and `codetools` unavailable while rebuilding the
introduction vignette. Packaged tests recorded **673 passes, one failure, one
warning, seven skips**; the failure/warning concern the excluded script and the
skips are source-only TDC transformation tests. The check build preceded the
final one-expectation controller-drop test strengthening and final documentation
clarifications; the final source suites above cover that strengthening. Runtime
implementation code was unchanged after the check build began.

Ran with vignettes enabled, manual/PDF checking disabled, and writable Quarto
caches:

```r
devtools::check(document = FALSE, manual = FALSE,
                check_dir = "/tmp/tarr-pop-phase2-check", error_on = "never")
```

The command uses `--as-cran`. Logs:
`/tmp/tarr-pop-phase2-check.log` and
`/tmp/tarr-pop-phase2-check/tarr.pop.Rcheck/00check.log`.

## Files changed

Phase 2 source files:

- `R/dim_applicability.R` (new)
- `R/dim_semantics.r`
- `R/poparray_class.r`
- `R/poparray_semantic_reductions.r`
- `R/collapse.R`
- `R/filter.R` (Phase 2 adds documentation; Phase 1 predicate changes retained)
- `R/cube_io.R`
- `R/open_pop_array.r`
- `R/cube_metadata_admin.R`
- `R/ingestion.R`

Phase 2 tests:

- `tests/testthat/test-dim-applicability.R` (new)

Phase 2 documentation/report:

- `man/DimSemantics.Rd`
- `man/new_dim_semantics.Rd`
- `man/validate_dim_semantics.Rd`
- `man/poparray.Rd`
- `man/subset-poparray.Rd`
- `man/dim_semantics.Rd`
- `man/cube_metadata.Rd` (manual page)
- `man/sum-poparray-method.Rd`
- `man/collapse_dim.Rd`
- `man/filter.poparray.Rd`
- `man/save_poparray.Rd`
- `man/open_poparray.Rd`
- `notes/phase_2_report.md` (new)

Previously uncommitted Phase 1 source, tests, generated docs, and reports were
preserved. `instruction_phase_1.md` and `notes/instruction_phase_2.md` were read
and left unchanged.

## Pre-existing issues observed

The completed diagnostic comparison matches the Phase 1 baseline (3 errors,
9 warnings, 5 notes). All error/warning/note categories and their substantive
messages are unchanged, except the corrected `dim_semantics<-` parameter
documentation and the larger count of passing tests.

- Errors: undefined `census` in `dim_labels` examples; packaged tests sourcing an
  excluded `data-raw` script; missing `codetools` during introduction-vignette
  rebuilding.
- Warnings: nonportable filenames/long cache paths; non-ASCII projection source;
  existing undeclared imports and missing/unexported DelayedArray/cli references;
  S3 signature mismatches; nonstandard replacement argument `values`; missing
  documentation; code/Rd mismatches; other undocumented usage arguments; and
  undeclared test dependency `withr`. Repository-index lookups also failed under
  restricted networking.
- Notes: included `.codex`; unverifiable remote clock; nonstandard top-level
  files/directories; unresolved existing globals/imports; and undeclared vignette
  dependencies.

No unrelated cleanup or API migration was made.

## Items deferred to Phase 3

Schema-dependent age harmonization/derivation of `85+` or `90+`, structural-NA-aware
numerical reduction, source-specific transition/race metadata, source-label
normalization, and any general relationship/predicate engine remain deferred.
Changing labels of applicability targets/controllers during grouped numerical
reduction needs the Phase 3 design. Broad argument renaming and unrelated package
check cleanup remain separate maintenance work. Phase 3 was not started.
