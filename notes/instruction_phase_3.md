# Codex Phase 3 — Schema-Aware Harmonization and Structural Missingness

## Purpose

Work on the `tarr.pop` R package. This is Phase 3 of the time-varying dimension-schema work.

Phase 1 established the current-label overlap contract. Phase 2 added optional time-varying `DimSemantics@applicability`, metadata-only validity, applicability-aware overlap checks, lazy subsetting, HDF5 persistence, and backward compatibility.

Phase 3 must make grouping/collapsing operations consume applicability correctly.

> Determine applicable source levels separately for each schema/context, exclude structurally inapplicable levels before numerical reduction, and continue to propagate genuine missing values from applicable source cells.

Follow `AI_GUIDELINES.md` as authoritative. Preserve DelayedArray/HDF5Array safety and do not realize the complete population cube.

## 1. Core problem

A rectangular `poparray` may contain the union of source levels across periods:

```text
70-74
75-79
80-84
85+
85-89
90-94
95+
```

with applicability:

```text
early: 70-74, 75-79, 80-84, 85+
later: 70-74, 75-79, 80-84, 85-89, 90-94, 95+
```

Cells for inapplicable levels may be `NA`. Those are structural/inapplicable, not necessarily missing observations.

For requested `70+`, use:

```text
early: 70-74 + 75-79 + 80-84 + 85+
later: 70-74 + 75-79 + 80-84 + 85-89 + 90-94 + 95+
```

A structural `NA` from later `"85+"` must not contaminate later `70+`. But if applicable `"90-94"` is genuinely `NA`, derived `70+` must remain `NA`.

This is not `na.rm = TRUE`.

## 2. Preserve the source cube

Do not manufacture values in the stored source representation. If later years contain detailed older ages but no source `"85+"`, do not fill source `"85+"` cells by summation.

```text
stored source cube   = source-faithful union representation
derived grouped cube = harmonized representation requested by user
```

## 3. Inspect before modifying

Inspect and report current implementations of:

- `collapse_dim()` generic/method;
- `group_ages()`;
- `group_array_by_levels()`;
- group normalization/mapping helpers;
- `pa_check_collapse_semantics()`;
- Phase 2 applicability helpers;
- the Phase 2 guard blocking changed applicability-controlled labels;
- blockwise HDF5 reduction and sparse mapping construction;
- `DelayedArray::aperm` use;
- HDF5 realization sink use;
- collapsed-result metadata construction;
- age parsing/interval utilities;
- relevant Phase 1/2 tests.

Before editing, explain how the current global mapping produces structural-NA contamination and how schema-aware mapping will prevent it. Do not begin with `na.rm = TRUE`.

## 4. Fundamental invariant

> Applicability determines membership in a reduction; missingness is evaluated only after membership is determined.

For each schema/context:

1. determine applicable current source levels;
2. determine which applicable levels contribute to each requested output;
3. exclude inapplicable levels;
4. reduce applicable contributors with normal missing-value propagation;
5. combine/write results using existing blockwise/HDF5-safe architecture.

```text
inapplicable NA -> excluded before reduction
applicable NA   -> participates and propagates normally
```

Never infer structural missingness from `is.na(value)` alone.

## 5. Schema-aware mapping

Extend the current single source-to-output mapping so an applicability-controlled dimension can use a different mapping per schema/context, conceptually `M_early`, `M_late`, etc.

Reuse sparse/blockwise processing where practical. Equivalent implementations are acceptable.

Requirements:

- no full-cube materialization;
- no full-cube data-frame/tibble conversion;
- continue blockwise HDF5 output where existing contract requires it;
- small metadata/mapping objects may be eager;
- avoid unnecessary duplication of large numeric blocks;
- deterministic/testable schema dispatch.

Verify uncertain DelayedArray/HDF5Array APIs before relying on them, consistent with `AI_GUIDELINES.md`.

## 6. Age grouping semantics

`group_ages()` must use interval semantics, not string matching:

```text
85+   = [85, Inf)
85-89 = [85, 90)
90-94 = [90, 95)
95+   = [95, Inf)
```

A requested age interval is derivable in a schema only when applicable source intervals provide the required exact coverage without unsafe overlap or gaps.

## 7. Required age behavior

### Harmonized `85+`

Given:

```text
early: 80-84, 85+
later: 80-84, 85-89, 90-94, 95+
```

requesting `80-84, 85+` must produce:

```text
early 85+ = source 85+
later 85+ = 85-89 + 90-94 + 95+
```

The derived result should describe harmonized `"85+"` as applicable across the represented controller span.

### Harmonized `70+`

```text
early 70+ = 70-74 + 75-79 + 80-84 + 85+
later 70+ = 70-74 + 75-79 + 80-84 + 85-89 + 90-94 + 95+
```

### Requested `90+`

```text
early: source only has 85+ -> not derivable -> NA
later: 90-94 + 95+ -> derive normally
```

Do not estimate or split early `85+`.

### Genuine missing contributor

If later values are:

```text
85-89 = 100
90-94 = NA
95+   = 20
```

derived later `85+` must be `NA`, not 120.

### Structural source NA

If later source `"85+"` is inapplicable/`NA`, while:

```text
85-89 = 100
90-94 = 40
95+   = 20
```

derived later `85+` must be 160.

## 8. Interval derivability

For interval dimensions, determine derivability from applicable source intervals for that schema.

A target is derivable only when contributors exactly cover it under half-open semantics, with no uncovered gaps, unsafe overlap, or contributors extending outside the target unless already permitted by the grouping contract.

- Exact applicable source interval: use directly.
- Several non-overlapping intervals exactly partitioning target: sum them.
- Incomplete coverage: target/context is non-derivable; do not guess.

Prefer existing `rage`/`ivs` or package interval utilities where appropriate.

## 9. Generic `collapse_dim()` behavior

Do not make `collapse_dim()` age-specific.

For explicit grouping lists, applicability determines which mapped source labels participate in each context.

Example union grouping:

```r
groups <- list(
  older = c("70-74", "75-79", "80-84", "85+",
            "85-89", "90-94", "95+")
)
```

must not sum all seven in every year.

```text
declared member + inapplicable -> exclude
declared member + applicable value NA -> propagate NA
```

`group_ages()` additionally performs interval derivability checks.

Preserve strict overlap safeguards.

## 10. Result applicability

After harmonization, `DimSemantics@applicability` must describe the result, not the source.

If source schemas differ but both become output `"85+"`, the result must not claim later `"85+"` is inapplicable.

If all periods now have identical output applicability, simplify safely. Applicability may become `NULL` if the result truly has one uniform applicability domain and that is semantically correct.

Source-schema history belongs in provenance, not operational result applicability.

## 11. Provenance

Use existing provenance/source metadata conventions to record harmonization where practical, e.g. operation, dimension, requested groups, and fact that source schemas differed.

Do not create a parallel provenance system or large per-cell mapping metadata. Applicability remains authoritative for computation.

If existing provenance cannot represent transformation history cleanly, report that rather than inventing a large API.

## 12. Race/schema generality

The implementation must generalize beyond age.

Motivating pattern:

```text
early: Asian not separately reported; included in broader source category
later: Asian separately reported
```

Do not invent numerical decomposition of an early broad category.

If requested output can be formed from applicable source categories, collapse may work. If it requires separating an unidentifiable component, it is non-derivable for that period.

Do not treat provenance such as “Asian included in Other” as permission to extract Asian numerically.

## 13. TDC/source-specific metadata

After the generic mechanism and synthetic tests work, inspect existing TDC ingestion/normalization code for authoritative evidence of:

1. old-age schema transition;
2. Asian/race schema transition.

Do not guess transition years.

If repository sources establish them, add applicability metadata during ingestion/normalization. If not, report what evidence is missing and do not hard-code a year.

Keep canonical dimension labels clean; source-label normalization belongs in existing provenance where supported.

## 14. Structural NA policy

Do not add a global `na.rm` policy.

```text
inapplicable source cell -> exclude before reduction
applicable source cell containing NA -> normal missing propagation
non-derivable target/context -> output NA
```

Zero remains a real value distinct from both structural and genuine missingness.

## 15. Non-derivable warning behavior

Recommended default:

- output `NA` for non-derivable target/context combinations;
- issue at most one informative warning per operation;
- summarize affected groups/controller periods where feasible;
- never warn per cell/year/area.

Do not silently fabricate values.

Do not overload `strict` with non-derivability unless that matches its existing documented meaning. If a new public argument seems necessary, stop and recommend it rather than adding it without review.

## 16. Preserve overlap safeguards

Within each schema/context, contributors must remain non-overlapping unless existing explicit override semantics permit otherwise.

Preserve `strict`, warning, and `allow_overlap` contracts. Applicability must not become a double-counting bypass.

## 17. Subsetting + grouping

Test compositions such as:

```r
x |> dplyr::filter(year >= ...) |> group_ages(...)
```

and base `[` followed by grouping.

Use applicability of the current object only. Do not reach back to removed historical levels.

## 18. Persistence of derived results

For HDF5-backed derived results verify:

- values;
- dimnames;
- `DimSemantics`;
- applicability;
- roles/data-column metadata;
- provenance where supported;
- save/open round-trip.

Do not rewrite the original source cube.

## 19. Performance/laziness

Preserve current blockwise HDF5 architecture.

Eager small objects are fine: schemas, interval bounds, grouping maps, sparse matrices, controller indices.

Unacceptable:

- full `as.array(pop)`;
- full `as.matrix(pop)`;
- full `as.data.frame(pop)`;
- full tibble conversion;
- loading all values to identify structural `NA`.

Bounded block realization already used by `collapse_dim()` is acceptable.

Add a laziness/memory regression using existing instrumentation.

## 20. Tests

Add at minimum:

1. Harmonized `85+`: early direct, later detailed sum.
2. Harmonized `70+`: different contributor sets by schema.
3. Structural later `85+ = NA` excluded and detailed values summed.
4. Genuine applicable detailed `NA` propagates.
5. Non-derivable `90+`: early `NA`, later derived; verify warning.
6. Exact applicable source category carried through.
7. Interval gap -> non-derivable.
8. Unsafe overlapping contributors -> strict behavior still blocks.
9. Explicit `collapse_dim()` union grouping uses applicable members per schema.
10. Result applicability describes harmonized output correctly.
11. Early-only, late-only, and cross-transition subset then group.
12. Extra area/sex dimension behaves independently.
13. HDF5 save/open harmonized result.
14. Legacy `applicability = NULL` behavior unchanged.
15. Laziness/full-cube non-realization.
16. Synthetic nominal/race applicability demonstrates generic collapse.
17. Impossible nominal decomposition does not invent an early category value.

Use synthetic schemas unless testing an established repository ingestion rule.

## 21. Documentation

Update roxygen2 docs for changed exported behavior.

Document:

- applicability-aware collapse/grouping;
- structural versus genuine missingness;
- non-derivable targets;
- age interval derivability;
- harmonized result semantics;
- legacy behavior without applicability.

For `group_ages()`, add a lightweight synthetic example if practical. Do not require external TDC data.

Regenerate documentation.

## 22. R CMD check baseline

Phase 2 baseline is:

```text
3 errors
9 warnings
5 notes
```

Known pre-existing categories include undefined `census`, excluded `data-raw` test support, vignette `codetools`, and existing import/signature/documentation/packaging findings.

Do not turn Phase 3 into package cleanup.

Run focused tests, full source tests, and `R CMD check`. Compare diagnostics with Phase 2. Any new Phase 3 diagnostic is a Phase 3 regression unless clearly justified.

## 23. API naming

Do not perform the Phase 1 argument-name migration.

Keep generic/method formals unchanged; keep `group_ages(pop, ...)`; retain current `collapse_dim()` formal; do not introduce a `pa` convention.

## 24. Out of scope

Do not implement:

- arbitrary cross-dimensional predicates;
- a general rule engine;
- numerical disaggregation;
- proportional splitting;
- missing-value imputation;
- global `na.rm = TRUE`;
- broad API argument renaming;
- unrelated package-check cleanup;
- a new public accessor unless reviewed first;
- rewriting source values to make schemas uniform.

## 25. Completion criteria

Before declaring Phase 3 complete:

1. Explain schema-aware reduction algorithm.
2. Demonstrate structural-NA exclusion.
3. Demonstrate genuine applicable-NA propagation.
4. Demonstrate harmonized `85+`.
5. Demonstrate harmonized `70+`.
6. Demonstrate non-derivable early `90+`.
7. Demonstrate generic explicit `collapse_dim()` behavior.
8. Demonstrate correct result applicability.
9. Demonstrate subsetting + grouping.
10. Demonstrate HDF5/blockwise behavior.
11. Demonstrate save/open round-trip.
12. Demonstrate legacy behavior unchanged.
13. Inspect TDC ingestion for authoritative transition metadata without guessing.
14. Run focused tests.
15. Run full source tests.
16. Run `R CMD check` and compare with Phase 2.
17. List every changed file.
18. Do not begin unrelated cleanup automatically.

## 26. Stop conditions

Stop and report before broader architecture/API changes if:

- correct reduction requires realizing the full cube;
- non-derivability requires a new public argument;
- result applicability requires changing the Phase 2 metadata contract;
- HDF5 blockwise schema dispatch cannot be implemented safely;
- TDC transition years are not established in repository sources;
- provenance would require a new public framework;
- interval derivability cannot be represented reliably with existing age semantics.

Propose the smallest alternative rather than silently expanding scope.

## 27. Final report

End with:

```text
Phase 3 algorithm:
...

Root cause of prior structural-NA behavior:
...

Implementation:
...

Structural vs genuine missingness:
...

Age derivability:
...

collapse_dim behavior:
...

Result applicability:
...

Subsetting composition:
...

TDC/source-specific findings:
...

Persistence:
...

Laziness / memory behavior:
...

Regression tests added:
...

Focused test results:
...

Full source test results:
...

R CMD check:
...

Files changed:
...

Pre-existing issues observed:
...

Remaining limitations / follow-up work:
...
```

Do not proceed into unrelated maintenance work unless explicitly instructed.
