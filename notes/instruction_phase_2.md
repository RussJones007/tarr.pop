# Codex Phase 2 — Add Time-Varying Dimension Applicability to `poparray` Metadata

## Purpose

Work on the `tarr.pop` R package.

This is **Phase 2 of a larger effort to support source schemas that change over time**.

Phase 1 established and regression-tested the existing overlap contract:

- overlap risk is derived from the **current object's labels**;
- `overlap_levels` is intrinsic semantic metadata, not a mutable cache of current overlap;
- filtering/subsetting remains lazy;
- `filter.poparray()` now supports `!=`.

Do not undo or redesign those Phase 1 behaviors.

The goal of Phase 2 is to add a **general metadata representation of dimension applicability**, integrate it with validity, subsetting, overlap safety, and HDF5 metadata persistence, while preserving backward compatibility and laziness.

This phase is intentionally **metadata-focused**.

Do **not** yet implement schema-dependent age collapsing/harmonization such as deriving later-year `85+` from `85-89 + 90-94 + 95+`. That is Phase 3.

Follow `AI_GUIDELINES.md` as authoritative.

---

# 1. Core semantic problem

A `poparray` can use the union of canonical levels needed across all source periods even when not every level is applicable in every period.

For example, a synthetic age dimension might contain the union:

```text
70-74
75-79
80-84
85+
85-89
90-94
95+
```

while the source schema varies by year:

```text
early period:
70-74
75-79
80-84
85+

later period:
70-74
75-79
80-84
85-89
90-94
95+
```

The union belongs in `dimnames()` because all of those canonical levels occur somewhere in the cube.

However, `"85+"` and the detailed older-age groups are **not simultaneously applicable** within a source schema.

The same abstraction must also be capable of representing non-age changes such as a race category becoming separately reported in later years.

Therefore implement applicability as a **general dimension-semantic concept**, not as an age-specific special case.

---

# 2. Architectural contract

Preserve this separation:

## 2.1 `dimnames()`

`dimnames()` contains the **union of canonical levels currently present in the object**.

It answers:

> What labels can occur anywhere along this dimension in this object?

It does not by itself say that every label is applicable at every time.

## 2.2 `dim_semantics`

`dim_semantics` contains intrinsic meaning, including the new applicability information.

It answers:

> Under what schema/context is each level applicable?

Applicability belongs here because it affects the valid domain and safe interpretation of dimension levels.

## 2.3 provenance/source metadata

Provenance explains **why** the schema changed, original source labels, source documentation, ingestion transformations, etc.

Do not encode provenance narratives or source-label mappings inside applicability merely because they are related historically.

Phase 2 should only establish the applicability mechanism.

---

# 3. Inspect before modifying

Before changing code, inspect and report the current implementation of:

- the S7 `DimSemantics` class;
- `new_dim_semantics()`;
- controlled update/replacement helpers;
- all `dim_semantics()` accessors/replacement functions;
- `validate_poparray()` and the S4 validity method;
- `subset_dim_semantics()` and `wrap_subset_poparray()`;
- `[` and `filter.poparray()`;
- `pa_dim_has_overlap_risk()`;
- `pa_has_interval_overlap()`;
- `pa_check_collapse_semantics()`;
- HDF5 serialization/deserialization of `DimSemantics`;
- `save_poparray()`, `open_poparray()`, and path-based metadata replacement;
- tests for metadata persistence/backward compatibility.

Then state the exact Phase 2 representation you intend to implement.

If the existing serialization or S7 design makes the representation below unsafe or needlessly incompatible, explain the conflict before changing the contract.

---

# 4. Applicability representation

Add an **optional** applicability field to `DimSemantics`.

The intended conceptual representation is:

```r
applicability = list(
  by = "year",
  schemas = list(
    list(
      from = NULL,
      through = "2016",
      levels = c(
        "70-74", "75-79", "80-84", "85+"
      )
    ),
    list(
      from = "2017",
      through = NULL,
      levels = c(
        "70-74", "75-79", "80-84",
        "85-89", "90-94", "95+"
      )
    )
  )
)
```

The year values above are **synthetic examples for testing the mechanism**, not a hard-coded claim about any TDC source transition.

Use the smallest general representation that works cleanly with the existing class and persistence model.

### Requirements

- `applicability = NULL` means no time-varying applicability has been declared.
- Existing `DimSemantics` objects without applicability remain valid and preserve current behavior.
- `by` names another dimension in the `poparray` whose ordered labels determine the schema period.
- `levels` contains canonical labels from the dimension whose `DimSemantics` object owns the applicability rule.
- `from = NULL` means beginning of the current controlling dimension's ordered domain.
- `through = NULL` means end of the current controlling dimension's ordered domain.
- Boundary matching should use the ordered labels of the controlling dimension, not assumptions that the labels are numeric years.
- Do not build a general predicate/rule language.
- Do not add `included_in`, `replaced_by`, arbitrary expressions, or cross-dimensional Boolean logic in this phase.

If a small internal normalized representation is preferable to storing the user-facing list literally, that is acceptable, but keep the public semantic concept simple and serializable.

---

# 5. Applicability validity

Applicability validation must be **metadata-only and cheap**.

Do not scan or realize the HDF5 population values.

When applicability is `NULL`, retain all existing validity behavior.

When applicability is present, validate at least the following.

## 5.1 Structural validity

- `by` is a single non-empty dimension name.
- the `by` dimension exists in the owning `poparray`;
- every schema has a valid `levels` character vector;
- schema levels are unique within a schema;
- every declared level exists in the current target dimension's `dimnames`;
- every non-NULL `from`/`through` boundary exists in the current `by` dimension;
- each range is ordered (`from` cannot occur after `through`);
- schema ranges must not overlap.

## 5.2 Coverage

For an object that declares applicability, every **currently present label of the controlling `by` dimension** must resolve to exactly one applicability schema.

Do not silently treat an uncovered period as "all levels applicable."

Ambiguous or uncovered current periods should make the metadata invalid.

This gives operations a deterministic interpretation.

## 5.3 Schema-level semantic validity

Validate semantic constraints using the **levels applicable within each schema**, not merely the union of all dimension labels.

For interval/age semantics, overlapping labels that occur in different, non-overlapping schema periods are allowed.

For example, the union may contain:

```text
85+
85-89
90-94
95+
```

without making the object invalid if `"85+"` is applicable only in one schema and the detailed groups only in another.

But a single schema that declares both:

```text
85+
85-89
```

must still be considered semantically overlapping/unsafe as appropriate under the existing age interval semantics.

Reuse existing interval logic where possible.

Do not use string matching for age overlap.

---

# 6. Important change to overlap-risk evaluation

Phase 1 confirmed that current overlap risk is computed from current labels.

Phase 2 must refine that invariant:

> **Overlap risk is derived from the current labels that are simultaneously applicable within the current schema/context.**

The union of labels in `dimnames()` must not itself create a false overlap merely because two overlapping labels belong to mutually exclusive schema periods.

### Required behavior

Consider a cube spanning two schemas:

```text
early:
80-84, 85+

later:
80-84, 85-89, 90-94, 95+
```

The union contains both `"85+"` and detailed ages.

The overlap guard must evaluate applicability and recognize that they are not simultaneously applicable.

### Preserve conservative behavior

Do not weaken safety for:

- legacy objects with no applicability metadata;
- dimensions whose overlap cannot be resolved from current semantics;
- schema periods that genuinely contain overlapping applicable levels.

Old objects must behave exactly as they did before Phase 2.

---

# 7. Subsetting behavior

Applicability metadata must remain correct after lazy subsetting.

Audit both:

```r
x[...]
```

and:

```r
dplyr::filter(x, ...)
```

## 7.1 Subsetting the controlling dimension

If a cube spanning multiple schema periods is subset to only an early period, the result should retain only the applicability information needed to describe that current object.

If subset to only a later period, likewise.

If subset across the transition, retain the relevant multiple schemas.

You may simplify or trim schema ranges when it is safe and clear, but **correctness is more important than canonical minimization**.

The resulting applicability metadata must validate against the resulting `dimnames()`.

## 7.2 Subsetting the target dimension

If target levels are filtered out, applicability `levels` must be updated/intersected with the current target dimension labels.

For example:

```r
filter(x, age.char != "85+")
```

must not leave `"85+"` referenced by applicability metadata when it is no longer in the object's `dimnames()`.

This must preserve the Phase 1 invariant that overlap safety concerns the object currently in hand.

## 7.3 Unrelated dimension subsetting

Filtering area, sex, or another unrelated dimension must not damage applicability metadata.

## 7.4 Laziness

All applicability updates during subsetting must be metadata-only.

Do not realize population values.

---

# 8. HDF5 persistence and backward compatibility

Applicability must survive the canonical persistence path.

Implement support through the existing metadata architecture used by:

- `save_poparray()`;
- `open_poparray()`;
- `dim_semantics()` / `dim_semantics<-` as applicable;
- metadata stored under the package's existing HDF5 metadata structure.

Use the existing serialization strategy where possible rather than inventing an unrelated storage mechanism.

## Required behavior

### Round-trip

```r
x
# has applicability metadata

save_poparray(x, path)

y <- open_poparray(path)
```

`y` must preserve applicability exactly enough to produce the same semantic behavior.

### Backward compatibility

Existing HDF5 cubes with no applicability field must continue to open successfully.

For these cubes:

```r
applicability
```

should normalize to `NULL` or the equivalent absent-state representation.

Do not require rewriting existing population datasets merely to add support for the new metadata field.

### Metadata-only evolution

Where the current package supports updating HDF5 metadata independently from population values, applicability updates should use that path.

Do not rewrite or realize the numeric population dataset solely to change applicability metadata.

---

# 9. Do not solve structural NA behavior yet

Phase 2 establishes **which levels are applicable**.

It does not yet change grouped numerical reduction behavior.

Do not implement any blanket:

```r
na.rm = TRUE
```

behavior.

Maintain the distinction for Phase 3 between:

- an inapplicable cell that is structurally outside a source schema; and
- an applicable cell whose population value is genuinely missing.

Phase 3 will use applicability to exclude structural/inapplicable cells from the relevant reduction while allowing genuine missing values to propagate.

Do not preempt that design here.

---

# 10. Do not implement age harmonization yet

The following desired future behavior belongs to Phase 3:

```text
early source:
85+

later source:
85-89 + 90-94 + 95+
```

followed by a request to produce a harmonized:

```text
85+
```

across all years.

Likewise, deciding that `90+` is not derivable in the early schema but is derivable later belongs to Phase 3.

Phase 2 should make those future decisions possible by providing reliable applicability metadata, but should not perform the calculations.

---

# 11. No TDC-specific rules in core implementation

Do not hard-code:

- TDC;
- 2017;
- specific age boundaries;
- Asian;
- Other;
- source-specific race labels.

Tests should use small synthetic examples that demonstrate the general mechanism.

TDC-specific ingestion metadata will be added only after the abstraction is proven.

---

# 12. Tests to add

Use `testthat`.

At minimum add the following test groups.

## 12.1 Constructor / semantics tests

Verify:

- `DimSemantics` without applicability remains valid;
- valid applicability can be created;
- malformed applicability is rejected;
- duplicate schema levels are rejected;
- unknown target levels are rejected when validated against a poparray;
- unknown `by` dimensions are rejected;
- unknown boundaries are rejected;
- reversed ranges are rejected;
- overlapping schema periods are rejected;
- uncovered current controlling labels are rejected.

## 12.2 Applicability-aware interval semantics

Create a synthetic age dimension whose union contains:

```text
80-84
85+
85-89
90-94
95+
```

and two non-overlapping schema periods.

Verify that:

- the full union is allowed when overlapping intervals are mutually exclusive by schema;
- putting `"85+"` and `"85-89"` in the same schema is detected as unsafe/invalid according to the existing semantic contract.

## 12.3 Overlap guard tests

Verify that strict overlap safety:

- does not report false overlap solely from mutually exclusive schema levels;
- still detects genuine overlap inside one applicability schema;
- preserves old conservative behavior when applicability is absent.

Do not require Phase 3 numerical harmonization in these tests.

## 12.4 Controlling-dimension subsetting

For an object with early and late schemas:

```r
early <- filter(x, year <= ...)
late  <- filter(x, year >= ...)
both  <- filter(x, ...)
```

Verify applicability metadata is correct and valid in each result.

Use whatever filter predicates are actually supported by the package; do not invent syntax that is not implemented.

## 12.5 Target-dimension subsetting

Filter out one or more age levels.

Verify:

- `dimnames()` updates;
- applicability levels update consistently;
- validity succeeds;
- overlap risk uses only current applicable levels.

## 12.6 Unrelated dimension subsetting

Filter area/sex/another unrelated dimension and verify applicability is preserved.

## 12.7 HDF5 round-trip

Save and reopen an applicability-aware `poparray`.

Verify semantic equivalence after reopening.

## 12.8 Legacy HDF5 compatibility

Open or construct a file using the old metadata representation with no applicability.

Verify it remains valid and behavior is unchanged.

## 12.9 Laziness

Instrument using the same or equivalent approach established in Phase 1.

Verify that:

- applicability inspection;
- applicability validity;
- overlap-risk checking;
- ordinary subsetting;
- metadata persistence operations where feasible

do not extract population values or realize the HDF5-backed cube.

Do not rely on undocumented DelayedArray internals merely for the test.

---

# 13. Documentation

Update roxygen2 documentation for any modified exported functions/classes/accessors.

Document clearly:

1. `dimnames()` may contain the union of levels across multiple source schemas.
2. Applicability identifies which levels are valid in each controlling-dimension period.
3. Applicability is semantic metadata, not provenance.
4. Overlap safety is evaluated among simultaneously applicable current levels.
5. Existing objects without applicability preserve legacy behavior.

If applicability is not directly exposed through a new public accessor, document how users can inspect it through the existing `dim_semantics()` interface.

Do not add a new exported accessor solely for convenience unless existing API structure clearly requires it.

If you believe a new public accessor such as `dimension_schema()` is necessary, stop and recommend it rather than expanding Phase 2's public API without review.

---

# 14. Base R / tidyverse / delayed-array considerations

This phase should manipulate only small metadata structures.

Using ordinary base R list/vector operations internally is acceptable and is **not EAGER with respect to the population cube**.

Using dplyr/tibble internally for tiny metadata structures is also acceptable where it improves clarity, but avoid NSE-heavy machinery unless justified.

Do **not** convert the population values to a data frame/tibble or call `as.array()`/`as.matrix()` to implement applicability.

Preserve the HDF5-backed/DelayedArray population seed.

---

# 15. Phase 1 package-check findings

Phase 1 reported package-wide `R CMD check` failures unrelated to the overlap implementation, including:

- undefined `census` in a `dim_labels` example;
- packaged tests sourcing a `data-raw` script excluded from the build;
- vignette/check dependency problems;
- existing generic/documentation/import issues.

Do not broaden Phase 2 into a general package-cleanup project.

However:

- rerun focused tests;
- rerun the full source test suite;
- rerun `R CMD check`;
- distinguish **new Phase 2 regressions** from **pre-existing findings**;
- do not claim Phase 2 passes `R CMD check` if the same package-wide errors remain.

If Phase 2 introduces any new check error/warning/note, treat that as a Phase 2 defect unless clearly justified.

---

# 16. API naming audit from Phase 1

Do not perform the proposed broad first-argument renaming in Phase 2.

Phase 1 found:

- generic/method signatures should retain their established names;
- `group_ages(pop, ...)` already follows the desired package-specific convention;
- several package-specific functions could migrate to `pop` in a future compatibility-aware change;
- `collapse_dim()` would require a generic-wide migration;
- polymorphic object/path accessors should generally retain `x`.

Applicability work must not be mixed with that compatibility migration.

---

# 17. Explicitly out of scope for Phase 2

Do **not** implement:

- schema-aware `collapse_dim()` numerical harmonization;
- derivation of `85+`;
- derivation of `90+`;
- structural-NA-aware reductions;
- `na.rm = TRUE` as a structural-NA solution;
- TDC-specific age transition rules;
- TDC-specific Asian/Other rules;
- source-label normalization rules;
- a general relationship/rule engine;
- arbitrary cross-dimensional predicates;
- broad API argument renaming;
- unrelated R CMD check cleanup.

Those belong to Phase 3 or separate maintenance work.

---

# 18. Completion criteria

Before declaring Phase 2 complete:

1. Explain the final applicability representation.
2. Show how it integrates with `DimSemantics`.
3. Demonstrate metadata-only validation.
4. Demonstrate applicability-aware overlap checking.
5. Demonstrate correct subsetting of both the target and controlling dimensions.
6. Demonstrate HDF5 save/open round-trip.
7. Demonstrate backward compatibility with applicability-absent objects/files.
8. Demonstrate that population values are not realized for metadata operations.
9. Run focused tests.
10. Run the full source `testthat` suite.
11. Run `R CMD check`.
12. Separate pre-existing check issues from newly introduced ones.
13. List every source/test/documentation file changed.
14. Do not proceed into Phase 3 automatically.

---

# 19. Stop conditions

Stop before making a broader architectural change if any of the following is discovered:

- the proposed applicability concept conflicts with an existing documented `DimSemantics` contract;
- HDF5 persistence would require rewriting the population dataset;
- the only practical implementation requires realizing population values;
- backward compatibility would require changing existing objects' semantics;
- implementing applicability safely requires a new public API that has not been reviewed;
- the representation cannot distinguish current object labels from applicability cleanly.

Explain the conflict and propose the smallest alternative.

---

# 20. Final report format

End with:

```text
Phase 2 representation:
...

Files inspected:
...

Implementation:
...

Validity rules:
...

Overlap behavior:
...

Subsetting behavior:
...

Persistence / backward compatibility:
...

Laziness:
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

Items deferred to Phase 3:
...
```

Do not begin Phase 3 until explicitly instructed.
