# Codex Phase 1 — Correct Dynamic Overlap Detection and Audit `poparray` API Argument Naming

## Purpose

Work on the `tarr.pop` R package.

This is **Phase 1 of a larger effort involving time-varying dimension schemas**. Do **not** implement time-varying applicability in this phase.

Phase 1 has two objectives:

1. Correct the existing overlap-safety behavior so that overlap risk is determined from the **current `poparray` object's levels**, not inherited from levels that have been removed by filtering or subsetting.
2. Audit first-argument naming across the `poparray` API and report recommendations, but **do not perform a broad argument-renaming refactor yet**.

Follow `AI_GUIDELINES.md` as authoritative. In particular, preserve laziness, dimensional integrity, metadata consistency, and existing S3/S4 contracts.

The existing `poparray` design states that overlap risk is to be derived at runtime from current labels plus intrinsic semantics rather than from a mutable historical overlap flag.

---

## Part A — Investigate before modifying

First trace the complete existing overlap-safety path.

Identify:

- the `DimSemantics` definition and constructor;
- how `partition_type`, `scale_type`, and `overlap_levels` are represented;
- all functions that determine whether a dimension currently has overlap risk;
- `validate_poparray()` and S4 validity methods;
- `sum()` and other reduction methods that honor `strict` and/or `allow_overlap`;
- `collapse_dim()`;
- `group_ages()` and related age helpers;
- `[` and `filter()` methods for `poparray`;
- any helper that copies or updates `dim_semantics` during subsetting;
- tests covering overlap, filtering, reduction safety, and metadata updates.

Before editing, provide a short explanation of **why the currently observed bug occurs**.

The observed bug is:

```r
# Conceptually:
x
# contains overlapping age levels

sum(x, strict = TRUE)
# correctly errors

y <- filter(x, age.char != "85+")

# y no longer contains the offending overlap

sum(y, strict = TRUE)
# currently still errors -- BUG
```

Determine whether this results from stale `overlap_levels`, a stored overlap flag, metadata that isn't updated during subsetting, or some other mechanism. Do not assume the cause before inspecting the code.

---

## Part B — Define and enforce the overlap invariant

Implement this invariant:

> **Overlap risk is determined from the dimension levels currently present in the `poparray`, interpreted using the dimension's intrinsic semantics.**

Historical membership in a parent cube must not make a derived/subsetted object permanently unsafe.

For example, suppose an age dimension contains:

```text
80-84
85+
85-89
90-94
95+
```

If `"85+"` overlaps the detailed older-age groups, strict aggregation should be blocked.

After:

```r
x2 <- dplyr::filter(x, age.char != "85+")
```

the current levels are:

```text
80-84
85-89
90-94
95+
```

If those constitute a valid non-overlapping partition, strict aggregation must no longer error.

Likewise, if the detailed groups are removed and `"85+"` remains, the resulting object should no longer be considered overlapping.

### Important distinction

Do not simply delete `overlap_levels` or otherwise discard useful intrinsic semantic information.

Determine what `overlap_levels` currently means in the class contract.

Separate, if necessary:

```text
intrinsic semantic information
```

from:

```text
derived current overlap risk
```

The latter must be recalculated from the current object.

---

## Part C — Subsetting must update semantics correctly

Audit both:

```r
x[...]
```

and:

```r
dplyr::filter(x, ...)
```

Subsetting is required to preserve laziness and update metadata consistently.

Confirm that when dimension levels disappear:

- `dimnames()` reflects the current levels;
- semantic metadata remains internally consistent;
- overlap detection sees only relevant current levels;
- no stale derived state causes false overlap warnings/errors;
- no HDF5 population data are unnecessarily realized.

Do not solve this by rebuilding or realizing the population cube.

---

## Part D — Preserve strict reduction behavior

Do not weaken the safety contract.

For an object that **still contains a genuine overlapping partition**:

```r
sum(x, strict = TRUE)
```

must continue to error.

Where currently supported:

```r
sum(x, strict = FALSE)
```

should continue to warn and proceed.

And:

```r
sum(x, allow_overlap = TRUE)
```

should continue to behave according to its documented contract.

The fix is specifically:

> eliminate false-positive overlap errors after the overlap has actually been removed.

It is **not**:

> make overlap checking less strict.

---

## Part E — Age semantics

For age dimensions, continue using interval semantics.

Do not detect overlap by ad hoc string matching.

The package contract defines age groups as intervals; for example:

```text
85+ = [85, Inf)
```

and single ages use half-open intervals.

Reuse the existing age/interval infrastructure where correct.

Do not introduce the future time-varying applicability mechanism in this phase.

---

## Part F — Tests

Add regression tests that reproduce the current bug before relying on the fix.

At minimum, test these situations.

### 1. Existing overlap is detected

Create a small `poparray` with genuinely overlapping age categories.

Verify:

```r
expect_error(
    sum(x, strict = TRUE)
)
```

or the equivalent existing reduction API.

### 2. Removing the broad category clears overlap

For example:

```r
x2 <- dplyr::filter(
    x,
    age.char != "85+"
)
```

Verify that strict reduction no longer raises the overlap error.

### 3. Removing detailed categories clears overlap

Keep `"85+"` and remove the overlapping detailed categories.

Verify that strict reduction is allowed.

### 4. Partial removal that leaves overlap remains unsafe

Construct a case where filtering removes some levels but a genuine overlap remains.

Verify that strict reduction **still errors**.

This guards against accidentally disabling the overlap check.

### 5. Base `[` subsetting

Repeat the essential regression test using the `[` method rather than only `filter()`.

Both APIs should result in correct current overlap semantics.

### 6. Unrelated dimension filtering

Filter a dimension such as year, sex, area, or race without changing the overlapping age levels.

Verify that the age overlap remains detected.

### 7. Non-age overlapping dimension

If the package currently supports overlap semantics for another dimension, add a corresponding test demonstrating that the fix is general and not hard-coded to `age.char`.

If no suitable existing semantics exist, do not invent one solely for this test.

### 8. Laziness

Verify that the overlap check and the relevant subsetting operations do not realize the complete DelayedArray/HDF5Array.

Use existing package testing patterns for laziness if available.

Do not add a brittle test that depends on undocumented internals of DelayedArray.

---

## Part G — API first-argument naming audit

This portion is **audit only** unless a very small internal inconsistency must be corrected as part of the overlap fix.

Inventory exported `tarr.pop` functions and methods whose principal input is a `poparray`.

For each, report:

```text
function
generic/method?
current first argument
exported?
recommended argument
reason
breaking change if renamed?
```

Use these proposed conventions when making recommendations:

### Established R/Bioconductor generic or method

Follow the generic's existing formal argument name. For example, if the generic uses `x`, retain `x`.

Do not rename method arguments merely for package-level uniformity.

### Package-specific user-facing verb

Prefer:

```r
pop
```

when the argument specifically represents a `poparray` and no generic contract dictates another name.

For example, conceptually:

```r
group_ages(pop, ...)
```

is preferable to:

```r
group_ages(pa, ...)
```

### Internal generic array helper

`x` may remain preferable when the code genuinely operates generically on an array-like object.

### Do not standardize on `pa`

Do not rename arguments globally to `pa`.

### Backward compatibility

Explicitly identify functions where changing:

```r
foo(x = my_pop)
```

to:

```r
foo(pop = my_pop)
```

would break named-argument callers.

Do not make those breaking changes in Phase 1.

---

## Part H — Documentation

Update roxygen documentation only where Phase 1 changes actual overlap behavior or corrects inaccurate documentation.

Document the invariant clearly:

> Overlap safety is evaluated from the dimension levels currently present in the `poparray`. Removing overlapping levels by subsetting can therefore remove overlap risk.

Do not document time-varying applicability yet.

Regenerate documentation if roxygen sources change.

---

## Part I — Explicitly out of scope

Do **not** implement any of the following during Phase 1:

- `DimSemantics$applicability`;
- time-varying age schemas;
- the 2017 TDC age-schema transition;
- Asian/Other time-varying applicability;
- deriving `85+` from later detailed ages;
- deriving `90+`;
- changing `collapse_dim()` to ignore structural `NA`;
- `na.rm = TRUE` as a workaround;
- a new schema rule engine;
- broad public API argument renaming.

Those belong to later phases.

---

## Part J — Completion criteria

Before considering Phase 1 complete:

1. Run the focused overlap/subsetting tests.
2. Run the full `testthat` suite.
3. Run `devtools::check()` or equivalent package check.
4. Report any warnings, notes, failures, or skipped tests.
5. Confirm whether the fix remains lazy.
6. Provide the API argument-name audit.
7. List every source and test file modified.
8. Summarize any behavior changes.
9. Identify anything discovered that should be handled in Phase 2 rather than expanding Phase 1.

Do not proceed into Phase 2 automatically.

### Final report to return

End with a concise report containing:

```text
Root cause:
...

Implementation:
...

Regression tests added:
...

Full test results:
...

R CMD check:
...

Laziness:
...

API argument audit:
...

Files changed:
...

Items deferred to Phase 2:
...
```

If investigation shows that fixing the overlap bug would require changing a fundamental `poparray` or `DimSemantics` contract, **stop before making that architectural change and explain the conflict**.

Phase 1 should leave us with a trustworthy existing overlap mechanism before we build applicability on top of it.
