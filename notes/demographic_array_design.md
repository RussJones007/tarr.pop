# Shared demographic array contract

## Scope and hierarchy

`demographic_array` is a virtual S4 subclass of `DelayedArray`. Its concrete
subclasses are `poparray` and `poparray_projection`; neither inherits from the
other. The superclass describes indexed demographic structure, not a particular
statistical measure or reduction rule. Existing constructors remain the entry
points, and existing HDF5 schemas remain unchanged.

## Shared structure

The shared slots are `time_role`, `area_role`, `strata_roles`, `dim_semantics`,
`data_col`, and `source`. Time and area must be distinct named axes. Semantic
entries align with current dimension names and labels, including applicability.
Class-specific special axes are excluded from population stratification roles.
The superclass does not require an HDF5 seed; the poparray constructor retains
its existing HDF5-backed contract. Analysis grouping and the dimension-label
cache remain poparray-specific. Projection confidence level, method, base years,
and creation time remain projection-specific.

## Method migration map

| Responsibility | Owner |
|---|---|
| Structural validity and semantic alignment | Shared superclass |
| Role, provenance, value-column, semantic access | Shared slot-based implementation; existing public names retained |
| Special axes | Internal S4 generic; projection declares `stat` |
| Reconstruction after structural transforms | Internal S4 generic with concrete subclass methods |
| Named/positional subsetting and filtering | Shared structural implementation |
| Dimension renaming and relocation | Shared implementation plus subclass reconstruction |
| Position slicing | Shared structural implementation |
| Dimension selection | Shared selection planning; count aggregation stays poparray-specific |
| Grouping and summarisation | poparray |
| Count reductions and cube saving | poparray |
| Forecast extraction, uncertainty, confidence intervals, projection reading | poparray_projection |
| View recipes and replay | Existing poparray APIs |

## Statistical boundaries

Projection's `stat` axis contains `projection` and/or `std_error`. It is not a
population partition and cannot be renamed into an ordinary demographic axis.
Selecting or filtering its labels retains projection identity while it remains
an axis. Removing a singleton `stat = "projection"` axis may return a poparray
when required roles and applicability controllers remain. Removing a standard
error axis returns an ordinary delayed result, never a population-count array.
Removing required roles or applicability controllers also returns the delayed
result. Reconstruction must preserve projection provenance and fit settings.

Projection selection can reorder and rename retained population axes, but
omitting axes is rejected until uncertainty-aware aggregation is implemented.
Scalar numerical reductions on the combined projection object must not add
counts and standard errors. Users explicitly extract counts with `as.poparray()`
or select delayed statistics with `projection()`/`std_error()`.

## Verification and delivery

1. Introduce the virtual superclass and shared validity; verify construction.
2. Consolidate accessors and introduce subclass reconstruction.
3. Share structural transformations, preserving laziness and semantic updates.
4. Verify numerical boundaries, persistence, coercion, and view replay.
5. Update class/help documentation and NEWS; run focused and full tests, install
   and package checks. Keep stable checkpoints as commits on inheritance-changes.

Regression coverage must exercise role invariants, special axes, subclass return
types, metadata editing, overlap pruning, applicability trimming and renaming,
independent projection fit metadata, no payload reads for structural operations,
unchanged source files, persistence round trips, and guarded reductions.
