# DimSematics Implementation Plan

This document records the migration plan for replacing the legacy per-dimension
semantic list contract in `poparray` with `DimSemantics` objects.

## Section 1. Contract Definition

Section 1 is the first implementation step and defines the target contract
before runtime refactoring.

### Target model

- `poparray@dim_semantics` will be a named list with one `DimSemantics` object
  per array dimension.
- The names of `poparray@dim_semantics` must exactly match the array dimension
  names and order.
- Each `DimSemantics` object stores intrinsic semantics only.
- Runtime overlap state is not stored in `DimSemantics`.
- `overlap` becomes derived state on demand from the current `poparray` labels
  and the intrinsic `DimSemantics` contract.

### Fields that remain intrinsic

The stored semantic contract for each dimension is:

- `dim_name`
- `domain`
- `scale_type`
- `partition_type`
- `validated`
- `overlap_levels`
- `notes`

### Fields removed from stored semantics

The legacy fields below are removed from persisted and in-memory
`DimSemantics` storage:

- `exclusive`
- `overlapping`

These values are considered redundant because they describe current-state or
derived behavior rather than intrinsic semantics.

### Runtime overlap model

- `overlap` belongs to the `poparray` runtime layer, not the `DimSemantics`
  object.
- Overlap is derived from the current labels present in the array slice.
- Derivation may use `partition_type`, `scale_type`, `overlap_levels`, and
  label parsing rules for interval dimensions.

### Persistence mapping

On-disk dimension semantics should map to one `DimSemantics` object per
dimension.

Legacy fields must be migrated as follows:

- `class == "partition"` -> `partition_type = "partition"`
- `class == "set"` -> `partition_type = "set"`
- `validated` -> `validated`
- `exclusive` -> dropped
- `overlapping` -> dropped

Defaults are still required when legacy cubes do not provide:

- `domain`
- `scale_type`
- `overlap_levels`
- `notes`

Those defaults must be defined consistently before reader/writer migration is
completed.

## Section 2. Refactor poparray validation to require DimSemantics objects

- Replace list-field validation with object validation.
- Require a named list of `DimSemantics` entries matching dimension names.
- Enforce role dimensions (`time`, `area`) as `partition_type == "partition"`.

## Section 3. Replace default and merge helpers

- Rewrite `default_dim_semantics()` to return `DimSemantics` objects.
- Rewrite `ensure_dim_semantics()` to fill gaps with `new_dim_semantics()`.
- Define deterministic defaults for `domain` and `scale_type`.

## Section 4. Remove stored exclusivity mutation from subsetting

- Rewrite `subset_dim_semantics()` so intrinsic semantics remain stable.
- Move overlap/exclusivity behavior to derived runtime logic.

## Section 5. Move reduction guards to derived overlap state

- Update semantic guards such as `sum(poparray)` to use derived overlap.
- Preserve strict/warn/bypass behavior while changing the underlying source of
  truth.

## Section 6. Update HDF5 persistence to DimSemantics fields

- Write the new `DimSemantics` fields to disk.
- Read them back as `DimSemantics` objects.
- Support legacy cube migration where needed.

## Section 7. Update poparray constructors and downstream call sites

- Update all code paths that build or propagate `dim_semantics`.
- Ensure projections, collapse, I/O, and coercion paths use the new object
  contract.

## Section 8. Rewrite tests around intrinsic semantics plus derived overlap

- Keep unit tests for `DimSemantics`.
- Rewrite integration tests that still assert `exclusive`/`overlapping`.
- Add coverage for overlap derivation and persistence round-trip.

## Section 9. Update documentation and exported semantics API

- Document `dim_semantics(x)` as returning `DimSemantics` objects.
- Remove references to the retired list-based fields.
- Document runtime overlap separately from stored dimension semantics.
