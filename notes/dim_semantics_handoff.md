# Dim Semantics Handoff

Status: Archived. Superseded by the `DimSemantics` object migration completed in
March 2026.

This handoff reflects an earlier design that used legacy fields such as
`exclusive`/`overlapping`. Keep for history only.

Current source of truth:

1. `inst/DimSematics_Implement.md`
2. `R/dim_semantics.r`
3. `R/poparray_class.r` and related I/O/reduction files

Date: 2026-02-23
Branch context: new branch focused on semantic safeguards for `poparray`.

## Objective

Implement a formal `dim_semantics` S4 slot contract for `poparray`, including:

- Slot definition
- Constructor enforcement
- Validation helper
- Subsetting behavior updates
- Strict `sum()` enforcement
- HDF5 metadata read/write support
- Tests
- Documentation updates

Primary source for scope: `inst/TOC.md`.

## Agreed Direction So Far

- This work should proceed on the current branch.
- `series_id` remains separate from `source/note` (already implemented in prior branch work and merged).
- We should create a reusable handoff artifact in-repo so future sessions can resume from file context.

## Pending Clarifications (Need User Decision)

1. Exact per-dimension `dim_semantics` schema:
   - proposed: `class`, `exclusive`, `overlapping`, `validated`

2. Backward compatibility when canonical `dim_semantics` metadata is missing on read:
   - strict default error vs opt-in permissive mode

3. If permissive mode is allowed, default synthesized semantics policy:
   - all dims conservative unsafe, or
   - time/area partition + others conservative, or
   - custom

4. HDF5 persistence shape:
   - JSON per dimension dataset at `cube/metadata/dim_semantics/<dim>`
   - or field-wise datasets at `cube/metadata/dim_semantics/<dim>/<field>`
   - recommendation from assistant: field-wise datasets

5. `sum()` implementation style:
   - S4 method `setMethod("sum", "poparray", ...)` as primary path

6. Constructor strictness:
   - require `dim_semantics` in all code paths immediately, including internal builders

7. Accessor:
   - export read-only `dim_semantics(x)` with no setter

8. Documentation output in-branch:
   - roxygen-only edits vs roxygen + regenerated `.Rd`

## Planned Implementation Sequence

1. Update `setClass("poparray", ...)` in `R/poparray_class.r` to include slot:
   - `dim_semantics = "list"`

2. Add internal validator in `R/poparray_class.r`:
   - `validate_dim_semantics()`
   - enforce name matching and required fields/types/invariants

3. Update constructor `new_poparray()`:
   - require and validate `dim_semantics`
   - enforce time/area `class = "partition"`
   - store in slot

4. Update `[` S4 method for `poparray`:
   - remove semantics for dropped dimensions
   - single-level subsets set `exclusive=TRUE`, `overlapping=FALSE`
   - preserve flags for partial multi-level slices
   - pass updated semantics into new object

5. Add new file `R/poparray_semantic_reductions.r`:
   - strict `sum` S4 method with `strict` and `allow_overlap`
   - no eager realization

6. Add persistence support in writer/open paths:
   - write/read `cube/metadata/dim_semantics/*`
   - rehydrate list and pass to constructor
   - maintain backward compatibility policy once confirmed

7. Add exported accessor:
   - `dim_semantics(x)`

8. Add tests:
   - `tests/testthat/test-dim-semantics.R` for all cases listed in `inst/TOC.md`

9. Update docs:
   - roxygen in `R/poparray_class.r`
   - sum method docs
   - `POPARRAY_CLASS.md`

## Resume Instructions

When resuming work, ask assistant to:

1. Read `inst/TOC.md`
2. Read this file: `notes/dim_semantics_handoff.md`
3. Confirm decisions for the pending clarifications above
4. Implement in the planned sequence with tests and doc updates
