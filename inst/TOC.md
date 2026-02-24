TOC.md

Table of contents for constructing, refactoring, or documenting the `tarr.pop` package.

## 🎯 Objective

-   Implement a formal dim_semantics S4 slot contract for poparray, including:

-   Slot definition

-   Constructor requirements

-   Validation logic

-   Subsetting updates

-   Strict sum() enforcement

-   Persistence support (metadata read/write)

-   Tests

## Phase 1 — Modify Class Definition

-   File: R/poparray_class.r

-   Steps: Add new S4 slot:

-   dim_semantics = "list"

-   Update setClass("poparray", ...) definition.

-   Ensure slot ordering remains consistent.

-   Do NOT modify inheritance from DelayedArray.

### Phase 2 — Constructor Enforcement

-   File: R/poparray_class.r

-   Target: new_poparray() (or equivalent constructor)

-   Tasks:

-   Require dim_semantics argument.

    If missing → error via cli::cli_abort().

-   Validate: Must be named list.

-   Names must match names(dim(x)).

-   Each entry must contain:

    -   class

    -   exclusive

    -   overlapping

    -   validated

-   Enforce invariants:

    -   Time role dimension must be class = "partition".

    -   Area role dimension must be class = "partition".

    -   Store in slot, not attribute.

    -   Do NOT inspect numeric data.

    -   Only validate structure.

### Phase 3 — Add Validation Helper

File: R/poparray_class.r

Add internal function: validate_dim_semantics()

Responsibilities:

-   Check name matching

-   Check required fields

-   Check logical types

-   Ensure no NULL entries

-   Fail informatively

-   Call this inside constructor.

### Phase 4 — Update Subsetting Method

-   File: R/poparray_class.r

-   Target: setMethod("[", "poparray", ...)

-   Tasks: After performing delayed subset:

    -   Determine which dimensions remain.

    -   Remove dropped dimension semantics.

    -   If dimension reduced to length 1:

    -   Set: exclusive = TRUE overlapping = FALSE

    -   Leave class unchanged.

    -   If dimension preserved fully → keep semantics unchanged.

    -   If partial slicing (multiple levels but fewer than original):

    -   Do NOT attempt recomputation.

    -   Preserve original semantic flags.

    -   (Future enhancement may recompute interval overlaps.)

    -   Assign updated dim_semantics to new object.

    -   Ensure no realization occurs.

    ### Phase 5 — Implement Strict sum() Method

-    Create new file: R/poparray_semantic_reductions.r

-   Add:

-   setMethod("sum", "poparray", function(x, ..., na.rm = FALSE,

-    strict = TRUE,

-    allow_overlap = FALSE) {

-   Inside:

    -   Extract dim_semantics.

    -   Compute unsafe dimensions:

-   Unsafe if:

    -   isFALSE(exclusive) \|\| isTRUE(overlapping)

-   If unsafe present:

-   If allow_overlap = TRUE → proceed silently.

-   Else if strict = FALSE → warn via cli::cli_warn().

-   Else → cli::cli_abort().

-   If safe → callNextMethod().

-   Do NOT use as.array().

-   Do NOT realize the DelayedArray.

-   Preserve laziness.

### Phase 6 — Metadata Persistence

-   File: Where HDF5 writing occurs

-   (e.g., save_poparray(), cube writer functions)

-   Tasks:

    -   Write dim_semantics into: cube/metadata/dim_semantics/\<dimension_name\>

    -   Store as JSON string or structured attributes.

    -   On open_poparray():

    -   Read metadata.

    -   Rehydrate into list.

    -   Pass into constructor.

    -   Validate after loading.

    -   Do not break backward compatibility.

    -   If metadata missing → error unless explicitly allowed.

### Phase 7 — Add Accessor

-   Add exported helper:

-   dim_semantics \<- function(x)

-   Returns slot.

-   No modification allowed via accessor (read-only).

### Phase 8 — Tests (testthat)

-   Create: tests/testthat/test-dim-semantics.R

-   Required test cases:

    -   Constructor fails if missing semantics.

    -   Constructor fails if names mismatch.

    -   Safe partition cube allows sum().

    -   Overlapping set dimension errors under strict mode.

    -   strict = FALSE gives warning.

    -   allow_overlap = TRUE allows sum().

    -   Subsetting to single level removes unsafe status.

    -   Dropped dimension removes semantic entry.

    -   HDF5 round-trip preserves semantics.

    -   Do not use large arrays in tests.

    -   Use small in-memory DelayedArray.

### Phase 9 — Documentation

-   Update roxygen in: poparray_class.r

-   ?sum.poparray

-   POPARRAY_CLASS.md

-   Document:

-   Algebraic distinction between partitions and sets.

-   Strict default behavior.

-   Override options.

-   Mark clearly:

-   This is intentional epidemiologic safeguard behavior.

### Phase 10 — Non-Goals (Do NOT Implement Now)

-   Automatic interval overlap detection

-   Automatic exclusivity inference

-   Smart recomputation on partial slicing

-   Changes to collapse_dim() semantics yet

-   Margin reduction enforcement yet

-   Keep first iteration minimal and strict.

### Acceptance Criteria

-   Codex implementation is complete when:

-   poparray constructor requires dim_semantics.

-   sum(poparray) blocks unsafe reductions.

-   Subsetting updates semantics correctly.

-   Laziness is preserved.

-   All new tests pass.

-   No existing tests break.

-   No forced realization introduced.

-   R CMD check passes cleanly.

###  Implementation Order for Agent

-   Modify class definition.

-   Add validation helper.

-   Update constructor.

-   Update [.poparray.

-   Implement sum() method.

-   Add accessor.

-   Add tests.

-   Update persistence.

-   Run devtools::document().

-   Run devtools::test().

-   Run devtools::check().

-   Final Instruction to Codex Agent

-   Follow changes incrementally.

-   After each phase:

-   Run tests.

-   Confirm no realization introduced.

-   Commit changes to a new branch.

-   Do not refactor unrelated code.
