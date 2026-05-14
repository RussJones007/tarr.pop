## Legacy Migration TOC (Archived)

Status: Superseded as of March 2026.

This file previously tracked the first dim-semantics migration plan built on
legacy fields (`class`, `exclusive`, `overlapping`).

Do not use this file as implementation guidance for new work.

Current source of truth:

1. `inst/DimSematics_Implement.md` for migration sections and sequencing.
2. `R/dim_semantics.r` for the active `DimSemantics` S7 contract.
3. `R/poparray_class.r`, `R/open_pop_array.r`, `R/cube_io.R`, and
   `R/poparray_semantic_reductions.r` for runtime behavior and persistence.

Notes:

- `poparray@dim_semantics` is a named list of `DimSemantics` objects.
- Stored semantics are intrinsic only (`partition_type`, `scale_type`, etc.).
- Overlap risk is derived at runtime from current labels and intrinsic
  semantics, not from stored mutable overlap flags.
