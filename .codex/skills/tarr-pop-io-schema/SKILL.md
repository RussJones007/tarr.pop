---
name: tarr-pop-io-schema
description: Design and manage HDF5-backed storage, metadata schema, and cube persistence for poparray objects using HDF5Array.
---

# Purpose

Design, validate, and maintain the HDF5-backed storage contract for `poparray` objects so cubes remain:

-   scalable
-   reproducible
-   metadata-complete
-   lazily loadable

This skill governs schema design, metadata layout, persistence, and shared cube storage strategies.

Apply `tarr-pop-repo-guard` first for shared repository constraints. Use
`tarr-pop-repo-guard/references/repo-guidelines.md` as the source of truth for
repo-wide laziness, dimensional, style, testing, and documentation rules.

------------------------------------------------------------------------

# Use this skill when

-   Working with:
    -   `open_poparray()`
    -   `save_poparray()`
    -   `create_poparray()`
-   Designing HDF5 schema
-   Storing one or more related cubes in a file
-   Managing metadata consistency and round-tripping
-   Implementing user-configurable or shared storage paths
-   Reviewing on-disk compatibility and migration
-   Recording, inspecting, restoring, or recovering cube metadata revisions

------------------------------------------------------------------------

# Canonical schema and metadata history

The current canonical storage schema is `1.2.0`. Keep population values at
`/cube/population`, current metadata at `/cube/metadata`, and revision records
at `/cube/metadata_history/<revision>/`. Current metadata includes
`schema_version`, a stable `cube_id`, and `metadata_revision`, the committed
revision marker. Read only revisions through that marker; staged or unfinished
history records must not appear as committed history.

Snapshots contain source metadata, roles, the value-column name, semantic
properties (including applicability), and dimension labels. Store snapshots as
serialized plain R lists (serialization version 2, integer byte vectors), not
serialized S7 objects. Reconstruct semantic objects when reading snapshots.
Keep revision, UTC timestamp, reason, action, schema version, and snapshot
format directly inspectable in HDF5. Optional registry/series metadata remains
in the canonical metadata; snapshots do not back up arbitrary HDF5 groups.

Use `cube_version()`, `cube_history()`, and `cube_metadata_at()` for metadata-only
inspection. History records provenance and coverage, **not previous population
values**. Never describe it as a numeric backup or population rollback.

## Version and compatibility policy

- Supported write formats are legacy cubes without a schema-version field and
  versions `1.0.0`, `1.1.0`, and `1.2.0`. Reject unsupported versions before
  mutation or replacement rather than silently relabeling them.
- Legacy cubes remain readable and report revision zero until tracking starts.
  Their first tracked edit or replacement records the original metadata as
  revision 1 and the update as revision 2, upgrading the storage schema to
  `1.2.0`. The baseline timestamp is adoption time, not original creation time.
- Schema changes require explicit implementation and migration tests. Ordinary
  note edits, year additions, or new estimate vintages create metadata revisions;
  they do not themselves require a schema-version bump.

## Supported update workflows

Use `update_cube_metadata(path, metadata, reason)` for an explicit change reason.
Path-based metadata setters also record revisions with default reasons. Require
the existing metadata-admin role for edits, restoration, and recovery; validate
roles and semantic compatibility together before persisting any bundle. A role
cannot be reassigned to a dimension with unknown partition semantics.
In-memory metadata edits and label renaming create no on-disk revision until
persisted. Manual HDF5 edits and external writers do not automatically track
revisions.

Pass `revision_reason` through `add_population_data()`, `ingest_population()`,
`build_poparray_from_df()`, `save_poparray()`, and the canonical writer as
applicable. Supported rebuilds and appends preserve the prior cube ID and
history, then record the updated labels and source notes. Describe which earlier
years a vintage rebuild revises; retaining history does not retain their values.

Use `restore_cube_metadata(path, revision, reason)` to create a new revision from
a historical snapshot. Require exactly matching current dimension labels and
valid semantics. Do not delete history, restore population values, or reverse
schema migrations. Earlier coverage cannot be restored onto a larger dataset.

## Replacement and recovery contract

Validate metadata first and stage a complete metadata-only file. Copy its new
metadata to `/cube/metadata_pending`, write complete history records, and retain
the old canonical group temporarily as `/cube/metadata_previous` while installing
the replacement. The canonical group's committed marker controls visibility.
Ordinary replacement errors restore the previous group.

HDF5 group replacement is not crash-atomic. After interruption,
`recover_cube_metadata()` restores the previous group if canonical metadata is
absent; otherwise it keeps canonical metadata and removes obsolete staging
groups. Recovery is not repair for disk corruption. Concurrent writers are
unsupported; shared access does not imply safe concurrent mutation.

Rebuilds and appends stage replacement files in the destination directory before
renaming them into place. Do not remove the original file before a successful
replacement. If the operating system cannot rename over the destination, fail
while preserving the original. No permanent numeric backup is created.

For detailed examples and limits, read
[the metadata-versioning note](../../../notes/cube_metadata_versioning.md).
Verify behavior against `R/cube_history.R`, `R/cube_io.R`,
`R/cube_metadata_admin.R`, and `R/ingestion.R` when changing this contract.

------------------------------------------------------------------------

# Core rules (in addition to tarr-pop-repo-guard)

-   Prefer lazy operations (DelayedArray / HDF5Array)
-   NEVER recommend realization unless explicitly labeled **EAGER**
-   Use dimension names (not positional indexing)
-   Preserve:
    -   `dimnames`
    -   roles (`time`, `area`)
    -   `dim_semantics`
-   Treat `"All"` as virtual (never physically stored)
-   Age must be treated as intervals, not strings
-   Do not allow unsafe aggregation across overlapping categories
-   Do not bypass semantic guards for convenience

------------------------------------------------------------------------

# Response requirements

Every response MUST include:

## 1. Interpretation

-   What storage or schema problem the user is solving
-   Whether the issue is file layout, metadata, loading, saving, or shared storage

## 2. Recommended lazy approach

-   Use HDF5Array-backed storage
-   Load cubes as delayed objects
-   Validate metadata before constructing objects
-   Preserve round-trip consistency

## 3. Base R alternative

-   Mark clearly as **EAGER** if it reads the full dataset into memory
-   Explain that this is only suitable for small arrays or debugging

## 4. Tidyverse alternative

-   Usually not applicable at the storage layer
-   If mentioned, clarify that tidyverse tools generally apply only after safe eager coercion

## 5. Tests to add (testthat)

-   Cover:
-   metadata round-trip integrity
-   dimension / dimname alignment
-   role preservation
-   dim_semantics preservation
-   delayed loading
-   multi-cube storage behavior if supported
-   stable identity and history across rebuilds and appends
-   legacy baseline adoption and rejection of unsupported schema writes
-   failed updates preserving canonical metadata and committed history
-   interrupted replacement recovery and metadata-admin access controls
-   restoration creating a new revision and rejecting mismatched labels
-   metadata inspection and edits leaving population values untouched

## 6. Roxygen notes (if exported)

-   Document:
-   canonical schema
-   required metadata fields
-   lazy vs EAGER loading behavior
-   shared storage assumptions
-   Include small examples of opening and saving cubes

------------------------------------------------------------------------

# Anti-patterns to flag

-   eager reads during open
-   schema drift across versions
-   missing metadata fields
-   inconsistent `dim_order`
-   hardcoded paths
-   storing derived aggregates like `"All"` as physical data
-   constructing objects before metadata validation

------------------------------------------------------------------------

# Verification snippets (when relevant)

``` r
getNamespaceVersion("HDF5Array")
getNamespaceVersion("DelayedArray")

exists("writeHDF5Array", where = asNamespace("HDF5Array"))
exists("DelayedArray", where = asNamespace("DelayedArray"))
```

------------------------------------------------------------------------

# Tone and behavior

-   Be precise, structured, and reproducibility-focused
-   Prefer stable schemas over clever ad hoc layouts
-   Keep file contracts explicit
-   Prioritize metadata correctness and delayed access

------------------------------------------------------------------------

# Notes for this skill

-   Canonical schema should clearly separate population data from metadata
-   Metadata should include at least:
-   dimension names
-   dimension order
-   roles
-   dim semantics
-   provenance/source as appropriate
-   Support shared storage patterns where multiple users can access the same cube files without unnecessary duplication
-   If multiple related cubes are stored together, explain the tradeoffs and maintain a clear contract
