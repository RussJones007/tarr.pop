# Cube schema and metadata versioning

The canonical cube schema is now `1.2.0`. The population dataset stays at
`/cube/population`; no previous population datasets are retained.

## Stored fields

- `/cube/metadata/schema_version`: storage-format version.
- `/cube/metadata/cube_id`: stable identifier carried through supported rebuilds
  and appends.
- `/cube/metadata/metadata_revision`: current committed metadata revision.
- `/cube/metadata_history/<revision>/`: timestamp (UTC), reason, action,
  schema version, snapshot format, and complete supported metadata snapshot.

Snapshots include source notes, roles, data-column name, semantic properties
(including applicability), and dimension labels. Full snapshots are serialized
plain R lists using serialization version 2, stored as integer byte vectors;
they do not serialize S7 classes. Scalar history fields remain directly
inspectable through HDF5 tools. Optional registry/series fields remain in the
current canonical metadata; snapshots are not a backup of arbitrary HDF5 groups.

`cube_version(path)` reports identifiers and versions. `cube_history(path)`
reports revisions and time coverage. `cube_metadata_at(path, revision)` reads a
historical snapshot and reconstructs semantic objects without reading values.

## Updating notes or other metadata

```r
options(tarr.pop.metadata_role = "admin")
metadata <- cube_metadata(path)
metadata$source$note <- paste(
  "2025 estimate vintage; coverage 2011–2025.",
  "Estimates for earlier years were revised by the source."
)
update_cube_metadata(path, metadata,
  reason = "Updated estimate vintage and caveats")
cube_history(path)
```

Existing path-based metadata setters also record revisions, with default
descriptions. In-memory label renaming and metadata edits do not write history
until persisted. All persisted bundles now validate roles and semantic
compatibility together; a role cannot be reassigned to a dimension with unknown
partition semantics.

## Adding a year or rebuilding with a new vintage

Pass `revision_reason` to `add_population_data()` for a new time slice, or
`ingest_population(..., overwrite = TRUE)` / `save_poparray(..., overwrite = TRUE)`
for a rebuild of an existing file. The low-level canonical cube writer also
accepts this argument. Default descriptions are provided when no reason is supplied.

The replacement cube carries the prior metadata history and cube ID forward,
then adds a snapshot reflecting the new time labels and source note. A vintage
rebuild can replace estimates for earlier years; its reason should explain
that scope. History is retained even though previous population values are not.
External writers and manual HDF5 edits do not automatically create revisions.

## Restoration and legacy cubes

`restore_cube_metadata(path, revision, reason)` creates a new revision; it never
deletes history or restores numeric data. Restoration requires exactly the same
dimension labels as the current cube and valid semantic metadata. A snapshot
from coverage 2011–2024 cannot restore labels onto a 2011–2025 dataset.

Older cubes remain readable and initially report revision zero. Their first
tracked edit or replacement records the original metadata as revision 1, then
records the update as revision 2. Supported schema versions are legacy files
without a version field and versions 1.0.0, 1.1.0, and 1.2.0. Metadata updates
upgrade these to 1.2.0. Unrecognized schema versions are rejected for writes
rather than silently relabeled. Storage-schema changes require explicit code
support and migration tests; ordinary notes or year additions do not themselves
require a schema bump.
The baseline timestamp records when history tracking was adopted, not when the
legacy cube was originally created.

## Failure handling and limits

Metadata updates validate first and build a temporary metadata-only file. The
new group is copied to `/cube/metadata_pending`; complete history records are
written before the group is installed. The old group is briefly retained as
`/cube/metadata_previous`. The committed revision marker lives in the canonical
metadata group, so unfinished history entries do not appear in `cube_history()`.
An ordinary replacement error restores the previous group.

HDF5 group replacement is not a crash-atomic database transaction. After a
process interruption, `recover_cube_metadata(path)` restores the previous group
if the canonical group is absent and removes obsolete staging groups. Disk
corruption still requires a real backup. Concurrent writers are unsupported.

Cube replacement uses a temporary file in the destination directory before
renaming it into place; no permanent numeric backup is kept. Replacement relies
on the operating system permitting rename over an existing destination. If it
cannot, the operation errors while preserving the original file.

## Storage skill follow-up

The project's `tarr-pop-io-schema` skill still gives sound guidance on lazy
loading, roles, semantics, and round-trip testing. It should later be extended
with the schema-version policy, metadata-history layout, committed revision
rules, recovery behavior, and distinction between metadata history and numeric
backups. The skill itself was not changed in this task.
