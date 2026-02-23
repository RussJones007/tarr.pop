TOC.md

Table of contents for constructing, refactoring, or documenting the `tarr.pop` package.

## 1. Poparray file storage schema migration (Completed)

Population cubes now use a canonical HDF5 layout where data and metadata live in one file:

-   `cube/population`
-   `cube/metadata/*`

Metadata now written/read in HDF5 includes:

-   dimension order and dim labels
-   role metadata (`time`, `area`, `strata`)
-   provenance/source metadata
-   value-column label (`data_col`)
-   optional registry fields

RDS-sidecar metadata files are no longer the primary source of truth for migrated cubes.

## 2. Poparray class inheritance migration (Completed)

The class has been migrated to S4 and now extends `DelayedArray` directly.

Current `poparray` slots are:

-   `time_role`
-   `area_role`
-   `strata_roles`
-   `data_col`
-   `source`

Constructor and validity behaviors implemented:

-   `new_poparray()` requires a `DelayedArray` and enforces HDF5-backed seeds
-   required role dimensions must exist in named `dimnames`
-   metadata/dimname consistency checks are enforced
-   validity checks avoid full array realization

Method migration status:

-   S4 `show` and S4 `[` methods are implemented for `poparray`
-   S3 compatibility methods remain where needed during transition
-   DelayedArray arithmetic/math behavior is still delegated to backend generics

## 3. Saving poparray objects (Current)

User-facing save helpers now use simplified names:

-   `save_poparray()`
-   `create_poparray()`

Design decisions:

-   `save_poparray()` accepts only `poparray`
-   metadata arguments for `data_col`, `source`, `time_dim`, and `area_dim` are not user inputs
-   those metadata are read directly from `x` slots/accessors
-   `create_poparray()` is a convenience wrapper that adds `series_id` to registry metadata

## 4. Open items and maintenance notes

-   remove remaining transitional S3 wrappers once all callers use S4 pathways
-   continue tightening docs to match slot-based metadata as authoritative
-   expand tests around save/create helpers and round-trip metadata integrity
