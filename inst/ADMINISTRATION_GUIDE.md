# tarr.pop Administration Guide

This guide is for package administrators or advanced users responsible for cube
storage, persistence, and metadata management.

## Administrative Areas

- cube storage location and initialization
- canonical HDF5 persistence
- metadata inspection
- elevated-role metadata editing

## Storage Management

Use these functions to configure and manage the cube store:

- `cube_path()`
- `set_cube_path()`
- `init_cubes()`
- `build_cube_if_missing()`
- `create_poparray()`
- `save_poparray()`

Typical setup:

```r
set_cube_path("/path/to/cubes")
init_cubes()
```

## Opening and Verifying Cubes

- `open_poparray()` opens registered cubes lazily
- `read_series_row()` and registry helpers are internal, but useful for package development
- `cube_metadata()` reads the canonical metadata bundle

## Metadata Administration

Metadata editing is treated as an elevated subsystem.

To enable write/edit operations:

```r
options(tarr.pop.metadata_role = "admin")
```

or:

```r
Sys.setenv(TARR_POP_METADATA_ROLE = "admin")
```

### Field-Specific Metadata APIs

- `dim_semantics()` and ``dim_semantics() <-``
- `roles()` and ``roles() <-``
- `source_meta()` and ``source_meta() <-``
- `data_col()` and ``data_col() <-``

### Bundled Metadata APIs

- `cube_metadata()` and ``cube_metadata() <-``

The bundled accessor is preferred when multiple canonical metadata fields need to
change together because they validate cross-field consistency in one step.

## Administrative Rules

- Do not edit cube metadata casually in production files.
- Prefer bundled metadata writes when changing roles and `dim_semantics` together.
- Preserve `dim_order` and dimension names already stored in the cube.
- Keep role dimensions (`time`, `area`) aligned with valid partition semantics.
- Do not rewrite `cube/population` when only metadata needs updating.

## Recommended Admin Workflows

### Read Current Metadata

```r
meta <- cube_metadata(path)
str(meta)
```

### Edit One Metadata Field

```r
src <- source_meta(path)
src$note <- "revised provenance"
source_meta(path) <- src
```

### Edit Roles and Semantics Together

```r
meta <- cube_metadata(path)
meta$roles <- list(time = "year", area = "sex")
meta$dim_semantics$sex <- pa_update_dim_semantics(
  meta$dim_semantics$sex,
  partition_type = "partition",
  overlap_levels = character()
)
cube_metadata(path) <- meta
```

## Safe Persistence Workflow

1. Open or create a cube.
2. Save with `save_poparray()` or `create_poparray()`.
3. Inspect metadata with `cube_metadata()`.
4. Apply metadata edits using the elevated accessor APIs.
5. Re-open the cube with `open_poparray()` to verify runtime behavior.
