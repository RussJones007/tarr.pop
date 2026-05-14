# Storage Architecture for tarr_pop

## Overview

The current implementation stores population cubes inside the package directory (`inst/extdata`). While functional, this
approach has limitations for scalability, sharing, and persistence.
This document defines a new storage architecture that moves cube storage to a user-configurable, persistent location
while preserving all existing lazy evaluation and metadata-driven design.

---

## Problem

Current approach:

- Cubes stored in `inst/extdata`
- Accessed via registry scanning
- Reinstalled with the package

Limitations:

- Duplicate storage across users
- Not shareable across environments
- Large HDF5 files are not ideal for package directories
- Reinstallation risks overwriting or duplication

---

## Goal

Create a flexible storage system that:

- Uses a user-defined directory
- Supports shared/network storage
- Avoids unnecessary duplication
- Integrates seamlessly with existing functions

###  Recommended Architecture
####Design Goals
- User chooses a cube root directory
- Directory is persistent across sessions
- Cubes are not overwritten if they already exist
- Works for:
    * Single user
    * Shared network drive
    * Server environment

### Recommended Lazy-First Design
Use:

- A global option: options(tarr.pop.cube_path = "...")
- A helper accessor: cube_path()
- A setter: set_cube_path()
- An initializer: init_cubes()

The configuration file continaing the path to the cubes should be in yaml and readable by package functions.  If it 
does not exists this means the user needs to be prompted for a path to be created or where the cubes are found. 

---

## Folder structure
files shown are example and are would be replaced by the package cubes. 

tarr_pop_cubes/
│
├── base/                  # Immutable, source-of-truth cubes
│   ├── tarrant_population.h5
│   ├── texas_population.h5
│   └── us_population.h5
│
├── derived/               # User-generated cubes (projections, subsets, variants)
   ├── projections/
   │   ├── tarrant_population_arima_2026.h5
   │   └── texas_population_ets_2026.h5
   │
   ├── filtered/
   │   ├── tarrant_female_only.h5
   │   └── texas_age_65_plus.h5
   │
   └── custom/

---

## Design

### 1. Global Cube Path

Define a configurable storage location:

```r
cube_path()
set_cube_path(path)
```

Behavior:

- Default: user-level directory (e.g., `~/.tarr_pop/cubes`)
- Override via options
- Used by all I/O functions

---

### 2. Replace extdata Usage

Replace:

```r
resolve_extdata_dir()
```

With:

```r
resolve_cube_dir()
```

This removes dependency on package installation location.

---

### 3. Registry Refactor

Update:

```r
tarr_series_registry()
```

New behavior:

- Scan `cube_path()`
- Build registry from available `.h5` files
- Preserve metadata parsing

---

### 4. Safe Cube Creation

Add:

```r
build_cube_if_missing(name, builder_fun)
```

Behavior:

- Skip if cube exists
- Build only if missing
- Prevent accidental overwrite

---

### 5. Save Integration

Update:

```r
save_poparray()
create_poparray()
```

Default behavior:

- Write to `cube_path()`
- Allow override via filepath

---

### 6. Open Integration

Update:

```r
open_poparray(series_id)
```

Behavior:

- Look up file in `cube_path()`
- Use registry to resolve metadata

---

## Workflow

### Setup

```r
set_cube_path("/data/population_cubes")
```

### Build

```r
build_cube_if_missing("tarrant_population", function(path) {
  create_poparray(x, filepath = file.path(path, "tarrant_population.h5"))
})
```

### Use

```r
pop <- open_poparray("tarrant_population")
proj <- project(pop, h = 5)
```

---

## Design Principles

- Lazy-first (DelayedArray/HDF5Array)
- No eager realization
- Metadata-driven reconstruction
- Non-destructive by default
- Backward compatible

---

## Testing

- Path creation
- Registry scanning
- Save/load roundtrip
- No overwrite behavior

---

## Summary

This architecture separates data storage from package installation, enabling scalable, shared, and persistent population cube management while preserving all existing functionality.


---

## Developer Notes (Roxygen-Style)

```r
#' Storage subsystem for tarr_pop cubes
#'
#' @description
#' Provides a configurable, persistent storage layer for HDF5-backed population
#' cubes used by the tarr_pop package. This replaces the previous approach of
#' storing cubes in `inst/extdata`.
#'
#' The storage system is path-driven and integrates with existing I/O functions
#' such as `save_poparray()` and `open_poparray()` without altering the HDF5
#' schema or DelayedArray behavior.
#'
#' @details
#' Key components:
#' - `cube_path()`: returns active storage directory
#' - `set_cube_path()`: sets user-defined storage directory
#' - `tarr_series_registry()`: scans cube directory for available datasets
#' - `build_cube_if_missing()`: safe creation wrapper to avoid overwriting
#'
#' Storage design principles:
#' - Lazy evaluation (no array realization)
#' - Metadata-driven reconstruction
#' - Non-destructive by default
#' - Compatible with shared/network storage
#'
#' @section File Structure:
#' Each cube is stored as a single HDF5 file with:
#' - `cube/population`
#' - `cube/metadata/*`
#'
#' @section Backward Compatibility:
#' Legacy support for `inst/extdata` may be retained as a fallback but should not
#' be used for new cube creation.
#'
#' @section Future Extensions:
#' - `list_cubes()`
#' - `delete_cube()`
#' - Versioned schema migration
#'
#' @keywords internal
NULL
```
