# tarr.pop API Reference

This reference groups the exported, user-facing API by module. The "Object
Class" column indicates the primary object type a function works with or
returns.

## Core Cube Objects

| Function | Purpose | Object Class |
| --- | --- | --- |
| `new_poparray()` | Construct a `poparray` from an HDF5-backed delayed array. | `poparray` |
| `as.poparray()` | Coerce supported objects to `poparray`. | `poparray` |
| `is.poparray()` | Test whether an object is a `poparray`. | logical / `poparray` |
| `poparray_projection()` | Construct a `poparray_projection`. | `poparray_projection` |

## Cube Accessors

| Function | Purpose | Object Class |
| --- | --- | --- |
| `time_role()` | Get the time dimension name. | `poparray` |
| `area_role()` | Get the area dimension name. | `poparray` |
| `data_col()` | Get the value-column name used for tabular coercion. | `poparray`, `poparray_projection` |
| `data_col<-()` | Set the value-column name. | `poparray`, array-like |
| `dim_semantics()` | Get per-dimension semantic metadata. | `poparray` |
| `get_source()` | Get source/provenance metadata. | `poparray` |

## Dimension Label Accessors

| Function | Purpose | Object Class |
| --- | --- | --- |
| `years()` | Return labels for `year`. | `poparray`, array-like |
| `areas()` | Return labels for `area.name`. | `poparray`, array-like |
| `ages()` | Return labels for `age.char`. | `poparray`, array-like |
| `sexes()` | Return labels for `sex`. | `poparray`, array-like |
| `races()` | Return labels for `race`. | `poparray`, array-like |
| `ethnicities()` | Return labels for `ethnicity`. | `poparray`, array-like |
| `dim_labels()` | Return labels for a named dimension. | `poparray`, array-like |

## Filtering and Subsetting

| Function | Purpose | Object Class |
| --- | --- | --- |
| `filter.poparray()` | Restrict dimensions by label predicates without materializing data. | `poparray` |
| `%between%` | Range helper for ordered dimension filtering. | predicate helper |
| `split.poparray()` | Split a cube into slices by one dimension. | `poparray` |
| `by.poparray()` | Apply a function by dimension groups. | `poparray` |

## Grouped Aggregation

| Function | Purpose | Object Class |
| --- | --- | --- |
| `collapse_dim()` | Collapse one dimension into grouped levels with semantic safety checks. | `poparray` |
| `group_ages()` | Age-specific grouped collapse built on `collapse_dim()`. | `poparray` |

## Summaries and Coercion

| Function | Purpose | Object Class |
| --- | --- | --- |
| `summary.poparray()` | Return summary statistics for a `poparray`. | `poparray` |
| `as.data.frame.poparray()` | Convert a `poparray` to a long data frame. **EAGER**. | `poparray` |
| `as_tibble.poparray()` | Convert a `poparray` to a tibble. **EAGER**. | `poparray` |
| `as.double.poparray()` | Convert cube values to double while preserving cube structure. | `poparray` |
| `sd()` | Standard deviation for `poparray` using delayed block reduction. | `poparray` |
| `sum()` | Semantically guarded sum for `poparray`. | `poparray` |

## Projection API

| Function | Purpose | Object Class |
| --- | --- | --- |
| `project()` | Build a projection from a `poparray`. | `poparray` -> `poparray_projection` |
| `projection()` | Extract projected values. | `poparray_projection` |
| `std_error()` | Extract standard errors. | `poparray_projection` |
| `as.poparray.poparray_projection()` | Coerce a projection to `poparray`. | `poparray_projection` -> `poparray` |
| `as.data.frame.poparray_projection()` | Convert projection to tabular output. **EAGER**. | `poparray_projection` |
| `as_tibble.poparray_projection()` | Convert projection to tibble output. **EAGER**. | `poparray_projection` |
| `plot.poparray_projection()` | Plot projections as time series or pyramids. | `poparray_projection` |

## Cube Storage and Persistence

| Function | Purpose | Object Class |
| --- | --- | --- |
| `cube_path()` | Get the active cube storage root. | storage config |
| `set_cube_path()` | Set the active cube storage root. | storage config |
| `init_cubes()` | Initialize cube storage directories and bundled cubes. | storage config |
| `build_cube_if_missing()` | Create a cube only if not already present. | storage / builder |
| `create_poparray()` | Create and save a cube in canonical HDF5 schema. | `poparray` |
| `save_poparray()` | Save a `poparray` to canonical HDF5 schema. | `poparray` |
| `open_poparray()` | Open a registered cube lazily. | `poparray` |
| `open_tarr_pop()` | Backward-compatible alias for `open_poparray()`. | `poparray` |

## Metadata Administration

These functions are intended for elevated metadata administration. Write/edit
operations require `options(tarr.pop.metadata_role = "admin")` or
`Sys.setenv(TARR_POP_METADATA_ROLE = "admin")`.

| Function | Purpose | Object Class |
| --- | --- | --- |
| `dim_semantics()` | Read or write `dim_semantics` metadata for a `poparray` or cube file. | poparray or cube metadata |
| `roles()` | Read or write role metadata for a `poparray` or cube file. | poparray or cube metadata |
| `source_meta()` | Read or write source/provenance metadata for a `poparray` or cube file. | poparray or cube metadata |
| `data_col()` | Read or write the value-column name for a `poparray` or cube file. | poparray or cube metadata |
| `cube_metadata()` | Read or write the bundled canonical metadata in one validated operation. | poparray or cube metadata bundle |

## Data Conversion Helpers

| Function | Purpose | Object Class |
| --- | --- | --- |
| `df_2_array()` | Convert a data frame to an array. | data frame -> array |
| `array_2_df()` | Convert an array to a data frame. | array -> data frame |
