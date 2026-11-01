---
editor_options: 
  markdown: 
    wrap: 120
---

# poparray: A Lazy, Role-Aware Population Array

## Overview

A **`poparray`** object represents population counts indexed over **time** and **area**, optionally stratified by
additional dimensions such as age, sex, race, household size, or economic characteristics.

The class is designed for **large, multi-dimensional population data**, with an emphasis on:

-   scalability (HDF5-backed storage),
-   delayed computation,
-   explicit dimensional contracts, and
-   clear separation between numeric data and semantic metadata.

`poparray` is intended for epidemiologic analysis, demographic reporting, and population projection workflows where
correctness, traceability, and memory safety are critical.

------------------------------------------------------------------------------------------------------------------------

## Core design principles

The design of `poparray` is governed by the following principles:

1.  **Time and area are structural invariants**\
    Every `poparray` must include:

    -   exactly one **time dimension** (e.g. `year`), and
    -   exactly one **area dimension** (e.g. `area`, `county`, `tract`).

2.  **All other dimensions are optional stratifications**\
    Additional dimensions represent analyst-defined stratifications and may include:

    -   demographic variables (age, sex, race, ethnicity),
    -   socioeconomic variables (income bracket, education level),
    -   household characteristics (household size, family type),
    -   or other analyst-defined groupings.

3.  **Only independent indexing axes are dimensions**\
    Hierarchical geography (e.g. tract → county → state) is **not** encoded as multiple array dimensions.\
    Instead, only one geographic resolution is represented in the array at any time.

4.  **Hierarchy and meaning live in metadata, not in the array**\
    Provenance is stored in metadata, not as an additional array axis. Geographic hierarchy mappings, when available,
    are supplied separately by the workflow. Explicit mappings can support aggregation from fine to coarser geography;
    the class does not supply or infer these relationships automatically.

5.  **Delayed computation by default**\
    Operations preserve `laziness` whenever possible and avoid realizing large arrays in memory.

------------------------------------------------------------------------------------------------------------------------

## What a poparray object contains

A `poparray` is an S4 object that extends `DelayedArray` around a multi-dimensional array (typically HDF5-backed) plus
semantic metadata slots.

### Numeric data

-   Stores its numeric data as a DelayedArray, typically backed by an HDF5 file via HDF5Array.
-   Dimension names (`dimnames`) are always present and meaningful.
-   Chunking and on-disk storage are handled by the HDF5Array infrastructure.

### Required dimensions

Every `poparray` includes:

| Role | Description                                                              |
|------|--------------------------------------------------------------------------|
| Time | An ordered time dimension (e.g. `year`)                                  |
| Area | A single geographic indexing dimension (e.g. `block`, `tract`, `county`) |

Dimension order is not significant; semantic roles are determined by metadata.

### Optional stratification dimensions

Any number of additional indexing dimensions may be present. Their categories need not be mutually exclusive,
and available categories may change over time. Independence of indexing axes does not imply statistical independence
or safe addition of every category. Applicability metadata describes which categories apply together in each period.
Dimensions may be collapsed or retained subject to semantic safety checks.

------------------------------------------------------------------------------------------------------------------------

## Metadata model (current implementation)

The current class contract stores core semantics in S4 slots:

-   `time_role` - name of the time dimension
-   `area_role` - name of the area dimension
-   `strata_roles` - character vector of all non-time/non-area dimensions
-   `dim_semantics` - named list of S7 `DimSemantics` objects describing the dimensions of this object:
    -   `dim_name`
    -   `domain`
    -   `scale_type`
    -   `partition_type`
    -   `validated`
    -   `overlap_levels`
    -   `notes`
    -   `applicability`
-   `group_vars` - ordered grouping dimension names for in-memory analysis; not persisted in cube metadata
-   `dn_cache` - internal cache of current dimension names and labels
-   `data_col` - value column name used for tabular coercion
-   `source` - named provenance list (e.g. `note`, `source`, `updated`, `population_type`)

For compatibility with older S3-era helpers, transitional attributes are still written by the constructor:

-   `attr(x, "dimroles")`
-   `attr(x, "data_col")`
-   `attr(x, "source")`

Methods should treat the S4 slots as authoritative.

### Semantic metadata follows the returned object

`dim_semantics(x)` is the semantic contract of the **current poparray**, not an untouched copy of the original
source definition. Filtering, subsetting, and selection verbs update semantic metadata as needed for the returned
object's dimensions, labels, and applicability. The transformed object need not retain every original semantic setting.
Settings that still apply are carried forward; metadata affected by the transformation is adjusted or removed.

The original semantic meaning and settings remain in the source cube on disk. These transformations do not rewrite
that file. Reopening the unchanged source cube restores its stored contract, whereas saving a transformed object to a
separate cube persists the transformed object's semantics. Explicitly overwriting a cube or editing its stored metadata
is a separate action that can replace its original settings.

The implementation distinguishes intrinsic descriptors from derived current-state facts:

-   `domain` and `scale_type` retain their meaning during subsetting. `overlap_levels` is trimmed to labels still
    present. When a true subset removes all declared overlap levels and the existing overlap guard confirms safety,
    `partition_type` becomes `"partition"` for the retained categories. This declares disjointness of the retained
    categories, not complete coverage of the original population universe. Unknown category relationships and
    remaining interval overlap are not promoted to a partition merely because `overlap_levels` is empty.
-   Current overlap risk is computed from active labels and those descriptors, within applicable schemas when present.
    It is not stored as a mutable overlap-present flag. Interval overlap is determined from interval labels.
-   `validated` declares canonical-source or externally reviewed semantics. It does not certify population values,
    completeness, or aggregation safety; structural validation does not automatically set it.

| Transformation | Semantic metadata behavior |
|---|---|
| Subset or filter | Keep entries for remaining dimensions; intersect applicable levels with retained labels and rebuild applicability ranges for the selected controller labels. Remove absent overlap declarations; mark a safe subset as a partition when all declared overlaps have been removed. |
| Rename dimensions | Update entry names, `dim_name`, role references, grouping names, and `applicability$by`; semantic domains retain their meaning. |
| Reorder dimensions | Keep semantic entries aligned with the new dimension order. |
| Collapse or group | Use the existing semantic guards; schema-aware reductions record output applicability and explanatory notes. Ordinary collapse does not generally infer a new partition declaration. Remove entries for dimensions explicitly dropped. |
| Select dimensions | Apply collapse, drop, reorder, and rename behavior together; omitted dimensions are aggregated rather than simply discarded. |

For example, filtering an array to selected years narrows applicability to those years. Grouping age categories across
changing source schemas records applicability for the resulting age groups, rather than retaining the original
source-age membership as the output definition. Source provenance remains separate; the semantic slot is not a
complete transformation history or an archived original contract.

The implementation routes `filter()` through the poparray `[` method. When the result remains a `poparray`,
`wrap_subset_poparray()` uses `subset_dim_semantics()` and `pa_subset_applicability()` to keep the semantic entries
aligned with retained dimensions and applicability aligned with the retained labels. `select()` composes guarded
`collapse_all()`, reordering, and dimension renaming, then validates the returned object. Consequently, semantic
updates follow the operations actually performed; selecting every dimension without changing anything can leave
semantics unchanged. These metadata updates do not require reading population values; selection that aggregates
omitted dimensions does perform bounded numerical computation.

### Applicability and structural missingness

`applicability = NULL` retains legacy behavior. Otherwise it is a list with `by` (the controlling dimension name)
and `schemas`. Each schema contains inclusive `from` and `through` controller labels (`NULL` denotes an open end)
and the `levels` applicable in that period. Boundaries follow controller-label order, not numeric year conversion.
The array can therefore contain a union of categories from several source schemas without treating them all as
simultaneously applicable. Structural absence differs from an applicable cell whose population count is missing.
Applicability does not impute or numerically harmonize values. Age operations use interval meaning through `agecraft`.

## Storage and persistence model

A saved single-measure cube normally uses one HDF5 file. An in-memory `poparray` is a delayed object and may instead
reference an existing source plus delayed transformations, or temporary HDF5 results from aggregation:

### HDF5 file

-   Stores numeric values in the `cube/population` dataset.

-   Handles chunking, compression, and on-disk access.

-   The `cube` group contains the `population` dataset and the `metadata` group.

-   Contains semantic metadata in `cube/metadata/*`:

    -   roles (`time`, `area`, `strata`)
    -   `data_col`
    -   source/provenance fields
    -   dimension order and per-dimension labels
    -   current per-dimension semantics, including applicability when present
    -   optional registry fields

-   From this structure, the `open_poparray()` function creates a poparray object.

This separation allows:

-   multiple derived views of the same numeric data,
-   safe sharing across users,
-   metadata evolution without rewriting large arrays.

## Supported operations

Operations may return realized R objects only when explicitly documented (e.g., summaries, plots). Functions that
realize the array are tagged with **EAGER**.

### Basic array interface

These operations behave like the corresponding base R generics and are expected to be cheap (metadata-only) where
possible.

-   `length(x)` Returns the total number of cells (`prod(dim(x))`).

-   `dim(x)` Returns the array dimensions (delegates to the delayed backend).

-   `dimnames(x)` and `names(x)` Returns dimension names and per-dimension labels (stored/maintained explicitly).

### Printing

-   `print(x)` Prints a compact summary including class, source metadata (if present), total length, dimensions, and the
    configured value-column name used by tabular coercions.

### Subsetting ([) or filter()

-   Preserves laziness.
-   Maintains dimensional integrity.
-   Updates metadata consistently.
-   Defaults to drop = FALSE.
-   Returns a `poparray` while time, area, and required applicability controllers remain. With `drop = TRUE`,
    dropping these dimensions can return the underlying delayed result instead.

### Collapsing dimensions

-   `collapse_dim()`, `collapse_all()`, and `group_ages()` aggregate under overlap and applicability guards;
    stratification dimensions are not unconditionally safe to sum.
-   Reductions compute bounded source blocks into temporary HDF5-backed results, rather than retaining an entirely
    unevaluated reduction or loading the entire cube into memory.
-   `drop = TRUE` can remove collapsed singleton dimensions, but required time/area roles and applicability
    controllers are protected. Changing an applicability controller's labels is restricted.
-   Geographic grouping uses explicit labels/mappings; it does not automatically infer a geographic hierarchy.
-   The resulting object remains a valid `poparray` with updated output metadata.

### Selecting, renaming, reordering, and grouping

-   `dplyr::select()` selects dimensions with tidyselect name expressions. Omitted dimensions are collapsed in original
    array order and dropped, except omitted time/area dimensions, which remain singleton axes. These reductions retain
    the existing safety guards.
-   `rename()`, `rename_with()`, and `relocate()` change names/order lazily while keeping dependent metadata aligned.
-   `group_by()` records analysis grouping in `group_vars`; it does not aggregate values. Grouping is an in-memory
    setting, distinct from population strata and from persistent dimension semantics.

### Coercion to tabular formats (EAGER)

These operations typically **realize** the selected data to construct a `data.frame`/`tibble`. For large cubes, slice
first (e.g., subset years/areas) before converting.

-   `as.data.frame(x)` (**EAGER**) Produces a long data frame via `as.table()` semantics with one row per cell.

-   `tibble::as_tibble(x)` (**EAGER**) Produces a tibble, typically by delegating through `as.data.frame()` for
    consistency.

### Plotting

-   plot(x, ...) A base R plot method may be provided (e.g., population pyramid panels). Plotting typically requires
    tabular data and may realize the subset being plotted.
-   ggplot2::autoplot(x, ...) Where implemented, returns a ggplot object (often faceted) and commonly relies on
    converting to a data frame first.

### Splitting

-   split(x, f, drop = FALSE) Splits a poparray by the values of a single dimension f (e.g., split by year or area).
    Returns a named list of poparray objects (or delayed arrays if drop = TRUE causes dimension dropping).

### Summary and numeric operations

-   Scalar reductions read population values and return small realized results; this does not require full-array
    realization in R memory. They can still scan many or all source blocks.
-   `sum()` uses semantic reduction guards. `mean()`, `var()`, `sd()`, `min()`, `max()`, and `range()` use bounded
    blocks and exclude structurally inapplicable cells. Applicable missing values follow the method's missing-value policy.
-   Mean, variance, and standard deviation check overlap risk; extrema do not require disjoint categories.
    These statistics describe population **cell counts**, not the distribution of individual residents' ages.
-   `summary()` currently delegates to DelayedArray reductions and can scan the backend repeatedly. It should not
    be treated as equivalent to the dedicated applicability-aware and guarded statistical methods.
-   Converting the selected cube to an ordinary array or tabular object is **EAGER**; select a manageable subset first.

### Semantic safeguards for reductions

-   `sum(poparray)` is strict by default.
-   Any remaining dimension with derived overlap risk is treated as unsafe for naive aggregation.
-   Overlap risk is derived at runtime from current labels plus intrinsic semantics (`partition_type`, `scale_type`,
    `overlap_levels`), not from stored mutable overlap flags.
-   Default behavior is to error to prevent accidental double counting in epidemiologic workflows.
-   Overrides exist for advanced users:
    -   `strict = FALSE` (warn, then continue)
    -   `allow_overlap = TRUE` (continue silently)

### Saving and persistence helpers

-   `save_poparray(x, filepath, ...)` writes a `poparray` to the canonical cube schema.
-   `create_poparray(x, filepath, series_id, ...)` is a convenience wrapper that adds `series_id` to registry metadata.
-   Saving persists the returned object's current semantic metadata; it does not restore the original source semantics.
-   These functions read `data_col`, `source`, `time_role`, and `area_role` from `x`; callers do not pass those
    separately.

### Persistent view recipes (experimental)

A `poparray_view` is a separate S7 definition for transformations of one registered source series. It is not a
`poparray` and contains no population values. Build it with `new_poparray_view()`, `view_filter()`, `view_step()`,
and `view_group_ages()`. `save_poparray_view()` saves a versioned plain-data RDS recipe, normally under
`views/<view_id>.rds`; `list_poparray_views()` maintains a separate rebuildable view registry.

`read_poparray_view()` reads the definition without replay. `open_poparray_view()` opens the source and replays the
recorded operations, returning a `poparray` whose metadata follows those operations. Aggregations may create
session-temporary HDF5 files that must remain available while the result is used. Reopening recomputes these results
from current source values, rather than restoring a frozen numeric snapshot.

The recipe captures a source compatibility contract separately from the returned array's evolving semantics.
Compatibility is currently strict: even appending years rejects an existing definition. Automatic conversion of an
already-transformed `poparray` into a recipe is not implemented. See [population view notes](notes/poparray_views_poc.md).

### Scope of geographic reconciliation

Geographic lookup tables, differential-privacy reconciliation, benchmarking, and raking are possible external workflow
steps, not guarantees of this class or automatic behavior of its collapse methods. Any such adjustments must be
explicit and their provenance recorded by the workflow.

### What poparray deliberately does not do

-   It does not encode hierarchical geography as multiple dimensions.
-   It does not assume a fixed set of demographic variables.
-   It does not silently reconcile or adjust data.
-   Metadata operations do not force population-value reads. Explicit coercions can realize large arrays; callers
    must subset first rather than relying on an automatic size warning.

### Intended use cases

-   Census and intercensal population analysis
-   Epidemiologic rate calculations
-   Limited population projections
-   Small-area estimation workflows
-   Shared, reproducible population cubes across teams

### Conceptual summary

A poparray is a lazy, role-aware population cube\
where arrays store values,\
metadata stores meaning,\
and role-aware methods enforce dimensional contracts
