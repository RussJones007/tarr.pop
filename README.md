## tarr.pop

An R package for retrieving and managing population figures for epidemiological and public health analysis.

### Overview

`tarr.pop` supports scalable workflows over large, multi-dimensional population cubes stored on disk and accessed lazily. The core data structure is **`poparray`**, an S3 class that wraps a `DelayedArray` backend together with explicit, role-aware dimension metadata.

### Data sources

The package integrates population data from the following sources:

-   Texas Demographic Center (TDC): <https://demographics.texas.gov/>; used for projections and estimates.
-   U.S. Census Bureau: decennial census data, annual estimates, and ZCTA-level estimates.
-   Surveillance, Epidemiology, and End Results (SEER) Program: <https://seer.cancer.gov/>; supplies county and census tract estimates.

### Storage model

Population cubes can be large, so numeric data are typically stored in HDF5-backed arrays (via `HDF5Array`/`DelayedArray`) to avoid loading full datasets into memory. Semantic meaning (dimension roles, provenance, and optional geographic lookup tables) is stored as metadata/attributes rather than encoded as additional array dimensions.

### poparray: core concept

A **`poparray`** represents population counts indexed over **time** and **area**, optionally stratified by other independent (“orthogonal”) dimensions such as age, sex, race, or ethnicity.\
Every `poparray` must include exactly one time dimension and one area dimension; all other dimensions are optional stratifications.

Design principles:

-   Time and area are structural invariants.
-   Other dimensions are optional stratifications and are not assumed to be present.
-   Hierarchical geography is not represented as multiple dimensions; hierarchy belongs in metadata (e.g., a lookup table) and is used for explicit aggregation/collapse operations.
-   Laziness is the default; realization is avoided unless explicitly documented (e.g., tabular coercions, some summaries/plots).

### Core capabilities

`poparray` supports:

-   Array-like interface: `dim()`, `length()`, `dimnames()`, `names()`.
-   Printing: `print()` shows a compact summary including roles, dimensions, and the configured value column used for tabular coercions.
-   Filtering/subsetting: `[` preserves laziness and updates dimension metadata consistently (default `drop = FALSE`), and `filter()` can be used via a `poparray` method.
-   Collapsing dimensions: stratification dimensions may be collapsed freely; collapsing time/area should be explicit and metadata-driven, and area collapsing uses lookup tables (replacing—not adding—the area dimension).
-   Splitting: `split(x, f)` returns a named list of slices by a chosen dimension (e.g., by year or by area).
-   Tabular coercion (**EAGER**): `as.data.frame(x)` and `tibble::as_tibble(x)` realize the selected slice; subset first for large cubes.
-   Plotting hooks where implemented (`plot()`, `autoplot()`), which may rely on tabular coercions and can realize subsets.

### Typical workflow

-   Open a population series into a `poparray`.
-   Filter to the time/area/strata you need using `filter()` (method for `poparray`, stays lazy).
-   Collapse one or more stratification dimensions using `collapse_dim()` (method for `poparray`); time/area collapsing should be explicit and metadata-driven.
-   Convert a manageable slice to a data frame/tibble for downstream analysis (**EAGER**).

### Example usage

\`\`\`r library(tarr.pop)

#### Open an array (returns a poparray)

pop \<- open_poparray("some_series_id")

#### Filter (generic; poparray method)

filtered \<- filter(pop, year == "2025", sex == "Female")

#### Collapse a dimension (generic; poparray method)

collapsed \<- collapse_dim(filtered, "age.char", c("0-4", "5-9", "10-14"))

#### EAGER: realize a manageable slice as a data.frame

df \<- as.data.frame(collapsed)
