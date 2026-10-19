# Linked population estimates and ACS margins of error

Implemented on the `codex/linked-measure-semantics` branch. The earlier shared
measure notes remain design background; the API below is the first implementation.

Construct `poparray_measures(list(population = estimates, population_moe = moes),
metadata)` from two poparrays with exactly matching coordinates, ordering, roles
and dimension semantics. Metadata is a named list with one contract per measure:

```r
compatibility <- list(source = "ACS", product = "acs5",
  period = "five-year period ending at each year coordinate",
  geography = "ZCTA", vintage = "2020", universe = "all people")
metadata <- list(
  population = list(statistic = "count", unit = "people", method = "sum",
    compatibility = compatibility),
  population_moe = list(statistic = "moe", unit = "people", method = "acs_moe",
    compatibility = compatibility, estimate = "population",
    uncertainty_kind = "margin_of_error", confidence_level = .9,
    derivation = "ACS published", status = "published",
    annotations = character()))
x <- poparray_measures(list(population = estimates, population_moe = moes), metadata)
zctas <- dimnames(x@measures$population)$area.name
prefix <- setNames(substr(zctas, 1L, 3L), zctas)
result <- collapse_dim(x, "area.name", prefix)
save_measures(result, "grouped_population.h5") # destination must not exist
result <- open_measures("grouped_population.h5")
selected <- select_measures(result, indices = list(area.name = "760"))
```

These prefixes are user-defined groups of ZCTAs, not official postal delivery
areas. Year coordinates and source period descriptions are retained. Reductions
across the time role are blocked; overlapping ACS releases must not be summed.

The shared grouping and semantic checks are constructed once. Registered count
and ACS MOE methods reuse the blockwise HDF5 reduction kernel. The MOE result is
`sqrt(sum(moe^2))`, marked derived with the covariance limitation in its contract.
See the Census [worked examples, sums and differences, pages 4–5](https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2021_ACS_Accuracy_Document_Worked_Examples.pdf).
This approximation can overestimate or underestimate uncertainty and is not a
pooled uncertainty rule for arbitrary statistics. Supply compatible same-confidence
MOEs; the confidence level is common to a complete measure. Mixed confidence
levels within one array are unsupported.

Missing estimates or MOEs propagate to both paired aggregate outputs. Negative
MOE sentinel values also invalidate both contributors. Import controlled or other
unavailable MOEs as NA and retain their source annotations in the contract;
zero must only represent a numeric zero, never an unavailable-value sentinel.
Annotations are retained as provenance, rather than interpreted as cell values.
Unknown statistics can be stored but cannot be collapsed. Multiple MOEs linked
to one estimate share the strict missingness policy. Selecting only population
removes the uncertainty contract; selecting an MOE without its estimate fails.
Numeric values remain delayed; small block results are realized during reduction.

Each measure lives at `cube/measures/<id>` with one shared coordinate/semantic
metadata bundle. `cube/measure_format` is `1.0.0`; plain R measure contracts are
serialized version-2 integer bytes at `cube/measure_contracts`. Dataset references
and stable IDs are generated from the list names. Shared metadata retains the
existing canonical metadata version; the separate measure marker describes this
additive file format. Use `open_measures()` for these files. Legacy single-measure
files continue to use `open_poparray()` and are never automatically certified or
migrated. Saves stage a new file and refuse overwrites. Existing metadata-history
administration and ingestion APIs are not supported for linked-measure files;
use a new destination to persist transformed bundles. History does not back up
numeric values or measure contracts.

Direct slot inspection provides arrays and contracts. Use `select_measures()` and
`collapse_dim()` for supported transformations; arbitrary single-poparray methods
are not automatically coordinated across a bundle. Percentages, means, medians,
replicate variance and public reducer registration are future extensions.
