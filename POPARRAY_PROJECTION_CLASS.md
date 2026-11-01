# poparray_projection: Class Purpose and Design Decisions

## Overview and hierarchy

`poparray_projection` is a concrete S4 subclass of virtual `demographic_array`,
which extends `DelayedArray`. It is a sibling of `poparray`, not its descendant.
Both classes share dimensional structure and semantic metadata; projection
uncertainty does not inherit population-count reduction rules.

```text
DelayedArray
└── demographic_array (virtual)
    ├── poparray
    └── poparray_projection
```

## Data and metadata

A projection contains one delayed array, usually HDF5-backed, with a special
`stat` dimension. Its labels are a non-empty subset of `projection` and
`std_error`. Full results from `project()` contain both. Standard errors retain
precision while final population estimates are rounded by the projection engine.
The `stat` dimension is a statistical axis, not a population stratum.

The inherited slots are `time_role`, `area_role`, `strata_roles`,
`dim_semantics`, `data_col`, and `source`. Projection-specific slots are
`level`, `method`, `base_years`, and `created`. Source is a provenance list;
it is not interchangeable with dimension semantics. Legacy S3 list shims remain
for compatibility, but they are not the current S4 representation.

Projection methods obtain the delayed backend through `pp_handle()`. This is
an internal accessor, not a separate `handle` slot. There is no embedded
`poparray` object and no duplication of count and error cubes.

## Validation

The shared superclass checks named axes, dimension extents, distinct existing
time and area roles, semantic alignment, applicability, and the value-column
name. Subclasses identify special axes that are excluded from `strata_roles`.

Projection adds a required `stat` axis, a validated non-additive statistic
contract, a supported forecasting method (`ARIMA`, `ETS`, or `CAGR`), a confidence
level from 0.5 to 0.99, base years, and provenance fields. `stat` cannot be used
as a time, area, or strata role, or renamed as an ordinary demographic axis.
Its overlap declarations follow its retained statistic labels, but it remains
a `set` even when only one statistic remains.

## Shared structural transformations

The superclass provides common subsetting and reconstruction infrastructure.
Existing `filter()`, `slice()`, `slice_head()`, `slice_tail()`, `rename()`,
`rename_with()`, and `relocate()` implementations also dispatch on
`demographic_array`. They preserve the concrete class, projection fit settings,
source metadata, and delayed backend. Dimension semantics and applicability
follow the current labels and role names. Source cube files are not rewritten.

`select()` can reorder and rename a projection's dimensions when all axes are
retained. Omitting axes is rejected: population sums and uncertainty propagation
need different numerical rules, and covariance assumptions cannot be inferred.
For count-only selection and aggregation, explicitly use `as.poparray()` first.
Analysis grouping and count summarisation remain `poparray` responsibilities.

### Return classes when dropping dimensions

- With `drop = FALSE`, required and singleton dimensions remain; a projection
  remains a projection while its valid `stat` axis remains.
- With `drop = TRUE`, removing a singleton `stat = "projection"` axis can return
  a `poparray` if time, area, and required applicability controllers remain
  and the backend satisfies the count class's HDF5 contract. An in-memory
  projection instead returns its ordinary delayed result without forced I/O.
  Forecasting method, confidence level, and base years are retained as
  compatibility attributes on this count-only result.
- Removing a singleton standard-error axis returns the underlying delayed
  result, never a population-count array.
- Removing time, area, or a required applicability controller returns the
  underlying delayed result rather than an invalid demographic object.

## Numerical operations and uncertainty

`projection(x)` and `std_error(x)` return delayed statistic slices, retaining a
singleton `stat` axis. `confint(x, level = ...)` derives delayed lower and upper
bounds from the estimates and errors. Both statistics must be available to
calculate intervals. `as.poparray(x)` explicitly extracts projected counts and
removes only `stat`, preserving other singleton axes and population semantics.
An uncertainty-only object cannot be converted to population counts.

Combined projection objects reject ordinary scalar numerical reductions,
including sum, mean, variance, standard deviation, and extrema. Extract a
statistic explicitly first. Count reductions then use the existing population
semantic guards. Aggregating standard errors requires a covariance model;
neither summation nor a zero-covariance assumption is silently introduced by
this inheritance migration. Existing plotting methods remain separate and are
not a general uncertainty aggregation interface.

Tabular coercions (`as.data.frame()` and `as_tibble()`) are **EAGER**: subset to a
manageable result first. They provide separate projection and standard-error
columns. Structural transformations remain lazy and do not extract numeric
payloads.

## Persistence and compatibility

The projection engine writes one HDF5 dataset, with `stat` last in newly
written output, using series/hyperslab processing. Methods locate axes by name,
so transformed objects need not retain that order. `read_poparray_projection()`
restores labels, semantic contracts, roles, and forecasting metadata without
reading the numeric payload. The inheritance migration does not change the
HDF5 schema. `save_poparray()` continues to accept count arrays only.

Persisted HDF5 cubes can be reopened through their existing constructors.
Serialized in-memory S4 objects created before the hierarchy change should be
recreated through constructors or reopened from their HDF5 files; they are not
a replacement for the storage schema.

## Implementation map

| Responsibility | Source |
|---|---|
| Shared class, accessors, subsetting, reconstruction generic | `R/demographic_array.R` |
| Count class and count reconstruction | `R/poparray_class.r` |
| Projection class, validation, reconstruction, numerical boundaries | `R/projection_class.r` |
| Shared structural S3 verbs | `R/filter.R`, `R/slice.R`, `R/rename.R`, `R/relocate.R`, `R/select.R` |
| Forecast orchestration and writer | `R/projection.r` |
| Projection plotting | `R/projection_plot.r` |

See [the shared contract](notes/demographic_array_design.md) and
[the population class description](POPARRAY_CLASS.md).
