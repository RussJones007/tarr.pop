# Shared storage for ACS measures

Discussion: October 13, 2026.

The proposed storage design keeps several numeric measures in one HDF5 file
with a shared set of dimension labels and roles. For example, ACS ZCTA
population estimates and their margins of error share year and ZCTA coordinates.
They should not require two copies of that coordinate metadata.

A possible layout is:

```
cube/metadata/dimnames/year
cube/metadata/dimnames/area.name
cube/metadata/roles/...
cube/measures/population
cube/measures/population_moe
cube/measure_metadata/population/...
cube/measure_metadata/population_moe/...
```

Each numeric dataset has the same shape and coordinate order. A measure-specific
view can expose one dataset as a delayed array while reusing the shared labels.
Separate datasets are preferable to a generic measure dimension when measures
have different units, meanings, or aggregation rules. This is a proposal, not a
change to the current cube schema or API.

Per-measure metadata should describe units, estimate type, confidence level for
margins of error, missing-value conventions, and supported reductions. Counts,
percentages, means, medians, and margins of error must not all inherit population
sum behavior. Percentages and means generally need appropriate denominators or
weights; medians cannot generally be combined from component medians. Margins
of error require a documented uncertainty-combination method and its assumptions,
rather than ordinary summation.

Shared dimensions alone do not establish compatibility. Measures must use the
same geography definitions, time coverage, category definitions, and coordinate
ordering. Reporting-schema applicability can be shared only when it is identical;
otherwise it needs measure-specific metadata. ACS five-year estimates also
represent overlapping periods, so uncertainty calculations need to respect the
source methodology.

Follow-up design work: define a measure catalog and selection interface, preserve
compatibility with existing single-measure population cubes, and specify safe
measure-specific operations before implementing migration or storage changes.

## Intended poparray behavior

Each measure should be stored individually with its own semantic contract,
including valid operations and any required companion measures. A future
`poparray` interface should select a measure, retain the shared dimensional
coordinates and lazy storage, and dispatch operations according to that measure's
rules. Retrieving several compatible measures into a table should produce shared
coordinate columns plus named value columns, such as population and its margin
of error. Their alignment must be validated rather than inferred from equal
array sizes. This describes intended behavior; no new API is implemented yet.

## Future example: align disease counts and population denominators

Discussion addition, October 13, 2026. Demonstrate matching disease-count arrays
and population cubes by year, geography, age intervals, and any other included
strata. rename() can make dimension names consistent and relocate() can match
axis order. Neither operation matches, reorders, or harmonizes coordinate labels.

Before division, independently align and verify each dimension's labels in the
same order, including exact age-interval bounds and terminal groups. Select the
same reporting periods and geographic definitions, and ensure the denominator
represents the eligible population for the numerator. Retain explicit missing
values. Define zero-denominator handling rather than leaving Inf/NaN as rates.

A small teaching example can convert the selected, summarized cube with
as.array(), explicitly labeled EAGER, and compare both dim() and dimnames()
against the disease-count array before dividing. Base array arithmetic uses
positions; matching dimension names alone does not align cells. Document any
scale factor and the numerator's definition (people versus events), denominator
basis, and time period. No rate helper is implemented by this note.

For larger workflows, consider a future alignment-checking interface and delayed
arithmetic that preserves coordinates and updates measure metadata from count
to rate. Such an interface must reject incompatible categories, units, periods,
and universes instead of silently recycling or assuming correspondence.
