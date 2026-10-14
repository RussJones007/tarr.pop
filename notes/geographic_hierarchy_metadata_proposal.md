# Geographic hierarchy and grouping metadata proposal

Discussion and source review: October 13, 2026. Proposed design only; no new
metadata schema, writer, hierarchy accessor, or aggregation API is implemented.

## Census relationships and identifiers

Standard state/county geography supports nesting blocks within block groups,
block groups within tracts, tracts within counties, and counties within states.
Use Census summary-level codes as identifiers of geographic presentations, not
as an ordinal depth or numerical aggregation rule. Examples include 040 state,
050 state/county, 140 state/county/tract, 150 state/county/tract/block group,
and 310 metropolitan/micropolitan statistical area. Record the source-product
specific code for blocks rather than assuming one code is universal.
Sources: [Census summary levels](https://www.census.gov/newsroom/blogs/random-samplings/2014/08/understanding-geographic-relationships-geographic-summary-levels.html),
[geography API](https://api.census.gov/data/2024/geoinfo/geography.html), and
[standard geographic definitions](https://tigerweb.geo.census.gov/tigerwebmain/TIGERweb_geography_details.html).

Store GEOIDs and summary-level codes as strings to retain leading zeros.
Names are display labels; they are not sufficient join keys. A geography's
identity must include geographic type, identifier, and boundary vintage.
Do not infer parentage from code prefixes for arbitrary geographic systems.

CBSAs use county-based delineations and belong in a separate grouping scheme;
they do not fit between county and state in the standard hierarchy. Obtain
membership from the relevant published delineation release. Custom service
regions can use the same grouping interface but need their own identifiers and
provenance. Sources: [metro/micro definitions](https://www.census.gov/programs-surveys/metro-micro/about.html)
and [delineation files](https://www.census.gov/programs-surveys/metro-micro/about/delineation-files.html).

## Proposed logical records

1. Entity catalog: entity_key, geoid, geography_type, summary_level,
   display_name, boundary_vintage, and source definition.
2. Coordinate binding: cube area label/index to entity_key, with time
   applicability if the cube combines different boundary vintages.
3. Relationship catalog: scheme_id, child_entity_key, parent_entity_key,
   relationship_type, valid_from/through, delineation_version, and provenance.
4. Scheme definition: standard containment versus custom membership; whether
   memberships are exclusive and exhaustive; compatible coordinate vintages.
5. Coverage information: expected members from the authoritative scheme,
   distinct from members actually present in the current cube or selection.

Use one immediate parent per child within a strict containment scheme and its
validity period. The catalog can hold multiple schemes: county-to-state,
county-to-CBSA, and county-to-custom-region are distinct relationships.
General memberships may overlap; never treat them as a partition by default.
Fractional spatial crosswalks need separately defined weights, weight basis,
and uncertainty. A containment relation must not imply permission to allocate
a coarse observation into finer units.

## Candidate HDF5 layout

Keep area as a single cube dimension at the source observation level. Do not
add state, county, tract, and block as independent Cartesian-product axes.
Keep aggregate parents out of the source value array, avoiding simultaneous
storage of a county and its state total as additive peer categories.

One possible column-oriented metadata layout is:

```
/cube/metadata/geography/entities/<column>
/cube/metadata/geography/coordinate_binding/<column>
/cube/metadata/geography/relationships/<column>
/cube/metadata/geography/schemes/<scheme_id>/...
```

This layout is illustrative. The current schema 1.2.0 does not support these
records. A formal extension must update validated metadata bundles, history
snapshots, save/open round trips, and legacy handling together. A shared external
geographic catalog is a later option; cubes should retain sufficient versioned
relationships for reproducibility rather than depend on an unversioned lookup.

Relationship arrays can be read independently of population values and indexed
by source coordinate. Large block-level catalogs may themselves need indexed
or on-demand reads; metadata should not be assumed negligible in size.

## Proposed rollup behavior

Resolve the requested grouping scheme and vintage first, then map current source
coordinates to target entities. Require matching source products, periods,
universes, and geographic definitions, plus an additive measure contract.
Check disjoint membership, duplicate coordinates, cycles, and reference integrity.
Compare current contributors against authoritative expected members; a subset
must not masquerade as a complete official area total. Missing observations are
not zero. If partial totals are allowed, label and record their incomplete coverage.

Perform the numeric reduction in bounded blocks using the existing HDF5-backed
collapse machinery where compatible. Return a cube whose area coordinates are
the target entities, retain time/demographic dimensions, and record grouping
scheme, membership version, source level, coverage, and transformation provenance.
No eager conversion to a full array or long table is needed to discover the map.

Separate aggregatable components from coarser source-published totals. A derived
sum should not claim to reproduce a published total unless the source's methods
and reconciliation guarantees establish that relationship. Do not alter released
counts merely to impose agreement. Numerical comparability and geographic
containment are separate contracts.

Counts can sometimes roll up by summation. Margins of error, percentages,
medians, and SVI ranks need their measure-specific rules; geography membership
alone does not make them additive. A tract-level SVI rank must not be summed to
produce a county SVI rank.

ACS does not publish block estimates; geographic metadata cannot create missing
block-level ACS data. Use the actual finest available level for each source/table,
with no implicit disaggregation. Sources: [Census blocks and ACS](https://www.census.gov/newsroom/blogs/random-samplings/2011/07/what-are-census-blocks.html)
and [ACS five-year coverage](https://www.census.gov/data/developers/data-sets/acs-5year.html).

## Suggested implementation sequence

Begin with a tiny county-to-state scheme and a county-to-custom-region scheme.
Define entity IDs, coverage, versioning, and validation before introducing a
user-facing rollup function. Test complete and partial selections, missing counts,
overlapping regions, renamed/relocated area axes, changing memberships, save/open,
metadata history restoration, and bounded-memory numeric reads. Add block/tract
support after the same contract is stable. Treat cross-vintage geographic
harmonization as separate work, not an ordinary rollup.
