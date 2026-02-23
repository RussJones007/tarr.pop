# Cube Storage Schema (Part 1)

This document defines the HDF5 storage layout used by `scripts/migrate_cube_storage_schema.R`.

## Source metadata inputs

Metadata source of truth is the `.rda` objects under `data/`, especially:

- `series_registry.rda` (`series_registry`)
- age label vectors (`ages_*`)
- year label vectors (`years_*`, `zcta_end_year_levels`)
- geography vectors (`county_levels`, `zcta_levels`)
- sex/race/ethnicity vectors

## New per-file HDF5 layout

- `cube/population`: numeric population cube (copied from legacy `/pop`)
- `cube/metadata/schema_version`: schema version string (`"1.0.0"`)
- `cube/metadata/migrated_on`: migration timestamp
- `cube/metadata/series_id`: canonical series identifier
- `cube/metadata/geo`: optional geography tag
- `cube/metadata/extendable_year`: optional extension flag
- `cube/metadata/roles/time`: time dimension name
- `cube/metadata/roles/area`: area dimension name
- `cube/metadata/roles/strata`: remaining stratification dimensions
- `cube/metadata/dim_order`: ordered dimension names
- `cube/metadata/dimnames/<dim>`: labels for each dimension
- `cube/metadata/source/note`: series id
- `cube/metadata/source/source`: source program name
- `cube/metadata/source/updated`: migration date
- `cube/metadata/source/population_type`: `type_key` value

## Series dimname mapping

- `census_decennial_county_1y`: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`
- `census_estimates_county_5y`: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`
- `census_zcta_estimates`: `end.year`, `zip.code`
- `seer_estimates_county_1y`: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`
- `seer_estimates_county_5y`: `year`, `area.name`, `sex`, `age.char`, `race`, `ethnicity`
- `tdc_estimates_county`: `year`, `area.name`, `sex`, `age.char`, `race.eth`
- `tdc_projections_county`: `year`, `area.name`, `sex`, `age.char`, `race.eth`

## Legacy compatibility note

Some legacy cubes include one extra terminal level in dimensions where `.rda`
metadata has one fewer labels. During migration, the script drops that terminal
level from `cube/population` so `"All"` is not stored in the cube.

Older migrated cubes may still include `cube/metadata/registry/*`. The package now
builds discovery rows from canonical `cube/metadata/*` fields first, with legacy
registry fields used only as read-time fallbacks.
