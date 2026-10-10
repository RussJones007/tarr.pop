# Updating Census annual estimates with a new vintage

Run from the package directory against this checkout. The example is in
`data-raw/census_update_estimates.r`. Sourcing it only defines functions; it does
not replace a cube automatically.

A new Census vintage revises earlier estimates as well as adding a year. For
Vintage 2025, retain the existing cube's 2010-2019 values and replace every July
estimate for 2020-2025 from `cc-est2025-alldata-48.csv`. Do not combine a newly
appended 2025 year with unrevised 2020-2024 values.

The [official Vintage 2025 file layout](https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2025/CC-EST2025-ALLDATA.pdf)
identifies YEAR 1 as the April 2020 estimates base, which is excluded. YEAR 2-7
are the July estimates for 2020-2025. AGEGRP 0 and Texas totals are excluded.
Five overlapping race-combination categories are retained, with separate
ethnicity; existing aggregation guards remain active. The `5y` series suffix
refers to age groups, not ACS five-year periods.

## 1. Load helpers and create a candidate

```r
source("data-raw/control_def.r")
source("data-raw/census_update_estimates.r")
cube_root <- tarr.pop::init_cubes()
input_dir <- file.path(tarr::paths$population, "Estimates", "Census")
candidate <- update_census_estimate_vintage(
  cube_root = cube_root,
  input_dir = input_dir,
  vintage = 2025L
)
```

This uses your normal source folder. If the CSV is missing, it first copies the
previously downloaded file from `<cube_root>/source-data/census`; otherwise it
can download from the Census Bureau. Existing CSVs are never overwritten. Set
`download_missing = FALSE` to prohibit downloading; copying a cached file is
still permitted. A preexisting candidate is also never overwritten: remove it
explicitly or supply a different `output_filepath`.

The candidate is under `<cube_root>/staging/`, outside the registered base
series. The production file is unchanged. The incoming rows must cover every
July year from 2020 through the vintage. Existing county selections are retained,
and all other labels must match. Missing expected source combinations,
duplicates, invalid counts, changed categories, or attempts to drop newer
existing years fail. The original cube must have consecutive years from 2010
through at least 2019 and the six-axis county-estimates schema.

The candidate preserves dimension order, label order, roles, value column,
series identifier, intrinsic semantics and available geographic/extendability
metadata. Applicability keeps the historical definitions, adds the replacement
period, and compresses identical adjacent schemas. Source metadata identifies
the replacement file and vintage while retaining previous notes.

## 2. Review the candidate

```r
candidate_metadata <- tarr.pop::get_cube_metadata(candidate)
print(candidate_metadata)
print(tarr.pop::dim_semantics(candidate))
```

Confirm the year labels, source note, retained race categories, and applicability
ranges. Check selected county/year/age values against the incoming CSV, including
July 2020 and 2025. Avoid summing all 11 race categories: five overlap.

The example validates metadata by reopening the staged HDF5 file before exposing
it as a candidate. Automated fixture tests compare every preserved historical
value and every replacement value. Metadata validation alone does not independently
certify a real source file's counts or provenance.

## 3. Install only after review

Close objects opened from the original cube, then run:

```r
base_file <- file.path(cube_root, "base", "census_estimates_county_5y.h5")
backup_file <- file.path(cube_root, "backups", paste0(
  "census_estimates_before_v2025_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".h5"
))
install_census_vintage_candidate(
  candidate = candidate,
  cube_path = base_file,
  backup_path = backup_file,
  cube_root = cube_root
)
updated <- tarr.pop::open_poparray("census_estimates_county_5y")
```

Installation moves the original to the specified new backup, then moves the
candidate into place and rebuilds the registry. The backup is not overwritten.
Keep these paths on the same filesystem. If candidate installation fails,
restoring the original is attempted and any failed rollback reports the backup
location. The two renames are not a single atomic transaction. If registry
refresh fails after installation, the installed cube and backup remain intact;
retry `tarr.pop::rebuild_poparray_registry(cube_root)`.

## Architecture and future use

`add_population_data()` intentionally rejects overlapping year labels. This
script explicitly discards the revisable period from a lazy view of the old
cube, then uses its underlying blockwise HDF5 writer to concatenate retained
history and the incoming replacement period into a new file. It does not patch
a live HDF5 dataset or realize the full old cube.

CSV reads, transformations, and the incoming-table ingestion are **EAGER**.
Historical reads and output copying are blockwise; reopened cubes remain lazy.
The script uses internal package helpers and is an example for this checkout,
not a new exported package API.

For a later 2020-based vintage, change `vintage` only after verifying the Census
file layout and source categories. A future base-decade or classification change
requires revisiting the replacement boundary and schema rather than simply
changing the vintage argument. This workflow does not update decennial or ACS
cubes and does not create the proposed multi-measure metadata design.

Offline tests:

```r
devtools::test(filter = "census-vintage-update")
```
