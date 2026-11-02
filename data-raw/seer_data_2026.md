# SEER February 2026 county cube rebuild

`seer_data_2026.r` replaces the population-estimate portion of the legacy
`seer_def.r`. The legacy file is retained for reference; standard populations
are outside this rebuild. Sourcing the new file only defines functions. It does
not load `control_def.r`, clear the workspace, download data, change cube paths,
write package datasets, or automatically rebuild cubes.

## Input and output

Use the Texas **1990–2024 expanded bridged-race by Hispanic-origin** files from
the [February 2026 release](https://seer.cancer.gov/popdata/download.html):

- `tx.1990_2024.singleages.through89.90plus.adjusted.txt.gz`
- `tx.1990_2024.20ages.adjusted.txt.gz`

The reader follows the [26-byte source dictionary](https://seer.cancer.gov/popdata/popdic.html):
year 1–4, state 5–6, state FIPS 7–8, county FIPS 9–11, race 14,
origin 15, sex 16, age 17–18, and population 19–26. It rejects unexpected
codes, non-Texas records, malformed lines, duplicate keys, unsorted or missing
years, and missing county or demographic-axis coverage.

Output names remain `seer_estimates_county_1y.h5` and
`seer_estimates_county_5y.h5`. Both contain these axes:

| Axis | Values |
| --- | --- |
| `year` | 1990–2024, July 1 estimates |
| `area.name` | All 254 Texas counties; **no Texas aggregate** |
| `sex` | Female, Male |
| `age.char` | 91 single ages (0–89, 90+) or 20 groups (0, 1–4, 5–9, …, 85–89, 90+) |
| `race` | White, Black, American Indian/Alaskan Native, Asian or Pacific Islander |
| `ethnicity` | Non-Hispanic, Hispanic |

The existing race label spelling is retained for compatibility. Source codes
are mapped explicitly, rather than inferred from the order records appear.
Age labels are canonicalized with `agecraft`; `90+` represents `[90, Inf)`.
State totals should be calculated from counties at runtime.

## Semantics, sparse counts, and applicability

Each axis is a validated partition with no declared overlap levels. Race is
SEER's four mutually exclusive **bridged** race categories; it is not the Census
alone-or-in-combination classification. Hispanic origin remains an independent
axis, and each joint race/origin cell has a valid meaning in every year.

The new grouped-age file applies the 20-category schema to the **entire**
1990–2024 series, including historical years. The old cube's 19 categories are
not mixed into this series. Every non-time axis receives an explicit single
applicability schema bounded by the first and last source years. No structurally
inapplicable cells are introduced; no historical race or age transition is
invented. If a future release changes categories or coverage, its mapping and
support must be reviewed before adapting this script.

The downloaded files are sparse positive-count records (no explicit zero rows).
The builder uses zero for absent keys **inside this release's declared full
county/sex/age/race/origin support**. This is an explicit source-specific
interpretation, not the package's default missing-data policy. Missing complete
years/counties/axis labels fail, and every grouped-age cell is compared with the
sum of the corresponding single ages before either destination is installed.
This catches source mismatches and inconsistent omissions; it cannot prove that
a shared omission in both source files is correct. Original gzip integrity and
source provenance still matter. Source filenames and MD5 checksums are recorded
in each cube's source note.

## Run against this checkout

From the package project directory:

```r
# Set before load_all() when using an explicit cube root without startup setup.
Sys.setenv(TARR_POP_SKIP_CUBE_SETUP = "true")
devtools::load_all()
source("data-raw/seer_data_2026.r")

# First create independent candidates in a separate root.
paths <- build_seer_2026(
  cube_root = "/tmp/seer-2026-review",
  input_dir = "~/R/Projects/Population/Estimates/SEER"
)
lapply(paths, tarr.pop::cube_version)
lapply(paths, function(p) dimnames(tarr.pop::open_poparray(filepath = p)))

# Explicit full replacement of the base cubes, after candidate review.
paths <- build_seer_2026(
  cube_root = "~/R/Projects/Population/Cubes",
  input_dir = "~/R/Projects/Population/Estimates/SEER",
  overwrite = TRUE
)
tarr.pop::rebuild_poparray_registry("~/R/Projects/Population/Cubes")
```

The build stages and cross-validates both resolutions before installing either.
Default `overwrite = FALSE` prevents accidental replacement. Each canonical
write uses the current schema (**1.2.0**), current semantic serialization,
a stable cube ID and revision history. A rebuild of a legacy cube adopts its
previous metadata as a baseline, then records the new coverage and semantics.
The canonical writer stages each replacement in the destination directory;
the two cube replacements are **not a joint transaction**. Do not run concurrent
writers. The registry refresh is explicit and does not change the active cube
path configuration.

This is a full vintage rebuild, not a 2024 append. The release includes revised
2010–2019 intercensal and 2020–2024 estimates; all source years are reread.
Metadata history retains descriptions of previous coverage and semantics,
**not previous population values**.

Source parsing and annual arrays are **EAGER**, bounded to one year plus the
input chunk. A full six-dimensional source table is never constructed. Annual
arrays are written to a temporary HDF5 dataset, then the canonical writer copies
blocks into the cube. Opening finished cubes and copying their values remain
HDF5-backed. Staging needs disk space for both candidates and temporary writes.

The script uses internal cube-writing and semantic helpers and should be run
against this checkout. Tests in `test-seer-2026.R` exercise code mapping, sparse
completion, missing/duplicate coverage, interval semantics, applicability,
canonical round trips, and cross-resolution numerical reconciliation.
