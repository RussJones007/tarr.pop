---

editor_options: 
  markdown: 
    wrap: 72
---

# Census cube builds

Run from the package directory:

``` r
source("data-raw/control_def.r")
source("data-raw/census_data_2.r")
```

The script rebuilds these series under the configured cube root's `base/` directory, then rebuilds the cube registry:

| Series | Source | Dimensions |
|------------------------|------------------------|------------------------|
| `census_decennial_county_1y` | 2000/2010 SF1 and 2020 DHC | year, area.name, sex, age.char, race, ethnicity |
| `census_estimates_county_5y` | Final 2010s intercensal and Vintage 2025 annual estimates | year, area.name, sex, age.char, race, ethnicity |
| `census_zcta_estimates` | ACS five-year B01003 population | year, zip.code |
| `census_zcta_estimates_moe` | ACS five-year B01003 90% margin of error | year, zip.code |

All 254 Texas counties are included; the Texas total and demographic `All` rows are excluded.
Race and ethnicity remain separate dimensions.
Annual estimates retain all 11 detailed race categories, including five overlapping alone-or-in-combination categories declared in `DimSemantics`.

The default decennial reader uses the existing processed `Census/Census_2000_thru_2020.parquet`.
Set `cache_file = NULL` to use the tidycensus reader.
API parsing supports both colon-free 2000 labels and later label formats.
Hispanic counts by race are derived as race total minus non-Hispanic counts, as in the original script; incomplete pairs or negative derived counts fail rather than being adjusted.

The default annual CSV directory is `<cube_root>/source-data/census`.
Missing files are downloaded from the Census Bureau.
`input_dir` can instead point to an existing downloaded-file directory.
The final intercensal file maps codes 2–11 to July 1, 2010–2019; codes 1 and 12 are April 1 observations and excluded.
The newer file maps code 2 to July 1, 2020 and continues through the configured vintage.
The `5y` series suffix describes age groups, not five-year estimate periods.
Support is generated independently from the published annual schema; missing expected combinations fail ingestion.

The ZCTA reader preserves the union of the spatial-file codes, Michelle's list, and the additional codes (93 identifiers in the current inputs).
It discovers consecutive ACS five-year end years from 2011 through the latest published Census API release, currently 2024.
Population and MOE cubes share the same keys.
Unreported codes and negative Census sentinel values become `NA`, never zero.
Catalog or reader failures are surfaced as errors.

ACS periods overlap in time, and margins of error are not additive.
These limitations are recorded in semantic notes.
The current class requires time and area roles to be partitions, so its overlap guards cannot enforce these two restrictions.
Do not sum across ACS period end years or add MOE values; uncertainty aggregation requires a separate statistical method.
ZCTA boundaries may change between releases, and the areas are Census ZCTAs rather than USPS delivery ZIP codes.

All dimension entries have `validated = TRUE`: their definitions follow canonical Census
source schemas and the documented mappings, including derived Hispanic counts.
This flag does not certify individual values, completeness, or safe aggregation.
For ZCTAs it describes Census geography and ACS periods, not independent validation
of the locally assembled Tarrant-area selection. Missing observations can remain `NA`.
Applicability updates and HDF5 persistence preserve the flag; overlap guards remain active.
Consecutive years with identical applicable category sets are stored as inclusive
`from`/`through` ranges. Category changes retain separate ranges, and compression
preserves explicit endpoints without extending source coverage.

Source reads, transformations, and ingestion's dense array construction are **EAGER**.
Stored cubes reopen lazily with HDF5-backed data.

To load helpers without writing cubes:

``` r
options(tarr.pop.census_build = FALSE)
source("data-raw/census_data_2.r")
cube_root <- tarr.pop::init_cubes()
build_census_decennial(cube_root, cache_file = NULL)
build_census_estimates(cube_root, vintage = 2025L)
build_census_zcta(cube_root)  # latest published release
tarr.pop::rebuild_poparray_registry(cube_root)
```

Helpers that could be reused in other build scripts include `census_county_names()` (FIPS-to-canonical-name resolution), `census_age_factor()` (parse unique interval labels once), `census_support_table()` (year-specific strata crossed with expected counties), `census_dimension_semantics()` (applicability metadata), and `ingest_census_table()` (standard table-to-cube ingestion).
They remain local functions; no exported package API or NAMESPACE changes are required.

Offline tests are in `tests/testthat/test-census-build.R`:

``` r
devtools::test(filter = "census-build")
```

Source references:

- [Final intercensal file layout](https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/intercensal/county/CC-EST2020INT-ALLDATA.pdf)
- [Vintage 2025 file layout](https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2025/CC-EST2025-ALLDATA.pdf)
- [ACS five-year API releases](https://www.census.gov/data/developers/data-sets/acs-5year.html)
