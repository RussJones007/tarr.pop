# tarr.pop 0.0.0.9226 (development version)

Changes since development version 0.0.0.9225.

* Added an experimental S7 `poparray_view` recipe with plain-data RDS
  persistence, fixed operation replay, minimal source compatibility checks,
  and source checkout support. Views record transformations without saving
  population values. Added `view_group_ages()`, default named storage, a separate
  view registry and `list_poparray_views()`, descriptive metadata editing, and
  printing without replay. `init_cubes()` creates a sibling `views` directory.

* Added `select()` for poparray dimensions with tidyselect name helpers,
  ordering, and renaming. Omitted dimensions are collapsed in original array
  order and dropped, while omitted time/area roles remain singleton dimensions.
  Predicate selectors are unsupported; existing overlap and applicability
  guards remain active. Reordering and renaming stay lazy, and reductions use
  bounded HDF5-backed computation.

* Fixed subsetting so conversion of a subsetted poparray to DelayedArray
  preserves dimension names and labels on the underlying delayed object.
* Expanded and refined the introduction with explanations of population cubes,
  array and HDF5 storage, metadata manipulation, and the TDC source.

## Linked-measure branch summary

* Added aligned population estimate and ACS margin-of-error bundles, measure
  selection, shared HDF5 persistence, and coordinated count/MOE reductions.
  Alignment, uncertainty compatibility, missingness, and aggregation guards
  protect these operations; usage notes and regression tests cover the APIs.
* Migrated age interval conversion, grouping, ordering, and overlap checks from
  rage to agecraft throughout package code, source scripts, and examples.
* Preserved dimension names during delayed subsetting and improved introductory
  documentation. The select.poparray() design remains future work.

# tarr.pop 0.0.0.9225 (development version)

Changes since development version 0.0.0.9224.

## Dependency migration

* Replaced rage with agecraft for age interval conversion, ordering, age
  grouping, and overlap checks. Updated data preparation scripts and vignettes
  to use agecraft, and age-group examples to use agecraft::age_group_presets.
* Added regression tests for preset-based grouping, population totals, delayed
  HDF5-backed results, metadata preservation, and infant/open-ended interval
  safety. Existing age filtering, applicability, collapse, and Census vintage
  tests also pass with agecraft.

## Linked population estimates and margins of error

* Added poparray_measures(), select_measures(), save_measures(), and
  open_measures() for aligned population estimates and companion ACS margins
  of error, with shared coordinates and semantics in HDF5 storage.
* Added coordinated collapse_dim() reductions: population counts are summed
  and compatible ACS margins of error use sqrt(sum(moe^2)), with derived
  uncertainty metadata recording the covariance limitation. Missing estimates,
  missing MOEs, and negative MOE sentinels propagate to paired results.
* Enforced alignment and compatibility contracts, blocked reductions across
  reference periods and unsupported statistics, and retained explicit handling
  of legacy single-measure cubes. Added usage documentation and tests for
  storage round trips, selection, safety checks, and fractional MOE results.

# tarr.pop 0.0.0.9224 (development version)

Changes since development version 0.0.0.9223.

## Example cube and documentation

* Regenerated the bundled TDC example for Collin, Dallas, Denton, and Tarrant
  counties in 2015–2018. Named subsetting preserves dimension semantics;
  older source cubes receive documented age and race/ethnicity applicability
  rules when those metadata are absent. Saving and reopening verifies that
  dimension labels and semantics survive the HDF5 round trip.
* Expanded the introduction's explanation of array storage, delayed computation,
  and dimension-label helpers. Removed manual applicability assignment because
  the bundled example now includes the reporting-period metadata.
* Added an open_poparray() internals vignette with call-sequence diagrams,
  support-function explanations, cache and validation limits, and executable
  direct-file, registry, checkout, and cold/warm trace examples. Linked the
  walkthrough from the function help and API reference.
* Clarified the package description's population coverage and delayed workflow.

## Maintenance

* Normalized checkout paths in open_poparray() tests for consistent comparisons
  on Windows and Linux, refreshed generic imports and method registrations,
  and removed obsolete commented utility code.
* Corrected the example script's object-size reference and used an explicit
  filepath argument when reopening the saved cube.

# tarr.pop 0.0.0.9223 (development version)

Changes since development version 0.0.0.9222.

## New features

* Added metadata-only `group_by()` and `ungroup()` for cube dimensions, with
  grouping preserved through lazy axis operations and inspected by `group_vars()`
  and `groups()`.
* Added `summarise()` / `summarize()` for one named population sum, including
  `.by`, output grouping controls, and `na.rm`. Required roles and applicability
  controllers must remain grouped. Overlap is checked before population reads;
  reductions use bounded blocks and temporary HDF5 storage with double sums.
  Structurally unavailable groups remain `NA`, even with `na.rm = TRUE`.
* Added lazy `slice_head()` and `slice_tail()` for first or last coordinates in
  an explicitly named dimension, supporting dplyr's `n` and `prop` size rules.
* Added lazy `rename_with()` for transforming selected dimension names while
  updating roles, semantic names, and applicability controller references.
* Added lazy `slice()` for integer positions in an explicitly named dimension.
  Dimensions and roles are retained, applicability is trimmed, duplicate
  retained positions are rejected, and time order is preserved.
* Added lazy `relocate()` for dimension order with tidy-select selectors and
  `.before`/`.after`. It preserves named semantics and roles without rewriting
  or rechunking the backing HDF5 dataset.

## Documentation and design notes

* Added a long-form `open_poparray()` implementation vignette with call-sequence
  diagrams, support-function explanations, cache and validation limits, and
  executable direct-file, registry, checkout, and cold/warm trace examples.
* Reorganized the API reference into purpose-based categories covering all
  exported functions and supported generic methods. Examples use the bundled
  population cube and explain metadata-only operations, lazy views, bounded
  computation, and EAGER conversions.
* Documented grouping as an in-memory analysis setting, supported summary
  expressions, required role/controller retention, and applicable missingness
  versus structural exclusions.
* Added proposals for ACS/SVI numeric-measure metadata, geographic hierarchy
  and grouping metadata, package separation and TexasPop, and shared storage
  for ACS measures. These notes describe future designs, not implemented APIs.

## Testing and fixes

* Replaced the utility test's local census-series dependency with the bundled
  example cube and exact label/removal expectations.
* Added regression coverage for lazy dimension verbs, grouping preservation,
  overlap preflight, applicability, missing values, empty dimensions, double
  sums beyond the integer limit, and HDF5 round trips.
* The full test suite passed 1,829 assertions with no failures or errors.
  Public help examples and API-reference chunks executed successfully, and
  the reference rendered to HTML.

# tarr.pop 0.0.0.9222

Changes since development version 0.0.0.9221.

## New features

* Cube schema 1.2.0 adds stable cube IDs and metadata revision history inside
  HDF5 files. Metadata setters record snapshots automatically; explicit updates
  accept a reason. Added history, version, snapshot, restoration, and interrupted
  metadata replacement recovery utilities. Rebuilds and appends preserve history
  and record time coverage without retaining earlier population datasets.
* Label helpers now resolve demographic dimensions by semantic domain and
  area/time dimensions by role, with conventional names as demographic
  fallbacks. Added `times()` with `years()` retained as an alias. Label helpers
  and `dim_labels()` accept `missing = "null"` for absent dimensions; errors
  remain the default, and ambiguous domains require explicit selection.
* `age_overlaps()` returns a logical index selecting whole age categories
  overlapping inclusive age bounds. It inspects labels only and supports
  ordinary lazy dimension indexing, including negated selections.
* `rename()` now renames poparray dimensions lazily using dplyr syntax. Roles,
  semantic names, and applicability references follow the rename; age range
  filtering retains its interval meaning after an age dimension is renamed.
* `filter()` now supports unary `!` and text predicates `str_detect()`,
  `startsWith()`, and `endsWith()` on dimension labels, including qualified
  calls and caller-supplied patterns. Negated age ranges complement whole
  category selections. Filtering stays lazy and preserves the restriction
  to one dimension per expression; scalar `&&`/`||` remain unsupported.
* Added blockwise `mean()`, `var()`, `min()`, `max()`, and `range()` methods for
  population cell counts. Structurally inapplicable cells are excluded;
  applicable missing values follow `na.rm`. Mean and variance guard overlap
  risk, while extrema allow overlapping categories. `range(finite = TRUE)`
  excludes non-finite values. Trimmed means and covariance are not supported.
* Standard deviation now shares a stable centered variance accumulator with
  `var()`, excludes structurally inapplicable cells, and guards overlap risk.
* `open_poparray(filepath = ...)` opens an HDF5 cube directly without
  consulting the series registry or requiring a configured cube folder.
  Supply exactly one of `series_id` and `filepath`; direct file opening also
  supports checkout copies and preserves cube metadata and delayed storage.

## Example data and documentation

* Added the small `inst/extdata/tdc_example.h5` cube containing TDC county
  estimates for 2015–2018, including the 2016–2017 age and race/ethnicity
  category changes. It can be located with `system.file()` and opened directly.
  Added a source script for preparing the example cube.
* Removed the older full population cubes from the installed package to reduce
  package size. Full cubes are built with the source scripts and stored in an
  external cube folder; installing the package supplies the small example.
* Rewrote the Introduction vignette as a beginner workflow using Dallas and
  Tarrant for all four bundled years. Examples execute and display actual
  tables and a plot. The guide explains applicability metadata, safe county
  totals, common age groups, and combining Asian and Other categories across
  the 2017 transition without inventing counts or discarding missing values.
* Expanded the package description and added design notes covering array verbs,
  semantic domains, cube metadata versioning, and source-script readability.

## Maintenance and validation

* Refactored Census build and vintage-update scripts and TDC age-support
  handling into smaller helpers and mapping pipelines while retaining source
  ordering and reporting categories. Added regression coverage for download
  cleanup, reader ordering, and separate ACS measure metadata.
* Made the `dim_labels()` help example self-contained with a temporary cube,
  removed a failing `drop_array_level()` example, and guarded TDC support tests
  when source-only helpers are absent from a built package.
* Added regression tests for metadata history and recovery, semantic label
  helpers, age overlap indexing, lazy renaming, negated and text filtering,
  applicability-aware statistics, and direct file opening and checkout.
  Removed tests that depended on the retired bundled full cubes.

# tarr.pop 0.0.0.9221

Changes since development version 0.0.0.9220.

## New features

* `overlaps()` reports declared overlap levels, declared levels still present,
  and current overlap risk for every dimension without reading population
  values. Risk uses the same interval and applicability checks as reductions;
  it can include uncertain overlap rather than positively established overlap.
* `drop_overlap_levels()` removes declared overlap-causing levels from selected
  dimensions using lazy subsetting. It preserves roles and semantic metadata,
  rejects removals that would empty a dimension, and leaves reduction guards
  active. Removing declared levels does not guarantee all overlap is resolved.
* `collapse_all()` accepts multiple dimension names or whole-number indices
  and collapses them sequentially. Selectors are validated before reduction,
  and numeric selectors refer to the original dimension order.
* `collapse_dim()` and `collapse_all()` now accept `drop = FALSE`. With
  `drop = TRUE`, singleton output dimensions selected for collapse are removed
  while other singleton dimensions remain. Required time/area dimensions and
  applicability controllers cannot be dropped. Results remain HDF5-backed
  `poparray` objects.

## Census cube construction and updates

* Added source scripts and an orchestration script for building decennial
  county, annual county estimate, and ACS five-year ZCTA population cubes
  through the package ingestion workflow. County cubes exclude state totals
  and demographic totals and retain separate race and ethnicity dimensions.
* Decennial readers support the processed Parquet cache or Census API inputs,
  including differences in label formats. Derived Hispanic counts reject
  incomplete source pairs and negative results.
* Annual estimate builds combine final 2010-2019 intercensal July estimates
  with July estimates from 2020 through the configured vintage. Independent
  source support checks reject missing expected demographic combinations;
  overlapping race alone-or-in-combination categories are recorded explicitly.
* ACS ZCTA builds discover consecutive published ACS5 end years and store
  population estimates and 90% margins of error in separate cubes with aligned
  keys. Unreported codes and negative source sentinels remain `NA`, not zero.
  Notes describe overlapping ACS periods, geographic limitations, and the
  non-additivity of margins of error.
* Added a documented Census vintage replacement workflow in `data-raw/`.
  It retains 2010-2019 history, replaces all July estimates from 2020 through
  the incoming vintage, aligns categories by labels, and writes a separate
  validated candidate using blockwise HDF5 copying. A separate installation
  step supports backup and rollback.

## Fixes and usability improvements

* Restored the packaged `population` lookup list.
* Collapse and sum overlap errors now recommend `overlaps()`, explicit removal
  of declared overlap levels, or filtering before retrying. Grouping errors
  explain how to repair source-label mappings or use `keep_unmapped` options.
* Consecutive applicability schemas with identical category sets are compressed
  into ranges in Census builds and vintage updates, preserving coverage,
  category transitions, and explicit or open endpoints.

## Documentation and validation

* Clarified that `DimSemantics` validation denotes canonical-source or reviewed
  dimension definitions, independently of value accuracy, completeness, and
  aggregation safety. Documented preservation during TDC annual updates and
  HDF5 persistence, plus limitations of locally selected ACS ZCTA coverage.
* Expanded Census build/update guides, collapse and overlap help, and the cube
  creation vignette. Archived superseded population-processing scripts.
* Added regression coverage for Census transformations and vintage replacement,
  overlap inspection/removal, applicability compression and persistence,
  multi-dimension collapse, and optional dimension dropping.
