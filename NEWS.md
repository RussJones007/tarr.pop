# tarr.pop 0.0.0.9221

Changes since development version 0.0.0.9220.

## New features

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
