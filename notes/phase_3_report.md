# Phase 3 — Schema-aware harmonization and structural missingness

## Phase 3 algorithm

Applicability determines membership before missingness is evaluated. The reducer resolves each current schema's controller indices, builds source-column contributors separately for each output/schema, checks those contributors for overlap, and dispatches bounded numeric rows by their controller schema. It sums only selected columns with `rowSums(..., na.rm = FALSE)`. Empty contributor lists produce `NA`. The existing target-last delayed permutation, bounded extraction, HDF5 realization sink, viewport writes, and inverse permutation remain in use. The original source is never rewritten.

The legacy `applicability = NULL` branch retains its global sparse mapping and existing reduction behavior. Public generic and method formals remain unchanged.

## Root cause of prior structural-NA behavior

Inspection before editing found a single source-to-output mapping in `collapse_dim_poparray_impl()`. `normalize_groups()` produces one mapping across the union of labels, and one sparse matrix is applied to every row without consulting applicability. Thus, union grouping includes later inapplicable `85+` alongside applicable detailed older ages, and its structural `NA` contaminates that group's sum. Phase 2 prevented changed applicability target/controller labels entirely, so the global numerical path did not yet support harmonization.

The correction is membership selection, not missing-value removal. Inapplicable columns never enter the schema's reduction, irrespective of whether their values are `NA`, zero, or nonzero.

## Implementation

Inspected `R/collapse.R` (generic, implementation, normalization, declared levels, semantic guard, sparse mapping, block sizing/ranges, delayed permutations, realization sink, metadata wrapping), `R/group_ages.R` (`group_ages()` and `group_array_by_levels()`), `R/dim_applicability.R` (Phase 2 validity, index resolution and subsetting), `R/poparray_class.r` (constructor, validity, semantic subsets, roles), `R/poparray_semantic_reductions.r`, `R/filter.R` and `R/utils.r` (interval utilities and source access), `R/cube_metadata_admin.R` (source normalization), and relevant Phase 1/2 grouping, overlap, class, persistence and applicability tests. Later inspected `data-raw/tdc_estimates.r`, `data-raw/tdc_estimates_support.r`, `data-raw/tdc_add_estimate_year.r`, and TDC tests.

Code discovery used the indexed `home-russ-R-Projects-tarr.pop` MCP graph, including `search_graph` and `get_code_snippet`; local reads supplied surrounding implementation and test details.

New internal helpers `pa_collapse_schema_plan()` and `pa_collapse_schema_block()` build small metadata plans and perform bounded row dispatch. `group_ages()` builds a private contributor plan on the group specification; no public argument or accessor was added. The generic collapse engine consumes the plan without age-specific numerical logic. `group_array_by_levels()` continues to delegate through `collapse_dim()` and inherits applicability-aware explicit mappings.

The Phase 2 target-label guard has been replaced by schema-aware processing. Changing an applicability **controller's** labels remains blocked when its mapping changes labels, because that requires defining how contexts themselves merge. Identity controller mappings and supported renames continue to work.

Installed API versions verified during development: DelayedArray 0.36.0, HDF5Array 1.38.0. Existing working permutation, sink and viewport APIs were retained. `rage::as.age_group()` plus `ivs::iv_start()`/`iv_end()` were checked locally for half-open bounds, including `< 1`, `85 +`, and `95 +`.

## Structural vs genuine missingness

Synthetic HDF5 fixtures use early `70-74=10`, `75-79=20`, `80-84=30`, `85+=160`, and later `70-74=10`, `75-79=20`, `80-84=30`, `85-89=100`, `90-94=40`, `95+=20`. Inapplicable source cells are stored as `NA`.

| Output | Early schema | Later schema |
| --- | --- | --- |
| `85+` | direct source `85+` = 160 | 100 + 40 + 20 = 160 |
| `70+` | 10 + 20 + 30 + 160 = 220 | 10 + 20 + 30 + 100 + 40 + 20 = 220 |
| `90+` | non-derivable, `NA` | 40 + 20 = 60 |

Replacing an applicable later `90-94` contributor with `NA` makes derived `85+` and `70+` genuinely missing. The later inapplicable source `85+ = NA` is excluded. Tests also preserve applicable zero and assert source HDF5 population values are unchanged.

## Age derivability

`pa_age_contributors()` obtains half-open interval bounds from `rage`/`ivs`. An exact applicable source interval is used directly. Otherwise, only whole applicable source intervals contained in the target can contribute. Their ordered union must begin at the target lower bound, continue without gaps and reach the target upper bound. Overlap candidates remain subject to the normal strict/explicit override guard. A coarser source interval cannot be split to obtain a narrower target.

Requested output intervals must not overlap, preserving the grouping contract that a source interval cannot contribute to multiple output groups. Review caught and corrected a potential partition-safety bypass from requesting both `85+` and `90+`. This rejection also applies when `allow_overlap = TRUE`, whose meaning remains permission for overlapping **source contributors** within a group.

The existing overlap parser now falls back to verified `rage`/`ivs` bounds for canonical labels such as `< 1`; unrecognized intervals remain conservatively unsafe.

With applicability, requested age targets remain present even when non-derivable throughout the selected span. This is documented separately from legacy `keep_empty` behavior.

## collapse_dim behavior

Explicit mappings intersect declared members with the current schema's applicable source labels. A union list containing both broad early and detailed later categories therefore uses different contributor sets in each context. A group without any applicable declared member receives `NA`, never an invented component of a broader category.

Actual contributors retain strict, warning and explicit `allow_overlap` safeguards. Non-derivability generates one summarized warning per operation, naming groups and represented controller ranges; it does not change `strict`. Existing unmapped-label warnings are separate mapping diagnostics.

## Result applicability

Each output schema lists only groups with derivable contributors. A uniformly derivable `85+` or `70+` result has one full-span schema containing its output groups. A partial `90+` result retains an empty early schema and a later schema containing `90+`. Uniform partial membership simplifies to one full-span schema with only its derivable groups. Derived results retain operational applicability even when uniform: erasing it would make a subsequent request for `90+` from harmonized `85+` use the legacy overlap mapping and fabricate a split. A chained-grouping regression verifies that this request instead warns and returns `NA`. This uses the unchanged Phase 2 list contract and leaves legacy NULL objects unchanged.

Operational applicability describes the result. Small transformation notes use existing `DimSemantics@notes` and source `note` fields. Source notes record the dimension, requested output labels and whether source level domains differed. They are one scalar string, matching the existing fieldwise persistence convention. No parallel provenance framework or per-cell metadata was introduced.

## Subsetting composition

Tests cover early-only, late-only and cross-transition `dplyr::filter()` and base `[` before grouping. Dispatch uses only the current object and its pruned schemas. A reordered non-time controller after additional dimensions demonstrates named-dimension dispatch and current label positions. Removing every age source level yields a non-derivable all-`NA` age target without using historical labels.

Extra area and sex coordinates are independent; genuine missingness in one coordinate does not contaminate other rows or contexts.

## TDC/source-specific findings

Repository evidence establishes 2017 for both transitions. `data-raw/tdc_estimates.r` states that Asian was included in Other during 2011–2016 and separately reported from 2017. Its support-construction comments describe early ages ending at `85 +` and later ages ending at `95 +`. `tdc_estimate_support_table()` already implements `transition_year = 2017L` and the corresponding single-age domains.

The build script now calls `tdc_estimate_semantics(support_table)`. Its optional internal support parameter builds age and race applicability from represented years/labels using those existing rules. Early race excludes separately reported Asian, without extracting Asian numerically from Other. Boundaries use actual represented labels; early-only, later-only and transition spans are tested. Canonical source labels and numeric source values are unchanged. The annual update script was inspected but not changed; the existing later schema's open endpoint represents subsequent years under the same established domain.

## Persistence

Derived results remain HDF5-backed and preserve dimnames, roles and data-column metadata. Existing save/open tests verify derived values, semantic objects, applicability and source provenance. The original source cube remains source-faithful. No persistence schema or dependency was added.

## Laziness / memory behavior

Only small labels, interval bounds, schema IDs and grouping lists are eager. Population values are extracted only by the existing bounded block loop; there is no full population array/data-frame conversion for schema discovery. The configured block shape accounts for the larger of source and output widths in the schema-aware path. The regression instrumentation traces returned `extract_array` values for HDF5ArraySeed and forces a small budget: multiple reads occur, no extraction exceeds 14 cells, and no extraction realizes the complete 112-cell fixture. Zero-length validity probes do not count as numeric reads.

Bounded block-to-array and block-to-matrix conversion remains intentional. Temporary numeric submatrices for selected schema/group contributors are bounded by that block. This is not a guarantee that all simultaneous allocations total exactly the configured input block bytes.

## Regression tests added

`tests/testthat/test-schema-harmonization.R` covers harmonized `85+`/`70+`, structural and genuine missingness, non-derivable `90+` and warnings, exact categories, gaps, strict/override overlap, explicit union collapse, output applicability, filter/base compositions, extra dimensions, named/reordered controllers, nominal schemas and impossible Asian decomposition, source preservation, zero, HDF5 save/open, legacy NULL behavior, bounded reads, overlapping-output rejection, entirely removed source levels and chained grouping of uniform derived results.

`tests/testthat/test-tdc-applicability.R` verifies repository-established transition metadata across early-only/later-only/cross-transition spans and canonical infant/open age interval handling. Existing Phase 2 tests were updated only where target grouping is now supported and identity target mappings now expose structural non-derivability.

Added **18 tests / 103 passing expectations**: 16 harmonization tests / 77 expectations and 2 TDC applicability tests / 26 expectations. The existing applicability test file retains 19 tests / 128 expectations after updating the superseded Phase 2 expectations.

## Focused test results

The focused suite passed **121 tests / 525 expectations**, with zero failures, errors, warnings or skips. Log: `/tmp/tarr-pop-phase3-focused.log`; results: `/tmp/tarr-pop-phase3-focused-results.rds`. A final harmonization-file follow-up passed **16 tests / 77 expectations**, with zero failures, errors, warnings or skips, checking the final provenance text as well. Log: `/tmp/tarr-pop-phase3-harmonization-final.log`; results: `/tmp/tarr-pop-phase3-harmonization-final-results.rds`.

## Full source test results

The full source suite passed **200 tests / 856 expectations**, with zero failures, errors, warnings or skips. It includes partial-result persistence and chained uniform-schema derivation. Log: `/tmp/tarr-pop-phase3-full.log`; results: `/tmp/tarr-pop-phase3-full-results.rds`.

## R CMD check

The final package check completed with **3 errors / 9 warnings / 5 notes**, matching Phase 2. It does not pass because the pre-existing package errors remain. The final command regenerated documentation, enabled vignette build/check, disabled PDF/manual checking, and used temporary Quarto/Deno caches. All final runtime code and all final tests are in this package snapshot.

Compared all 17 diagnostic sections with `/tmp/tarr-pop-phase2-check/tarr.pop.Rcheck/00check.log`, normalizing timings and whitespace. There are **no new ERROR/WARNING/NOTE categories and no changed substantive diagnostics**. The sole diagnostic-block difference is the packaged test summary: Phase 2 had 673 passes / 1 failure / 1 warning / 7 skips; Phase 3 has **753 passes / 1 failure / 1 warning / 8 skips**. The additional skip is the intentional source-only TDC applicability-rule test, because `data-raw` is excluded from the built package; its source-suite version passes.

The three errors remain:

1. `dim_labels` example references undefined `census`.
2. Packaged `test-tdc-estimates-support.R` unconditionally sources excluded `../../data-raw/tdc_estimates_support.r`.
3. Introduction-vignette rebuilding cannot load `codetools` during initial knitr setup.

Other diagnostics retain the existing categories: non-portable vignette cache filenames, non-ASCII projection code, undeclared/missing/internal imports, generic and replacement signatures, existing globals, missing/codoc/usage documentation, and packaging/vignette-dependency notes. No unrelated cleanup was performed.

Logs: `/tmp/tarr-pop-phase3-complete-check.log` and `/tmp/tarr-pop-phase3-complete-check/tarr.pop.Rcheck/00check.log`; packaged test evidence: `/tmp/tarr-pop-phase3-complete-check/tarr.pop.Rcheck/tests/testthat.Rout.fail`. Earlier development snapshots were superseded after review fixes. Roxygen regeneration completed; changed Rd files passed `tools::checkRd()`, and `git diff --check` passed.

## Files changed

Phase 3 changes, in addition to preserved uncommitted Phase 1/2 work:

| File | Phase 3 change |
| --- | --- |
| `R/collapse.R` | Schema-aware engine integration, target guard replacement, output-aware block sizing, result semantics/source notes, roxygen |
| `R/collapse_applicability.R` | New internal schema plan and bounded block reducer |
| `R/group_ages.R` | Interval-derived schema contributors, output-overlap guard, docs and synthetic example |
| `R/poparray_semantic_reductions.r` | Canonical rage-label fallback for interval overlap bounds |
| `data-raw/tdc_estimates.r` | Repository-established age/race applicability attached in build workflow |
| `tests/testthat/test-schema-harmonization.R` | New harmonization regressions |
| `tests/testthat/test-tdc-applicability.R` | New source-rule/interval regressions |
| `tests/testthat/test-dim-applicability.R` | Update superseded Phase 2 grouping guard/identity expectations |
| `man/collapse_dim.Rd` | Regenerated collapse documentation |
| `man/group_ages.Rd` | Regenerated grouping documentation/example |
| `notes/phase_3_report.md` | This report |

Instruction files, Phase 1/2 reports and all other earlier work were preserved. No commit or broad API argument migration was performed.

## Pre-existing issues observed

Phase 2's known package errors are the undefined `census` example, excluded `data-raw` support sourced by a packaged test, and `codetools` during introduction-vignette rebuilding. Other existing findings include import/signature/documentation issues, non-ASCII projection code, non-portable vignette cache filenames and packaging notes. Phase 3 does not include unrelated package cleanup.

## Remaining limitations / follow-up work

Applicability-controller aggregation remains blocked when labels change; combining contexts requires a separate semantic contract. No numerical disaggregation, proportional splitting, imputation, arbitrary cross-dimensional predicates, global NA removal policy or new public framework was introduced. Existing grouping limitations on zero-length non-target/controller axes remain outside this harmonization change; entirely removed **target** source levels are handled by age derivation. Legacy cubes receive no automatic applicability inference or migration. TDC metadata attaches when rebuilding through the inspected script; existing stored cubes are not rewritten. Provenance uses existing text fields rather than a structured operation-history API.
