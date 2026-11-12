# tarr.pop migration to the renamed agecraft interface

Status: implementation plan for a future tarr.pop task. Prepared 2026-11-12.
No tarr.pop files were modified while preparing this plan.

## Goal and scope

Update tarr.pop to use agecraft after both naming phases while preserving its
population results, interval safety, dimension metadata and delayed workflows.
This is a dependency-interface migration, not a redesign of age grouping.

The critical distinction is:

| Object | Final agecraft class | Use in tarr.pop |
| --- | --- | --- |
| Age ranges with bounds | age_interval | Parse, order, compare and map age dimension labels |
| Assigned categorical values | age_group | Use only if an actual assignment vector is needed |
| Reusable classification definition | age_scheme | Existing use only; new integration is outside this task |

Do not replace old interval calls with the new age_group() constructor. The
spelling was reused for ordered-factor assignments. Population age dimensions
remain interval-valued labels; do not turn whole population cubes or their cells
into assignment vectors.

## Verified starting points

The read-only inventory used:

- agecraft branch refactor/age-class-names, final refactor commit 040ba11;
  phase-one commit e25155c; pre-refactor commit e7cf2a2.
- tarr.pop HEAD fc8c456, on master, ahead of origin/master by one commit.
  AGENTS.md had an existing uncommitted user change. Preserve it.
- tarr.pop currently declares agecraft in Imports without a minimum version.
- The naming-phase commits reported version 0.1.0.9020, which also identified
  the old interface. The completed refactor has since been assigned development
  version 0.1.0.9021; require that version or a verified later release.

Recheck all of these when implementation starts. Line numbers below are
navigation hints from this snapshot, not permanent patch locations.

## Instructions for Codex

Work in /home/russ/R/Projects/tarr.pop. Read its AGENTS.md and the required
repository guard and reference documents before implementation. Read this plan,
the agecraft migration guide and the current agecraft API policy. Also read any
applicable documentation guidance in tarr.pop; do not assume files from agecraft
are automatically authoritative in another repository.

Create a dedicated tarr.pop branch, such as migrate/agecraft-interval-api, from
its appropriate current development base. Inspect status first; preserve unrelated
changes and the existing local commit. Do not reset to origin/master or switch
away from user work destructively. Do not alter the agecraft implementation,
merge, push, publish, download production datasets or rebuild production cubes
as part of this task.

The tarr.pop guard reference currently mentions rage::age_group() as an interval
example. Treat that example as superseded by the approved dependency migration;
retain its interval, laziness and aggregation rules. Correct obsolete guidance
when revising the project. If editing skill files/references, follow the applicable
skill-maintenance workflow and record those changes in Git rather than creating
a conflicting second source of domain rules.

## API map

Apply mappings only after determining what an occurrence means.

| Earlier agecraft interface | Final interface | Migration note |
| --- | --- | --- |
| as.age_group(x, ...) | as_age_interval(x, ...) | Primary change in current tarr.pop code |
| Interval age_group(start, end, ...) | age_interval(start, end, ...) | Numeric bounds; not the new factor constructor |
| is.age_group(x) / interval class check | is_age_interval(x) | Do not substitute is_age_group() for interval checks |
| compare_age_groups() | compare_age_intervals() | Preserve operation/result semantics |
| age_group_to_char() | age_interval_to_char() | Prefer ordinary as.character() where already used |
| valid_age_group_symbol() | valid_age_interval_symbol() | Only if found during inventory |
| age_group_presets / age.groups | age_interval_presets | Old data aliases remain; use canonical name in current code/docs |
| Assignment age_category(x, intervals) | age_group(x, intervals) | Only for actual assignments, not age dimension ranges |
| is_age_category() | is_age_group() | Assignment predicate |
| category_groups() / category_intervals() | group_intervals() | Definitions associated with assignment levels |
| scheme_groups() | scheme_intervals() | Scheme interval accessor |
| age_scheme(groups = ...) | age_scheme(intervals = ...) | Stored field is also intervals |
| age_cat_iv(age, age_group = ...) | age_cat_iv(age, intervals = ...) | Generic name and ordinary-factor behavior retained |

categorize_age() keeps its name and options; default factor output is an age_group
ordered factor. output = "interval" returns age_interval. Keep conversion values
age_to_group/group_to_age and the distinction between definition units and the
legacy observation units attribute. No new scheme or assignment integration is
required by this plan.

Keep tarr.pop public group_ages(), view_group_ages(), age_groups arguments,
age.char dimension names, group/index result columns, and semantic metadata
names unless a separate incompatibility is established. Ordinary words such as
“age groups” are still appropriate. Do not perform a global text replacement.

## Phase A: inventory, dependency verification and baseline

1. Record branch, HEAD, status, installed dependency versions, .libPaths() and
   find.package("agecraft"). Search R/, tests/, data-raw/, inst/, scripts/,
   vignettes/, man/, NAMESPACE, DESCRIPTION and guidance files. Use rg when
   available; otherwise grep. Include unqualified calls, class strings, S3 method
   names, dynamic get()/data() lookups and old rage/tarr namespace references.
2. Classify matches as runtime, active build script, documentation source,
   generated output, historical/archive, or unrelated tarr API. Inspect actual
   call sites before editing. Do not silently migrate another package's API.
3. Capture targeted tarr.pop results against a verified pre-refactor agecraft
   installation, if available, using small synthetic cubes. Record both commits.
   Avoid changing a shared default R library; use separate library directories
   or processes to compare old and new dependencies. Capture labels/order,
   grouped counts, NA behavior, roles, semantics and applicability.
4. If an old installation is unavailable, use existing independent fixture
   expectations as the baseline and state that old/new execution parity could
   not be run. Failures from missing as.age_group() with the final dependency
   are known interface failures, not evidence of an aggregation defect.
5. Verify the actual final dependency API before implementation. Do not assume
   that an already loaded agecraft namespace changes after installation.

An illustrative check in a fresh R process:

```r
# Prepend a dedicated library explicitly if needed, then load the namespace.
# .libPaths(c("/path/to/dedicated/library", .libPaths()))
ns <- asNamespace("agecraft")
required <- c("as_age_interval", "age_interval", "is_age_interval",
              "age_group", "is_age_group", "group_intervals")
stopifnot(all(required %in% getNamespaceExports(ns)))
x <- agecraft::as_age_interval(c("0-4", "5+"))
stopifnot(agecraft::is_age_interval(x), inherits(x, "ivs_iv"),
          !agecraft::is_age_group(x))
y <- agecraft::categorize_age(c(1, 5), by = x)
stopifnot(identical(class(y), c("age_group", "ordered", "factor")))
```

Require `agecraft (>= 0.1.0.9021)` in tarr.pop and verify the actual installed
API in a fresh session. Do not declare >= 0.1.0.9020 sufficient: that version also
identified the old API. Use a verified commit/build of 0.1.0.9021 or a later
release in CI and local testing, following the repository's dependency-management
practice. If this development build is not available on the configured package
repository, document how it is obtained; a minimum version does not install an
unpublished build automatically. Do not silently fall back to old constructors.
No unrelated new dependency is required.

## Phase B: runtime migration

| Location observed | Required review and change |
| --- | --- |
| R/group_ages.R:84–85 | Source and target labels use as_age_interval(); preserve ivs overlap/alignment and collapse_dim backend |
| R/group_ages.R:135–136, pa_age_contributors() | Parse source/target ranges through as_age_interval(); preserve exact whole-interval coverage logic |
| R/poparray_semantic_reductions.r:26, pa_has_interval_overlap() | Update canonical-label fallback; keep malformed/unknown labels conservatively risky |
| R/poparray_class.r:1005, polish_df() | Update interval parsing before sorting age factor levels; preserve existing All handling and output types |
| R/poparray_view.R and R/poparray_view_storage.R | Audit delegation and saved age_groups arguments; keep public names and storage behavior |
| DESCRIPTION / NAMESPACE | Establish real dependency requirement; update imports only if found; regenerate registrations rather than hand-edit generated files |

The tryCatch fallback in pa_has_interval_overlap() is especially important:
a missing old function can be swallowed and treated as overlap risk, causing
otherwise valid reductions to be rejected. Test canonical labels that exercise
this fallback, not only labels handled by tarr.pop's own parser.

Preserve these safeguards:

- Half-open [start, end) ranges, integer-label interpretation and open upper ends.
- Numerical interval ordering rather than alphabetic string ordering.
- Population counts are additive only across compatible, nonoverlapping ranges.
- Never split an aggregated 85+ population into 85–89 and 90+ by interval overlap.
- With applicability metadata, use whole source intervals to cover each target
  exactly within the relevant schema. Preserve non-derivable NA results and the
  summarized warning; keep applicable missingness distinct from inapplicability.
- Keep strict/allow_overlap, target-overlap rejection and keep_empty behavior.
- Preserve dimnames, named dimensions, roles, dim_semantics, and applicability.
- All remains virtual. Label formatting helpers may recognize it separately;
  do not introduce physical All cells into HDF5 arrays.

Do not rewrite reduction algorithms to accommodate the new names. If an existing
behavior appears unsafe, reproduce and report it separately rather than silently
changing statistical behavior within this migration.

## Phase C: ingestion, documentation and persisted objects

Active build-script candidates found in this snapshot:

- data-raw/tdc_estimates_support.r
- data-raw/tdc_estimates.r
- data-raw/tdc_add_estimate_year.r
- data-raw/census_data_2.r
- data-raw/seer_data_2026.r

Update agecraft calls in the active paths and test extracted transformations with
small local fixtures. Do not run complete population ingestion or network jobs
just to validate renamed calls. Preserve source labels, canonical output ordering,
dataset provenance and cube structure.

data-raw/seer_def.r and data-raw/census_data.r contain tarr::as.age_group()
references; archived files and experiment scripts contain rage or unqualified
calls. Establish whether they are active before editing. Inventory inactive
occurrences as historical rather than treating them as current runtime defects.

Update roxygen in group_ages(), current examples and active .qmd/.Rmd sources,
including vignettes/population_cube_creation.qmd. Inspect the version_2 guide
for indirect dependencies. Generated HTML still contains rage references in this
snapshot: identify its authoritative source and regenerate only current outputs.
Do not hand-edit generated HTML/Rd or rewrite intentionally archived artifacts.
Regenerate help with devtools::document(); update NEWS, dependency notes and any
current contributor guidance. Follow applicable reader-directed documentation
rules. State what callers supply and receive, not the internal rename history.

Audit RDS/RDA fixtures, packaged data, cube sidecars, metadata and saved views for
actual serialized old age_group intervals or age_category assignments. A text
match cannot establish their presence or absence. Inspect small metadata objects
without loading population arrays into memory.

- Character/factor age labels alone are not old interval objects. Reconstruct
  transient intervals when needed; do not rewrite population data gratuitously.
- If old interval objects exist, migrate bounds, units, names and display symbols
  through age_interval(), not class-attribute reassignment.
- If saved assignments exist, reconstruct labels and complete definitions through
  age_group(); preserve unused levels, missing values and observation metadata.
- If old schemes exist, inspect nested field/class changes and provenance. Avoid
  creating derivative ancestry merely because the storage interface changed.
- Keep originals until round-trip validation succeeds. Do not infer that every
  existing HDF5 cube needs rebuilding from the agecraft class-name change.

## Phase D: tests and validation

Select existing tests by actual dependency paths; these files exist in the
inspected repository and should be rechecked at implementation time:

| Test area | Starting files |
| --- | --- |
| Age grouping and parser fallback | test-group-ages.R, test-age-overlaps.R, test-current-overlap.R |
| Applicability and exact source coverage | test-dim-applicability.R, test-schema-harmonization.R, test-tdc-applicability.R |
| General semantic reduction guards | test-dim-semantics.R, test-semantic-convenience.R, test-collapse.R, test-overlaps.R |
| Data-frame age ordering | test-array-2-df.R, test-poparray_classr.R; add direct polish_df coverage if absent |
| View delegation and serialization | test-poparray-view.R |
| Active ingestion helpers | test-census-build.R, test-tdc-estimates-support.R, test-tdc-estimates-transform.R, test-seer-2026.R |

Add focused assertions where current tests do not cover the migration:

1. Equivalent target character labels, numeric preset breaks, and age_interval
   inputs give matching supported group_ages() results. Keep saved-view input
   restrictions separate; do not broaden view_group_ages() simply because the
   direct grouping function accepts additional types.
2. Canonical labels < 1, 1-4, 85 + and 95 + retain parsing, coverage and overlap
   behavior. Adjacent ranges do not overlap; truly overlapping ranges do.
3. Age levels sort by bounds, and formatting retains the current spelling and
   factor order. Exclude/reappend virtual totals exactly as the existing contract
   requires. Verify no valid label becomes NA during factor reconstruction.
4. Exact aggregation preserves counts; gaps, overlaps and forbidden splits retain
   their errors/warnings. Verify 85+ is not used to derive 90+ and that
   85–89 + 90–94 + 95+ can cover 85+ under the applicable schema.
5. Metadata, dimension roles, applicability and output age labels survive grouping
   and data-frame conversion. Test names rather than assuming age dimension position.
6. Outputs remain HDF5-backed/delayed through the existing supported backend.
   Small test-only as.array() comparisons are acceptable for value assertions;
   do not introduce full-array realization into production paths. Grouped
   reductions may perform their established blockwise work; do not incorrectly
   promise that a reduction performs no computation or I/O.
7. Former interval class checks cannot accept the new categorical age_group as
   equivalent ranges. Handle unsupported assignment inputs deliberately rather
   than treating factor codes as ages. Preserve previously documented accepted
   input types; any new assignment support is a separate design choice.

Verify dependency APIs against tarr.pop's target DelayedArray 0.36.3 and HDF5Array
1.38.0 or explicitly report unavailable configurations. Do not redesign their
backend contracts during this migration.

During edits, run targeted tests and affected callers until they pass. At the
completed dependency-migration checkpoint run the full suite and R CMD check,
including relevant examples/vignettes, consistent with tarr.pop's broad-change
validation policy. Document external-data or rendering limits instead of masking
failures. Run an installed-package smoke test in a fresh process with the verified
final agecraft dependency, not only devtools::load_all() on either source tree.

Search again for removed agecraft symbols, obsolete imports, stale cross-links
and class strings. Allow only explicitly identified migration/history occurrences
and deliberate dataset-alias tests. Record commands, actual results, skips and
limitations; do not reuse agecraft's assertion counts as tarr.pop validation.

## Completion criteria and report

- All active runtime and maintained build/documentation paths use the final API.
- Existing tarr.pop public names, population results, age semantics and lazy
  storage contracts remain intact.
- The required agecraft revision/API is reproducible and documented; version
  requirements are added only when they distinguish the final interface.
- Any real serialized-object migration is tested, or the audit documents that
  none was found in the inspected artifacts. No production cube was rebuilt
  without a demonstrated need and explicit scope.
- Targeted and checkpoint tests/checks pass, or outstanding environmental limits
  are stated precisely. No unexplained regression is accepted as a rename effect.
- Report changed files, dependency revision, compatibility implications, baseline
  comparisons, value/metadata/laziness checks and remaining release work.
- Leave the work on its development branch for review. Do not merge, push,
  publish or change other repositories without further instruction.
