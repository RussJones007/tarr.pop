# agecraft interval API migration — 2026-11-12

Implementation branch: `codex/agecraft-interval-api`, based on tarr.pop
`fc8c456`. The existing local commit, user changes to `AGENTS.md`, and supplied
`tarr_pop_agecraft_migration_plan.md` were preserved. No merge, push, release,
production cube rebuild, or change to the agecraft repository was performed.

## Dependency and scope

The plan agrees with agecraft's current `docs/API_POLICY.md` and
`docs/age_class_naming_migration.md`. Validation uses agecraft `da09133`,
version 0.1.0.9021, installed from `/home/russ/R/Projects/agecraft` into
`/tmp/tarr-pop-agecraft-new`. The shared default library initially contained
0.1.0.9020; a later fresh process observed 0.1.0.9021 there. This task's installs
targeted only the temporary library. DESCRIPTION requires >= 0.1.0.9021 because 0.1.0.9020
does not distinguish the final API. README describes installing the development
dependency; no CI dependency installer exists in this repository.

Runtime parsing, canonical-label overlap fallback, and data-frame ordering now
use `as_age_interval()`. Maintained TDC, Census and SEER ingestion scripts and
the cube-creation vignette use the same API. Current examples and tests use
`age_interval_presets`. `group_ages()` explicitly rejects categorical
`age_group` assignments, which agecraft's factor conversion would otherwise
accept through their labels. Existing ordinary factor inputs remain supported.
Public tarr.pop function/argument names and saved-view character-label contracts
are unchanged. Help was regenerated with `devtools::document()`.

Only age-dimension metadata is parsed into transient interval vectors. Cube
values retain their existing delayed/HDF5 representation; aggregation still
computes bounded blocks into HDF5 output. No new full-array realization was
introduced. Small synthetic arrays are realized only for test comparisons.

## Historical and persisted artifacts

The old `data-raw/census_data.r` and `data-raw/seer_def.r` are explicitly retained
for reference by their maintained replacements. Their old tarr namespace calls,
`data-raw/Archived/`, `scripts/experiment_1.r`, archived vignette sources,
historical notes/NEWS and historical generated HTML were left unchanged.
`population_cube_creation.html` is ignored generated output and was regenerated
from its current QMD source. The version_2 guide sources the maintained TDC
helpers indirectly and has no obsolete direct agecraft call.

Recursively inspected all four packaged `.rda` objects, including their
attributes; no agecraft interval, assignment, scheme or bare ivs objects were
found. Read only metadata for all eight current cubes under
`/home/russ/R/Projects/Population/Cubes/base` (roughly 0.35–1.1 MB per metadata
bundle), and inspected `cache/cube_registry.rds`; no serialized agecraft objects
were found. No saved-view RDS files were present in that cube root. Population
datasets were not read for this audit. The two historical population archives
were not loaded; these conclusions do not cover arbitrary external saved R
workspaces or user RDS files.

## Validation

Installed dependencies: agecraft 0.1.0.9021, DelayedArray 0.36.0, HDF5Array
1.38.0. DelayedArray 0.36.3 is unavailable in this environment; validation at
that exact target version remains outstanding. No dependency backend API was
changed or added.

The pre-migration synthetic fixture used the old installed agecraft 0.1.0.9020
and tarr.pop source before the runtime edits. After migration, population
values, dimension labels, roles and semantic metadata compare equal under the
new dependency. This is a small-fixture parity check, not whole-production-data
validation. The attempted wider old-dependency test run overlapped source edits
and is not a valid baseline; its counts are deliberately excluded.

Targeted tests cover grouping, interval overlap, schema/applicability, semantic
guards, tabular conversion, saved-view replay/serialization and local synthetic
ingestion transformations. New assertions cover interval/break/label input
equivalence, assignment rejection, unknown-label conservatism and numeric age
ordering with virtual totals. Validation results are recorded below.

Latest targeted results across 17 test files: 154 test blocks, 1,011 passing
assertions, zero failures/errors, zero skips and zero warnings. The original
run had one overstrict integer/double identity assertion in the new ordering
test; it was corrected to compare values and both affected test files rerun
successfully (68 assertions). Counts above replace those files with their
latest results rather than counting reruns twice.

The installed-package smoke test passed in a fresh process, including final
dependency exports, interval-vector input, expected counts, age labels, roles
and HDF5 backing. `devtools::document()` regenerated `man/group_ages.Rd`;
existing unrelated roxygen messages concern hand-maintained Rd files and a
missing title for `slice_tail.poparray`. The repository guard skill passes
`quick_validate.py`. Rendering `population_cube_creation.qmd --no-cache`
succeeded with execution disabled; no source-data jobs ran.

Commands used for package validation (in fresh R processes with
`.libPaths(c("/tmp/tarr-pop-agecraft-new", .libPaths()))` and
`TARR_POP_SKIP_CUBE_SETUP=true`):

```r
devtools::test(filter = paste0(
  "^(group-ages|age-overlaps|current-overlap|dim-applicability|",
  "schema-harmonization|tdc-applicability|dim-semantics|semantic-convenience|",
  "collapse|overlaps|array-2-df|poparray_classr|poparray-view|census-build|",
  "tdc-estimates-support|tdc-estimates-transform|seer-2026)$"))
devtools::test(filter = "^(group-ages|array-2-df)$") # after assertion correction
devtools::check(document = FALSE, build_args = "--no-manual",
                args = "--no-manual",
                check_dir = "/tmp/tarr-pop-agecraft-check", error_on = "never")
```

The full test suite runs through `R CMD check`; manual PDF generation is
excluded. Full check logs are in `/tmp/tarr-pop-agecraft-check.log` and
`/tmp/tarr-pop-agecraft-check/tarr.pop.Rcheck/00check.log`.

The full installed-package suite passed: 2,100 assertions, zero failures,
zero warnings, and 25 skips (about 293 seconds). Source-only ingestion scripts
are excluded from built packages; relevant migration paths were exercised by
the targeted source-checkout run. All examples passed. Vignettes built during
package preparation and passed the installed vignette check.

The downstream `test-census-vintage-update.R` source-only tests were also run
after inspecting the installed-suite skips: 30 passing assertions, zero
failures/errors/skips/warnings. Combined latest targeted coverage is therefore
1,041 passing assertions across 18 source test files (157 test blocks).

`R CMD check --no-manual` completed with **1 error, 6 warnings and 5 notes**.
The error is the rebuild of the unchanged `package_introduction.qmd`: its
restricted rendering process cannot load `codetools` in the first knitr chunk.
`codetools` 0.2.20 is installed at `/usr/lib/R/site-library/codetools` and is
visible in an ordinary fresh R process, so this is a check/rendering library
availability issue, not a removed agecraft symbol. Both population cube guides
rebuilt successfully, as did the other vignettes. The full package check is not
clean and must not be reported as passing.

Warnings concern existing nonportable filenames/vignette asset paths,
non-ASCII projection code, missing/unexported `cli::abort` and
`DelayedArray::{max,min,sum}` plus private `write_block`, S3 signature
consistency, missing documentation and Rd usage issues. Notes concern hidden
`.codex`, DESCRIPTION formatting, nonstandard top-level files (including the
supplied migration plan), undefined globals, and undeclared vignette dependencies.
These unrelated packaging issues were not changed by this migration.

The introduction subsequently rendered successfully outside restricted check
with execution enabled and cache disabled, using a temporary R startup profile
that explicitly prepends the migrated installed package and the existing 4.5
and 4.6 dependency libraries. Output:
`/tmp/tarr-pop-agecraft-introduction/package_introduction.html`. An initial
standalone attempt selected the old default installed tarr.pop; explicitly
pinning library paths avoided that stale installation. This successful render
does not turn the original full check into a clean check. No full-suite repeat
was needed because these follow-ups changed no package code.

## Separate existing safety defect

Without applicability metadata, the global overlap mapping can use an entire
`85+` source category to produce `90+`. This behavior predates the API rename:
both old and new interval interfaces locate the same overlap. The applicability
path correctly rejects this derivation via whole-interval coverage. The plan
explicitly excludes reduction algorithm changes, so this defect remains a
separate follow-up and the migration must not be interpreted as resolving it.

Minimal reproduction of the mapping, with the current API:

```r
source <- agecraft::as_age_interval("85+")
target <- agecraft::as_age_interval("90+")
ivs::iv_locate_overlaps(source, target, relationship = "many-to-one") |>
  ivs::iv_align(source, target, locations = _)
```

The same mapping can be demonstrated with old `as.age_group()` in a fresh
process using the old dependency. Correcting it requires a separate regression
test and deliberate containment/coverage behavior for cubes without schemas.
