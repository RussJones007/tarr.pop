# Population view proof of concept

This implementation uses an independent S7 `poparray_view` with
integer, character, and list properties. The class validates its format version,
identifiers, source contract, operation arguments, and provenance. It does not
inherit from `poparray` and does not contain population values.

The recording API deliberately leaves the existing manipulation functions alone.
`view_filter()` uses the existing filter parser, resolves external values at
recording time, and removes its captured environments. `view_step()` records a
small allowlist of operations with plain arguments. Saved files contain a plain
list, not the S7 object, a delayed operation tree, or temporary HDF5 paths.

```r
view <- new_poparray_view(
  source_series_id = "tdc_2024",
  view_id = "selected_counties_age10",
  description = "Selected counties with ten-year age groups"
)
view <- view_filter(view, area.name %in% c("Tarrant", "Johnson"))
view <- view_group_ages(view,
  age_groups = c("0-9", "10-19", "20-29", "30-39", "40-49",
                 "50-59", "60-69", "70-79", "80+"))
view <- view_step(view, "select",
  dimensions = c("year", county = "area.name", "age.char"))

# Save under the configured population root, using the view ID as its name.
save_poparray_view(view)
list_poparray_views()

# A definition can be inspected without opening its source.
definition <- read_poparray_view(view_id = "selected_counties_age10")
print(definition)
result <- open_poparray_view(view_id = "selected_counties_age10")
# Or open_poparray_view("selected_counties_age10", checkout = TRUE,
#                       checkout_dir = "local-cubes")

# Edit only descriptive fields; preserve creation provenance and the recipe.
definition <- update_poparray_view_metadata(definition,
  description = "Updated county analysis", notes = "Reviewed age intervals")
save_poparray_view(definition, overwrite = TRUE)
```

Use the actual source series ID, dimensions, and county labels in your registry.
These examples are illustrative; source availability and age intervals determine
which operations are valid. `group_ages()` remains responsible for interval
matching, top-coded groups, applicability, and overlap checks.

## Supported operation arguments

| Operation | Arguments |
|---|---|
| `filter` | Use `view_filter()` and the existing population-filter grammar |
| `subset` | `indices = list(area.name = c("Tarrant", "Johnson"))`; character labels, dimensions retained |
| `select` | `dimensions = c("year", county = "area.name", "age.char")`; output order and optional rename |
| `rename` | `mapping = c(county = "area.name")`; new names mapped to old names |
| `relocate` | `dimensions` containing every current dimension once, in desired order |
| `collapse_dim` | Existing `dim`, `groups`, `keep_empty`, `name`, `strict`, `allow_overlap`, `drop` arguments |
| `collapse_all` | Existing `dim`, `label`, `strict`, `allow_overlap`, `drop` arguments |
| `group_ages` | Existing `age_groups`, `dimension_name`, `keep_empty`, `strict`, `allow_overlap` arguments; age groups as character labels |

Factors, custom functions, closures, arbitrary calls, positional subsetting,
and automatic conversion of transformed arrays into recipes are outside this
proof of concept. Rename functions can be applied outside the recipe to produce
an explicit character mapping. Population coercions and summaries are also
outside the recipe; coercing the returned result to an array or data frame is
**EAGER** and should be limited to small selected results.

## Validation and replay

Construction opens the registered source only to capture dimension names/order,
labels, time/area roles, and relevant intrinsic semantics including applicability.
Semantic notes are excluded from compatibility checks. This is a direct
comparison, not a generalized schema migration system. Source provenance and
values are read from the current source at opening time. There is no frozen
numeric snapshot and no support for substituting another source series.

The current policy is deliberately strict: appending another year also rejects
an existing view, even when its filter only uses older years. This limitation is
in the opening-time comparison, not the recipe structure or replay mechanism.
A later append-compatible policy can compare the saved labels against the
current source without reconstructing user operations or changing the plain
recipe format. Before relaxing it, decide whether an unfiltered time dimension
should include newly appended years and revalidate applicability over the
relevant periods. No append-compatible policy is implemented in this prototype.

Recording validates recipe structure and argument types but does not execute
transformations. Unknown dimensions, invalid labels, unsafe aggregation, or
incompatible age groups can therefore fail at opening time. Replay reports the
operation number and operation name with the original error. Existing semantic
and aggregation guards are used unchanged. Each result must remain a valid
`poparray`.

Filtering, slicing, renaming, and reordering stay delayed. Age grouping and
collapsing, including omitted dimensions in `select()`, compute bounded source
blocks into session-temporary HDF5 results. Reopening repeats these computations.
The block-size option limits source blocks, not total process memory. Keep
session-temporary results available while using returned arrays; save the recipe
rather than serializing the returned result for future sessions.

Regression tests verify that several independently opened views use distinct
temporary HDF5 files and remain readable after garbage collection, after another
result is removed, and after a failed replay. Cross-session tests verify that
the child session's HDF5 results disappear on exit while its saved recipe still
reopens correctly. No view-specific cleanup removes backing files during the
session; ordinary R temporary-directory lifetime applies. Manually deleting
those files while using a returned array will still break that array.

Saving updates the saved modification provenance and preserves creation
provenance. Reading and opening do not change timestamps. Writing stages the
new RDS file beside its destination before replacing it. RDS inputs must be
trusted, just as for ordinary `readRDS()` use.

## Storage, discovery, and descriptive metadata

The default file is `views/<view_id>.rds` under the configured population root.
Default filenames require an ID beginning with a letter or digit and containing
only letters, digits, underscores, periods, or hyphens. Display names can contain
spaces and are independent of the ID. Explicit `filepath` exports remain
supported; exports outside the managed `views/` directory are not indexed.

`list_poparray_views()` validates the definitions and refreshes
`cache/view_registry.rds`. Refreshing on discovery keeps this small initial
registry simple and ensures missing files, externally edited metadata, and stale
or corrupt cache files cannot produce stale lookup results. A malformed
individual definition or duplicate view ID produces an informative error.
Registry paths are relative to the population root. A failed cache write warns
but does not invalidate readable authoritative definitions.

`read_poparray_view(view_id = ...)` reads a definition without opening its
source. `open_poparray_view(view_id = ...)`, or a positional ID, resolves its
recipe and replays it against the same source series. An explicit `filepath`
argument is also available. Opening, listing, and printing never change a
recipe's modification timestamp. Printing shows the source and numbered steps
without evaluating them.

`update_poparray_view_metadata()` returns an edited definition, accepting a
definition, ID, or path. It can change the display name, description, and notes.
It preserves the ID, source association, operations, creator, and creation time,
and updates the modifier and modification time. Changes are persisted only when
saved, using `overwrite = TRUE` for an existing managed view. Saving refreshes
the saved modification time and retains the declared modifier. Failed staged
replacement preserves the previous definition and removes the staging file.

## Deferred work

View deletion/renaming, append-compatible source policies, and evaluating a
numerical preview remain for later iterations. No permanent derived cube is
created. Source checkout uses the existing `open_poparray()` checkout
implementation unchanged.
