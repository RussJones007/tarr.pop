# Phase 1: poparray first-argument audit

Scope: package-defined functions exported in `NAMESPACE`, registered S3 methods,
and package-defined S4 methods taking a `poparray`. Formals were checked against
the loaded source namespace; factory-generated label accessors were inspected at
runtime. Inherited DelayedArray methods are not package-defined API. No arguments
were renamed in Phase 1.

“Breaking” means changing the first formal would break callers using its current
name, including direct calls to method functions. “Registered” means available
through generic dispatch, rather than a separately exported method function.
Recommendations are for a later compatibility-aware change, not immediate edits.

| Function | Generic/method? | Current first argument | Exported? | Recommended argument | Reason | Breaking if renamed? |
| --- | --- | --- | --- | --- | --- | --- |
| `filter.poparray` | dplyr S3 method | `.data` | Registered | `.data` | Match `dplyr::filter` | Yes; retain |
| `[` | Base S4 method | `x` | Registered | `x` | Match indexing generic | Yes; retain |
| `dimnames` | Base S4 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `dimnames<-` | Base S4 replacement | `x` | Registered | `x` | Match replacement generic | Yes; retain |
| `show` | methods S4 method | `object` | Registered | `object` | Match `show` generic | Yes; retain |
| `sum` | Base S4 method | `x` | Yes (`exportMethods`) | `x` | Preserve existing S4 reduction signature; base generic uses `...` | Yes; retain |
| `sd` | stats S4 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `names.poparray` | Base S3 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `length.poparray` | Base S3 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `summary.poparray` | Base S3 method | `object` | Registered | `object` | Match generic | Yes; retain |
| `as.double.poparray` | Base S3 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `as.data.frame.poparray` | Base S3 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `as_tibble.poparray` | tibble S3 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `split.poparray` | Base S3 method | `x` | Registered | `x` | Match generic | Yes; retain |
| `by.poparray` | Base S3 method | `data` | Registered | `data` | Match generic | Yes; retain |
| `collapse_dim` | Package S4 generic | `x` | Yes | `pop` (future generic-wide migration) | Package verb specifically takes a poparray | Yes: `collapse_dim(x = ...)` and S4 signature/dispatch compatibility |
| `collapse_dim,poparray` | Package S4 method | `x` | Registered | Match generic (`x` today) | Never rename just the method; migrate generic and method together only if authorized | Yes; retain in Phase 1 |
| `group_ages` | Package function | `pop` | Yes | `pop` | Already follows population-specific verb convention | Yes; retain |
| `project` | Package function | `parray` | Yes | `pop` | Population-specific forecasting verb | Yes: `project(parray = ...)` |
| `save_poparray` | Package function | `x` | Yes | `pop` | Saves a poparray specifically | Yes: `save_poparray(x = ...)` |
| `create_poparray` | Package function | `x` | Yes | `pop` | Despite its name, takes a poparray and delegates to saving | Yes: `create_poparray(x = ...)` |
| `dim_labels` | Package function | `arr` | Yes | `pop` | Explicitly validates a poparray | Yes: `dim_labels(arr = ...)` |
| `ages` | Factory-generated function | `arr` | Yes | `pop` | Poparray label accessor | Yes: `ages(arr = ...)` |
| `areas` | Factory-generated function | `arr` | Yes | `pop` | Poparray label accessor | Yes: `areas(arr = ...)` |
| `years` | Factory-generated function | `arr` | Yes | `pop` | Poparray label accessor | Yes: `years(arr = ...)` |
| `sexes` | Factory-generated function | `arr` | Yes | `pop` | Poparray label accessor | Yes: `sexes(arr = ...)` |
| `races` | Factory-generated function | `arr` | Yes | `pop` | Poparray label accessor | Yes: `races(arr = ...)` |
| `ethnicities` | Factory-generated function | `arr` | Yes | `pop` | Poparray label accessor | Yes: `ethnicities(arr = ...)` |
| `time_role` | Package function | `x` | Yes | `pop` | Validates a poparray specifically | Yes: `time_role(x = ...)` |
| `area_role` | Package function | `x` | Yes | `pop` | Validates a poparray specifically | Yes: `area_role(x = ...)` |
| `dim_semantics` | Package function | `x` | Yes | `x` | Accepts a poparray or an HDF5 cube path | Yes; retain polymorphic input |
| `dim_semantics<-` | Package replacement function | `x` | Yes | `x` | Supports poparray or path; match accessor | Yes; retain |
| `data_col` | Package function | `x` | Yes | `x` | Supports poparray, path, or attribute-bearing object | Yes; retain polymorphic input |
| `data_col<-` | Package replacement function | `x` | Yes | `x` | Supports object or path; match accessor | Yes; retain |
| `roles` | Package function | `x` | Yes | `x` | Accepts poparray or path | Yes; retain polymorphic input |
| `roles<-` | Package replacement function | `x` | Yes | `x` | Accepts poparray or path; match accessor | Yes; retain |
| `source_meta` | Package function | `x` | Yes | `x` | Accepts poparray or path | Yes; retain polymorphic input |
| `source_meta<-` | Package replacement function | `x` | Yes | `x` | Accepts poparray or path; match accessor | Yes; retain |
| `cube_metadata` | Package function | `x` | Yes | `x` | Accepts poparray or path | Yes; retain polymorphic input |
| `cube_metadata<-` | Package replacement function | `x` | Yes | `x` | Accepts poparray or path; match accessor | Yes; retain |
| `get_source` | Package function | `obj` | Yes | `x` | Also retrieves source attributes from other objects | Yes: `get_source(obj = ...)`; docs incorrectly call it `x` |
| `add_population_data` | Package function | `cube` | Yes | `cube` | Accepts poparray, cube path, or series identifier | Yes; retain descriptive broader input |
| `is.poparray` | Package predicate | `x` | Yes | `x` | Tests any object; input need not be a poparray | Yes; retain |
| `array_2_df` | Package array helper | `arr` | Yes | `x` | Works with generic array/table input, including poparray | Yes: `array_2_df(arr = ...)` |

Adjacent APIs whose principal input is not a poparray:

| Function | Kind | Current first argument | Exported? | Recommended argument | Reason | Breaking if renamed? |
| --- | --- | --- | --- | --- | --- | --- |
| `new_poparray` | Constructor | `x` | Yes | `x` | Wraps an HDF5-backed DelayedArray | Yes; retain |
| `as.poparray` | Package S3 generic | `x` | Yes | `x` | Coercion of generic objects | Yes; retain |
| `as.poparray.array` | Package S3 method | `x` | Registered | `x` | Match coercion generic | Yes; retain |
| `as.poparray.default` | Package S3 method | `x` | Registered | `x` | Match coercion generic | Yes; retain |
| `as.poparray.poparray_projection` | Package S3 method | `x` | Registered | `x` | Input is projection; match coercion generic | Yes; retain |
| `poparray_projection` | Constructor | `projection` | Yes | `projection` | Projected DelayedArray values | Yes; retain |
| `projection`, `std_error` | Package accessors | `x` | Yes | `x` | Input is a poparray_projection | Yes; retain |
| Projection `plot`, `print`, `as.data.frame`, `as_tibble`, `[` methods | Established S3/S4 methods | `x` | Registered | `x` | Match corresponding generic; outside poparray input scope | Yes; retain |
| `confint.poparray_projection` | stats S3 method | `x` | Registered | `object` (future compatibility fix) | `stats::confint` uses `object`, `parm`, `level`, `...`; current method is inconsistent | Yes: direct `confint.poparray_projection(x = ...)`; generic signature needs separate review |

Other exports (`df_2_array`, ingestion, cube opening/storage/registry setup, and
`%between%`) take data frames, reader functions, identifiers, paths, or vectors.
They do not belong to the population-input naming migration. Internal
`group_array_by_levels(arr, ...)` takes a poparray and could use `pop` later;
internal generic array helpers may keep `x`. No recommendation standardizes on
`pa`.

A future migration should preserve old named arguments through a documented
compatibility strategy and deprecation period, with dedicated dispatch tests for
`collapse_dim`. Renaming a formal alone is a breaking change even when positional
calls continue working. Documentation mismatches such as `get_source(obj)` versus
its `@param x`, `dimnames(x)` versus `@param poparray`, and the nonstandard second
argument `data_col<-(x, values)` are follow-up audit findings, not Phase 1 edits.
The package check also identifies the additional-argument mismatch in
`as.data.frame.poparray` and the `confint.poparray_projection` generic mismatch;
matching a first argument alone does not establish complete generic compatibility.
