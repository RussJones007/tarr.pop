# ACS and SVI numeric measures: initial metadata proposal

Reviewed October 13, 2026 using Census Bureau ACS documentation and CDC/ATSDR
SVI documentation. This is a research and design note, not an implemented
class change, storage migration, or exhaustive source catalog. The examples use
2024 ACS five-year tables and the documented 2022 SVI release; this review does
not assert that these are the newest releases available.

Related discussions: domain-vocabulary-discussion.md,
shared_measure_storage_discussion.md, and package_separation_discussion.md.

## Separate subject, statistic, unit, and universe

A subject describes what is measured (income, household size, travel time).
A statistic describes how it is summarized (count, total, mean, median,
percentage, percentile rank). The unit describes the numeric scale (people,
minutes, dollars, percentage points). The universe defines the eligible entities.
These are separate from the dimensions identifying each coordinate.

For example, household income bands can be dimension labels whose values are
household counts, while median household income is a dollar-valued measure.
An age dimension does not imply a cell contains a population count: it might
contain a percentage, median income, or its uncertainty.

The ACS API suffix E identifies an estimate, not its statistical meaning.
M identifies a corresponding margin of error; profile PE/PM fields represent
percent estimates and their uncertainty. Annotation fields must also be retained.
Source: [Census API variable notes](https://www.census.gov/data/developers/data-sets/acs-1year/data-notes.html).

## Candidate numeric measure types

Table IDs below are verified examples, not proof of availability for every
release, geography, or demographic cross-tabulation. Aggregation policies are
proposed package behavior, not algorithms implemented in tarr.pop.

| Proposed statistic kind | ACS example | Unit | Proposed combination policy |
| --- | --- | --- | --- |
| estimated_count | B01003 population; B11016 households by type/size | persons or households | Sum only compatible disjoint categories/areas within one period; preserve missingness |
| percentage | DP03 economic percentages | percent, explicitly 0–100 | Recompute from compatible numerator and denominator; do not sum or take an unweighted average |
| mean | B25010 average household size; DP03_0025E travel time | persons per household; minutes | Require matching weights or underlying aggregate and eligible count |
| median | B01002 age; B19013 household income | years; dollars | No pooled median from component medians alone |
| aggregate_total | B19025 aggregate household income; B08013 aggregate travel time | dollars; minutes | Conditional sum with compatible universes and nonoverlapping contributors |
| per_capita / ratio | B19301 per-capita income | dollars per person | Require numerator/denominator definition; no ordinary summation |
| median_percentage | B25071 median gross rent as a percentage of household income | percent | Retain median semantics; cannot pool as a simple proportion |
| index | B19083 income-inequality Gini | unitless index | Preserve methodology; block population-style collapse |
| margin_of_error | Associated ACS M/PM field | parent unit; percentage points for PM | Link to estimate; require uncertainty-specific method |

Sources: [detailed-table catalog](https://api.census.gov/data/2024/acs/acs5/groups.html),
[DP03](https://api.census.gov/data/2024/acs/acs5/profile/groups/DP03.html),
[B25010](https://api.census.gov/data/2024/acs/acs5/groups/B25010.html),
[B19013](https://api.census.gov/data/2024/acs/acs5/groups/B19013.html),
[B19025](https://api.census.gov/data/2024/acs/acs5/groups/B19025.html),
[B19301](https://api.census.gov/data/2024/acs/acs5/groups/B19301.html),
[B25071](https://api.census.gov/data/2024/acs/acs5/groups/B25071.html), and
[B19083](https://api.census.gov/data/2024/acs/acs5/groups/B19083.html).

Other useful count subjects include education (B15003), poverty (B17001), health
insurance (B27001), and internet subscriptions (B28002). Published tables define
available combinations: separate marginal tables cannot create a joint cube.
The exact universe must be captured from source definitions before ingestion.

ACS five-year results describe periods, not five independent annual values.
Store period start/end and product explicitly alongside the release label.
Source: [choosing ACS products](https://www.census.gov/programs-surveys/acs/guidance/estimates.html).

## Uncertainty and status are first-class companions

Keep numeric uncertainty aligned with its estimate. Store confidence level,
uncertainty kind, source variable, and estimate link. Published ACS margins of
error normally use 90% confidence. Standard error or confidence bounds may be
supported as derived measures, with formula and assumptions recorded; do not
claim that every table publishes them directly.

Some selected ACS tables provide 80 variance replicates, which can support
uncertainty calculations including covariance. Treat this as optional advanced
support; it is not available for every measure.
Source: [ACS variance replicate tables](https://www.census.gov/programs-surveys/acs/data/variance-tables.html).

Preserve annotations and raw source codes when decoding missing values. The ACS
uses annotations for unavailable, controlled, and open-ended median results.
A bounded/censored median should not silently become an exact numeric value.
An absent or controlled MOE must not automatically become zero.
Source: [Census profile annotations](https://www.census.gov/data/developers/notes-on-data-profiles.html).

## CDC/ATSDR SVI fit

SVI has four themes plus an overall percentile ranking:

| Component | Published field |
| --- | --- |
| Socioeconomic status | RPL_THEME1 |
| Household characteristics | RPL_THEME2 |
| Racial and ethnic minority status | RPL_THEME3 |
| Housing type and transportation | RPL_THEME4 |
| Overall vulnerability | RPL_THEMES |

The ranks range from 0 to 1 and are relative to a state or national comparison
set. They describe places, not individual diagnoses. Keep geography and ranking
scope explicit. Source: [SVI FAQ](https://atsdr.cdc.gov/place-health/php/svi/svi-frequently-asked-questions-faqs.html).

The 2022 dictionary distinguishes E_/M_ estimates and MOEs, EP_/MP_ percentages
and their MOEs, EPL_ indicator percentiles, SPL_ sums of percentiles, RPL_ theme
and overall ranks, and F_ flags. Individual flags are binary; theme/overall flags
count flagged indicators. The source uses -999 for missing values. Source:
[SVI 2022 documentation](https://www.atsdr.cdc.gov/place-health/media/pdfs/2024/10/SVI2022Documentation.pdf).

Proposal: add percentile_rank, score, binary_flag, and flagged_component_count
kinds to the measure catalog. Keep their domains and bounds separate from data
type. A numeric double is not sufficient to distinguish percentage from rank;
an integer is not sufficient to distinguish population from flag counts.

A theme/component axis is useful when storing comparable ranks together. It
should be a nominal selector with reduction disabled: the overall rank is not
an additive fifth theme. Separately stored measures can instead record component
identity in each measure definition, avoiding a new axis unless useful.

Indicators have different universes and units. Prefer individual aligned measure
datasets and a small catalog mapping indicators to themes rather than creating
an artificial joint household/person classification. National and Texas-relative
rankings can be separate measures sharing coordinates; an explicit reference-set
axis is another design option, never an ordinary additive dimension.

Store methodology/release and geographic-boundary vintage. A subset of national
ranks must retain its national reference definition; filtering to Texas must not
silently rerank. Do not sum or average tract ranks to manufacture an official
county rank. Published ranks across releases are not straightforward trends.
Source: [SVI comparisons across releases](https://atsdr-origin.cdc.gov/place-health/php/svi/svi-data-documentation-download.html).

Do not assign the ACS input MOEs to the derived SVI ranks. Any uncertainty for
ranks would require its own justified methodology.

## Proposed metadata contract

Shared coordinate metadata: dimension names/labels, roles, category semantics,
applicability, geography identifiers and boundary vintage, and reference periods.
Share only what is identical; universe and applicability may vary by measure.

Per-measure catalog fields:

| Field group | Proposed fields |
| --- | --- |
| Identity/storage | measure_id, label, dataset_path, coordinate_set_id, storage_dtype |
| Interpretation | statistic_kind, unit, scale_multiplier, valid_bounds |
| Universe | entity_type, eligibility, referent, source_universe_definition |
| Source | agency, product, table_id, variable_id, release, definition_url |
| Context | currency/base_year where relevant; index_method/version; ranking_reference_set |
| Companions | estimate_id, uncertainty_ids, numerator_id, denominator_id, weight_id |
| Uncertainty | uncertainty_kind, confidence_level, derivation_method |
| Status | annotation_dataset, source_missing_codes, decoding_policy |
| Operations | allowed_reductions by dimension, required_inputs, method/assumptions |
| Provenance | published_or_derived, transformation recipe, dependency measure IDs |

These are logical groups, not final field names or an approved HDF5 layout.
Metadata-only selection should stay lazy; reductions should remain bounded-memory.
Unknown older measures need an explicit unknown contract, not a guess from names.
Compatibility checks should reject invalid mixing of units, universes, periods,
and comparison sets before calculation.

A mean of stored cell values remains a descriptive summary of those cells. It is
not automatically the pooled household mean or an official aggregated SVI score.
Document this difference explicitly when extending the current statistics API.

## Suggested first implementation scope

Start with population and household counts plus MOEs, a percentage with linked
numerator/denominator, and stored medians with aggregation blocked. Add SVI ranks
and flags as readable/filterable measures with explicit nonadditive contracts.
Use tiny fixtures for alignment, metadata round trips, period compatibility,
annotation preservation, missingness, lazy reads, and rejected unsafe reductions.

Support existing single-population cubes during migration. Update class and
persistence contracts only after selecting the measure-interface design. More
advanced weighted calculations, replicate variance, and index reconstruction can
follow separately. No statistical formulas or new numeric transformations are
implemented by this note.
