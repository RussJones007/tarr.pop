# Candidate domain vocabulary, counting universes, and measures

Discussion summary, October 10, 2026. This is a design proposal, not an
implemented metadata schema or a definitive vocabulary. Additions should follow
the needs and documented definitions of actual source cubes.

## Three distinct concepts

| Concept | Question answered | Example |
|---|---|---|
| Dimension domain | What do the dimension labels represent? | Age, race, household size, housing tenure |
| Counting universe | Which entities and eligible population does the cube describe? | People aged 25 and older; occupied housing units |
| Measure | What does each numeric cell contain? | Count, percentage, median, index score, margin of error |

Time and area remain required operational roles. A domain identifies semantic
meaning independently of a dimension's actual name. Role assignments identify
the particular dimensions used operationally. A shared domain does not establish
compatibility of categories, geographic boundaries, universes, or measures.

## Starting vocabulary

| Canonical domain | Meaning | Candidate family |
|---|---|---|
| `time` | Observation or reference time | Temporal |
| `area` | Geographic unit | Geographic |
| `age` | Person's age, including single ages and grouped intervals | Demographic |
| `sex` | Source-defined sex classification | Demographic |
| `race` | Source-defined race classification | Demographic |
| `ethnicity` | Source-defined ethnicity classification | Demographic |
| `race_ethnicity` | Combined race/ethnicity classification | Demographic |
| `statistic` | Statistical component, such as estimate or standard error | Statistical |

Standardize naming variants rather than merging distinct concepts. For example,
use `age` instead of the phrase "age interval"; interval behavior belongs in
`scale_type` and labels. Counties and ZCTAs can share `area`, but their geographic
types and identifiers need separate description. Keep sex distinct from any
future gender domain.

Broader families can support discovery and display through a vocabulary lookup;
they need not become additional fields on every semantic object and must not
imply common aggregation rules.

## Combined domains and optional component tags

The current `DimSemantics` contract requires a scalar domain. The preferred
proposal is to retain one primary domain and optionally add component tags later.
For example, a `race_ethnicity` dimension could have race and ethnicity tags for
discovery. These tags would not make it two independently usable dimensions.

TDC's combined classification cannot answer every race-only or ethnicity-only
question without additional mappings. Replacing the scalar with a set is possible,
but consumers would need explicit rules for exact combinations, any matching
concept, and independently usable classifications. No such change is implemented.

Allowing multiple dimensions with the same domain may be useful, such as origin
and destination geography. Discovery should then return all matches; operations
requiring one dimension should reject ambiguity or accept an explicit name.
Standard keys should coexist with documented custom domains as needed.

## Candidate ACS-based extensions

These keys are package proposals informed by ACS subjects, not an official Census
domain vocabulary. Exact categories, universes, and availability depend on the
source table and release.

| Family | Candidate domains |
|---|---|
| Household | `household_size`, `household_type`, `relationship_to_householder` |
| Education | `educational_attainment`, `school_enrollment`, `field_of_degree` |
| Social | `marital_status`, `disability_status`, `veteran_status` |
| Migration and language | `citizenship_status`, `place_of_birth`, `residence_one_year_ago`, `language_at_home`, `english_proficiency` |
| Employment | `employment_status`, `occupation`, `industry`, `class_of_worker` |
| Economic | `household_income`, `personal_income`, `poverty_status`, `health_insurance_coverage` |
| Housing | `housing_tenure`, `occupancy_status`, `vacancy_status`, `units_in_structure`, `year_structure_built`, `rooms`, `bedrooms`, `occupants_per_room` |
| Housing resources and costs | `vehicles_available`, `heating_fuel`, `plumbing_facilities`, `kitchen_facilities`, `internet_access`, `gross_rent`, `housing_value`, `housing_cost_burden` |

The ACS [subject catalog](https://www.census.gov/programs-surveys/acs/guidance/subjects.html)
and [2024 five-year detailed table catalog](https://api.census.gov/data/2024/acs/acs5/groups.html)
provide the source concepts. Income, rent, and value can be category dimensions
when cells count entities in bands; numeric income or rent values instead belong
to the measure definition.

Housing examples:

| Question | More precise domain | Published ACS example |
|---|---|---|
| Household size | Number of people in a household | B11016: Household Type by Household Size |
| Physical house size | Rooms or bedrooms; avoid conflating with household size | B25017: Rooms; B25041: Bedrooms |
| Age of house | Year structure built; building age would be derived | B25034: Year Structure Built |
| Construction type | Units in structure; does not establish building materials | B25024: Units in Structure |

Table names are verified in the linked ACS table catalog. Census explains its
[units-in-structure, rooms, and bedrooms questions](https://www.census.gov/acs/www/about/why-we-ask-each-question/rooms/).
Begin with a manageable extension: household size/type, housing tenure, units in
structure, year built, rooms/bedrooms, education, employment, income, and poverty.

## Vulnerability and disease candidates

These are conceptual additions discussed for future sources, not a verified
catalog of available datasets.

| Family | Candidate domains | Definition needed |
|---|---|---|
| Vulnerability | `vulnerability_index`, `vulnerability_component`, `vulnerability_category` | Index methodology/version, component meaning, thresholds, reference population, geographic assignment |
| Health | `disease`, `disease_status`, `disease_severity` | Condition definition, case definition, status/severity scheme, population eligibility |

A vulnerability index identity or version may belong in source/measure metadata
rather than a dimension if it is constant for the cube. Overall scores and
components are not independent additive groups. Threshold categories can change
between releases and may need applicability metadata.

A person can have multiple diseases. Counts across conditions can overlap, even
when statuses within one condition form a partition. An area's vulnerability
classification is contextual and must not be interpreted as an individual
diagnosis or an independently measured personal characteristic.

## Counting universe candidates

Candidate entity types include people, households, families, and housing units.
The universe also needs restrictions: age range, occupied versus all housing
units, household versus group-quarters population, employment eligibility, or
the population eligible for a disease indicator.

People classified by housing type and housing units classified by housing type
are different cubes even when their domains and labels match. In household
tables, race or age may refer to the householder rather than every resident.
That referent must be documented. For health data, distinguish people from case
or event counts and record whether repeated events are possible.

## Measure candidates and aggregation

| Measure | Metadata or handling needed |
|---|---|
| Count or estimated count | Counted entity, eligible universe, category exclusivity |
| Percentage or proportion | Numerator and denominator definitions; do not sum routinely |
| Rate or prevalence | Denominator, scaling unit, period, and any standardization |
| Median or mean | Underlying distribution or weights for aggregation; do not sum |
| Monetary amount | Currency, reference year, nominal versus adjusted dollars, and whether total or average |
| Vulnerability score or rank | Methodology, scale, reference population, and valid aggregation procedure |
| Standard error or margin of error | Associated estimate, confidence level where applicable, and uncertainty-combination method |

Measure type is separate from the dimension's `scale_type`. An interval-valued
dimension does not make its cell values additive. Non-count cubes would require
review of existing population-oriented aggregation behavior before being supported.

## Source and safety requirements

- Use published cross-tabulations for joint dimensions. Separate marginal tables
  do not establish a joint distribution; any estimation requires a documented method.
- Keep classification identity, source release, geographic definition, and
  measurement universe explicit. Shared domain names alone cannot harmonize sources.
- Retain existing partition, overlap, and applicability guards. Canonical-source
  `validated = TRUE` is assurance about semantic definitions, not certification
  of every value or unconditional aggregation safety.
- Use applicability ranges for changing categories or thresholds. Merge adjacent
  identical level sets without extending documented source coverage.
- ACS five-year periods overlap. Adjacent release labels are not independent
  annual observations, and margins of error are not additive.

## Future direction: explicit universe and measure metadata

The current source descriptions and value-column name provide context but do not
fully specify the counted entities, eligibility restrictions, or valid numerical
operations. Before broadening beyond population counts, consider structured
cube-level universe and measure definitions alongside dimension semantics.

Start with small definitions rather than a comprehensive ontology:

- Universe: entity type, eligibility/restrictions, and source definition.
- Measure: type, unit, and any denominator, reference period, or confidence level
  required to interpret the values.
- Compatibility: checks for operations combining cubes with different universes,
  units, definitions, or measures.

Preserve these definitions through subsetting, transformations, saving, and
reopening. Operations that change a universe or measure must update the definition
deliberately. Older cubes should have an explicit unknown state until reviewed;
do not certify their meaning by inferring it solely from filenames or column names.
Introduce aggregation checks gradually after metadata persistence is established.

## Future direction: multiple measures in one cube

The current Tarrant-area ACS ZCTA population estimate and margin-of-error series
are separate cubes with matching year/geography keys. A future cube could contain
both measures, keeping estimate and uncertainty aligned through selection and
other operations.

Two candidate architectures remain open:

| Architecture | Benefit | Design consideration |
|---|---|---|
| One array with a measure/statistic dimension | Fits the existing array structure; selections naturally align measures | Requires measure-specific metadata and guards against reductions across statistical components |
| One cube object containing several aligned arrays | Each measure can have its own definition, storage, and operation rules | Requires shared dimension contracts, coordinated selection, and changes to class and persistence behavior |

The existing projection class already uses a statistic dimension for projection
and standard error. Review that implementation as a starting point, without
assuming it determines the general cube design.

Whichever architecture is chosen:

- Link each uncertainty measure explicitly to its estimate and record the
  uncertainty type and confidence level when applicable.
- Apply operations by measure. Summing estimates must not also sum margins of
  error; uncertainty combination requires a documented statistical method and
  assumptions about dependence.
- Preserve aligned keys and meaningful missingness across measures.
- Define what selecting a single measure returns and which operations are
  available when several measures remain selected.
- Support current single-measure cubes during any migration.

These are future directions to revisit after the basic population-cube behavior
is stable. This discussion authorizes documentation, not a class or HDF5 schema
change, and neither storage architecture has been selected.

## Open design decisions

1. Where and how to persist counting universe and measure metadata.
2. How to represent component tags and domain families, if needed.
3. Which operations require exact domain matches versus broader discovery.
4. How to enforce aggregation rules for rates, scores, monetary values, and uncertainty.
5. Whether expanding beyond population counts requires additional class contracts.
6. Whether multiple measures use an additional dimension or aligned arrays.
7. How estimates and uncertainty are linked, selected, and aggregated together.
8. How existing cubes acquire reviewed universe and measure definitions.

This note does not change classes, builders, or saved cubes. Implementation should
follow a separately reviewed plan with tests for metadata preservation and safety.
