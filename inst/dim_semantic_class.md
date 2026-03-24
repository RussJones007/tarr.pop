---
editor_options: 
  markdown: 
    wrap: 72
---

## Status

Archived design memo. The class described here has been implemented in
`R/dim_semantics.r`, and some draft snippets below are no longer
current.

Use `R/dim_semantics.r` and generated man pages as the canonical
contract.

## Create a new class in a new file called dim_semantics.r

This will be an R S7 class called DimSemantics. The following fields are
to be included. The purpose of each field can be included in the class
documentation.

-   dim_name : character(1) The actual dimension name in the array
    (e.g., "race", "age.char").

-   domain : character(1) Semantic key (e.g., "age", "income", "race",
    "sex"). Not redundant: supports aliasing across dim names.

-   scale_type : character(1) Allowed: "nominal", "ordinal", "interval"

-   partition_type : character(1) Allowed: "partition", "set", "unknown"

-   validated : logical(1)

-   overlap_levels : character() Levels/labels known to cause overlap.
    Only meaningful when partition_type == "set". Can be empty when
    overlap-causers are unknown or not enumerated.

-   notes : character() Free-text notes; can be length 0.

### The S7 validator returns:

-   NULL when valid

-   character vector of problems when invalid

### Rules:

-   dim_name must be non-missing, length 1, nzchar().

-   domain must be non-missing, length 1, nzchar().

-   scale_type must be one of c("nominal","ordinal","interval").

-   partition_type must be one of c("partition","set","unknown").

-   validated must be length 1 logical and not NA.

-   If partition_type == "partition" then overlap_levels must be length

    0.  

-   (Because overlap-causers don’t make sense for a mutually exclusive
    partition.)

-   If partition_type %in% c("set","unknown"), allow any length of
    overlap_levels (including 0).

-   Explicitly document: empty overlap_levels does not mean “never
    safe”; poparray guards may still allow safe operations when n_levels
    \<= 1 or when interval overlap check finds none.

### Constructor requirements

Provide:

new_dim_semantics(

dim_name,

domain,

scale_type,

partition_type = "unknown",

validated = FALSE,

overlap_levels = character(),

notes = character()

)

-   Defaults should produce a valid object.

-   Helper functions (lowercase pa\_ prefix)

-   Implement these as pure predicates / small utilities:

-   pa_is_interval(sem) → TRUE if scale_type == "interval"

-   pa_is_set(sem) → TRUE if partition_type == "set"

-   pa_is_partition(sem) → TRUE if partition_type == "partition"

-   Do not implement policy methods like “allow_sum” inside S7.

-   Controlled updater (internal)

-   Implement: pa_update_dim_semantics(sem, ...)

### Behavior:

-   returns an updated DimSemantics

-   re-validates via S7 assignment

-   intended for internal use by poparray construction or validated
    pipelines

-   Do not implement poparray logic here.

-   REQUIRED documentation block at top of file

-   Codex must include a header comment like:

-   DimSemantics stores intrinsic semantic descriptors only.

-   Current overlap presence is computed by poparray methods from
    current labels.

-   overlap_levels is a list of known overlap-causing levels (may be
    empty if unknown).

-   Interval overlap is determined from label parsing (outside S7
    object), not by overlap_levels.

    This is the “future understanding” piece you asked for.

### Code skeleton Codex should generate

\# R/dim_semantics_s7.r

\# DimSemantics (S7)

\# -----------------

\# This class is declarative: it describes intrinsic semantics of ONE
dimension.

\# It MUST NOT store current-state facts (e.g., "has_overlaps" /
"overlap_present").

\# poparray methods compute current overlap presence using current dim
labels and these semantics.

\# - For partition_type == "set": overlap presence may be detected via
overlap_levels membership (if provided).

\# - For scale_type == "interval": overlap presence must be computed
from interval intersections of labels.

\# overlap_levels may be empty when overlap-causers are unknown or not
enumerated.

DimSemantics \<- S7::new_class(

"DimSemantics",

properties = list(

dim_name = S7::class_character,

domain = S7::class_character,

scale_type = S7::class_character,

partition_type = S7::class_character,

validated = S7::class_logical,

overlap_levels = S7::class_character,

notes = S7::class_character

),

validator = function(self) {

probs \<- character()

if (length(self\@dim_name) != 1L \|\| is.na(self\@dim_name) \|\|
!nzchar(self\@dim_name)) {

probs \<- c(probs, "\@dim_name must be a non-empty character(1).")

}

if (length(self\@domain) != 1L \|\| is.na(self\@domain) \|\|
!nzchar(self\@domain)) {

probs \<- c(probs, "\@domain must be a non-empty character(1).")

}

allowed_scale \<- c("nominal", "ordinal", "interval")

if (length(self\@scale_type) != 1L \|\| is.na(self\@scale_type) \|\|
!(self\@scale_type %in% allowed_scale)) {

probs \<- c(probs, sprintf("\@scale_type must be one of: %s.",
paste(allowed_scale, collapse = ", ")))

}

allowed_partition \<- c("partition", "set", "unknown")

if (length(self\@partition_type) != 1L \|\| is.na(self\@partition_type)
\|\| !(self\@partition_type %in% allowed_partition)) {

probs \<- c(probs, sprintf("\@partition_type must be one of: %s.",
paste(allowed_partition, collapse = ", ")))

}

if (length(self\@validated) != 1L \|\| is.na(self\@validated)) {

probs \<- c(probs, "\@validated must be logical(1) and not NA.")

}

\# overlap_levels only meaningful for set/unknown; must be empty for
partition

if (!is.na(self\@partition_type) && identical(self\@partition_type,
"partition")) {

if (length(self\@overlap_levels) != 0L) {

probs \<- c(probs, "\@overlap_levels must be empty when \@partition_type
== 'partition'.")

}

}

if (length(probs) == 0L) NULL else probs

}

)

new_dim_semantics \<- function(dim_name,

domain,

scale_type,

partition_type = "unknown",

validated = FALSE,

overlap_levels = character(),

notes = character()) {

DimSemantics(

dim_name = dim_name,

domain = domain,

scale_type = scale_type,

partition_type = partition_type,

validated = validated,

overlap_levels = overlap_levels,

notes = notes

)

}

pa_is_interval \<- function(sem) {

inherits(sem, DimSemantics) && identical(sem\@scale_type, "interval")

}

pa_is_set \<- function(sem) {

inherits(sem, DimSemantics) && identical(sem\@partition_type, "set")

}

pa_is_partition \<- function(sem) {

inherits(sem, DimSemantics) && identical(sem\@partition_type,
"partition")

}

pa_update_dim_semantics \<- function(sem, ...) {

stopifnot(inherits(sem, DimSemantics))

dots \<- list(...)

for (nm in names(dots)) {

sem[[nm]] \<- dots[[nm]]

}

sem

}

-   Codex should add roxygen tags (\@keywords internal, \@noRd) to all
    of these unless you want to export them now.

-   Tests to have Codex add (minimal)

-   Create tests/testthat/test-dim_semantics_s7.R:

-   Valid construction works for all scale_type + partition_type combos

-   Invalid scale_type errors

-   Invalid partition_type errors

-   partition_type="partition" + non-empty overlap_levels errors
