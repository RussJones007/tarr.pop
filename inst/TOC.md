TOC.md

Table of contents for constructing, refactoring, or documenting the tarr.pop package

## 1. Update poparray file storage schema

This will involve placing meta data for a population cube in the same HDF5 file as the population data.

-   The current meta data for a poparray cube is found in several \*.rds files. These file are used to make a registry of the cubes and meta data. Identify all metadata currently stored in separate RDS files for creating the registry and assigning meta data to the poparray. Things like dimension names, provenance, or other attributes.
-   Next, define a clear structure inside the HDF5 file. The top-level group—let’s say "cube"—with a sub-groups including a "population" group for data and a "metadata" group for all attribut
-   Then, for each RDS, write a new migration script with needed functions read and use the the metadata and write it into the HDF5 "metadata" group as attributes or data sets.
-   Check that the script should work as intended.
-   Then back up the current cubes found in inst/extdata to a sub-folder.
-   Migrate the old cubes to the new structure.
-   Once migrated, I will update all code to read metadata from the HDF5 file, not RDS.
-   Finally, I will confirm that the final HDF5 file holds both data and metadata, and that the RDS files are no longer needed.

## 2. Change the poparray class inheritance.

Convert the `poparray` class to extend `DelayedArray` (not `HDF5Array`).

-   First, I will convert the `poparray` class so it extends `"DelayedArray"` using `setClass()`.\

    > `setClass( "poparray", contains = "DelayedArray", slots = c( roles = "list") \# e.g., time, area dimension mapping )`

-   Next, I will remove the old “has-a” logic where `poparray` wrapped an `HDF5Array` inside a list or `handle` element.\
    Then, I will adjust the constructor so it:

    -   Accepts a `DelayedArray` object.
    -   Verifies that the underlying seed is an `HDF5Array`.
    -   Attaches metadata read from the HDF5 file.

    Something like :\
    `new_poparray <- function(x, roles) { stopifnot(is(x, "DelayedArray"))`

    `if (!is(DelayedArray::seed(x), "HDF5Array")) { cli::abort("poparray must be backed by an HDF5Array seed.") }`

    `new("poparray", x, roles = roles) }`

    -   I will not manually copy slots from DelayedArray — let inheritance handle that.

        Replace print.poparray with setMethod("show", "poparray", ...)

        Replace S3 generics with setGeneric() + setMethod()

        Do NOT redefine arithmetic or [

        Let DelayedArray handle those.

        In short, I will shift from wrapping an HDF5Array to inheriting from DelayedArray and enforcing HDF5 backing at construction time.

-   After that, I will implement a `setValidity()` method to enforce:

    -   Required dimension roles (time and area).
    -   Dimensional integrity.
    -   Consistency between metadata and the underlying array.
    -   Similar to this:\
        setValidity("poparray", function(object) {\
        \# check:\
        \# - roles\$time exists and is valid dimension name \# - roles\$area exists\
        \# - dimnames match metadata\
        \# - no missing dimension names })

-   Then, I will override or extend only what is unique to `poparray`, such as:

    -   Custom `show()` method.
    -   Domain-specific helpers.
    -   Metadata accessors.

Finally, I will ensure all `poparray` operations (subsetting, collapsing, arithmetic, etc.) operate directly on the inherited `DelayedArray` structure and do not rebuild wrapper objects.


Replace print.poparray with setMethod("show", "poparray", ...)

Replace S3 generics with setGeneric() + setMethod()

Do NOT redefine arithmetic or '[' Let DelayedArray handle those.

Ensure:
- [.poparray returns a poparray

- Metadata and roles are preserved

-   drop = FALSE is respected

In short, I will shift from wrapping an HDF5Array to inheriting from DelayedArray and enforcing HDF5 backing at construction time.

Guradrails:
Do not realize the array during migration.
Do not use as.array() anywhere.

Do not duplicate DelayedArray slot definitions.
Do not reimplement arithmetic operators.
