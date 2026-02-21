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

## 2. Change the poparray class inheritance (Revised for S4 Migration)

Convert the poparray class to extend DelayedArray (not HDF5Array).

All internal helpers must assume x is already a DelayedArray and operate directly on it without extracting a seed unless absolutely necessary.

### Step 1 — Define the S4 class

-   Define poparray as an S4 class that:

-   Contains "DelayedArray"

-   Does NOT redefine or duplicate DelayedArray slots

-   Adds only minimal additional slots required for domain roles

-   Example:

`setClass(`

`"poparray",`

`contains = "DelayedArray",`

`slots = c(`

`roles = "list" # e.g., list(time = "year", area = "county")`

`)`

`)`

Do not redefine slots already defined by DelayedArray.

### Step 2 — Implement a constructor

-   Create a constructor new_poparray() that:

    -   Accepts a DelayedArray object

    -   Verifies DelayedArray::seed(x) is an "HDF5Array"

    -   Validates required metadata

    -   Does NOT copy DelayedArray slots manually

    -   Uses new("poparray", x, roles = roles)

    -   Example logic:

        ``` r
        new_poparray <- function(x, roles) {
        ```

``` r
stopifnot(is(x, "DelayedArray"))
```

``` r
if (!is(DelayedArray::seed(x), "HDF5Array")) {
```

``` r
cli::abort("poparray must be backed by an HDF5Array seed.")
```

``` r
}
```

``` r
new("poparray", x, roles = roles)
```

``` r
}
```

### Step 3 — Implement setValidity()

Add a setValidity("poparray", ...) method that enforces:

-   roles\$time exists and matches a dimension name

-   roles\$area exists and matches a dimension name

-   No duplicated roles

-   dimnames exist

-   Metadata in HDF5 matches dimensional structure

-   Do not realize the array during validation.

### Step 4 — Replace S3 methods with S4 methods

Replace print.poparray with:

-   setMethod("show", "poparray", function(object) { ...
    })

-   Replace S3 generics with S4 generics using:

-   setGeneric("collapse_dim", function(x, ...) standardGeneric("collapse_dim"))

-   setMethod("collapse_dim", "poparray", function(x, ...) { ...
    })

-   Do not reimplement:

    -   \+

    -   \-

    -   \*

    -   sum

    -   aperm

    -   dim

    -   dimnames

Let DelayedArray handle these.

### Step 5 — Override subsetting safely

Override [ for poparray so it:

`Calls callNextMethod()`

`Wraps the result back into a poparray`

`Preserves roles`

`Respects drop = FALSE`

`Example pattern:`

``` r
setMethod(
```

``` r
"[",
```

``` r
signature(x = "poparray"),
```

``` r
function(x, i, j, ..., drop = FALSE) {
```

``` r
out <- callNextMethod()
```

``` r
new("poparray", out, roles = x@roles)
```

``` r
}
```

``` r
)
```

Do not manually rebuild arrays.

### Step 6 — Remove wrapper logic

Remove:

-   \$handle

-   manual forwarding methods

-   any logic that rebuilds DelayedArray objects

-   duplicated dim/dimnames methods

### Guardrails

-   Do NOT realize the array.

-   Do NOT use as.array().

-   Do NOT duplicate DelayedArray slot definitions.

-   Do NOT reimplement arithmetic operators.

-   Do NOT override DelayedArray math generics.
