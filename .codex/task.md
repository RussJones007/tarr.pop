TASKS 8–10 — PERFORMANCE REGRESSION TESTS, METADATA READ OPTIMIZATION, AND FINAL BENCHMARKING

Follow AI_GUIDELINES.md throughout this work.

Do NOT implement local caching, checkout, local cube copying, synchronization, or cache invalidation. Those features are explicitly reserved for a separate development effort.

Preserve lazy DelayedArray/HDF5Array behavior and all dimensional and semantic integrity contracts.


======================================================================
TASK 8 — Add performance regression tests based on HDF5 I/O counts
======================================================================

The previous tasks substantially reduce repeated filesystem and HDF5 metadata operations.

Now add automated tests that prevent future changes from accidentally reintroducing repeated HDF5 hierarchy scans.

Do NOT use elapsed-time thresholds for these tests.

Network speed, disk caching, operating systems, CI environments, and machine load make wall-clock timing tests unreliable.

Instead, test the STRUCTURAL I/O behavior of open_poparray().


REQUIREMENTS

1. Add testthat tests that count or mock calls to the package's HDF5 inventory helper.

   If previous tasks introduced an internal wrapper such as:

       h5_inventory(path)

   prefer counting calls to that wrapper rather than attempting to mock rhdf5 internals directly.

2. Keep the wrapper small and focused. Do not create a large I/O abstraction layer solely for testing.

3. Establish the following performance contract for ordinary opening of a canonical current-schema cube:

   - registry lookup must not inspect unrelated HDF5 cubes;
   - the selected cube's HDF5 hierarchy must be enumerated no more than once;
   - increasing the number of dimensions must not increase the number of hierarchy scans;
   - validObject() must not cause another HDF5 hierarchy scan;
   - opening must not realize population values.

4. Create temporary canonical HDF5 cubes for testing.

5. Include at least two cubes in tests so it can be demonstrated that opening cube A does not interrogate cube B.

6. Include cubes with different dimensionality.

   For example:

       2-dimensional cube
       6-dimensional cube

   Reading the 6-dimensional cube will naturally require more metadata values such as dimnames and DimSemantics.

   That is acceptable.

   What must NOT increase proportionally is the number of complete h5ls()/inventory scans.

7. Add a test specifically for DimSemantics reconstruction.

   The number of dimensions must not multiply hierarchy enumeration.

8. Add a test for:

       methods::validObject(x)

   after a poparray has been opened.

   This operation must not cause a new HDF5 hierarchy scan.

9. Add a test demonstrating that population values are not realized during open_poparray().

   Verify the returned object remains HDF5Array/DelayedArray-backed according to the package's existing class architecture.

10. Do not make tests depend on an actual network drive.

11. Do not use production cubes.

12. Do not test exact numbers of small h5read() calls unless there is a strong reason.

    The primary regression risk is repeated hierarchy scanning.

    Avoid making tests unnecessarily brittle to harmless metadata implementation changes.


TARGET PERFORMANCE CONTRACT

Opening one current-schema registered cube should approximately produce:

    unrelated HDF5 cubes:
        0 hierarchy scans

    selected HDF5 cube:
        <= 1 hierarchy scan

    population data realization:
        0

    validObject() after construction:
        0 additional hierarchy scans

If the implementation from earlier tasks safely eliminates hierarchy enumeration altogether, adjust the test to enforce the better behavior.


RUN:

    devtools::test()
    devtools::check()

Commit these tests independently before proceeding.


======================================================================
TASK 9 — Review and eliminate unnecessary duplicate metadata reads
======================================================================

The major repeated h5ls()/hierarchy-scan problem should now be solved.

Perform a focused review of open_poparray() and its helper functions for remaining duplicate HDF5 metadata reads.

Do NOT redesign the HDF5 schema during this task.

The canonical fieldwise metadata schema should remain unchanged.


REQUIREMENTS

1. Trace one complete call to:

       open_poparray(series_id)

   from registry lookup through new_poparray() construction.

2. Identify every call to:

       rhdf5::h5read()

   made during the opening process.

3. Determine whether any metadata dataset is read more than once during the same open operation.

4. If a metadata value has already been read, pass or reuse the in-memory value rather than rereading it from HDF5.

5. Pay particular attention to:

       cube/metadata/dim_order
       cube/metadata/dimnames/*
       cube/metadata/roles/*
       cube/metadata/source/*
       cube/metadata/dim_semantics/*

6. Also review whether constructor or validation helpers reread metadata that open_poparray() has already obtained.

7. Do not remove required integrity checks merely to reduce I/O.

   Instead, perform checks using already-read values whenever possible.

8. Preserve:

       validate_labels_against_cube()

   or equivalent dimensional-integrity checking.

9. Preserve DimSemantics validation.

10. Preserve source/provenance metadata.

11. Preserve time and area role validation.

12. Do not read population values during opening.

13. Do not combine all HDF5 metadata into one large new dataset merely to reduce reads.

    That would constitute a schema redesign and is outside this task.

14. Do not introduce local caching or checkout behavior.

15. Keep the implementation simple.


DESIRED PATTERN

    read metadata once
        ->
    keep it in local R objects
        ->
    pass those objects through construction/validation
        ->
    do not reread the same HDF5 metadata


AVOID THIS PATTERN

    helper A reads metadata
        ->
    helper B rereads same metadata
        ->
    constructor rereads same metadata
        ->
    validity rereads same metadata


TESTS

Add focused tests for any duplicate-read behavior that is eliminated.

Do not create overly brittle tests requiring an exact total h5read() count unless appropriate.

It is acceptable to test that specific important datasets are not reread.

Confirm that:

    dim()
    dimnames()
    time_role()
    area_role()
    dim_semantics()
    get_source()

on the resulting poparray return the expected information.

Confirm that population data remains lazy and HDF5-backed.


RUN:

    devtools::test()
    devtools::check()

Commit this task independently before proceeding.


======================================================================
TASK 10 — Benchmark, document, and verify optimized open_poparray()
======================================================================

The functional optimization should now be complete.

Create a developer-oriented benchmark and final verification of the new open_poparray() architecture.

This benchmark is NOT an automated performance test with required timing thresholds.

Its purpose is to make the performance improvement observable and provide a tool for future investigation.


REQUIREMENTS

1. Create a small developer benchmark script in an appropriate development location.

   Do not place benchmark code in normal package runtime code.

   Follow existing repository conventions if a benchmark or development directory already exists.

2. The benchmark should create or use representative canonical test cubes.

3. Measure:

       system.time(open_poparray(...))

   but treat elapsed time as descriptive information only.

4. More importantly, instrument and report counts for:

       directory/registry enumeration
       HDF5 hierarchy inventory/h5ls calls
       metadata h5read operations
       population-data realization

5. Demonstrate that ordinary opening now approximately follows:

       lightweight registry lookup
           ->
       select one filepath
           ->
       inspect selected HDF5 cube once
           ->
       read required metadata
           ->
       construct lazy poparray

6. Demonstrate that it does NOT follow:

       scan directory
           ->
       open every HDF5 cube
           ->
       repeatedly h5ls selected cube
           ->
       repeatedly reread metadata
           ->
       construct object

7. Confirm explicitly that opening does not realize population values.

8. If a network share is available during development, optionally run the benchmark against representative network-hosted cubes.

   Do NOT make network availability part of automated tests.

9. If possible, record representative before/after measurements in developer documentation or the commit/PR description.

10. Do not promise a particular speed improvement because network characteristics vary.

11. Document the architectural performance contract in an appropriate developer-facing location.


DOCUMENT THESE PRINCIPLES

Ordinary open_poparray() must inspect only the selected cube.

Ordinary open_poparray() must not rebuild the HDF5 registry.

Metadata existence checks must use an already obtained HDF5 inventory rather than repeatedly calling h5ls().

The number of dimensions must not multiply complete HDF5 hierarchy scans.

S4 object validity must validate in-memory object structure and must not rescan persisted HDF5 storage.

Persisted HDF5 schema validation belongs at the file/opening/persistence boundary.

Population data must remain lazy during opening.


ROXYGEN2 REVIEW

12. Review roxygen2 documentation for any exported functions introduced during Tasks 1–9.

13. If the registry rebuild/refresh function introduced in Task 5 is exported, document:

       @param
       @return
       @details
       @examples

   as appropriate.


FINAL PACKAGE CHECK

14. Run:

       devtools::document()
       devtools::test()
       devtools::check()

15. Resolve any errors, warnings, or notes introduced by this work.


FINAL VERIFICATION

Before considering the optimization complete, confirm all of the following:

1. open_poparray(series_id) returns a valid poparray.

2. Population storage remains HDF5Array/DelayedArray-backed.

3. Population values are not realized during opening.

4. Opening one series does not inspect unrelated cubes.

5. Registry reconstruction is not performed during normal opening.

6. The selected cube hierarchy is enumerated no more than once.

7. Reading DimSemantics does not cause repeated hierarchy scans.

8. validObject() does not access the backing HDF5 file.

9. Metadata and dimensional integrity checks remain intact.

10. Existing filtering and other lazy operations continue to work.

11. devtools::test() passes.

12. devtools::check() passes.


======================================================================
OUT OF SCOPE FOR TASKS 8–10
======================================================================

Do NOT implement:

    local cube caching
    checkout/check-in
    automatic network-to-local copying
    cache expiration
    cache manifests
    checksums for cached copies
    synchronization
    write-back to network cubes

Those features will be designed and implemented separately after the network-opening optimization has been benchmarked.
