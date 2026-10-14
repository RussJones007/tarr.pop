# Package separation and TexasPop discussion

Discussion: October 13, 2026. These are proposals for possible future changes;
no package split, rename, migration, or new download API has been implemented.

## Proposed responsibilities

Keep a general cube engine for poparray classes, delayed reading, filtering,
grouping, measure-specific operations, and generic cube persistence. Retain a
small example dataset so users can learn without external downloads.

Move source-specific downloading, transformation, and cube build/update workflows
into an optional builder package with documented functions. Census and ACS
builders should accept state and geography inputs; TDC support is Texas-specific.
The current data-raw directory is excluded from installed packages, so a builder
package would expose workflows that users currently cannot obtain by installing
tarr.pop alone.

Distribute prepared, versioned HDF5 cubes independently of engine releases,
either through data packages or downloadable files with a catalog and selective
download helper. Large collections favor selective downloads to a configurable
user folder. Record source vintage, schema version, integrity checks, and reader
compatibility to support reproducible analysis.

## TexasPop concept

Consider TexasPop as the Texas-facing offering with a prepared Texas cube catalog,
Texas-focused documentation, and a convenient setup/download workflow. Ordinary
Texas users should be able to obtain prepared cubes without running builders.
Users in other states should be able to use the general builder tools to create
their own state data. Keep local paths, Texas county selections, and TDC-specific
reporting rules outside the general engine and national-source builders.

The naming and number of packages remain undecided. Define responsibilities and
compatibility boundaries before deciding whether to rename tarr.pop or add a
separate TexasPop entry point.

## Tradeoffs and follow-up

Benefits: smaller installations, fewer optional builder dependencies for analysts,
independent data updates, reusable builders, and a clear Texas-centered product.
Costs: multiple release processes, cross-package compatibility tests, documentation
of setup and storage locations, and maintenance of data downloads and catalogs.

Follow-up work should inventory generic versus source-specific code, define
stable builder inputs and storage contracts, decide distribution and versioning,
and design migration for existing users. This can build on the separate shared
numeric-measure storage proposal in shared_measure_storage_discussion.md.
