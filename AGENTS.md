# tarr.pop development instructions

## Role and scope

Assist with designing, implementing, testing, documenting, and releasing
the tarr.pop R package.

The package stores population estimates and projections from multiple
sources using HDF5Array and DelayedArray infrastructure.

Use clear, concise, practical explanations suited to an experienced R
developer.

## Authoritative package specification

Read and follow:
- .codex/skills/tarr-pop-repo-guard/SKILL.md
- .codex/skills/tarr-pop-repo-guard/references/repo-guidelines.md

These files are authoritative for package-domain behavior.
If these workflow instructions conflict with the repository guard's
domain rules, follow the repository guard.

Keep detailed domain rules in the repository guard rather than
duplicating them here.

For response workflow, the instructions here supersede the requirements in
sections 6 and 13 of the referenced guidelines to provide separate Base R
and tidyverse alternatives. Present the recommended lazy approach, relevant
memory implications, tests, and documentation without requiring alternative
implementations.

## Lazy computation and memory

Prefer solutions that preserve delayed computation and remain safe for
DelayedArray and HDF5Array objects.

Explicitly identify operations that realize delayed data or materialize
large objects. Explain their memory implications.

Avoid introducing eager computation merely for implementation convenience.

Avoid unnecessary nonstandard evaluation.

## Dependency and API verification

Target versions:
- DelayedArray 0.36.3
- HDF5Array 1.38.0

Before using or recommending dependency functions, verify that they exist
and support the required behavior in the target versions.

Use installed package inspection, version-specific documentation, or
official source code. Do not treat availability in a newer release as
proof of availability in the target version.

If verification is unavailable, state the uncertainty and provide an R
verification snippet and a supported fallback where possible.

## Implementation

Write modular, maintainable R code.

Prefer appropriate methods for existing generics when their contracts
fit the intended behavior.

Follow the repository's style and object-system conventions.
Use checkmate and other development tools where consistent with the
existing package.

Keep changes focused on the requested task.

## Testing

Add or update meaningful testthat tests for changed behavior.

Include relevant coverage for laziness, dimensions, metadata, age
semantics, and method contracts when affected.

Place tests under tests/testthat/.

Run focused tests during development and broader package checks as
appropriate. Report what ran, failures, and anything that could not be
verified.

## Documentation

Update roxygen2 documentation for changed public behavior, including
parameters, return values, examples, and exports as applicable.

Regenerate documentation with devtools::document() when needed.
Update vignettes when a change affects documented workflows.

## Version control and releases

Respect the existing Git repository and CI configuration.

Configure Git, GitHub, or GitHub Actions only when relevant to the task.
Do not initialize a replacement repository unnecessarily.

Prepare release checks and documentation when requested.
Publish releases or submit to CRAN only when explicitly authorized.

## Reporting

Lead with the recommended lazy approach or completed change.

Explain relevant laziness and memory implications, tests, documentation
changes, and unresolved limitations.

Consult authoritative sources for external technical claims.
Distinguish verified behavior from assumptions.



## Development testing policy

Use a risk-based testing strategy to minimize unnecessary repeated full-suite execution.

## During implementation

1. Run the relevant tests for the functionality being modified.
2. Add regression tests for confirmed defects.
3. When modifying shared helpers, also run tests for affected public interfaces and downstream callers.
4. Re-run targeted tests after corrections until they pass.
5. Do not automatically run the entire test suite or `R CMD check` after every individual change.
6. Use the actual repository test filenames and dependencies to select appropriate tests.

## Full validation checkpoints

Run the complete test suite and `R CMD check`:

- When incrementing the development package version.
- Before merging a development branch into the main branch.
- Before a release.
- After substantial architectural changes or changes with broad package-wide effects.
- When specifically requested.

At these checkpoints, include relevant examples and vignette checks.

## Reporting

For each implementation task, report:

- Tests selected and why.
- Number of passing, failing, and skipped tests.
- Any affected functionality not covered by the targeted run.
- Whether full-suite validation was performed or deferred.

Do not describe targeted testing as a complete package validation.

If a change is unusually risky, recommend broader testing and explain why before running it.

**Default:** Targeted tests during development; comprehensive validation at defined milestones.
