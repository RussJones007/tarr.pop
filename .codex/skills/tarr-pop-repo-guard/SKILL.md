---
name: tarr-pop-repo-guard
description: Repository-wide guardrails for any tarr.pop task involving poparray, poparray_projection, DelayedArray, HDF5Array, cube metadata, dimensional semantics, lazy workflows, or epidemiologic aggregation safety.
---

# Purpose

Provide the shared repository rules for `tarr.pop` work so task-specific skills
do not need to duplicate them.

# Use this skill when

- The task involves `tarr.pop` package code, docs, tests, or design
- The task touches:
  - `poparray` or `poparray_projection`
  - `DelayedArray` or `HDF5Array`
  - cube storage or metadata
  - dimensional semantics, roles, or labels
  - aggregation safety
  - age interval handling

# Core workflow

1. Keep operations lazy unless realization is explicitly required.
2. Preserve `dimnames`, roles, and `dim_semantics`.
3. Use dimension names, not positional assumptions.
4. Treat `"All"` as virtual unless explicitly justified otherwise.
5. Treat `age.char` as interval-valued, not plain strings.
6. Fail clearly when semantic or metadata contracts would be violated.

# What to read

- Read `references/repo-guidelines.md` before making substantial code, test,
  documentation, or design changes in this repository.

# What this skill is for

- Shared repository standards
- Lazy-vs-eager guardrails
- Source-of-truth guidance for code style, testing, and documentation

# What this skill is not for

- It is not a replacement for specialized skills such as I/O schema,
  projection, semantics guard, or tests/docs.
- Use those skills for task-specific workflows after applying these repo-wide
  constraints.
