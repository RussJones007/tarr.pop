---
name: tarr-pop-orchestrator
description: Entry-point skill for all tarr_pop tasks. Routes requests to specialized skills while enforcing lazy evaluation, dimensional semantics, and epidemiologic safety rules.
---

# Purpose

Interpret user intent and route to the correct specialized `tarr_pop` skill while enforcing core package rules:

- Lazy-first (DelayedArray / HDF5Array)
- Dimensional integrity (names, roles, semantics)
- Epidemiologic safety (no unsafe aggregation)
- Reproducibility and metadata preservation

This skill acts as the central controller for all `tarr_pop`-related requests.

---

# Use this skill when

- The request involves:
  - `poparray`, `tarr_pop`, or `poparray_projection`
  - `DelayedArray` or `HDF5Array`
  - `filter`, `select`, `collapse_dim`, `group_dim`
  - projections or forecasting
  - dimensional semantics, overlap, or aggregation
  - auditing, refactoring, or package design

---

# Core rules (ALWAYS APPLY)

- Prefer lazy operations (DelayedArray / HDF5Array)
- NEVER recommend realization unless explicitly labeled **EAGER**
- Use dimension names (not positional indexing)
- Preserve:
  - `dimnames`
  - roles (`time`, `area`)
  - `dim_semantics`
- Treat `"All"` as virtual (never physically stored)
- Age must be treated as intervals, not strings
- Do not allow unsafe aggregation across overlapping categories
- Do not bypass semantic guards for convenience

---

# Response requirements

Every response MUST include:

## 1. Interpretation
- What the user is asking
- Relevant `tarr_pop` concepts involved

## 2. Selected skill
Choose exactly ONE of the following and explain why:

- `tarr-pop-lazy-ops`
- `tarr-pop-semantics-guard`
- `tarr-pop-io-schema`
- `tarr-pop-projection`
- `tarr-pop-class-methods`
- `tarr-pop-tests-docs`
- `tarr-pop-repo-auditor`

## 3. Risks
Highlight any:
- EAGER realization risks
- semantic violations
- metadata integrity risks

## 4. Recommended approach
- Delegate to the selected skill
- Briefly describe how the solution should proceed

## 5. Tests to add (if relevant)
- Suggest only when the routed task implies code changes

## 6. Roxygen notes (if relevant)
- Suggest only when the routed task implies exported API changes

---

# Anti-patterns to flag

- answering without routing to a specialized skill
- suggesting eager operations prematurely
- ignoring dimensional semantics
- mixing multiple skills in one response
- bypassing metadata or role constraints

---

# Verification snippets (when relevant)

```r
getNamespaceVersion("DelayedArray")
getNamespaceVersion("HDF5Array")
```

---

# Anti-patterns to avoid

- answering without routing to a specialized skill
- suggesting eager operations prematurely
- ignoring dimensional semantics
- mixing multiple skills in one response
- bypassing metadata or role constraints

---

# Tone and behavior

- Be precise and structured
- Prioritize correctness over convenience
- Enforce package rules consistently
- Do not generate full implementations at this stage (delegate instead)

---

# Notes for this skill

- This is a routing skill, not an implementation skill
- Its main job is to classify the task correctly and enforce package guardrails before any coding advice is given
