# Codex Session Starter Template

## Project

tarr-pop R package

## Context

-   This is a population analytics package built in R

-   Core architecture uses DelayedArray (lazy evaluation, avoid eager computation)

-   Primary classes include:

    -   poparray
    -   poparray_projection

-   Data is multi-dimensional (year, area, sex, age, race, ethnicity)

-   Dimension semantics and invariants must be preserved

## Key Design Principles

-   Prefer lazy operations (DelayedArray) over eager computation
-   Maintain semantic integrity of dimensions (no silent aggregation errors)
-   Ensure reproducibility and correctness for epidemiological use
-   Avoid breaking existing APIs unless explicitly requested

## Rules (from AI_guidelines.md)

-   Be precise and actionable
-   Prefer minimal, high-impact fixes over large rewrites
-   Do NOT rewrite entire files unless explicitly asked
-   Focus on correctness, safety, and performance
-   Preserve class structure and metadata contracts
-   Respect existing naming conventions (underscore style)

## Coding Preferences

-   Follow base R / S3 / S4 patterns already established in the package
-   Maintain compatibility with DelayedArray and Bioconductor ecosystem
-   Avoid unnecessary dependencies
-   Keep functions small and composable
-   Favor clarity over cleverness

## Task

[Replace this section with your specific request]

## Output Requirements

-   Explain reasoning briefly (only where needed)

-   Provide code that is ready to use

-   Highlight any risks or edge cases

-   If suggesting changes:

    -   Show minimal diffs or focused snippets
    -   Do not dump entire files unless requested

## Optional (use when needed)

### Files to Reference

-   README.md
-   AI_guidelines.md
-   class documentation (e.g., poparray_class.md)

### Mode

Choose one:

-   Audit
-   Refactor
-   Optimize
-   Document
-   Design
