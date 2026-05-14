# tarr_pop Codex Skills

This directory contains Codex skills used to assist in the development of the `tarr_pop` R package.

The skills are designed to enforce:

-   Lazy computation (DelayedArray / HDF5Array)
-   Strong dimensional semantics
-   Metadata integrity
-   Reproducible and scalable workflows

------------------------------------------------------------------------

# ⚠️ Strict Mode

This skill suite operates under **Strict Mode** by default.

In Strict Mode:

-   Solutions that violate lazy evaluation SHOULD NOT be provided unless explicitly requested
-   Unsafe aggregation across overlapping categories MUST NOT be allowed
-   Dimensional semantics MUST be respected at all times
-   Metadata integrity MUST NOT be compromised
-   EAGER operations MUST be:
    -   clearly labeled
    -   justified
    -   avoided when a lazy alternative exists

If a request conflicts with these rules, the correct behavior is to:

-   explain why the request is unsafe or invalid
-   propose a safe alternative

Strict Mode ensures that all generated code is:

-   correct
-   scalable
-   reproducible
-   aligned with epidemiologic best practices

# Overview

The skill system is modular and centered around a routing layer (`tarr-pop-orchestrator`) that delegates tasks to specialized skills.

Each skill focuses on a specific concern within the package architecture.

------------------------------------------------------------------------

# Skill Architecture

## Core router

### tarr-pop-orchestrator

-   Entry point for all requests
-   Interprets intent and routes to the correct skill
-   Enforces global package rules

------------------------------------------------------------------------

## Specialized skills

### tarr-pop-lazy-ops

-   Implements and optimizes array operations
-   Ensures all transformations remain lazy when possible
-   Handles:
    -   filter
    -   select
    -   collapse_dim
    -   group_dim
    -   subsetting

------------------------------------------------------------------------

### tarr-pop-semantics-guard

-   Enforces dimensional semantics and aggregation safety
-   Prevents double-counting and invalid reductions
-   Handles:
    -   overlap detection
    -   safe vs unsafe aggregation
    -   age interval logic

------------------------------------------------------------------------

### tarr-pop-io-schema

-   Manages HDF5 storage and metadata schema
-   Ensures consistent cube structure and round-tripping
-   Handles:
    -   open/save/create_poparray
    -   metadata validation
    -   shared storage strategies

------------------------------------------------------------------------

### tarr-pop-projection

-   Handles forecasting and projection workflows
-   Operates per-series to avoid full cube realization
-   Manages uncertainty and projection outputs

------------------------------------------------------------------------

### tarr-pop-class-methods

-   Defines constructors, validators, and methods
-   Maintains class contracts and return types
-   Handles:
    -   S3/S4 methods
    -   coercions
    -   summary and display

------------------------------------------------------------------------

### tarr-pop-tests-docs

-   Creates and maintains tests and documentation
-   Ensures correctness and usability
-   Handles:
    -   testthat
    -   roxygen2
    -   vignettes

------------------------------------------------------------------------

### tarr-pop-repo-auditor

-   Audits code for:
    -   lazy violations
    -   semantic errors
    -   metadata inconsistencies
    -   missing methods
-   Provides prioritized, actionable fixes

------------------------------------------------------------------------

# Design Principles

## 1. Lazy-first computation

All operations should remain delayed unless explicitly required.

-   Avoid:
    -   as.array()
    -   as.data.frame()
    -   apply()

These are considered **EAGER** and must be explicitly labeled.

------------------------------------------------------------------------

## 2. Dimensional semantics

Each dimension has explicit semantics:

-   partition (mutually exclusive)
-   set (potential overlap)
-   interval (e.g., age)

Operations must respect these semantics at all times.

------------------------------------------------------------------------

## 3. Safe aggregation

Aggregation must not occur when categories overlap.

-   Unsafe operations should:
    -   error, or
    -   warn explicitly

Filtering may change a dimension from unsafe → safe.

------------------------------------------------------------------------

## 4. Metadata integrity

All transformations must preserve:

-   dimnames
-   roles (time, area)
-   dim_semantics

Metadata must always remain aligned with the data.

------------------------------------------------------------------------

## 5. Reproducibility

-   All cubes must be fully described by metadata
-   HDF5 schema must be consistent and version-safe
-   No implicit assumptions about structure

------------------------------------------------------------------------

# Expected behavior of skills

All skills should:

-   Prefer lazy solutions
-   Clearly mark **EAGER** alternatives
-   Preserve dimensional and metadata integrity
-   Suggest:
    -   tests (testthat)
    -   documentation (roxygen2)

------------------------------------------------------------------------

# Usage pattern

Typical workflow:

1.  A request is made (e.g., “Refactor filter()”)
2.  `tarr-pop-orchestrator` classifies the task
3.  The request is routed to a specialized skill
4.  The skill returns a structured, rule-compliant response

------------------------------------------------------------------------

# Extending the system

New skills can be added under: ".codex/skills/"

------------------------------------------------------------------------

Examples:

-   tarr-pop-optimizer
-   tarr-pop-visualization
-   tarr-pop-geo

All new skills should:

-   follow the shared template
-   respect core rules
-   integrate with the orchestrator

------------------------------------------------------------------------

# Notes

-   This system is designed for development-time assistance, not runtime execution
-   Skills should favor correctness and safety over convenience
-   When in doubt, prefer:
    -   lazy evaluation
    -   explicit validation
    -   conservative behavior
