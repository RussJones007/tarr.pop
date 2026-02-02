## Fixing the projection API

### ✅ Use **multiple threads**, but follow a **fixed anchoring pattern**

Each thread should:

1.  Target **one issue or one refactor decision**
2.  Start with a **short context block**
3.  Reference the **canonical file and design decisions**

------------------------------------------------------------------------

## How to start each thread (copy/paste template)

When you open a new thread, start it like this:

> **Context** We are refactoring `projection.r` in the `tarr-pop` R package. This is a user-facing projection API (not build-time). Key design decisions already made:
>
> -   Projections support **one confidence band at a time** (`lower`, `upper`, default level = 95)
> -   Cubes may be **5D (TDC: `race_eth`)** or **6D (Census: `race` + `ethnicity`)**
> -   Projections operate on filtered `tarr_pop` objects
> -   Correctness \> convenience; no silent realization
>
> **This thread focuses on:** `<one specific issue>`

That’s enough for me to reconstruct the full mental model instantly.

------------------------------------------------------------------------

## Suggested thread breakdown for *your* situation

Here’s how I would split the work cleanly:

### Thread 1 — Projection API shape (design-level)

**Focus:**

-   `level` vs `levels`
-   return schema (`mean`, `lower`, `upper`, `level`)
-   reconciliation semantics

(You’ve already mostly done this, but it’s worth locking down.)

------------------------------------------------------------------------

### Thread 2 — Dimension schema normalization (5D vs 6D)

**Focus:**

-   robust detection of TDC vs Census cubes
-   canonical internal representation
-   avoiding `has_dim(tp, "race_eth")` fragility

This one is *architectural* and deserves isolation.

------------------------------------------------------------------------

### Thread 3 — `infer_base_years()` correctness

**Focus:**

-   logic bug (returning all years)
-   expected semantics (preferred window vs contiguous tail)
-   tests

This is a small, surgical fix — perfect for its own thread.

------------------------------------------------------------------------

### Thread 4 — Series extraction & subsetting consistency

**Focus:**

-   unify `filter()` vs `[` usage
-   year type coercion (character vs integer)
-   avoiding silent empty series

This impacts correctness and reproducibility.

------------------------------------------------------------------------

### Thread 5 — Performance / safety guards

**Focus:**

-   grid explosion warnings
-   chunking strategy
-   user-facing messaging

Optional, but very worthwhile.

------------------------------------------------------------------------

## Will I understand the context later?

**Yes — if you anchor each thread as above.** No problem understanding:

-   earlier design decisions
-   why a function behaves the way it does
-   how changes interact with the rest of the package

If you *don’t* anchor, I’ll still help — but we’ll waste time re-deriving assumptions.

------------------------------------------------------------------------

## My strong suggestion

Start **Thread 1** with either:

-   “Let’s finalize the new projection API signature”, **or**
-   “Let’s fix the 5D vs 6D schema detection first”

Those two unlock everything else.

When you open the next thread, paste the context block and say:

> “This thread focuses on X”

I’ll pick it up instantly and stay consistent across threads.
