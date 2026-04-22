# Open Research Topics

> A running list of design questions flagged for later work — things we
> haven't committed to, but don't want to lose. Items get added as they
> come up in discussions. Once resolved they migrate to
> [DesignDecisions.md](DesignDecisions.md) as an ADR.
>
> Format: one short paragraph per item, with a date-added, and a link
> to any related concept doc or paper.

## Active

- [ ] **Invocation of optional functions** *(added 2026-04-22)*

  What happens when the *function itself* is optional in scope — e.g.
  a value of type `Option<(x: Number) -> Number>` that you want to
  call? Swift handles this with optional chaining (`f?(x: 1)`). Null
  / Option values in a call position need their own semantic story.
  Related: whether we want a dedicated short-circuit operator, or
  whether this folds into the same framework as optional *arguments*
  below.

- [ ] **Comfortable call-site syntax for optional arguments** *(added 2026-04-22)*

  Once functions take named records as arguments (ADR-005), optional
  slots become expressible as *unions with `absent`* per Sun & Oliveira
  (ESOP 2025, [Named Arguments as Intersections, Optional Arguments as
  Unions](https://i.cs.hku.hk/~bruno/papers/esop25named.pdf)). Open
  design questions:
  - Syntax at the definition site — `y: Number?`, `y?: Number`, `y: Number | absent`?
  - Default values — `y: Number = 0` or separate form?
  - Can the caller omit optional slots wholesale, or does each one
    need explicit `y: absent` / `y: omit`?
  - Does this interact with auto-partial under ADR-005 — i.e. is
    "omitting an optional" distinguishable from "under-applying a
    required arg"?

## Resolved

*(Migrated to [DesignDecisions.md](DesignDecisions.md) as ADRs when decided.)*
