# Design Decisions Log

> A short log of decisions made about TypeFighter's language design.
> Each entry is light-weight ADR-style: context, decision, status. The
> long-form *reasoning* lives in the concept docs in this folder — this
> file is just the ledger that says "we decided this, on that date,
> because of that rationale."
>
> Statuses used:
> - **Decided** — committed to; code may or may not reflect it yet
> - **Exploratory lean** — working hypothesis; subject to real-code
>   evidence before committing
> - **Parked** — considered and shelved; reason recorded so it doesn't
>   recycle in three months
> - **Done** — decision + implementation both landed
> - **Superseded** — replaced by a later ADR

---

## ADR-001 — Remove `IntersectionTyp`

**Status:** Done
**Date:** pre-2026 (folded into the het-records plan)
**Context:** The AST once had `IntersectionTyp` as a distinct constructor
for `A & B` between records.
**Decision:** `&` between two record operands is normalised at parse time
into a single merged `RecordTyp`. Conflicting fields are rejected by the
parser. No separate intersection constructor in the AST.
**Related:** [TypeSyntaxWithSets.md §6](TypeSyntaxWithSets.md)

---

## ADR-002 — Records become heterogeneous sets (named + positional)

**Status:** Decided — AST-level refactor planned as a no-op first step
**Date:** 2026 (design phase)
**Context:** A record is not really a map of named fields; it's a set of
items, some of which happen to be properties, some of which are
positional.
**Decision:** `Field` becomes `RecordItem = Property | Positional`. The
type stores two rows (named + positional), with bag-semantics on the
positional row so row polymorphism applies symmetrically.
**Related:** [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md)

---

## ADR-003 — Surface syntax: multi-arg calls, curried core

**Status:** Exploratory lean
**Date:** 2026-04
**Context:** Currying has real costs (invisible arity errors, unfamiliar
call syntax, awkward tooltips). Pipelines + lambdas dominate modern
style, making curry's value proposition smaller than it used to be.
**Decision (tentative):** Surface becomes multi-arg (`f(a, b)`,
`(a, b) => e`) with `_` placeholder sections for partial application.
**The HM core stays curried** — the parser desugars both directions. No
commitment yet; revisit once real code exists.
**Related:** [DoWeNeedCurrying.md](DoWeNeedCurrying.md)

---

## ADR-004 — Record syntax uses round brackets, not curly

**Status:** Superseded by ADR-009 (2026-04-22)
**Date:** 2026-04-22
**Context:** Research into Scala 3 named tuples, Smalltalk, Rémy/Wand
row polymorphism, and the [Sun & Oliveira (ESOP 2025) paper on named
arguments as intersection types](https://i.cs.hku.hk/~bruno/papers/esop25named.pdf)
converges on a single bracket shape for records, tuples, and call-site
argument bundles — with `( … )` as the mainstream choice (Scala, Nim,
Swift, Python, Rust, Haskell).
**Decision:** Value-level records use `( … )`. Type-level record syntax
from [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md) migrates with them.
The old `{ … }` is retired.
**Consequences:**
- `(42)` is grouping; 1-element records need a different disambiguator
  (trailing comma `(42,)` is the Python precedent — open question).
- `()` is a natural candidate for *unit*.
- Function calls already use `( … )`; under [ADR-005](#adr-005--no-positional-arguments-at-call-sites) they
  become record literals, not a separate construct.
**Related:** [RecordsTuplesUnified.md](RecordsTuplesUnified.md),
[CallsAreRecords.md](CallsAreRecords.md)

---

## ADR-005 — No positional arguments at call sites

**Status:** Exploratory lean
**Date:** 2026-04-22
**Context:** Once records and tuples share `( … )`, the natural next
question is whether function *calls* are also records. Smalltalk
demonstrates the answer is "yes, and it's fine" — every call is
keyword-form. Sun & Oliveira (ESOP 2025) give the type-theoretic
framing: named arguments correspond to intersection types, matching the
`&` machinery TypeFighter already has.
**Decision (tentative):** Calls look like `add(x: 1, y: 2)` instead of
`add(1, 2)`. The callee sees one record argument. Positional tuples
survive as local `(1, 2)` literals with synthetic positional items,
destructuring still works — but *call sites* are named-only.
**Reasons to commit:**
1. Arity errors and argument-order bugs become impossible by
   construction.
2. Call sites are self-documenting (Smalltalk readability claim).
3. The algebra is already in place (row polymorphism + `&`).
4. Matches the beginner / LLM-friendliness target from
   [SyntaxForAIAndBeginners.md](SyntaxForAIAndBeginners.md).

**Reasons to wait:**
- Verbosity (`concat(a: "hi", b: name)` vs `concat("hi", name)`).
- Interaction with [ADR-003](#adr-003--surface-syntax-multi-arg-calls-curried-core)
  is conjoined — if we go Smalltalk-style, multi-arg placeholders become
  a slightly different beast.
- No real code to measure against yet.

**Related:** [CallsAreRecords.md](CallsAreRecords.md),
[DoWeNeedCurrying.md](DoWeNeedCurrying.md)

---

## ADR-006 — Parked: "Property as 2-element positional set"

**Status:** Parked (rejected for now)
**Date:** 2026-04-22
**Context:** Eliminating `RecordTyp` + `FieldDefinition` by encoding a
property as an ordered 2-tuple `(String-literal, Type)` inside a
generic container type. Theoretically elegant — one container
constructor instead of two.
**Decision:** Don't do this.
**Reasons:**
- The constructor count doesn't actually drop — you trade `RecordTyp` +
  `FieldDefinition` for an ordered `TupleTyp` and an unordered `SetTyp`.
  Same count, worse algebra.
- Breaks the "row polymorphism twice" symmetry from [ADR-002](#adr-002--records-become-heterogeneous-sets-named--positional)
  because property slots need to be *ordered*, while positional items
  are bag-semantics.
- Unifies `O(1) field lookup` into `filter-plus-uniqueness-check` —
  costlier, and uniqueness must be enforced outside the container's own
  type, not by `Set` equality.
- PureScript ships this exact design. Its error messages are famously
  unreadable because the printer has to heuristically detect
  "this `Cons "name" a r` is really a record" to pretty-print.
- Net: symbolic elegance in the AST, zero win at the user surface,
  real costs in the solver and the error printer.

**Related:** [RecordsTuplesUnified.md §8](RecordsTuplesUnified.md)

---

## ADR-007 — Parked: "Type-keyed" item kind

**Status:** Parked
**Date:** 2026-04-22
**Context:** A fourth item kind beyond named / positional: items
identified by their type alone, with uniqueness at the type level
("this bag contains exactly one `String`"). Unlocks effect rows, typed
dependency-injection bags, discriminated-union payloads without tags.
**Decision:** Don't implement now; revisit once het-records ship and
real patterns emerge that beg for it.
**Reasons:**
- No mainstream language offers this as a first-class surface item —
  no precedent for error-message UX.
- Solver complexity: "is there already a `String` in this bag?" is
  expensive at unification time.
- Interacts ambiguously with patterns (is `n: Number` a named item
  with a type annotation, or a destructure of the type-keyed `Number`?)

**Related:** [RecordsTuplesUnified.md §3.1](RecordsTuplesUnified.md)

---

## ADR-008 — `UnionTyp` stays as a distinct constructor

**Status:** Decided (confirmed 2026-04-22)
**Date:** 2026-04-22
**Context:** `RecordTyp` and `UnionTyp` both wrap a `Set<MonoTyp>`.
Worth asking: collapse them?
**Decision:** No. Keep both as separate constructors.
**Reasons:**
- Semantics are opposite: `RecordTyp S` is "contains all of S"
  (product / AND); `UnionTyp S` is "is one of S" (sum / OR).
- Collapsing to `SetTyp of Mode * Set<MonoTyp>` is cosmetic bundling —
  the unification logic stays fully separate per mode.
- The only world in which the collapse is genuine is set-theoretic
  type theory à la CDuce, which is a different solver architecture
  (subtyping-based, not unification-based). Not what TypeFighter is.

**Related:** [TypeSyntaxWithSets.md §4.1](TypeSyntaxWithSets.md)

---

## ADR-009 — Curly braces for records; parens are grouping only

**Status:** Decided (supersedes ADR-004)
**Date:** 2026-04-22
**Context:** ADR-004's unified `(…)` for grouping + records forced
the parser to sniff content to pick its meaning, and needed a
trailing-separator tax (`(x,)` vs `(x)`, `(T &)` vs `(T)`) for the
1-element case. Reconsidering against the broader research matrix
— ML-classic, F#-mixed, Mainstream — none of the three pure models
gave all four properties we want (records/calls rhyme, auto-partial
on named under-application, optional-args with union-type fundament,
beginner-friendly). A synthesis works: Mainstream-style record
brackets `{ … }` on the surface, with the call-site still reading
named args into a record (so `f(name: "Ada")` is sugar for
`f({ name: "Ada" })`).
**Decision:** Value-level record literals use `{ … }`. Type-level
record syntax uses `{ … }`. Parens `( … )` are pure grouping —
one expression inside, no sniffing. Tuples are subsumed by
positional records (`{ 1, 2, 3 }`). Arrays keep `[ … ]`. Empty
parens `()` remain the unit value as a special case.
**Consequences:**
- Reverts the surface-level changes from Slices A and B; printers
  back to `{ … }`.
- Each bracket shape now has a single role — parser-cognitive cost
  drops.
- Call-site named-arg sugar (from the Slice after A) stays as-is;
  it was independent of ADR-004.
- Tuples don't get their own AST constructor; positional records
  cover the use case.
**Related:** [RecordBracketsReconsidered.md](RecordBracketsReconsidered.md),
[CallsAreRecords.md](CallsAreRecords.md),
[TypeSyntaxWithSets.md](TypeSyntaxWithSets.md)

---

## How to add an entry

- Copy an existing block; fill in the fields.
- Status says whether we've committed. If you're tempted to write
  "ongoing discussion", the decision isn't ready for this log — keep it
  in the concept doc until it is.
- Link the long-form reasoning in the concept doc; don't copy it here.
- Never edit a prior ADR's decision retroactively — add a new ADR with
  status `Superseded` pointing at the old one.
