# TypeFighter Design Notes

Working notes for language and parser design. Most files here are
*design documents* — options, tradeoffs, open questions.

Firm commitments (what we've actually decided) live in
[DesignDecisions.md](DesignDecisions.md) as a short ADR-style ledger.
Check there first when wondering "is X decided or still speculative?"

Flagged-but-unresolved questions live in
[OpenResearch.md](OpenResearch.md) — a running to-do list that
migrates into ADRs as items get answered.

## Language surface

- [SyntaxAlternatives.md](SyntaxAlternatives.md) — five genuinely different
  concrete syntaxes (Pythonesque, BASIC, Pipeline-first, Smalltalk,
  Concatenative) with the same small vocabulary in each.
- [SyntaxForAIAndBeginners.md](SyntaxForAIAndBeginners.md) — what research
  says helps LLM code generation and novice programmers; re-ranks the five
  syntax alternatives against those criteria.
- [SyntaxForPxlClock.md](SyntaxForPxlClock.md) — applying the same lens to
  PXL Clock pixograms specifically; shader-style and turtle come out on top.

## Semantics

- [ImplicitBinds.md](ImplicitBinds.md) — can we drop F#-style `let!` and make
  `let` auto-bind when the RHS is monadic? Catalogue of ambiguities, three
  resolutions, recommendation toward algebraic effects over row-tracked types.
- [WrapperUnpackProtocol.md](WrapperUnpackProtocol.md) — parked refinement to
  implicit-bind: the wrapper type itself opts in. Fits row polymorphism,
  fixes the List-vs-Task intent problem, does not solve monad composition.
- [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) — generalise
  records from *maps of named fields* to *sets of mixed items* (properties +
  positional values). AST, parser, and inference implications, with a
  recommended path (row polymorphism, twice).
- [RecordsTuplesUnified.md](RecordsTuplesUnified.md) — push the het-record
  further: one `(…)` bracket for records + tuples, plus a fourth "type-keyed"
  item kind. Surveys Scala 3 named tuples, Nim, Swift, Roc, CDuce; tentative
  lean is to park both extensions until the simpler het-record ships.
- [CallsAreRecords.md](CallsAreRecords.md) — the convergence: Sun & Oliveira
  (ESOP 2025) give the type-theoretic frame, Smalltalk gives the precedent,
  Rémy/Wand row polymorphism is the engine we already have. Decides round
  brackets (ADR-004); leans toward named-only call sites (ADR-005).
- [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md) — type-level counterpart:
  `|`, `&`, and `{ … }` as set combinators with literal types. Subsumes
  `UnionTyp` as a surface concept, gives discriminated unions for free,
  sets up pattern matching.
- [DoWeNeedCurrying.md](DoWeNeedCurrying.md) — is mandatory currying still
  paying for itself given planned pipelines and operators? Tentative lean
  toward multi-arg surface + `_` placeholder sections, curried HM core.
