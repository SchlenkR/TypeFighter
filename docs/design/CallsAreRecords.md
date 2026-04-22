# Calls Are Records — Surface-Level Unification

> **Status:** Round-bracket syntax decided (ADR-004). Named-only call
> sites is the exploratory lean (ADR-005). See
> [DesignDecisions.md](DesignDecisions.md) for the ledger.
>
> Builds on: [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md),
> [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md),
> [RecordsTuplesUnified.md](RecordsTuplesUnified.md),
> [DoWeNeedCurrying.md](DoWeNeedCurrying.md).

## 1. The one-sentence version

Records, tuples, and function-call argument bundles are all the same
thing — a heterogeneous set written with `( … )` — and on the function
side, a call is just application to that one record argument.

## 2. What TypeFighter already has

This is not a greenfield design. Row-polymorphic records, literal
types, and union types are already in the type AST. From
[Lang.fs](../../src/TypeFighter/Lang.fs):

```fsharp
type MonoTyp =
    | TVar of VarNum
    | SaturatedTyp of {| name: string; args: MonoTyp list |}
    | FunTyp of MonoTyp * MonoTyp
    | RecordTyp of RecordDefinition
    | LiteralTyp of Literal
    | UnionTyp of Set<MonoTyp>

and RecordDefinition =
    { fields: Set<FieldDefinition>
      positionals: MonoTyp list }
```

The `fields` row already does structural ("works on any record with at
least field X") row polymorphism. `&` is already the intersection-like
record combinator at parse time (see
[TypeSyntaxWithSets.md §6](TypeSyntaxWithSets.md)). The test suite
already passes for row-polymorphic access on records of different
shapes (when generalisation permits).

**What's missing** to land the unification:

1. Value-level records move from `{ … }` to `( … )` (ADR-004).
2. The planned `RecordItem = Property | Positional` AST refactor
   (ADR-002, currently unimplemented).
3. Call sites evolve from curried application `f(x)(y)` to record-shaped
   calls `f(x: …, y: …)` (ADR-005, exploratory lean).
4. Type-level syntax migrates to match (`( … )` at both levels).

## 3. Why this is the right moment

Three independent bodies of evidence point at the same answer.

### 3.1 Research anchor — Sun & Oliveira, ESOP 2025

[Named Arguments as Intersections, Optional Arguments as
Unions](https://i.cs.hku.hk/~bruno/papers/esop25named.pdf)
(ESOP 2025, Hamilton). The core claim:

- **Named arguments** correspond to **intersection types** — the
  function demands `x: A ∧ y: B`.
- **Optional arguments** correspond to **union types** — a slot is
  either `A ∨ absent`.

The paper demonstrates type-safety bugs in popular Python and Ruby
type-checkers that arise precisely because named arguments are
retrofitted as an ad-hoc feature instead of a first-class
type-algebraic construct. The fix they formalise is to treat named
arguments as intersections all the way down.

This is the type algebra TypeFighter already has: `&` at the record
level, `|` at the union level. The ESOP paper tells us the algebra is
*exactly right* for named arguments, too. We don't need a new feature —
we need to stop treating calls as a second-class construct.

### 3.2 Historical precedent — Smalltalk

Smalltalk has no positional arguments. Every method argument is a
keyword, written at the call site alongside its value:

```smalltalk
dict at: key put: value
```

The selector is `at:put:` (yes, including the colons), the arguments are
`key` and `value`. Smalltalk literature consistently cites this as one
of the most readable syntactic decisions in mainstream language design:

> "Each keyword communicates the role of the argument, whereas in
> positional argument list versions like Java/C++, it's easier for
> programmers to mix up parameters."

> "Easy to understand, easy to implement, easy to use … a form of
> verbosity that lends itself well to self-explanatory code."

The thing Smalltalk doesn't have, and TypeFighter *does* have, is a
structural type system with row polymorphism underneath. We can take
the Smalltalk call style and wire it straight into the existing
inference machinery without Smalltalk's dynamism cost.

### 3.3 Theoretical lineage — Wand, Rémy, Leijen

Row-polymorphic records came from
[Mitchell Wand (1989)](https://en.wikipedia.org/wiki/Row_polymorphism)
and were formalised for ML by
[Didier Rémy (1992, "Type Inference for Records in a Natural Extension
of ML")](https://www.cs.cmu.edu/~aldrich/courses/819/row.pdf). Daan
Leijen generalised to scoped labels in
[Extensible Records with Scoped Labels
(2005)](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf),
which is the theory Koka and PureScript use.

TypeFighter's current record system descends directly from this line.
"Records as call-site argument bundles" is not a new idea — it's the
natural application of row polymorphism to function arguments.

## 4. The community debate

### 4.1 The anti-tuple camp

Pedro Rijo, [*"Why you should think twice before using
Tuples"*](https://pedrorijo.com/blog/tuples-are-evil/): "a trap you
should avoid at all cost." Main arguments:

- Poor readability — no names, no meaning without context.
- Evolution breaks code — adding one field changes the type
  everywhere.
- Tuples as return values hide intent at call sites.

### 4.2 The pro-tuple camp

Gilad Bracha (Newspeak designer),
[*"Room 101: Tuples"*](https://gbracha.blogspot.com/2007/02/tuples.html):
"tuples are great. Every language should have them." Main arguments:

- Multi-value returns without a class-defining ceremony.
- Varargs alternatives; cleaner than C-style `...`.
- Short local packaging of loosely related values.

Bracha is the sharpest voice here because he also designed Smalltalk's
successor Newspeak — so he's holding *both* positions simultaneously
(no positional at call sites, but tuples as a general packaging tool).
That's close to where ADR-004 + ADR-005 land: records for calls, tuples
for local packaging, same bracket shape.

### 4.3 The pragmatic middle — Odersky / Scala 3 SIP-58

[Scala 3 SIP-58 Named Tuples](https://docs.scala-lang.org/sips/58.html),
landed in Scala 3.7 (2024): `(name: String, age: Int)` is a tuple with
labels. The most-quoted restriction:

> "It is illegal to mix named and unnamed elements in a tuple."

And Odersky's reasoning, from the
[pre-SIP discussion](https://contributors.scala-lang.org/t/pre-sip-named-tuples/6403):
allowing `named <: unnamed` subtyping would cause operations like
concatenation to *silently strip names* instead of error on
duplicates. That's a brittleness argument, not a purity argument.

### 4.4 The "record system is hard" voice — Peyton Jones

Simon Peyton Jones, in a
[Haskell Exchange 2022 talk](https://simon.peytonjones.org/assets/pdfs/haskell-exchange-22.pdf):

> "Record systems are a place where there's a lot of variation and
> it's hard to know which is best. So again, we chose something
> simple."

Even 35 years in, Haskell folks cite "the syntactic problems of lists
and tuples" as live friction. This is a caution, not a veto: "the
space is hard, pick carefully." The TypeFighter direction is a
carefully-picked convergence of Smalltalk, Rémy, and recent ESOP
theory — not an improvisation.

## 5. What the decision looks like on the page

Before:

```
let p = { name: "Ada", age: 42 }
let r = concat("Hello, ")(p.name)
```

After ADR-004 (round brackets):

```
let p = (name: "Ada", age: 42)
let r = concat("Hello, ")(p.name)
```

After ADR-005 (named-only calls, still exploratory):

```
let p = (name: "Ada", age: 42)
let r = concat(a: "Hello, ", b: p.name)
```

Tuples still work as local positional packaging:

```
let pair = (1, 2)               // positional record, synthetic labels
let (x, y) = pair               // destructuring
```

Function definition mirrors the call shape:

```
let greet = (name: String) => concat(a: "Hello, ", b: name)
greet(name: "Ada")
```

The callee's parameter list *is* a record pattern. Type inference
drives everything from one algebra — the same `&` that builds records
at the type level.

## 6. Open questions

- **Empty vs 1-element.** `()` wants to be unit. `(x)` has to be
  grouping, not a 1-element record. 1-element records need a
  distinguishing rule: trailing comma `(x,)` is the Python / Rust
  precedent and probably the call to make.
- **Multi-arg call under ADR-003.** If calls become records under
  ADR-005, and the multi-arg surface from ADR-003 is adopted, then
  `f(a, b)` *is* a record-argument call with two positional slots.
  Consistent. But the placeholder `_` for partial application
  (ADR-003) needs to cohabit with named-only calls — figure-ably, but
  open.
- **Record concatenation / merge.** With named-only calls, splatting
  becomes prominent: `greet({ ...defaults, name: "Ada" })`. Requires
  committing to either Leijen-style scoped labels (duplicates allowed,
  shadowing) or a hard-error-on-duplicate rule.
- **Pattern syntax symmetry.** If `(name: String) => …` is a function
  parameter, then `match x with | (name: "Ada") → …` is the matching
  pattern. Clean, but worth writing out before committing.
- **Tooltip rendering.** A call `f(a: 1, b: 2)` has inferred function
  type what exactly? `(a: Number & b: Number) → Number`? Or
  `{ a: Number, b: Number } → Number`? Prefer the former (matches the
  ESOP framing), but the printer needs to decide and be consistent.

## 7. What to build next

The decisions in ADR-004 and ADR-005 don't need to land together.
Ordering:

1. **First:** the AST-level refactor from
   [RecordsAsHeterogeneousSets.md §5](RecordsAsHeterogeneousSets.md)
   (ADR-002). Pure no-op at the type level. Unblocks everything else.
2. **Second:** surface-syntax swap `{ … }` → `( … )` (ADR-004).
   Parser + pretty-printer change; the type system doesn't notice.
   Update the playground examples, regenerate the smoke-test JSON.
3. **Third:** migrate type-level syntax to match (ADR-004 + docs in
   [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md)).
4. **Fourth** (optional, exploratory): go Smalltalk-style on calls
   (ADR-005). Can be a flag behind a feature gate first; evaluate on
   real code before committing.

Pattern matching (currently a TODO per
[TypeSyntaxWithSets.md §5](TypeSyntaxWithSets.md)) is orthogonal to
this work but sequences naturally after step 3.

## 8. Relation to other decisions

| Concept doc                                                    | Gets affected by this note                                      |
| -------------------------------------------------------------- | --------------------------------------------------------------- |
| [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) | Still valid; step 1 (AST refactor) is the next concrete task.   |
| [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md)                 | Type-level syntax needs to migrate to `( … )` to match values.  |
| [RecordsTuplesUnified.md](RecordsTuplesUnified.md)             | Bracket swap committed; "type-keyed" kind still parked (ADR-007).|
| [DoWeNeedCurrying.md](DoWeNeedCurrying.md)                     | Interacts directly with ADR-005 — decide together on real code. |
| [SyntaxForAIAndBeginners.md](SyntaxForAIAndBeginners.md)       | Named-only calls pull hard toward the criteria this doc values. |
