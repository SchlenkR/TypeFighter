# Record Brackets Reconsidered — `{ }` for Records, `( )` for Grouping

> **Status:** concept doc capturing the reconsideration of ADR-004.
> Landing decision lives in [DesignDecisions.md](DesignDecisions.md)
> as ADR-009 (which supersedes ADR-004).

## 1. Why we're looking again

ADR-004 put value-level records behind `( … )`, mirroring Scala 3's
named tuples. Slices A and B landed it. Ronald's pushback — *parens
should just be mathematical grouping; a separate bracket shape should
carry the "is a record" meaning* — exposed a tension that wasn't
visible at the decision point: the `(…)` bracket now silently does
two different things, and the parser has to sniff the contents to
decide which.

That's Option 1 from our last discussion. Option 2 (F#-style `,`
builds records, parens are pure grouping) fixes the conceptual
cleanliness but creates concrete collisions:

- `[1, 2, 3]` becomes ambiguous (a 1-element array containing a
  3-tuple?) — F# solves with `;` but that's a breaking change to
  everything that uses arrays today.
- `f(x, y)` can only be "apply to tuple"; curried multi-arg needs
  space application, which means introducing a new parsing rule.
- `(x, y) => body` flips from curried-desugar to record-destructure,
  entangling with ADR-005 before it's ready.

So *neither* Option 1 nor Option 2 is a comfortable long-term
resting place. We want a third option.

## 2. The research — three families across the field

### 2.1 ML-classic (Haskell, OCaml, Elm, PureScript)

Space application `f x y`, curried by default. `(x, y)` is the
tuple literal; parens are technically grouping but appear around
every tuple literal. Records either use `{…}` (Elm, PureScript) or
don't exist as a separate construct (Haskell — records are algebraic
data type constructors).

**Partial application is free.** Every function that takes N args
produces an N-1-arg function when given one arg.

**Named / optional arguments:** only OCaml, via `~arg` and `?arg`
labels with erasure semantics.

### 2.2 F#-mixed

Both space and paren-comma application work, with different
semantics. `f x y` is curried. `f(x, y)` applies `f` to the tuple
`(x, y)`. Comma itself is the tuple constructor at expression level:
`1, 2` without parens is still a tuple.

Records use `{ A = 1; B = 2 }`. Curly braces for records is a strict
F# convention.

**Partial application** is free for curried forms, not for tupled.

**Named / optional arguments** exist at method boundaries via `?arg`
syntax.

### 2.3 Mainstream (Roc, Rust, Swift, Kotlin, Scala 3, TypeScript)

Paren-comma calls `f(x, y)` as the primary form. Parens are *only*
for call args or grouping; commas separate args. Records use `{…}`
(Rust struct literals, TypeScript objects), or a type constructor
call (Roc's record syntax).

**Roc is notable** because it explicitly chose paren-comma despite
its ML heritage. Richard Feldman's stated reason: "visual familiarity
for language adoption" — so TS/JS/Python users can read Roc on first
sight.

**Partial application** is explicit. Scala `_` placeholder
(`add(5, _)`), Kotlin `it`, Rust closures. No auto-partial by
under-arity.

**Named / optional arguments** are idiomatic across this family.
Kotlin `fn(x: Int, y: Int = 0)`, Swift labeled args, Python
`fn(x=1, y=2)`.

## 3. The properties TypeFighter wants

Pulling together everything we've flagged in prior docs:

1. **Record and call surface rhyme** — calls with named args
   (`f(name: "Ada")`) feel like records without ceremony, per the
   ESOP 2025 framing [Named Arguments as Intersections, Optional
   Arguments as Unions].
2. **Automatic partial application** when a caller under-provides
   named args, without the arity-invisibility problem the original
   currying design had.
3. **Optional arguments** with a clean type-theoretic story —
   Sun & Oliveira's `Optional = Union with absent` lines up with
   our existing `|` algebra.
4. **Beginner and LLM friendliness** — [SyntaxForAIAndBeginners.md
   priorities]. Familiar call syntax, forgiving errors, readable at
   a glance.
5. **No breaking changes to arrays or other working idioms** unless
   we absolutely have to.

None of the three families above delivers all five. Each compromises
on something:

- ML-classic gets (2) for free but struggles with (3), (4).
- F#-mixed is cleanest conceptually but demands `;` for arrays,
  which kills (5).
- Mainstream is friendliest but gives up (2) — no auto-partial.

## 4. The synthesis

What if we keep `{…}` for records (Mainstream-style, familiar), but
retain the ESOP-style *semantic equivalence* between records and
call-site argument bundles? The surface reads like Kotlin/Swift;
the semantics stays at the ML/ESOP level underneath.

**Surface rules:**

| Form | Meaning |
|---|---|
| `{}` | empty record |
| `{ x: 1 }` | 1-named record |
| `{ x: 1, y: 2 }` | 2-named record |
| `{ 1, 2, 3 }` | 3-positional record (subsumes tuples) |
| `{ "Circle", radius: 3 }` | mixed (positional tag + named field) |
| `( x )` | grouping — pure, `x` unchanged |
| `()` | the unit value — the empty record *as a value* |
| `[ 1, 2, 3 ]` | array |
| `f(x)` | call with one positional arg |
| `f(x, y)` | call with two positional args (curried fold, unchanged) |
| `f(name: "Ada")` | call with named arg — sugar for `f({ name: "Ada" })` |
| `f(x, name: "Ada")` | call with mixed args — sugar for `f({ x, name: "Ada" })` |

**Implications:**

- Parens are back to pure mathematical grouping at the primary level.
  `(x)` is `x`. No content sniffing.
- Records have their own bracket shape `{…}`, optically distinct from
  both arrays and calls.
- Tuples are not a separate construct. A positional record `{ 1, 2 }`
  plays the tuple role.
- Call-site with named args *sugars* to "apply function to a record
  constructed from the arguments." One consistent story.
- Optional args land naturally later: the callee's parameter record
  has slots that are `T | absent`, calls omit them.
- Arrays keep `[ 1, 2, 3 ]` with comma separators. No breaking change.
- `()` as unit makes sense at both value and type level — the
  degenerate empty record.

**What survives from ADR-004's surface/semantic thinking:**

- The call-site auto-record-wrapping when named args appear.
- Calls-are-records as the conceptual frame.
- Tuples don't need a separate AST constructor; positional records
  cover them.

**What ADR-004 got wrong in hindsight:**

- Collapsing records and grouping onto `(…)` made the parser
  sniff-and-decide, which is cognitively expensive for readers *and*
  parsers. Two meanings, one shape.
- The trailing-comma rule for 1-element records (`(x,)` as record vs
  `(x)` as grouping) is a micro-tax paid per 1-element record.
- The type-level `(…)` collided awkwardly with type grouping too
  (Slice B's trailing-`&` disambiguation was the same tax).

## 5. What this changes concretely

- **Value-level record literal:** `{ x: 1, y: 2 }` (was `( x: 1, y: 2 )`).
- **Value-level grouping:** `( x )` returns `x` (unchanged — ADR-004
  already did this for 1-positional).
- **Type-level record:** `{ x: Number & y: Number }` (was `( x: Number & y: Number )`).
- **Type-level grouping:** `( T )` returns `T` (unchanged).
- **ShowTyp / ShowExpr printers:** records back to `{ … }`.
- **Call-site syntax:** unchanged — `f(x: 1)` still works, `f(x, y)`
  still curried, `f({x: 1})` still valid (explicit record arg).
- **Arrays:** unchanged — `[ 1, 2, 3 ]`.

## 6. What this does NOT change

- The AST (`MkRecord`, `RecordTyp`, `RecordDefinition`). The refactor
  from [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md)
  step 1 stays; only the surface brackets move.
- The prelude — `concat`, `log`, `ToString`, `UnitValue` are unchanged.
- Currying semantics in the core — curried-by-default still holds;
  multi-arg calls still fold to `App`-chain. ADR-003 (multi-arg
  surface with `_` placeholders) and ADR-005 (named-only calls,
  auto-partial) are both still open decisions layered *on top of*
  this syntactic shape.

## 7. Tradeoffs we're accepting

- **We reverse ADR-004.** Slice A and Slice B landed changes that now
  un-land. That's the cost of having iterated publicly — the docs
  show the reasoning so future readers don't have to rerun the
  experiment.
- **Two bracket shapes stay in the language** (`{ }` and `( )`), each
  with a single job — not polymorphic. This is the Mainstream
  tradition and the strongest beginner/LLM signal.
- **Tuples via positional records** means `{ 1, 2 }` carries the
  tuple role. The word "tuple" becomes a usage pattern, not a
  distinct construct — aligning with Roc's "tuples are records at
  runtime" framing while going one step further semantically.

## 8. Relation to other notes

| Doc | Status after this note |
|---|---|
| [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) | Still valid. AST direction unchanged. |
| [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md) | Back in force — `{ … }` at type level as originally proposed. |
| [RecordsTuplesUnified.md](RecordsTuplesUnified.md) | Superseded in part. Round-bracket exploration ends; type-keyed item kind remains parked (ADR-007). |
| [CallsAreRecords.md](CallsAreRecords.md) | The "calls are records" semantic stays; the surface sugar `f(name: x)` stays; only the record literal bracket moves back to `{ }`. |
| [DoWeNeedCurrying.md](DoWeNeedCurrying.md) | Unchanged. Orthogonal. |

## 9. The open question this leaves

`()` vs `{}` for the empty/unit value:

- `{}` is the canonical empty record. No ambiguity.
- `()` is traditional unit. But under pure-grouping rules, `()` is
  "grouping of nothing" — a parse error, or a special-case value.

Leading choice: make `()` an alias for `{}` *at the value level*,
and `()` at the type level similarly for the empty record type. Both
represent "no payload," and we get the math-friendly writing `f()`
for nullary calls without awkward `f({})`.

This is the one small retreat from "parens are ONLY grouping." The
empty case is special-cased. Documented, one rule, done.
