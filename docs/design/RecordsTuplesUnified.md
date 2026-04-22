# One Container to Rule Them All — Records, Tuples, Sets Unified

> **Status:** concept note. Extends
> [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) and
> [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md) with Ronald's further
> generalisation: if records already *are* heterogeneous sets, do we
> still need a separate tuple form at all?

## 1. The proposal

Today's plan says `{ … }` constructs a record that may carry both named
and positional items. Ronald's extension pushes this one step further:

1. **Replace `{ … }` with `( … )`.** One bracket shape for everything
   — what a record is today, what a tuple is in ML/Haskell, what an
   anonymous struct is in Rust — collapses into a single construct.
2. **Recognise four kinds of item**, not two:
   - **Named**: `name: "Ada"` — today's property.
   - **Positional**: `"Ada"` — today's bare value in the het-record;
     position-and-order matters.
   - **Unnamed non-positional**: appears once in the container, accessed
     *by type* rather than by name or index. "This bag contains one
     `String`."
   - **Named-positional**: maybe — a slot that has both an index *and*
     a label (cf. Swift, labeled tuple elements). Open question.
3. **No separate tuple type.** `(1, 2)` is the same construct family as
   `(name: "Ada", age: 30)` — just with different item kinds inside.

The name for the fourth kind matters. "Unnamed non-positional" is
clumsy. Candidates: **typed slot**, **signature slot**, **type-keyed
item**. The defining property is "identified by its type, not by name
or index." For the rest of this note I'll call it **type-keyed**.

## 2. What the landscape looks like

Five broad strategies show up across modern languages. None of them
does *exactly* what Ronald is proposing — every language tilts
somewhere. The closest precedents in bold.

### 2.1 Named and unnamed elements coexist in one form

- **Scala 3 named tuples (2024 — most relevant precedent).**
  `(name: String, age: Int)` is a tuple whose elements carry names.
  Regular tuples are a *subtype* of named tuples so migration is
  smooth. But the language **forbids mixing** named and unnamed
  elements within the same tuple — it's all-named or all-positional.
  Runtime erases the names entirely; a named tuple *is* a regular
  tuple at runtime.

- **Swift.** Tuple labels can be added or dropped by the subtyping
  rules: `(X, Y) <: (x: X, y: Y)` and `(x: X, y: Y) <: (X, Y)`. So
  labels are essentially cosmetic at the type level — values flow
  between labeled and unlabeled forms.

- **Nim.** Named tuples (`tuple[name: string, age: int]`) and
  anonymous tuples (`(string, int)`) coexist and anonymous tuples are
  implicitly assignable to matching named tuples. **But field names
  are part of type equivalence** — `tuple[a: int]` ≠ `tuple[b: int]`.

- **C# `ValueTuple`.** Named elements (`(Name: "Ada", Age: 30)`)
  exist but are erased to `Item1`/`Item2` at runtime. Labels are
  "there only to help the developer," not to create nominal
  distinction.

- **TypeScript labeled tuple members** (since 4.0). `[lat: number,
  long: number]`. Labels are explicitly "purely there for
  documentation and tooling … no effect on type compatibility." All
  labeled or all unlabeled, no mixing.

### 2.2 Mixing is allowed; structural, lightweight

- **Lua tables.** One container. Bare values auto-index
  (`t = {1, 2, name = "x"}` → keys `1`, `2`, `"name"`). No types.
- **JSX / React.** Attributes (named) + children (positional) in one
  element — exactly the "mixed record-set" shape, but at the XML/AST
  level, not a general language feature.
- **Python kwargs at call sites** — positional args and keyword args
  cohabit in one call frame. Doesn't generalise to data structures.

### 2.3 Tuples are distinct but runtime-equivalent to records

- **Roc.** Tuples and records compile to the same runtime
  representation. Tuples exist "primarily for conciseness when
  positional access is more natural than labeled fields" — the
  distinction is *purely surface*. Worth noting: Roc still keeps them
  as separate surface forms.

### 2.4 Tuples and records are genuinely separate

- **Haskell, OCaml (default), Elm, Gleam, Rescript.** Tuples and
  records are different types with different operations. Gleam's
  guidance: "use custom types [records] unless truly heterogeneous
  return."

### 2.5 Role-based / protocol-based unification

- **Raku.** `Positional` and `Associative` are *roles*. Any class can
  implement one, the other, or both. Sigils (`@`, `%`) select which
  view you want. Full unification is *expressible* but opt-in per
  class.

### 2.6 The theoretical end — set-theoretic types

- **CDuce.** Types are built from union, intersection, and negation.
  Records are maps from labels to types, and pattern matching
  operates on the full set algebra. Literal types, discriminated
  unions, refinement — all derive from one semantic framework.
  TypeFighter's existing direction (literals as types, `|`/`&` at the
  type level) is already CDuce-shaped. Scaling that to "a record may
  contain a String" at the type level (the **type-keyed** idea) is a
  natural extension.

## 3. The four item kinds

| Kind                | Syntax                  | How identified           | Duplicates allowed?  |
| ------------------- | ----------------------- | ------------------------ | -------------------- |
| Named               | `name: value`           | by string name           | no (same name twice is an error) |
| Positional          | `value` (in order)      | by index                 | yes                  |
| Type-keyed          | `value` (unordered)     | by value's type          | no (one per type)    |
| Named-positional    | `name: value` at index  | both name *and* index    | no (name collision)  |

The open question is whether a container really needs *all four*, or
whether three is the sweet spot.

### 3.1 Named vs type-keyed — is there room for both?

Named items say *"this value is called `name`."* Type-keyed items say
*"somewhere in this bag is a `String`, and there's only one."* These
do different work:

- Named is how you build domain records today.
- Type-keyed is how you build `Option<T> = ( "None" ) | ( "Some" & T )`
  without having to invent a tag — the type-keyed item *is* the
  payload, identified by its type. It's also the natural shape for
  effect rows, readers, dependency-injection bags, and "message
  contains exactly one timestamp of type `Instant`."

The type-keyed kind is **the genuinely novel bit**. No mainstream
language offers it as a first-class surface item, because most type
systems can't afford the check ("is there already a `String` in here
at the type level?"). CDuce-style set-theoretic types make it
tractable: type-keyed = "a record with field _ (anonymous) of type T,"
plus a uniqueness constraint on the anonymous-row.

### 3.2 Positional vs type-keyed — pick one?

These two kinds overlap uncomfortably. Both are "values without
labels." The differences:

- Positional has an *order* (index 0, 1, 2).
- Type-keyed has *uniqueness at the type level* (one `String` max).

A container could support both, but the user cost is real — two
similar-looking items with subtly different behaviours. Two options:

1. **Keep both.** Positional = ordered, duplicates OK. Type-keyed =
   unordered, unique-per-type. The parser picks based on whether a
   type already appears in the container.
2. **Drop positional; keep type-keyed.** Pure set-algebra. You can
   still encode tuples as `( first: T1, second: T2, third: T3 )` with
   explicit names. Loses `(1, 2, 3)` terseness.
3. **Drop type-keyed; keep positional.** Stay with today's
   het-record model. Don't introduce set-uniqueness at the type
   level. Simpler, less novel.

My instinct: start with option 3 (keep today's het-record), spike
option 2 on paper, and decide empirically once the parser work is
done. The type-keyed kind is the flashy one but the most costly to
unification.

## 4. Round brackets — the cost

`{ … }` today is unambiguous: it's always a record. Moving to `( … )`
buys unification but creates three collisions:

1. **Grouping.** `(expr)` is how you disambiguate precedence.
2. **Function application.** `f(x)` is a call; `f(x, y)` is a
   two-arg call (especially in a multi-arg surface — see
   [DoWeNeedCurrying.md](DoWeNeedCurrying.md)).
3. **Tuple patterns in match.** Same bracket, different position.

These are all resolvable by parsing context. Swift, Scala, Nim, Rust,
and Python all reuse `(…)` for both grouping *and* tuples without
real confusion, because:

- A 1-element "tuple" with parens ambiguates with grouping; most
  languages require a trailing comma (`(x,)` is a 1-tuple; `(x)` is
  grouping).
- Function call vs tuple-argument is the same construct in many
  languages (a "function takes a tuple").

For TypeFighter specifically, the grouping-vs-record ambiguity
is the most interesting, because:

- `(42)` has to be "the number 42 in parens" (grouping), not "a
  1-element record containing `42`."
- `(name: "Ada")` is unambiguous — the `name:` can only be a record
  item.
- `(42,)` with trailing comma could be the 1-element positional
  record, if we keep positionals.
- `()` is the empty record = unit.

This is tractable but has sharper edges than `{ … }`. Every language
that reuses `(…)` for tuples has to make these calls.

## 5. Interaction with existing TypeFighter design

The direction we've already committed to in
[TypeSyntaxWithSets.md](TypeSyntaxWithSets.md) uses `{ … }` at **both**
value and type level, with `&` as the item combinator and `|` as the
type-level union. Switching value-level to `(…)` has knock-on effects:

- **Type-level syntax.** Should types also move to `(…)`? That would
  keep value/type rhyme. But `(A | B)` then looks a lot like
  "grouping with a union," which is what it currently means. The
  reading "record containing an `A-or-B`" would need `(A | B,)` or
  similar.
- **Discriminated unions.** `Shape = ("Circle" & radius: Number) | …`
  still reads fine. Ditto `Option<T> = ("None") | ("Some" & T)`.
- **Pattern matching.** `match shape with | ("Circle" & radius) → …`
  — parens-pattern for a record destructure works, reads OK.

The alternative: **keep `{ … }` for records *and* embrace it as
tuples.** This is Roc's position — records and tuples are the same
runtime thing, just two surface forms. We'd drop one: `{ … }` does
both. Less novel, less disruptive, loses the "tuples look like
everywhere else" familiarity.

## 6. The specific Scala-3 lesson

Scala 3's named tuples are the single closest precedent to what
Ronald is describing, and they deliberately **don't** allow mixing:

> "It is illegal to mix named and unnamed elements in a tuple."

The reasoning (from the SIP): mixing creates a fan of edge cases for
type checking, pattern matching, and field access. *What does
`.name`* mean *on a `(1, name: "Ada", 2)`?* Is it the labeled element,
or is it reserved for records-proper?

Scala's answer is to ban the mix and keep named tuples *almost*
indistinguishable from records — their niche is precisely "return
multiple things from a function with readable names." This is a
conservative unification.

TypeFighter's existing
[RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md)
explicitly *does* allow mixing, and explains how (two rows in the
type: named row + positional row). That design choice predates
Scala 3's, and is the more ambitious one. **The Scala lesson isn't
"you must ban mixing"; it's "when you allow mixing, you owe the user
a very clear access story."**

## 7. Tradeoffs summary

| Choice                                           | Wins                                                 | Loses                                                     |
| ------------------------------------------------ | ---------------------------------------------------- | --------------------------------------------------------- |
| `( … )` replaces `{ … }`                         | Unifies tuples+records syntactically; one bracket; matches Scala/Nim/Swift familiarity | Parser edges around grouping vs 1-element record; loses rhyme with value-level `{}` of today |
| Keep both `{ … }` and `( … )`                    | Low-disruption migration; tuples feel tuple-y        | Two syntaxes for one concept; users have to choose        |
| Add **type-keyed** kind                          | Novel, composes with set-theoretic types; powerful for DI / effect rows / tags | Unification cost ("is there already a `String`?"); hard to surface in error messages |
| Drop **positional** kind                         | Cleaner algebra (set + map only); no bag-semantics   | Loses `(1, 2, 3)` terseness; force users to name everything |
| Ban mixing (Scala 3 way)                         | Simpler type checking; one clear access story        | Loses the "one container fits all" pitch                  |
| Allow mixing (existing plan)                     | Most expressive; collapses use cases                 | Access story is harder; `.foo` vs `[0]` vs `$Type` all live together |

## 8. Tentative lean

There are two levers Ronald is pulling simultaneously. They're
independent — we can pull either, both, or neither.

1. **Syntactic unification (`( … )` instead of `{ … }`).** My lean:
   **wait**. The current `{ … }` plan already unifies records and
   tuples at the *AST* level. Changing the bracket is cosmetic and
   costs more than it buys right now. Revisit once the AST-level
   unification ships and we have real code to evaluate "how often do
   users actually want tuple-like terseness?"

2. **The type-keyed item kind.** My lean: **park as a research
   branch**. It's the most interesting idea in the note —
   set-theoretic types plus type-keyed items give us effect rows,
   typed-injection bags, and discriminated-union payloads "for
   free" — but it's also the one with the least precedent, the
   highest solver complexity, and the easiest-to-explain failure
   mode ("you have two `String`s in this bag, which did you mean?").
   Build the het-record (three kinds: named, positional, literal-tag)
   first. If patterns emerge that beg for "there's exactly one `X`
   in this bag," the type-keyed kind is a natural next layer.

So: **don't swap the brackets yet, and don't add type-keyed items
yet.** Ship what's already designed
([RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) + the
type syntax in [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md)), use
it for the playground examples, let the shape of real programs tell
us whether the further unification pays for itself.

## 9. Open questions

- **Access syntax for type-keyed items.** If `bag = (42, "hello",
  name: "Ada")` and the `42` / `"hello"` are type-keyed, how do you
  get them out? `bag.Number`? `bag[String]`? `bag.get<Number>`?
  None of these reads obviously to a newcomer.
- **Pattern matching for type-keyed.** `match bag with | (n:
  Number, s: String, name) → …` — is `n: Number` here a named item
  `n` constrained to `Number`, or a *destructure of the type-keyed
  `Number`*? The ambiguity is real.
- **Trailing-comma rule.** If `(expr)` is grouping and `(x,)` is a
  1-element record, we've committed to a Python-flavoured edge case
  that users reliably stumble over. Worth it?
- **Empty record.** `()` is unit today (or absent). Would the empty
  record become the new `unit`? Unification across the whole stdlib.
- **Type-level syntax symmetry.** Do we move types to `(…)` too, or
  keep `{ … }` at the type level and `(…)` at the value level?
  Breaking the rhyme is a real cost.

## 10. Relation to other notes

- [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) —
  the existing plan: named + positional items in `{ … }`. This note
  pushes that further.
- [TypeSyntaxWithSets.md](TypeSyntaxWithSets.md) — type-level
  counterpart. If we swap brackets at the value level, this design
  needs revisiting.
- [DoWeNeedCurrying.md](DoWeNeedCurrying.md) — if we go multi-arg at
  the surface, `f(a, b)` and "a 2-element record" become visually
  identical. That's a genuine syntax collision worth thinking about
  before committing to `( … )` at the value level.
- [WrapperUnpackProtocol.md](WrapperUnpackProtocol.md) — orthogonal,
  but the "protocol a type opts into" idea could apply here too: a
  type declares "I want to be a type-keyed item" at definition time.
