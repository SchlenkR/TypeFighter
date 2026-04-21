# Records as Heterogeneous Sets — Analysis

> *"Ein Record ist nicht wirklich ein Record im Sinne von etwas Benanntem
> — es ist eine Menge von Dingen, manche davon sind Properties (Name+Wert),
> manche sind einfach Rohwerte."*

## 1. Where we are today

TypeFighter currently models a record as a list of named fields:

```fsharp
type Field<'tv>    = { fname: string; value: Expr<'tv> }
type Expr.MkRecord = {| fields: Field<'tv> list; tvar: 'tv |}

{ age: 22, name: "John" }
  ≡ MkRecord [ Field "age"  (Lit 22)
               Field "name" (Lit "John") ]
```

Type side: a row of `{label -> type}` pairs, closed or open. Row
polymorphism lets us express *"any record that has a `.name` field of type
`string`."*

This is the standard ML / Elm / PureScript model: records are **maps**
from labels to values.

## 2. The proposed model

A record becomes a **heterogeneous set** whose items can be:

- **Property** — the today-style named entry, but now a *first-class
  value*: `{ name: string; value: some T }`.
- **Positional value** — any expression appearing in the record *without*
  a leading `name:`. Lists, strings, ints, other records, anything.

Concrete example — a "Mindset":

```
mindset = {
    [1, 2, 3],           // a list, positional
    "foo",               // a string, positional
    "bar",               // a string, positional
    name:  "Ronald",     // Property
    age:   45,           // Property
    level: "expert",     // Property
}
// = one list + two strings + three properties, in one value
```

Key design moves:

1. `Property` becomes **a first-class runtime type**: two-slot value
   carrying a name and a wrapped inner value. This means `name: "Ronald"`
   *has* a type of its own — you can pass a property around, not just use
   it as a field in a record literal.
2. A record is a **collection** of items, some of which happen to be
   Properties and some of which don't.
3. The old record `{ age: 22, name: "John" }` is exactly the subset of
   this model where *every* item is a Property.

So the model is a **strict generalisation** of today's. All existing
records remain valid in the new model with no change in meaning.

## 3. Precedents

This shape is not a new idea. Every language that ships a mixed
keyed/positional container has met the same design space:

| Language        | Shape                                                       |
| --------------- | ----------------------------------------------------------- |
| **Lua tables**  | one container; keys are either strings or integers (auto-indexed when bare). `t = {1, 2, name = "x"}` is idiomatic. |
| **PHP arrays**  | same idea as Lua, less consistent semantics.                |
| **JSX / HTML**  | attributes (named) + children (positional). The React tree is exactly the Mindset shape. |
| **Clojure**     | keeps them separate (maps vs. vectors). Explicit merge.     |
| **Racket / Lisp** | s-expressions are positional; a-lists add names optionally. |
| **Python kwargs** | function arguments are positional + keyword — the same split, limited to call sites. |
| **Ruby hashes-with-positionals** | method calls allow trailing hash: `f(1, 2, name: "x")`. |

The closest match is **JSX/HTML** (attributes + children) and **Lua
tables**. Both are widely used, so the shape isn't exotic — but the
research around *how to type it well* is thin compared to records.

## 4. Inference — the hard part

Today's row-polymorphic record type gives us:

```
TRecord(row)   where row = { label_1 : τ_1, …, label_n : τ_n | ρ }
```

`ρ` is an open row variable — what makes row polymorphism work.

The generalised record needs to track **two** shapes at the type level:

```
TRecord(namedRow, positionalPart)
```

- `namedRow` — as today: `{ label_i : τ_i | ρ }`. Handles the Property
  items. Row polymorphism continues to work.
- `positionalPart` — new. Choices:

### Option X — positionals as a homogeneous list

```
positionalPart ≡ TList(elemType)
```

All positional items unify to a single element type. `{ 1, "foo" }`
becomes `TList(int | string)` which we can't express without a union
type. So in practice this forces the user to keep positionals
homogeneous, or we widen to `TList(any)` and lose the static types.

**Verdict:** gives up too much.

### Option Y — positionals as a heterogeneous tuple

```
positionalPart ≡ TTuple(τ_1, …, τ_k)
```

Each positional slot carries its own type. `{ 1, "foo", [1,2,3] }`
becomes `TTuple(int, string, List<int>)`.

**Problem:** the type now has *arity*. Adding a positional changes the
type radically. Inference cannot generalise "records with at least these
positionals and some more" without a positional row — which brings us
to Option Z.

### Option Z — positionals as a row of integer-keyed entries

```
positionalPart ≡ { 0 : τ_0, 1 : τ_1, …, k-1 : τ_(k-1) | ρ_pos }
```

Treat positionals as a second row, keyed by auto-assigned integers.
Row polymorphism works identically here: `"a record with at least one
string at position 0."` Now we have symmetry: **two rows**, one indexed
by string labels, one by integers.

**Tradeoff:** adding or reordering positionals changes their integer
keys, so type changes under refactor. Can be mitigated by requiring
*structural*, not positional, matching (i.e., "this record contains a
string and a list" rather than "this record has a string at position 0").
But then we've given up ordering, and it's a bag, not a tuple.

### Option W — drop the positional/named distinction, unify under one row

Go all the way: positional entries get synthetic string keys ("0", "1",
…) and live in the same row as named ones. Lua's model. One type per
record, with mixed-sort keys.

**Problem:** user-defined labels `"0"` collide with positional indexing.
Requires reserving numeric-looking labels, or a key-kind distinction.

**The cleanest outcome** for TypeFighter, given we already have row
polymorphism for records:

- Use **Option Z (two rows)** with *bag semantics* for positionals (not
  tuple). The type says "this record contains a string, another string,
  and a list somewhere, plus these named fields." Duplicates allowed.
- Order-preserving at the *value* level (for pretty-printing and
  iteration) but order-insensitive at the *type* level (for inference).

This maps cleanly onto our existing row-poly machinery — it's "row
polymorphism, twice."

## 5. AST changes — minimum viable

Today:

```fsharp
type Expr<'tv> =
    …
    | MkRecord of {| fields: Field<'tv> list; tvar: 'tv |}

type Field<'tv> = { fname: string; value: Expr<'tv> }
```

Proposed:

```fsharp
type RecordItem<'tv> =
    | Property   of name: string * value: Expr<'tv>
    | Positional of value: Expr<'tv>

type Expr<'tv> =
    …
    | MkRecord of {| items: RecordItem<'tv> list; tvar: 'tv |}
```

Migration of existing code: `X.MkRecord [ X.Field "a" e ]` becomes
`X.MkRecord [ RecordItem.Property ("a", e) ]`. One find-and-replace
across `ExpressionDsl.fs`, `Syntax.fs`, and the test suite.

Parser change (in [src/TypeFighter.Parser01/Syntax.fs](../../src/TypeFighter.Parser01/Syntax.fs)):

```fsharp
let private recordItem =
    let asProperty =
        parse {
            let! name  = identifier
            let! _     = sym ":"
            let! value = expr
            return
                { range = Range.add name.range value.range
                  result = RecordItem.Property (name.result, value.result) }
        }
    let asPositional =
        expr |> map RecordItem.Positional
    asProperty <|> asPositional
```

Plus updating `recordLit` to collect `recordItem` instead of `field`.

(Backtracking question: `asProperty` tries `identifier : …` first. If it
fails after consuming the identifier, we backtrack and retry `expr`.
TheBlunt's `<|>` does exactly that, but we'd want to verify the failure
actually resets the cursor. Covered by the backtracking TODO block in
[Syntax.fs](../../src/TypeFighter.Parser01/Syntax.fs).)

## 6. New language features this unlocks

### First-class properties

Today `name: "Ronald"` is a *syntactic piece* of a record literal — it
has no type of its own, no way to be constructed or passed around. With
the generalisation, we get:

```
let greeting = name: "Ronald"       // value of type Property<string>
let record   = { greeting, age: 45 } // splat a property into a record
```

This is small but *compositional*. Building records becomes a matter of
collecting properties, a data-style approach that LLMs and beginners
take to very naturally.

### Pattern syntax

A record pattern stays recognisable:

```
match mindset with
| { name, age } -> …                       // grab named slots
| { name, *positionals } -> …              // named slot + bag-rest
| { *all } -> …                            // unpack everything
```

### Splat / concat

```
{ ...base, name: "override", extra: 1 }    // merge records
```

Already natural in Lua, JS (`{...base, name: "x"}`), Python
(`{**base, "name": "x"}`). Syntactically low-cost, fits the set
model.

## 7. Challenges and open questions

- **Duplicate property names.** Today forbidden implicitly. In the new
  model still forbidden for named items; free for positionals (`{"a",
  "a"}` is a valid bag).
- **Ordering semantics.** Do positionals preserve order at runtime?
  (Yes, for sane pretty-printing and iteration.) Does the *type* preserve
  order? (Probably no — bag semantics at the type level.)
- **Mixed access.** `rec.name` for properties; how do we access
  positionals? Options:
  - `rec[0]` — indexed, but clashes with property named `"0"` if we ever
    allow numeric labels.
  - `items(rec)` — a built-in returning the positionals.
  - `for x in rec` — iteration visits all positionals (or all items?).
- **Definition sites.** Does a user *define* a "Mindset" type, or does
  "Mindset" exist only as a *pattern* on a generic record-set? If
  nominal, we need a type-declaration form. If structural, the
  pattern-match is the contract.
- **Interaction with the Property type itself.** A Property *is* a value;
  a record-set contains Properties. Can a record-set contain a Property
  *as a positional*, distinguishable from a named slot? E.g.:
  - `{ name: "foo" }` — one named slot `name=foo`, no positionals.
  - `{ Property("name", "foo") }` — one positional whose value happens to
    be a Property.

  The two must have *different* types, or the model collapses.

- **Parser ambiguity for bare identifiers.** `{ foo }` could mean:
  - shorthand for `{ foo: foo }` (JS-style property shorthand),
  - a positional reference to the variable `foo`.

  Today's parser doesn't support `{ foo }`. The new model forces us to
  pick; the JS-style shorthand is the more useful of the two, and we can
  disambiguate by requiring shorthand to match a local name (which is a
  type-check-time decision, not a parse decision).

## 8. Could we "just convert"?

Yes — the change is mechanical at three layers, and each layer is
well-contained:

1. **AST** — replace `Field` with `RecordItem` (union of `Property` and
   `Positional`). Update `ExpressionDsl` helpers.
2. **Parser** — add a `recordItem` alternative (`asProperty <|>
   asPositional`) inside `{ … }`.
3. **Inference** — **the real work**. The record type grows a second
   row for positionals (Option Z above). Unification handles both rows
   in parallel. Row polymorphism applies to both.

Layers 1 and 2 are a half-day. Layer 3 is the design choice we're
actually making — Option X / Y / Z / W each ripple through the solver
differently.

## 9. Recommendation

1. **Do the AST + parser change first, as a no-op refactor.** Every
   existing test still produces a `RecordItem.Property(…)` list, the
   solver sees no change, all 37 parser tests continue to pass. This is a
   safe, non-disruptive step — it just opens the door.
2. **Only then** decide which of Option X / Y / Z / W to commit to on
   the inference side. I'd advocate **Option Z (two rows, bag-semantics
   on positionals)** because it reuses the row machinery we already
   have — "row polymorphism, twice" is the smallest generalisation that
   preserves what works.
3. **Only then** introduce `Property` as a first-class type — the case
   for it is much stronger once record-sets exist, and shallow (it's a
   two-field record in disguise) but it unlocks the splat/compose
   ergonomics that are the whole point of the redesign.

## 10. What this does *not* answer

- `Property` as a declared, nominal type vs. a structural convention.
- Whether a "Mindset" (or any user-defined shape) is a *type* we can
  declare once, or a *pattern* we match against.
- Indexed access syntax for positionals.
- Set vs. ordered-list semantics for the positional row — we parked this
  at "bag at the type level, list at runtime"; that may or may not age
  well.

Pick these off one at a time in the next design passes.
