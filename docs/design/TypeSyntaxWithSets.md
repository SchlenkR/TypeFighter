# Type Syntax with Set Operators

> **Status:** design exploration. Follow-up to [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md)
> and the `UnionTyp`-vs-record-set discussion.

## 1. Premise

If a record is a **heterogeneous set** at the value level
([RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md)), then the
natural type-level counterpart is a **type-set** with the same shape. We
already have the ingredients:

- `UnionTyp of Set<MonoTyp>` — a "bag of type alternatives" ([Lang.fs:40](../../src/TypeFighter/Lang.fs#L40)).
- `RecordTyp of RecordDefinition` — a row of named fields.
- `LiteralTyp of Literal` — literal-as-type for `Number`, `String`, `Boolean`.
- `BuiltinTypes.boolean = UnionTyp { true, false }` — unions already used for `bool`.

So the *machinery* is there. What's missing is a **uniform surface syntax**
that exposes it and mirrors the value-level record syntax.

## 2. The proposal

Three combinators on types, using set-theoretic notation:

| Surface         | Meaning                                            | Maps to today's        |
| --------------- | -------------------------------------------------- | ---------------------- |
| `A \| B`        | disjunction — "is an A **or** a B"                 | `UnionTyp {A, B}`      |
| `A & B`         | conjunction — "contains **both** A and B"          | record item combinator |
| `{ … }`         | record-set bracket — package the contents          | `RecordTyp` / het-set  |
| `"foo"`, `42`   | literal types (TypeScript-style)                   | `LiteralTyp`           |
| `name: A`       | named property (a property is a record item)       | `FieldDefinition`      |

The `{ … }` bracket is the crucial move: **the same bracket that constructs
records at the value level now constructs record-sets at the type level.**
Inside the braces, `&` is how you add another item (replaces the comma),
`|` is how you say a slot is alternative.

### 2.1 Worked examples

```
// Primitives and literal unions
Bool         = true | false
TrafficLight = "red" | "yellow" | "green"
HttpStatus   = 200 | 201 | 204 | 400 | 404 | 500

// Het-records with positional + named items
Point        = { Number & Number }
NamedPoint   = { x: Number & y: Number }
Mindset      = { [1,2,3] & "foo" & "bar"
               & name: String & age: Number & level: String }

// Discriminated unions — the tag is a positional literal
Option<T>    = { "None" } | { "Some" & T }
Result<T,E>  = { "Ok"  & T } | { "Err" & E }
Shape        = { "Circle" & radius: Number }
             | { "Square" & side:   Number }
             | { "Tri"    & a: Number & b: Number & c: Number }

// Row polymorphism — openness expressed uniformly
HasName      = { name: String & …r }        // open record over named row
```

### 2.2 Two distinct meanings of `|` at the record level

A landmine that the parser and the explainer both have to handle cleanly:

- `{ A } | { B }` — **union of two record types**. A value is *either* a
  record-with-A *or* a record-with-B. Union lives outside the braces.
- `{ A | B }` — **one record with one slot whose type is A-or-B**. Union
  lives inside one slot.

Both are expressible and semantically distinct. TypeScript handles the same
distinction identically. Worth a short tutorial section, but not a blocker.

## 3. Precedence

**Decided:** `&` binds tighter than `|`, following the universal
"and binds tighter than or" convention from C, TypeScript, Boolean
logic, and essentially every language a user has seen before. Both
operators are **left-associative**.

```
0 | 1 & String      ≡  0 | (1 & String)      // empty inner intersection
{ 0 | 1 & String }  ≡  { 0 | (1 & String) }  // same, inside a record
```

The "empty intersection of disjoint literals" (`1 & String` ≡ `never`)
is not what the user probably wanted for `{ 0 | 1 & String }` — so
when the user means "a record containing a (0 or 1) and a String", they
write it with parentheses:

```
{ (0 | 1) & String }       // record: (0 or 1) AND String
```

This is the TypeScript norm and matches muscle memory. Explicit
parens make the record-item-combinator reading unambiguous.

## 4. What this unlocks

### 4.1 `UnionTyp` becomes redundant as a surface concept

The `UnionTyp` constructor can stay inside the solver (where `CHasMember`
collects alternatives, then `closeUnion` packages them — [Lang.fs:688](../../src/TypeFighter/Lang.fs#L688)),
but it's no longer a *distinct surface syntax*. Users write `A | B`;
the solver produces the same `UnionTyp` internally.

Even `BuiltinTypes.boolean` stops being hardcoded special: `Bool = true | false`
becomes an ordinary library-level type definition.

### 4.2 Discriminated unions for free

Because literals are types and positionals can be literals, a Shape union is
just:

```
Shape = { "Circle" & radius: Number }
      | { "Square" & side:   Number }
```

No `type` with constructors, no tag-injection machinery. Pattern-matching
discriminates by matching the positional literal:

```
match shape with
| { "Circle" & radius } -> …
| { "Square" & side   } -> …
```

The match binds `radius` / `side` because the named slot is in scope when
the positional tag matches. This is **exactly the narrowing rule** we
need for pattern matching on unions generally (see §5).

### 4.3 Literal refinement without a special feature

```
type HttpStatus = 200 | 201 | 204 | 400 | 404 | 500
fun handle: HttpStatus -> String = …
handle 200   // ok
handle 999   // type error: 999 is not a member of HttpStatus
```

TypeScript's literal-type power, available because `LiteralTyp` already
exists in our type AST.

### 4.4 Row polymorphism stays uniform

An open record "has at least these fields" becomes:

```
{ name: String & …r }       // open: rest row r admits more items
{ name: String }            // closed: exactly this
```

Same `&` combinator, plus a row-variable tail — no new syntax for openness.

## 5. Interaction with pattern matching

The big payoff. The previous design discussions left pattern matching as
a TODO (no `Expr.Match` in [Lang.fs:138](../../src/TypeFighter/Lang.fs#L138)).
With this syntax, the match primitive becomes obvious: it's the *destructor*
for record-sets, and it automatically narrows disjunctions.

```
// scrutinee : { "Ok" & T } | { "Err" & String }
match result with
| { "Ok"  & value }   -> …        // in this arm, result : { "Ok"  & T }
| { "Err" & message } -> …        // in this arm, result : { "Err" & String }
```

Narrowing rule: in the body of each arm, the scrutinee's type is
intersected with the matched pattern's type. If the patterns cover the
union, the match is exhaustive; otherwise the solver raises a warning.

## 6. Conflicts with today's AST

| Today                                           | After this change                           |
| ----------------------------------------------- | ------------------------------------------- |
| `UnionTyp of Set<MonoTyp>`                      | Keep as internal repr; no surface syntax.   |
| `RecordTyp of RecordDefinition` (named only)    | Extended by [RecordsAsHeterogeneousSets.md](RecordsAsHeterogeneousSets.md) to carry a positional row. |
| `LiteralTyp of Literal`                         | Unchanged. Surface exposes it directly.     |
| `BuiltinTypes.boolean` hardcoded                | Becomes library type `Bool = true \| false`. |

> **Resolved (Step 5).** `IntersectionTyp` was removed. At the surface,
> top-level `&` between two record operands normalises at parse time
> into a single merged `RecordTyp`. Conflicting fields are rejected by
> the parser. No separate intersection constructor survives in the AST.

## 7. Parser implications

Type expressions grow to a small pratt-style grammar:

```
typeExpr     = altType { "&" altType }                // & is lowest-prec
altType      = primType { "|" primType }              // | is higher
primType     = literal
             | ident
             | "{" typeExpr "}"                        // record-set
             | "(" typeExpr ")"                        // grouping
             | ident "<" typeExpr { "," typeExpr } ">" // applied
```

Inside `{ … }`, a named item `name: typeExpr` is alternative to a bare
`typeExpr`. Mirrors the value-level parser proposal in
[RecordsAsHeterogeneousSets.md §5](RecordsAsHeterogeneousSets.md).

## 8. Open questions

- **`A & B` outside braces.** Should this be legal? It reads as "type-level
  intersection of A and B without record packaging." If A and B are
  disjoint literals, the result is empty — is that an error, or the empty
  type `never`? Flow and TypeScript both allow it (and call the result
  `never`). Cheap to support.
- **`&` vs comma — choose one regime, not both mixed.** Two serious
  candidates; mixed usage is out:
  1. **Abolish comma at the type level.** `{ A & B }` is the only form.
     Philosophically cleanest — the set-algebra reading is the *only*
     reading. Users coming from other languages may blink once, but
     the rule is trivially learnable.
  2. **Treat `&` and `,` as exact synonyms.** `{ A, B }` and `{ A & B }`
     produce identical types. Lower friction for newcomers, keeps the
     value-level record syntax uniform (since values also use commas).
     The cost: two ways to write the same thing, and the parser has
     to accept both.
  **Tentative lean:** option 2 (synonymous), because value-level records
  use commas and we want type syntax to *rhyme* with value syntax without
  forcing a change on one side. Decide when we do the parser work.
- **Row polymorphism syntax for positionals.** `{ Number & …r }` — is `r`
  an open *positional* row or a fully open row? Probably need
  `{ Number & …p & …r }` where `p` is positional-row, `r` is named-row
  — or one combined row variable.
- **Exhaustiveness for `|` at the record level.** When `match` covers
  some but not all arms of a record-type union, is that an error or a
  warning? TypeScript warns; Rust errors. Pick one and stick with it.
- **Shorthand.** TypeScript allows `{ x, y }` as shorthand for
  `{ x: x; y: y }` at values; do we mirror that at the type level too?
  Probably yes — it's the same symmetry.

## 9. Recommended sequencing

1. **Lock the value-level het-record change first** (AST refactor from
   [RecordsAsHeterogeneousSets.md §9 step 1](RecordsAsHeterogeneousSets.md)).
   No-op, half a day.
2. **Add literal types and `|` to the *surface syntax* for type
   annotations**, wiring straight to existing `UnionTyp`. Small change,
   unlocks the TypeScript-style literal-union demos immediately.
3. **Decide precedence** by trying both on a handful of real examples.
4. **Add `{ … }` and `&` at the type level**, aligned with the
   het-record value syntax. This is where the surface becomes uniform.
5. **Reconcile `&` with existing records.** Parser-level `&` between
   two record operands merges them into a single `RecordTyp`. The
   legacy `IntersectionTyp` constructor is dropped entirely.
6. **Add `Expr.Match`** to the expression AST, with narrowing over
   record-set types. This is the real "pattern matching" milestone.

Steps 1–4 are independent of each other in principle; 5–6 depend on the
earlier ones being in place.

## 10. Why this is worth doing

Three languages' worth of nice properties fall out of *one* consistent
surface:

- TypeScript-style literal unions and discriminated unions.
- OCaml/F#-style sum types — but without a separate `type` declaration.
- Structural row polymorphism — using the same combinators as everything
  else.

And the value-level and type-level syntax finally **rhyme**: `{ … }`
builds stuff, `&` adds to a collection, `|` picks one, on both sides
of the colon. That's the kind of small surface that makes a language
feel learnable.
