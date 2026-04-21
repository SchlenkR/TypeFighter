# Wrapper-Type Opt-In for Implicit Bind

> **Status:** parked idea, not a plan. Written down so we don't lose it.
>
> Companion to [ImplicitBinds.md](ImplicitBinds.md).

## The idea

`ImplicitBinds.md` argued that a blanket rule *"any monadic RHS auto-binds"*
has real ambiguity problems (Option swallows failure paths, List vs Task
have different intent, etc.). Ronald's refinement sidesteps the blanket
rule:

> **The wrapper type itself declares whether it wants to be auto-unpacked
> by `let`.**

Different wrapper types have genuinely different intent:

| Wrapper type     | Default on `let x = rhs`                         |
| ---------------- | ------------------------------------------------ |
| `Task<T>`        | unpack — the caller wants `T`, not the pending Task |
| `Async<T>`       | unpack — same as Task                            |
| `List<T>`        | **don't** unpack — I asked for a list, I want a list |
| `Option<T>`      | arguable; let the *type author* decide         |
| `LocalState<T>`  | unpack — you want the result, not the state computation |
| User monads      | author's choice                                  |

The claim: this *isn't a syntactic rule*, it's a *protocol the type opts
into*. A wrapper type provides a field (say `_unpackOnLet`) that the `let`
elaborator inspects. Present and true → auto-bind. Absent or false → plain
assignment.

Because our type model already has row-polymorphic records, a "wrapper
type declares behavior" check is *just a row-access at the type level*.
No new kinds, no new metasystem — the mechanism is something TypeFighter
can already express.

## Why this is better than the blanket rule

- **List stays a list.** No surprising element-binding. `let xs = getList()`
  keeps `xs` as the list.
- **Option-as-failure is opt-in per use-site.** The `Option` type itself
  can default to non-unpack; if you want the short-circuit semantics, you
  pick it up by calling something that explicitly enters an Option-failing
  scope (or by using a wrapper that does opt in — e.g. `Fail<T>`).
- **Authors communicate intent.** Creating a new wrapper forces the
  question "should `let` unpack this?" to be answered at type-definition
  time, not at every use-site.

## Ambiguities this still does *not* solve

Read `ImplicitBinds.md` for the full catalogue. The ones that survive:

- **§2.2 Multiple auto-unpack monads in one scope** — if `Task<T>` and
  `IO<T>` are both auto-unpack and both appear in one function, the
  composition question is unchanged. Still need a "which monad first?"
  rule, or an explicit transformer, or effect-style composition.
- **§2.3 Pass-through** — if I *want* the `Task<T>` itself (e.g. to pass to
  `Task.WhenAll`), auto-unpack is wrong. Need an escape hatch
  (annotation, `raw` keyword, `let* x = …`, …).
- **§2.5 Type-directed runtime semantics** — the same `let` token still
  means different things depending on the RHS type. The *rule* is now
  readable ("the wrapper declares it"), but the reader still has to look up
  the wrapper's declaration to know what happens.

## Relationship to algebraic effects (the §3 Option C path)

Algebraic effects *also* solve the "which monads do we auto-unpack?"
question, but at a deeper level: there are *no* wrapper types to unpack,
because effects aren't values. Handlers at the boundary decide how the
effect is interpreted.

| Axis                    | Wrapper-opt-in                     | Algebraic effects                   |
| ----------------------- | ---------------------------------- | ----------------------------------- |
| What moves the value    | The wrapper type is a value        | Effects are separate from values    |
| Composition             | Still needs transformers / rules   | Free; effect rows unify             |
| Uses existing TypeFighter row machinery | Yes (row-access at type level) | Yes (effect row ≅ field row)       |
| Size of change          | Small                              | Medium-large                        |
| Solves §2.2             | No                                 | Yes                                 |
| Solves §2.3             | No                                 | Yes (by construction)               |
| Authorial ergonomics    | Good — one field to set            | Good — effect declaration           |

**Verdict:** wrapper-opt-in is a *cheaper, smaller, and still-useful*
refinement of "blanket implicit bind." It fixes the List-vs-Task intent
problem cleanly. It does **not** replace algebraic effects as the long-term
principled answer.

A sensible sequencing: ship wrapper-opt-in first (small), learn how it
feels, then decide whether to invest in effects.

## Open questions for later

- What exactly is the magic field name? (`_unpackOnLet`, `__bind__`,
  `computeUnit`?) Not a trivial call — it's a reserved identifier.
- Does the wrapper also declare its `Bind` implementation via the same
  record, or is `Bind` resolved separately (by row / by type class / by
  marker)?
- What's the user-facing syntax for declaring a wrapper type? Presumably
  the same record-definition syntax, with the magic field among others.
- If two wrappers both opt in and appear in one function body, whose
  `Bind` runs first? (Same question as §2.2 — wrapper-opt-in does not
  solve it, only makes the trigger legible.)
