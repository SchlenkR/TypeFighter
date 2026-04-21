# Implicit Binds — Design Analysis

> *"Was wäre, wenn `let` automatisch wie `let!` funktioniert, sobald die
> rechte Seite monadisch ist?"*

Companion notes:
[SyntaxAlternatives.md](SyntaxAlternatives.md),
[SyntaxForAIAndBeginners.md](SyntaxForAIAndBeginners.md),
[SyntaxForPxlClock.md](SyntaxForPxlClock.md).

---

## 1. The idea, restated

F# computation expressions require an explicit opt-in for bind:

```fsharp
async {
    let! x = fetchUser()          // ! = Bind
    let! y = fetchBalance(x)      // ! = Bind
    let  local = format(y)        // no Bind
    return local                   // Return wraps back into Async
}
```

Ronald's proposal: drop the `!`. If the inferred type of the right-hand side
of a `let` is a "monadic" type, the compiler inserts `Bind` automatically.
`let` *is* `let!` whenever it needs to be.

```
// Same semantic program, without `!`:
let fetchAccountValue () =
    let x     = fetchUser()       // RHS is Task<User>  → auto Bind
    let y     = fetchBalance(x)   // RHS is Task<Money> → auto Bind
    let local = format(y)         // RHS is string       → plain let
    local                          // auto Return
```

The promise: **CE-light without ceremony**. `async/await` becomes invisible.

The question Ronald asked: *which ambiguities and problems does this
create?* Concrete answer below, then three ways to resolve, then a
recommendation.

---

## 2. The catalogue of ambiguities

Seven real problems, roughly in order of how badly each hurts.

### 2.1 Nested monads — how deep do we unwrap?

```
let xs = fetchListOfOptions()     // Task<List<Option<User>>>
```

What type does `xs` have?

- `List<Option<User>>` — one level of Bind (Task away).
- `Option<User>` — two levels.
- `User` — all the way.

In Haskell's `do`-notation the rule is trivial: *exactly one level, of the
monad named by the enclosing block*. Ronald's proposal has no enclosing
block, so there is no name to follow. We have to invent a rule. Every rule
surprises somebody.

### 2.2 Two different monads in one scope

```
let greet(name) =
    let u = findUser(name)              // Option<User>
    let t = fetchGreetingTemplate()     // Task<Template>
    render(t, u)
```

What is the return type of `greet`? `Option<Task<…>>`? `Task<Option<…>>`?
These are different monads with different semantics (fail-then-await vs.
await-then-fail), and the compiler would have to pick one.

In Haskell and F#, you solve this with a monad **transformer** (e.g.
`OptionT<Task>`). But you have to *write* the transformer explicitly. The
moment you want implicit binds across multiple monads, you need either:

- auto-lifting (dangerous — imagine `Option` being auto-lifted into `Task`
  for every call that expected a `Task<X>`),
- or a hard error,
- or a single pinned monad (back to the block).

**This is the hardest structural problem.** Classical monad theory says
monads don't compose without transformers. Implicit bind pretends they do.

### 2.3 Pass-through — I want the `Task`, not the value

```
let requests = [ fetchUser(1), fetchUser(2), fetchUser(3) ]
let all = Task.WhenAll(requests)
let results = all
```

If `let` always auto-unwraps, each list element becomes `User`, not
`Task<User>`. `Task.WhenAll` on `User` is a type error. The user has to
say *"don't unwrap here."*

The only ways to signal "don't unwrap":
- Explicit type annotation on the binder (`let requests: List<Task<User>> = …`).
- A different binder keyword (`rawlet`, `let*`, …). Brings the sigil back
  under a different name.
- Heuristic: "only unwrap if the RHS is a direct monad call, not if it sits
  inside a collection." Fragile and surprising.

Every option reintroduces some kind of marker, defeating part of the point.

### 2.4 Return wrapping — `return` goes where?

If our syntax drops `return`, the last expression of the function must be
auto-wrapped in the scope monad. That requires us to know the scope monad
at the `return` site. If the function has no signature (pure HM), the
compiler infers the monad from somewhere else — usually the last
expression's type. Circular in ambiguous cases (e.g. the function body
contains no monadic operation at all: plain function or `Id`-monad return?).

### 2.5 Type-directed runtime semantics

The deep philosophical issue: with implicit bind, the *same* syntax has
*different* runtime behavior depending on types inferred elsewhere.

```
let x = foo(arg)
print(x)
```

Without looking up `foo`'s type, a reader doesn't know whether:
- `x` is `Option<T>` and `print` is called on the option, **or**
- `x` is `T` (unwrapped) and a whole branch of execution has been silently
  short-circuited at that `let`.

Concretely:

- **Refactoring is hazardous.** A library maintainer changes a function's
  return type from `T` to `Option<T>`; every caller that used `let x =
  lib.foo()` now silently gets short-circuit semantics, without a syntactic
  change.
- **IDE burden.** The editor must decorate every auto-bound `let` to make
  the invisible visible. Without that, users read the wrong program.

F#'s choice to *require* the `!` is precisely so that this can never happen
silently. Ronald's proposal inverts that choice.

### 2.6 Option / Result silently swallow failure paths

Treat `Option` as implicitly bindable:

```
let userName(id) =
    let u = findUser(id)            // Option<User> → auto Bind
    u.name
```

Now `userName` has type `int -> Option<string>`. Every caller of
`userName` also becomes bindable. The "I'm not in the failure business"
code path cascades failure semantics outward, and the user never typed an
`Option` themselves.

This is exactly how F#'s `maybe` CE is supposed to work — but F# makes you
*opt in* by writing `maybe { … }`. Implicit bind opts you in by accident.

### 2.7 HM interaction — when can we decide?

TypeFighter is Hindley-Milner. At the point of seeing `let x = rhs`, the
type of `rhs` is often a free type variable. We can't decide "is this
monadic?" until unification resolves it. Options:

- **Post-unification elaboration.** Keep the `let` syntactic; after type
  inference, rewrite any `let` whose RHS unifies with `m<τ>` for some
  known monad `m` as a bind. Doable, but adds a typed pass *after*
  inference — standard in GHC but a new thing for us.
- **Type-class-style dispatch.** Require the monad to be named (e.g.,
  `instance Monad Task { … }`), then bind is a dispatch on the type-class.
  That's Haskell's answer; requires adding type classes to TypeFighter's
  inference, which is a large project.
- **Marker-directed.** Every type that participates carries a flag. Ad-hoc.

All three are feasible; all three enlarge the core.

---

## 3. What other languages did

- **Haskell `do`.** Explicit block, implicit bind inside, pinned to one
  monad (selected by the block's inferred type). Explicit `return`.
- **F# CE.** Explicit block (`async { }`, `task { }`, …), explicit
  `let!`, explicit `return`. Maximum verbosity; zero hidden behavior.
- **Scala `for`-comprehension.** Explicit block, `<-` for bind, `yield`
  for return. Again scoped.
- **Idris `!`-notation.** Sugar `let x = !m` inside `do`-blocks. Reduces
  nesting; still inside an explicit block; still carries `!`.
- **Koka, Unison, Eff, Frank, OCaml 5, Effekt.** Drop monads entirely in
  favour of **algebraic effects**. Operations are called like normal
  functions; effects are tracked as rows in the type; handlers are
  lexically scoped. *This is the modern research answer to "I want implicit
  binds without `let!`."*

The shift from "monads" to "effects" exists specifically because of the
problems listed in §2 — and the most damning one, §2.2 (composition).
Algebraic effects compose by construction; monads need transformers.

---

## 4. Three ways to resolve it

### Option A — scoped block, no `let!`

Keep the block (`do`, `effect`, `compute`, whatever we call it). Drop the
`!`. The block *pins* the monad, so every `let` inside is implicit-bind
for that one monad.

```
task {
    let x = fetchUser()           // Bind against Task
    let y = fetchBalance(x)       // Bind against Task
    y                              // Return into Task
}
```

- **Solves:** 2.1, 2.2, 2.5, 2.6, 2.7 — all handled by the block
  pinning the monad.
- **Still broken:** 2.3 pass-through (can't store a `Task<T>` inside the
  block without unwrapping).
- **Cost:** low. Mostly sugar over the existing CE machinery.
- **Feel for the user:** barely different from F#. The block is still
  there; you just write `let` instead of `let!`.

### Option B — inference-directed, no block

No block. The function's return type (or an annotation on the function)
names the monad. Inside, every `let` of that monad is bound.

```
async let fetchAccountValue () =
    let x = fetchUser()            // Bind against Async
    let y = fetchBalance(x)        // Bind against Async
    y                               // Return into Async
```

- **Solves:** 2.1 (one level), 2.5 (annotation is visible), 2.6 (monad
  declared at function).
- **Still broken:** 2.2 (still one pinned monad), 2.3 (pass-through),
  and the annotation is semantic, not decorative.
- **Cost:** medium. Requires a new "effect on function" marker and
  post-inference elaboration.
- **Feel:** closer to what Ronald described — but honestly, `async let f()`
  is a mark just as visible as `let!`, only relocated.

### Option C — algebraic effects with row tracking

This is the one that actually matches Ronald's feel. Effects are tracked
in types as rows (which TypeFighter already has for records). Operations
are called plainly. Effects compose by row-union. Handlers are lexically
scoped at the program boundary.

```
let fetchAccountValue () =          -- inferred effect row: <Http, Fail>
    let x = fetchUser()              -- performs Http, may Fail
    let y = fetchBalance(x)          -- performs Http, may Fail
    y

// At the program boundary — effects handled here, not threaded through:
runWith {
    handle Http using realHttp
    handle Fail using returnNone
    fetchAccountValue()
}
```

- **Solves:** all seven items in §2. Effects compose (no transformer
  problem). The operations are call sites, so pass-through is just "don't
  call the operation." No monad to name. Return-wrapping vanishes because
  effects aren't value-carriers.
- **Cost:** high. New kind in the type system (effect rows — though we
  already have a row-kinded machinery for records). Handlers are a new
  runtime construct. Effect inference is an active research area.
- **Feel:** exactly what Ronald described. `await` is implicit. `maybe`
  is implicit. `log` is implicit. And — unlike Option B — it *really*
  composes: one function can perform Http + Fail + Log in any combination.
- **Bonus:** this is a direction where TypeFighter could actually be novel.
  Row polymorphism + algebraic effects is a small, elegant research combo
  (Koka is the reference, Frank and Effekt the recent academic contenders).

---

## 5. Recommendation

For TypeFighter specifically, **Option C (algebraic effects over rows) is
the principled path** because:

1. We already have row polymorphism in the core; effects as rows reuses
   that machinery. Effects = rows of operations, just like records = rows
   of fields. The *mathematics* is the same; only the interpretation
   differs.
2. It delivers what Ronald actually wants — implicit `await`, implicit
   `Option`-threading, implicit logging — without any of the seven
   problems in §2. Monads do not.
3. It's a genuinely interesting research angle. "TypeFighter: HM + row
   polymorphism + algebraic effects" is a coherent, small, and novel
   design. "TypeFighter: HM + implicit monad binds" is a design that
   Haskell already rejected in 1992 for the reasons listed above.

**If Option C is too large a bet for now, Option A (scoped block, no `let!`)
is the cheap fallback.** It keeps the block, which means it keeps the
safety; it just drops the `!`. That's small, harmless, and familiar —
closer to Haskell `do` than to anything radical. It's a ~2-day change.

**Option B I would argue against.** The annotation `async let f () = …`
looks lighter than `async { let! x = … }`, but semantically it's the same
weight — and it spreads that weight across the function signature rather
than concentrating it at the binder site, which is harder, not easier, to
read.

### Concrete next step

If you want to explore this, the smallest useful experiment is:

1. Add **one** algebraic effect (`Fail` is the simplest — it's the
   `Option` monad in effect clothing) to TypeFighter's type system.
2. Write an inference rule that tracks the effect row alongside normal
   types.
3. Build a single `handle Fail using …` combinator.
4. See whether `let x = mightFail(arg)` *just works* without `!`, and
   whether the inferred type of the enclosing function cleanly shows
   `<Fail>` in its effect row.

If that clicks, add `Http` and `Log` and watch them compose. If it
doesn't — we learn something real about the inference core, and Option A
is still there as the fallback.

---

## Sources

- [Koka — A Functional Programming Language (Microsoft Research)](https://koka-lang.github.io/koka/doc/index.html)
- [Unison — Abilities and Ability Handlers](https://www.unisonweb.org/docs/language-reference/)
- [Frank — Conor McBride et al.](https://github.com/frank-lang/frank)
- [Eff — Programming Language for Algebraic Effects](https://www.eff-lang.org/)
- [Effekt — Lightweight Effect Polymorphism](https://effekt-lang.org/)
- [F# Computation Expressions (docs.microsoft.com)](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions)
- [Haskell `do`-notation (Haskell Wiki)](https://wiki.haskell.org/Keywords#do)
- [Scala for-comprehensions (Scala docs)](https://docs.scala-lang.org/tour/for-comprehensions.html)
- [Algebraic Effects for the Rest of Us — Dan Abramov](https://overreacted.io/algebraic-effects-for-the-rest-of-us/)
