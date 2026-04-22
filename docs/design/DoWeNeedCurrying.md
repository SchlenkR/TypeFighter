# Do We Need Currying?

> **Status:** concept note, not a plan. Written down to force the question:
> is the cost of currying still paying for itself, given where TypeFighter
> is heading?

## The question, concretely

TypeFighter today is curried through and through:

```
let add = a => b => concat(a)(b);
log(add("foo")("bar"));
```

There is no `(a, b) => …` form. Multi-argument functions are all
`a => b => …`. Application is one-at-a-time: `f(x)(y)`, never `f(x, y)`.

This is the ML/Haskell tradition. It's also surprising to users coming
from JS, C#, Python, Java, Go, Rust, Swift — every mainstream language
uses multi-argument calls `f(a, b)`. Currying is a tax we pay on
familiarity; the question is whether the things it buys us still justify
the tax once the other planned features land.

## What currying pays for

1. **Partial application is free.** `let add5 = add(5)` — no ceremony,
   no placeholder syntax, no `bind` or `apply` method. Every function is
   a partial-application machine by construction.

2. **Clean composition.** `compose : (b -> c) -> (a -> b) -> a -> c`
   works out of the box. Point-free is a natural expression style, not
   a stunt.

3. **Uniform function type.** Every function has exactly one argument
   and one result. `a -> b -> c` is just `a -> (b -> c)`. Type rules,
   unification, substitution — all stay simple.

4. **HM inference is a little easier.** One-arg, one-result means
   unification always has two sides. Nothing in modern HM *requires*
   this (F# and OCaml both handle tuple-parameter functions fine), but
   a curried core is the textbook form and has less to go wrong.

5. **Pipelines chain cheaply.** `x |> f(y) |> g(z)` is literally
   `g(z)(f(y)(x))` — the pipe operator is just reverse application,
   no special "inject as last argument" rule needed.

6. **Operator sections fall out.** If `+` is a function of type
   `Number -> Number -> Number`, then `(+1)` is just `(+)(1)` — a unary
   function. `map(+1)` reads like it means.

## What currying costs

1. **Arity errors are invisible.** `f(x)` when `f` is two-argument
   silently produces a partially-applied function instead of an error.
   The mistake surfaces later, somewhere else, usually as a confusing
   "expected `Number`, got `String -> Number`" message. In a multi-arg
   language the error would name the call site directly.

2. **Unfamiliar call syntax.** `add(1)(2)` instead of `add(1, 2)` is a
   visible deviation on every single call. LLMs, beginners, and anyone
   pasting from another language get it wrong on the first try.

3. **No named / optional arguments.** Currying assumes a fixed,
   positional, order-dependent argument tape. Named arguments
   (`f(x = 1, y = 2)`) and optional arguments don't have a natural
   curried shape. Variadic functions are even worse.

4. **Hover tooltips and signature help feel wrong.** `add : Number ->
   Number -> Number` is mathematically correct and pedagogically
   useful, but most IDEs and most users read it as "a function that
   takes a Number and returns a function". The mental translation back
   to "takes two numbers" is constant overhead.

5. **Every application allocates.** In the emitted JS, `f(x)(y)` calls
   `f(x)` to get a closure, then calls that. Modern JITs inline this
   well in practice, but the emitted code is noisier to read.

6. **Awkward with multi-return or tupled arguments.** If you want to
   pass a pair you've already built as one value, `f(pair)` is fine —
   but then `f` has type `(a, b) -> c`, a separate shape, and now you
   have two function families in the language.

## Alternatives

### A. Multi-arg + pipeline-injection (Elixir / Gleam flavour)

```
let add = (a, b) => concat(a)(b);
let add5 = bind(add, 5);             // explicit partial, or placeholder
let greet = name => "Hello, " |> add(name);   // |> injects as first or last arg
```

`|>` does real work here: `x |> f(y)` desugars to `f(x, y)` (Elixir)
or `f(y, x)` (Elm-ish) — language picks one. Partial application
becomes explicit (`bind`, a `_` placeholder, or an inline lambda).

**Wins:** familiar call syntax, arity errors at the call site,
pipelines still ergonomic.
**Loses:** no free partial application, `compose` needs a placeholder
or wrapping lambda, point-free style is harder.

### B. Multi-arg + placeholder sections (Scala flavour)

```
let add = (a, b) => concat(a)(b);
let add5 = add(5, _);                // section → closes over remaining slot
let doubled = map(_ * 2, xs);        // operator section
```

A `_` in argument position desugars to a lambda closing over that
slot. Composes well with operators: `(+1)` becomes `_ + 1`. Explicit
and powerful — you can partial on *any* argument, not just a prefix.

**Wins:** familiar calls, explicit partial, works on any slot.
**Loses:** extra syntax rule (placeholder capture scope), `compose`
still not as clean as curried.

### C. Keep currying, add multi-arg as sugar

```
let add = (a, b) => concat(a)(b);    // sugars to a => b => concat(a)(b)
let add5 = add(5);                   // same curried machinery
add(1, 2);                           // sugars to add(1)(2)
```

The core stays curried; the surface accepts both styles. This is the
F# approach (with tuples) — actually F# has both curried *and* tupled
forms as distinct shapes, which creates its own confusion.

**Wins:** familiar call syntax, curried ergonomics for HOFs,
one-flag-two-styles pragmatism.
**Loses:** two ways to write the same thing, arity errors still
invisible on curried calls, tooltips still render curried shape.

### D. Keep currying as-is, document hard

Accept the tax, lean into the ML tradition, teach it well. If the
target audience is functional-curious, this is fine. If it's
mainstream / beginner / LLM-first, it's a headwind forever.

## How this interacts with other planned features

**Pipeline (`|>`).** With currying, `|>` is a trivial reverse-apply
— it already works and needs no special syntax. Without currying,
`|>` needs an injection rule (first or last arg). Either is
tractable, but *only currying makes `|>` syntactically free*.

**Operators.** If operators are going to be first-class functions
(e.g. `fold(+)(0)(xs)`), currying is what makes them cheap. Without
currying, every operator usage in HOF position wants an inline lambda
or placeholder. `map(+1)` only reads well in curried-land.

**Implicit binds / wrapper-unpack protocol.** Orthogonal. Works
either way.

**Records as heterogeneous sets / set-theoretic types.** Orthogonal.
Nothing in the type-level design cares about currying.

**Pattern matching.** Orthogonal at the language surface. One small
win for multi-arg: matching against a tuple-argument function is
slightly clearer than against a chain of curried lambdas.

**Beginner / LLM friendliness (`SyntaxForAIAndBeginners.md`).** Pulls
hard toward multi-arg. Every evaluator — humans new to FP, LLM
codegen, IDE tooltip readers — pattern-matches `f(a, b)` correctly on
sight. `f(a)(b)` is a consistent source of friction.

## The shape of the tradeoff

Currying earns its keep **when functional composition is the dominant
style** — when you write a lot of `compose`, a lot of `map f . filter
p`, a lot of point-free plumbing, a lot of operator sections. In that
world currying is not a tax, it's the enabling feature.

It starts to feel like a tax **when the dominant style is pipelines
with lambdas**: `xs |> map(x => x * 2) |> filter(x => x > 5)`. Here
the curried machinery is invisible — the lambda carries the intent.
`map` being curried versus multi-arg doesn't show up at the call
site. But the *cost* (unfamiliar call syntax, invisible arity errors)
is paid every line.

Pipelines with inline lambdas are the style most modern, beginner-
facing, LLM-read languages converge on (Elixir, Gleam, Rescript,
pieces of modern Swift). The fact that this is *also* the style
TypeFighter's existing playground examples reach for is a signal.

## Tentative lean

I'd put the question this way: **do we expect most TypeFighter code
to look like Haskell one-liners, or like Elixir pipelines?**

- If Haskell one-liners: keep currying. It's paying for itself.
- If Elixir pipelines with inline lambdas: multi-arg + placeholder
  sections (Alternative B) is the better fit. We lose cheap
  point-free but we gain familiarity, arity errors, and cleaner
  tooltips. Pipelines stay good; operator sections stay good;
  `compose` gets a little uglier but it's rarely the fulcrum of real
  code.

My lean today: **Alternative B (multi-arg + `_` placeholder
sections)**. Rationale:

1. Beginner/LLM accessibility is on the roadmap, and multi-arg is
   strictly friendlier there.
2. Visible arity errors are a real quality-of-life upgrade the current
   language is missing.
3. Pipelines and operators both still work well with placeholder
   sections — we don't lose the ergonomic ceiling.
4. The current implementation is already one-arg internally, so
   multi-arg is a *surface* change: the AST can keep its unary `App`
   and `Fun`, and the parser/emitter bridge multi-arg on both sides.
   HM stays curried under the hood.

That last point matters: we don't actually have to choose between
curried and non-curried semantics. We can keep the *core* curried
(HM loves it, the solver is simple, the emitter is trivial) and make
the *surface* multi-arg. The parser translates `f(a, b)` to `f(a)(b)`
and `(a, b) => e` to `a => b => e`. Partial application with `_` is
parser-level desugaring to a curried lambda.

## Open questions

- **Placeholder scope.** Where does `_` stop being a partial-app
  marker? `f(g(_))` — does the `_` bind to `f` or to `g`? Scala bounds
  it to the *nearest enclosing call*, which is one workable rule.
- **Tuple arguments as a separate shape.** If we want true tuple-taking
  functions (as opposed to curried-but-called-like-tuples), that's a
  distinct type. Probably don't introduce — it's the exact confusion
  F# suffers from.
- **Error-message UX.** `f(1)` when `f` takes two — in the surface
  multi-arg world, this is an arity error with the call site
  highlighted. Worth building well from day one.
- **Does the hover show `Number -> Number -> Number` or
  `(Number, Number) -> Number`?** If the surface is multi-arg, the
  tooltip should render multi-arg too, even if the core stores it
  curried. One translation layer in the type printer.

## Relation to other notes

- [SyntaxAlternatives.md](SyntaxAlternatives.md) — five concrete
  surface syntaxes; most of them are *implicitly* multi-arg, which is
  another signal.
- [SyntaxForAIAndBeginners.md](SyntaxForAIAndBeginners.md) — reranks
  those five against novice / LLM criteria. Directly relevant: the
  criteria that matter there (familiar shape, forgiving errors,
  readable at a glance) all tilt away from currying.
