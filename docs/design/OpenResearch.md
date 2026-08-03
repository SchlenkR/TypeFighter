# Open Research Topics

> A running list of design questions flagged for later work — things we
> haven't committed to, but don't want to lose. Items get added as they
> come up in discussions. Once resolved they migrate to
> [DesignDecisions.md](DesignDecisions.md) as an ADR.
>
> Format: one short paragraph per item, with a date-added, and a link
> to any related concept doc or paper.

## Active

- [ ] **Invocation of optional functions** *(added 2026-04-22)*

  What happens when the *function itself* is optional in scope — e.g.
  a value of type `Option<(x: Number) -> Number>` that you want to
  call? Swift handles this with optional chaining (`f?(x: 1)`). Null
  / Option values in a call position need their own semantic story.
  Related: whether we want a dedicated short-circuit operator, or
  whether this folds into the same framework as optional *arguments*
  below.

- [ ] **Comfortable call-site syntax for optional arguments** *(added 2026-04-22)*

  Once functions take named records as arguments (ADR-005), optional
  slots become expressible as *unions with `absent`* per Sun & Oliveira
  (ESOP 2025, [Named Arguments as Intersections, Optional Arguments as
  Unions](https://i.cs.hku.hk/~bruno/papers/esop25named.pdf)). Open
  design questions:
  - Syntax at the definition site — `y: Number?`, `y?: Number`, `y: Number | absent`?
  - Default values — `y: Number = 0` or separate form?
  - Can the caller omit optional slots wholesale, or does each one
    need explicit `y: absent` / `y: omit`?
  - Does this interact with auto-partial under ADR-005 — i.e. is
    "omitting an optional" distinguishable from "under-applying a
    required arg"?

- [ ] **Stream blocks become an opt-in library, not part of the language** *(added 2026-08-03)*

  The step *protocol* stays in the language (ADR-010) — that is the
  mechanism, and the core is the right place for it. What does **not**
  belong there is the ~51 concrete blocks built on top of it:
  `math.ema`, `gen.counter`, `logic.hold`, `dsp.peakHold` and friends.
  A host that only evaluates formulas or rules should be able to embed
  TypeFighter without any of them existing — no signatures in the
  prelude, no completion entries, no DSP dependency.

  Where it sits today:
  - The core (`src/TypeFighter/`) knows nothing about the blocks —
    correct already.
  - `libs/DSP` holds the actual stream engine — separate already.
  - `TypeFighter.Web/Builtins.fs` mixes two unrelated things in one
    file: the *language* builtins (`log`, `concat`, `ToString`, `op_*`,
    `head`, `at`, `cond`, …) and `importedBlocks`, the block table.
    `prelude` and `csImports` concatenate both, so every host gets the
    blocks whether it wants them or not.
  - `TypeFighter.Web/DspAdapter.fs` (304 lines) is pure block
    runtime.
  - `Api.mkOracle` hard-wires `Web.Builtins.namespaceMembers`.

  The injection path already exists and is the obvious lever: every
  entry point takes `extraPrelude`, and `Eval` takes `ExtraBinding`
  values — the mechanism a host already uses to inject its value records. So the work
  is packaging, not redesign: split the block table plus the adapter
  into their own project (referencing DSP), have it expose one
  bundle of {prelude signatures, runtime closures, namespace members},
  and let a host pass that in instead of `Api` reaching for it.

  Open points: `namespaceMembers` needs a route through the oracle
  rather than a direct reference; runtime closures (`csImports`) need
  the same treatment as the type signatures; and it is worth deciding
  whether the namespace-record prelude entries are a general
  "namespace pack" feature or stay specific to blocks.

- [ ] **Type aliases, so protocol markers stay out of signatures** *(added 2026-08-03)*

  After ADR-010 the marker is invisible in normal source, but it is still
  visible in *types*: the raw shape is `{ __machine: { init: S; step: ... } }`.
  Today only the web host hides it, via a `ShowTyp` printer that renders
  step-records as `Stream<V>` — a display-level patch in `Api.fs`, not a
  language feature, so any other host sees the raw record.

  A real type alias (`Step<V, S>` naming the structural record) would fix
  this properly: aliases are pure surface, so structural unification is
  untouched, but hovers, prelude signatures and error messages could all
  read `Stream<Number>`. The type system has no alias concept at all
  today, so this is new machinery in an untouched area — worth it only if
  aliases earn their keep beyond this one case. Interacts with the item
  above: once the blocks ship as a library, their signatures are exactly
  where the raw shape would otherwise leak into a host's face.

- [ ] **Block library: consistency defects found by running all 51** *(added 2026-08-03)*

  Every block was executed through Roslyn, not just read: all 51 compile and
  produce values, and the arithmetic checks out (`movingAverage`,
  `cumulativeProduct`, `rollingMin/Max`, `diff`, `clamp`, `rescale`,
  `delayByN`, `bitCrush`, `hardClip` all verified by hand). The defects are
  in consistency, not correctness:

  - **`dsp.adsr` takes no sample rate**, unlike every other time-based DSP
    block (`envFollow(src, sr, atk, rel)`, `lowPass1(src, cutoff, sr)`).
    Its times are therefore in ticks, not seconds — measured, `a = 0.1`
    reaches full level in a single tick. Silent wrong results for anyone
    who reads them as seconds.
  - **`logic.latch(trigger, src, def)` puts the trigger first**, breaking
    the documented rule "the source is always the FIRST argument" that
    method-call syntax relies on. The only violation among the 51.
  - **Three oscillators, three phase conventions**: measured at identical
    parameters, `sineOsc` starts at 0, `sawOsc` at -1, `triangleOsc` at +1
    descending. Surprising as soon as they are mixed.
  - **`gen.fibonacci` yields `[1, 2, 3, 5, 8, …]`** — the leading 1 is
    missing; the sequence starts at the third term.
  - **`math.ema` and `dsp.lowPass1` are the same filter** (both 1-pole
    low-pass) in two namespaces, with different parametrisation and
    different capability: `lowPass1`'s cutoff is a modulatable stream,
    `ema`'s alpha is a fixed number.
  - **Tri-state blocks are misnamed and mis-typed**: `logic.threshold`
    sounds like a comparison but is an edge detector (measured
    `[0,0,0,1,0,0,…]`) returning `Stream<Number>`, as do `edge` and
    `crossover` (-1/0/1) — sitting in the same namespace as `changed`,
    `toggle`, `debounce`, which return real Bool streams.
  - **`rollingStdDev` uses the population formula** (÷n, verified:
    `sqrt(2/3)`), which is nowhere documented.
  - **`countSince` returns 1 before anything has ever triggered.**

  Two structural points: the four exposed namespaces (`math`, `gen`,
  `logic`, `dsp`) do not match the library's own eleven-folder taxonomy —
  `logic` merges five library areas, and `gen.sineOsc` comes from
  `Dsp/Oscillators` while everything else under `Dsp/` maps to `dsp`. And
  **seven blocks exist in the library but are never exported**: the whole
  `TimeSeries` area (LookBack, LookAhead, WindowAggregate,
  StreamInterpolate, the Aggregate resamplers), plus `Counting/TimeSince`
  and `Windowing/Segment` — presumably because they work on `DataPoint<'v>`
  rather than `float`, but nothing records that.

  Fixing the first three is a breaking change to block signatures and
  behaviour, so it needs a decision, not a patch.

- [ ] **Parser and combinator library: three open questions** *(added 2026-08-03, moved out of `Syntax.fs`)*

  These sat as an 81-line comment block at the top of the parser. Moved here
  verbatim in substance; one claim in the original was already outdated.

  **1. Trivia handling.** "Trivia" = whitespace, comments, anything between
  meaningful tokens that carries no semantic value. TheBlunt has no built-in
  notion of it, so `ws` and `tok` are sprinkled ad-hoc through the parser:
  noisy, and easy to forget at a single call site. Trivia is also silently
  dropped — nothing is recorded on the AST, so source cannot be round-tripped
  for tooling (formatter, refactoring, hover). Options: push trivia into
  TheBlunt itself (a `ws` slot every combinator skips), or attach
  leading/trailing trivia per node for a lossless AST — the bigger model
  change, since ranges grow into `leadingTrivia + token + trailingTrivia`.
  *(The original block claimed "there is no support for line/block comments
  yet". Line comments exist — `lineComment`, wired into `ws`. Block comments
  `/* … */` still do not, and would raise the nesting question TheBlunt's flat
  model does not handle well.)*

  **2. What range does a parser return?** A TheBlunt subtlety: parent
  combinators (`many`, `andThen`, `bind`, …) advance the cursor from the
  *returned* `range.endIdx` of a child — not from where the child actually
  walked. A `parse { … }` block returning a hand-crafted PVal whose range stops
  short of what was consumed makes the outer parser silently re-parse or fail
  at the wrong position; `callArgs` / `propTail` hit exactly this. The rule
  discovered: **a parser's returned `range.endIdx` must equal the cursor
  position it actually leaves behind.** Nothing checks this. Options: document
  the contract in TheBlunt; have the combinator library track the cursor itself
  so the range is derived rather than asserted; or stop using `range.endIdx` as
  a cursor at all and thread an explicit cursor through every result — removes
  a class of bugs at the cost of one field per PVal.

  **3. Backtracking, and the missing `commit`.** `<|>` and `pchoice`
  backtrack unconditionally: every failure resets and tries the next
  alternative. Deep syntax errors — a malformed expression inside a record
  literal — therefore surface as the *outer* "no alternative matched", with the
  real cause lost. TheBlunt has no `commit`/fatal notion. A `pcommit`
  combinator (once a parser passes a "we are definitely in this branch" point,
  e.g. the `{` of a record literal, later failures become fatal and propagate
  past `<|>`) would improve error messages a lot, but changes the semantics of
  the combinator algebra.

## Resolved

*(Migrated to [DesignDecisions.md](DesignDecisions.md) as ADRs when decided.)*
