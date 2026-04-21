# Syntax Exploration — A Language for the PXL Clock

Applied thinking: if we were to design a language *specifically* for PXL Clock
pixograms, shaped by what PXL Clock *actually does* (24×24, per-frame, heavy
AI authoring, also beginners), which non-mainstream syntaxes are worth
exploring?

Companion notes:
[SyntaxAlternatives.md](SyntaxAlternatives.md),
[SyntaxForAIAndBeginners.md](SyntaxForAIAndBeginners.md).

---

## 1. What PXL Clock programs actually look like

Reading the existing C# demos and clock faces, pixograms cluster around a
very narrow set of patterns:

- A single scene lambda `(DrawingContext ctx) => { ... }` called every frame.
- Time-driven math: `var t = ctx.Elapsed.TotalSeconds; Math.Sin(t * 2) * 8`.
- Either (a) draw primitives (`DrawCircle`, `DrawPoint`, `DrawTextVar4x5`)
  or (b) iterate over pixels and set colours directly
  (`foreach cell in ctx.Pixels.Cells → pixels[x, y] = color`).
- Colour construction: `Color.FromHsv360(hue, 1.0, value)`.
- Range-based positioning via `switch` expressions (e.g. "seconds 0..21 map
  to top row, 22..25 to right column …").
- Trails via `for i in 0..5` with a time offset.

So the *actual* domain is:

> **"for every pixel (x,y) and every time t, decide a colour."**

Plus a little state for things that clearly benefit from being imperative
(clock hands, trails that depend on previous frames).

This is a *much* narrower domain than "general-purpose programming" —
narrower than TypeFighter targets — which is exactly the reason a DSL-style
language can win big here.

---

## 2. Five non-mainstream syntaxes worth exploring

Each has a real tradition, a real user base, and maps *naturally* to what
PXL Clock programs do. Mainstream alternatives (Python, Lua, JS) are
deliberately excluded — they're baselines, not exploration targets.

### A. Shader-style (Shadertoy / GLSL-inspired)

The idea: a pixogram is **a pure function** `pixel(x, y, t) -> color`. No
mutable canvas, no `DrawPoint` side-effects — the runtime calls your function
576 times per frame, once per pixel.

```glsl
pixel(x, y, t) =
    let v = sin(x * 0.5 + t)
          + sin(y * 0.5 + t)
          + sin((x + y) * 0.3 + t)
          + sin(sqrt(x*x + y*y) * 0.3)
    hsv((v + 4) / 8, 1.0, 0.5)
```

**Why it fits PXL Clock:**
- Matches the dominant existing pattern (the plasma demo is literally this).
- 576 pixels × 40 FPS is tiny — we can afford the "pure function per pixel"
  model that big displays can't.
- Compositional: layering is just `pixel1 over pixel2`; time-dilation is just
  `pixel(x, y, t * 2)`.

**Why it fits AI:**
- Shadertoy is one of the single largest curated corpora of creative-coding
  code on the web. LLMs are *extremely good* at GLSL-style output.
- One function, no loops, no state — there's exactly one thing to get right.

**Why it fits beginners:**
- One concept to learn: *"what colour is this pixel at this moment?"*
- No loops, no coordinate bookkeeping, no `foreach`. The runtime handles it.
- Hard mental step: *pure functions feel unfamiliar at first*. But once it
  clicks, it stays clicked.

**Downside:** stateful effects (previous-frame trails, Game of Life) don't
fit. Fix: add a `pixelWithHistory(x, y, t, prevFrame)` variant.

### B. Turtle / Logo — modernised

The idea: a pixogram commands a **turtle** that walks the canvas, plots, and
turns. Born in 1967 as a beginner language, still unmatched for spatial
intuition.

```
turtle goto(12, 12)
turtle pen(red)
repeat 4:
    turtle forward(8)
    turtle turn(90)
```

For a clock face:

```
for s in 0..now.second:
    turtle goto(ring(outer, s))
    turtle plot(hsv(190, 1, fade(s)))
```

**Why it fits PXL Clock:**
- Clock faces are naturally *paths*: ring of seconds, ring of minutes,
  spiral, trail. Turtle expresses paths directly.
- Pixel art and turtle graphics share a lineage (early Logo implementations
  ran on 40×40 pixel displays).

**Why it fits AI:**
- Medium-size corpus (lots of Logo, Python turtle tutorials). LLMs handle
  it well when the command set is small and named verbs are imperative.
- Linear, stateful commands are easy to generate token-by-token.

**Why it fits beginners:**
- The most successfully-taught beginner paradigm of all time. Kids start
  here. Adults take to it almost as well.
- Spatial reasoning is immediate: "walk forward, turn, plot" maps to body
  knowledge.

**Downside:** per-pixel effects (plasma, gradients) are awkward. Best as a
*layer* on top of a canvas, not a whole language.

### C. Livecoding / dataflow (Hydra-inspired)

The idea: describe the scene as a **chain of visual transforms**. No loops,
no per-pixel thinking — just compose visual generators and modulators.

```
osc(40, 0.1, 0.8)       -- base: oscillating stripes
  .rotate(t * 0.2)      -- spin over time
  .kaleid(4)            -- fourfold symmetry
  .colorize(palette)    -- recolor via palette
  .mask(clockFace)      -- only inside the clock circle
  .out()
```

**Why it fits PXL Clock:**
- The PXL VS Code extension *already* has live hot reload and a built-in
  simulator — this is the exact Hydra-style workflow.
- Composable transforms scale to surprising complexity without loops or
  state, which is great on a 24×24 target.

**Why it fits AI:**
- Hydra itself has a small but growing corpus. The method-chain style is
  very close to JS fluent APIs, which LLMs handle extremely well.
- Every line is an additive transform — easy to generate incrementally and
  verify against a running simulator.

**Why it fits beginners:**
- *Nothing* breaks. Every transform is safe; there are no index errors.
- Visual feedback is immediate and additive: add a `.rotate(t)`, see it
  rotate.
- Unusual for beginners in that it teaches *composition* before *control
  flow* — which is arguably the right order today.

**Downside:** step-by-step debugging is weird. There's no "step into"; the
whole pipeline fires at once. Fine for visual output, bad for logic.

### D. Array-oriented (BQN-ish, readable)

The idea: the canvas **is a value**. You manipulate 24×24 grids as first-
class objects — no loops, no indexing.

Here with more readable words than classical APL glyphs:

```
let xs  = arange(24)                   -- [0..23]
let grid = outer(xs, xs, (x, y) ->
    hsv((x + y + t) / 48, 1, 0.5))     -- a 24×24 colour grid
render grid
```

Rings of pixels are just a mask:

```
let dist  = map(cells, (x, y) -> hypot(x - 12, y - 12))
let ring  = (dist >= 10) and (dist < 11)
render where(ring, white, background)
```

**Why it fits PXL Clock:**
- 24×24 is ridiculously small. *Every* operation is a whole-canvas
  operation, basically for free.
- Array thinking expresses "all pixels whose distance is between 10 and 11"
  in one line, without any for-loop.

**Why it fits AI:**
- Mixed: classical APL has *tiny* training data. But BQN-ish readable
  spellings (and equivalent NumPy code) are well-represented.
- Single-line solutions are easier to self-check than ten-line imperative
  equivalents.

**Why it fits beginners:**
- Radically different mental model. Once internalised, removes an enormous
  category of bugs (off-by-one, forgotten inner loop) that beginners
  actually hit.
- But the learning curve is real — this is the *"expert-friendly, beginner-
  unfriendly"* outlier in this list.

**Downside:** off-mainstream enough that many people's first encounter will
be confusion. Works best as an *optional* style alongside a more
conventional one.

### E. Cellular / rule-based

The idea: you write a **rule** that takes the previous-frame pixel and its
neighbours and returns the new colour. The runtime applies the rule to
every cell every frame.

```
rule pixel(self, neighbours, t):
    let alive = count(neighbours, (n) -> n.alpha > 0.5)
    if self.alpha > 0.5 and alive in (2, 3): white
    elif self.alpha < 0.5 and alive == 3:    white
    else:                                     black
```

**Why it fits PXL Clock:**
- Game of Life is *already* in the existing demos. Plasma, diffusion,
  reaction-diffusion, falling sand, fire effects, CRT aftershock — all of
  these are neighbour-rules.
- Neighbours fit 24×24 perfectly (fast, no boundary pain worth speaking of).

**Why it fits AI:**
- Clear specification: neighbour count → colour. LLMs find this much easier
  than "write the whole Game of Life iteration" because the per-cell rule
  *is* the abstraction.
- Small, composable rule syntax means fewer places to get the generation
  wrong.

**Why it fits beginners:**
- The mental model ("pixels react to their neighbours") is visually obvious
  on the device and lights up curiosity fast.
- It's how almost every emergent-behaviour tutorial starts; a natural
  gateway to "wait, complex things emerge from simple rules?"

**Downside:** static images (e.g. a clock face showing the current time)
don't fit a cellular model. Works best as a *mode* alongside A or B.

---

## 3. Side-by-side: "plasma" in all five

All five express the plasma-like effect from the existing
`18-pixel-iteration.cs` demo.

**A. Shader-style**

```glsl
pixel(x, y, t) =
    let v = sin(x*0.5 + t) + sin(y*0.5 + t) + sin((x+y)*0.3 + t)
              + sin(sqrt(x*x + y*y) * 0.3)
    hsv((v + 4) / 8, 1, 0.5)
```

**B. Turtle** — not a natural fit. Turtle works for paths; plasma is a
field. Would be forced.

**C. Livecoding / dataflow**

```
osc(10, 0.1, 1).add(osc(10, 0.1, 1).rotate(1.57))
               .add(noise(3, 0.1))
               .colorize(rainbow)
               .out()
```

**D. Array-oriented**

```
let grid = map(cells, (x, y) ->
    hsv((sin(x*0.5 + t) + sin(y*0.5 + t)
         + sin((x+y)*0.3 + t) + sin(hypot(x,y)*0.3) + 4) / 8, 1, 0.5))
render grid
```

**E. Cellular** — not the natural fit for plasma either; plasma is
computed from `(x, y, t)`, not from neighbours.

---

## 4. Side-by-side: "seconds ring" (the Around-The-Clock demo) in each

This is where the strengths flip.

**A. Shader-style** — awkward. Ring is a condition on `(x, y)`; you'd write
a big `if` nested in the pixel function.

**B. Turtle** — natural.

```
for s in 0..now.second:
    turtle goto(outerRing[s])
    turtle plot(hsv(190, 1, fade(s)))
```

**C. Livecoding / dataflow** — natural once you have a "ring" generator.

```
ring(radius=11, count=60, upTo=now.second)
  .colorize(cyanFade)
  .out()
```

**D. Array-oriented** — natural via masks, though position-to-index
mapping is a bit fiddly.

**E. Cellular** — not a fit at all; this is a direct draw, not an emergent
one.

---

## 5. Recommendation

No single alternative wins across the board — each one is *exactly right*
for some effects and wrong for others. The honest conclusion is that PXL
Clock benefits from **two complementary surfaces**:

- A **shader-style `pixel(x, y, t) -> color`** mode for per-pixel and
  field-based effects (plasma, gradients, noise, fractals). This is the
  sweet spot for AI authoring: tiny function, massive training corpus,
  nothing to get syntactically wrong.
- A **turtle / direct-draw** mode for path-based effects (clock rings,
  trails, text, cursors). This is the sweet spot for beginners and for
  anything where the structure is "a list of things in specific places."

The other three (dataflow, array, cellular) are each worth a stand-alone
experiment as *third-party* modes, but don't belong in the core.

**If I had to pick one to prototype first: A (shader-style).** It matches
more of the existing demos than any other mode, has the cleanest mapping to
the 24×24×40 FPS cadence, and is the single strongest fit for LLM
authorship. Turtle (B) would be the obvious second mode to add once A
exists.

---

## Sources

- [Shadertoy — How To](https://www.shadertoy.com/howto)
- [Shadertoy — Wikipedia](https://en.wikipedia.org/wiki/Shadertoy)
- [Hydra Visual Synth — Strudel integration](https://strudel.cc/learn/hydra/)
- [TidalCycles](https://tidalcycles.org/)
- [BQN: an APL for your flying saucer](https://mlochbaum.github.io/BQN/)
- [Why use BQN?](https://mlochbaum.github.io/BQN/commentary/why.html)
- [PICO-8 — Wikipedia](https://en.wikipedia.org/wiki/PICO-8)
- [TIC-80 — Wikipedia](https://en.wikipedia.org/wiki/TIC-80)
- [Logo (programming language) — Wikipedia](https://en.wikipedia.org/wiki/Logo_(programming_language))
