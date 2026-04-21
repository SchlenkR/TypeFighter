# Syntax for AI and Beginners — Research Notes

Companion to [SyntaxAlternatives.md](SyntaxAlternatives.md). This note
summarises what current research and practice say about which syntactic
properties serve **LLM code generation** and **novice programmers**, and then
re-ranks the five alternatives through that lens.

## 1. What helps LLMs produce correct code

### What hurts
- **Significant whitespace.** Python-style indentation forces the model to
  track an implicit stack; token-level probabilistic generation means the
  model occasionally emits the wrong number of spaces and breaks the block
  structure. Multiple studies call this out as a recurring failure mode of
  LLM Python output.
- **Hidden context.** Languages with lots of implicit scope (globals, ambient
  `this`, dynamic dispatch) force the model to guess what's in scope. Guesses
  fail silently.
- **Many ways to do one thing.** High-variance training data (Perl, Ruby)
  pushes the model to average across idioms and produce plausible-but-wrong
  code.

### What helps
- **Explicit block delimiters** (`{ }`, `end`, keyword-terminated blocks).
  Braces or `END` are single tokens — stable under the model's token-by-token
  generation.
- **Strong, fast compiler feedback.** Rust is the current poster child: the
  borrow checker and rich error messages give an LLM a tight correction loop
  when it's wrong. The language *forces* explicit `Result` handling, which
  removes a whole class of "forgot the error path" bugs.
- **Uniform conventions** (rustfmt, gofmt, Prettier). A formatter that
  collapses the training-data variance sharpens the model's output.
- **Explicit types at the boundary.** Types are a specification the model can
  check its guesses against; type errors become a correction signal.
- **Verbose, boring, linear code.** Stack Overflow's recent guidance for
  AI-consumable code is literally *"be simple, explicit, and boring."*
  Clever tricks cost the model accuracy.

### What's in tension
- There's an active line of research on **AI-oriented grammar** — grammars
  designed for *minimum token count* rather than human readability. For
  human-coauthored code that's the wrong tradeoff; for AI-internal
  representations it's worth knowing about.
- The MultiPL-E benchmark shows LLMs do best on **JavaScript / TypeScript /
  C++ / Scala / Python**, roughly in that order. This is largely a function
  of training-data volume, not syntax quality — the rankings move with the
  training corpus, not with language design.

## 2. What helps beginners

### What hurts
- **C-family punctuation density** (`int main(int argc, char **argv)`) dumps
  a lot of ceremony on day one.
- **Pointer / memory ceremony** (C, C++, early Rust) loads concepts beginners
  don't yet have.
- **Significant whitespace** cuts both ways here: it *reduces* syntax errors
  (no missing brace) but *introduces* invisible errors (mixed tabs/spaces).
- **Many correct ways to do the same thing.** Novices get lost choosing.

### What helps
- **English-like keywords** (`if`, `then`, `else`, `while`, `return`). The
  original selling point of BASIC; still the reason Python wins beginner
  rankings year after year.
- **Immediate feedback.** JavaScript's "refresh the browser and see it
  work" loop is cited as the single biggest accelerator for novices.
- **One obvious way.** Languages with strong style norms (Python, Go) reduce
  cognitive load by removing style decisions.
- **Block-based crutches** (Scratch, Snap!, Blockly). Zero syntax errors;
  good for concepts-first teaching. Not suitable as a production target but
  a relevant data point: *the biggest win is eliminating syntax errors as a
  class*.
- **Inferred types with good errors.** Beginners don't want to write
  annotations, but they *do* want the system to tell them clearly when
  something's wrong. This is TypeFighter's sweet spot.

### Cognitive load research (educational psychology)
- The consistent finding across 15+ years: programming load for novices is
  *syntax + semantics + problem-solving simultaneously*. Anything the
  language eliminates from the syntax layer frees cognitive budget for the
  other two.
- Block-based environments show measurable reductions in load — the lesson
  is not "use blocks" but "the syntax layer is the one most worth
  simplifying, because beginners don't yet have schemas for it."

## 3. Where AI and beginners agree

These are the properties **both** audiences want:

| Property                              | Why beginners like it        | Why LLMs like it              |
| ------------------------------------- | ---------------------------- | ----------------------------- |
| Explicit block delimiters             | No invisible state to track  | Single-token structure        |
| One obvious way to do a thing         | Less decision fatigue        | Less training-data variance   |
| Readable, English-like keywords       | Lower vocabulary barrier     | Strong prior from NL training |
| Inferred types + clear type errors    | No annotation burden + good correction signal | Types = spec to self-check |
| Fast, localised error messages        | Shortens learning loop       | Shortens correction loop      |
| Pattern matching with literal cases   | Reads like a spec            | Easy to generate, hard to get syntactically wrong |

## 4. Where they disagree

| Property                     | Beginners               | LLMs                     |
| ---------------------------- | ----------------------- | ------------------------ |
| Significant whitespace       | Slight plus             | Minus                    |
| Extreme terseness (APL, J)   | Minus                   | Minus (low data volume)  |
| Explicit, verbose annotation | Minus (ceremony)        | Plus (more structure)    |
| Many idioms / macros         | Minus (which to pick?)  | Minus (variance)         |
| Long, English-like keywords  | Plus                    | Neutral-to-plus          |

The most interesting tension is **annotation**. Beginners benefit from *not*
writing types; LLMs benefit from types being *somewhere*. TypeFighter's
answer — full HM inference with row polymorphism — is a legitimate
resolution: **the types exist, the programmer just doesn't type them**. The
*error messages* then have to do the work that annotations would normally do.

## 5. Re-ranking the five alternatives

Scoring `++ / + / 0 / - / --` purely on the criteria above, setting aside
what we personally like:

| Alternative        | LLM score | Beginner score | Notes                                              |
| ------------------ | --------- | -------------- | -------------------------------------------------- |
| A. Pythonesque     | 0         | +              | Great for humans; whitespace is an LLM liability.  |
| B. BASIC revived   | ++        | ++             | Explicit blocks, loud keywords. Both win.          |
| C. Pipeline-first  | +         | +              | Pipes read naturally; `do/end` keeps LLMs happy.   |
| D. Smalltalk       | -         | -              | Unfamiliar; little training data; keyword-message syntax is a high novice barrier. |
| E. Concatenative   | --        | --             | Tiniest training corpus; hardest mental model.     |

**The clear sweet-spot candidate is B (BASIC revived).** It's the one that
the research and practice literature independently recommend for *both*
audiences, for the same underlying reason: explicit keyword-bracketed
structure is the single feature that helps beginners *and* LLMs the most.

**Strong runner-up: C (Pipeline-first with `do/end` blocks).** Pipelines are
a demonstrably good teaching tool (data flow is visible) and LLMs handle
them well when the arrow/pipe tokens are unambiguous.

## 6. Implications for TypeFighter

If we want TypeFighter to be *both* a good first language *and* a good
language for AI pair-programming, the design bets are:

1. **Keyword-terminated blocks**, not indentation. `if … then … else … end`,
   `fun x = … end`, `let x = …` (no block needed). Pay the extra tokens;
   gain robustness.
2. **HM inference everywhere**, but invest heavily in **error messages**.
   Every inference failure should name the row field / function argument
   that caused it, in plain language. Beginners learn from these; LLMs
   self-correct from these.
3. **One obvious way** for each construct. Don't expose a short form *and*
   a block form of the same thing. Pick one.
4. **Row polymorphism exposed via property access**, so "get the name of a
   user" and "accept anything that has a `name`" look the same. This is
   unusually friendly for both audiences: beginners think
   structurally ("it has a name"), LLMs infer structurally.
5. **An autoformatter from day one.** Ship the formatter with the parser.
   This is the single cheapest way to collapse the variance in
   AI-generated code.
6. **A REPL / live sandbox.** The JavaScript-style instant-feedback loop
   is what beginners actually respond to — and it's also what gives an LLM
   agent a tight validation signal.

## Sources

- [Where Do LLMs Still Struggle? An In-Depth Analysis of Code Generation Benchmarks (arXiv 2511.04355)](https://arxiv.org/html/2511.04355v1)
- [What's Wrong with Your Code Generated by Large Language Models? An Extensive Study (arXiv 2407.06153)](https://arxiv.org/html/2407.06153v1)
- [MultiPL-E: A Scalable and Polyglot Approach to Benchmarking Neural Code Generation](https://nuprl.github.io/MultiPL-E/)
- [AI Coders Are Among Us: Rethinking Programming Language Grammar Towards Efficient Code Generation (arXiv 2404.16333)](https://arxiv.org/html/2404.16333v1)
- [Choosing Rust for LLM-Generated Code (RunMat)](https://runmat.org/blog/why-rust)
- [Building shared coding guidelines for AI (and people too) — Stack Overflow](https://stackoverflow.blog/2026/03/26/coding-guidelines-for-ai-agents-and-people-too/)
- [Managing Cognitive Load in Introductory Programming Courses (ACM/SAGE)](https://journals.sagepub.com/doi/abs/10.3233/jid-2013-0004)
- [How block-based programming supports novice learners' coding comprehension (ScienceDirect, 2025)](https://www.sciencedirect.com/science/article/abs/pii/S0360131525001988?dgcid=rss_sd_all)
- [The effect of embedded structures on cognitive load for novice learners (Int. J. STEM Education)](https://link.springer.com/article/10.1186/s40594-023-00432-9)
- [Best Programming Language to Learn First in 2025 — WeCloudData](https://weclouddata.com/blog/learning-guide/best-programming-language-to-learn-first/)
- [Best Programming Languages To Learn In 2025 — Zero To Mastery](https://zerotomastery.io/blog/best-programming-languages-to-learn/)
