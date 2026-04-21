# TypeFighter — Syntax Alternatives (Exploration)

This file is a **note**. Nothing here compiles, nothing is wired up. It's a
brainstorm of five genuinely different concrete syntaxes we could put on top of
the existing TypeFighter inference core (HM + row-polymorphic records,
AST: `Lit / Var / App / Fun / Let / Do / PropAcc / MkArray / MkRecord`).

Goal: get out of the ML/JS comfort zone and look at what other language
families do. Each alternative is shown with the **same small vocabulary** so
they can be compared directly:

- binding: `name = expr` / `let name = expr` / …
- call: `f` applied to one or two arguments
- record: `{ age: 22, name: "John" }`
- property: `user.name`
- arrow: single- and two-parameter lambda
- HOF: define a `map`-style function and use it
- control flow: one `if` / `then` / `else` form
- realistic: *"sum of doubled numbers"*, ~5 lines

Every alternative ends with a note on how it **interacts with the inference
core** — what rows and HM like or dislike about it.

Each section uses the closest matching syntax highlighter (Python, VB, Elixir,
Smalltalk, Factor) so the snippets render with approximately correct colouring;
keywords and strings will highlight, but the parsers are fictional, so expect
the odd operator to look "off".

The five alternatives:

1. [Pythonesque / off-side rule](#a-pythonesque--off-side-rule)
2. [BASIC revived](#b-basic-revived)
3. [Pipeline-first (Elixir / Elm flavour)](#c-pipeline-first-elixir--elm-flavour)
4. [Smalltalk keyword messages](#d-smalltalk-inspired-keyword-messages)
5. [Concatenative / stack (Forth / Factor flavour)](#e-concatenative--stack-forth--factor-flavour)

---

## A. Pythonesque / off-side rule

**Inspirations:** Python, Nim, F# light, Koka, a pinch of CoffeeScript.
**Feel:** mainstream, friendly, zero visual noise. Blocks open with `:` and
close by dedent. No braces anywhere.

### Primitives

```python
x = 42                              # binding (no keyword, Python-like)
name = "Ada"

f(1)                                # call
add(1, 2)

user = { "age": 22, "name": "John" }    # record
user.name                               # property

square = lambda x: x * x                # single-parameter arrow
plus   = lambda x, y: x + y             # two-parameter arrow

def map(f, xs):                         # named function definition
    pass                                # body indented

if n > 0:
    n
else:
    -n
```

### "Sum of doubled numbers"

```python
data    = [1, 2, 3]
doubled = map(lambda x: x * 2, data)
total   = sum(doubled)
total
```

### Longer demo — record + branch

```python
def greet(user):
    if user.age > 18:
        return "Welcome " + user.name
    else:
        return "Come back later"

ada = { "name": "Ada", "age": 36 }
greet(ada)
```

### Inference notes

- Very good fit. `x = expr` maps straight to `Let`. Indentation gives
  unambiguous `Do`-chains: every non-last statement in a block becomes a
  `Do` node, last one is the result.
- Big win for error messages: dedent boundaries are natural "commit" points
  for a future `pcommit` combinator.
- **Lexer pain:** has to emit virtual INDENT/DEDENT tokens. That's a new
  concept for TheBlunt — today it only sees characters. Feasible but
  non-trivial.
- Row polymorphism works as-is: `user.age` is still a `PropAcc`.

---

## B. BASIC revived

**Inspirations:** classic BASIC, Visual Basic, Modula-2, Pascal.
**Feel:** line-oriented, loud keywords, every block terminated by an explicit
`END`. Extremely approachable for non-programmers; zero ambiguity for the
parser.

### Primitives

```vbnet
LET x = 42                      ' binding
LET name = "Ada"

CALL f(1)                       ' call-as-statement
LET y = f(1)                    ' call-as-expression

LET user = RECORD               ' record literal spelled out
    age  = 22
    name = "John"
END RECORD

LET n = user.name               ' property access unchanged

FUNCTION square(x) : RETURN x * x : END FUNCTION
FUNCTION plus(x, y) : RETURN x + y : END FUNCTION

FUNCTION map(f, xs)             ' block-bodied function
    ' body
END FUNCTION

IF n > 0 THEN
    n
ELSE
    0 - n
END IF
```

### "Sum of doubled numbers"

```vbnet
LET data    = [1, 2, 3]
LET doubled = MAP(FUNCTION(x) RETURN x * 2 END FUNCTION, data)
LET total   = SUM(doubled)
total
```

### Longer demo

```vbnet
FUNCTION greet(user)
    IF user.age > 18 THEN
        RETURN "Welcome " + user.name
    ELSE
        RETURN "Come back later"
    END IF
END FUNCTION

LET ada = RECORD name = "Ada", age = 36 END RECORD
greet(ada)
```

### Inference notes

- Parser heaven. Every construct is bracketed by keywords, so there is never
  a "where does this end?" question — perfect target for commit-on-keyword
  error recovery.
- Uppercase keywords are easy to reserve without colliding with identifier
  conventions (lowercase camelCase).
- **Block-vs-expression tension:** `IF ... END IF` as an expression is fine
  (Algol-68 did it). But we have to decide: is `FUNCTION f ... END FUNCTION`
  a value or just a statement that binds a name?
- Zero impact on HM/rows — the semantics are the same as A, only the
  wrapping differs.

---

## C. Pipeline-first (Elixir / Elm flavour)

**Inspirations:** Elixir, Elm, F# pipeline style, Unix shell.
**Feel:** data flows **left-to-right**. `|>` is the spine; functions come
second, data comes first. Pattern matching optional but idiomatic.

### Primitives

```elixir
x = 42                              # no keyword, like A
name = "Ada"

f.(1)                               # call still familiar
1 |> f.()                           # or pipe-style

user = %{age: 22, name: "John"}     # record
user.name                           # property

square = fn x -> x * x end          # `fn ... -> ... end`
plus   = fn x, y -> x + y end

map = fn f, xs ->                   # multi-clause body
    Enum.map(xs, f)
end

case n do
    0 -> "zero"
    _ -> "nonzero"
end
```

### "Sum of doubled numbers"

```elixir
[1, 2, 3]
|> Enum.map(fn x -> x * 2 end)
|> Enum.sum()
```

### Longer demo — pipelines + records

```elixir
greet = fn user ->
    "Hello " <> user.name
end

[
    %{name: "Ada", age: 36},
    %{name: "Ben", age: 12},
]
|> Enum.filter(fn u -> u.age > 18 end)
|> Enum.map(greet)
|> Enum.join(", ")
```

### Inference notes

- The pipe `x |> f` is just `f x` in reverse — desugars to `App` with
  arguments swapped. Zero cost for HM.
- **Partial application is super useful here:** `|> map(fn x -> ...)` needs
  `map(f)` to be "the function that takes `xs` and maps". Our AST has only
  unary `Fun` / `App`, so this is trivially a left fold of `App`s — already
  how we curry today.
- Row polymorphism is a natural match because pipelines expose property
  access inline: `users |> map(.name)` could desugar to
  `map(fn u -> u.name)` — literal gold for row inference, it forces a
  "has a `.name` field" constraint on the element type.
- **Downside:** pipelines read less well when the computation branches.
  Imperative `Do` chains feel awkward; we'd likely allow a block
  `do ... end` for the occasional side-effect.

---

## D. Smalltalk-inspired keyword messages

**Inspirations:** Smalltalk-80, Self, Pharo, Io. Keyword-message calls read
like English sentences; operators are just unary or binary messages sent to an
object.
**Feel:** very foreign at first, extremely regular once it clicks. Every
interaction with a value is a message send.

### Primitives

```smalltalk
x := 42.                            "binding uses `:=`"
name := 'Ada'.                      "strings in single quotes"

f value: 1.                         "unary-style call"
add first: 1 second: 2.             "keyword call (curried)"

user := Dictionary new.             "record-ish; fields via message"
user at: #age  put: 22;
     at: #name put: 'John'.
user name.                          "property access is a message"

square := [ :x | x * x ].           "block (lambda) in brackets"
plus   := [ :x :y | x + y ].

map := [ :f :xs | xs collect: f ].

n > 0
    ifTrue:  [ n ]
    ifFalse: [ n negated ].
```

### "Sum of doubled numbers"

```smalltalk
data    := #(1 2 3).
doubled := data collect: [ :x | x * 2 ].
doubled sum.
```

### Longer demo — cascaded messages (`;`)

```smalltalk
greeter := [ :user |
    user age > 18
        ifTrue:  [ 'Welcome ', user name ]
        ifFalse: [ 'Come back later' ] ].

users := (OrderedCollection new)
    add: (Dictionary new at: #name put: 'Ada'; at: #age put: 36; yourself);
    add: (Dictionary new at: #name put: 'Ben'; at: #age put: 12; yourself);
    yourself.

users collect: greeter.
```

### Inference notes

- Property access `user name` being a message is conceptually beautiful for
  row polymorphism: *"send `name`"* == *"row has field `name`"*. Exactly our
  `PropAcc` semantics.
- Keyword messages are multi-argument at the syntactic level, but map to
  curried `App`-chains the same way JS calls do.
- **Identifiers-as-messages blur the line between "variable" and "method".**
  Need to decide whether `user age` looks up a field or resolves to a
  standalone function. Simplest: treat bare identifier after receiver as
  `PropAcc`; use `f (user)` when `f` is free-standing.
- Big win for readability of the AST: the surface mirrors the tree almost 1:1.

---

## E. Concatenative / stack (Forth / Factor flavour)

**Inspirations:** Forth, Factor, Joy, Cat, Kitten. Programs are a **sequence
of words** that push/pop values on an implicit stack. Quotations `[ ... ]` are
first-class code values.
**Feel:** point-free / tacit, terse, takes getting used to, but composition
is function composition.

### Primitives

```factor
! binding: push, name, "set"
42 \ x set
"Ada" \ name set

! call: push args, apply word
1 f
1 2 add

! record built from stack; `@` = get field off receiver
H{ { "age" 22 } { "name" "John" } } \ user set
user "name" @

! define words
: square ( x -- x' ) dup * ;
: plus   ( x y -- z ) + ;

: map    ( xs quot -- ys ) ... ;

! if/else: takes two quotations
0 > [ "positive" ] [ "non-positive" ] if
```

### "Sum of doubled numbers"

```factor
{ 1 2 3 } [ 2 * ] map sum
```

### Longer demo

```factor
: greet ( user -- msg )
    dup "age" @ 18 >
    [ "Welcome " swap "name" @ append ]
    [ drop "Come back later" ]
    if ;

H{ { "name" "Ada" } { "age" 36 } } greet
```

### Inference notes

- This is the most radical alternative. Stack-based languages are typed via
  **stack effects:** every word has a signature `( in1 in2 -- out1 )`. *That
  is row polymorphism*, just on a stack row instead of a record row.
  TypeFighter's row types would map astonishingly well: the rest-of-stack is
  an open row variable, exactly like the rest-of-record today.
- **But:** our current AST assumes unary `App`. A concatenative front-end
  either (a) compiles away the stack by allocating a fresh AST variable per
  stack position, or (b) extends the AST with a new `Sequence` node and
  tracks stack types during inference.
- Zero need for `Let` bindings in idiomatic code — `dup`, `swap`, `over`
  replace names. This is a huge philosophical departure and probably too far
  for a first experiment, but worth listing because it highlights what
  row-polymorphism can do beyond records.

---

## Side-by-side: the same tiny program in all five

> *"Double everything in the list and sum."*

**A. Pythonesque**

```python
data = [1, 2, 3]
total = sum(map(lambda x: x * 2, data))
total
```

**B. BASIC**

```vbnet
LET data  = [1, 2, 3]
LET total = SUM(MAP(FUNCTION(x) RETURN x * 2 END FUNCTION, data))
total
```

**C. Pipeline-first**

```elixir
[1, 2, 3]
|> Enum.map(fn x -> x * 2 end)
|> Enum.sum()
```

**D. Smalltalk**

```smalltalk
(#(1 2 3) collect: [ :x | x * 2 ]) sum.
```

**E. Concatenative**

```factor
{ 1 2 3 } [ 2 * ] map sum
```

---

## Closing thoughts — which to try first?

- **Minimal parser risk → B (BASIC).** Blocks are explicit, no INDENT
  tokens, zero ambiguity, every keyword is a commit point.
- **Maximum reward from row polymorphism → C (Pipeline) or D (Smalltalk).**
  Both make `PropAcc` feel like a first-class operation in the surface,
  which is exactly what row inference rewards.
- **Makes TypeFighter feel unique → A (Pythonesque with inline pipes) or E
  (Concatenative).** A is a mainstream-feeling twist; E is the research bet
  — it would force the inference core to handle stack rows in addition to
  record rows, and that is a genuinely novel combination.

None of this locks us in. The inference core doesn't care about concrete
syntax; it sees `Expr<'tv>`. A sensible next step would be to pick **one** and
build it side-by-side with the current JS-flavoured `Parser01`, so we can feel
the difference before committing.
