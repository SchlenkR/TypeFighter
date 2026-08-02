module PureState.TypeFighter.Parser01.Syntax

open TheBlunt
open PureState.TypeFighter


// =================================================================
// Source spans — used by IntelliSense-style features in the host.
// -----------------------------------------------------------------
// The Expr<_> AST deliberately does not carry source ranges (keeps
// the type system decoupled from the concrete syntax). To still give
// tooling enough to work with, the parser records a flat side-channel
// of Spans during parsing: each identifier reference, binding name,
// lambda parameter, and property field gets a `Span` with its source
// range. `parseWithSpans` hands the collected list back.
//
// FUTURE: if we later want hover types for arbitrary sub-expressions
// (not just identifiers), we'll need to attach ranges to every AST
// node. The least invasive shape is probably a sibling `RangeTree`
// built in lockstep with the Expr, walked alongside the solver's
// numbered AST to produce `Map<VarNum, Range>`. That requires a
// walker inside the TypeFighter assembly (Expr constructors are
// internal) but still leaves `Lang.fs` untouched.
//
// NOTE on dedup: `<|>` backtracks on failure, so a failed branch may
// have already pushed spans before bailing. The collector therefore
// dedupes by (startIdx, endIdx, kind) at the end — the successful
// branch's spans dominate because they're the ones the AST reflects.
// =================================================================

type SpanKind =
    | VarRef of string      // usage of an identifier in expression position
    | BindingDef of string  // the name in `let NAME = …`
    | FunParam of string    // lambda parameter name
    | PropField of string   // field name in `expr.FIELD`

type Span = { range: Range; kind: SpanKind }

let private spanCollector = ResizeArray<Span>()
let private recordSpan (range: Range) (kind: SpanKind) =
    spanCollector.Add { range = range; kind = kind }

// Side-map of Expr<unit> -> Range, keyed by reference identity. Filled
// by `withRange` during parsing; consumed by callers via the returned
// `IReadOnlyDictionary` (see `parseWithSpansAndRanges`). Reference
// identity works because F# DUs are reference types, so each
// `X.SomeConstructor ...` allocation produces a distinct instance.
let private exprRangeDict = System.Collections.Generic.Dictionary<obj, Range>(HashIdentity.Reference)
let private withRange (range: Range) (expr: Expr<unit>) : Expr<unit> =
    exprRangeDict.[box expr] <- range
    expr

// =================================================================
// JS-oriented concrete syntax for TypeFighter
// -----------------------------------------------------------------
// A small recursive-descent parser built on TheBlunt combinators.
// No type annotations — syntax only. Supported forms:
//
//   literals:     42    "hello"    true    false
//   identifiers:  foo
//   arrow fns:    x => body         (x, y) => body
//   calls:        f(x)              f(x, y)
//   prop access:  obj.field
//   arrays:       [ 1, 2, 3 ]
//   records:      { age: 22, name: "John" }
//   parens:       ( expr )
//   bindings:     let x = expr;     const x = expr;
//   programs:     stmt; stmt; resultExpr
//
// The result is a TypeFighter Expr<unit> that can be fed into the
// solver just like expressions built with the X DSL.
// =================================================================


// =================================================================
// TODO — open design questions to revisit
// -----------------------------------------------------------------
// 1) Trivia handling.
//
//    "Trivia" = whitespace, comments, and anything else that sits
//    *between* meaningful tokens but doesn't carry semantic value.
//    TheBlunt has no built-in concept of trivia; today we sprinkle
//    `ws` and `tok` ad-hoc through this file. That works, but:
//
//      • It's noisy and easy to forget at a single call site.
//      • There is no support for line/block comments yet.
//      • Trivia is silently dropped — we keep no record of it on
//        the AST, so we can't round-trip source for tooling
//        (formatter, refactoring, hover/quick-info) later.
//
//    Discuss:
//      a) Push trivia into TheBlunt itself (a `ws` slot every
//         combinator skips automatically), or keep it explicit
//         here?  The implicit version is convenient but couples
//         the combinator library to one notion of whitespace.
//      b) If we ever want lossless ASTs, we need to *attach*
//         leading/trailing trivia to each node instead of dropping
//         it. That's a bigger model change (ranges grow into
//         "leadingTrivia + token + trailingTrivia").
//      c) Comments: `// line` and `/* block */` would be the JS
//         baseline. Block comments raise the question of nesting,
//         which TheBlunt's flat parsing model doesn't model
//         elegantly today.
//
// 2) The Range relation / "what range do I return?".
//
//    Subtle TheBlunt quirk: parent combinators (`many`, `andThen`,
//    `bind`, …) advance the cursor based on the *returned*
//    `range.endIdx` of a child parser — NOT on where the child
//    actually walked the cursor internally. If a `parse { … }`
//    block returns a hand-crafted PVal whose range stops short of
//    what was consumed, the outer parser silently re-parses (or
//    fails) at the wrong position. We hit exactly this with
//    `callArgs` / `propTail` initially: bind walked past `)` /
//    past the property name, but the returned range stopped one
//    token earlier, so `eoi` fired in the middle of valid input.
//
//    There is no compiler-checked contract for this anywhere. The
//    rule we discovered is: a parser's returned `range.endIdx`
//    MUST equal the cursor position the parser actually leaves
//    behind (i.e. the endIdx of its last consumed sub-parser).
//
//    Discuss:
//      a) Document this contract in TheBlunt (the unfinished
//         `// TODO: make clear: Parsers that` line at line 372 of
//         TheBlunt.fs hints Ronald started this thought already).
//      b) Better: have the combinator library track the cursor
//         itself, so user code can't get this wrong. The PVal's
//         `range` would then be derived, not asserted.
//      c) Even better for diagnostics: stop using `range.endIdx`
//         as a cursor at all and pass an explicit cursor through
//         every result. That removes a class of bugs at the cost
//         of one extra field on every PVal.
//
// 3) Backtracking & "commit/fatal" errors.
//
//    `<|>` and `pchoice` are unconditionally backtracking — every
//    failure resets to the original position and tries the next
//    alternative. That's convenient for prototyping but means
//    deep-down syntax errors (e.g. a malformed expression inside
//    a record literal) bubble up as the *outer* "no alternative
//    matched" error, with the real cause lost.
//
//    TheBlunt has no `commit` / "fatal" notion today. The TODO at
//    line 267 of TheBlunt.fs ("this propably would be a fatal,
//    most propably an unexpected error") is a related symptom.
//
//    Discuss whether we want a `pcommit` combinator: once a
//    parser passes some "we are definitely in this branch now"
//    point (e.g. saw the `{` of a record literal), subsequent
//    failures are FATAL and propagate past `<|>` instead of
//    triggering backtracking. This would dramatically improve
//    parser error messages but changes the semantics of the
//    combinator algebra.
// =================================================================


// -------- Whitespace & tokens --------

let private spaceChar =
    pchar
        (fun c -> c = ' ' || c = '\t' || c = '\r' || c = '\n')
        (sprintf "Expected whitespace, but got '%c'.")

// Line comment: `//` until end-of-line (or end-of-input). The newline
// itself is left for `spaceChar` to consume in the outer `ws` loop.
let private lineComment : Parser<unit> =
    pstr "//"
    .>> many (pchar (fun c -> c <> '\n') (sprintf "Expected non-newline, but got '%c'."))
    |> pignore

let private ws = many ((spaceChar |> pignore) <|> lineComment) |> pignore

let private tok p = p .>> ws

let private sym (s: string) = pstr s |> tok

/// Match a literal string that must NOT run into an identifier char.
/// Used for keywords (`let`, `true`, …).
let private keyword (s: string) =
    parse {
        let! k = pstr s
        let! _ = pnot (letter <|> digit <|> pstr "_")
        return k
    } |> tok


// -------- Identifiers --------

let private reservedWords =
    set [ "let"; "const"; "true"; "false"; "return"; "if"; "then"; "else" ]

let private identStart = letter <|> pstr "_"
let private identCont  = letter <|> digit <|> pstr "_"

// Backtick-quoted identifier: `any chars except backtick`. Lets users name
// bindings/fields with spaces or characters the plain rule rejects (e.g.
// `Field Name with Space`). Reserved-word check is skipped for these.
let private quotedIdentifier =
    parse {
        let! q1 = pstr "`"
        let! body = pstringUntil (pstr "`")
        let! q2 = pstr "`"
        return
            { range = Range.add q1.range q2.range
              result = body.result }
    } |> tok

let private plainIdentifier =
    let raw =
        parse {
            let! h = identStart
            let! t = many identCont |> pconcat
            return
                { range = Range.add h.range t.range
                  result = h.result + t.result }
        }
    parse {
        let! x = raw
        if reservedWords.Contains x.result then
            return
                { idx = x.range.startIdx
                  message = $"Reserved word '{x.result}' used as identifier." }
        else
            return x
    } |> tok

let private identifier = quotedIdentifier <|> plainIdentifier


// -------- Literals --------

// Floats must be tried before the integer fallback: otherwise `3.14`
// would parse as the int `3` and leave `.14` to confuse the call-chain
// rule. `<|>` backtracks on failure so committing to the fractional
// branch is safe.
let private floatLit =
    parse {
        let! intPart  = many1Str digit
        let! dot      = pstr "."
        let! fracPart = many1Str digit
        let r = Range.merge [ intPart.range; dot.range; fracPart.range ]
        return
            { range = r
              result = withRange r (X.Lit (float (intPart.result + "." + fracPart.result))) }
    }

let private intLit =
    parse {
        let! n = many1Str digit
        return { range = n.range; result = withRange n.range (X.Lit (int n.result)) }
    }

let private numberLit = (floatLit <|> intLit) |> tok

let private stringLit =
    parse {
        let! openQ = pstr "\""
        let! body  = pstringUntil (pstr "\"")
        let! closeQ = pstr "\""
        let r = Range.merge [ openQ.range; body.range; closeQ.range ]
        return { range = r; result = withRange r (X.Lit body.result) }
    } |> tok

let private boolLit =
    (parse {
        let! kw = keyword "true"
        return { range = kw.range; result = withRange kw.range (X.Lit true) }
     })
    <|>
    (parse {
        let! kw = keyword "false"
        return { range = kw.range; result = withRange kw.range (X.Lit false) }
     })


// -------- Forward-declared expression parser --------
//
// Several of the parsers below (arrays, records, arrows, calls) are
// mutually recursive with `expr`. We use a mutable cell to break the
// cycle: the real implementation is assigned at the bottom of the
// module, after every rule has been constructed.

// `ref` (not `mutable`) is load-bearing here: Fable's optimizer inlines
// the one-and-only initial value of a `mutable` binding into closures that
// close over it, which defeats the forward reference trick and makes the
// emitted JS throw "expr used before initialization." at runtime. A `ref`
// cell is a heap object whose `.contents` is read at call time.
let private exprImpl : Parser<Expr<unit>> ref =
    ref (mkParser (fun _ -> failwith "Parser01.Syntax.expr used before initialization."))

let private expr : Parser<Expr<unit>> =
    mkParser (fun inp -> getParser exprImpl.Value inp)


// -------- Primaries: ( expr ), arrays, records, var --------
// `{ … }` builds records, `( … )` groups one expression. ADR-009.

let inline private commaList (p: Parser<'a>) : Parser<'a list> =
    (psepBy1 (sym ",") p |> map (fun pvs -> pvs |> List.map (fun pv -> pv.result)))
    <|> mkParser (fun inp -> POk.create inp.idx inp.idx [])

let private arrayLit =
    parse {
        let! openB  = sym "["
        let! items  = commaList expr
        let! closeB = sym "]"
        let r = Range.merge [ openB.range; items.range; closeB.range ]
        return { range = r; result = withRange r (X.MkArray items.result) }
    }

// Used in BOTH record literals `{ … }` and call argument lists `( … )`.
// Call args treat a bare ident as a positional, NOT as a shorthand —
// otherwise `f(x)` would parse as a single named-arg call.
let private recordItem =
    let asProperty =
        parse {
            let! name  = identifier
            let! _     = sym ":"
            let! value = expr
            return
                { range = Range.add name.range value.range
                  result = X.Property name.result value.result }
        }
    let asPositional =
        expr |> map X.Positional
    asProperty <|> asPositional

// Used ONLY inside `{ … }`. A bare identifier becomes field shorthand:
// `{ name }` ≡ `{ name: name }` — common ergonomic in JS / Rust / etc.
let private recordLitItem =
    let asProperty =
        parse {
            let! name  = identifier
            let! _     = sym ":"
            let! value = expr
            return
                { range = Range.add name.range value.range
                  result = X.Property name.result value.result }
        }
    let asExprOrShorthand =
        expr
        |> map (fun e ->
            match e with
            | Expr.Var v -> X.Property v.ident (X.Var v.ident)
            | _ -> X.Positional e)
    asProperty <|> asExprOrShorthand

let private recordLit =
    parse {
        let! openB  = sym "{"
        let! items  = commaList recordLitItem
        let! closeB = sym "}"
        let r = Range.merge [ openB.range; items.range; closeB.range ]
        return { range = r; result = withRange r (X.MkRecord items.result) }
    }

let private parenExpr =
    // `()` is the unit value (an empty record), `(x)` groups x.
    parse {
        let! openP = sym "("
        let! inner = commaList expr
        let! closeP = sym ")"
        let range = Range.merge [ openP.range; inner.range; closeP.range ]
        let result =
            match inner.result with
            | []  -> X.MkRecord []
            | [e] -> e
            | _   ->
                // Multiple items inside `(...)` — not a grouping and
                // not (under ADR-009) a record literal. Parse error via
                // primary-level fallback; here we pick the first to let
                // the outer grammar fail cleanly.
                List.head inner.result
        return { range = range; result = result }
    }

let private varExpr =
    identifier |> mapPVal (fun pv ->
        recordSpan pv.range (VarRef pv.result)
        withRange pv.range (X.Var pv.result))

let private primary =
    pchoice
        [
            numberLit
            stringLit
            boolLit
            arrayLit
            recordLit
            parenExpr
            varExpr
        ]


// -------- Arrow functions --------

let private arrow =
    // `x => body`
    let singleParam =
        parse {
            let! x    = identifier
            let! _    = sym "=>"
            let! body = expr
            recordSpan x.range (FunParam x.result)
            let r = Range.add x.range body.range
            return { range = r; result = withRange r (X.Fun (X.Ident x.result) body.result) }
        }
    // `(x, y, …) => body`  — curried as Fun x (Fun y body)
    // We lose per-param ranges with `commaList identifier` (the list
    // strips PVals), so rebuild the argument parser to keep them.
    let multiParam =
        parse {
            let! openP  = sym "("
            let! params' =
                psepBy1 (sym ",") identifier
                <|> mkParser (fun inp -> POk.create inp.idx inp.idx [])
            let! _      = sym ")"
            let! _      = sym "=>"
            let! body   = expr
            params'.result
            |> List.iter (fun pv -> recordSpan pv.range (FunParam pv.result))
            let r = Range.add openP.range body.range
            let curried =
                match params'.result with
                | [] -> withRange r (X.Fun (X.Ident "_") body.result)
                | args ->
                    List.foldBack
                        (fun (pv: PVal<string>) acc ->
                            withRange (Range.add pv.range body.range) (X.Fun (X.Ident pv.result) acc))
                        args
                        body.result
            return { range = r; result = curried }
        }
    singleParam <|> multiParam


// -------- Call / property chains --------
//
// A call-chain is a primary followed by any number of tails:
//     primary tail*
// where each tail is either
//     ( args )         →  App-fold of args
//     . ident          →  PropAcc
//
// Each tail is represented as an `Expr -> Expr` transformer so we can
// fold left over them starting from the primary.

type private CallTail = Expr<unit> -> Expr<unit>

// Call-site `(…)` accepts the same shape as a record primary.
// Named items or a trailing `,` ⇒ one record arg; pure-positional
// lists keep today's curried fold so existing code is unaffected.
let private callArgs : Parser<CallTail> =
    let trailingComma =
        (sym "," |> map (fun _ -> true))
        <|> mkParser (fun inp -> POk.create inp.idx inp.idx false)
    parse {
        let! openP  = sym "("
        let! items  = commaList recordItem
        let! comma  = trailingComma
        let! closeP = sym ")"
        let callRange = Range.merge [ openP.range; items.range; closeP.range ]
        let apply (src: Expr<unit>) =
            let hasNamed =
                items.result |> List.exists (fun i -> i.TryAsPositional.IsNone)
            // Range for an App = the source-fragment that produced it: from
            // the func's start (we don't have func's range here directly, so
            // use the call-site range; combined later when needed).
            match items.result, comma.result, hasNamed with
            | [], _, _ ->
                let unit = withRange callRange (X.Var "UnitValue")
                withRange callRange (X.App src unit)
            | _, true, _
            | _, _, true ->
                let rec' = withRange callRange (X.MkRecord items.result)
                withRange callRange (X.App src rec')
            | xs, false, false ->
                let exprs = xs |> List.choose (fun i -> i.TryAsPositional)
                exprs |> List.fold (fun acc a -> withRange callRange (X.App acc a)) src
        return { range = callRange; result = apply }
    }

let private propTail : Parser<CallTail> =
    parse {
        let! dot  = sym "."
        let! name = identifier
        recordSpan name.range (PropField name.result)
        let r = Range.add dot.range name.range
        return { range = r; result = fun src -> withRange r (X.PropAcc src name.result) }
    }

// `xs[i]` desugars to `at(xs)(i)`. Just another CallTail alongside `(args)`
// and `.field`. The `at` binding lives in the prelude.
let private indexTail : Parser<CallTail> =
    parse {
        let! openB = sym "["
        let! idx   = expr
        let! closeB = sym "]"
        let r = Range.merge [ openB.range; idx.range; closeB.range ]
        return
            { range = r
              result = fun src ->
                let atVar = withRange r (X.Var "at")
                let atSrc = withRange r (X.App atVar src)
                withRange r (X.App atSrc idx.result) }
    }

let private callChain =
    parse {
        let! base' = primary
        let! tails = many (callArgs <|> propTail <|> indexTail)
        let folded =
            tails.result
            |> List.fold (fun acc pvTail -> pvTail.result acc) base'.result
        let finalRange =
            match tails.result with
            | [] -> base'.range
            | _  -> Range.add base'.range tails.range
        return { range = finalRange; result = folded }
    }


// -------- Operator precedence --------
//
// Operators desugar to function calls against `op_*` bindings in the
// prelude. `a + b` becomes `App (App (Var "op_add") a) b`. The prelude
// gives `op_add` a constrained-polymorphic type (T in {Number,String});
// the rest are concrete (Number arithmetic, Boolean logic, comparisons).

// Operator combiners. We don't have direct access to operand ranges via
// `buildExprParser`'s `combine` signature, so we look them up via the
// parser's reference-identity dict and re-tag the synthesized App/Var
// nodes accordingly. Without this, operator-built ASTs would be missing
// ranges and type-error squiggles wouldn't fire on `1 + true` etc.
let private rangeOf (e: Expr<unit>) : Range option =
    match exprRangeDict.TryGetValue(box e) with
    | true, r -> Some r
    | _ -> None

let private mkBin (name: string) : Expr<unit> -> Expr<unit> -> Expr<unit> =
    fun l r ->
        let varNode = X.Var name
        let inner = X.App varNode l
        let outer = X.App inner r
        let combined =
            match rangeOf l, rangeOf r with
            | Some lr, Some rr -> Some (Range.add lr rr)
            | Some r, _ | _, Some r -> Some r
            | _ -> None
        match combined with
        | Some r ->
            withRange r varNode |> ignore
            withRange r inner |> ignore
            withRange r outer |> ignore
        | None -> ()
        outer

let private mkUnary (name: string) : Expr<unit> -> Expr<unit> =
    fun v ->
        let varNode = X.Var name
        let app = X.App varNode v
        match rangeOf v with
        | Some r ->
            withRange r varNode |> ignore
            withRange r app |> ignore
        | None -> ()
        app

let private opTable : Op<Expr<unit>> list list =
    [
        // loosest
        [ Infix ("||", Left, mkBin "op_or") ]
        [ Infix ("&&", Left, mkBin "op_and") ]
        [ Infix ("==", Left, mkBin "op_eq")
          Infix ("!=", Left, mkBin "op_neq") ]
        [ Infix ("<=", Left, mkBin "op_le")
          Infix (">=", Left, mkBin "op_ge")
          Infix ("<",  Left, mkBin "op_lt")
          Infix (">",  Left, mkBin "op_gt") ]
        [ Infix ("+", Left, mkBin "op_add")
          Infix ("-", Left, mkBin "op_sub")
          Prefix ("-", mkUnary "op_neg") ]
        // tightest
        [ Infix ("*", Left, mkBin "op_mul")
          Infix ("/", Left, mkBin "op_div")
          Prefix ("!", mkUnary "op_not") ]
    ]

let private opExpr : Parser<Expr<unit>> =
    buildExprParser (fun s -> sym s |> pignore) callChain opTable


// -------- if/then/else as sugar for the `cond` builtin --------
// `if c then a else b` desugars to `cond(c, a, b)`. The whole construct
// is a regular expression — works in any expr position.

let private ifThenElse : Parser<Expr<unit>> =
    parse {
        let! kwIf  = keyword "if"
        let! cond_ = expr
        let! _     = keyword "then"
        let! thenE = expr
        let! _     = keyword "else"
        let! elseE = expr
        let r = Range.add kwIf.range elseE.range
        let call =
            X.App
                (X.App
                    (X.App (X.Var "cond") cond_.result)
                    thenE.result)
                elseE.result
        return { range = r; result = withRange r call }
    }


// -------- Initialize the forward-declared expression parser --------

exprImpl.Value <- ifThenElse <|> arrow <|> opExpr


// -------- Statements & program --------
//
// A program is a sequence of statements separated by `;` where every
// statement but the last is either a `let`-binding or a plain
// expression. The *final* statement has no trailing `;` and provides
// the program's result.
//
//   let x = 1;            StmtLet  → wraps the rest in `Let x 1 …`
//   expr;                 StmtExpr → wraps the rest in `Do expr …`
//   resultExpr            → the return value of the whole program

type private Stmt =
    | StmtLet  of name: string * value: Expr<unit>
    | StmtExpr of Expr<unit>

let private letStmt =
    // `name = expr` — `let` / `const` are tolerated for backwards-compat
    // but no longer required in surface syntax. The two paths share the
    // body via backtracking on the keyword absence.
    let withKw =
        parse {
            let! kw    = keyword "let" <|> keyword "const"
            let! name  = identifier
            let! _     = sym "="
            let! value = expr
            recordSpan name.range (BindingDef name.result)
            return
                { range = Range.add kw.range value.range
                  result = StmtLet(name.result, value.result) }
        }
    let bare =
        parse {
            let! name  = identifier
            let! _     = sym "="
            let! value = expr
            recordSpan name.range (BindingDef name.result)
            return
                { range = Range.add name.range value.range
                  result = StmtLet(name.result, value.result) }
        }
    withKw <|> bare

let private exprStmt =
    expr |> map StmtExpr

let private stmt = letStmt <|> exprStmt

let private program =
    parse {
        let! _     = ws                                   // leading whitespace
        // Stop the prefix loop from eating the last `stmt;` when the
        // source ends right after it — otherwise `log(x);` at end of
        // file leaves nothing for `final` to parse. Also stop when only
        // more `;` follows so `42;;;` doesn't loop past the first `;`.
        let! prefix = many (stmt .>> sym ";" .>> pnot (eoi <|> pignore (sym ";")))
        let! final = expr
        let! tail  = many (sym ";")                       // tolerate trailing `;`
        let body =
            List.foldBack
                (fun s acc ->
                    match s with
                    | StmtLet(n, v) -> X.Let (X.Ident n) v acc
                    | StmtExpr e    -> X.Do e acc)
                (prefix.result |> List.map (fun pv -> pv.result))
                final.result
        // `tail.range` covers the (possibly empty) trailing `;` run and
        // always ends at the parser cursor — use it as the program's
        // end so `.>> eoi` checks the right position.
        let startIdx =
            match prefix.result with
            | [] -> final.range.startIdx
            | _  -> prefix.range.startIdx
        return
            { range = { startIdx = startIdx; endIdx = tail.range.endIdx }
              result = body }
    }
    .>> eoi


// -------- Type-expression parser --------
//
// Grammar (precedence low → high; `&` binds tighter than `|`):
//
//   typExpr     = altTyp { "|" altTyp }               // top-level: union
//   altTyp      = primTyp { "&" primTyp }             // tighter: intersection  (Step 3 wires this up end-to-end)
//   primTyp     = literalTyp                          // 42, "foo", true, false
//               | identTyp                            // Number, String, MyThing
//               | appliedTyp                          // Array<Number>
//               | "(" typExpr ")"
//               | "{" recordTyp "}"                   // (Step 3)
//
// See docs/design/TypeSyntaxWithSets.md.

let private builtinTypeName (name: string) : MonoTyp option =
    match name with
    | "Number" -> Some BuiltinTypes.number
    | "String" -> Some BuiltinTypes.string
    | "Bool"   -> Some BuiltinTypes.boolean
    | "Unit"   -> Some BuiltinTypes.unit
    | _        -> None

let private typeIdentifier =
    let raw =
        parse {
            let! h = identStart
            let! t = many identCont |> pconcat
            return
                { range = Range.add h.range t.range
                  result = h.result + t.result }
        }
    raw |> tok

let private floatLitTyp =
    parse {
        let! intPart  = many1Str digit
        let! dot      = pstr "."
        let! fracPart = many1Str digit
        return
            { range = Range.merge [ intPart.range; dot.range; fracPart.range ]
              result = LiteralTyp (Number (float (intPart.result + "." + fracPart.result))) }
    }

let private intLitTyp =
    parse {
        let! n = many1Str digit
        return { range = n.range; result = LiteralTyp (Number (float n.result)) }
    }

let private numberLitTyp = (floatLitTyp <|> intLitTyp) |> tok

let private stringLitTyp =
    parse {
        let! openQ = pstr "\""
        let! body  = pstringUntil (pstr "\"")
        let! closeQ = pstr "\""
        return
            { range = Range.merge [ openQ.range; body.range; closeQ.range ]
              result = LiteralTyp (String body.result) }
    } |> tok

let private boolLitTyp =
    (keyword "true"  |> map (fun _ -> LiteralTyp (Boolean true)))
    <|>
    (keyword "false" |> map (fun _ -> LiteralTyp (Boolean false)))

// Forward declaration: typExpr recurses into parens / applied args.
let mutable private typExprImpl : Parser<MonoTyp> =
    mkParser (fun _ -> failwith "Parser01.Syntax.typExpr used before initialization.")

let private typExpr : Parser<MonoTyp> =
    mkParser (fun inp -> getParser typExprImpl inp)

// Forward declaration: primTyp is referenced by `unionOverPrim` (used
// inside `recordTypBraces`) and by `altTyp`, but its real definition
// (which references `recordTypBraces` in turn) comes further below.
let mutable private primTypImpl : Parser<MonoTyp> =
    mkParser (fun _ -> failwith "Parser01.Syntax.primTyp used before initialization.")

let private primTyp : Parser<MonoTyp> =
    mkParser (fun inp -> getParser primTypImpl inp)

// Flatten `A | B | C` into a single `UnionTyp` set (or a single member
// when the set collapses to one element).
let private flattenUnion (ts: MonoTyp list) : MonoTyp =
    let members =
        set [
            for t in ts do
                match t with
                | UnionTyp inner -> yield! inner
                | other          -> yield other
        ]
    match Set.toList members with
    | [single] -> single
    | _        -> UnionTyp members

let private appliedOrIdentTyp =
    // `Name` or `Name< T, U, … >`
    parse {
        let! name = typeIdentifier
        let! args =
            (parse {
                let! openA  = sym "<"
                let! xs     = commaList typExpr
                let! closeA = sym ">"
                return
                    { range = Range.merge [ openA.range; xs.range; closeA.range ]
                      result = xs.result }
             })
            <|> mkParser (fun inp -> POk.create inp.idx inp.idx [])
        let typ =
            match args.result with
            | [] ->
                match builtinTypeName name.result with
                | Some t -> t
                | None   -> TDef.SaturatedWith name.result []
            | args ->
                // `Array<T>` is the idiomatic built-in; user types are also saturated.
                TDef.SaturatedWith name.result args
        return
            { range =
                match args.result with
                | [] -> name.range
                | _  -> Range.add name.range args.range
              result = typ }
    }

// A record-type item inside `{ … }`: `name: T` or a bare `T`.
type private RecTypItem =
    | NamedItem of string * MonoTyp
    | PositionalItem of MonoTyp

// `A | B | C` over primTyp — used at top level and inside record items,
// where `&` is the item separator and can't be consumed by altTyp.
let private unionOverPrim =
    parse {
        let! head = primTyp
        let! tail =
            many (
                parse {
                    let! _ = sym "|"
                    let! t = primTyp
                    return t
                })
        let all = head.result :: [ for pv in tail.result -> pv.result ]
        let finalRange =
            match tail.result with
            | [] -> head.range
            | _  -> Range.add head.range tail.range
        return { range = finalRange; result = flattenUnion all }
    }

let private parenTypExpr =
    parse {
        let! openP = sym "("
        let! t     = typExpr
        let! closeP = sym ")"
        return
            { range = Range.merge [ openP.range; t.range; closeP.range ]
              result = t.result }
    }

// `{ item1 [& item2]* }` — record-set type. Items `&`-separated.
let private recordTypBraces =
    let recordItemTyp =
        let asNamed =
            parse {
                let! name  = typeIdentifier
                let! _     = sym ":"
                let! value = unionOverPrim
                return
                    { range = Range.add name.range value.range
                      result = NamedItem (name.result, value.result) }
            }
        let asPositional =
            unionOverPrim |> map PositionalItem
        asNamed <|> asPositional
    let buildRecord items =
        let named =
            [ for i in items do
                match i with
                | NamedItem (n, t) -> yield n, t
                | _ -> () ]
        let positional =
            [ for i in items do
                match i with
                | PositionalItem t -> yield t
                | _ -> () ]
        TDef.RecordWithItems named positional
    parse {
        let! openB  = sym "{"
        let! items  =
            (psepBy1 (sym "&") recordItemTyp
             |> map (fun pvs -> pvs |> List.map (fun pv -> pv.result)))
            <|> mkParser (fun inp -> POk.create inp.idx inp.idx [])
        let named =
            [ for i in items.result do
                match i with
                | NamedItem (n, _) -> yield n
                | _ -> () ]
        let duplicates =
            named |> List.groupBy id |> List.filter (fun (_, xs) -> List.length xs > 1) |> List.map fst
        let! closeB = sym "}"
        if not (List.isEmpty duplicates) then
            return
                { idx = openB.range.startIdx
                  message = sprintf "Duplicate named fields in record type: %A" duplicates }
        else
            return
                { range = Range.merge [ openB.range; items.range; closeB.range ]
                  result = buildRecord items.result }
    }

// `boolLitTyp` precedes `appliedOrIdentTyp` so `true` / `false` parse
// as literal types, not identifiers.
let private primTypReal : Parser<MonoTyp> =
    numberLitTyp
    <|> stringLitTyp
    <|> boolLitTyp
    <|> recordTypBraces
    <|> parenTypExpr
    <|> appliedOrIdentTyp

do primTypImpl <- primTypReal

// altTyp: `A & B & C` at the TOP level (outside braces).
// All operands must be records; the result is a MERGED record (Step 5).
// The two-row record (named + positional) is exactly what the `&`
// algebra wants: named fields become a set-union (same-name fields
// must agree), positionals concatenate.
let private altTyp : Parser<MonoTyp> =
    parse {
        let! head = primTyp
        let! tail =
            many (
                parse {
                    let! _ = sym "&"
                    let! t = primTyp
                    return t
                })
        match tail.result with
        | [] ->
            return { range = head.range; result = head.result }
        | _ ->
            let all = head.result :: [ for pv in tail.result -> pv.result ]
            let nonRecord =
                all |> List.tryFind (function RecordTyp _ -> false | _ -> true)
            match nonRecord with
            | Some other ->
                return
                    { idx = head.range.startIdx
                      message =
                          sprintf
                              "Type intersection `&` outside `{ … }` requires record operands; got %A. Use `{ A & B }` to build a record-set."
                              other }
            | None ->
                let recordDefs =
                    [ for t in all do
                        match t with
                        | RecordTyp r -> yield r
                        | _ -> () ]
                // Merge: collect all named fields, detect name collisions
                // with incompatible types.
                let allNamed =
                    [ for r in recordDefs do yield! r.fields ]
                let grouped = allNamed |> List.groupBy (fun f -> f.fname)
                let conflict =
                    grouped
                    |> List.tryFind (fun (_, fs) ->
                        fs |> List.map (fun f -> f.typ) |> List.distinct |> List.length > 1)
                match conflict with
                | Some (name, fs) ->
                    return
                        { idx = head.range.startIdx
                          message =
                              sprintf
                                  "Conflicting types for field '%s' in record intersection: %A"
                                  name
                                  (fs |> List.map (fun f -> f.typ)) }
                | None ->
                    let mergedFields =
                        grouped |> List.map (fun (_, fs) -> List.head fs) |> Set.ofList
                    let mergedPositionals =
                        [ for r in recordDefs do yield! r.positionals ]
                    let merged =
                        { fields = mergedFields
                          positionals = mergedPositionals }
                    return
                        { range = Range.add head.range tail.range
                          result = RecordTyp merged }
    }

// typExpr: `A | B | C`. Union of alternatives.
//
// Folding rule: `A | B | C` → `UnionTyp {A, B, C}` (flattened).
// `flattenUnion` is defined earlier (above `unionOverPrim`).

let private typUnion =
    parse {
        let! head = altTyp
        let! tail =
            many (
                parse {
                    let! _ = sym "|"
                    let! t = altTyp
                    return t
                })
        let all = head.result :: [ for pv in tail.result -> pv.result ]
        let finalRange =
            match tail.result with
            | [] -> head.range
            | _  -> Range.add head.range tail.range
        return { range = finalRange; result = flattenUnion all }
    }

do typExprImpl <- typUnion

let private typeProgram =
    parse {
        let! _ = ws
        let! t = typExpr
        return t
    }
    .>> eoi


// -------- Public API --------

type ParseError = { message: string; idx: int; line: int; col: int }

/// Per-Expr-node lookup: returns the source range of the AST node if
/// the parser tracked one. Built once at parse time, queried later by
/// the solver / IDE layer. Reference identity, so copies of the same
/// `Expr<unit>` value won't match.
type ExprRangeLookup = Expr<unit> -> Range option

let private snapshotLookup () : ExprRangeLookup =
    let copy = System.Collections.Generic.Dictionary<obj, Range>(exprRangeDict, HashIdentity.Reference)
    fun e ->
        match copy.TryGetValue(box e) with
        | true, r -> Some r
        | _ -> None

// =================================================================
// Newline-as-statement-separator preprocessor.
// -----------------------------------------------------------------
// The user writes line-based code (no semicolons). This pass inserts
// `;` at significant newlines so the existing `;`-based parser can do
// its job unchanged. A newline is significant when:
//   - bracket depth is 0 (we're not inside (...), [...], {...}),
//   - the previous non-whitespace, non-comment char isn't a continuation
//     marker (operators, `(`, `[`, `{`, `,`, `.`, `:`, `=>`-ending `>`),
//   - the next such char isn't a continuation marker either
//     (operators, `.`).
// Existing `;` are tolerated; we only ADD, never strip.
// =================================================================
let preprocessNewlines (source: string) : string =
    let sb = System.Text.StringBuilder(source.Length + 32)
    let mutable depth = 0
    let mutable inString = false
    let mutable inLineComment = false
    // Initialise to `;` (treated as a stmt-terminator continuation): it
    // means "no open statement to close" — no `;` will be inserted at
    // the very start of input.
    let mutable lastMeaning = ';'

    let isContAfter c =
        match c with
        | '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '&' | '|'
        | ',' | '.' | ':' | '(' | '[' | '{' | ';' -> true
        | _ -> false
    let isContBefore c =
        match c with
        | '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '&' | '|'
        | ',' | '.' -> true
        | _ -> false

    let rec nextMeaningful (i: int) (inLC: bool) : char =
        if i >= source.Length then ' '
        elif inLC then
            if source.[i] = '\n' then nextMeaningful (i + 1) false
            else nextMeaningful (i + 1) true
        elif source.[i] = '/' && i + 1 < source.Length && source.[i + 1] = '/' then
            nextMeaningful (i + 2) true
        elif source.[i] = ' ' || source.[i] = '\t' || source.[i] = '\r' || source.[i] = '\n' then
            nextMeaningful (i + 1) inLC
        else source.[i]

    // `;` insertion at a newline boundary, regardless of whether we just
    // exited a line comment. Updates `lastMeaning` if it inserts.
    let insertSemiAtNewline (idxAfterNewline: int) =
        if depth = 0 then
            let nextC = nextMeaningful idxAfterNewline false
            if nextC <> ' '
                && not (isContAfter lastMeaning)
                && not (isContBefore nextC)
            then
                sb.Append ';' |> ignore
                lastMeaning <- ';'

    let mutable i = 0
    while i < source.Length do
        let c = source.[i]
        if inLineComment then
            sb.Append c |> ignore
            if c = '\n' then
                inLineComment <- false
                // The newline is now in `sb`; insert any synthesised `;`
                // AFTER it so it doesn't get swallowed by the comment.
                insertSemiAtNewline (i + 1)
            i <- i + 1
        elif inString then
            sb.Append c |> ignore
            if c = '"' && (i = 0 || source.[i - 1] <> '\\') then
                inString <- false
            i <- i + 1
        else
            match c with
            | '"' ->
                inString <- true
                sb.Append c |> ignore
                lastMeaning <- '"'
                i <- i + 1
            | '/' when i + 1 < source.Length && source.[i + 1] = '/' ->
                inLineComment <- true
                sb.Append c |> ignore
                i <- i + 1
            | '(' | '[' | '{' ->
                depth <- depth + 1
                sb.Append c |> ignore
                lastMeaning <- c
                i <- i + 1
            | ')' | ']' | '}' ->
                depth <- depth - 1
                sb.Append c |> ignore
                lastMeaning <- c
                i <- i + 1
            | '\n' ->
                sb.Append c |> ignore
                insertSemiAtNewline (i + 1)
                i <- i + 1
            | ' ' | '\t' | '\r' ->
                sb.Append c |> ignore
                i <- i + 1
            | _ ->
                sb.Append c |> ignore
                lastMeaning <- c
                i <- i + 1
    sb.ToString()


// spanCollector/exprRangeDict are module-global mutable state: concurrent
// parses (parallel compile-info/hover requests) corrupt each other. All
// entry points therefore serialize on this gate -- a parse is fast (ms),
// so the lock is harmless.
let private parseGate = obj ()

/// Parse a full JS-like program into a TypeFighter `Expr`.
/// Returns the resulting AST or a structured error with position info.
let parse (source: string) : Result<Expr<unit>, ParseError> =
  lock parseGate (fun () ->
    spanCollector.Clear()
    exprRangeDict.Clear()
    match run source program with
    | POk pval -> Ok pval.result
    | PError err ->
        let pos = DocPos.create err.idx source
        Error { message = err.message; idx = err.idx; line = pos.ln; col = pos.col })

/// Like `parse` but also returns the side-channel span list that tools
/// can use for hover, go-to-definition, and completions. See the
/// comment at the top of this file for the backtracking / dedup story.
let parseWithSpans (source: string) : Result<Expr<unit> * Span list, ParseError> =
  lock parseGate (fun () ->
    spanCollector.Clear()
    exprRangeDict.Clear()
    match run source program with
    | POk pval ->
        let unique =
            spanCollector
            |> Seq.distinctBy (fun s -> s.range.startIdx, s.range.endIdx, s.kind)
            |> List.ofSeq
        Ok (pval.result, unique)
    | PError err ->
        let pos = DocPos.create err.idx source
        Error { message = err.message; idx = err.idx; line = pos.ln; col = pos.col })

/// Like `parseWithSpans` but additionally returns a per-node range lookup.
/// The caller can hand this to the solver to get back source ranges for
/// any AST node (e.g. on a unification error).
let parseWithSpansAndRanges
    (source: string)
    : Result<Expr<unit> * Span list * ExprRangeLookup, ParseError>
    =
  lock parseGate (fun () ->
    spanCollector.Clear()
    exprRangeDict.Clear()
    match run source program with
    | POk pval ->
        let unique =
            spanCollector
            |> Seq.distinctBy (fun s -> s.range.startIdx, s.range.endIdx, s.kind)
            |> List.ofSeq
        Ok (pval.result, unique, snapshotLookup ())
    | PError err ->
        let pos = DocPos.create err.idx source
        Error { message = err.message; idx = err.idx; line = pos.ln; col = pos.col })

/// Parse a TypeFighter type expression.
let parseTyp (source: string) : Result<MonoTyp, string> =
    match run source typeProgram with
    | POk pval -> Ok pval.result
    | PError err ->
        let pos = DocPos.create err.idx source
        Error (sprintf "Type parse error at line %d, column %d: %s" pos.ln pos.col err.message)
