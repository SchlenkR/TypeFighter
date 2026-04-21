module TypeFighter.Parser01.Syntax

open TheBlunt
open TypeFighter

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

let private ws = many spaceChar |> pignore

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
    set [ "let"; "const"; "true"; "false"; "return" ]

let private identStart = letter <|> pstr "_"
let private identCont  = letter <|> digit <|> pstr "_"

let private identifier =
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


// -------- Literals --------

let private intLit =
    parse {
        let! n = many1Str digit
        return { range = n.range; result = X.Lit (int n.result) }
    } |> tok

let private stringLit =
    parse {
        let! openQ = pstr "\""
        let! body  = pstringUntil (pstr "\"")
        let! closeQ = pstr "\""
        return
            { range = Range.merge [ openQ.range; body.range; closeQ.range ]
              result = X.Lit body.result }
    } |> tok

let private boolLit =
    (keyword "true"  |> map (fun _ -> X.Lit true))
    <|>
    (keyword "false" |> map (fun _ -> X.Lit false))


// -------- Forward-declared expression parser --------
//
// Several of the parsers below (arrays, records, arrows, calls) are
// mutually recursive with `expr`. We use a mutable cell to break the
// cycle: the real implementation is assigned at the bottom of the
// module, after every rule has been constructed.

let mutable private exprImpl : Parser<Expr<unit>> =
    mkParser (fun _ -> failwith "Parser01.Syntax.expr used before initialization.")

let private expr : Parser<Expr<unit>> =
    mkParser (fun inp -> getParser exprImpl inp)


// -------- Primaries: ( expr ), arrays, records, var --------

let private parenExpr =
    parse {
        let! openP = sym "("
        let! e     = expr
        let! closeP = sym ")"
        return
            { range = Range.merge [ openP.range; e.range; closeP.range ]
              result = e.result }
    }

let inline private commaList (p: Parser<'a>) : Parser<'a list> =
    (psepBy1 (sym ",") p |> map (fun pvs -> pvs |> List.map (fun pv -> pv.result)))
    <|> mkParser (fun inp -> POk.create inp.idx inp.idx [])

let private arrayLit =
    parse {
        let! openB  = sym "["
        let! items  = commaList expr
        let! closeB = sym "]"
        return
            { range = Range.merge [ openB.range; items.range; closeB.range ]
              result = X.MkArray items.result }
    }

let private recordItem =
    // A record item is either `name: expr` (property) or a bare expr
    // (positional). `Property` is tried first because it commits on the
    // `identifier : ` prefix — if that prefix fails, we backtrack to
    // parse a plain expression.
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

let private recordLit =
    parse {
        let! openB  = sym "{"
        let! items  = commaList recordItem
        let! closeB = sym "}"
        return
            { range = Range.merge [ openB.range; items.range; closeB.range ]
              result = X.MkRecord items.result }
    }

let private varExpr = identifier |> map X.Var

let private primary =
    pchoice
        [
            intLit
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
            return
                { range = Range.add x.range body.range
                  result = X.Fun (X.Ident x.result) body.result }
        }
    // `(x, y, …) => body`  — curried as Fun x (Fun y body)
    let multiParam =
        parse {
            let! openP  = sym "("
            let! params' = commaList identifier
            let! _      = sym ")"
            let! _      = sym "=>"
            let! body   = expr
            let curried =
                match params'.result with
                | [] -> X.Fun (X.Ident "_") body.result     // `() => body` → Fun "_" body
                | args ->
                    List.foldBack
                        (fun arg acc -> X.Fun (X.Ident arg) acc)
                        args
                        body.result
            return
                { range = Range.add openP.range body.range
                  result = curried }
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

let private callArgs : Parser<CallTail> =
    parse {
        let! openP  = sym "("
        let! args   = commaList expr
        let! closeP = sym ")"
        let apply (src: Expr<unit>) =
            match args.result with
            | []   -> X.App src (X.Var "UnitValue")           // f() ≡ f UnitValue
            | xs   -> xs |> List.fold (fun acc a -> X.App acc a) src
        return
            { range = Range.merge [ openP.range; args.range; closeP.range ]
              result = apply }
    }

let private propTail : Parser<CallTail> =
    parse {
        let! dot  = sym "."
        let! name = identifier
        return
            { range = Range.add dot.range name.range
              result = fun src -> X.PropAcc src name.result }
    }

let private callChain =
    parse {
        let! base' = primary
        let! tails = many (callArgs <|> propTail)
        let folded =
            tails.result
            |> List.fold (fun acc pvTail -> pvTail.result acc) base'.result
        let finalRange =
            match tails.result with
            | [] -> base'.range
            | _  -> Range.add base'.range tails.range
        return { range = finalRange; result = folded }
    }


// -------- Initialize the forward-declared expression parser --------

do exprImpl <- arrow <|> callChain


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
    parse {
        let! kw    = keyword "let" <|> keyword "const"
        let! name  = identifier
        let! _     = sym "="
        let! value = expr
        return
            { range = Range.add kw.range value.range
              result = StmtLet(name.result, value.result) }
    }

let private exprStmt =
    expr |> map StmtExpr

let private stmt = letStmt <|> exprStmt

let private program =
    parse {
        let! _     = ws                                   // leading whitespace
        let! prefix = many (stmt .>> sym ";")
        let! final = expr
        let body =
            List.foldBack
                (fun s acc ->
                    match s with
                    | StmtLet(n, v) -> X.Let (X.Ident n) v acc
                    | StmtExpr e    -> X.Do e acc)
                (prefix.result |> List.map (fun pv -> pv.result))
                final.result
        return
            { range =
                (match prefix.result with
                 | [] -> final.range
                 | _  -> Range.add prefix.range final.range)
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

let private numberLitTyp =
    parse {
        let! n = many1Str digit
        return { range = n.range; result = LiteralTyp (Number (float n.result)) }
    } |> tok

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

let private parenTypExpr =
    parse {
        let! openP = sym "("
        let! t     = typExpr
        let! closeP = sym ")"
        return
            { range = Range.merge [ openP.range; t.range; closeP.range ]
              result = t.result }
    }

// A record-type item inside `{ … }` at the type level. Either
// `name: T` (named field) or a bare `T` (positional item).
type private RecTypItem =
    | NamedItem of string * MonoTyp
    | PositionalItem of MonoTyp

// Flatten `A | B | C` over a sub-grammar that starts at `primTyp`.
// Used both for the top-level typExpr and for the "item type" position
// inside `{ … }` — the latter cannot consume `&` (it's the item
// separator), so it goes via primTyp directly, skipping altTyp.
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

// `{ item1 [& item2]* }` — items separated by `&` (serving as the
// record-item conjunction combinator). The TYPE of each item may
// itself contain `|` (alternative slot), but NOT a top-level `&` —
// that's the separator.
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
        // Duplicate-fields check (runs at parse time).
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

// The real primTyp implementation. Order matters: `boolLitTyp` must
// come before `appliedOrIdentTyp` so that `true` / `false` are parsed
// as literal types, not as identifiers. Numbers and strings are fine
// first. Record-set braces precede parens (distinct opening tokens).
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

/// Parse a full JS-like program into a TypeFighter `Expr`.
/// Returns the resulting AST or an error with position info.
let parse (source: string) : Result<Expr<unit>, string> =
    match run source program with
    | POk pval -> Ok pval.result
    | PError err ->
        let pos = DocPos.create err.idx source
        Error (sprintf "Parse error at line %d, column %d: %s" pos.ln pos.col err.message)

/// Parse a TypeFighter type expression.
let parseTyp (source: string) : Result<MonoTyp, string> =
    match run source typeProgram with
    | POk pval -> Ok pval.result
    | PError err ->
        let pos = DocPos.create err.idx source
        Error (sprintf "Type parse error at line %d, column %d: %s" pos.ln pos.col err.message)
