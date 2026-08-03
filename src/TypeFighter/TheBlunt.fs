// Copyright (c) Ronald Schlenker
// Evaluation use only. Commercial use prohibited.

module TheBlunt

open System

type Str = System.ReadOnlySpan<char>


// -----------------------------------------------------------------------------------------------
// BEGIN :)
// -----------------------------------------------------------------------------------------------


type Parser<'value> = Cursor -> ParserResult<'value>

and [<Struct>] Cursor =
    {
        original: string
        idx: int
    }

and [<Struct>] ParserResult<'out> =
    | POk of ok: PVal<'out>
    | PError of error: ParseError

and [<Struct>] PVal<'out> =
    {
        range: Range
        result: 'out
    }

and [<Struct>] ParseError =
    {
        idx: int
        message: string
    }

and [<Struct>] Range = 
    {
        startIdx: int
        endIdx: int
    }

type [<Struct>] DocPos =
    {
        idx: int
        ln: int
        col: int
    }

let inline mkParser ([<InlineIfLambda>] parser: Parser<_>) = parser
let inline getParser ([<InlineIfLambda>] parser: Parser<_>) = parser

type Cursor with
    /// Forward-only navigation: `idx >= c.idx` rejects rewinds; the upper
    /// bound permits landing exactly on end-of-input (needed by zero-width
    /// successors like `eoi` and empty `many`).
    member c.CanGoto(idx: int) =
        idx >= c.idx && idx <= c.original.Length
    member c.CanWalkFwd(steps: int) = c.CanGoto(c.idx + steps)
    member c.IsAtEnd = c.idx = c.original.Length
    member c.HasRest = c.idx < c.original.Length
    member c.Rest : Str = c.original.AsSpan(c.idx)
    member c.StartsWith(s: string) = c.Rest.StartsWith(s.AsSpan())
    member c.Goto(idx: int) =
        if not (c.CanGoto(idx)) then
            failwithf "Index %d is out of range of string of length %d." idx c.original.Length
        { idx = idx; original = c.original }
    member c.WalkFwd(steps: int) = c.Goto(c.idx + steps)
    member c.MoveNext() = c.WalkFwd(1)

module Range =
    let inline create startIdx endIdx = { startIdx = startIdx; endIdx = endIdx }
    let zero = { startIdx = 0; endIdx = 0 }
    let inline add r1 r2 = { startIdx = r1.startIdx; endIdx = r2.endIdx }
    let inline merge ranges = ranges |> List.reduce add

module POk =
    let inline createFromRange range result =
        POk { range = range; result = result }
    let inline create startIdx endIdx result =
        createFromRange (Range.create startIdx endIdx) result

module PError =
    let inline create (idx: int) (message: string) =
        PError { idx = idx; message = message }

module PVal =
    let inline map ([<InlineIfLambda>] proj) (pval: PVal<'a>) =
        { range = pval.range; result = proj pval.result }
    let ranges (pvals: PVal<'a> list) =
        pvals |> List.map (fun x -> x.range) |> Range.merge
    /// Build a PVal whose range covers the given input ranges.
    /// Convenient inside `parse { ... return PVal.ofRanges [a.range; b.range] (...) }`.
    let inline ofRanges (ranges: Range list) (result: 'a) : PVal<'a> =
        { range = Range.merge ranges; result = result }

// TODO: Perf: The parser combinators could track that, instead of computing it from scratch.
module DocPos =
    let create (index: int) (input: string) =
        if index < 0 || index > input.Length then
            failwithf "Index %d is out of range of input string of length %d." index input.Length
        let lineStart = 1
        let columnStart = 1
        let mutable currIdx = 0
        let mutable line = lineStart
        let mutable column = columnStart
        while currIdx <> index do
            let isLineBreak = input.[currIdx] = '\n'
            if isLineBreak then
                line <- line + 1
                column <- columnStart
            else
                column <- column + 1
            currIdx <- currIdx + 1
        { idx = index; ln = line; col = column }
    let ofInput (pi: Cursor) = create pi.idx pi.original

module Cursor =
    let hasRemainingChars n =
        fun (inp: Cursor) ->
            if not (inp.CanWalkFwd n)
            then PError.create inp.idx (sprintf "Expected %d more characters." n)
            else POk.create inp.idx inp.idx ()
    let inline notAtEnd cursor = 
        hasRemainingChars 1 cursor

let inline pwhen pred ([<InlineIfLambda>] p) =
    mkParser <| fun inp ->
        match pred inp with
        | PError err -> PError.create inp.idx err.message
        | POk _ -> getParser p inp

let inline mkParserWhen pred ([<InlineIfLambda>] pf) =
    pwhen pred <| mkParser pf

let inline bind ([<InlineIfLambda>] f) ([<InlineIfLambda>] parser) =
    mkParser <| fun inp ->
        match getParser parser inp with
        | PError error -> PError error
        | POk pRes ->
            let fParser = getParser (f pRes)
            fParser (inp.Goto(pRes.range.endIdx))

type ParserBuilder() =
    member inline _.Bind([<InlineIfLambda>] p, [<InlineIfLambda>] f) =
        bind f p
    member _.Return(pval: PVal<_>) =
        mkParser (fun inp -> POk pval)
    member _.Return(err: ParseError) =
        mkParser (fun inp -> PError err)

let parse = ParserBuilder()

let pseq (s: _ seq) =
    let enum = s.GetEnumerator()
    mkParser (fun inp ->
        if enum.MoveNext()
        then POk.create inp.idx inp.idx enum.Current
        else PError.create inp.idx "No more elements in sequence."
    )

let inline run (text: string) ([<InlineIfLambda>] parser) =
    getParser parser { idx = 0; original = text }

let inline map ([<InlineIfLambda>] proj) ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | PError error -> PError error
        | POk pres -> POk { range = pres.range; result = proj pres.result }
    )

let inline mapPVal ([<InlineIfLambda>] proj) ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | PError error -> PError error
        | POk pres -> POk { range = pres.range; result = proj pres }
    )

let inline pignore ([<InlineIfLambda>] p) =
    map (fun _ -> ()) p

let inline pattempt ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | POk res -> POk.createFromRange res.range (Some res)
        | PError err -> PError err
    )

let inline ptry ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | POk res -> POk.createFromRange res.range (Some res)
        | PError err -> POk.createFromRange (Range.create inp.idx inp.idx) None
    )

let inline pisOk ([<InlineIfLambda>] p) = 
    mkParser (fun inp ->
        match getParser p inp with
        | POk res -> POk.createFromRange res.range true
        | PError err -> POk.create inp.idx inp.idx false
    )

let inline pisErr ([<InlineIfLambda>] p) =
    pisOk p |> map not

// TODO: A strange thing is this
let inline pnot ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser (pattempt p) inp with
        | POk _ -> PError.create inp.idx "Unexpected." // TODO
        | PError _ -> POk.create inp.idx inp.idx ()
    )

let punit =
    mkParser (fun inp ->
        POk.create inp.idx inp.idx ()
    )

let pstr (s: string) =
    mkParser (fun inp ->
        if inp.StartsWith(s)
        then POk.create inp.idx (inp.idx + s.Length) s
        else PError.create inp.idx (sprintf "Expected: '%s'" s)
    )
let ( ~% ) = pstr

let pgoto (idx: int) =
    mkParser (fun inp ->
        if inp.CanGoto(idx) then 
            POk.create inp.idx idx ()
        else
            // TODO: this propably would be a fatal, most propably an unexpected error
            let msg = sprintf "Index %d is out of range of string of length %d." idx inp.original.Length
            PError.create idx msg
    )

let inline orThen ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) =
    mkParser (fun inp ->
        match getParser pa inp with
        | POk res -> POk res
        | PError _ -> getParser pb inp
    )
let ( <|> ) = orThen

let inline andThen ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) =
    mkParser (fun inp ->
        match getParser pa inp with
        | POk ares ->
            match getParser pb (inp.Goto ares.range.endIdx) with
            | POk bres -> POk.create inp.idx bres.range.endIdx (ares.result, bres.result)
            | PError error -> PError error
        | PError error -> PError error
    )
// type AndThen = AndThen with
//     static member inline ($) (AndThen, x: (Parser<_> * Parser<_>)) =
//         let a,b = x
//         andThen a b
// let inline ( <&> ) a b = (($) AndThen) (a, b)
let inline ( .>. ) ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) = andThen pa pb
let inline ( .>> ) ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) = andThen pa pb |> map fst
let inline ( >>. ) ([<InlineIfLambda>] pa) ([<InlineIfLambda>] pb) = andThen pa pb |> map snd

let firstOf parsers = parsers |> List.reduce orThen

let inline manyN minOccurances ([<InlineIfLambda>] p: Parser<_>) =
    mkParser (fun inp ->
        let mutable currIdx = inp.idx
        let mutable run = true
        let mutable iterations = 0
        let res =
            [ while run do
                match getParser p (inp.Goto(currIdx)) with
                | POk res ->
                    yield res
                    do currIdx <- res.range.endIdx
                    do
                        if inp.idx = currIdx
                        then run <- false
                        else iterations <- iterations + 1
                | PError _ ->
                    do run <- false
            ]
        if iterations < minOccurances 
        then PError.create currIdx $"Expected {minOccurances} occurances, but got {iterations}."
        else POk.create inp.idx currIdx res
    )

let inline many ([<InlineIfLambda>] p) = manyN 0 p

let inline many1 ([<InlineIfLambda>] p) =
    parse {
        let! res = many p
        match res.result with
        | [] -> return { idx = res.range.startIdx; message = "Expected at least one element." }
        | _ -> return res
    }

// TODO: sepBy
// TODO: skipN

let anyChar =
    mkParserWhen Cursor.notAtEnd (fun inp ->
        POk.create inp.idx (inp.idx + 1) (inp.Rest.[0].ToString())
    )

let inline noRanges ([<InlineIfLambda>] p: Parser<PVal<'a> list>) =
    map (fun pvals -> pvals |> List.map (fun x -> x.result)) p

let inline pconcat ([<InlineIfLambda>] p: Parser<PVal<string> list>) =
    map (fun pvals -> pvals |> List.map (fun x -> x.result) |> String.concat "") p

// TODO: anyCharExcept(c,p)

let eoi =
    mkParser (fun inp ->
        if inp.IsAtEnd
        then POk.create inp.idx inp.idx ()
        else PError.create inp.idx "Expected end of input."
    )

let blank = pstr " "
let blanks = many blank |> pconcat
let blanks1 = many1 blank |> pconcat

let inline pstringUntil ([<InlineIfLambda>] puntil) =
    mkParser (fun inp ->
        let rec iter currIdx =
            match getParser (pattempt puntil) (inp.Goto currIdx) with
            | POk _ -> POk.create inp.idx currIdx (inp.original.Substring(inp.idx, currIdx - inp.idx))
            | PError _ ->
                if not (inp.CanGoto(currIdx + 1))
                then PError.create currIdx "End of input."
                else iter (currIdx + 1)
        iter inp.idx
    )

// TODO: make clear: Parsers that

let many1Str2 p1 p2 =
    parse {
        let! r1 = p1
        let! r2 = many p2 |> pconcat
        return
            {
                range = Range.add r1.range r2.range
                result = r1.result + r2.result
            }
    }

let inline setErrorMessage msg ([<InlineIfLambda>] p) =
    mkParser (fun inp ->
        match getParser p inp with
        | POk _ as res -> res
        | PError err -> PError { err with message = msg }
    )

let inline many1Str ([<InlineIfLambda>] p) = many1Str2 p p

let pchar predicate errMsg =
    mkParserWhen Cursor.notAtEnd <| fun inp ->
        let c = inp.Rest.[0]
        if predicate c
        then POk.create inp.idx (inp.idx + 1) (string c)
        else PError.create inp.idx (errMsg c)

let letter =
    pchar (Char.IsLetter) (sprintf "Expected letter, but got '%c'.")

let digit =
    pchar (Char.IsDigit) (sprintf "Expected letter, but got '%c'.")

let inline notFollowedBy ([<InlineIfLambda>] p) suffix =
    parse {
        let! x = p
        let! _ = pnot (pstr suffix)
        return x
    }

let inline psepBy1 ([<InlineIfLambda>] psep) ([<InlineIfLambda>] pelem: Parser<_>) =
    parse {
        let! x = pelem
        let! xs = many (psep >>. pelem)
        return
            {
                range = Range.add x.range xs.range
                result = x :: xs.result
            }
    }

/// Like `psepBy1` but accepts zero elements.
let inline psepBy0 ([<InlineIfLambda>] psep) ([<InlineIfLambda>] pelem: Parser<_>) =
    psepBy1 psep pelem
    <|> mkParser (fun inp -> POk.create inp.idx inp.idx [])

/// Drop the per-element PVal wrapping that `many`/`psepBy*` produce.
let inline pvalues ([<InlineIfLambda>] p: Parser<PVal<'a> list>) : Parser<'a list> =
    map (fun pvals -> pvals |> List.map (fun x -> x.result)) p

/// Tries each parser in order and returns the first success. On total failure
/// it reports the alternative that consumed the furthest into the input -- that
/// one is most likely the intended path, so its message is the most useful.
let pchoice parsers =
    mkParser <| fun inp ->
        let mutable furthest = { idx = inp.idx; message = "no parsers tried" }
        let rec iter parsers =
            match parsers with
            | [] -> PError furthest
            | p :: ps ->
                match getParser p inp with
                | POk _ as res -> res
                | PError err ->
                    if err.idx >= furthest.idx then furthest <- err
                    iter ps
        iter parsers

// ---------------------------------------------------------------------------
// Expression parser: operator precedence by table.
// Caller supplies a `pSurface` that turns a literal operator string into a
// `Parser<unit>` (typically wrapping pstr with whitespace-handling), the
// terminal/atom parser, and a precedence table where the FIRST level binds
// loosest and the LAST level binds tightest. Per level: same associativity
// for all infix operators (classic constraint); prefix/postfix chains are
// applied around the next level's term before the infix fold.

type Assoc = Left | Right

type Op<'a> =
    | Infix of surface: string * assoc: Assoc * combine: ('a -> 'a -> 'a)
    | Prefix of surface: string * combine: ('a -> 'a)
    | Postfix of surface: string * combine: ('a -> 'a)

let buildExprParser
    (pSurface: string -> Parser<unit>)
    (term: Parser<'a>)
    (table: Op<'a> list list)
    : Parser<'a>
    =
    let pid : Parser<'a -> 'a> =
        mkParser (fun inp -> POk.create inp.idx inp.idx id)

    let buildLevel (ops: Op<'a> list) (next: Parser<'a>) : Parser<'a> =
        // Sort by surface length desc so e.g. "<=" matches before "<".
        let infixes =
            ops
            |> List.choose (function Infix (s,a,c) -> Some (s,a,c) | _ -> None)
            |> List.sortByDescending (fun (s,_,_) -> (s: string).Length)
        let prefixes =
            ops
            |> List.choose (function Prefix (s,c) -> Some (s,c) | _ -> None)
            |> List.sortByDescending (fun (s,_) -> (s: string).Length)
        let postfixes =
            ops
            |> List.choose (function Postfix (s,c) -> Some (s,c) | _ -> None)
            |> List.sortByDescending (fun (s,_) -> (s: string).Length)

        let assoc =
            infixes |> List.tryHead |> Option.map (fun (_,a,_) -> a) |> Option.defaultValue Left

        let unaryChoice (xs: (string * ('a -> 'a)) list) : Parser<'a -> 'a> =
            xs |> List.map (fun (s,c) -> pSurface s |> map (fun _ -> c)) |> pchoice

        let prefixChain : Parser<'a -> 'a> =
            if List.isEmpty prefixes then pid
            else
                many (unaryChoice prefixes)
                |> map (fun pvals ->
                    pvals
                    |> List.map (fun pv -> pv.result)
                    |> fun fs -> List.foldBack (fun f acc -> f << acc) fs id)

        let postfixChain : Parser<'a -> 'a> =
            if List.isEmpty postfixes then pid
            else
                many (unaryChoice postfixes)
                |> map (fun pvals ->
                    pvals
                    |> List.map (fun pv -> pv.result)
                    |> List.fold (fun acc f -> f << acc) id)

        let unaryWrapped : Parser<'a> =
            parse {
                let! pre = prefixChain
                let! atom = next
                let! post = postfixChain
                return PVal.ofRanges [pre.range; atom.range; post.range]
                                     (post.result (pre.result atom.result))
            }

        match infixes with
        | [] -> unaryWrapped
        | _ ->
            let opParser =
                infixes
                |> List.map (fun (s,_,c) -> pSurface s |> map (fun _ -> c))
                |> pchoice
            match assoc with
            | Left ->
                parse {
                    let! seed = unaryWrapped
                    let! rest = many (opParser .>. unaryWrapped)
                    let folded =
                        rest.result
                        |> List.fold
                            (fun acc pv ->
                                let comb, r = pv.result
                                comb acc r)
                            seed.result
                    return PVal.ofRanges [seed.range; rest.range] folded
                }
            | Right ->
                parse {
                    let! seed = unaryWrapped
                    let! rest = many (opParser .>. unaryWrapped)
                    let folded =
                        match rest.result with
                        | [] -> seed.result
                        | _ ->
                            let pairs = rest.result |> List.map (fun pv -> pv.result)
                            let valArr = seed.result :: List.map snd pairs |> List.toArray
                            let combArr = pairs |> List.map fst |> List.toArray
                            let mutable acc = valArr.[valArr.Length - 1]
                            for i = valArr.Length - 2 downto 0 do
                                acc <- combArr.[i] valArr.[i] acc
                            acc
                    return PVal.ofRanges [seed.range; rest.range] folded
                }

    // First level binds loosest -> built outermost. foldBack does that.
    List.foldBack buildLevel table term
