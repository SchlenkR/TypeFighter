module PureState.TypeFighter.Tests.TheBluntTests

open System
open NUnit.Framework
open FsUnit
open TheBlunt

// --- Helpers ----------------------------------------------------------------

let private runOk parser input =
    match run input parser with
    | POk pv -> pv.result
    | PError e -> failwithf "expected ok at %d, got error: %s" e.idx e.message

let private runErr parser input =
    match run input parser with
    | POk pv -> failwithf "expected error, got ok: %A" pv.result
    | PError e -> e

let private runRange parser input =
    match run input parser with
    | POk pv -> pv.range.startIdx, pv.range.endIdx
    | PError e -> failwithf "expected ok, got error: %s" e.message

// --- pstr -------------------------------------------------------------------

[<Test>]
let ``pstr matches literal`` () =
    runOk (pstr "hello") "hello world" |> should equal "hello"

[<Test>]
let ``pstr fails on mismatch`` () =
    let err = runErr (pstr "hello") "hi there"
    err.idx |> should equal 0

[<Test>]
let ``pstr range covers the matched literal`` () =
    runRange (pstr "abc") "abcdef" |> should equal (0, 3)

// --- orThen / <|> -----------------------------------------------------------

[<Test>]
let ``<|> takes the first success`` () =
    runOk (pstr "yes" <|> pstr "no") "yes" |> should equal "yes"

[<Test>]
let ``<|> falls through to the second on failure`` () =
    runOk (pstr "yes" <|> pstr "no") "no" |> should equal "no"

[<Test>]
let ``<|> reports an error if all alternatives fail`` () =
    runErr (pstr "yes" <|> pstr "no") "maybe" |> ignore

// --- andThen .>. .>> >>. ----------------------------------------------------

[<Test>]
let ``.>. returns a tuple of both`` () =
    runOk (pstr "a" .>. pstr "b") "ab" |> should equal ("a", "b")

[<Test>]
let ``.>> keeps left, drops right`` () =
    runOk (pstr "a" .>> pstr "b") "ab" |> should equal "a"

[<Test>]
let ``>>. drops left, keeps right`` () =
    runOk (pstr "a" >>. pstr "b") "ab" |> should equal "b"

// --- many / many1 -----------------------------------------------------------

[<Test>]
let ``many matches zero occurrences without failing`` () =
    let p = many (pstr "a") |> pvalues
    runOk p "" |> should equal ([] : string list)

[<Test>]
let ``many matches greedily`` () =
    let p = many (pstr "a") |> pvalues
    runOk p "aaab" |> should equal [ "a"; "a"; "a" ]

[<Test>]
let ``many1 fails on zero occurrences`` () =
    let p = many1 (pstr "a")
    runErr p "" |> ignore

// --- ptry / pchoice ---------------------------------------------------------

[<Test>]
let ``ptry yields Some on success and None on failure (no consume)`` () =
    let p = ptry (pstr "x")
    let some = runOk p "xy"
    some |> Option.map (fun pv -> pv.result) |> should equal (Some "x")
    runOk p "ab" |> should equal None

[<Test>]
let ``pchoice picks the first match`` () =
    let p = pchoice [ pstr "alpha"; pstr "beta"; pstr "gamma" ]
    runOk p "beta!" |> should equal "beta"

[<Test>]
let ``pchoice fails with the furthest-progress error`` () =
    let abcd = pstr "ab" .>. pstr "cd" |> map (fun (a, b) -> a + b)
    let xy = pstr "xy"
    let p = pchoice [ abcd; xy ]
    let err = runErr p "abZZ"
    // The `ab .>. cd` branch consumed past the `ab` before failing -- that
    // error reaches further than the leading `xy` mismatch and wins.
    err.idx |> should be (greaterThan 0)

// --- psepBy / psepBy0 -------------------------------------------------------

[<Test>]
let ``psepBy1 parses a non-empty separated list`` () =
    let p = psepBy1 (pstr ",") (pstr "x") |> pvalues
    runOk p "x,x,x" |> should equal [ "x"; "x"; "x" ]

[<Test>]
let ``psepBy0 accepts an empty list`` () =
    let p = psepBy0 (pstr ",") (pstr "x") |> pvalues
    runOk p "" |> should equal ([] : string list)

// --- parse{} computation expression -----------------------------------------

[<Test>]
let ``parse builder threads cursor and aggregates ranges`` () =
    let p =
        parse {
            let! a = pstr "("
            let! body = pstr "hello"
            let! b = pstr ")"
            return { range = Range.add a.range b.range; result = body.result }
        }
    let pv =
        match run "(hello)" p with
        | POk pv -> pv
        | PError e -> failwithf "%s" e.message
    pv.result |> should equal "hello"
    pv.range.startIdx |> should equal 0
    pv.range.endIdx |> should equal 7

// --- pstringUntil / eoi -----------------------------------------------------

[<Test>]
let ``pstringUntil captures verbatim up to the terminator`` () =
    let p =
        parse {
            let! _ = pstr "<"
            let! body = pstringUntil (pstr ">")
            let! _ = pstr ">"
            return body
        }
    runOk p "<inside>" |> should equal "inside"

[<Test>]
let ``eoi succeeds at end of input only`` () =
    runOk (pstr "ok" .>> eoi) "ok" |> should equal "ok"
    runErr (pstr "ok" .>> eoi) "okk" |> ignore

// --- map / pisOk / pnot -----------------------------------------------------

[<Test>]
let ``map transforms the result without touching the cursor`` () =
    let p = pstr "42" |> map int
    runOk p "42 rest" |> should equal 42

[<Test>]
let ``pisOk returns true on success and false on failure without consuming`` () =
    runOk (pisOk (pstr "x")) "x" |> should equal true
    runOk (pisOk (pstr "x")) "y" |> should equal false

[<Test>]
let ``pnot succeeds when the inner parser fails`` () =
    let p =
        parse {
            let! _ = pnot (pstr "x")
            let! v = pstr "y"
            return v
        }
    runOk p "y" |> should equal "y"
