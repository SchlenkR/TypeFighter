module TypeFighter.Parser01.Tests.TestHelper

open TypeFighter
open TypeFighter.Parser01

let parse (src: string) : Expr<unit> =
    match Syntax.parse src with
    | Ok e    -> e
    | Error m -> failwithf "Parse failed: %s" m

let shouldParseTo (expected: Expr<unit>) (src: string) =
    let actual = parse src
    if actual <> expected then
        failwithf "Parse mismatch.\n  Expected: %A\n  Actual:   %A" expected actual

let shouldFailToParse (src: string) =
    match Syntax.parse src with
    | Error _ -> ()
    | Ok e    -> failwithf "Expected parse error, but got: %A" e

let shouldParseSuccessfully (src: string) =
    match Syntax.parse src with
    | Ok _    -> ()
    | Error m -> failwithf "Expected parse to succeed, but got: %s" m
