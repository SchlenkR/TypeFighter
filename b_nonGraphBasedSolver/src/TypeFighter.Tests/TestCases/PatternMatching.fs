
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.PatternMatching
#endif

open TypeFighter.Tests.TestHelper

// do this _after_ opening TestHelper
#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter.Lang


// ------------------------------------------
// What's important here: The tests shall be executable
// via dotnet test (this project), but also via FSI.
// ------------------------------------------




(*
    match (isValid 42) with
    | true -> "valid"
    | false -> 34
*)
// ERROR: Can't unify Number and String
let [<Test>] ``error - match branches unification error`` () =

    let x = ExprCtx()

    x.Match (x.App (x.Var "isValid") (x.Lit "42")) [
        x.Case "true" None (x.Lit "valid")
        x.Case "false" None (x.Lit "34")
    ]
    |> solve
        [
            yield "isValid", Mono (BuiltinTypes.number ^-> BuiltinTypes.boolean)
        ]
    |> shouldFail




(*
    match (isValid 42) with
    | true -> "valid"
    | false -> "invalid"
*)
let [<Test>] ``match branches ok`` () =

    let x = ExprCtx()

    x.Match (x.App (x.Var "isValid") (x.Lit "42")) [
        x.Case "True" None (x.Lit "valid")
        x.Case "False" None (x.Lit "invalid")
    ]
    |> solve
        [
            yield "isValid", Mono (BuiltinTypes.number ^-> BuiltinTypes.boolean)
        ]
    |> shouldSolveType (Mono BuiltinTypes.string)




(*
    match (isValid 42) with
    | true -> 1
    | false -> 2
*)
let [<Test>] ``match branches ok 2`` () =

    let x = ExprCtx()

    x.Match (x.App (x.Var "isValid") (x.Lit "42")) [
        x.Case "True" None (x.Lit "22")
        x.Case "False" None (x.Lit "22")
    ]
    |> solve
        [
            "isValid", Mono (BuiltinTypes.number ^-> BuiltinTypes.boolean)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)


