
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

    X.Match (X.App (X.Var "isValid") (X.Lit "42")) [
        X.Case "true" None (X.Lit "valid")
        X.Case "false" None (X.Lit "34")
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

    X.Match (X.App (X.Var "isValid") (X.Lit "42")) [
        X.Case "True" None (X.Lit "valid")
        X.Case "False" None (X.Lit "invalid")
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

    X.Match (X.App (X.Var "isValid") (X.Lit "42")) [
        X.Case "True" None (X.Lit "22")
        X.Case "False" None (X.Lit "22")
    ]
    |> solve
        [
            "isValid", Mono (BuiltinTypes.number ^-> BuiltinTypes.boolean)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)


