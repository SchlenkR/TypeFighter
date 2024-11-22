
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Base
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
    10
*)
let [<Test>] ``literal`` () =

   
    X.Lit 10
    // |> TypeSystem.generateConstraints Map.empty
    // |> TypeSystem.Helper.printConstraints
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    fun x -> x
*)
let [<Test>] ``simple fun`` () =

    // TODO: Re-Number when generalizing

    X.Fun (X.Ident "x") (X.Var "x")
    |> solve []
    |> shouldSolveType (TDef.Generalize (%1 ^-> %1))






(*
    (fun x -> x) "fdf"
*)
let [<Test>] ``simple fun app`` () =
    X.App (X.Fun (X.Ident "x") (X.Var "x")) (X.Lit "xxx")
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.string)





(*
    (fun x -> x) 50
*)
let [<Test>] ``simple fun app 2`` () =

    X.App (X.Fun (X.Ident "x") (X.Var "x")) (X.Lit 50)
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.number)





(*
    add 100 10
*)
let [<Test>] ``add 100 10`` () =

    X.App (X.App (X.Var "add") (X.Lit 100)) (X.Lit 10)
    |> solve 
        [
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)





(*
    add 100
*)
let [<Test>] ``partial application`` () =

    X.App (X.Var "add") (X.Lit 100)
    |> solve 
        [
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    |> shouldSolveType (Mono (BuiltinTypes.number ^-> BuiltinTypes.number))






(*
    let x = 10
    let y = 20
    add x y
*)
let [<Test>] ``simple let`` () =

    X.Let (X.Ident "x") (X.Lit 10) (
        X.Let (X.Ident "y") (X.Lit 20) (
            X.App (X.App (X.Var "add") (X.Var "x")) (X.Var "y")
    ))
    |> solve
        [
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    addDays Now 10
*)
let [<Test>] ``addDays Now 10`` () =

    X.App (X.App (X.Var "addDays") (X.Var "Now")) (X.Lit 10)
    |> solve
        [
            "addDays", Mono (BuiltinTypes.date ^-> BuiltinTypes.number ^-> BuiltinTypes.date)
            "Now", Mono BuiltinTypes.date
        ]
    |> shouldSolveType (Mono BuiltinTypes.date)



(*
    add 10 "Hello"
*)
// ERROR: no implicit conversion from string to number (Can't unify Number and String)
let [<Test>] ``error - add wrong types`` () =

    X.App (X.App (X.Var "add") (X.Lit 10)) (X.Lit "Hello")
    |> solve 
        [
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    |> shouldFail

