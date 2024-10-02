
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

    let x = ExprCtx()
    
    x.Lit "10"
    // |> TypeSystem.generateConstraints Map.empty
    // |> TypeSystem.Helper.printConstraints
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    fun x -> x
*)
let [<Test>] ``simple fun`` () =

    let x = ExprCtx()
    
    x.Fun (x.Ident "x") (x.Var "x")
    |> solve []
    |> shouldSolveType (TDef.Generalize (%1 ^-> %1))






(*
    (fun x -> x) "fdf"
*)
let [<Test>] ``simple fun app`` () =

    let x = ExprCtx()

    x.App (x.Fun (x.Ident "x") (x.Var "x")) (x.Lit "xxx")
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.string)





(*
    (fun x -> x) 50
*)
let [<Test>] ``simple fun app 2`` () =

    let x = ExprCtx()
    
    x.App (x.Fun (x.Ident "x") (x.Var "x")) (x.Lit "50")
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.number)





(*
    add 100 10
*)
let [<Test>] ``add 100 10`` () =

    let x = ExprCtx()
    
    x.App (x.App (x.Var "add") (x.Lit "100")) (x.Lit "10")
    |> solve 
        [
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)





(*
    add 100
*)
let [<Test>] ``partial application`` () =

    let x = ExprCtx()
    
    x.App (x.Var "add") (x.Lit "100")
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

    let x = ExprCtx()
    
    x.Let (x.Ident "x") (x.Lit "10") (
        x.Let (x.Ident "y") (x.Lit "20") (
            x.App (x.App (x.Var "add") (x.Var "x")) (x.Var "y")
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

    let x = ExprCtx()

    x.App (x.App (x.Var "addDays") (x.Var "Now")) (x.Lit "10")
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

    let x = ExprCtx()

    x.App (x.App (x.Var "add") (x.Lit "10")) (x.Lit "Hello")
    |> solve 
        [
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    |> shouldFail

