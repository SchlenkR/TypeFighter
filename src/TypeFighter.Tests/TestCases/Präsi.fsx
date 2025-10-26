#load "../TestHelperFsiOverrides.fsx"
open TypeFighter.Tests.TestHelper

// do this _after_ opening TestHelper
open TestHelperFsiOverrides

open NUnit.Framework
open TypeFighter


// ------------------------------------------
// What's important here: The tests shall be executable
// via dotnet test (this project), but also via FSI.
// ------------------------------------------




let env =
    [
        "if", TDef.Generalize (BuiltinTypes.boolean ^-> %1 ^-> %1 ^-> %1)
        "equals", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.boolean)
        "random", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
    ]


(*
    let x = 42
    toString x
*)
let expr =
    let env = 
        [
            "toString", TDef.Generalize (BuiltinTypes.number ^-> BuiltinTypes.string)
        ]
    X.Let (X.Ident "x") (X.Lit 42) (
        X.App (X.Var "toString") (X.Var "x")
    )
    |> writeInitialAst



(*
    if x == "Hello" then
        return -1
    let y = 100
    return y + 23

Desugared pseudo-code:
    if x == "Hello" then
        -1
    else
        let y = 100
        y + 23
*)
let earlyReturnExpr =
    let env =
        [
            "if", TDef.Generalize (BuiltinTypes.boolean ^-> %1 ^-> %1 ^-> %1)
            "equalsStr", Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.boolean)
            "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        ]
    X.App (X.App (X.App (X.Var "if") (X.App (X.App (X.Var "equalsStr") (X.Var "x")) (X.Lit "Hello"))) (X.Lit (-1))) (
        X.Let (X.Ident "y") (X.Lit 100) (X.App (X.App (X.Var "add") (X.Var "y")) (X.Lit 23))
    )
    |> writeInitialAst




(*
    let play = number =>
        if (equals(number, 42), "win", "lose")
    play(99)
*)

let expr =
    X.Let (X.Ident "play") (
        X.Fun (X.Ident "number") (
            let cond = X.App (X.App (X.Var "equals") (X.Var "number")) (X.Lit 42)
            X.App (X.App (X.App (X.Var "if") cond) (X.Lit "win")) (X.Lit "lose")
        )
    ) (
        X.App (X.Var "play") (X.Lit 99)
    )



for i in 0..30 do
    expr |> solve env (Some i)
    System.Console.ReadLine() |> ignore


expr |> writeInitialAst
