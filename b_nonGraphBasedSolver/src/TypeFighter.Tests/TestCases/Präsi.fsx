
#load "../TestHelperFsiOverrides.fsx"
open TypeFighter.Tests.TestHelper

// do this _after_ opening TestHelper
open TestHelperFsiOverrides

open NUnit.Framework
open TypeFighter.Lang


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
    let play = number =>
        if (equals(number, 42), "win", "lose")
    let ourChoice = random(0, 10)
    play(ourChoice)
*)

X.Let (X.Ident "play") (
    X.Fun (X.Ident "number") (
        let cond = X.App (X.App (X.Var "equals") (X.Var "number")) (X.Lit "42")
        X.App (X.App (X.App (X.Var "if") cond) (X.Lit "win")) (X.Lit "lose")
    )
) (
    X.App (X.Var "play") (X.Lit "99")
)
|> writeInitialAst
// |> solve env
// |> shouldSolveType (Mono BuiltinTypes.string)
// |> ignore

