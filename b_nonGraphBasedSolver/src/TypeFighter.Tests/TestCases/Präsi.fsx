
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
        "concat", Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string)
    ]

(*
    let play = name number =>
        let result = if (equals number 42) "wins" "loses"
        concat name result
    let ourChoice = random 0 100
    play "Ronald" ourChoice
*)


let playExpr =
    let ifExpr =
        let condExpr = X.App (X.App (X.Var "equals") (X.Var "number")) (X.Lit "42")
        X.App (X.App (X.App (X.Var "if") condExpr) (X.Lit "wins")) (X.Lit "loses")
    X.Fun (X.Ident "name") (X.Fun (X.Ident "number") (
        X.Let (X.Ident "result") ifExpr (
            X.App (X.App (X.Var "concat") (X.Var "name")) (X.Var "result")
        )))
let ourChoiceExpr = X.App (X.App (X.Var "random") (X.Lit "0")) (X.Lit "100")
let ast = X.App (X.App playExpr (X.Lit "Ronald")) ourChoiceExpr

ast
|> solve env
|> shouldSolveType (Mono BuiltinTypes.string)
