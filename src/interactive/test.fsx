
#load "visuBase.fsx"

open TestBase
open VisuBase
open TypeFighter
open TypeFighter.Api
open TypeFighter.Api.Dsl
open TypeFighter.Api.ImportedFunctionNames
open TypeFighter.Api.Types
open TypeFighter.Tests
open TypeFighter.Tests.Expect




(*
    fun f -> f 42.0
*)
(
    (Abs "f" (App (Var "f") (Num 42.0)))
)
|> showSolvedAst []




(*
    (fun id -> id "Hello World")(fun x -> x)
*)
(
    (App
        (Abs "id" (App (Var "id") (Str "Hello World")))
        (Abs "x" (Var "x")))
)
|> showSolvedGraph []
//|> showSolvedAst []

