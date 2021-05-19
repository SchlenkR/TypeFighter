
#load "visuBase.fsx"

open TypeFighter
open TypeFighter.Api
open TypeFighter.Api.Dsl
open TypeFighter.Api.ImportedFunctionNames
open TypeFighter.Api.Types
open TypeFighter.Tests
open TypeFighter.Tests.Expect
open TestBase
open VisuBase




(*
    let id = fun x -> x
    (id "Hello World", id 42.0)
*)

(
    (Let "id" (Abs "x" (Var "x"))
    (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]))
)
|> showSolvedGraph []
|> showSolvedAst []




(*
    (fun id -> id "Hello World", id 42.0)(fun x -> x)
*)
(
    (App
        (Abs "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]) )
        (Abs "x" (Var "x")))
)
|> showSolvedGraph []



(
    (Abs "f" (App (Var "f") (Num 42.0)))
)
|> showSolvedAstWEnv []


(*
    (fun x -> x) 0.0
*)
       
(
    App (Abs "x" (Var "x")) (Num 0.0)
)
|> showSolvedAst []
|> showSolvedGraph []
