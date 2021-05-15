
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
    map (fun x -> tostring x)
*)

(
    App (Var(fst mapp)) (Abs "x" (App (Var "tostring") (Var "x")))
)
|> showSolvedAst [ tostring; mapp ]


(
    App (Abs "x" (Var "x")) (Num 0.0)
)
|> showSolvedAst []


(*
    fun f -> f 42.0
*)
(
    (Abs "f" (App (Var "f") (Num 42.0)))
)
|> showSolvedAst []
|> showSolvedGraph []




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


(
    (App
        (Abs "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]) )
        (Abs "x" (Var "x")))
)
|> showSolvedAst []



(*
    let id = fun x -> x
    (id "Hello World", id 42.0)
*)

(
    (Let "id" (Abs "x" (Var "x"))
    (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]))
)
|> showSolvedGraph []

