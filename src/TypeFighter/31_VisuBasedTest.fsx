
#load "30_Visu.fsx"
open ``01_Core``
open ``10_Api``
open ``11_Expect``
open ``30_Visu``

open Dsl
open ImportedFunctionNames
open Types










(*
    "Hello World"
*)

(Str "Hello World")
// |> showSolvedGraph [] |> ignore
|> showSolvedAst []

















(*
    (12, "Item 2", 99)
*)

(Tuple [ Num 12.0; Str "Item 2"; Num 99.0 ])
// |> showSolvedGraph [] |> ignore
|> showSolvedAst [] |> ignore













(*
    let id = fun x -> x
    (id "Hello World", id 42.0)
*)

(
    (Let "id" (Fun "x" (Var "x"))
    (Tuple [
        App (Var "id") (Str "Hello World")
        App (Var "id") (Num 42.0) ]))
)
// |> showSolvedGraph [] |> ignore
|> showSolvedAst [] |> ignore









(*
    (fun id -> id "Hello World", id 42.0) (fun x -> x)
*)

(
    (App
        (Fun "id" (
            Tuple [ 
                App (Var "id") (Str "Hello World")
                App (Var "id") (Num 42.0) ]))
        (Fun "x" (Var "x")))
)
// |> showSolvedGraph [] |> ignore
|> showSolvedAst [] |> ignore











(*
    let x = 42
    let y = "Hello World"
    let makeTuple = fun x y -> (x, y)
    makeTuple x y
*)

(
    (Let "x" (Num 42.0)
    (Let "y" (Str "Hello World")
    (Let "makeTuple" (Fun "x" (Fun "y" (Tuple [ Var "x"; Var "y" ])))
    (App (App (Var "makeTuple") (Var "x")) (Var "y")))))
)
// |> showSolvedGraph [] |> ignore
|> showSolvedAst [] |> ignore














(*
    fun f -> f 42.0
*)

(
    (Fun "f" (App (Var "f") (Num 42.0)))
)
// |> showSolvedGraph [] |> ignore
|> showSolvedAst [] |> ignore

