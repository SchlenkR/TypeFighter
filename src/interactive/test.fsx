
#load "visuBase.fsx"

open TypeFighter
open TypeFighter.Api
open TypeFighter.Api.Dsl
open TypeFighter.Api.ImportedFunctionNames
open TypeFighter.Api.Types
//open TypeFighter.Tests
//open TypeFighter.Tests.Expect
open TestBase
open VisuBase



(*
    (fun x -> x) 42.0
*)
(
    App (Abs "x" (Var "x")) (Num 42.0)
)
|> showConstraintGraph []
|> showConstraints []
|> showAnnotatedAst []



module ImportantInference =

    (*
        let id = fun x -> x
        (id "Hello World", id 42.0)
    *)

    (
        (Let "id" (Abs "x" (Var "x"))
        (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]))
    )
    |> showAnnotatedAst []




    (*
        (fun id -> id "Hello World", id 42.0)(fun x -> x)
    *)
    (
        (App
            (Abs "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]) )
            (Abs "x" (Var "x")))
    )
    |> showAnnotatedAst []



    (*
        (fun x -> x) 42.0
    *)
    (
        App (Abs "x" (Var "x")) (Num 42.0)
    )
    |> showAnnotatedAst []


    (*
        fun f -> f 42.0
    *)
    (
        (Abs "f" (App (Var "f") (Num 42.0)))
    )
    |> showSolvedAst []
    |> showSolvedGraph []


    (*
        fun f -> f 42.0, f "xxx"
    *)
    (
        (Abs "f" (Tuple [ App (Var "f") (Num 42.0); App (Var "f") (Str "xxx") ]))
    )
    |> showSolvedAst []
    |> showSolvedGraph []



module EnvBoundValues =


    (*
        (fun id -> id "Hello World", id 42.0)(fun x -> x)
    *)
    (
        (App
            (Abs "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]) )
            (Abs "x" (Var "x")))
    )
    |> showAnnotatedAst []




    (*
        (fun x -> x)
    *)
    (
        (Abs "x" (Var "x"))
    )
    |> showAnnotatedAst []




    (*
        (fun x -> x) 42.0
    *)
    (
        App (Abs "x" (Var "x")) (Num 42.0)
    )
    |> showConstraints []
    |> showConstraintGraph []
    |> showAnnotatedAst []




    (*
        (fun x -> x) 42.0
    *)
    (
        Abs "x" (Var "x")
    )
    |> showConstraintGraph []
    |> showAnnotatedAst []




    (*
        let f = (fun x -> x)
        let rec = { myFunc = f }
        rec.myFunc 42.0
    *)
    (
        (Let "f" (Abs "x" (Var "x"))
        (Let "rec" (Record [ "myFunc", Var "f" ])
        (App (Prop "myFunc" (Var "rec")) (Num 42.0))
        ))
    )
    |> showAnnotatedAst []

