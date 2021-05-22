
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



module ImportantInference =

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



    (*
        (fun x -> x) 42.0
    *)
    (
        App (Abs "x" (Var "x")) (Num 42.0)
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

