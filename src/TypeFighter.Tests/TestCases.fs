module TypeFighter.Tests.TestCases

open Expecto
open TypeFighter.Api.Dsl
open TypeFighter.Api.Types
open TypeFighter.Tests.Expect

[<Tests>]
let tests = testList "Main Tests" [

    test "map numbers using add" {
        (*
            let x = 10.0
            map Numbers (number ->
                add number x)
        *)

        (
            (Let "x" (Num 10.0)
            (MapX (Var "Numbers") (Abs "number"
                (Appn (Var "add") [ Var "number"; Var "x" ] ))))
        )

        |> inferType [ map; add; numbers ] (seqOf numberTyp)
    }
    
    test "Get record property" {
        (*
            let x = { a = 5.0; b = "hello" }
            x.b
        *)

        (
            (Let "x" (Record [ ("a", Num 5.0); ("b", Str "hello") ])
            (Prop "b" (Var "x")))
        )

        |> inferType [] stringTyp
    }
    
    test "List of numbers" {
        (*
            [ 1.0; 2.0; 3.0 ]
        *)

        (
            NewList [ Num 1.0; Num 2.0; ]
        )

        |> inferType [ cons; emptyList ] (seqOf numberTyp)
    }

    test "Heterogeneous list (error)" {
        (*
            [ 1.0; 2.0; "xxx" ]
        *)

        (
            NewList [ Num 1.0; Num 2.0; Str "xxx" ]
        )

        |> inferError [ cons; emptyList ]
    }


    test "Lambda applied to map" {
        (*
            map (fun x -> tostring x)
        *)

        (
            App (Var(fst mapp)) (Abs "x" (App (Var "tostring") (Var "x")))
        )

        |> inferType [ tostring; mapp ] (seqOf %3 ^-> seqOf stringTyp)
    }

    test "Lambda string" {
        (*
            fun x -> tostring x
        *)

        (
            (Abs "x" (App (Var "tostring") (Var "x")))
        )

        |> inferType [ tostring ] (%2 ^-> stringTyp)
    }

    test "Polymorphic let" {
        (*
            let id = fun x -> x
            (id "Hello World", id 42.0)
        *)

        (
            (Let "id" (Abs "x" (Var "x"))
            (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]))
        )

        |> inferType [] (stringTyp * numberTyp)
    }

    test "Pass id to lambda" {
        (*
            (fun id -> id "Hello World")(fun x -> x)
        *)
        (
            (App
                (Abs "id" (App (Var "id") (Str "Hello World")))
                (Abs "x" (Var "x")))
        )

        |> inferType [] (stringTyp)
    }

    test "No polymorphic abs" {
        (*
            (fun id -> id "Hello World", id 42.0)(fun x -> x)
        *)

        (
            (App
                (Abs "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]) )
                (Abs "x" (Var "x")))
        )

        |> inferError []
    }

    test "Currying" {
        let mul2 = App (Var "mul") (Num 2.0)
        let add3 = App (Var "add") (Num 3.0)
        let tostr = Var "tostring"

        (*
            tostr (mul 2.0 (add 3.0 10.0))
        *)
       
        (
            (App tostr
            (App mul2
            (App add3
            (Num 10.0))))
        )

        |> inferType [ add; mul; sub; div; tostring ] stringTyp
    }

    test "Shadowing" {
        (*
            let x = 7.0
            let x = "Hello"
            x
        *)
       
        (
            (Let "x" (Num 7.0)
            (Let "x" (Str "Hello")
            (Var "x")))
        )

        |> inferType [] stringTyp
    }

    test "App num to id" {
        (*
            (fun x -> x) 0.0
        *)
       
        (
            App (Abs "x" (Var "x")) (Num 0.0)
        )

        |> inferType [] numberTyp
    }

    test "App num to lambda bound function" {
        (*
            fun f -> f 42.0
        *)
       
        (
            (Abs "f" (App (Var "f") (Num 42.0)))
        )

        |> inferType [] ((numberTyp ^-> %2) ^-> %2)
    }
]

