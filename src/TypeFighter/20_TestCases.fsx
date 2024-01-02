#load "11_Expect.fsx"

open ``01_Core``
open ``10_Api``
open ``11_Expect``

open Dsl
open Types


(*
    let x = 10.0
    map Numbers (number ->
        add number x)
*)

(
    (Let "x" (Num 10.0)
    (MapX (Var "Numbers") (Fun "number"
        (Appn (Var "add") [ Var "number"; Var "x" ] ))))
)

|> assertInferredType [ map; add; numbers ] (seqOf numberTyp)


(*
    let x = { a = 5.0; b = "hello" }
    x.b
*)

(
    (Let "x" (Record [ ("a", Num 5.0); ("b", Str "hello") ])
    (Prop "b" (Var "x")))
)

|> assertInferredType [] stringTyp



(*
    [ 1.0; 2.0; 3.0 ]
*)

(
    NewList [ Num 1.0; Num 2.0; ]
)

|> assertInferredType [ cons; emptyList ] (seqOf numberTyp)



(*
    [ 1.0; 2.0; "xxx" ]
*)

(
    NewList [ Num 1.0; Num 2.0; Str "xxx" ]
)

|> assertInferenceError [ cons; emptyList ]



(*
    map (fun x -> tostring x)
*)

(
    App (Var(fst mapp)) (Fun "x" (App (Var "tostring") (Var "x")))
)

|> assertInferredType [ tostring; mapp ] (seqOf %3 ^-> seqOf stringTyp)



(*
    fun x -> tostring x
*)

(
    (Fun "x" (App (Var "tostring") (Var "x")))
)

|> assertInferredType [ tostring ] (%2 ^-> stringTyp)



(*
    let id = fun x -> x
    (id "Hello World", id 42.0)
*)

(
    (Let "id" (Fun "x" (Var "x"))
    (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]))
)

|> assertInferredType [] (stringTyp * numberTyp)



(*
    (fun id -> id "Hello World")(fun x -> x)
*)
(
    (App
        (Fun "id" (App (Var "id") (Str "Hello World")))
        (Fun "x" (Var "x")))
)

|> assertInferredType [] (stringTyp)



(*
    (fun id -> id "Hello World", id 42.0)(fun x -> x)
*)

(
    (App
        (Fun "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]) )
        (Fun "x" (Var "x")))
)

|> assertInferenceError []




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

|> assertInferredType [ add; mul; sub; div; tostring ] stringTyp




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

|> assertInferredType [] stringTyp




(*
    (fun x -> x) 0.0
*)

(
    App (Fun "x" (Var "x")) (Num 0.0)
)

|> assertInferredType [] numberTyp




(*
    fun f -> f 42.0
*)

(
    (Fun "f" (App (Var "f") (Num 42.0)))
)

|> assertInferredType [] ((numberTyp ^-> %2) ^-> %2)

