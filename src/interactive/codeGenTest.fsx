
#load "visuBase.fsx"

open TypeFighter
open TypeFighter.Api
open TypeFighter.Api.Dsl
open TypeFighter.Api.ImportedFunctionNames
open TypeFighter.Api.Types
open TypeFighter.DotNetCodeGen
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
|> renderDisplayClasses []







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
|> renderDisplayClasses [ map; add; numbers ]





(*
    let id = fun x -> { whatever = 23.0 }
    { myString = id "Hello World"; myNumber = id 42.0 }
*)
(
    (Let "id" (Abs "x" (Record [ "whatever", Num 23.0 ] ))
    (Record [ "myString", App (Var "id") (Str "Hello World"); "myNumber", App (Var "id") (Num 42.0) ]))
)
|> renderDisplayClasses []



(*
    let id = fun x -> x
    let add =
        fun a ->
            fun b -> { a = a; b = b }
    add "Hello" (id 42.0)
*)
(
    (Let "id" (Abs "x" (Var "x"))
    (Let "add" (Abs "a" (Abs "b" (Record [ "field1", Var "a"; "field2", Var "b" ])))
    (App (App (Var "add") (Str "Hello")) (App (Var "id") (Num 42.0)))))
)
|> renderDisplayClasses [ ]



(*
    let myValue = "yyyy"
    let id = fun x -> x
    let res = id 23.9
    res
*)
(
    (Let "myValue" (Str "yyyy")
    (Let "id" (Abs "x" (Var "x"))
    (Let "res" (App (Var "id") (Num 23.9))
    (Var "res"))))
)
|> showSolvedGraph []
|> renderDisplayClasses []



(*
    let add = fun a -> { field1 = a }
    let print = fun f -> f 42.0
    print add
*)

(
    (Let "add" (Abs "a" (Record [ "field1", Var "a" ]))
    (Let "print" (Abs "f" (App (Var "f") (Num 42.0)))
    (App (Var "print") (Var "add"))))
)
|> renderDisplayClasses [ ]



(*
    fun f -> f 42.0
*)
(Abs "f" (App (Var "f") (Num 42.0)))
|> renderDisplayClasses [ ]


