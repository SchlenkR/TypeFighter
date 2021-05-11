
#load "visuBase.fsx"
open TypeFighter
open TypeFighter.DotNetCodeGen
open TestBase
open VisuBase



let env1 = env [ map; add; numbers ]

(*
let x = 10.0
map Numbers (number ->
    add number x)
*)

(Let "x" (Num 10.0)
(MapX (Var "Numbers") (Abs "number"
    (Appn (Var "add") [ Var "number"; Var "x" ] ))))
|> renderDisplayClasses env1
//|> render env1





(*
    let id = fun x -> { whatever = 23.0 }
    { myString = id "Hello World"; myNumber = id 42.0 }
*)
let env2 = env [ ]

(Let "id" (Abs "x" (Record [ "whatever", Num 23.0 ] ))
(Record [ "myString", App (Var "id") (Str "Hello World"); "myNumber", App (Var "id") (Num 42.0) ])
)
|> renderDisplayClasses env2



(*
    let id = fun x -> x
    let add = fun a -> fun b -> { a = a; b = b }
    add "Hello" (id 42.0)
*)
let env3 = env [ ]

(Let "id" (Abs "x" (Var "x"))
(Let "add" (Abs "a" (Abs "b" (Record [ "field1", Var "a"; "field2", Var "b" ])))
(App (App (Var "add") (Str "Hello")) (App (Var "id") (Num 42.0)))
))
|> renderDisplayClasses env3

//|> solve env3 |> fun res -> res.substs
//|> showSolvedAst env3



(*
    let add = fun a -> { field1 = a }
    let print = fun f -> f 42.0
    print add
*)
let env4 = env [ ]

(Let "add" (Abs "a" (Record [ "field1", Var "a" ]))
(Let "print" (Abs "f" (App (Var "f") (Num 42.0)))
(App (Var "print") (Var "add"))
))
|> showSolvedAst env4
|> renderDisplayClasses env4

(Abs "f" (App (Var "f") (Num 42.0)))
|> showSolvedAst env4
|> renderDisplayClasses env4

//|> solve env3 |> fun res -> res.substs
//|> showSolvedAst env3


