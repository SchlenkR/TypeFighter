
#load "visuBase.fsx"
open VisuBase
open TestBase



let env1 = env [ map; add; numbers ]

(*
    let x = 10.0
    map Numbers (number ->
        add number x)
*)

(Let "x" (Num 10.0)
(Map (Var "Numbers") (Abs "number"
    (Appn (Var "add") [ Var "number"; Var "x" ] ))))
|> Test.isOfType "map numbers by add" env1 (seqOf numberTyp)
|> showSolvedAst env1




let env2 = env [ ]
(*
    let x = { a = 5.0; b = "hello" }
    x.b
*)

(Let "x" (Record [ ("a", Num 5.0); ("b", Str "hello") ])
(Prop "b" (Var "x")))
|> Test.isOfType "Get record property" env2 stringTyp
|> showSolvedAst env2




let env3 = env [ cons; emptyList ]

(*
    [ 1.0; 2.0; 3.0 ]   
*)

NewList [ Num 1.0; Num 2.0; Num 3.0 ]
|> Test.isOfType "Num list" env3 (seqOf numberTyp)
|> showSolvedAst env3

NewList [ Num 1.0; Num 2.0; Str "xxx" ]
|> Test.isError "Disjunct list element types" env3
|> showSolvedAst env3





let env4 = env [ add; tostring; mapp; filterp; cons; emptyList ]
(*
    [ 1.0 ] |> map (fun x -> tostring x)
*)

//(Pipe
//(NewList [ Num 1.0 ])
//(MapP (Abs "x" (App (Var "tostring") (Var "x") )))
//)
//|> showSolvedAst env4

//(App 
//    (FComp
//        (MapP (Abs "x" (App (Var "tostring") (Var "x") )))
//        (FilterP (Abs "x" True ))
//        )
//    (NewList [ Num 1.0 ])
//)
//|> showSolvedGraph env4
//|> showSolvedAst env4

//|> showLightAst env4
//|> showAnnotatedAst env4


MapP (Abs "x" (App (Var "tostring") (Var "x")))
|> Test.isOfType "Lambda applied to MapP" env4 (seqOf %1 ^-> seqOf stringTyp)
//|> showSolvedAst env4 |> fun x -> x.substs

(*
    x => tostring(x)
*)

(Abs "x" (App (Var "tostring") (Var "x")))
|> Test.isOfType "Lambda with anon type" env4 (%1 ^-> stringTyp)
|> showSolvedAst env4





// Polymorphic let
let env5 = env []
(*
    let id = fun x -> x
    (id "Hello World", id 42.0)
*)

(Let "id" (Abs "x" (Var "x"))
(Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ])
)
|> Test.isOfType "Polymorphic let" (env5) (stringTyp * numberTyp)
|> showSolvedAst env5


(App
(Abs "id" (Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ]))
(Abs "x" (Var "x")))
|> Test.isError "No polymorphic abs" env5
|> showSolvedAst env5




let env6 = env [ add; mul; sub; div; tostring ]

let mul2 = App (Var "mul") (Num 2.0)
let add3 = App (Var "add") (Num 3.0)
let tostr = Var "tostring"

(App tostr
(App mul2
(App add3
(Num 10.0))))
|> Test.isOfType "Many abs / currying" env6 stringTyp
|> showSolvedAst env6




let env7 = env [ ]

(Let "x" (Num 7.0)
(Let "x" (Str "Hello")
(Var "x")))
|> Test.isOfType "Shadowing" env7 stringTyp
|> showSolvedAst env7




// TODO: Generic records / instanciation of generic records
let env8 = env [ ]
(Let "id" (Abs "x" (Record [ "whatever", Var "x" ] ))
(Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ])
)
//|> Test.isOfType "Generic records / instanciation of generic records" (env8) (stringTyp * numberTyp)
|> showSolvedAst env8


