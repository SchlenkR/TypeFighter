
#load "testBase.fsx"
open TypeFighter
open TypeFighter.CodeGen
open TestBase

//#r "nuget: Basic.Reference.Assemblies"
//#r "nuget: Microsoft.CSharp"
//#r "nuget: Microsoft.CodeAnalysis"
//#r "nuget: Microsoft.CodeAnalysis.CSharp"

let solve env exp =
    let annoRes = AnnotatedAst.create env exp
    let nodes = annoRes.resultExp |> ConstraintGraph.create
    let res = ConstraintGraph.solve annoRes.newGenVar nodes
    do ConstraintGraph.applyResult annoRes.resultExp res.allNodes
    annoRes.resultExp
let render env exp =
    exp
    |> solve env 
    |> CsCodeGen.render 
    |> fun res ->
        printfn "------------------"
        printfn ""
        printfn "%s" res.records
        printfn ""
        printfn "%s" res.body
        printfn ""
        printfn "------------------"





let env1 = env [ map; add; numbers ]

(*
let x = 10.0
map Numbers (number ->
    add number x)
*)

(Let "x" (Num 10.0)
(Map (Var "Numbers") (Abs "number"
    (Appn (Var "add") [ Var "number"; Var "x" ] ))))
|> render env1






let env8 = env [ ]

(Let "id" (Abs "x" (Record [ "whatever", Var "x" ] ))
(Record [ "myString", App (Var "id") (Str "Hello World"); "myNUmber", App (Var "id") (Num 42.0) ])
)
//|> Test.isOfType "Generic records / instanciation of generic records" (env8) (stringTyp * numberTyp)
|> render env8


