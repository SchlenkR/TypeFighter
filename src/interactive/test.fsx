
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

















//// TODO: Generic records / instanciation of generic records
//let env8 = env [ ]
//(Let "id" (Abs "x" (Record [ "whatever", Var "x" ] ))
//(Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ])
//)
////|> showSolvedAst env8
//|> Test.inferType "Generic records / instanciation of generic records" (env8) (stringTyp * numberTyp)



