
#load "../interactive/0_fsi.fsx"
open ``0_fsi``

open TypeFighter.Lang
open TypeFighter.Tools


(*
    let x = 10
    let y = 20
    add x y
*)

Visu.writeExpr <| fun x ->
// PseudoCodeRendering.print <| fun x ->
    x.Let (x.Ident "x") (x.Lit "10") (
        x.Let (x.Ident "y") (x.Lit "20") (
            x.App (x.App (x.Var "add") (x.Var "x")) (x.Var "y")
    ))




(*
    let myRecord = 
        { 
            birthDate = addDays now -365
            name = "John"
        }
    match isValid myRecord.birthDate with
    | true -> "valid"
    | false ->
        LogError "invalid birth date"
        "invalid"
*)

// Visu.writeExpr <| fun x ->
PseudoCodeRendering.print <| fun x ->
    x.Let (x.Ident "myRecord") (
        x.Record [
            x.Field "birthDate" (x.App (x.App (x.Var "addDays") (x.Var "now")) (x.Lit "-365"))
            x.Field "name" (x.Lit "John")
        ]) (
    x.Match (x.PropAccN [ "isValid"; "myRecord"; "birthDate" ]) [
        x.Case "true" None (x.Lit "valid")
        x.Case "false" None (
            x.Do (x.App (x.Var "LogError") (x.Lit "invalid birth date")) (x.Lit "invalid"))
    ])




(*
    let x = { a = 10; b = 20 }
    let y = x.a
    let z = x.b
    add y z
*)

// Visu.writeExpr <| fun x ->
PseudoCodeRendering.print <| fun x ->
    x.Let (x.Ident "x") (
        x.Record [
            x.Field "a" (x.Lit "10")
            x.Field "b" (x.Lit "20")
        ]) (
    x.Let (x.Ident "y") (x.PropAcc (x.Var "x") "a") (
    x.Let (x.Ident "z") (x.PropAcc (x.Var "x") "b") (
        x.App (x.App (x.Var "add") (x.Var "y")) (x.Var "z")
    )))




let xmlFile = __SOURCE_DIRECTORY__ + "/../data/wf.xml"
System.Xml.Linq.XDocument.Load(xmlFile).Root
|> WfXml.parse
|> WfAst.build (ExprCtx())
// |> _.ast
// |> PseudoCodeRendering.renderExpr |> printfn "%s"
// |> Visu.writeAnnotatedAst true


