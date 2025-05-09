#load "./TypeFighter/Utils.fs"
#load "./TypeFighter/Lang.fs"
#load "./TypeFighter/Tools.fs"

#load "./visu/visu.fsx"
open Visu

open TypeFighter.Lang

module Visu =

    let writeNumberedAst
        (root: Expr<VarNum>)
        (solution: option<TypeSystem.SolutionItem list>) 
        (exprToEnv: Map<Expr<VarNum>, Env>)
        = 
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]

        let rec createNodes (expr: Expr<_>) =
            let tryGetExprTyp tvar =
                match solution with
                | None -> None
                | Some solution ->
                    solution 
                    |> List.tryFind (fun s -> s.tvar = tvar)
                    |> Option.map (_.typ.ToString())
            let getExprTyp tvar =
                tryGetExprTyp tvar |> Option.defaultValue ""
            let getIdentDetails (ident: Ident<_>) =
                let typ = tryGetExprTyp ident.tvar |> Option.defaultValue ""
                $"IDENT = {ident.identName}   TVAR = {ident.tvar}   TYP = {typ}"
            let env =
                exprToEnv
                |> Map.tryFind expr
                |> Option.map (fun env ->
                    env
                    |> Seq.map (fun kvp -> 
                        match kvp.Value with 
                        | EnvItem.External _ -> None 
                        | EnvItem.Internal t -> Some (kvp.Key, t))
                    |> Seq.choose id
                    |> Seq.map (fun (ident, varNum) ->
                        let solvedTyp = getExprTyp varNum
                        $"{ident}: {varNum} ({solvedTyp})")
                    |> String.concat "; "
                )
                |> Option.defaultValue ""
            let createExprNode name additionalInfo children =
                Tree.expr (let (VarNum x) = expr.TVar in x) (getExprTyp expr.TVar) name env additionalInfo children
            match expr with
            | Expr.Lit x ->
                let litTyp,litValue = 
                    match x.value with 
                    | Number value -> "number", (value.ToString())
                    | String value -> "string", "'" + value + "'"
                    | Boolean value -> "boolean", (value.ToString())
                createExprNode $"""Lit ({litValue}) """ "" []
            | Expr.Var x ->
                createExprNode $"""Var "{x.ident}" """ "" []
            | Expr.App x ->
                createExprNode "App" "" [ createNodes x.func; createNodes x.arg ]
            | Expr.Fun x ->
                let details = getIdentDetails x.ident
                createExprNode $"Fun {x.ident.identName} -> ..." details [ createNodes x.body ]
            | Expr.Let x ->
                let details = getIdentDetails x.ident
                createExprNode $"Let {x.ident.identName} = ..." details [ createNodes x.value; createNodes x.body ]
            | Expr.Do x ->
                createExprNode $"Do ..." "" [ createNodes x.action; createNodes x.body ]
            | Expr.Match x ->
                let caseNames = 
                    [
                        for x in x.cases do
                            let binding =
                                match x.ident with
                                | Some ident -> $"as {ident.identName}: {ident.tvar} "
                                | None -> $""
                            $"    | {x.disc} {binding}-> ... ({x.body.TVar})"
                    ]
                    |> String.concat "\n"
                createExprNode $"MatchDU ..." $"cases =\n{caseNames}\n" [ createNodes x.expr; for c in x.cases do createNodes c.body ]
            | Expr.PropAcc x ->
                createExprNode $"""PropAcc "{x.ident.identName}" """ $"var(ident) = {x.ident.tvar}" [ createNodes x.source ]
            | Expr.MkArray x ->
                createExprNode $"MkArray []" "" [ for e in x.values do createNodes e ]
            | Expr.MkRecord x ->
                let fieldNames = 
                    x.fields 
                    |> List.map (fun f -> $"{f.fname}: {f.value.TVar}")
                    |> String.concat "; " 
                    |> sprintf "{ %s }"
                createExprNode $"Record" $"fields = {fieldNames}" [ for f in x.fields do createNodes f.value ]
        
        do createNodes root |> flatten |> Tree.write

    let writeAst (root: Expr<unit>) = 
        let numGen = NumGen.mkGenerator ()
        writeNumberedAst (Expr.toNumberedExpr root numGen)
