
// why ref directly? Just to speed up load times a bit
// #r "nuget: Newtonsoft.Json"
#r @"../_deps/Newtonsoft.Json-13.0.3/net6.0/Newtonsoft.Json.dll"

#load "../TypeFighter/Utils.fs"
#load "../TypeFighter/Lang.fs"
#load "../TypeFighter/Tools.fs"

#load "./visu/visu.fsx"
open Visu

open TypeFighter.Lang

module Visu =

    let writeAnnotatedAst 
        (solution: option<TypeSystem.SolutionItem list>) 
        (exprToEnv: Map<Expr, Env>)
        (root: Expr)
        =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]

        let rec createNodes (expr: Expr) =
            let getIdentDetails (ident: Ident) = $"{ident.identName} : {ident.tvar}"
            let getExprTyp tvar =
                let unknown = "???"
                match solution with
                | None -> unknown
                | Some solution ->
                    solution 
                    |> List.tryFind (fun s -> s.tvar = tvar)
                    |> Option.map (_.typ.ToString())
                    |> Option.defaultValue unknown
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
                Tree.expr (VarNum.number expr.TVar) (getExprTyp expr.TVar) name env additionalInfo children
            match expr with
            | Lit x ->
                createExprNode $"""Lit ("{x.value}") """ "" []
            | Var x ->
                createExprNode $"""Var "{x.ident}" """ "" []
            | App x ->
                createExprNode "App" "" [ createNodes x.func; createNodes x.arg ]
            | Fun x ->
                let details = getIdentDetails x.ident
                createExprNode $"Fun {x.ident.identName} -> ..." details [ createNodes x.body ]
            | Let x ->
                let details = getIdentDetails x.ident
                createExprNode $"Let {x.ident.identName} = ..." details [ createNodes x.value; createNodes x.body ]
            | Do x ->
                createExprNode $"Do ..." "" [ createNodes x.action; createNodes x.body ]
            | Match x ->
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
            | PropAcc x ->
                createExprNode $"""PropAcc "{x.ident.identName}" """ $"var(ident) = {x.ident.tvar}" [ createNodes x.source ]
            | MkArray x ->
                createExprNode $"MkArray []" "" [ for e in x.values do createNodes e ]
            | MkRecord x ->
                let fieldNames = 
                    x.fields 
                    |> List.map (fun f -> $"{f.fname}: {f.value.TVar}")
                    |> String.concat "; " 
                    |> sprintf "{ %s }"
                createExprNode $"Record" $"fields = {fieldNames}" [ for f in x.fields do createNodes f.value ]
        
        do createNodes root |> flatten |> Tree.write

    let writeExpr (f: ExprCtx -> Expr) =
        f (ExprCtx()) |> writeAnnotatedAst None Map.empty
