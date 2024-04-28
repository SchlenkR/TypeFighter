#load "../TypeFighter/Utils.fs"
#load "../TypeFighter/Lang.fs"
#load "../TypeFighter/Tools.fs"

#load "./visu/visu.fsx"
open Visu

open TypeFighter.Utils
open TypeFighter.Lang

module Visu =

    module Format =
        let rec longTyp (typ: Typ) =
            let indent = "\u00AD     "
            let getNameHint nameHint =
                match nameHint with
                | Anonymous -> ""
                | Named name -> $" // name = {name}"
            match typ with
            | Mono typ ->
                match typ with
                | TProvideMembers record ->
                    [ for f in record.fields do
                        $"{indent}{f.fname}: {f.typ}" ]
                    |> String.concat "\n"
                    |> fun s -> $"{{{getNameHint record.nameHint} \n{s} }}"
                | TProvideCases union ->
                    [ for c in union.cases do
                        $"{indent}{c.disc}: ..." ]
                    |> String.concat "\n"
                    |> fun s -> $"{{{getNameHint union.nameHint} \n{s} }}"
                | _ -> typ.ToString()
            | _ -> typ.ToString()
    
    let writeAnnotatedAst 
            (showVar: bool) 
            // (showEnv: bool)
            // (showSubsts: bool)
            (solution: option<TypeSystem.SolutionItem list>)
            (root: Expr)
            // (exprConstraintStates: Map<TExp, ConstraintState * Set<Subst>>)
            // (envConstraintStates: Map<IExp, ConstraintState * Set<Subst>>)
        =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
        let rec createNodes (expr: Expr) =
            let details =
                [
                    // let constrSubsts = lazy (exprConstraintStates |> Map.find expr)

                    if showVar then
                        yield expr.TVar.ToString()
                    match solution with
                    | None -> ()
                    | Some solution ->
                        let typ = 
                            solution 
                            |> List.tryFind (fun s -> s.tvar = expr.TVar)
                            |> Option.map (_.typ)
                            |> Option.map Format.longTyp
                            |> Option.defaultValue "???"
                        yield $"typ = {typ}"
                    // if showSubsts then
                    //     yield $"substs = {Format.substs (snd constrSubsts.Value)}"
                    // if showEnv then
                    //    yield $"env = {Format.env exp.meta envConstraintStates}"
                ]
                |> String.concat "\n"
            let getIdentDetails (ident: Ident) =
                $"{ident.identName} : {ident.tvar} \n{details}"
            match expr with
            | Lit x ->
                Tree.var $"""Lit ("{x.value}") """ details []
            | Var x ->
                Tree.var $"""Var "{x.ident}" """ details []
            | App x ->
                Tree.var "App" details [ createNodes x.func; createNodes x.arg ]
            | Fun x ->
                let details = getIdentDetails x.ident
                Tree.var $"Fun {x.ident.identName} -> ..." details [ createNodes x.body ]
            | Let x ->
                let details = getIdentDetails x.ident
                Tree.var $"Let {x.ident.identName} = ..." details [ createNodes x.value; createNodes x.body ]
            | Do x ->
                Tree.var $"Do ..." details [ createNodes x.value; createNodes x.body ]
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
                let details = $"cases =\n{caseNames}\n\n{details}"
                Tree.var $"MatchDU ..." details [ createNodes x.expr; for c in x.cases do createNodes c.body ]
            | PropAcc x ->
                let details = $"var(ident) = {x.ident.tvar}\n{details}" 
                Tree.var $"""PropAcc "{x.ident.identName}" """ details [ createNodes x.source ]
            | MkArray x ->
                Tree.var $"MkArray []" details [ for e in x.values do createNodes e ]
            | MkRecord x ->
                let fieldNames = 
                    x.fields 
                    |> List.map (fun f -> $"{f.fname}: {f.value.TVar}")
                    |> String.concat "; " 
                    |> sprintf "{ %s }"
                let details = $"fields = {fieldNames}\n{details}"
                Tree.var $"Record" details [ for f in x.fields do createNodes f.value ]
        
        do createNodes root |> flatten |> Tree.write

    let writeExpr (f: ExprCtx -> Expr) =
        f (ExprCtx()) |> writeAnnotatedAst true None

