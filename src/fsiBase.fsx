#load "./TypeFighter/Utils.fs"
#load "./TypeFighter/Lang.fs"
#load "./TypeFighter/ExpressionDsl.fs"

open System.IO
open System.Text.Json
open TypeFighter

module SolverRunPrinter =
    open TypeFighter.TypeSystem

    let indent = "    "

    let printEquations (elements: list<{| t1: string; t2: string; trivia: string |}>) widthColLeft widthColRight =
        let sb = System.Text.StringBuilder()
        match elements with
        | [] -> sb.AppendLine($"{indent}<empty>") |> ignore
        | _ -> 
            for e in elements do
                sb.AppendLine($"""{indent}{e.t1.PadRight(widthColLeft, ' ')} = {e.t2.PadRight(widthColRight, ' ')}  {e.trivia}""") |> ignore
        sb.ToString()

    let printSolverRun (sr: SolverRun) =
        let constraints =
            [
                for c in sr.constraints do
                    {| 
                        t1 = c.t1.ToString()
                        t2 = c.t2.ToString()
                        trivia =
                            let (VarNum var) = c.triviaSource.TVar
                            let sources = ($"{var}            ")[.. 10]
                            $"   :::  {sources}"
                    |} 
            ]
        let solutions = 
            [
                for solutionItem in sr.substitutions do 
                    {| 
                        t1 = (TVar solutionItem.tvar).ToString()
                        t2 = solutionItem.typ.ToString()
                        trivia = ""
                    |} 
            ]
        let widthColLeft,widthColRight =
            let getColWidth (getter: _ -> string) =
                let getColWidth es (getter: _ -> string) = 
                    es
                    |> List.map (fun e -> (getter e).Length)
                    |> List.fold max 0
                max (getColWidth constraints getter) (getColWidth solutions getter)
            getColWidth (fun x -> x.t1), getColWidth (fun x -> x.t2)
        
        printfn $""
        printfn $""
        printfn $""
        printfn $""
        printfn $"Solver run {sr.cycle}"
        printfn $""
        printfn $"  Constraints:"
        printfn "%s" (printEquations constraints widthColLeft widthColRight)
        printfn $""
        printfn $"  Solutions:"
        printfn "%s" (printEquations solutions widthColLeft widthColRight)
        printfn $""
        printfn $"  Records:"
        for r in sr.recordRefs do
            printfn $"""{indent}recordRef_({r.Key}) = {ShowTyp.Show("", r.Value)}"""
        printfn $""

    let printSolverRuns (solverRuns: SolverRun list) =
        for sr in solverRuns do
            printSolverRun sr

module Visu =

    type JsNode =
        {
            key: int
            name: string
            varNum: int
            code: string
            additionalInfo: string
            exprTyp: string
            env: JsEnvItem list
            children: JsNode list
        }

    and JsEnvItem =
        {
            ident: string
            solvedTyp: string
            origin: string
        }

    type JsEquation =
        {
            t1: string
            t2: string
        }

    type JsRecordRef =
        {
            key: int
            fields: {| name: string; typ: string |} list
        }

    type JsSolverRun =
        {
            constraints: JsEquation list
            solutions: JsEquation list
            recordRefs: JsRecordRef list
            error: string option
        }

    let createJsNode
        (root: Expr<VarNum>)
        (substitutions: TypeSystem.Substitution list)
        (exprToEnv: Map<Expr<VarNum>, Env>)
        =
        // let rootEnv =
        //     exprToEnv
        //     |> Map.tryFind root
        //     |> Option.defaultValue Map.empty
        // printfn $"""Root-Env = {rootEnv.Keys |> String.concat ", "}"""

        let rec createNodes (expr: Expr<_>) =
            let tryGetExprTyp tvar =
                substitutions 
                |> List.tryFind (fun s -> s.tvar = tvar)
                |> Option.map (_.typ.ToString())
            let getIdentDetails (ident: Ident<_>) =
                let typ = tryGetExprTyp ident.tvar |> Option.defaultValue ""
                $"{ident.identName}: {ident.tvar}"
            let env =
                exprToEnv
                |> Map.tryFind expr
                |> Option.map (fun env ->
                    env
                    |> Seq.map (fun kvp -> 
                        match kvp.Value with 
                        | EnvItem.External t -> 
                            { 
                                ident = kvp.Key
                                solvedTyp = t.ToString()
                                origin = "EXTERNAL"
                            }
                        | EnvItem.Internal t ->
                            { 
                                ident = kvp.Key
                                solvedTyp = tryGetExprTyp t |> Option.defaultValue $"{t}"
                                origin = ""
                            }
                    )
                    |> Seq.toList)
                |> Option.defaultValue []
            let createExprNode name code additionalInfo children =
                let node : JsNode =
                    {
                        key = let (VarNum x) = expr.TVar in x
                        name = name
                        varNum = let (VarNum x) = expr.TVar in x
                        code = code
                        additionalInfo = additionalInfo
                        exprTyp = tryGetExprTyp expr.TVar |> Option.defaultValue ""
                        env = env
                        children = children
                    }
                node
            match expr with
            | Expr.Lit x ->
                let litTyp,litValue = 
                    match x.value with 
                    | Number value -> "number", (value.ToString())
                    | String value -> "string", "'" + value + "'"
                    | Boolean value -> "boolean", (value.ToString())
                createExprNode "LIT" $"{litValue}" "" []
            | Expr.Var x ->
                createExprNode "VAR" $"{x.ident}" "" []
            | Expr.App x ->
                createExprNode "APP" "" "" [ createNodes x.func; createNodes x.arg ]
            | Expr.Fun x ->
                let details = getIdentDetails x.ident
                createExprNode "FUN" $"{x.ident.identName} -> ..." details [ createNodes x.body ]
            | Expr.Let x ->
                let details = getIdentDetails x.ident
                createExprNode "LET" $"({x.ident.identName}: {x.ident.tvar}) = ..." details [ createNodes x.value; createNodes x.body ]
            | Expr.Do x ->
                createExprNode "DO" "..." "" [ createNodes x.action; createNodes x.body ]
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
                createExprNode "MATCH-DU" "..." $"cases =\n{caseNames}\n" [ createNodes x.expr; for c in x.cases do createNodes c.body ]
            | Expr.PropAcc x ->
                createExprNode "PROP-ACC" $"_.{x.ident.identName}" $"var(ident) = {x.ident.tvar}" [ createNodes x.source ]
            | Expr.MkArray x ->
                createExprNode "MK-ARRAY" "" "" [ for e in x.values do createNodes e ]
            | Expr.MkRecord x ->
                let fieldNames = 
                    x.fields 
                    |> List.map (fun f -> $"{f.fname}: {f.value.TVar}")
                    |> String.concat "; " 
                    |> sprintf "{ %s }"
                createExprNode "MK-RECORD" "" $"fields = {fieldNames}" [ for f in x.fields do createNodes f.value ]
        createNodes root

    let writeVisuData (runs: (JsNode * JsSolverRun) list) =
        let treesForSolverRuns =
            runs
            |> List.map fst
            |> fun v -> JsonSerializer.Serialize(v, JsonSerializerOptions(WriteIndented = true))
        
        let solverRuns =
            runs
            |> List.map snd
            |> fun v -> JsonSerializer.Serialize(v, JsonSerializerOptions(WriteIndented = true))
        
        let json = $"
window.treesForSolverRuns = {treesForSolverRuns};
window.solverRuns = {solverRuns};
        "

        let dataPath = Path.Combine(__SOURCE_DIRECTORY__, "visu/data/data.js")
        File.WriteAllText(dataPath, json)

    let writeNumberedAst
        (root: Expr<VarNum>)
        (substitutions: TypeSystem.Substitution list)
        (exprToEnv: Map<Expr<VarNum>, Env>)
        =
        createJsNode root substitutions exprToEnv
        |> fun node -> node, { constraints = []; solutions = []; recordRefs = []; error = None }
        |> List.singleton
        |> writeVisuData

    let writeSolverRuns (solution: Solver.Solution) =
        solution.solverRuns
        |> List.map (fun sr ->
            let node = createJsNode solution.numberedExpr sr.substitutions solution.exprToEnv
            let jsSolverRun =
                {
                    constraints =
                        sr.constraints
                        |> List.map (fun c ->
                            {
                                t1 = c.t1.ToString()
                                t2 = c.t2.ToString()
                            })
                    solutions =
                        sr.substitutions
                        |> List.sortBy (fun s -> s.tvar)
                        |> List.map (fun s ->
                            {
                                t1 = (TVar s.tvar).ToString()
                                t2 = s.typ.ToString()
                            })
                    recordRefs =
                        sr.recordRefs
                        |> Map.toList
                        |> List.map (fun (key, fields) ->
                            {
                                key = key
                                fields =
                                    fields
                                    |> Set.toList
                                    |> List.map (fun field ->
                                        {|
                                            name = field.fname
                                            typ = field.typ.ToString()
                                        |})
                            })
                    error =
                        match solution.result with
                        | Ok _ -> None
                        | Error err -> Some err
                }
            node, jsSolverRun)
        |> writeVisuData

    let writeAst (root: Expr<unit>) solution exprToEnvMap = 
        let numGen = NumGen.mkGenerator ()
        writeNumberedAst (Expr.toNumberedExpr root numGen) solution exprToEnvMap
