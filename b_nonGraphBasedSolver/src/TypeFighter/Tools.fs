namespace TypeFighter.Tools

open TypeFighter.Lang

module PrintHelper =
    open TypeFighter.Lang.TypeSystem

    let indent = "    "

    let printElements (elements: list<{| t1: string; t2: string; trivia: string |}>) widthColLeft widthColRight =
        match elements with
        | [] -> printfn $"{indent}<empty>"
        | _ -> 
            for e in elements do
                printfn $"""{indent}{e.t1.PadRight(widthColLeft, ' ')} = {e.t2.PadRight(widthColRight, ' ')}  {e.trivia}"""
    
    let printSolverRun (sr: SolverRun) =
        let constraints = 
            [ for c in sr.constraints do 
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
            let solutions = sr.solutionItems |> List.map (fun s -> { tvar = s.tvar; typ = Mono s.monoTyp })
            [ for s in solutions do 
                {| 
                    t1 = (TVar s.tvar).ToString()
                    t2 = s.typ.ToString()
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
        printElements constraints widthColLeft widthColRight
        printfn $""
        printfn $"  Solutions:"
        printElements solutions widthColLeft widthColRight
        printfn $""
        printfn $"  Records:"
        for r in sr.recordRefs do
            printfn $"""{indent}recordRef_({r.Key}) = {ShowTyp.Show("", r.Value)}"""
        printfn $""

    let printSolverRuns (solverRuns: SolverRun list) =
        for sr in solverRuns do printSolverRun sr


module PseudoCodeRendering =

    let rec renderExpr (expr: Expr) =
        let sb = System.Text.StringBuilder()
        do sb.AppendLine() |> ignore
        let rec renderExpr (indent: int) (expr: Expr) =
            let mkIndent indent = String.replicate indent "    "
            let append (s: string) = sb.Append(s) |> ignore
            let newLineIndent addedIndent =
                sb.AppendLine().Append(mkIndent (indent + addedIndent))
                |> ignore
            match expr with
            | Expr.Lit s ->
                match System.Int32.TryParse s.value with
                | true, _ -> append s.value
                | _ ->
                    match s.value with
                    | s when s.Equals("true", System.StringComparison.OrdinalIgnoreCase) -> append "true"
                    | s when s.Equals("false", System.StringComparison.OrdinalIgnoreCase) -> append "false"
                    | s -> append ("\"" + s + "\"")
            | Expr.App x ->
                renderExpr indent x.func
                append " ("; renderExpr indent x.arg; append ")"
            | Expr.Fun x ->
                append $"fun {x.ident.identName} ->"
                newLineIndent 1
                renderExpr (indent + 1) x.body
            | Expr.Var x ->
                match x.ident with
                | BuiltinValues.unitValueIdent -> append "()"
                | _ -> append x.ident
            | Expr.Let x ->
                append $"let {x.ident.identName} = "; renderExpr indent x.value
                newLineIndent 0; renderExpr indent x.body
            | Expr.Do x ->
                append "do ";  renderExpr indent x.action
                newLineIndent 0
                renderExpr indent x.body
            | Expr.Match x ->
                append $"match "; renderExpr indent x.expr; append $" with"
                for case in x.cases do
                    newLineIndent 0; append $"| {case.disc} ->"
                    newLineIndent 1; renderExpr (indent + 1) case.body
            | Expr.PropAcc x ->
                renderExpr indent x.source
                append x.ident.identName
            | Expr.MkArray x ->
                append "["
                for item in x.values do
                    newLineIndent 1; renderExpr (indent + 1) item
                newLineIndent 0
                append "]"
            | Expr.MkRecord x ->
                append "{"
                for field in x.fields do
                    newLineIndent 1; append $"{field.fname} = "
                    renderExpr (indent + 1) field.value
                newLineIndent 0
                append "}"
        do renderExpr 0 expr
        sb.AppendLine() |> ignore
        sb.ToString()

    let print (f: ExprCtx -> Expr) =
        f (ExprCtx()) |> renderExpr |> printfn "%s"

