namespace TypeFighter.Tools

open TypeFighter.Lang

module PrintHelper =
    open TypeFighter.Lang.TypeSystem

    let printSolution (solutions: SolutionItem list) =
        let indent = "    "
        let printSolution (solution: SolutionItem) =
            printfn $"{indent}{TVar (solution.tvar)} = {solution.typ}"
        match solutions with
        | [] -> printfn $"{indent}<empty>"
        | _ -> for s in solutions do printSolution s
    
    let printConstraints (constraints: Constraint list) =
        let indent = "    "
        let printConstraint c =
            let (VarNum var) = c.triviaSource.TVar
            let sources = ($"{var}            ")[.. 10]
            printfn $"{indent}{sources}   :::   {c.t1} = {c.t2}"
        match constraints with
        | [] -> printfn $"{indent}<empty>"
        | _ -> for c in constraints do printConstraint c
    
    let printSolverRuns (solverRuns: list<Constraint list * MSolutionItem list>) =
        for i, (constraints, solutions) in solverRuns |> List.indexed do
            printfn $""
            printfn $"Solver run {i}"
            printfn $"  Constraints:"
            printConstraints constraints
            printfn $"  Solutions:"
            printSolution (solutions |> List.map (fun s -> { tvar = s.tvar; typ = Mono s.monoTyp }))
            printfn $""


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
            | Lit s ->
                match System.Int32.TryParse s.value with
                | true, _ -> append s.value
                | _ ->
                    match s.value with
                    | s when s.Equals("true", System.StringComparison.OrdinalIgnoreCase) -> append "true"
                    | s when s.Equals("false", System.StringComparison.OrdinalIgnoreCase) -> append "false"
                    | s -> append ("\"" + s + "\"")
            | App x ->
                renderExpr indent x.func
                append " ("; renderExpr indent x.arg; append ")"
            | Fun x ->
                append $"fun {x.ident.identName} ->"
                newLineIndent 1
                renderExpr (indent + 1) x.body
            | Var x ->
                match x.ident with
                | BuiltinValues.unitValueIdent -> append "()"
                | _ -> append x.ident
            | Let x ->
                append $"let {x.ident.identName} = "; renderExpr indent x.value
                newLineIndent 0; renderExpr indent x.body
            | Do x ->
                append "do ";  renderExpr indent x.value
                newLineIndent 0
                renderExpr indent x.body
            | Match x ->
                append $"match "; renderExpr indent x.expr; append $" with"
                for case in x.cases do
                    newLineIndent 0; append $"| {case.disc} ->"
                    newLineIndent 1; renderExpr (indent + 1) case.body
            | PropAcc x ->
                renderExpr indent x.source
                append x.ident.identName
            | MkArray x ->
                append "["
                for item in x.values do
                    newLineIndent 1; renderExpr (indent + 1) item
                newLineIndent 0
                append "]"
            | MkRecord x ->
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

