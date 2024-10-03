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
