#load "../_fsiBase.fsx"
open ``_fsiBase``

#r "nuget: FsUnit, 6.0.0"
#load "TestHelper.fs"

open System.IO
open TypeFighter.Lang
open TypeFighter.Tools

let solve
    (externalEnv: (string * Typ) list) 
    maxSolverRuns

    (expr: Expr<unit>)
    =

    let solverResult = TypeFighter.Tests.TestHelper.solve externalEnv maxSolverRuns expr 
    
    do
        printfn "SOLVER RUNS"
        printfn "==========="
        PrintHelper.printSolverRuns solverResult.solverRuns
        printfn ""

    do
        match solverResult.finalResult with
        | Ok res -> 
            printfn $"Final type:\n    {res.typ}"
            printfn ""

            do Visu.writeNumberedAst solverResult.numberedExpr res.solution solverResult.exprToEnv
        | Error err ->
            printfn $"Error:\n    {err}"
            printfn ""
            
            let finalSolution =
                match solverResult.solverRuns |> List.tryLast with
                | Some lastSolverRun -> lastSolverRun.solution
                | None -> []

            do Visu.writeAst expr finalSolution solverResult.exprToEnv
    solverResult

let writeInitialAst (expr: Expr<unit>) =
    do Visu.writeAst expr [] Map.empty

let resolveDataPath (path: string) =
    let up = ".."
    Path.Combine(__SOURCE_DIRECTORY__, up, up, "data", path)
    |> Path.GetFullPath
