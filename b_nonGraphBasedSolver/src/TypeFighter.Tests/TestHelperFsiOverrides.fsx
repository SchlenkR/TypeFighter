#load "../_fsiBase.fsx"
open ``_fsiBase``

#r "nuget: FsUnit, 6.0.0"
#load "TestHelper.fs"

open System.IO
open TypeFighter.Lang
open TypeFighter.Tools

let solve (externalEnv: (string * Typ) list) (expr: Expr<unit>) =
    do Visu.writeAst expr None Map.empty
    let solution = TypeFighter.Tests.TestHelper.solve externalEnv expr
    
    do
        printfn "SOLVER RUNS"
        printfn "==========="
        PrintHelper.printSolverRuns solution.solverRuns
        printfn ""

    do
        match solution.result with
        | Ok res -> 
            printfn $"Final type:\n    {res.finalTyp}"
            printfn ""

            do Visu.writeNumberedAst solution.numberedExpr (Some res.solution) solution.exprToEnv
        | Error err ->
            printfn $"Error:\n    {err}"
            printfn ""
            
            let finalSolution =
                let s =
                    match solution.solverRuns |> List.tryLast with
                    | Some lastSolverRun -> lastSolverRun
                    | None -> { cycle = 0; constraints = []; solutionItems = []; recordRefs = Map.empty }
                TypeSystem.finalizeSolution s.solutionItems s.recordRefs

            do Visu.writeAst expr (Some finalSolution) solution.exprToEnv
    solution

let writeInitialAst (expr: Expr<unit>) =
    do Visu.writeAst expr None Map.empty

let resolveDataPath (path: string) =
    let up = ".."
    Path.Combine(__SOURCE_DIRECTORY__, up, up, "data", path)
    |> Path.GetFullPath
