#load "../interactive/0_fsi.fsx"
open ``0_fsi``

#r "nuget: FsUnit, 6.0.0"
#load "TestHelper.fs"

open System.IO
open TypeFighter.Lang
open TypeFighter.Tools

let solve (env: (string * Typ) list) (expr: Expr) =
    do expr |> Visu.writeAnnotatedAst true None
    let solution = TypeFighter.Tests.TestHelper.solve env expr
    
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
            
            do expr |> Visu.writeAnnotatedAst true (Some res.solution)
        | Error err ->
            printfn $"Error:\n    {err}"
            printfn ""
            
            let lastSolverRun = 
                solution.solverRuns
                |> List.tryLast
                |> Option.map snd
                |> Option.defaultValue []
                |> TypeSystem.finalizeSolution
            do expr |> Visu.writeAnnotatedAst true (Some lastSolverRun)
    solution

let resolveDataPath (path: string) =
    let up = ".."
    Path.Combine(__SOURCE_DIRECTORY__, up, up, "data", path)
    |> Path.GetFullPath
