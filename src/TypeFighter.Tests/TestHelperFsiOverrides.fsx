#load "../fsiBase.fsx"
open FsiBase

#r "nuget: FsUnit, 6.0.0"
#load "TestHelper.fs"

open System.IO
open TypeFighter

let solve
    (externalEnv: (string * Typ) list) 
    maxSolverRuns

    (expr: Expr<unit>)
    =

    let solution = TypeFighter.Tests.TestHelper.solve externalEnv maxSolverRuns expr 
    
    do
        printfn "SOLVER RUNS"
        printfn "==========="
        SolverRunPrinter.printSolverRuns solution.solverRuns
        printfn ""

    do
        match solution.result with
        | Ok res -> 
            printfn $"Final type:\n    {res.typ}"
            printfn ""
        | Error err ->
            printfn $"Error:\n    {err}"
            printfn ""

    do
        Visu.writeSolverRuns solution

    solution

let writeInitialAst (expr: Expr<unit>) =
    do Visu.writeAst expr [] Map.empty

let resolveDataPath (path: string) =
    let up = ".."
    Path.Combine(__SOURCE_DIRECTORY__, up, up, "data", path)
    |> Path.GetFullPath
