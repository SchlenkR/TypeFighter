module TypeFighter.Tests.TestHelper

open System.IO
open System.Reflection

open FsUnit
open NUnit.Framework

open TypeFighter

[<SetUpFixture>]
type InitMsgUtils() =
    inherit FSharpCustomMessageFormatter()

let solve (env: (string * Typ) list) maxSolverRuns (expr: Expr<_>) =
    Solver.solve env maxSolverRuns expr

let shouldSolveType (typ: Typ) (solution: Solver.SolverResult) =
    match solution.finalResult with
    | Ok res when res.typ <> Some typ -> failwithf $"Expected typ '{typ}', but got '{res.typ}'"
    | Ok _ -> ()
    | Error err -> failwithf $"{err}"

let shouldFail (solution: Solver.SolverResult) =
    match solution.finalResult with
    | Ok _ -> failwithf $"Expected failure, but got success"
    | Error err -> printfn $"Test failed as expected: {err}"

let resolveDataPath (path: string) =
    let up = ".."
    Path.Combine(Assembly.GetExecutingAssembly().Location, up, up, up, up, up, up, "data", path)
    |> Path.GetFullPath
