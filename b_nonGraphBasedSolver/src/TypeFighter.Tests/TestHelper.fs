module TypeFighter.Tests.TestHelper

open System.IO
open System.Reflection

open FsUnit
open NUnit.Framework

open TypeFighter.Lang
open TypeFighter.Lang.Services

[<SetUpFixture>]
type InitMsgUtils() =
    inherit FSharpCustomMessageFormatter()

let solve (env: (string * Typ) list) (expr: Expr) =
    solve env expr

let shouldSolveType (typ: Typ) (solution: SolveResult) =
    match solution.result with
    | Ok res ->
        if res.finalTyp <> typ then
            failwithf $"Expected typ '{typ}', but got '{res.finalTyp}'"
    | Error err ->
        failwithf $"Cannot solve: {err}"

let shouldFail (solution: SolveResult) =
    match solution.result with
    | Ok _ -> failwithf $"Expected failure, but got success"
    | _ -> ()

let resolveDataPath (path: string) =
    let up = ".."
    Path.Combine(Assembly.GetExecutingAssembly().Location, up, up, up, up, up, up, "data", path)
    |> Path.GetFullPath
