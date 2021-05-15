module TypeFighter.Tests.Expect

open TypeFighter
open Expecto

let private fail msg =
    raise (AssertException msg)

let private error expected actual =
    let msg = $"type mismatch.\nExpected:\n    {Format.tau expected}\nActual:\n    {Format.tau actual}"
    fail msg

let private infer env exp =
    let annoRes = AnnotatedAst.create env exp
    let res = annoRes |> ConstraintGraph.create |> ConstraintGraph.solve annoRes
    res.exprConstraintStates |> Map.find res.annotationResult.root

let inferType env typ exp =
    match infer (Map.ofList env) exp with
    | Constrained c,_ -> if c <> typ then error typ c
    | UnificationError e,_ -> fail $"ERROR ({e})"

let inferError env exp =
    match infer (Map.ofList env) exp with
    | Constrained c,_ -> fail $"Expected: ERROR, but was: {(Format.tau c)}"
    | _ -> ()
