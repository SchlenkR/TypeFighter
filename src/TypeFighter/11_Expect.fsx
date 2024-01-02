#load "10_Api.fsx"

open ``01_Core``
open ``10_Api``

let private fail msg =
    failwith msg

let private error expected actual =
    let msg = $"type mismatch.     Expected :: {Format.tau expected}      Actual :: {Format.tau actual}"
    fail msg

let inferType env exp =
    let annoRes = AnnotatedAst.create (Map.ofList env) exp
    let res = annoRes |> ConstraintGraph.create |> ConstraintGraph.solve annoRes
    res.exprConstraintStates |> Map.find res.annotationResult.root

let assertIsType typ res =
    match res with
    | Constrained c,_ -> if c <> typ then error typ c
    | UnificationError e,_ -> fail $"ERROR ({e})"

let assertIsError typ res =
    match res with
    | Constrained c,_ -> fail $"Expected: ERROR, but was: {(Format.tau c)}"
    | _ -> ()

let assertInferredType env typ exp =
    inferType env exp
    |> assertIsType typ

let assertInferenceError env exp =
    inferType env exp
    |> assertIsError
