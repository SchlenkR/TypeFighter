module TypeFighter.Tests.Expect

open TypeFighter
open Expecto

let private fail msg =
    raise (AssertException msg)

let private error expected actual =
    let msg = $"type mismatch.     Expected :: {Format.tau expected}      Actual :: {Format.tau actual}"
    fail msg

let private infer env exp =
    let annoRes = AnnotatedAst.create env exp
    let res = annoRes |> ConstraintGraph.create |> ConstraintGraph.solve annoRes
    res.exprConstraintStates |> Map.find res.annotationResult.root

let inferType env expected exp =
    match infer (Map.ofList env) exp with
    | Constrained actual,_ ->
        let error() = error expected actual
        
        let varsInActual = Tau.getGenVars actual
        let varsInExpected = Tau.getGenVars expected

        if varsInActual.Count <> varsInExpected.Count then 
            error()

        let expected =
            let mappedVars = 
                (varsInActual, varsInExpected)
                ||> Seq.zip
                |> Seq.map (fun (a,e) -> { genTyVar = e; substitute = TGenVar a})
                |> Set.ofSeq
            ConstraintGraph.Unification.subst mappedVars expected

        if actual <> expected then
            error()
    | UnificationError e,_ -> fail $"ERROR ({e})"

let inferError env exp =
    match infer (Map.ofList env) exp with
    | Constrained c,_ -> fail $"Expected: ERROR, but was: {(Format.tau c)}"
    | _ -> ()
