
#load "./visu/visu.fsx"

type Lit = { typeName: string; value: string }

type Exp =
    | ELit of Lit
    | EVar of string
    | EApp of Exp * Exp
    | EFun of string * Exp
    | ELet of string * Exp * Exp

type TyVar = int
type Ident = string
type Env = Map<Ident, TyVar>

type Annotated<'var, 'expr> =
    { annotated: 'expr
      tyvar: 'var
      env: Env }

type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of Annotated<TyVar, TExp> * Annotated<TyVar, TExp>
    | TEFun of Annotated<TyVar, Ident> * Annotated<TyVar, TExp>
    | TELet of Ident * Annotated<TyVar, TExp> * Annotated<TyVar, TExp>

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some tyvar)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound."
        | Some t -> t

module Infer =
    type private Newvar() =
        let mutable varCounter = -1
        member this.fresh() =
            varCounter <- varCounter + 1
            varCounter

    let annoExp (env: Env) (exp: Exp) =
        let newvar = Newvar()
        let rec annoExp (env: Env) (exp: Exp) =
            { tyvar = newvar.fresh()
              annotated =
                  match exp with
                  | ELit x ->
                      TELit x
                  | EVar ident ->
                      TEVar ident
                  | EApp (e1, e2) ->
                      TEApp (annoExp env e1, annoExp env e2)
                  | EFun (ident, body) ->
                      let tyvarIdent = newvar.fresh()
                      let newEnv = env |> Env.bind ident tyvarIdent
                      let annotatedIdent = { annotated = ident; tyvar = tyvarIdent; env = env }
                      TEFun (annotatedIdent, annoExp newEnv body)
                  | ELet (ident, e, body) ->
                      let newEnv = env |> Env.bind ident (newvar.fresh())
                      TELet (ident, annoExp env e, annoExp newEnv body)
              env = env }
        annoExp Env.empty exp

    type Constraint =
        | CAny
        | CBaseType of string
        | CFun of Constraint * Constraint
    
    type GraphNode =
        { tyvar: TyVar
          inputs: ResizeArray<Constraint> }

    //let constrainExp (annoExp: Annotated<TyVar, TExp>) =
    //    let nodes = ResizeArray<GraphNode>()
    //    let rec constrainExp (annoExp: Annotated<TyVar, TExp>) =
    //        match annoExp.annotated with
    //        | TELit x ->
    //            let node =
    //                let constraints = [ CBaseType x.typeName ]
    //                { tyvar = annoExp.tyvar
    //                  inputs = ResizeArray(constraints) }
    //            nodes.Add node
    //            node
    //        | TEVar ident ->
    //            let node =
    //                { tyvar = annoExp.tyvar
    //                  inputs = ResizeArray() }
    //            nodes.Add node
    //            node
    //        | TEApp (e1, e2) ->
    //            let c1 = constrainExp e1
    //            let c2 = constrainExp e2
                
    //            c1.inputs.Add (CFun ())

    //            let node =
    //                let constraints = [
    //                ]
    //                { tyvar = annoExp.tyvar
    //                  inputs = ResizeArray() }
    //            nodes.Add node
                
    //            Node.var $"App" (showTyvar annoExp.tyvar) [ child1; child2 ]
    //        | TEFun (ident, body) ->
    //            let newEnv = env |> Env.bind ident.annotated ident.tyvar
    //            let child = createNodes newEnv body

    //            Node.var
    //                $"fun ({ident.annotated}{showTyvar ident.tyvar}) -> [e{showTyvar body.tyvar}]"
    //                (showTyvar annoExp.tyvar)
    //                [child]
    //        | TELet (ident, e, body) ->
    //            let child1 = createNodes env e

    //            let newEnv = env |> Env.bind ident e.tyvar
    //            let child2 = createNodes newEnv body

    //            Node.var
    //                $"let {ident} = [e1{showTyvar e.tyvar}] in [e2{showTyvar body.tyvar}]"
    //                (showTyvar annoExp.tyvar)
    //                [ child1; child2 ]


module Show =
    open Visu

    let annotated (annoExp: Annotated<TyVar, TExp>) =
        let showTyvar (ident: string) (tyvar: TyVar) =
            $"{{{ident}: {tyvar}}}"

        let showTyvarAndEnv annoExp =
            let envVars =
                match annoExp.env |> Map.toList with
                | [] -> "[ ]"
                | [(ident, tyvar)] -> $"[ {showTyvar ident tyvar} ]"
                | _ ->
                    [ for x in annoExp.env do $"-  {showTyvar x.Key x.Value}" ]
                    |> String.concat "\n"
                    |> fun s -> $"[\n{s} ]"
            ($"var = {annoExp.tyvar}") + "\nenv = " + envVars
        
        let rec createNodes (annoExp: Annotated<TyVar, TExp>) =
            match annoExp.annotated with
            | TELit x ->
                Node.var $"Lit ({x.value}: {x.typeName})" (showTyvarAndEnv annoExp) []
            | TEVar ident ->
                let tyvar = Env.resolve ident annoExp.env                
                Node.var $"Var {showTyvar ident tyvar}" (showTyvarAndEnv annoExp) []
            | TEApp (e1, e2) ->
                let child1 = createNodes e1
                let child2 = createNodes e2
                
                Node.var $"App" (showTyvarAndEnv annoExp) [ child1; child2 ]
            | TEFun (ident, body) ->
                let child = createNodes body

                Node.var
                    $"""fun {showTyvar ident.annotated ident.tyvar} -> {showTyvar "e" body.tyvar}"""
                    (showTyvarAndEnv annoExp)
                    [child]
            | TELet (ident, e, body) ->
                let child1 = createNodes e
                let child2 = createNodes body

                Node.var
                    $"""let {ident} = {showTyvar "e1" e.tyvar} in {showTyvar "e2" body.tyvar}"""
                    (showTyvarAndEnv annoExp)
                    [ child1; child2 ]

        let node = createNodes annoExp
        let flattened =
            let rec flatten (node: Node) =
                [
                    yield node
                    for c in node.children do
                        yield! flatten c
                ]
            flatten node
        
        openGraph flattened









[<AutoOpen>]
module Dsl =
    let knownBaseTypes =
        {| int = "Int"
           float = "Float"
           string = "String" |}
           
    let cint (x: int) = ELit { typeName = knownBaseTypes.int; value = string x }
    let cfloat (x: float) = ELit { typeName = knownBaseTypes.float; value = string x }
    let cstr (x: string) = ELit { typeName = knownBaseTypes.string; value = x }

//let env : Env = Map.ofList [
//    "libcall_add", tfun(tint, tfun(tint, tint))
//    ]
let env = Env.empty

let showAnnotated exp =
    Infer.annoExp env exp
    |> Show.annotated

let idExp = EFun("x", EVar "x")




// polymorphic let
ELet("f", idExp,
    ELet("res1", EApp(EVar "f", cint 99),
        ELet("res2", EApp(EVar "f", cstr "HelloWorld"),
            EVar("res2")
)))
|> showAnnotated


//showAnnotated idExp
//showAnnotated <| ELet("id", idExp, EApp(EVar "id", cstr "sss"))
//showAnnotated <| EApp(idExp, cstr "sss")

//showAnnotated <| cint 43 
//showAnnotated <| ELet("hurz", cint 43, cstr "sss") 
//showAnnotated <| ELet("id", idExp, EApp(EVar "id", cstr "sss"))
//showAnnotated <| EFun("x", cstr "klököl")
//showAnnotated <| EApp(EFun("x", EVar "x"), cint 2)
//showAnnotated <| EApp(EFun("x", EVar "x"), cstr "Hello")

//// unbound var "y":
//showAnnotated <| EFun("x", EFun("y", EVar "x"))

//showAnnotated <| ELet("k", EFun("x", ELet("f", EFun("y", EVar "x"), EVar "f")), EVar "k")
//showAnnotated <| ELet("k", cint 43, ELet("k", cstr "sss", EVar "k"))


//let expr1 = cint 42
//let expr2 = ELet("hurz", cint 43, cint 32)
//let expr3 =
//    let addA = EFun("a", EApp(EVar "libcall_add", EVar "a"))
//    let addB = EFun("b", EApp(addA, EVar "b"))
//    ELet("hurz", cint 43, ELet("f", addB, EApp(EApp(EVar "f", EVar "hurz"), cint 99)))
