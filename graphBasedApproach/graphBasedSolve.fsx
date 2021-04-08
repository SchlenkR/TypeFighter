
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

type Annotated<'var, 'expr> =
    { annotated: 'expr
      tvar: 'var }

type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of Annotated<TyVar, TExp> * Annotated<TyVar, TExp>
    | TEFun of Annotated<TyVar, Ident> * Annotated<TyVar, TExp> // TODO: Wieso hier IdentAnno und nicht nur Ident?
    | TELet of Ident * Annotated<TyVar, TExp> * Annotated<TyVar, TExp>

type Constraint =
    | Class of {| name: string; constraints: Constraint list |}
    | Func of Constraint * Constraint
    
type Env = Map<Ident, TyVar>

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some tyvar)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound."
        | Some t -> t

module Infer =
    type Newvar() =
        let mutable varCounter = -1
        member this.fresh() =
            varCounter <- varCounter + 1
            varCounter

    let annoExp (newvar: Newvar) (exp: Exp) =
        let rec annoExp (exp: Exp) =
            let thisVar = newvar.fresh()
            let texp =
                match exp with
                | ELit x ->
                    TELit x
                | EVar ident ->
                    TEVar ident
                | EApp (e1, e2) ->
                    TEApp (annoExp e1, annoExp e2)
                | EFun (ident, body) ->
                    let annotatedIdent = { annotated = ident; tvar = newvar.fresh() }
                    TEFun (annotatedIdent, annoExp body)
                | ELet (ident, e, body) ->
                    TELet (ident, annoExp e, annoExp body)
            { annotated = texp
              tvar = thisVar }
        annoExp exp

module Graph =
    open Visu

    let showAnnotated (env: Env) (annoExp: Annotated<TyVar, TExp>) =
        let showTvar (tvar: TyVar) = $": {tvar}"
        let rec createNodes (env: Env)  (annoExp: Annotated<TyVar, TExp>) =
            match annoExp.annotated with
            | TELit x ->
                Node.var $"Lit ({x.value}:{x.typeName})" (showTvar annoExp.tvar) []
            | TEVar ident ->
                let tyvar = Env.resolve ident env                
                Node.var $"Var ({ident}{showTvar tyvar})" (showTvar annoExp.tvar) []
            | TEApp (e1, e2) ->
                let child1 = createNodes env e1
                let child2 = createNodes env e2
                
                Node.var $"App" (showTvar annoExp.tvar) [ child1; child2 ]
            | TEFun (ident, body) ->
                let newEnv = env |> Env.bind ident.annotated ident.tvar
                let child = createNodes newEnv body

                Node.var
                    $"fun ({ident.annotated}{showTvar ident.tvar}) -> [e{showTvar body.tvar}]"
                    (showTvar annoExp.tvar)
                    [child]
            | TELet (ident, e, body) ->
                let child1 = createNodes env e

                let newEnv = env |> Env.bind ident e.tvar
                let child2 = createNodes newEnv body

                Node.var
                    $"let {ident} = [e1{showTvar e.tvar}] in [e2{showTvar body.tvar}]"
                    (showTvar annoExp.tvar)
                    [ child1; child2 ]

        let node = createNodes env annoExp
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
    Infer.annoExp (Infer.Newvar()) exp
    |> Graph.showAnnotated env

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
