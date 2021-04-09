
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
type Annotated<'expr> =
    { annotated: 'expr
      tyvar: TyVar
      env: Env }
type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of Annotated<TExp> * Annotated<TExp>
    | TEFun of Annotated<Ident> * Annotated<TExp>
    | TELet of Ident * Annotated<TExp> * Annotated<TExp>

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some tyvar)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound."
        | Some t -> t

type Incr(f) =
    let mutable varCounter = 0
    member this.next() =
        varCounter <- f varCounter 1
        varCounter

let annotate (env: Env) (exp: Exp) =
    let newvar = Incr((+)).next

    let rec annotate (env: Env) (exp: Exp) =
        { tyvar = newvar()
          annotated =
              match exp with
              | ELit x ->
                  TELit x
              | EVar ident ->
                  TEVar ident
              | EApp (e1, e2) ->
                  TEApp (annotate env e1, annotate env e2)
              | EFun (ident, body) ->
                  let tyvarIdent = newvar()
                  let newEnv = env |> Env.bind ident tyvarIdent
                  let annotatedIdent = { annotated = ident; tyvar = tyvarIdent; env = env }
                  TEFun (annotatedIdent, annotate newEnv body)
              | ELet (ident, e, body) ->
                  let newEnv = env |> Env.bind ident (newvar())
                  TELet (ident, annotate env e, annotate newEnv body)
          env = env }
    annotate env exp

type Constraint =
    | CAny
    | CBaseType of string
    | CVar of TyVar
    | CFun of Constraint * Constraint
    
type Var =
    { tyvar: TyVar
      mutable cachedConstr: Constraint option }
type Op =
    | ToFunction of int
type Node =
    | Source of int
    | Var of Var
    | Op of Op
type Edge =
    { fromNode: Node
      toNode: Node
      mutable cachedConstr: Constraint option }
type GraphItem =
    | Node of Node
    | Edge of Edge

let createConstraintGraph (exp: Annotated<TExp>) =
    let allNodes = System.Collections.Generic.Dictionary<TyVar, Node>()
    let addVarNode (tyvar: TyVar) =
        let varNode = { tyvar = tyvar; cachedConstr = None }
        allNodes.Add(tyvar, Var varNode)
        Var varNode
    let connect a b c = { fromNode = a; toNode = b; cachedConstr = c }
    let nextId = Incr((-)).next

    let rec constrainExp (exp: Annotated<TExp>) =
        [
            match exp.annotated with
            | TELit x ->
                let node = addVarNode exp.tyvar
                let edge = connect (Source (nextId())) node (Some (CBaseType x.typeName))

                yield Node node
                yield Edge edge
            | TEVar ident ->
                let node = (addVarNode exp.tyvar)
                
                // there has to be a node in the allNodes
                let edge =
                    let tyvarIdent = exp.env |> Env.resolve ident
                    connect allNodes.[tyvarIdent] node None
                
                yield Node node
                yield Edge edge
            | TEApp (e1, e2) ->
                // IMPORTANT: order of execution is significant here due to state manipulation
                yield! constrainExp e1
                yield! constrainExp e2

                // TODO: seems a bit weired due to mutable ResizeArray
                // but there now should exist entries for the e1.tyvar and e2.tyvar in allNodes
                let ne1 = allNodes.[e1.tyvar]
                let ne2 = allNodes.[e2.tyvar]

                let node = addVarNode exp.tyvar
                let nfunc = Op (ToFunction (nextId()))

                let ee2func = connect ne2 nfunc None
                let enodefunc = connect node nfunc None
                let efunce1 = connect nfunc ne1 None

                yield Node node
                yield Node nfunc
                yield Edge ee2func
                yield Edge enodefunc
                yield Edge efunce1
            | TEFun (ident, body) ->
                // IMPORTANT: order of execution is significant here due to state manipulation
                let nident = addVarNode ident.tyvar
                yield Node nident
                
                // IMPORTANT: order of execution is significant here due to state manipulation
                yield! constrainExp body

                // TODO: seems a bit weired due to mutable ResizeArray
                // but there now should exist entries for the e1.tyvar and e2.tyvar in allNodes
                let nbody = allNodes.[body.tyvar]

                let node = addVarNode exp.tyvar
                let nfunc = Op (ToFunction (nextId()))

                let eidentfunc = connect nident nfunc None
                let ebodyfunc = connect nbody nfunc None
                let efuncnode = connect nfunc node None

                yield Node node
                yield Node nfunc
                yield Edge eidentfunc
                yield Edge ebodyfunc
                yield Edge efuncnode
            | TELet (ident, e, body) ->
                let nident =
                    // important: we have to use the body's env to resolve ident
                    let tyvarIdent = body.env |> Env.resolve ident
                    addVarNode tyvarIdent
                yield Node nident

                yield! constrainExp e
                yield! constrainExp body

                // TODO: seems a bit weired due to mutable ResizeArray
                // but there now should exist entries for the e1.tyvar and e2.tyvar in allNodes
                let ne = allNodes.[e.tyvar]
                let nbody = allNodes.[body.tyvar]

                let node = addVarNode exp.tyvar

                let ebodynode = connect nbody node None
                let eeident = connect ne nident None

                yield Node node
                yield Edge ebodynode
                yield Edge eeident
        ]
    constrainExp exp

module GraphVisu =
    open Visu

    let showTyvar (ident: string) (tyvar: TyVar) =
        $"{{{ident} : {tyvar}}}"

    let showTyvarAndEnv exp =
        let envVars =
            match exp.env |> Map.toList with
            | [] -> "[ ]"
            | [(ident, tyvar)] -> $"[ {showTyvar ident tyvar} ]"
            | _ ->
                [ for x in exp.env do $"-  {showTyvar x.Key x.Value}" ]
                |> String.concat "\n"
                |> fun s -> $"[\n{s} ]"
        ($"var = {exp.tyvar}") + "\nenv = " + envVars

    let showAst (exp: Annotated<TExp>) =
        let flattenedNodes =
            let rec flatten (node: TreeNode) =
                [
                    yield node
                    for c in node.children do
                        yield! flatten c
                ]
            
            let rec createNodes (exp: Annotated<TExp>) =
                match exp.annotated with
                | TELit x ->
                    TreeNode.var $"Lit ({x.value}: {x.typeName})" (showTyvarAndEnv exp) []
                | TEVar ident ->
                    let tyvar = Env.resolve ident exp.env                
                    TreeNode.var $"Var {showTyvar ident tyvar}" (showTyvarAndEnv exp) []
                | TEApp (e1, e2) ->
                    let child1 = createNodes e1
                    let child2 = createNodes e2
                    
                    TreeNode.var $"App" (showTyvarAndEnv exp) [ child1; child2 ]
                | TEFun (ident, body) ->
                    let child = createNodes body

                    TreeNode.var
                        $"""fun {showTyvar ident.annotated ident.tyvar} -> {showTyvar "e" body.tyvar}"""
                        (showTyvarAndEnv exp)
                        [child]
                | TELet (ident, e, body) ->
                    let child1 = createNodes e
                    let child2 = createNodes body

                    TreeNode.var
                        $"""let {ident} = {showTyvar "e1" e.tyvar} in {showTyvar "e2" body.tyvar}"""
                        (showTyvarAndEnv exp)
                        [ child1; child2 ]

            createNodes exp |> flatten
        createTree flattenedNodes

    let showConstraintGraph (items: GraphItem list) =
        let allNodes =
            items |> List.choose (fun x -> match x with | Node n -> Some n | _ -> None)
        let getIndex n =
            match n with
            | Source i -> i
            | Var var -> var.tyvar
            | Op (ToFunction i) -> i
        let jsLinks =
            items
            |> List.choose (fun x -> match x with | Edge e -> Some e | _ -> None)
            |> List.map (fun edge ->
                { from = getIndex edge.fromNode
                  ``to`` = getIndex edge.toNode })
        let jsNodes =
            allNodes |> List.map (fun n ->
                match n with
                | Source i ->
                    { key = i
                      name = "SOURCE"
                      desc = ""
                      fig = NodeTypes.op }
                | Var var ->
                    { key = var.tyvar
                      name = string var.tyvar
                      desc = string var.cachedConstr
                      fig = NodeTypes.var }
                | Op (ToFunction index) as a ->
                    { key = index
                      name = string a
                      desc = ""
                      fig = NodeTypes.op }
            )
        createGraph jsNodes jsLinks Layouts.graph







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

let showAst exp = 
    annotate env exp |> GraphVisu.showAst
let showConstraintGraph exp =
    annotate env exp 
    |> createConstraintGraph 
    |> GraphVisu.showConstraintGraph

let idExp = EFun("x", EVar "x")



// polymorphic let
ELet("f", idExp,
    ELet("res1", EApp(EVar "f", cint 99),
        ELet("res2", EApp(EVar "f", cstr "HelloWorld"),
            EVar("res2")
)))
//|> annotate env |> createConstraintGraph
//|> showAst
|> showConstraintGraph


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
