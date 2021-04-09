
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
    | CUnknown
    | CPoly of TyVar list
    | CClass of string * TyVar list
    | CBaseType of string
    | CFun of Constraint * Constraint
    
type Var =
    { tyvar: TyVar
      mutable constr: Constraint option }
type Op =
    | MakeFunc
    | ApplyFunc
type Node =
    | Source of Constraint
    | Var of Var
    | Op of Op
type IndexedNode = { i: int; n: Node }
type Edge =
    { fromNode: IndexedNode
      toNode: IndexedNode }
type GraphItem =
    | Node of IndexedNode
    | Edge of Edge

type [<ReferenceEquality>] ConnectedNode =
    { i: int
      n: Node
      incoming: ResizeArray<ConnectedEdge>
      outgoing: ResizeArray<ConnectedEdge> }
and [<ReferenceEquality>] ConnectedEdge =
    { mutable constr: Constraint option
      fromNode: ConnectedNode
      toNode: ConnectedNode }

module Graph =
    let getAllNodes (items: GraphItem list) =
        items |> List.choose (fun x -> match x with | Node x -> Some x | _ -> None)
    let getAllEdges (items: GraphItem list) =
        items |> List.choose (fun x -> match x with | Edge x -> Some x | _ -> None)
    let toConnectedGraph (nodes: IndexedNode list) (edges: Edge list) =
        let connectedNodesLookup =
            nodes  
            |> List.map (fun x -> 
                let connectedItem =
                    { i = x.i
                      n = x.n
                      incoming = ResizeArray()
                      outgoing = ResizeArray() }
                (x.i, connectedItem))
            |> dict
        let connectedEdges = [
            for edge in edges do
                let connectedFrom = connectedNodesLookup.[edge.fromNode.i]
                let connectedTo = connectedNodesLookup.[edge.toNode.i]
                let connectedEdge = { fromNode = connectedFrom; toNode = connectedTo; constr = None }
                do
                    connectedFrom.outgoing.Add connectedEdge
                    connectedTo.incoming.Add connectedEdge
                yield connectedEdge ]
        let connectedNodes = connectedNodesLookup.Values |> Seq.toList
        (connectedNodes, connectedEdges)

let createConstraintGraph (exp: Annotated<TExp>) =
    let nextId = Incr((-)).next
    let withId x = { i = nextId(); n = x }
    let makeVarNode (tyvar: TyVar) =
        let node = { tyvar = tyvar; constr = None }
        let nodei = withId (Var node)
        nodei
    let makeSourceNode (tyname: string) =
        Source (CBaseType tyname) |> withId
    let connect a b = { fromNode = a; toNode = b }
    let makeFunction n1 n2 ntarget =
        [
            let nfunc = Op MakeFunc |> withId

            let e1func = connect n1 nfunc
            let e2func = connect n2 nfunc
            let efunctarget = connect nfunc ntarget

            yield Node nfunc
            yield Edge e1func
            yield Edge e2func
            yield Edge efunctarget
        ]
    let applyFunction nsource ntarget =
        [
            let nfunc = Op ApplyFunc |> withId

            let esourcefunc = connect nsource nfunc
            let efunctarget = connect nfunc ntarget

            yield Node nfunc
            yield Edge esourcefunc
            yield Edge efunctarget
        ]
    let findNode (tyvar: TyVar) (allNodes: IndexedNode list) =
        allNodes |> List.find (fun x ->
            match x.n with 
            | Var var when var.tyvar = tyvar -> true
            | _ -> false)
    let rec generateGraph (exp: Annotated<TExp>) (allNodes: IndexedNode list) =
        match exp.annotated with
        | TELit x ->
            let node = makeVarNode exp.tyvar
            let nsource = makeSourceNode x.typeName
            let edge = connect nsource node
            node, [
                yield Node node
                yield Node nsource
                yield Edge edge ]
        | TEVar ident ->
            let node = makeVarNode exp.tyvar
            let edge =
                let tyvarIdent = exp.env |> Env.resolve ident
                connect (findNode tyvarIdent allNodes) node
            node, [ 
                yield Node node
                yield Edge edge ]
        | TEApp (e1, e2) ->
            let ne1, e1Nodes = generateGraph e1 allNodes
            let ne2, e2Nodes = generateGraph e2 allNodes
            let node = makeVarNode exp.tyvar
            node, [
                yield! e1Nodes
                yield! e2Nodes
                yield Node node
                yield! makeFunction ne2 node ne1
                yield! applyFunction ne1 node ]
        | TEFun (ident, body) ->
            let nident = makeVarNode ident.tyvar
            let nbody,bodyNodes = generateGraph body (nident :: allNodes)
            let node = makeVarNode exp.tyvar
            node, [
                yield Node nident
                yield Node node
                yield! bodyNodes
                yield! makeFunction nident nbody node ]
        | TELet (ident, e, body) ->
            let nident = body.env |> Env.resolve ident |> makeVarNode
            let allNodes = nident :: allNodes
            let ne, enodes = generateGraph e allNodes
            let nbody, bodyNodes =  generateGraph body allNodes
            let node = makeVarNode exp.tyvar
            node, [
                yield Node node
                yield Edge (connect nbody node)
                yield Edge (connect ne nident)
                yield Node nident
                yield! enodes
                yield! bodyNodes ]

    generateGraph exp [] |> snd

let solve (graph: GraphItem list) =
    let allEdges = Graph.getAllEdges graph
    let allNodes = Graph.getAllNodes graph
    let connectedNodes,connectedEdges =
        Graph.toConnectedGraph allNodes allEdges
    let allVarNodes =
        connectedNodes |> List.choose (fun x -> 
            match x.n with 
            | Var var -> Some {| x with var = var |}
            | _ -> None)
    let allPolyNodes =
        connectedNodes |> List.filter (fun n -> n.incoming.Count = 0)
    let applyConstraint constr (edges: ConnectedEdge seq) =
        for e in edges do
            e.constr <- Some constr
    
    // constrain all poly nodes
    for x in allPolyNodes do
        let outgoingEdges = x.outgoing
        match x.n with
        | Source c -> applyConstraint c outgoingEdges
        | Var var ->
            let constr = CPoly [ var.tyvar ]
            var.constr <- Some constr
            applyConstraint constr outgoingEdges
        | Op _ -> failwith "Operator node without incoming edges detected."

    let visitedNodes = ResizeArray<int>()
    let startNode = connectedNodes |> List.sortBy (fun x -> x.n) |> List.head
    startNode

    // already visited and cannot do anything: ERROR


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

        createNodes exp |> flatten |> writeTree

    let showConstraintGraph (items: GraphItem list) =
        let jsLinks =
            items
            |> List.choose (fun x -> match x with | Edge e -> Some e | _ -> None)
            |> List.map (fun edge ->
                { from = edge.fromNode.i
                  ``to`` = edge.toNode.i })
        let jsNodes =
            Graph.getAllNodes items |> List.map (fun x ->
                match x.n with
                | Source constr ->
                    { key = x.i
                      name = "SOURCE"
                      desc = string constr
                      fig = NodeTypes.op }
                | Var var ->
                    { key = x.i
                      name = string var.tyvar
                      desc = string var.constr
                      fig = NodeTypes.var }
                | Op op ->
                    { key = x.i
                      name = string op
                      desc = ""
                      fig = NodeTypes.op }
            )

        writeGraph jsNodes jsLinks Layouts.graph







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
let showSolved exp =
    annotate env exp 
    |> createConstraintGraph
    |> solve

let idExp = EFun("x", EVar "x")


// polymorphic let
(*
let id x = x
let f = id
let res1 = f 99
let res2 = f "Hello World"
res2
*)
ELet("f", idExp,
    ELet("res1", EApp(EVar "f", cint 99),
        ELet("res2", EApp(EVar "f", cstr "HelloWorld"),
            EVar("res2")
)))
//|> annotate env |> createConstraintGraph
//|> showAst
|> showSolved

idExp |> showSolved


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
