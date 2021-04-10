
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

module Count =
    let create f =
        let mutable varCounter = 0
        fun () ->
            varCounter <- f varCounter 1
            varCounter
    let up () = create (+)
    let down () = create (-)

type System.Collections.Generic.List<'a> with
    member this.RemoveSafe(element: 'a) =
        if this.Remove element = false then failwith "element is not in list."

let annotate (env: Env) (exp: Exp) =
    let newvar = Count.up()

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
    
type Op =
    | MakeFunc
    | ApplyFunc
type [<ReferenceEquality>] Node =
    | Source of Constraint
    | Var of TyVar
    | Op of Op
type Edge =
    { fromNode: Node
      toNode: Node }
type GraphItem =
    | Node of Node
    | Edge of Edge

module Node =
    let connect a b = { fromNode = a; toNode = b }
    let makeFuncNode n1 n2 ntarget =
        [
            let nfunc = Op MakeFunc

            let e1func = connect n1 nfunc
            let e2func = connect n2 nfunc
            let efunctarget = connect nfunc ntarget

            yield Node nfunc
            yield Edge e1func
            yield Edge e2func
            yield Edge efunctarget
        ]
    let makeApplyFuncNode nsource ntarget =
        [
            let nfunc = Op ApplyFunc

            let esourcefunc = connect nsource nfunc
            let efunctarget = connect nfunc ntarget

            yield Node nfunc
            yield Edge esourcefunc
            yield Edge efunctarget
        ]
    let findNode (tyvar: TyVar) (allNodes: Node list) =
        allNodes |> List.find (fun x ->
            match x with 
            | Var x when x = tyvar -> true
            | _ -> false)
    let getInitialConstraint n =
        match n with | Source c -> Some c | _ -> None

module Graph =
    let getNodes (graph: GraphItem list) =
        graph |> List.choose (fun x -> match x with | Node x -> Some x | _ -> None)
    let getEdges (graph: GraphItem list) =
        graph |> List.choose (fun x -> match x with | Edge x -> Some x | _ -> None)

type [<ReferenceEquality>] ConnectedNode =
    { n: Node
      mutable constr: Constraint option
      incoming: ResizeArray<ConnectedEdge>
      outgoing: ResizeArray<ConnectedEdge> }
and [<ReferenceEquality>] ConnectedEdge =
    { mutable constr: Constraint option
      fromNode: ConnectedNode
      toNode: ConnectedNode }
and ConnectedGraph(graph: GraphItem list) =
    let nodesList = 
        let edges = Graph.getEdges graph
        let nodes = Graph.getNodes graph
        let connectedNodesLookup =
            nodes  
            |> List.map (fun n ->
                let connectedItem =
                    { n = n
                      constr = Node.getInitialConstraint n
                      incoming = ResizeArray()
                      outgoing = ResizeArray() }
                (n, connectedItem))
            |> readOnlyDict
        
        for edge in edges do
            let fromNode = connectedNodesLookup.[edge.fromNode]
            let toNode = connectedNodesLookup.[edge.toNode]
            let edge = { fromNode = fromNode; toNode = toNode; constr = None }
            do
                fromNode.outgoing.Add edge
                toNode.incoming.Add edge
            //yield connectedEdge ]
        ResizeArray connectedNodesLookup.Values
    member this.nodes = nodesList
    member this.getVarNodes() =
        nodesList |> Seq.choose (fun x -> 
            match x.n with 
            | Var var -> Some {| x with var = var |}
            | _ -> None)
    member this.getPolyNodes() =
        nodesList |> Seq.filter (fun n -> n.incoming.Count = 0)
    member this.getRootNode() =
        this.getVarNodes() |> Seq.sortBy (fun x -> x.var) |> Seq.head
    member this.remove (node: ConnectedNode) =
        if node.outgoing.Count > 0 then failwith "can only remove sinks"
        nodesList.RemoveSafe node
        [ for incomingEdge in node.incoming do
            let incomingNode = incomingEdge.fromNode
            incomingNode.outgoing.RemoveSafe incomingEdge
            yield incomingNode ]

let createConstraintGraph (exp: Annotated<TExp>) =
    let rec generateGraph (exp: Annotated<TExp>) (allNodes: Node list) =
        match exp.annotated with
        | TELit x ->
            let node = Var exp.tyvar
            let nsource = Source (CBaseType x.typeName)
            let edge = Node.connect nsource node
            node, [
                yield Node node
                yield Node nsource
                yield Edge edge ]
        | TEVar ident ->
            let node = Var exp.tyvar
            let edge =
                let tyvarIdent = exp.env |> Env.resolve ident
                Node.connect (Node.findNode tyvarIdent allNodes) node
            node, [ 
                yield Node node
                yield Edge edge ]
        | TEApp (e1, e2) ->
            let ne1, e1Nodes = generateGraph e1 allNodes
            let ne2, e2Nodes = generateGraph e2 allNodes
            let node = Var exp.tyvar
            node, [
                yield! e1Nodes
                yield! e2Nodes
                yield Node node
                yield! Node.makeFuncNode ne2 node ne1
                yield! Node.makeApplyFuncNode ne1 node ]
        | TEFun (ident, body) ->
            let nident = Var ident.tyvar
            let nbody,bodyNodes = generateGraph body (nident :: allNodes)
            let node = Var exp.tyvar
            node, [
                yield Node nident
                yield Node node
                yield! bodyNodes
                yield! Node.makeFuncNode nident nbody node ]
        | TELet (ident, e, body) ->
            let nident = body.env |> Env.resolve ident |> Var
            let allNodes = nident :: allNodes
            let ne, enodes = generateGraph e allNodes
            let nbody, bodyNodes =  generateGraph body allNodes
            let node = Var exp.tyvar
            node, [
                yield Node node
                yield Edge (Node.connect nbody node)
                yield Edge (Node.connect ne nident)
                yield Node nident
                yield! enodes
                yield! bodyNodes ]
    generateGraph exp [] |> snd

let solve (unconnectedNodes: GraphItem list) =
    let graph = ConnectedGraph unconnectedNodes
    let setEdgeConstraint constr (edges: ConnectedEdge seq) =
        for e in edges do e.constr <- Some constr
    
    // Nodes with no incoming edges are forall constrained
    for x in graph.getPolyNodes() do
        let outgoingEdges = x.outgoing
        match x.n with
        | Source c -> setEdgeConstraint c outgoingEdges
        | Var tyvar ->
            let constr = CPoly [ tyvar ]
            x.constr <- Some constr
            setEdgeConstraint constr outgoingEdges
        | Op _ -> failwith "Operator node without incoming edges detected."

    // Nodes that have no outgoing edges (except for the root node) can be ignored:
    // Remove them (recursively) from the graph.
    let removeSinks (nodes: ConnectedNode seq) =
        let sinks = nodes |> Seq.filter (fun n -> n.outgoing.Count = 0) |> Seq.toList
        for s in sinks do
            printfn $"Removing: {s}"
            graph.remove s |> ignore
    removeSinks graph.nodes

    // already visited and cannot do anything: ERROR

    let rootNode = graph.getRootNode()
    
    graph


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

    let showConstraint (c: Constraint option) =
        match c with
        | None -> "()"
        | Some c -> string c

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

    let showConstraintGraph (graph: ConnectedGraph) =
        let edges =
            [ for n in graph.nodes do
              yield! n.incoming
              yield! n.outgoing ]
            |> List.distinct
        let indexedNodes = graph.nodes |> Seq.indexed |> Seq.toList
        let nodesLookup = indexedNodes |> List.map (fun (a,b) -> b,a) |> readOnlyDict
        let jsLinks =
            edges
            |> List.map (fun edge ->
                { fromNode = nodesLookup.[edge.fromNode]
                  toNode = nodesLookup.[edge.toNode] })
        let jsNodes =
            [ for i,x in indexedNodes do
                let name, layout =
                    match x.n with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Var tyvar -> string tyvar, NodeTypes.var
                    | Op op -> string op, NodeTypes.op
                { key = i
                  name = name
                  desc = showConstraint x.constr
                  layout = layout }
            ]

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
    annotate env exp
    |> GraphVisu.showAst
let showConstraintGraph exp =
    annotate env exp 
    |> createConstraintGraph
    |> ConnectedGraph
    |> GraphVisu.showConstraintGraph
let showSolved exp =
    annotate env exp 
    |> createConstraintGraph
    |> solve
    |> GraphVisu.showConstraintGraph

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
//|> showConstraintGraph
|> showSolved

//idExp |> showSolved


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
