
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
type Node =
    | Source of Constraint
    | Var of TyVar
    | Op of Op
type IndexedNode =
    { i: int
      n: Node }
type Edge =
    { fromNode: IndexedNode
      toNode: IndexedNode }
type GraphItem =
    | Node of IndexedNode
    | Edge of Edge

type [<ReferenceEquality>] ConnectedNode =
    { i: int
      n: Node
      mutable constr: Constraint option
      incoming: ResizeArray<ConnectedEdge>
      outgoing: ResizeArray<ConnectedEdge> }
and [<ReferenceEquality>] ConnectedEdge =
    { mutable constr: Constraint option
      fromNode: ConnectedNode
      toNode: ConnectedNode }

module Node =
    let makeNode id x = { i = id; n = x; }
    let makeVarNode id (tyvar: TyVar) = makeNode id (Var tyvar)
    let makeSourceNode id (tyname: string) = makeNode id (Source (CBaseType tyname))
    let makeOpNode id op = makeNode id (Op op)
    let connect a b = { fromNode = a; toNode = b }
    let makeFuncNode id n1 n2 ntarget =
        [
            let nfunc = makeOpNode id MakeFunc

            let e1func = connect n1 nfunc
            let e2func = connect n2 nfunc
            let efunctarget = connect nfunc ntarget

            yield Node nfunc
            yield Edge e1func
            yield Edge e2func
            yield Edge efunctarget
        ]
    let makeApplyFuncNode id nsource ntarget =
        [
            let nfunc = makeOpNode id ApplyFunc

            let esourcefunc = connect nsource nfunc
            let efunctarget = connect nfunc ntarget

            yield Node nfunc
            yield Edge esourcefunc
            yield Edge efunctarget
        ]
    let findNode (tyvar: TyVar) (allNodes: IndexedNode list) =
        allNodes |> List.find (fun x ->
            match x.n with 
            | Var x when x = tyvar -> true
            | _ -> false)

module Graph =
    let getAllNodes (graph: GraphItem list) =
        graph |> List.choose (fun x -> match x with | Node x -> Some x | _ -> None)
    let getAllEdges (graph: GraphItem list) =
        graph |> List.choose (fun x -> match x with | Edge x -> Some x | _ -> None)
    let connect (graph: GraphItem list) =
        let edges = getAllEdges graph
        let nodes = getAllNodes graph
        let connectedNodesLookup =
            nodes  
            |> List.map (fun x -> 
                let connectedItem =
                    { i = x.i
                      n = x.n
                      constr = None
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
    let nextId = Count.down()

    let rec generateGraph (exp: Annotated<TExp>) (allNodes: IndexedNode list) =
        match exp.annotated with
        | TELit x ->
            let node = Node.makeVarNode (nextId()) exp.tyvar
            let nsource = Node.makeSourceNode (nextId()) x.typeName
            let edge = Node.connect nsource node
            node, [
                yield Node node
                yield Node nsource
                yield Edge edge ]
        | TEVar ident ->
            let node = Node.makeVarNode (nextId()) exp.tyvar
            let edge =
                let tyvarIdent = exp.env |> Env.resolve ident
                Node.connect (Node.findNode tyvarIdent allNodes) node
            node, [ 
                yield Node node
                yield Edge edge ]
        | TEApp (e1, e2) ->
            let ne1, e1Nodes = generateGraph e1 allNodes
            let ne2, e2Nodes = generateGraph e2 allNodes
            let node = Node.makeVarNode (nextId()) exp.tyvar
            node, [
                yield! e1Nodes
                yield! e2Nodes
                yield Node node
                yield! Node.makeFuncNode (nextId()) ne2 node ne1
                yield! Node.makeApplyFuncNode (nextId()) ne1 node ]
        | TEFun (ident, body) ->
            let nident = Node.makeVarNode (nextId()) ident.tyvar
            let nbody,bodyNodes = generateGraph body (nident :: allNodes)
            let node = Node.makeVarNode (nextId()) exp.tyvar
            node, [
                yield Node nident
                yield Node node
                yield! bodyNodes
                yield! Node.makeFuncNode (nextId()) nident nbody node ]
        | TELet (ident, e, body) ->
            let nident = body.env |> Env.resolve ident |> Node.makeVarNode (nextId())
            let allNodes = nident :: allNodes
            let ne, enodes = generateGraph e allNodes
            let nbody, bodyNodes =  generateGraph body allNodes
            let node = Node.makeVarNode (nextId()) exp.tyvar
            node, [
                yield Node node
                yield Edge (Node.connect nbody node)
                yield Edge (Node.connect ne nident)
                yield Node nident
                yield! enodes
                yield! bodyNodes ]

    generateGraph exp [] |> snd

let solve (graph: GraphItem list) =
    let connectedNodes = Graph.connect graph |> fst
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
    
    //// constrain all poly nodes
    //for x in allPolyNodes do
    //    let outgoingEdges = x.outgoing
    //    match x.n with
    //    | Source c -> applyConstraint c outgoingEdges
    //    | Var tyvar ->
    //        let constr = CPoly [ tyvar ]
    //        x.constr <- Some constr
    //        applyConstraint constr outgoingEdges
    //    | Op _ -> failwith "Operator node without incoming edges detected."

    let rootNode = connectedNodes |> List.sortBy (fun x -> x.n) |> List.head
    rootNode

    //let removeIrrelevantNodes (nodes: ConnectedNode list) =
    //    let irrelevantNodes = nodes |> List.filter (fun n -> n.outgoing.Count = 0)

    // - Nodes with no incoming edges are forall constrained
    // - Nodes that have no outgoing edges (except for the root node) can be ignored
    //   - OPT: They can be removed (recursively) from the graph
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
                { fromNode = edge.fromNode.i
                  toNode = edge.toNode.i })
        let jsNodes =
            Graph.getAllNodes items |> List.map (fun x ->
                match x.n with
                | Source constr ->
                    { key = x.i
                      name = "SOURCE"
                      desc = string constr
                      layout = NodeTypes.op }
                | Var tyvar ->
                    { key = x.i
                      name = string tyvar
                      desc = "()"
                      layout = NodeTypes.var }
                | Op op ->
                    { key = x.i
                      name = string op
                      desc = "()"
                      layout = NodeTypes.op } )

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
|> showConstraintGraph
//|> showSolved

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
