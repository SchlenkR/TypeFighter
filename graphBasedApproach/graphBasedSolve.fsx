
#load "./visu/visu.fsx"

type Lit =
    | LString of string
    | LNum of float
    | LBool of bool
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

module Lit =
    let getDotnetTypeName (l: Lit) =
        match l with
        | LString _ -> "string"
        | LNum _ -> "num"
        | LBool _ -> "bool"
    let getValue (l: Lit) =
        match l with
        | LString x -> x :> obj
        | LNum x -> x :> obj
        | LBool x -> x :> obj

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some tyvar)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound."
        | Some t -> t

module Counter =
    let create f =
        let mutable varCounter = 0
        fun () ->
            varCounter <- f varCounter 1
            varCounter
    let up () = create (+)
    let down () = create (-)

let annotate (env: Env) (exp: Exp) =
    let newvar = Counter.up()

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

type GenTyVar = int
type Tau =
    | TGenVar of GenTyVar
    | TApp of string * Tau list
    | TFun of Tau * Tau
type Forall = GenTyVar list * Tau
type Sigma =
    | Forall of Forall
type Constraint =
    | CTau of Tau
    | CSigma of Sigma
type ConstraintState =
    | UnificationError of string
    | Constrained of Constraint

type Op =
    | MakeFun
    | ApplyFun
type NodeData =
    | Source of Constraint
    | Var of TyVar
    | Op of Op
type [<ReferenceEquality>] Node =
    { data: NodeData
      mutable constr: ConstraintState option
      mutable incoming: Edge list
      mutable outgoing: Edge list }
and [<CustomEquality; CustomComparison>] Edge =
    { fromNode: Node
      toNode: Node }
        interface System.IComparable with
            member this.CompareTo(other) =
                this.GetHashCode().CompareTo((other :?> Edge).GetHashCode())
        override this.Equals(other) =
            let other = other :?> Edge
            this.fromNode = other.fromNode && this.toNode = other.toNode
        override this.GetHashCode() = hash (this.fromNode, this.toNode)

and Graph =
    { root: Node 
      nodes: ResizeArray<Node> }

module Constraint =
    let norm c : Forall =
        match c with
        | CSigma(Forall(vars,tau)) -> vars,tau
        | CTau tau -> [],tau

    let denorm (vars,tau) =
        match vars with
        | [] -> CTau tau
        | _ -> CSigma(Forall (vars,tau))

    let rec makeFun (a: Constraint) (b: Constraint) =
        match norm a, norm b with
        | (args1, tau1), (args2, tau2) ->
            let args = set (args1 @ args2) |> Set.toList
            denorm (args, TFun(tau1, tau2))

    let zip f (a: ConstraintState) (b: ConstraintState) =
        match a,b with
        | UnificationError a, UnificationError b ->
            UnificationError $"{a} AND {b}"
        | _, UnificationError x
        | UnificationError x, _ ->
            UnificationError x
        | Constrained a, Constrained b ->
            f a b

module Node =
    let getIncomingConstraints (n: Node) =
        n.incoming |> Seq.choose (fun e -> e.fromNode.constr) |> Seq.toList
    let getIncomingIncompleteEdges (n: Node) =
        n.incoming |> Seq.filter (fun e -> Option.isNone e.fromNode.constr) |> Seq.toList
    let connectNodes (fromNode: Node) (toNode: Node) =
        let edge = { fromNode = fromNode; toNode = toNode }
        do
            fromNode.outgoing <- edge :: fromNode.outgoing
            toNode.incoming <- edge :: toNode.incoming

module Graph =
    let addNode n (nodes: ResizeArray<Node>) =
        let node =
            { data = n
              constr = match n with | Source c -> Some(Constrained c) | _ -> None
              incoming = []
              outgoing = [] }
        do
            nodes.Add node
        node
    let addVarNode n nodes = addNode (Var n) nodes
    let addFuncNode n1 n2 ntarget nodes =
        let nfunc = nodes |> addNode (Op MakeFun)
        do
            Node.connectNodes n1 nfunc
            Node.connectNodes n2 nfunc
            Node.connectNodes nfunc ntarget
    let addApplyFuncNode nsource ntarget nodes =
        let napp = nodes |> addNode (Op ApplyFun)
        do
            Node.connectNodes nsource napp
            Node.connectNodes napp ntarget

    /// nodes with no incoming edges
    let getSources (nodes: Node seq) =
        nodes
        |> Seq.filter (fun n -> n.incoming.Length = 0)
        |> Seq.toList

    let findNode (tyvar: TyVar) (nodes: Node seq) =
        nodes |> Seq.find (fun n ->
            match n.data with
            | Var v when v = tyvar -> true
            | _ -> false)

let createConstraintGraph (exp: Annotated<TExp>) =
    let nodes = ResizeArray()
    let rec generateGraph (exp: Annotated<TExp>) =
        match exp.annotated with
        | TELit x ->
            let node = nodes |> Graph.addVarNode exp.tyvar
            let nsource = nodes |> Graph.addNode (Source(CTau(TApp(Lit.getDotnetTypeName x, []))))
            do
                Node.connectNodes nsource node
            node
        | TEVar ident ->
            let node = nodes |> Graph.addVarNode exp.tyvar
            let tyvarIdent = exp.env |> Env.resolve ident
            do
                Node.connectNodes (nodes |> Graph.findNode tyvarIdent) node
            node
        | TEApp (e1, e2) ->
            let ne1 = generateGraph e1
            let ne2 = generateGraph e2
            let napp = nodes |> Graph.addVarNode exp.tyvar
            do
                nodes |> Graph.addFuncNode ne2 napp ne1
                nodes |> Graph.addApplyFuncNode ne1 napp
            napp
        | TEFun (ident, body) ->
            let nident = nodes |> Graph.addVarNode ident.tyvar
            let nfun = nodes |> Graph.addVarNode exp.tyvar
            do
                nodes |> Graph.addFuncNode nident (generateGraph body) nfun
            nfun
        | TELet (ident, e, body) ->
            let nident = nodes |> Graph.addVarNode (Env.resolve ident body.env)
            let nlet = nodes |> Graph.addVarNode exp.tyvar
            do
                Node.connectNodes (generateGraph body) nlet
                Node.connectNodes (generateGraph e) nident
            nlet
    let rootNode = generateGraph exp
    { nodes = nodes; root = rootNode }

let solve (graph: Graph) =

    let newGenVar = Counter.up()

    let rec unify (a: Forall) (b: Forall) =
        let (a1,t1), (a2,t2) = a,b
        match t1, t2 with
        | TApp (n1, taus1), TApp (n2, taus2)
            when n1 = n2  && taus1.Length = taus2.Length ->
            Error "TODO"
        | TFun (ta1, ta2), _ -> 
            Error "TODO"
        | _ -> 
            Error  $"Cannot unity types '{a}' and '{b}'." 

    let mergeConstraints incomingConstraints node =
        // TODO: is there some list.toLookup (see below)?
        let incomingError =
            incomingConstraints 
            |> List.choose (fun cs ->
                match cs with | UnificationError _ -> Some cs | _ -> None)
            |> List.tryHead
        match incomingError with
        | Some error -> error
        | None ->
            let incomingConstraints =
                incomingConstraints |> List.choose (fun cs ->
                    match cs with | Constrained c -> (Some (Constraint.norm c)) | _ -> None)
            match node.data, incomingConstraints with
            | Source c, [] -> Constrained c
            | Var _, [] ->
                let freshGenVar = newGenVar()
                Constrained(CSigma(Forall(([freshGenVar], TGenVar freshGenVar))))
            | Op MakeFun, [ (varsa, taua); (varsb, taub) ] ->
                let vars = varsa @ varsb |> List.distinct
                Constrained(Constraint.denorm (vars, TFun (taua, taub)))
            | Op ApplyFun, [ (vars, TFun (_, tb)) ] ->
                Constrained(Constraint.denorm(vars, tb))
            | Var _, forall :: foralls ->
                let res =
                    (Ok forall, foralls) 
                    ||> List.fold (fun state curr -> 
                        match state with
                        | Error _ -> state
                        | Ok c -> unify c curr)
                match res with
                | Error e -> UnificationError e
                | Ok res -> Constrained(Constraint.denorm res)
            | _ ->
                UnificationError $"TODO: Implement unifier for: {incomingConstraints} and {node.data}"

    let rec constrainNode (n: Node) (comingFrom: Edge list) =
        printfn $"Processing node: {n.data}"

        let incomingIncompleteEdges = Node.getIncomingIncompleteEdges n

        if incomingIncompleteEdges.Length > 0 then
            for e in incomingIncompleteEdges |> List.except comingFrom do
                constrainNode e.fromNode [e]
        else
            // merge incoming constraints
            let resultingConstraint =
                let incomingConstraints = Node.getIncomingConstraints n
                mergeConstraints incomingConstraints n

            // TODO: on error, we could terminate earlier
            n.constr <- Some resultingConstraint

            // wohin als nächstes:
            // Edges, die noch gar nicht gegangen wurden
            let unprocessedEdges = n.outgoing |> List.except comingFrom
            for unprocessedEdge in unprocessedEdges do
                constrainNode unprocessedEdge.toNode (comingFrom @ [unprocessedEdge])

        ()

    let rec processNodes (nodes: Node list) =
        for n in nodes do
            constrainNode n []
        //processNodes (Seq.toList waitingForCompletion)

    // we start with the root nodes
    let sourceNodes = Graph.getSources graph.nodes
    processNodes sourceNodes

    ()


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

    let showConstraint (c: ConstraintState option) =
        match c with
        | None -> "()"
        | Some c -> string c

    let showAst (exp: Annotated<TExp>) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
    
        let rec createNodes (exp: Annotated<TExp>) =
            match exp.annotated with
            | TELit x ->
                Tree.var $"Lit ({Lit.getValue x}: {Lit.getDotnetTypeName x})" (showTyvarAndEnv exp) []
            | TEVar ident ->
                let tyvar = Env.resolve ident exp.env                
                Tree.var $"Var {showTyvar ident tyvar}" (showTyvarAndEnv exp) []
            | TEApp (e1, e2) ->
                let child1 = createNodes e1
                let child2 = createNodes e2
            
                Tree.var $"App" (showTyvarAndEnv exp) [ child1; child2 ]
            | TEFun (ident, body) ->
                let child = createNodes body

                Tree.var
                    $"""fun {showTyvar ident.annotated ident.tyvar} -> {showTyvar "e" body.tyvar}"""
                    (showTyvarAndEnv exp)
                    [child]
            | TELet (ident, e, body) ->
                let child1 = createNodes e
                let child2 = createNodes body

                Tree.var
                    $"""let {ident} = {showTyvar "e1" e.tyvar} in {showTyvar "e2" body.tyvar}"""
                    (showTyvarAndEnv exp)
                    [ child1; child2 ]

        createNodes exp |> flatten |> Tree.write

    let showConstraintGraph (graph: Graph) =
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
                    match x.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Var tyvar -> string tyvar, NodeTypes.var
                    | Op op -> string op, NodeTypes.op
                { key = i
                  name = name
                  desc = showConstraint x.constr
                  layout = layout }
            ]

        Graph.write jsNodes jsLinks






[<AutoOpen>]
module Dsl =
    let knownBaseTypes =
        {| int = "Int"
           float = "Float"
           string = "String" |}
           
    let cstr x = ELit (LString x)
    let cnum x = ELit (LNum x)
    let cbool x = ELit (LBool x)

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
    let graph = annotate env exp |> createConstraintGraph 
    do
        solve graph
        GraphVisu.showConstraintGraph graph

let idExp = EFun("x", EVar "x")


// polymorphic let
(*
let id = fun x -> x in
    let f = id in
        let res1 = f 99 in
            let res2 = f "Hello World" in
                res2
*)
ELet("f", idExp,
    ELet("res1", EApp(EVar "f", cnum 99.0),
        ELet("res2", EApp(EVar "f", cstr "HelloWorld"),
            EVar("res2")
)))
//|> annotate env |> createConstraintGraph
//|> showAst
//|> showConstraintGraph
|> showSolved

