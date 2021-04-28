
#load "./visu/visu.fsx"

type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit
type Exp =
    | Lit of Lit
    | Var of string
    | App of Exp * Exp
    | Abs of string * Exp
    | Let of string * Exp * Exp

type GenTyVar = int
type Tau =
    | TGenVar of GenTyVar
    | TApp of string * Tau list
    | TFun of Tau * Tau
type Forall = GenTyVar list * Tau
type Constraint =
    | CTau of Tau
    | CSigma of Forall

type TyVar = int
type Ident = string
type EnvItem =
    | Extern of Constraint
    | Intern of TyVar
type Env = Map<Ident, EnvItem>
type Annotated<'expr> =
    { annotated: 'expr
      tyvar: TyVar
      env: Env }
type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of Annotated<TExp> * Annotated<TExp>
    | TEAbs of Annotated<Ident> * Annotated<TExp>
    | TELet of Ident * Annotated<TExp> * Annotated<TExp>

module KnownTypeNames =
    let string = "String"
    let number = "Number"
    let bool = "Bool"
    let unit = "Unit"
    let seq = "Seq"

module Lit =
    let getTypeName (l: Lit) =
        match l with
        | LString _ -> KnownTypeNames.string
        | LNumber _ -> KnownTypeNames.number
        | LBool _ -> KnownTypeNames.bool
        | LUnit _ -> KnownTypeNames.unit

    let getValue (l: Lit) =
        match l with
        | LString x -> x :> obj
        | LNumber x -> x :> obj
        | LBool x -> x :> obj
        | LUnit -> "()" :> obj

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some(Intern tyvar))
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound. Env: {env}"
        | Some t -> t

module Counter =
    let up() =
        let mutable varCounter = 0
        fun () ->
            varCounter <- varCounter + 1
            varCounter

module AnnotatedAst =
    let create (env: Env) (exp: Exp) =
        let newvar = Counter.up()
        let allExp = ResizeArray<Annotated<TExp>>()
        let rec annotate (env: Env) (exp: Exp) =
            let res =
                { tyvar = newvar()
                  annotated =
                      match exp with
                      | Lit x ->
                          TELit x
                      | Var ident ->
                          TEVar ident
                      | App (e1, e2) ->
                          TEApp (annotate env e1, annotate env e2)
                      | Abs (ident, body) ->
                          let tyvarIdent = newvar()
                          let newEnv = env |> Env.bind ident tyvarIdent
                          let annotatedIdent = { annotated = ident; tyvar = tyvarIdent; env = env }
                          TEAbs (annotatedIdent, annotate newEnv body)
                      | Let (ident, e, body) ->
                          let newEnv = env |> Env.bind ident (newvar())
                          TELet (ident, annotate env e, annotate newEnv body)
                  env = env }
            do allExp.Add(res)
            res
        let res = annotate env exp
        res, allExp |> Seq.toList

module Format =
    let tyvar (ident: string) (x: string) =
        $"'{ident}' : {x}"

    let texpName (annoExp: TExp) =
        match annoExp with
        | TELit _ -> "Lit"
        | TEVar _ -> "Var"
        | TEApp _ -> "App"
        | TEAbs _ -> "Abs"
        | TELet _ -> "Let"

    let rec tau (t: Tau) =
        match t with
        | TGenVar genVar ->
            $"'{char (genVar + 96)}"
        | TApp (name, args) ->
            match args with
            | [] -> name
            | _ ->
                let args = args |> List.map tau |> String.concat ", "
                $"{name}<{args}>"
        | TFun (t1, t2) ->
            $"({tau t1} -> {tau t2})"

    let forall (fa: Forall) = tau (snd fa)

    let constr (c: Constraint) =
        match c with
        | CTau t -> tau t
        | CSigma fa -> forall fa
        
    let envItem ident envItem =
        match envItem with
        | Intern tv ->
            $"{tyvar ident (string tv)}"
        | Extern c ->
            $"{tyvar ident (constr c)}"

    let tyvarAndEnv exp =
        let envVars =
            match exp.env |> Map.toList with
            | [] -> "[ ]"
            | [(ident, item)] ->
                $"[ {envItem ident item} ]"
            | _ ->
                [ for x in exp.env do $"-  {envItem x.Key x.Value}" ]
                |> String.concat "\n"
                |> fun s -> $"\n{s}"
        ($"var = {exp.tyvar}") + "\nenv = " + envVars

module ConstraintGraph =
    let newGenVar = Counter.up()

    type ConstraintState =
        | UnificationError of string
        | Constrained of Constraint

    type Subst =
        { genTyVar: GenTyVar
          constr: Constraint }

    type ArgOp =
        | In
        | Out
    type Op =
        | MakeFun
        | Arg of ArgOp
    type NodeData =
        | Source of Constraint
        | Var of TyVar
        | Op of Op

    type Node (data: NodeData, constr: ConstraintState option) =
        member this.data = data
        member val constr = constr with get, set
        member val incoming: Edge list = [] with get, set
        member val outgoing: Edge list = [] with get, set
    and Edge (fromNode: Node, toNode: Node) =
        member this.fromNode = fromNode
        member this.toNode = toNode
    and Graph(root: Node, nodes: ResizeArray<Node>) =
        member this.root = root
        member this.nodes = nodes
        member val substs: Subst list = [] with get, set

    module Constraint =
        let norm c : Forall =
            match c with
            | CSigma(vars,tau) -> vars,tau
            | CTau tau -> [],tau
        let denorm (vars,tau) =
            match vars with
            | [] -> CTau tau
            | _ -> CSigma(Forall (vars,tau))

    module Graph =
        let connectNodes (fromNode: Node) (toNode: Node) =
            let edge = Edge(fromNode, toNode)
            fromNode.outgoing <- edge :: fromNode.outgoing
            toNode.incoming <- edge :: toNode.incoming
        let addNode n (nodes: ResizeArray<Node>) =
            let node = Node(n, match n with | Source c -> Some(Constrained c) | _ -> None)
            nodes.Add node
            node
        let addVarNode n nodes =
            addNode (Var n) nodes
        let addFuncNode n1 n2 ntarget nodes =
            let nfunc = nodes |> addNode (Op MakeFun)
            connectNodes n1 nfunc
            connectNodes n2 nfunc
            connectNodes nfunc ntarget
        let addArgNode op nsource ntarget nodes =
            let napp = nodes |> addNode (Op(Arg(op)))
            connectNodes nsource napp
            connectNodes napp ntarget
        let findNode (tyvar: TyVar) (nodes: Node seq) =
            nodes |> Seq.find (fun n ->
                match n.data with
                | Var v when v = tyvar -> true
                | _ -> false)

    let create (exp: Annotated<TExp>) =
        let nodes = ResizeArray()
        let rec generateGraph (exp: Annotated<TExp>) =
            match exp.annotated with
            | TELit x ->
                let nlit = nodes |> Graph.addVarNode exp.tyvar
                let nsource = nodes |> Graph.addNode (Source(CTau(TApp(Lit.getTypeName x, []))))
                Graph.connectNodes nsource nlit
                nlit
            | TEVar ident ->
                let nvar = nodes |> Graph.addVarNode exp.tyvar
                let identEnvItem = Env.resolve ident exp.env
                match identEnvItem with
                | Intern tyvarIdent ->
                    Graph.connectNodes (nodes |> Graph.findNode tyvarIdent) nvar
                | Extern c -> 
                    let nsource = nodes |> Graph.addNode (Source c)
                    Graph.connectNodes nsource nvar
                nvar
            | TEApp (e1, e2) ->
                // TODO: where to check? SOmetimes implicit (unification), but not always?
                // (check: ne1 must be a fun type) implicit
                // check: t<app> = t<e2>
                // infer: t<app> <- argOut(t<e1>)
                // infer: t<e2> <- argIn(t<e1>)
                let ne1 = generateGraph e1
                let ne2 = generateGraph e2
                let napp = nodes |> Graph.addVarNode exp.tyvar
                //Graph.connectNodes ne2 napp
                nodes |> Graph.addArgNode Out ne1 napp
                nodes |> Graph.addArgNode In ne1 ne2
                napp
            | TEAbs (ident, body) ->
                let nident = nodes |> Graph.addVarNode ident.tyvar
                let nabs = nodes |> Graph.addVarNode exp.tyvar
                nodes |> Graph.addFuncNode nident (generateGraph body) nabs
                nabs
            | TELet (ident, e, body) ->
                let nident =
                    let identEnvItem = body.env |> Env.resolve ident
                    match identEnvItem with
                    | Intern tyvarIdent ->
                        nodes |> Graph.addVarNode tyvarIdent
                    | Extern c -> 
                        nodes |> Graph.addNode (Source c)
                let nlet = nodes |> Graph.addVarNode exp.tyvar
                Graph.connectNodes (generateGraph body) nlet
                Graph.connectNodes (generateGraph e) nident
                nlet
        let rootNode = generateGraph exp
        Graph(rootNode, nodes)

    let solve (graph: Graph) =

        let emptySubst : Subst list  = []

        let rec unify (a: Forall) (b: Forall) : Result<Forall * Subst list, string> =
            let (_,t1), (_,t2) = a,b
            match t1,t2 with
            | x,y when x = y ->
                Ok (a, emptySubst)
            | TGenVar x, _ ->
                Ok (b, [ { genTyVar = x; constr = Constraint.denorm b } ])
            | _, TGenVar x ->
                Ok (a, [ { genTyVar = x; constr = Constraint.denorm a } ])
            | TApp (n1, taus1), TApp (n2, taus2)
                when n1 = n2  && taus1.Length = taus2.Length ->
                Error "TODO"
            | TFun (ta1, ta2), _ ->
                Error "TODO"
            | _ -> 
                Error  $"""Cannot unity types "{Format.forall a}" and "{Format.forall b}"."""

        let merge incomingConstraints (node: Node) =
            let incomingError =
                incomingConstraints 
                |> List.choose (function | UnificationError _ as cs -> Some cs | _ -> None)
                |> List.tryHead

            match incomingError with
            | Some error ->
                error, emptySubst
            | None ->
                let incomingConstraints =
                    incomingConstraints
                    |> List.choose (function | Constrained c -> (Some (Constraint.norm c)) | _ -> None)

                match node.data, incomingConstraints with
                | Op MakeFun, [ (varsa, taua); (varsb, taub) ] ->
                    let vars = varsa @ varsb |> List.distinct
                    Constrained(Constraint.denorm (vars, TFun (taua, taub))), emptySubst
                | Op(Arg In), [ (vars, TFun (ta, _)) ] ->
                    Constrained(Constraint.denorm(vars, ta)), emptySubst
                | Op(Arg Out), [ (vars, TFun (_, tb)) ] ->
                    Constrained(Constraint.denorm(vars, tb)), emptySubst
                | Source c, [] ->
                    Constrained c, emptySubst
                | Var _, [] ->
                    let freshGenVar = newGenVar()
                    Constrained(CSigma(Forall(([freshGenVar], TGenVar freshGenVar)))), emptySubst
                | Var _, [forall] ->
                    Constrained(CSigma(forall)), emptySubst
                | Var tyvar, forall :: foralls ->
                    let res =
                        // TODO: this looks weired - why 2 times (we have to merge substs)?
                        (Ok (forall, emptySubst), foralls)
                        ||> List.fold (fun state curr -> 
                            match state with
                            | Error _ as e -> e
                            | Ok (forall, substs) ->
                                match unify forall curr with
                                | Error msg -> Error (msg, substs)
                                | Ok (unifiedFoarll, newSubsts) -> Ok(unifiedFoarll, substs @ newSubsts))
                    match res with
                    | Error (msg, substs) ->
                        UnificationError msg, substs
                    | Ok (forall, substs) ->
                        Constrained(Constraint.denorm forall), substs
                | _ ->
                    failwith $"Invalid graph: incomingConstraints={incomingConstraints} ;;; node={node.data}"

        let (|CanMerge|_|) (incoming: Edge list) =
            let constraints = incoming |> List.choose (fun e -> e.fromNode.constr)
            if constraints.Length = incoming.Length then Some constraints else None

        let rec processNodes (unfinishedNodes : Node list) (finishedNodes : Node list) (substitutions : Subst list) =
            let res = [
                for node in unfinishedNodes do
                    match node.incoming with
                    | CanMerge constraints ->
                        let merged,substs = merge constraints node
                        do node.constr <- Some merged
                        yield Choice1Of3 node
                        yield! substs |> List.map Choice3Of3
                    | _ ->
                        yield Choice2Of3 node
                ]

            // TODO: find a better partitioning
            let newUnfinishedNodes = res |> List.choose (function Choice2Of3 x -> Some x | _ -> None)
            let newFinishedNodes = res |> List.choose (function Choice1Of3 x -> Some x | _ -> None)
            let newSubstitutions = res |> List.choose (function Choice3Of3 x -> Some x | _ -> None)
            if unfinishedNodes <> newUnfinishedNodes
                    || finishedNodes <> newFinishedNodes
                    || substitutions <> newSubstitutions
                then substitutions @ (processNodes newUnfinishedNodes newFinishedNodes newSubstitutions)
                else substitutions @ newSubstitutions
        let substs = processNodes (graph.nodes |> Seq.toList) [] []
        graph.substs <- substs


module Visu =
    open Visu

    let showUntypedAst (exp: Exp) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
    
        let rec createNodes (exp: Exp) =
            match exp with
            | Lit x ->
                Tree.var $"Lit" $"{Lit.getValue x} : {(Lit.getTypeName x)}" []
            | Var ident ->
                Tree.var $"Var" ident []
            | App (e1, e2) ->
                Tree.var $"App" "" [ createNodes e1; createNodes e2 ]
            | Abs (ident, body) ->
                Tree.var $"Abs {ident} -> e" "" [ createNodes body ]
            | Let (ident, e, body) ->
                Tree.var $"Let" ident [ createNodes e; createNodes body ]

        createNodes exp |> flatten |> Tree.write
    
    let showAnnotatedAst (exp: Annotated<TExp>) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
    
        let rec createNodes (exp: Annotated<TExp>) =
            match exp.annotated with
            | TELit x ->
                Tree.var $"Lit ({Lit.getValue x}: {Lit.getTypeName x})" (Format.tyvarAndEnv exp) []
            | TEVar ident ->
                let envItem = Env.resolve ident exp.env                
                Tree.var $"Var {Format.envItem ident envItem}" (Format.tyvarAndEnv exp) []
            | TEApp (e1, e2) ->
                Tree.var $"App" (Format.tyvarAndEnv exp) [ createNodes e1; createNodes e2 ]
            | TEAbs (ident, body) ->
                Tree.var
                    $"""fun {Format.tyvar ident.annotated (string ident.tyvar)} -> {Format.tyvar "e" (string body.tyvar)}"""
                    (Format.tyvarAndEnv exp)
                    [ createNodes body ]
            | TELet (ident, e, body) ->
                Tree.var
                    $"""let {ident} = {Format.tyvar "e1" (string e.tyvar)} in {Format.tyvar "e2" (string body.tyvar)}"""
                    (Format.tyvarAndEnv exp)
                    [ createNodes e; createNodes body ]

        createNodes exp |> flatten |> Tree.write

    open ConstraintGraph

    let showConstraintGraph (allAnnoExp: Annotated<TExp> list) (graph: Graph) =
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
                { Visu.JsLink.fromNode = nodesLookup.[edge.fromNode]
                  Visu.JsLink.toNode = nodesLookup.[edge.toNode] })
        let jsNodes =
            [ for i,x in indexedNodes do
                let name, layout =
                    match x.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Var tyvar ->
                        let expName =
                            match allAnnoExp |> List.tryFind (fun a -> a.tyvar = tyvar) with
                            | None -> "Env"
                            | Some x -> Format.texpName x.annotated
                        $"{tyvar} ({expName})", NodeTypes.var
                    | Op op -> string op, NodeTypes.op
                { key = i
                  name = name
                  desc =
                    match x.constr with
                    | Some (Constrained c) -> Format.constr c
                    | Some (UnificationError e) -> $"ERROR: {e}"
                    | None -> "???"
                  layout = layout }
            ]

        Graph.write jsNodes jsLinks




[<AutoOpen>]
module Dsl =
    let Str x = Lit (LString x)
    let Num x = Lit (LNumber x)
    let Bool x = Lit (LBool x)
    let Unit = Lit LUnit

    let Var x = Var x
    let App e1 e2 = App (e1, e2)
    let Abs x e = Abs (x, e)
    let Let x e1 e2 = Let (x, e1, e2)

    // convenience
    let Appn e (es: List<Exp>) =
        let rec apply (current: Exp) (es: List<Exp>) =
            match es with
            | [] -> current
            | [x] -> App current x
            | x :: xs ->
                let current = App current x
                apply current xs
        apply e es



//let env = AnnotatedAst.Env.empty
// TODO: convenience for importing .Net methods
module EnvCfg =
    module Predefined =
        let numberTyp = TApp(KnownTypeNames.number, [])
        let unitTyp = TApp(KnownTypeNames.unit, [])

        let add =
            "add",
            let typ =
                let typ = TFun(numberTyp, TFun(numberTyp, numberTyp))
                Extern(CTau(typ))
            in typ
        let read =
            "read",
            let typ =
                let typ = TFun(unitTyp, numberTyp)
                Extern(CTau(typ))
            in typ
        let map =
            "map",
            let typ =
                let v1,v2 = ConstraintGraph.newGenVar(), ConstraintGraph.newGenVar()
                let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
                let projTyp = TFun(TGenVar v1, TGenVar v2)
                let retTyp = TApp(KnownTypeNames.seq, [TGenVar v2])
                let typ = Forall([v1; v2], TFun(seqTyp, TFun(projTyp, retTyp)))
                Extern(CSigma(typ))
            in typ
        let take =
            "take",
            let typ =
                let v1 = ConstraintGraph.newGenVar()
                let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
                let retTyp = TGenVar v1
                let typ = Forall([v1], TFun(seqTyp, TFun(numberTyp, retTyp)))
                Extern(CSigma(typ))
            in typ
        let skip =
            "skip",
            let typ =
                let v1 = ConstraintGraph.newGenVar()
                let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
                let retTyp = TGenVar v1
                let typ = Forall([v1], TFun(seqTyp, TFun(numberTyp, retTyp)))
                Extern(CSigma(typ))
            in typ
        let numbers =
            "Numbers",
            let typ =
                let typ = TApp(KnownTypeNames.seq, [ TApp(KnownTypeNames.number, []) ])
                Extern(CTau(typ))
            in typ
        //let demoContext =
        //    Compiler.contextArgName,
        //    let typ =
        //        let r = TRecord [
        //            "BlockEdges", TApp(KnownTypeNames.seq, [ TApp("BlockEdge", []) ])
        //        ]
        //        Extern(CTau(r))
        //    in typ

    open Predefined

    let smallEnv = Map.ofList [ add; read ]
    let fullEnv = Map.ofList [ add; read; map; take; skip; numbers (*demoContext*) ]

let showUntypedAst exp =
    do Visu.showUntypedAst exp
    exp
let showAnnotatedAst env exp =
    let annoExp = AnnotatedAst.create env exp |> fst
    do annoExp |> Visu.showAnnotatedAst
    exp
let showConstraintGraph env exp =
    let annoExp,allAnnoExp = AnnotatedAst.create env exp
    do annoExp |> ConstraintGraph.create |> Visu.showConstraintGraph allAnnoExp
    exp
let showSolvedGraph env exp =
    let annoExp,allAnnoExp = AnnotatedAst.create env exp
    let graph = annoExp |> ConstraintGraph.create
    do ConstraintGraph.solve graph
    do graph |> Visu.showConstraintGraph allAnnoExp
    graph.substs


(*
let x = 10.0
map Numbers (\number ->
    add number x)
*)

(Let "x" (Num 10.0)
(Appn (Var "map") [ Var "Numbers"; Abs "number"
(Appn (Var "add") [ Var "number"; Var "x" ] )] ))
|> showUntypedAst
|> showAnnotatedAst EnvCfg.fullEnv
|> showConstraintGraph EnvCfg.fullEnv
|> showSolvedGraph EnvCfg.fullEnv



//let idExp = Abs "x" (Var "x")
//// polymorphic let
//(*
//let id = fun x -> x in
//    let f = id in
//        let res1 = f 99 in
//            let res2 = f "Hello World" in
//                res2
//*)
//Let "f" idExp
//(Let("res1", App(Var "f", Num 99.0),
//    Let("res2", App(Var "f", Str "HelloWorld"),
//        Var("res2")
//)))
////|> annotate env |> createConstraintGraph
////|> showAst
//|> showConstraintGraph EnvCfg.fullEnv

