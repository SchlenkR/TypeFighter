﻿
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

type TyVar = int
type Ident = string
type EnvItem =
    | Extern of Tau
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

    // TODO: this is crap!
    let genVar (x: GenTyVar) = $"'{char (x + 96)}"

    let rec tau (t: Tau) =
        match t with
        | TGenVar x -> 
            genVar x
        | TApp (name, args) ->
            match args with
            | [] -> name
            | _ ->
                let args = args |> List.map tau |> String.concat ", "
                $"{name}<{args}>"
        | TFun (t1, t2) -> 
            $"({tau t1} -> {tau t2})"

    let envItem ident envItem =
        match envItem with
        | Intern tv -> $"{tyvar ident (string tv)}"
        | Extern t -> $"{tyvar ident (tau t)}"

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

module rec ConstraintGraph =
    let newGenVar = Counter.up()

    type ConstraintState =
        | Initial
        | Constrained of Tau
        | UnificationError of string

    type Subst = { genTyVar: GenTyVar; constr: Tau; anchor: TyVar }

    type VarData = { tyvar: TyVar; incs: Node list }
    type MakeFunData = { inc1: Node; inc2: Node }
    type ArgOp = In | Out
    type ArgData = { argOp: ArgOp; inc: Node }
    //type UnifyData = { incs: Node list }
    type NodeData =
        | Source of Tau
        | Var of VarData
        | MakeFun of MakeFunData
        | Arg of ArgData

    type Node (data: NodeData, constr: ConstraintState) =
        member this.data = data
        member val constr = constr with get, set
    and Graph(root: Node, nodes: ResizeArray<Node>) =
        member this.root = root
        member this.nodes = nodes
        member val substs: Subst list = [] with get, set

    module Node =
        let getIncoming (node: Node) =
            match node.data with
            | Source _ -> []
            | Var x -> x.incs
            | MakeFun x -> [ x.inc1; x.inc2 ]
            | Arg { argOp = _; inc = x } -> [x]


    let create (exp: Annotated<TExp>) =
        let nodes = ResizeArray()

        let addNode n =
            let node = Node(n, match n with | Source c -> Constrained c | _ -> Initial)
            do nodes.Add node
            node
        let addVarNode tyvar incs = addNode (Var { tyvar = tyvar; incs = incs })
        let addFuncNode inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 })
        let addArgNode op inc = addNode (Arg { argOp = op; inc = inc })
        let findVarNode (tyvar: TyVar) =
            nodes |> Seq.find (fun n ->
                match n.data with
                | Var v when v.tyvar = tyvar -> true
                | _ -> false)

        let rec generateGraph (exp: Annotated<TExp>) (inc: Node list) =
            match exp.annotated with
            | TELit x ->
                let nsource = addNode (Source(TApp(Lit.getTypeName x, [])))
                addVarNode exp.tyvar [nsource]
            | TEVar ident ->
                let nsource =
                    match Env.resolve ident exp.env with
                    | Intern tyvarIdent -> findVarNode tyvarIdent
                    | Extern c -> addNode (Source c)
                addVarNode exp.tyvar [nsource]
            | TEApp (e1, e2) ->
                // TODO: where to check? SOmetimes implicit (unification), but not always?
                // (check: ne1 must be a fun type) implicit
                // check: t<app> = t<e2>
                // infer: t<app> <- argOut(t<e1>)
                // infer: t<e2> <- argIn(t<e1>)
                let ne1 = generateGraph e1 []
                let ne2 = generateGraph e2 [ addArgNode In ne1 ]
                addVarNode exp.tyvar [ addArgNode Out ne1 ]
            | TEAbs (ident, body) ->
                addVarNode exp.tyvar [ addFuncNode (addVarNode ident.tyvar []) (generateGraph body []) ]
            | TELet (ident, e, body) ->
                let _ =
                    // TODO: why do we have this exception? Can we express that let bound idents are always intern?
                    match Env.resolve ident body.env with
                    | Intern tyvarIdent -> addVarNode tyvarIdent [ generateGraph e [] ]
                    | Extern _ -> failwith "Invalid graph: let bound identifiers must be intern in env."
                addVarNode exp.tyvar [ generateGraph body [] ]
        let rootNode = generateGraph exp []
        Graph(rootNode, nodes)

    let solve (graph: Graph) =
        let emptySubst : Subst list  = []

        //let rec collectGenVars (t: Tau) =
        //    match t with
        //    | TGenVar a -> [a]
        //    | TApp (_, a) -> a |> List.collect collectGenVars
        //    | TFun (a, b) -> (collectGenVars a @ collectGenVars b)
        //    |> List.distinct

        let rec unify (a: Tau) (b: Tau) (anchor: TyVar) : Result<Tau * Subst list, string> =
            let error (msg: string) = Error $"""Cannot unity types "{Format.tau a}" and "{Format.tau b}": {msg}"""
            let rec unifyTaus t1 t2 =
                match t1,t2 with
                | x,y when x = y ->
                    Ok (x, emptySubst)
                | TGenVar x, y
                | y, TGenVar x ->
                    Ok (y, [ { genTyVar = x; constr = y; anchor = anchor } ])
                | TApp (_, taus1), TApp (_, taus2)
                    when taus1.Length <> taus2.Length ->
                        error "Arg count mismatch"
                | TApp (n1,_), TApp (n2, _)
                    when n1 <> n2 ->
                        error "Type (name) mismatch"
                | TApp (name, taus1), TApp (_, taus2) ->
                    let unifiedTaus = [ for t1,t2 in List.zip taus1 taus2 do unifyTaus t1 t2 ]
                    let rec allOk (taus: Tau list) (allSubsts: Subst list) remainingTaus =
                        match remainingTaus with
                        | [] -> Ok (taus, allSubsts)
                        | x :: xs ->
                            match x with
                            | Ok (tau, substs) -> allOk (taus @ [tau]) (allSubsts @ substs) xs
                            | Error e -> Error e
                    let res = allOk [] [] unifiedTaus
                    match res with
                    | Ok (taus, substs) ->
                        Ok (TApp (name, taus), substs)
                    | Error e ->
                        error e
                | TFun (ta1, ta2), _ ->
                    error "TODO"
                | _ -> error "Unspecified cases"
            unifyTaus a b

        let (|C|E|I|) (node: Node) =
            match node.constr with
            | Constrained tau -> C tau
            | UnificationError e -> E e
            | Initial -> I

        let (|Cs|Es|Ns|) (nodes: Node list) =
            let cs = nodes |> List.choose (function | C x -> Some x | _ -> None)
            let es = nodes |> List.choose (function | E x -> Some x | _ -> None)
            let ns = nodes |> List.choose (function | I x -> Some x | _ -> None)
            match cs,es,ns with
            | _,es::_,_ -> Es es
            | _,_,ns::_ -> Ns ns
            | cs,[],[] -> Cs cs

        let merge (node: Node) =
            match Node.getIncoming node with
            | Ns _ -> Initial, emptySubst
            | Es es -> UnificationError es, emptySubst
            | _ ->
                match node.data with
                | MakeFun { inc1 = C t1; inc2 = C t2  } ->
                    Constrained(TFun (t1, t2)), emptySubst
                | Arg { argOp = In; inc = C(TFun(t1,_)) } ->
                    Constrained(t1), emptySubst
                | Arg { argOp = Out; inc = C(TFun(_,t2)) } ->
                    Constrained(t2), emptySubst
                | Source c ->
                    Constrained c, emptySubst
                | Var { tyvar = _; incs = [] } ->
                    let freshGenVar = newGenVar()
                    Constrained(TGenVar freshGenVar), emptySubst
                | Var { tyvar = _; incs = [C tau] } ->
                    Constrained(tau), emptySubst
                | Var { tyvar = tyvar; incs = (C x) :: (Cs xs) } ->
                    let res =
                        // TODO: this looks weired - why 2 times (we have to merge substs)?
                        (Ok (x, emptySubst), xs)
                        ||> List.fold (fun state curr -> 
                            match state with
                            | Error _ as e -> e
                            | Ok (forall, substs) ->
                                match unify forall curr tyvar with
                                | Error msg -> Error(msg, substs)
                                | Ok (unifiedFoarll, newSubsts) -> Ok(unifiedFoarll, substs @ newSubsts))
                    match res with
                    | Error (msg, substs) -> UnificationError(msg), substs
                    | Ok (tau, substs) -> Constrained(tau), substs
                | _ ->
                    failwith $"Invalid graph at node: {node.data}"

        let rec processNodes (unfinished : Node list) (finished: Node list) (substs : Subst list) =
            let res = [
                for node in unfinished do
                    let resc,substs = merge node
                    do node.constr <- resc
                    yield! substs |> List.map Choice3Of3
                    match resc with
                    | Constrained _ | UnificationError _ -> yield Choice2Of3 node
                    | _ -> yield Choice1Of3 node
                ]

            // TODO: find a better partitioning
            let newUnfinished = res |> List.choose (function Choice2Of3 x -> Some x | _ -> None)
            let newFinished = res |> List.choose (function Choice1Of3 x -> Some x | _ -> None)
            let newSubsts = res |> List.choose (function Choice3Of3 x -> Some x | _ -> None)
            if unfinished <> newUnfinished
                    || finished <> newFinished
                    || substs <> newSubsts
                then substs @ (processNodes newUnfinished newFinished newSubsts)
                else substs @ newSubsts
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
        let indexedNodes = graph.nodes |> Seq.indexed |> Seq.toList
        let jsLinks =
            [ 
                let nodesLookup = indexedNodes |> List.map (fun (a,b) -> b,a) |> readOnlyDict
                for n in graph.nodes do
                    for i in Node.getIncoming n do
                        { Visu.JsLink.fromNode = nodesLookup.[i]
                          Visu.JsLink.toNode = nodesLookup.[n] }
            ]
        let jsNodes =
            [ for i,x in indexedNodes do
                let name, layout =
                    match x.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Var x ->
                        let expName =
                            match allAnnoExp |> List.tryFind (fun a -> a.tyvar = x.tyvar) with
                            | None -> "Env"
                            | Some x -> Format.texpName x.annotated
                        $"{x.tyvar} ({expName})", NodeTypes.var
                    | MakeFun _ -> "MakeFun", NodeTypes.op
                    | Arg { argOp = x; inc = _ } -> $"Arg {x}", NodeTypes.op
                { key = i
                  name = name
                  desc =
                    match x.constr with
                    | Constrained tau -> Format.tau tau
                    | UnificationError e -> $"ERROR: {e}"
                    | Initial -> "???"
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
                Extern(typ)
            in typ
        let read =
            "read",
            let typ =
                let typ = TFun(unitTyp, numberTyp)
                Extern(typ)
            in typ
        let map =
            "map",
            let typ =
                let v1,v2 = ConstraintGraph.newGenVar(), ConstraintGraph.newGenVar()
                let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
                let projTyp = TFun(TGenVar v1, TGenVar v2)
                let retTyp = TApp(KnownTypeNames.seq, [TGenVar v2])
                let typ = TFun(seqTyp, TFun(projTyp, retTyp))
                Extern(typ)
            in typ
        let take =
            "take",
            let typ =
                let v1 = ConstraintGraph.newGenVar()
                let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
                let retTyp = TGenVar v1
                let typ = TFun(seqTyp, TFun(numberTyp, retTyp))
                Extern(typ)
            in typ
        let skip =
            "skip",
            let typ =
                let v1 = ConstraintGraph.newGenVar()
                let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
                let retTyp = TGenVar v1
                let typ = TFun(seqTyp, TFun(numberTyp, retTyp))
                Extern(typ)
            in typ
        let numbers =
            "Numbers",
            let typ =
                let typ = TApp(KnownTypeNames.seq, [ TApp(KnownTypeNames.number, []) ])
                Extern(typ)
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

