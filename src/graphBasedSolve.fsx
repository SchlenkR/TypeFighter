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
type ConstraintState =
    | Initial
    | Constrained of Tau
    | UnificationError of string

type TyVar = int
type Ident = string
type EnvItem =
    | Extern of Tau
    | Intern of TyVar
type Env = Map<Ident, EnvItem>
type Annotated<'expr> =
    { annotated: 'expr
      tyvar: TyVar
      env: Env
      mutable constr: ConstraintState }
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
        fun () -> varCounter <- varCounter + 1; varCounter

module AnnotatedAst =
    let create (env: Env) (exp: Exp) =
        let newvar = Counter.up()
        let allExp = ResizeArray<Annotated<TExp>>()
        let rec annotate (env: Env) (exp: Exp) =
            let res =
                { tyvar = newvar()
                  constr = Initial
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
                        let annotatedIdent =
                            { annotated = ident
                              tyvar = tyvarIdent
                              env = env
                              constr = Initial }
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
        | TEAbs _ -> "Fun"
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

    let constraintState cs =
        match cs with
        | Constrained t -> tau t
        | UnificationError e -> $"ERROR: {e}"
        | Initial -> "???"

    let envItem ident envItem =
        match envItem with
        | Intern tv -> $"{tyvar ident (string tv)}"
        | Extern t -> $"{tyvar ident (tau t)}"

    let expTyvar exp = $"var = {exp.tyvar}"

    let env exp =
        let envVars =
            match exp.env |> Map.toList with
            | [] -> "[ ]"
            | [(ident, item)] ->
                $"[ {envItem ident item} ]"
            | _ ->
                [ for x in exp.env do $"-  {envItem x.Key x.Value}" ]
                |> String.concat "\n"
                |> fun s -> $"\n{s}"
        "env = " + envVars

module rec ConstraintGraph =
    type Subst = { genTyVar: GenTyVar; constr: Tau }

    type VarData = { tyvar: TyVar; inc1: Node option; inc2: Node option }
    type MakeFunData = { inc1: Node; inc2: Node }
    type ArgOp = In | Out
    type ArgData = { argOp: ArgOp; inc: Node }
    type UnifySubstData = { substSource: Node; substIn: Node; applyTo: Node }
    type NodeData =
        | Source
        | Var of VarData
        | MakeFun of MakeFunData
        | Arg of ArgData
        | UnifySubst of UnifySubstData
    
    // TODO: maybe get rid of this and make everything immutable
    type Node (data: NodeData, constr: ConstraintState) =
        member this.data = data
        member val constr = constr with get, set

    type SolveResult =
        { constrainedNodes: Node list
          errorNodes: Node list
          unfinishedNodes: Node list
          allNodes: Node list
          success: bool }
    
    let newGenVar = Counter.up()

    module Node =
        let getIncoming (node: Node) =
            match node.data with
            | Source _ -> []
            | Var x -> [ x.inc1; x.inc2 ] |> List.choose id
            | MakeFun x -> [ x.inc1; x.inc2 ]
            | Arg { argOp = _; inc = a } -> [a]
            | UnifySubst { substSource = a; substIn = b; applyTo = c } -> [a;b;c]
        
        let findVarNode (tyvar: TyVar) (nodes: Node seq) =
            nodes |> Seq.find (fun n ->
                match n.data with
                | Var v when v.tyvar = tyvar -> true
                | _ -> false)

    let create (exp: Annotated<TExp>) =
        let nodes = ResizeArray()

        let addNode n constr =
            let node = Node(n, constr)
            do nodes.Add node
            node
        let source tau = addNode Source (Constrained tau)
        let var tyvar constr inc1 inc2 = addNode (Var { tyvar = tyvar; inc1 = inc1; inc2 = inc2 }) constr
        let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) Initial
        let arg op inc = addNode (Arg { argOp = op; inc = inc }) Initial
        let argIn = arg In
        let argOut = arg Out
        let applySubst substSource substIn apply =
            addNode (UnifySubst { substSource = substSource; substIn = substIn; applyTo = apply }) Initial

        let rec generateGraph (exp: Annotated<TExp>) (inc: Node option) =
            let ( => ) x f = Some x |> f
            let ( ==> ) (x,y) f = (Some x,y) ||> f
            
            match exp.annotated with
            | TELit x ->
                let nsource = source (TApp(Lit.getTypeName x, []))
                (nsource, inc) ==> var exp.tyvar exp.constr
            | TEVar ident ->
                let nsource =
                    match Env.resolve ident exp.env with
                    | Intern tyvarIdent -> Node.findVarNode tyvarIdent nodes
                    | Extern c -> source c
                (nsource, inc) ==> var exp.tyvar exp.constr
            | TEApp (e1, e2) ->
                // TODO: where to check? SOmetimes implicit (unification), but not always?
                // (check: e1 must be a fun type) implicit
                // (check: app = e2) ??
                // NEIN! Das ist zu allgemein (wir müssen das eine Zeile unten machen)
                //     infer: argOut(e1) ==> app
                // JA!
                //     subst: getSubst (argIn e1) e2 |> applySubst (argOut e1) ==> app
                //     -- oder --
                //     subst: applySubst e2 (argIn e1) (argOut e1) ==> app
                // infer: argIn(e1) ==> e2
                let ne1 = None |> generateGraph e1
                let ne2 = (argIn ne1) => generateGraph e2 
                (applySubst ne2 (argIn ne1) (argOut ne1), inc) ==> var exp.tyvar exp.constr
                //(argOut ne1, inc) ==> var exp.tyvar
            | TEAbs (ident, body) ->
                let nfunc = makeFunc (var ident.tyvar Initial None None) (generateGraph body None)
                (nfunc, inc) ==> var exp.tyvar exp.constr
            | TELet (ident, e, body) ->
                let _ =
                    // TODO: why do we have this exception? Can we express that let bound idents are always intern?
                    match Env.resolve ident body.env with
                    | Intern tyvarIdent ->
                        (generateGraph e None, None) ==> var tyvarIdent Initial
                    | Extern _ ->
                        failwith "Invalid graph: let bound identifiers must be intern in env."
                (generateGraph body None, inc) ==> var exp.tyvar exp.constr
        do generateGraph exp None |> ignore
        nodes

    let solve (nodes: Node seq) =
        let emptySubst : Subst list  = []
        
        let rec unify (a: Tau) (b: Tau) =
            let error (msg: string) = Error $"""Cannot unify types "{Format.tau a}" and "{Format.tau b}": {msg}"""
            let rec unify1 t1 t2 =
                match t1,t2 with
                | x,y when x = y ->
                    Ok (x, emptySubst)
                | TGenVar x, y
                | y, TGenVar x ->
                    Ok (y, [ { genTyVar = x; constr = y } ])
                | TApp (_, taus1), TApp (_, taus2) when taus1.Length <> taus2.Length ->
                    error "Arg count mismatch"
                | TApp (n1,_), TApp (n2, _) when n1 <> n2 ->
                    error "Type (name) mismatch"
                | TApp (name, taus1), TApp (_, taus2) ->
                    let res = unifyn taus1 taus2
                    match res with
                    | Ok (taus, substs) ->
                        Ok (TApp (name, taus), substs)
                    | Error e ->
                        error e
                | TFun (ta1, ta2), TFun (tb1, tb2) ->
                    match unifyn [ta1; tb1] [ta2; tb2] with
                    | Ok ([tres1; tres2], substs) ->
                        Ok (TFun (tres1, tres2), substs)
                    | Error e ->
                        error e
                    | _ ->
                        failwith $"inconsistent TFun unification: {t1} <> {t2}"
                | TFun (ta1, ta2), TApp (name, taus)
                | TApp (name, taus), TFun (ta1, ta2) ->
                    error "TODO: FUN-APP"
                | _ ->
                    error "Unspecified cases"
            // TODO: muss das sein? "and" -> kann das nicht wieder nach innen?
            and unifyn taus1 taus2 =
                let unifiedTaus = [ for t1,t2 in List.zip taus1 taus2 do unify1 t1 t2 ]
                let rec allOk (taus: Tau list) (allSubsts: Subst list) remainingTaus =
                    match remainingTaus with
                    | [] -> Ok (taus, allSubsts)
                    | x :: xs ->
                        match x with
                        | Ok (tau, substs) -> allOk (taus @ [tau]) (allSubsts @ substs) xs
                        | Error e -> Error e
                allOk [] [] unifiedTaus
            // TODO: consistency check of unifiers? (!IMPORTANT)
            unify1 a b

        let rec substVarInAwithB (tvar: GenTyVar) (a: Tau) (b: Tau) =
            match a with
            | TGenVar i when i = tvar -> b
            | TFun (t1, t2) -> TFun (substVarInAwithB tvar t1 b, substVarInAwithB tvar t2 b)
            | TApp (name, taus) -> TApp(name, [ for tau in taus do substVarInAwithB tvar tau b ])
            | _ -> a

        let constrainNode (node: Node) =
            let (|C|E|I|) (node: Node) =
                match node.constr with
                | Constrained tau -> C tau
                | UnificationError e -> E e
                | Initial -> I

            let (|AnyError|AnyInitial|AllConstrained|) (nodes: Node list) =
                let cs = nodes |> List.choose (function | C x -> Some x | _ -> None)
                let es = nodes |> List.choose (function | E x -> Some x | _ -> None)
                let ns = nodes |> List.choose (function | I x -> Some x | _ -> None)
                match cs,es,ns with
                | _,es::_,_ -> AnyError es
                | _,_,ns::_ -> AnyInitial ns
                | cs,[],[] -> AllConstrained cs

            match Node.getIncoming node with
            | AnyInitial _ -> Initial
            | AnyError es -> UnificationError es
            | _ ->
                match node.data with
                | Source ->
                    node.constr
                // The next 2 are edge cases. we could also model inc1 and inc2 an not optional
                // and normalize them with an unconstrained source node. But this would
                // lead to a blown up graph with much more nodes and gen vars.
                | Var { tyvar = _; inc1 = None; inc2 = None } ->
                    Constrained(TGenVar(newGenVar()))
                | Var { tyvar = _; inc1 = Some(C tau); inc2 = None }
                | Var { tyvar = _; inc1 = None; inc2 = Some(C tau) } ->
                    Constrained(tau)
                | Var { tyvar = _; inc1 = Some(C t1); inc2 = Some(C t2) } ->
                    match unify t1 t2 with
                    | Error msg -> UnificationError msg
                    | Ok (unifiedTau,_) -> Constrained unifiedTau
                | MakeFun { inc1 = C t1; inc2 = C t2  } ->
                    Constrained(TFun (t1, t2))
                | Arg { argOp = In; inc = C(TFun(t1,_)) } ->
                    Constrained(t1)
                | Arg { argOp = Out; inc = C(TFun(_,t2)) } ->
                    Constrained(t2)
                | UnifySubst { substSource = C substSource; substIn = C substIn; applyTo = C applyTo } ->
                    // TODO: it matters if we use "b a " or "a b", but we have to know that :(
                    match unify substIn substSource with
                    | Error msg -> UnificationError msg
                    | Ok (_,substs) ->
                        let rec doit (substs: Subst list) =
                            match substs with
                            | x :: xs ->
                                // TODO: this is far from complete
                                let substituted = substVarInAwithB x.genTyVar applyTo x.constr
                                substituted
                            | [] ->
                                applyTo
                        Constrained(doit substs)
                | _ ->
                    failwith $"Invalid graph at node: {node.data}"

        let rec constrainNodes (unfinished: ResizeArray<Node>) (constrained: ResizeArray<Node>) (errors: ResizeArray<Node>) =
            let mutable goOn = false
            for node in unfinished |> Seq.toArray do
                let c = constrainNode node
                node.constr <- c
                match c with
                | Constrained _ ->
                    goOn <- true
                    unfinished.Remove(node) |> ignore
                    constrained.Add(node)
                | UnificationError _ ->
                    goOn <- true
                    unfinished.Remove(node) |> ignore
                    errors.Add(node)
                | _ -> ()
            if not goOn then
                { constrainedNodes = constrained |> List.ofSeq
                  errorNodes = errors |> List.ofSeq
                  unfinishedNodes = unfinished |> List.ofSeq
                  allNodes = [ yield! constrained; yield! unfinished; yield! errors ]
                  success = errors.Count = 0 && unfinished.Count = 0 }
            else
                constrainNodes unfinished constrained errors
        
        constrainNodes (ResizeArray nodes) (ResizeArray()) (ResizeArray())

    let applyResult exp (nodes: Node list) =
        let constrainExp (exp: Annotated<_>) =
            let findConstraint tyvar =
                let node = Node.findVarNode tyvar nodes
                node.constr
            exp.constr <- findConstraint exp.tyvar
        let rec applyResult (exp: Annotated<TExp>) =
            constrainExp exp
            match exp.annotated with
            | TELit x -> ()
            | TEVar ident -> ()
            | TEApp (e1, e2) ->
                applyResult e1
                applyResult e2
            | TEAbs (ident, body) ->
                constrainExp ident
                applyResult body
            | TELet (ident, e, body) ->
                applyResult e
                applyResult body
        applyResult exp


module Visu =
    open Visu
    
    let writeAnnotatedAst (showVar: bool) (showEnv: bool) (showConstraint: bool) (exp: Annotated<TExp>) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
        let rec createNodes (exp: Annotated<TExp>) =
            let details =
                [
                    if showVar then yield Format.expTyvar exp
                    if showConstraint then yield Format.constraintState exp.constr
                    if showEnv then yield Format.env exp
                ]
                |> String.concat "\n"
            match exp.annotated with
            | TELit x ->
                Tree.var $"Lit ({Lit.getValue x})" details []
            | TEVar ident ->
                Tree.var $"Var ({ident})" details []
            | TEApp (e1, e2) ->
                Tree.var "App" details [ createNodes e1; createNodes e2 ]
            | TEAbs (ident, body) ->
                Tree.var $"Fun ({ident.annotated})" details [ createNodes body ]
            | TELet (ident, e, body) ->
                Tree.var $"Let {ident}" details [ createNodes e; createNodes body ]
        createNodes exp |> flatten |> Tree.write

    let writeConstraintGraph (allAnnoExp: Annotated<TExp> list) (nodes: ConstraintGraph.Node seq) =
        let indexedNodes = nodes |> Seq.indexed |> Seq.toList
        let jsLinks =
            [ 
                let nodesLookup = indexedNodes |> List.map (fun (a,b) -> b,a) |> readOnlyDict
                for n in nodes do
                    for i in ConstraintGraph.Node.getIncoming n do
                        { Visu.JsLink.fromNode = nodesLookup.[i]
                          Visu.JsLink.toNode = nodesLookup.[n] }
            ]
        let jsNodes =
            [ for i,x in indexedNodes do
                let name, layout =
                    match x.data with
                    | ConstraintGraph.Source _ -> "SOURCE", NodeTypes.op
                    | ConstraintGraph.Var x ->
                        let expName =
                            match allAnnoExp |> List.tryFind (fun a -> a.tyvar = x.tyvar) with
                            | None -> "Env"
                            | Some x -> Format.texpName x.annotated
                        $"{x.tyvar} ({expName})", NodeTypes.var
                    | ConstraintGraph.MakeFun _ -> "MakeFun", NodeTypes.op
                    | ConstraintGraph.Arg { argOp = x; inc = _ } -> $"Arg {x}", NodeTypes.op
                    | ConstraintGraph.UnifySubst _ -> $"ApplySubst", NodeTypes.op
                { key = i
                  name = name
                  desc = Format.constraintState x.constr
                  layout = layout }
            ]
        Graph.write jsNodes jsLinks
    
    let private showAst env (showVar: bool) (showEnv: bool) (showConstraint: bool) exp =
        let annoExp = AnnotatedAst.create env exp |> fst
        do annoExp |> writeAnnotatedAst showVar showEnv showConstraint
        exp
    let showLightAst env exp = showAst env false false false exp
    let showAnnotatedAst env exp = showAst env true true false exp
    let showConstraintGraph env exp =
        let annoExp,allAnnoExp = AnnotatedAst.create env exp
        do annoExp |> ConstraintGraph.create |> writeConstraintGraph allAnnoExp
        exp
    let showSolvedGraph env exp =
        let annoExp,allAnnoExp = AnnotatedAst.create env exp
        let nodes = annoExp |> ConstraintGraph.create
        let res = ConstraintGraph.solve nodes
        do res.allNodes |> writeConstraintGraph allAnnoExp
        exp
    let showSolvedAst env exp =
        let annoExp,_ = AnnotatedAst.create env exp
        let nodes = annoExp |> ConstraintGraph.create
        let res = ConstraintGraph.solve nodes
        do ConstraintGraph.applyResult annoExp res.allNodes
        do annoExp |> writeAnnotatedAst false false true



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


(*
let x = 10.0
map Numbers (\number ->
    add number x)
*)

(Let "x" (Num 10.0)
(Appn (Var "map") [ Var "Numbers"; Abs "number"
(Appn (Var "add") [ Var "number"; Var "x" ] )] ))
|> Visu.showLightAst EnvCfg.fullEnv
|> Visu.showAnnotatedAst EnvCfg.fullEnv
|> Visu.showConstraintGraph EnvCfg.fullEnv
|> Visu.showSolvedGraph EnvCfg.fullEnv
|> Visu.showSolvedAst EnvCfg.fullEnv



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
