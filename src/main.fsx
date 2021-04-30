
type GenTyVar = int
type Tau =
    | TGenVar of GenTyVar
    | TApp of string * Tau list
    | TFun of Tau * Tau
    | TTuple of Tau list
    | TRecord of (string * Tau) list
type ConstraintState =
    | Initial
    | Constrained of Tau
    | UnificationError of string

// TODO: in type inference, respect the fact that annos can be initially constrained

type TyVar = int
type Ident = string
type EnvItem =
    | Extern of Tau
    | Intern of TyVar
type Env = Map<Ident, EnvItem>

type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit
type Exp<'meta> =
    | Lit of Lit
    | Var of Ident
    | App of MExp<'meta> * MExp<'meta>
    | Abs of Meta<Ident, 'meta> * MExp<'meta>
    | Let of Ident * MExp<'meta> * MExp<'meta>
    | Prop of Ident * MExp<'meta>
    | Tuple of MExp<'meta> list
    | Record of List<Ident * MExp<'meta>>
and Meta<'exp, 'meta> = { exp: 'exp; meta: 'meta }
and MExp<'meta> = Meta<Exp<'meta>, 'meta>

type Anno =
    { tyvar: TyVar
      env: Env
      mutable constr: ConstraintState }
type UExp = Meta<Exp<unit>, unit>
type TExp = Meta<Exp<Anno>, Anno>



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
    let create (env: Env) (exp: UExp) =
        let newvar = Counter.up()
        let allExp = ResizeArray<TExp>()
        let rec annotate (env: Env) (exp: UExp) =
            let res =
                { meta =
                    { tyvar = newvar() 
                      env = env
                      constr = Initial }
                  exp =
                    match exp.exp with
                    | Lit x ->
                        Lit x
                    | Var ident -> 
                        Var ident
                    | App (e1, e2) -> 
                        App (annotate env e1, annotate env e2)
                    | Abs (ident, body) ->
                        let tyvarIdent = newvar()
                        let newEnv = env |> Env.bind ident.exp tyvarIdent
                        let annotatedIdent =
                            { meta =
                                { tyvar = tyvarIdent
                                  env = env
                                  constr = Initial }
                              exp = ident.exp }
                        Abs (annotatedIdent, annotate newEnv body)
                    | Let (ident, e, body) ->
                        let newEnv = env |> Env.bind ident (newvar())
                        Let (ident, annotate env e, annotate newEnv body)
                    | Prop (ident, e) -> 
                        Prop (ident, annotate env e)
                    | Tuple es -> 
                        Tuple (es |> List.map (annotate env))
                    | Record fields -> 
                        Record [ for ident,e in fields do ident, annotate env e ]
                }
            do allExp.Add(res)
            res
        let res = annotate env exp
        res, allExp |> Seq.toList

module Format =
    // TODO: this is crap!
    let genVar (x: GenTyVar) = $"'{char (x + 96)}"

    let rec tau (t: Tau) =
        match t with
        | TGenVar x -> 
            genVar x
        | TApp (name, taus) ->
            match taus with
            | [] -> name
            | _ ->
                let taus = taus |> List.map tau |> String.concat ", "
                $"{name}<{taus}>"
        | TFun (t1, t2) -> 
            $"({tau t1} -> {tau t2})"
        | TTuple taus ->
            taus |> List.map tau |> String.concat " * " |> sprintf "(%s)"
        | TRecord fields ->
            [ for n,t in fields do $"{n}: {tau t}" ] |> String.concat "; " |> sprintf "{ %s }"



module rec ConstraintGraph =
    type Subst = { genTyVar: GenTyVar; constr: Tau }

    type Ast = { tyvar: TyVar; inc1: Node option; inc2: Node option }
    type MakeFun = { inc1: Node; inc2: Node }
    type ArgOp = In | Out
    type Arg = { argOp: ArgOp; inc: Node }
    type UnifySubst = { substSource: Node; substIn: Node; applyTo: Node }
    type GetProp = { field: string; inc: Node }
    type MakeTuple = { incs: Node list }
    type MakeRecord = { fields: string list; incs: Node list }

    type NodeData =
        | Source
        | Ast of Ast
        | MakeFun of MakeFun
        | GetProp of GetProp
        | MakeTuple of MakeTuple
        | MakeRecord of MakeRecord
        | Arg of Arg
        | UnifySubst of UnifySubst
    
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

    let getIncomingNodes (node: Node) =
        match node.data with
        | Source _ -> []
        | Ast x -> [ x.inc1; x.inc2 ] |> List.choose id
        | MakeFun x -> [ x.inc1; x.inc2 ]
        | GetProp x -> [ x.inc ]
        | MakeTuple x -> x.incs
        | MakeRecord x -> x.incs
        | Arg x -> [ x.inc ]
        | UnifySubst x -> [ x.substSource; x.substIn; x.applyTo ]
        
    let findVarNode (tyvar: TyVar) (nodes: Node seq) =
        nodes |> Seq.find (fun n ->
            match n.data with
            | Ast v when v.tyvar = tyvar -> true
            | _ -> false)

    let create (exp: TExp) =
        let nodes = ResizeArray()

        let addNode n constr =
            let node = Node(n, constr)
            do nodes.Add node
            node
        let source tau = addNode Source (Constrained tau)
        let ast tyvar constr inc1 inc2 = addNode (Ast { tyvar = tyvar; inc1 = inc1; inc2 = inc2 }) constr
        let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) Initial
        let getProp field inc = addNode (GetProp { field = field; inc = inc }) Initial
        let makeTuple incs = addNode (MakeTuple { incs = incs }) Initial
        let makeRecord fields incs = addNode (MakeRecord { fields = fields; incs = incs }) Initial
        let arg op inc = addNode (Arg { argOp = op; inc = inc }) Initial
        let argIn = arg In
        let argOut = arg Out
        let applySubst substSource substIn apply =
            addNode (UnifySubst { substSource = substSource; substIn = substIn; applyTo = apply }) Initial

        let rec generateGraph (exp: TExp) (inc: Node option) =
            let ( => ) x f = Some x |> f
            let ( ==> ) (x,y) f = (Some x,y) ||> f
            
            let nthis = ast exp.meta.tyvar exp.meta.constr
            match exp.exp with
            | Lit x ->
                let nsource = source (TApp(Lit.getTypeName x, []))
                (nsource, inc) ==> nthis
            | Var ident ->
                let nsource =
                    match Env.resolve ident exp.meta.env with
                    | Intern tyvarIdent -> findVarNode tyvarIdent nodes
                    | Extern c -> source c
                (nsource, inc) ==> nthis
            | App (e1, e2) ->
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
                (applySubst ne2 (argIn ne1) (argOut ne1), inc) ==> nthis
                //(argOut ne1, inc) ==> var exp.tyvar
            | Abs (ident, body) ->
                let nfunc = makeFunc (ast ident.meta.tyvar Initial None None) (generateGraph body None)
                (nfunc, inc) ==> nthis
            | Let (ident, e, body) ->
                do
                    // TODO: why do we have this exception? Can we express that let bound idents are always intern?
                    match Env.resolve ident body.meta.env with
                    | Intern tyvarIdent ->
                        (generateGraph e None, None) ==> ast tyvarIdent Initial
                    | Extern _ ->
                        failwith "Invalid graph: let bound identifiers must be intern in env."
                    |> ignore
                (generateGraph body None, inc) ==> nthis
            | Prop (ident, e) -> 
                let nprop = (ident, generateGraph e None) ||> getProp
                (nprop, inc) ==> nthis
            | Tuple es ->
                let ntuple = [ for e in es do generateGraph e None ] |> makeTuple
                (ntuple, inc) ==> nthis
            | Record fields ->
                let fieldnames = fields |> List.map fst
                let es = fields |> List.map snd |> List.map (fun e -> generateGraph e None)
                let nrecord = (fieldnames, es) ||> makeRecord
                (nrecord, inc) ==> nthis
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

            match getIncomingNodes node with
            | AnyInitial _ -> Initial
            | AnyError es -> UnificationError es
            | _ ->
                match node.data with
                | Source ->
                    node.constr
                // The next 2 are edge cases. we could also model inc1 and inc2 an not optional
                // and normalize them with an unconstrained source node. But this would
                // lead to a blown up graph with much more nodes and gen vars.
                | Ast { tyvar = _; inc1 = None; inc2 = None } ->
                    Constrained(TGenVar(newGenVar()))
                | Ast { tyvar = _; inc1 = Some(C tau); inc2 = None }
                | Ast { tyvar = _; inc1 = None; inc2 = Some(C tau) } ->
                    Constrained(tau)
                | Ast { tyvar = _; inc1 = Some(C t1); inc2 = Some(C t2) } ->
                    match unify t1 t2 with
                    | Error msg -> UnificationError msg
                    | Ok (unifiedTau,_) -> Constrained unifiedTau
                | MakeFun { inc1 = C t1; inc2 = C t2 } ->
                    Constrained(TFun (t1, t2))
                | GetProp { field = field; inc = C(TRecord recordFields) } ->
                    recordFields 
                    |> List.tryFind (fun (n,_) -> n = field)
                    |> Option.map snd
                    |> Option.map Constrained
                    |> Option.defaultValue (UnificationError $"Field not found: {field}")
                | MakeTuple { incs = AllConstrained incs } ->
                    Constrained(TTuple incs)
                | MakeRecord { fields = fields; incs = AllConstrained incs } ->
                    let fields = List.zip fields incs
                    Constrained(TRecord fields)
                | Arg { argOp = In; inc = C(TFun(t1,_)) } ->
                    Constrained t1
                | Arg { argOp = Out; inc = C(TFun(_,t2)) } ->
                    Constrained t2
                | UnifySubst { substSource = C substSource; substIn = C substIn; applyTo = C applyTo } ->
                    // TODO: it matters if we use "b a " or "a b", but we have to know that :(
                    match unify substIn substSource with
                    | Error msg -> UnificationError msg
                    | Ok (_,substs) ->
                        match substs with
                        | [] -> 
                            Constrained applyTo
                        | substs -> 
                            let substsAndC =
                                let tempCVar = substs |> List.map (fun s -> s.genTyVar) |> List.max |> (+) 1
                                (substs |> List.map (fun s -> s.genTyVar, s.constr))
                                @ [ tempCVar, applyTo ]
                            // ersetze das 1. in den anderen. da kommt eine Liste zurück. mit der wird weitergemacht
                            let rec substitute substs =
                                match substs with
                                | [] -> failwith "can't we do that better? We know that we always have a 'last' element..."
                                | [applyTo] -> applyTo
                                | (genvar, subst) :: xs ->
                                    let newSubsts = [ for v,s in xs do v, substVarInAwithB genvar s subst ]
                                    substitute newSubsts
                            let final = substitute substsAndC |> snd
                            Constrained final
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
        let constrainExp (exp: Meta<_,Anno>) =
            let findConstraint tyvar =
                let node = findVarNode tyvar nodes
                node.constr
            exp.meta.constr <- findConstraint exp.meta.tyvar
        let rec applyResult (exp: TExp) =
            constrainExp exp
            match exp.exp with
            | Lit x -> ()
            | Var ident -> ()
            | App (e1, e2) ->
                applyResult e1
                applyResult e2
            | Abs (ident, body) ->
                constrainExp ident
                applyResult body
            | Let (ident, e, body) ->
                applyResult e
                applyResult body
            | Prop (ident, e) ->
                applyResult e
            | Tuple es ->
                for e in es do applyResult e
            | Record fields ->
                for _,e in fields do applyResult e
        applyResult exp

