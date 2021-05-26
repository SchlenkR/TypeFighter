namespace TypeFighter

type GenTyVar = int
type Tau =
    | TGenVar of GenTyVar
    | TApp of string * Tau list
    | TFun of Tau * Tau
    | TTuple of Tau list
    | TRecord of TRecordFields
and TRecordField = string * Tau
and TRecordFields = Set<TRecordField>
type UnificationError =
    | Inherit
    | Origin of string
type ConstraintState =
    | Constrained of Tau
    | UnificationError of UnificationError
type Subst = { genTyVar: GenTyVar; substitute: Tau }
type Instanciation = { oldVar: GenTyVar; newVar: GenTyVar }

// TODO: in type inference, respect the fact that annos can be initially constrained

type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit
and Exp<'meta> =
    | Lit of Lit
    | Var of Ident
    | App of MetaExp<'meta> * MetaExp<'meta>
    | Abs of MetaIdent<'meta> * MetaExp<'meta>
    | Let of MetaIdent<'meta> * MetaExp<'meta> * MetaExp<'meta>
    | Tuple of MetaExp<'meta> list

and Meta<'exp, 'meta> = { exp: 'exp; meta: 'meta }
and MetaExp<'meta> = Meta<Exp<'meta>, 'meta>
and MetaIdent<'meta> = Meta<Ident, 'meta>

and TyVar = int
and Ident = string
and EnvItem =
    | Extern of Tau
    | Intern of IExp
and Env = Map<Ident, EnvItem>

and Anno =
    { tyvar: TyVar
      env: Env
      initialConstr: Tau option }
and UExp = Meta<Exp<unit>, unit>
and TExp = MetaExp<Anno>
and IExp = MetaIdent<Anno>

type AstExp =
    | SynExp of TExp
    | EnvExp of IExp

type Counter = unit -> int

[<AutoOpen>]
module Helper =
    let newCounter f exclSeed =
        let mutable varCounter = exclSeed
        fun () -> varCounter <- f varCounter 1; varCounter
    let newUpCounter = newCounter (+)
    let newDownCounter = newCounter (-)
    
    module Map =
        let private xtract f (m: Map<_,_>) = m |> Seq.map f |> Seq.toList
        let keys (m: Map<_,_>) = xtract (fun kvp -> kvp.Key) m
        let values (m: Map<_,_>) = xtract (fun kvp -> kvp.Value) m
        let ofListUnique l =
            let rec build l map =
                match l with
                | [] -> map
                | (k,v) :: xs ->
                    if map |> Map.containsKey k then
                        failwith $"Key already in map: {k}"
                    let map = map |> Map.add k v
                    build xs map
            build l Map.empty

module TypeNames =
    let [<Literal>] string = "String"
    let [<Literal>] number = "Number"
    let [<Literal>] bool = "Bool"
    let [<Literal>] unit = "Unit"
    let [<Literal>] seq = "Seq"

module Lit =
    let getTypeName (l: Lit) =
        match l with
        | LString _ -> TypeNames.string
        | LNumber _ -> TypeNames.number
        | LBool _ -> TypeNames.bool
        | LUnit _ -> TypeNames.unit

module Env =
    let empty : Env = Map.empty
    let bind (exp: IExp) (env: Env) : Env =
        env |> Map.change exp.exp (fun _ -> Some(Intern exp))
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound. Env: {env}"
        | Some t -> t
    let getInterns (env: Env) =
        env 
        |> Map.toList
        |> List.map (fun (ident,v) ->
            match v with 
            | Intern x -> Some (ident,x)
            | _ -> None)
        |> List.choose id
        |> Map.ofListUnique

module Tau =
    let map (proj: Tau -> 'a list) (projGenVar: GenTyVar -> 'a list) (tau: Tau) =
        match tau with
        | TGenVar v -> projGenVar v
        | TApp (_, vars) -> vars |> List.collect proj
        | TFun (t1, t2) -> [ yield! proj t1; yield! proj t2 ]
        | TTuple taus -> taus |> List.collect proj
        | TRecord fields -> [ for _,t in fields do yield! proj t ]

    let mapTree (proj: Tau -> Tau) (projGenVar: GenTyVar -> Tau) (tau: Tau) =
        match tau with
        | TGenVar var -> projGenVar var
        | TApp (ident, taus) -> TApp (ident, [ for t in taus do proj t ])
        | TFun (t1, t2) -> TFun (proj t1, proj t2)
        | TTuple taus -> TTuple [ for t in taus do proj t ]
        | TRecord fields -> TRecord (set [ for f,t in fields do f, proj t ])
    
    let tau = function | Constrained t -> t | _ -> failwith "TODO: unconstrained!"

    let getGenVars (t: Tau) =
        let rec getGenVars (t: Tau) : GenTyVar list =
            t |> map getGenVars (fun v -> [v])
        getGenVars t |> set

    let getGenVarsMany (taus: Tau list) =
        taus |> List.map getGenVars |> List.fold (+) Set.empty

module Format =
    let getUnionCaseName x =
        match Reflection.FSharpValue.GetUnionFields(x, x.GetType()) with
        | c, _ -> c.Name

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

module Annotation =
          
    type AnnotationResult =
        { newGenVar: Counter
          root: TExp
          allExpressions: List<AstExp>
          envExpressions: List<IExp>
          synExpressions: List<TExp>
          identLinks: Map<IExp, TExp option> }
    
    // TODO: Brauchen wir das wirklich?
    let private globalizeGenVars (env: Env) : Counter * Env =
        let newGenVar = newUpCounter(0)
        let mutable varMap = Map.empty<GenTyVar, GenTyVar>
        newGenVar, env |> Map.map (fun _ v ->
            match v with
            | Intern _ -> v
            | Extern tau ->
                let rec remap tau =
                    tau |> Tau.mapTree remap (fun var -> 
                        varMap
                        |> Map.tryFind var
                        |> Option.defaultWith (fun () ->
                            let newVar = newGenVar()
                            varMap <- varMap |> Map.add var newVar
                            newVar)
                        |> TGenVar)
                Extern (remap tau))

    let annotate (env: Env) (exp: UExp) =
        let newGenVar,env = globalizeGenVars env
        let newTyVar = newUpCounter(0)
        let mutable allExpressions = List.empty<AstExp>
        let mutable envExpressions = List.empty<IExp>
        let mutable synExpressions = List.empty<TExp>
        let addExp exp =
            allExpressions <- exp :: allExpressions
            match exp with
            | SynExp exp -> synExpressions <- exp :: synExpressions
            | EnvExp exp -> envExpressions <- exp :: envExpressions
        let rec annotate (env: Env) (exp: UExp) =
            let thisTyvar = newTyVar()
            let newIdent ident env =
                let tyvarIdent = newTyVar()
                let exp =
                    { meta = { tyvar = tyvarIdent; env = env; initialConstr = None }
                      exp = ident.exp }
                do addExp (EnvExp exp)
                let newEnv = env |> Env.bind exp
                exp, newEnv
            let resExp =
                match exp.exp with
                | Lit x ->
                    Lit x
                | Var ident -> 
                    Var ident
                | App (e1, e2) -> 
                    App (annotate env e1, annotate env e2)
                | Abs (ident, body) ->
                    let annotatedIdent,newEnv = newIdent ident env
                    Abs (annotatedIdent, annotate newEnv body)
                | Let (ident, e, body) ->
                    let annotatedIdent,newEnv = newIdent ident env
                    Let (annotatedIdent, annotate env e, annotate newEnv body)
                | Tuple es -> 
                    Tuple (es |> List.map (annotate env))
            let texp =
                { meta = { tyvar = thisTyvar; env = env; initialConstr = None }
                  exp = resExp }
            do addExp (SynExp texp)
            texp
        
        let res = annotate env exp

        let identLinks =
            let mutable boundValues = Map.empty<IExp, TExp option>
            let rec walk (exp: TExp) currentTarget : unit =
                let add x e =
                    boundValues <- boundValues |> Map.add x e
                // TODO: we need a "map" function for TExp
                match exp.exp with
                | Lit _
                | Var _ -> 
                    ()
                | App (e1, e2) ->
                    walk e2 None
                    walk e1 (Some e2)
                | Abs (ident, body) ->
                    do add ident currentTarget
                    walk body None
                | Let (ident, e, body) ->
                    do add ident (Some e)
                    walk e None
                    walk body None
                | Tuple es -> 
                    es |> List.iter (fun e -> walk e None)
            do walk res None
            boundValues

        { newGenVar = newGenVar
          root = res
          allExpressions = allExpressions
          envExpressions = envExpressions
          synExpressions = synExpressions
          identLinks = identLinks }

module ConstraintGraph =

    open Annotation
    type NodeId = int
    
    type TAst = { exp: TExp; inc: NodeId }
    type IAst = { exp: IExp; inc: NodeId }
    type MakeFun = { inc1: NodeId; inc2: NodeId }
    type ArgOut = { inc: NodeId }
    type Unify = { inc1: NodeId; inc2: NodeId }
    type MakeTuple = { incs: NodeId list }
    type MakeRecord = { fields: string list; incs: NodeId list }
    type Inst = { scope: string; inc: NodeId }

    type NodeData =
        | Source of Tau
        | TAst of TAst
        | IAst of IAst
        | MakeFun of MakeFun
        | MakeTuple of MakeTuple
        | ArgOut of ArgOut
        | Unify of Unify
        | Inst of Inst
    
    // TODO: maybe get rid of this and make everything immutable
    and Node (id: NodeId, data: NodeData, constr: (ConstraintState * Set<Instanciation> * Set<Subst>) option) =
        member this.id = id
        member this.data = data
        member val constr = constr with get, set

    type Graph =
        { nodes: Map<NodeId, Node> }

    type SolveResult =
        { graph: Graph
          constrainedNodes: List<Node>
          errorNodes: List<Node>
          unfinishedNodes: List<Node>
          allNodes: List<Node>
          exprConstraintStates: Map<TExp, ConstraintState * Set<Instanciation> * Set<Subst>>
          envConstraintStates: Map<IExp, ConstraintState * Set<Instanciation> * Set<Subst>>
          annotationResult: AnnotationResult
          success: bool }

    module Instanciation =
        let empty = Set.empty<Instanciation>

    module Subst =
        let empty : Set<Subst> = Set.empty

        let instanciate (insts: Set<Instanciation>) (t: Tau) =
            (t, insts) ||> Set.fold (fun t inst ->
                let rec instanciate (t: Tau) =
                    t |> Tau.mapTree instanciate (fun var ->
                        let res = if var = inst.oldVar then inst.newVar else var
                        TGenVar res)
                instanciate t)

        let subst (substs: Set<Subst>) tau =
            let rec substRec (substs: Set<Subst>) tau =
                // TODO: quite similar with remapGenVars
                let substVarInAwithB (tvar: GenTyVar) (a: Tau) (b: Tau) =
                    let rec substTau tau =
                        tau |> Tau.mapTree substTau (fun var -> if var = tvar then b else tau)
                    substTau a
                match substs |> Set.toList with
                | [] -> tau
                | x :: xs ->
                    let taus = substVarInAwithB x.genTyVar tau x.substitute
                    let substs = xs |> List.map (fun next ->
                        let substitute = substVarInAwithB x.genTyVar next.substitute x.substitute
                        { genTyVar = next.genTyVar; substitute = substitute })
                    substRec (set substs) taus
            substRec substs tau

        // TODO: Does this terminate?
        let rec flatten (substs: Set<Subst>) =
            let varGroups = substs |> Set.toList |> List.groupBy (fun x -> x.genTyVar)
            
            // For each genvar, we have a group of n constraints (=substs =taus), where n>0.
            // For each group, we build a unified tau, and that unification also yields
            // a merged list of new substitutions.
            [ for var,substs in varGroups do
                match substs with
                | [] -> failwith "can never happen because we have at least 1 elem in a group"
                | x::xs ->
                    let allSubstitutes = xs |> List.map (fun s -> s.substitute)
                    (Ok(x.substitute, empty), allSubstitutes)
                    ||> List.fold (fun state curr ->
                        state |> Result.bind (fun (utau,us1) ->
                            unify utau curr |> Result.bind (fun (utau,us2) ->
                                // we don't use merge here (why? termination?)
                                Ok (utau, us1 + us2))))
                    |> Result.map (fun (utau,us) ->
                        [
                            { genTyVar = var; substitute = utau }
                            yield! us
                        ] |> set)
            ]
            |> List.fold
                (fun state x ->
                    state |> Result.bind (fun state ->
                        x |> Result.bind (fun x ->
                            Ok (state + x))))
                (Ok Set.empty)
        
        and unify (a: Tau) (b: Tau) =           
            let rec unify1 t1 t2 =
                let error (msg: string) =
                    Error $"""Cannot unify types "{Format.tau t1}" and "{Format.tau t2}": {msg}"""
                match t1,t2 with
                | x,y when x = y ->
                    Ok (x, empty)
                | TGenVar x, y
                | y, TGenVar x ->
                    Ok (y, set [ { genTyVar = x; substitute = y } ])
                | TApp (_, taus1), TApp (_, taus2) when taus1.Length <> taus2.Length ->
                    error "Generic argument count mismatch"
                | TApp (n1,_), TApp (n2, _) when n1 <> n2 ->
                    error "Type mismatch"
                | TApp (name, taus1), TApp (_, taus2) ->
                    let res = unifyMany taus1 taus2
                    match res with
                    | Ok (taus, substs) ->
                        Ok (TApp (name, taus), substs)
                    | Error e -> 
                        Error e
                | TFun (ta1, ta2), TFun (tb1, tb2) ->
                    match unifyMany [ta1; ta2] [tb1; tb2] with
                    | Ok ([tres1; tres2], substs) ->
                        Ok (TFun (tres1, tres2), substs)
                    | Error e ->
                        Error e
                    | _ ->
                        failwith $"inconsistent TFun unification: {t1} <> {t2}"
                | _ ->
                    error "Unspecified cases"
            
            and unifyMany taus1 taus2 =
                let unifiedTaus = [ for t1,t2 in List.zip taus1 taus2 do unify1 t1 t2 ]
                let rec allOk (taus: Tau list) (allSubsts: Set<Subst>) remainingTaus =
                    match remainingTaus with
                    | [] -> Ok (taus, allSubsts)
                    | x :: xs ->
                        match x with
                        | Ok (tau, substs) -> allOk (taus @ [tau]) (allSubsts + substs) xs
                        | Error e -> Error e
                allOk [] empty unifiedTaus
            
            // TODO: consistency check of unifiers? (!IMPORTANT)
            unify1 a b
        
    let getIncomingNodeIds (node: Node) =
        match node.data with
        | Source _ -> []
        | TAst x -> [ x.inc ]
        | IAst x -> [ x.inc ]
        | MakeFun x -> [ x.inc1; x.inc2 ]
        | MakeTuple x -> x.incs
        | ArgOut x -> [ x.inc ]
        | Unify x -> [ x.inc1; x.inc2 ]
        | Inst x -> [ x.inc ]

    // TODO: Rückgabe als annoRes + nodes, damit man besser pipen kann
    let createGraph (annoRes: AnnotationResult) : Graph =
        let mutable nodes = Map.empty<NodeId, Node>

        let addNode =
            // TODO: das ist ein bisschen wackelig
            let nextNodeId = newDownCounter(0)
            fun n constr tyvar ->
                let nodeId = tyvar |> Option.defaultValue (nextNodeId())
                let node = Node(nodeId, n, constr)
                do nodes <- nodes |> Map.add node.id node
                node.id
        let source tau = addNode (Source tau) (Some (Constrained tau, Instanciation.empty, Subst.empty)) None
        let newGenVarSource() = source (TGenVar (annoRes.newGenVar()))
        let tast exp constr inc = addNode (TAst { exp = exp; inc = inc }) constr (Some exp.meta.tyvar)
        let iast exp inc = addNode (IAst { exp = exp; inc = inc }) None (Some exp.meta.tyvar)
        let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) None None
        let makeTuple incs = addNode (MakeTuple { incs = incs }) None None
        let argOut inc = addNode (ArgOut { inc = inc }) None None
        let unify inc1 inc2 = addNode (Unify { inc1 = inc1; inc2 = inc2 }) None None
        let inst scope inc = addNode (Inst { scope = scope; inc = inc }) None None

        let ( ==> ) (n1, maybeN2) f =
            match maybeN2 with
            | None -> f n1
            | Some n2 -> unify n1 n2 |> f
        
        let rec generateGraph (exp: TExp) (inc: NodeId option) =
            let nthis parent =
                let constr = 
                    exp.meta.initialConstr 
                    |> Option.map (fun t -> Constrained t, Instanciation.empty, Subst.empty)
                tast exp constr parent
            
            let res =
                match exp.exp with
                | Lit x ->
                    let nsource = source (TApp(Lit.getTypeName x, []))
                    (nsource, inc) ==> nthis
                | Var ident ->
                    let nsource =
                        match Env.resolve ident exp.meta.env with
                        | Intern exp ->
                            // TODO: Hier kann ein Fehler auftreten, wenn ident nicht gebunden ist
                            nodes
                            |> Seq.find (fun x ->
                                match x.Value.data with
                                | IAst { exp = envExp }
                                  when envExp = exp -> true
                                | _ -> false)
                            |> fun n -> n.Key
                        | Extern c -> source c
                    let nsourceInst = nsource |> inst (Format.getUnionCaseName exp.exp)
                    (nsourceInst, inc) ==> nthis
                | App (e1, e2) ->
                    (*
                        e1 e2
                            - t(e1): t1 -> t2
                            - t(e2) = t1
                            - t(app): t2
                    *)
                    let ne2 = None |> generateGraph e2
                    let nto = newGenVarSource()
                    let nfunc = makeFunc ne2 nto
                    let ne1 = None |> generateGraph e1
                    let uniAndArgOut = unify ne1 nfunc |> argOut
                    (uniAndArgOut, inc) ==> nthis
                | Abs (ident, body) ->
                    // TODO: anstatt (newGenVarSource()) muss nun der andere Weretswertwertfg
                    let nsource =
                        match annoRes.identLinks |> Map.find ident with
                        | Some identRef -> identRef.meta.tyvar
                        | None -> newGenVarSource()
                    let nident = iast ident nsource
                    let nbody = generateGraph body None
                    let nfunc = makeFunc nident nbody
                    (nfunc, inc) ==> nthis
                | Let (ident, e, body) ->
                    do
                        (generateGraph e None, None) ==> iast ident
                        |> ignore
                    (generateGraph body None, inc) ==> nthis
                | Tuple es ->
                    let ntuple = [ for e in es do generateGraph e None ] |> makeTuple
                    (ntuple, inc) ==> nthis
            res

        do generateGraph (annoRes.root) None |> ignore
        
        { nodes = nodes }
    
    let solve (annoRes: AnnotationResult) (graph: Graph) =

        // TODO: Hier ggf. was machen
        let (|Cons|Err|Init|) (nodeId: NodeId) =
            match graph.nodes |> Map.tryFind nodeId with
            | Some node ->
                match node.constr with
                | Some (Constrained tau, insts, substs) -> Cons (tau,insts,substs)
                | Some (UnificationError e, insts, substs) -> Err e
                | None -> Init
            | None -> Err (Origin "TODO: Inconsistent graph")

        let (|AnyError|AnyInitial|AllConstrained|) (nodes: NodeId list) =
            let cs = nodes |> List.choose (function | Cons x -> Some x | _ -> None)
            let es = nodes |> List.choose (function | Err x -> Some x | _ -> None)
            let ns = nodes |> List.choose (function | Init x -> Some x | _ -> None)
            match cs,es,ns with
            | _,es::_,_ -> AnyError es
            | _,_,ns::_ -> AnyInitial ns
            | cs,[],[] ->
                let taus = cs |> List.map (fun (x,_,_) -> x)
                let insts = cs |> List.map (fun (_,x,_) -> x) |> List.fold (+) Set.empty
                let substs = cs |> List.map (fun (_,_,x) -> x) |> List.fold (+) Set.empty
                AllConstrained (taus,insts,substs)
                
        let constrainNodeData nodeData =
            match nodeData with
            | Source tau ->
                Constrained tau, Instanciation.empty, Subst.empty
            | TAst { inc = Cons(t,i,s) }
            | IAst { inc = Cons(t,i,s) } ->
                let flattenedSubsts = s |> Subst.flatten
                match flattenedSubsts with
                | Error msg ->
                    UnificationError(Origin msg), Instanciation.empty, Subst.empty
                | Ok substs ->
                    let t = Subst.instanciate i t
                    let t = Subst.subst substs t
                    Constrained t, i, substs
            | MakeFun { inc1 = Cons(t1,i1,s1); inc2 = Cons(t2,i2,s2) } ->
                Constrained(TFun (t1, t2)), i1 + i2, s1 + s2
            | MakeTuple { incs = AllConstrained (taus,insts,substs) } ->
                Constrained(TTuple taus), insts, substs
            | ArgOut { inc = Cons(TFun(_,t2), i, s) } ->
                Constrained t2, i, s
            | ArgOut { inc = Cons (t,i,s) } ->
                UnificationError(Origin $"Function type expected, but was: {Format.tau t}"), i, s
            | Unify { inc1 = Cons (t1,i1,s1); inc2 = Cons (t2,i2,s2) } ->
                // TODO: unify2Types is not distributiv (macht aber auch nichts, oder?)
                match Subst.unify t1 t2 with
                | Error msg ->
                    UnificationError(Origin msg), Instanciation.empty, Subst.empty
                | Ok (tres,sres) ->
                    Constrained tres, (i1 + i2), (sres + s1 + s2)
            | Inst { inc = Cons(t,i,s) } ->
                let ftv =
                    let tgenvars = Tau.getGenVars t
                    let substsGenVars = s |> Set.map (fun x -> x.genTyVar)
                    tgenvars - substsGenVars
                let instances = ftv |> Set.map (fun x -> 
                    { oldVar = x
                      newVar = annoRes.newGenVar() })
                Constrained t, i + instances, s
            | _ ->
                // for now, let's better fail here so that we can detect valid error cases and match them
                failwith $"Invalid graph at node: {nodeData}", Instanciation.empty, Subst.empty

        let rec constrainNodes
                (unfinished: ResizeArray<Node>) 
                (constrained: ResizeArray<Node>) 
                (errors: ResizeArray<Node>) =
            let mutable goOn = false
            for node in unfinished |> Seq.toArray do
                let constrainRes = 
                    let incomingNodes = getIncomingNodeIds node
                    match incomingNodes with
                    | AnyInitial _ -> 
                        None
                    | AllConstrained _ ->
                        Some(constrainNodeData node.data)
                    | AnyError err ->
                        let inh = Some(UnificationError Inherit, Instanciation.empty, Subst.empty)
                        let err = Some(UnificationError err, Instanciation.empty, Subst.empty)
                        let allIncomingsAreOperations = 
                            let isOp =
                                function
                                | Source _ 
                                | TAst _ | IAst _ -> false 
                                | _ -> true
                            incomingNodes
                            |> List.map (fun nodeId -> graph.nodes |> Map.find nodeId)
                            |> List.map (fun n -> isOp n.data)
                            |> List.contains false 
                            |> not
                        match allIncomingsAreOperations, node.data with
                        | true, _
                        | false, IAst _ -> err
                        | _ -> inh

                node.constr <- constrainRes
                match constrainRes with
                | Some x ->
                    unfinished.Remove(node) |> ignore
                    goOn <- true
                    match x with
                    | Constrained _, _, _ -> constrained.Add(node)
                    | UnificationError _, _, _ -> errors.Add(node)
                | _ -> ()

            if not goOn then
                { graph = graph
                  constrainedNodes = constrained |> List.ofSeq
                  errorNodes = errors |> List.ofSeq
                  unfinishedNodes = unfinished |> List.ofSeq
                  allNodes = [
                    yield! constrained
                    yield! unfinished
                    yield! errors ]
                  exprConstraintStates = Map.empty
                  envConstraintStates = Map.empty
                  annotationResult = annoRes
                  success = errors.Count = 0 && unfinished.Count = 0 }
            else
                constrainNodes unfinished constrained errors
        
        let res = constrainNodes (ResizeArray (graph.nodes |> Map.values)) (ResizeArray()) (ResizeArray())

        let filterNodes f =
            [ for node in graph.nodes |> Map.values do
                let x = f node.data
                match x with
                | Some x -> Some (x, node.constr.Value)
                | _ -> None
            ]
            |> List.choose id 
            |> Map.ofListUnique

        let envConstraintStates =
            filterNodes (function | IAst ast -> Some ast.exp | _ -> None)

        let exprConstraintStates =
            filterNodes (function | TAst ast -> Some ast.exp | _ -> None)

        { res with
            exprConstraintStates = exprConstraintStates
            envConstraintStates = envConstraintStates }
