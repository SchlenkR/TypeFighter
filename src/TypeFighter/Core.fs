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

// TODO: in type inference, respect the fact that annos can be initially constrained

type TyVar = int
type Ident = string
type EnvItem =
    | Extern of Tau
    | Intern of thisVar: TyVar * parentVar: TyVar
type Env = Map<Ident, EnvItem>

type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit
type Exp<'meta> =
    | Lit of Lit
    | Var of Ident
    | App of MetaExp<'meta> * MetaExp<'meta>
    | Abs of MetaIdent<'meta> * MetaExp<'meta>
    | Let of MetaIdent<'meta> * MetaExp<'meta> * MetaExp<'meta>
    | Prop of Ident * MetaExp<'meta>
    | Tuple of MetaExp<'meta> list
    | Record of (Ident * MetaExp<'meta>) list
and Meta<'exp, 'meta> = { exp: 'exp; meta: 'meta }
and MetaExp<'meta> = Meta<Exp<'meta>, 'meta>
and MetaIdent<'meta> = Meta<Ident, 'meta>

type Anno =
    { tyvar: TyVar
      env: Env
      initialConstr: Tau option }
type UExp = Meta<Exp<unit>, unit>
type TExp = MetaExp<Anno>
type IExp = MetaIdent<Anno>



[<AutoOpen>]
module Helper =
    type Counter(exclSeed) =
        let mutable varCounter = exclSeed
        member this.next() = varCounter <- varCounter + 1; varCounter
    
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
    let bind ident (tyvar: TyVar) (parentTyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some(Intern (tyvar,parentTyvar)))
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound. Env: {env}"
        | Some t -> t
    let getInterns (env: Env) =
        env 
        |> Map.toList
        |> List.map (fun (ident,v) ->
            match v with 
            | Intern (tyvar,parentTyvar) -> Some (ident,(tyvar,parentTyvar))
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

    let mapEndo (proj: Tau -> Tau) (projGenVar: GenTyVar -> Tau) (tau: Tau) =
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


module AnnotatedAst =

    type AnnotationResult =
        { newGenVar: Counter
          root : TExp
          allExpressions: List<TExp> }

    let private remapGenVars (env: Env) : Counter * Env =
        let newGenVar = Counter(0)
        let mutable varMap = Map.empty<GenTyVar, GenTyVar>
        newGenVar, env |> Map.map (fun _ v ->
            match v with
            | Intern _ -> v
            | Extern tau ->
                let rec remap tau =
                    tau |> Tau.mapEndo remap (fun var -> 
                        varMap
                        |> Map.tryFind var
                        |> Option.defaultWith (fun () ->
                            let newVar = newGenVar.next()
                            varMap <- varMap |> Map.add var newVar
                            newVar)
                        |> TGenVar)
                Extern (remap tau))

    let create (env: Env) (exp: UExp) =
        let newGenVar,env = remapGenVars env
        let newTyVar = Counter(0)
        let mutable allExp = []
        let rec annotate (env: Env) (exp: UExp) =
            let newIdent ident parentTyvar env =
                let tyvarIdent = newTyVar.next()
                let newEnv = env |> Env.bind ident.exp tyvarIdent parentTyvar
                let exp =
                    { meta =
                        { tyvar = tyvarIdent
                          env = env
                          initialConstr = None }
                      exp = ident.exp }
                exp, newEnv
            let texp =
                let thisTyvar = newTyVar.next()
                { meta =
                    { tyvar = thisTyvar
                      env = env
                      initialConstr = None }
                  exp =
                    match exp.exp with
                    | Lit x ->
                        Lit x
                    | Var ident -> 
                        Var ident
                    | App (e1, e2) -> 
                        App (annotate env e1, annotate env e2)
                    | Abs (ident, body) ->
                        let annotatedIdent,newEnv = newIdent ident thisTyvar env
                        Abs (annotatedIdent, annotate newEnv body)
                    | Let (ident, e, body) ->
                        let eexp = annotate env e
                        let annotatedIdent,newEnv = newIdent ident eexp.meta.tyvar env
                        Let (annotatedIdent, eexp, annotate newEnv body)
                    | Prop (ident, e) -> 
                        Prop (ident, annotate env e)
                    | Tuple es -> 
                        Tuple (es |> List.map (annotate env))
                    | Record fields -> 
                        Record [ for ident,e in fields do ident, annotate env e ]
                }
            do allExp <- texp :: allExp
            texp
        let res = annotate env exp
        { newGenVar = newGenVar
          root = res
          allExpressions = allExp |> Seq.toList }

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

module ConstraintGraph =

    type Ast = { exp: AstExp; inc: Node }
    and AstExp =
        | TExp of TExp
        | IExp of IExp
    and MakeFun = { inc1: Node; inc2: Node }
    and ArgOut = { inc: Node }
    and Unify = { inc1: Node; inc2: Node }
    and GetProp = { field: string; inc: Node }
    and MakeTuple = { incs: Node list }
    and MakeRecord = { fields: string list; incs: Node list }
    and Inst = { scope: string; inc: Node }

    and NodeData =
        | Source of Tau
        | Ast of Ast
        | MakeFun of MakeFun
        | GetProp of GetProp
        | MakeTuple of MakeTuple
        | MakeRecord of MakeRecord
        | ArgOut of ArgOut
        | Unify of Unify
        | Inst of Inst
    
    // TODO: maybe get rid of this and make everything immutable
    and Node (data: NodeData, constr: (ConstraintState * Set<Subst>) option) =
        member this.data = data
        member val constr = constr with get, set

    type SolveResult =
        { constrainedNodes: List<Node>
          errorNodes: List<Node>
          unfinishedNodes: List<Node>
          allNodes: List<Node>
          exprConstraintStates: Map<TExp, ConstraintState * Set<Subst>>
          envConstraintStates: Map<IExp, ConstraintState * Set<Subst>>
          annotationResult: AnnotatedAst.AnnotationResult
          success: bool }

    module Unification =
        let empty : Set<Subst> = Set.empty

        let subst (substs: Set<Subst>) tau =
            let rec substRec (substs: Set<Subst>) tau =
                // TODO: quite similar with remapGenVars
                let substVarInAwithB (tvar: GenTyVar) (a: Tau) (b: Tau) =
                    let rec substTau tau =
                        tau |> Tau.mapEndo substTau (fun var -> if var = tvar then b else tau)
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
                            { Subst.genTyVar = var; Subst.substitute = utau }
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

    let getIncomingNodes (node: Node) =
        match node.data with
        | Source _ -> []
        | Ast x -> [ x.inc ]
        | MakeFun x -> [ x.inc1; x.inc2 ]
        | GetProp x -> [ x.inc ]
        | MakeTuple x -> x.incs
        | MakeRecord x -> x.incs
        | ArgOut x -> [ x.inc ]
        | Unify x -> [ x.inc1; x.inc2 ]
        | Inst x -> [ x.inc ]
        
    // TODO: Rückgabe als annoRes + nodes, damit man besser pipen kann
    let create (annoRes: AnnotatedAst.AnnotationResult) =
        let nodes = ResizeArray()

        let addNode n constr =
            let node = Node(n, constr)
            do nodes.Add node
            node
        let source tau = addNode (Source tau) (Some (Constrained tau, Unification.empty))
        let newGenVarSource() = source (TGenVar (annoRes.newGenVar.next()))
        let ast exp constr inc = addNode (Ast { exp = exp; inc = inc }) constr
        let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) None
        let getProp field inc = addNode (GetProp { field = field; inc = inc }) None
        let makeTuple incs = addNode (MakeTuple { incs = incs }) None
        let makeRecord fields incs = addNode (MakeRecord { fields = fields; incs = incs }) None
        let argOut inc = addNode (ArgOut { inc = inc }) None
        let unify inc1 inc2 = addNode (Unify { inc1 = inc1; inc2 = inc2 }) None
        let inst scope inc = addNode (Inst { scope = scope; inc = inc }) None

        let rec generateGraph (exp: TExp) (inc: Node option) =
            let ( ==> ) (n1, maybeN2) f =
                match maybeN2 with
                | None -> f n1
                | Some n2 -> unify n1 n2 |> f
            
            let nthis parent =
                let constr = exp.meta.initialConstr |> Option.map (fun t -> Constrained t, Unification.empty)
                ast (TExp exp) constr parent
            
            let res =
                match exp.exp with
                | Lit x ->
                    let nsource = source (TApp(Lit.getTypeName x, []))
                    (nsource, inc) ==> nthis
                | Var ident ->
                    let nsource =
                        match Env.resolve ident exp.meta.env with
                        | Intern (tyvarIdent,parentTyvar) ->
                            nodes |> Seq.find (fun n ->
                                match n.data with
                                | Ast { exp = TExp { meta = meta } }
                                | Ast { exp = IExp { meta = meta } }
                                  when meta.tyvar = tyvarIdent ->
                                    true
                                | _ ->
                                    false)
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
                    let nident = ast (IExp ident) None (newGenVarSource())
                    let nbody = generateGraph body None
                    let nfunc = makeFunc nident nbody
                    (nfunc, inc) ==> nthis
                | Let (ident, e, body) ->
                    do
                        (generateGraph e None, None) ==> ast (IExp ident) None
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
            res

        do generateGraph (annoRes.root) None |> ignore
        nodes |> Seq.toList
    
    let solve (annoRes: AnnotatedAst.AnnotationResult) (nodes: Node list) =
        // TODO: Hier ggf. was machen
        let (|Cons|Err|Init|) (node: Node) =
            match node.constr with
            | Some (Constrained tau, substs) -> Cons (tau,substs)
            | Some (UnificationError e, substs) -> Err e
            | None -> Init

        let (|AnyError|AnyInitial|AllConstrained|) (nodes: Node list) =
            let cs = nodes |> List.choose (function | Cons x -> Some x | _ -> None)
            let es = nodes |> List.choose (function | Err x -> Some x | _ -> None)
            let ns = nodes |> List.choose (function | Init x -> Some x | _ -> None)
            match cs,es,ns with
            | _,es::_,_ -> AnyError es
            | _,_,ns::_ -> AnyInitial ns
            | cs,[],[] ->
                let taus = cs |> List.map fst
                let substs = cs |> List.map snd |> List.fold (+) Set.empty
                AllConstrained (taus,substs)
                
        let constrainNodeData nodeData =
            match nodeData with
            | Source tau ->
                Constrained tau, Unification.empty
            | Ast { exp = exp; inc = Cons(t,s) } ->
                let flattenedSubsts = s |> Unification.flatten
                match flattenedSubsts with
                | Error msg ->
                    UnificationError(Origin msg), Unification.empty
                | Ok substs ->
                    let t = Unification.subst substs t
                    Constrained t, substs
            | MakeFun { inc1 = Cons(t1,s1); inc2 = Cons(t2,s2) } ->
                Constrained(TFun (t1, t2)), s1 + s2
            | GetProp { field = field; inc = Cons(TRecord recordFields, s) } ->
                let res =
                    recordFields
                    |> Seq.tryFind (fun (n,_) -> n = field)
                    |> Option.map snd
                    |> Option.map Constrained
                    // TODO: this is not a unification error!
                    |> Option.defaultValue (UnificationError(Origin $"Field not found: {field}"))
                res, s
            | MakeTuple { incs = AllConstrained (taus,substs) } ->
                Constrained(TTuple taus), substs
            | MakeRecord { fields = fields; incs = AllConstrained (taus,substs) } ->
                let fields = List.zip fields taus
                Constrained(TRecord (set fields)), substs
            | ArgOut { inc = Cons(TFun(_,t2), s) } ->
                Constrained t2, s
            | ArgOut { inc = Cons (t,s) } ->
                UnificationError(Origin $"Function type expected, but was: {Format.tau t}"), s
            | Unify { inc1 = Cons (t1,s1); inc2 = Cons (t2,s2) } ->
                // TODO: unify2Types is not distributiv (macht aber auch nichts, oder?)
                match Unification.unify t1 t2 with
                | Error msg ->
                    UnificationError(Origin msg), Unification.empty
                | Ok (tres,sres) ->
                    Constrained tres, (sres + s1 + s2)
            | Inst { inc = Cons(t, s) } ->
                let ftv =
                    let tgenvars = Tau.getGenVars t
                    let substsGenVars = s |> Set.map (fun x -> x.genTyVar)
                    tgenvars - substsGenVars
                let instances = ftv |> Set.map (fun x -> 
                    { Subst.genTyVar = x; Subst.substitute = TGenVar (annoRes.newGenVar.next()) })
                let replacedTau = Unification.subst instances t
                Constrained replacedTau, s + instances
            | _ ->
                // for now, let's better fail here so that we can detect valid error cases and match them
                failwith $"Invalid graph at node: {nodeData}", Unification.empty

        let rec constrainNodes
                (unfinished: ResizeArray<Node>) 
                (constrained: ResizeArray<Node>) 
                (errors: ResizeArray<Node>) =
            let mutable goOn = false
            for node in unfinished |> Seq.toArray do
                let constrainRes = 
                    let incomingNodes = getIncomingNodes node
                    match incomingNodes with
                    | AnyInitial _ -> 
                        None
                    | AllConstrained _ ->
                        Some(constrainNodeData node.data)
                    | AnyError err ->
                        let inh = Some(UnificationError Inherit, Unification.empty)
                        let err = Some(UnificationError err, Unification.empty)
                        let allIncomingsAreOperations = 
                            let isOp = function | Source _ | Ast _ -> false | _ -> true
                            incomingNodes |> List.map (fun n -> isOp n.data)|> List.contains false |> not
                        match allIncomingsAreOperations,node.data with
                        | true, _ -> err
                        | false, Ast ast ->
                            match ast.exp with
                            | TExp _ -> inh
                            | IExp _ -> err
                        | false, _ -> inh

                node.constr <- constrainRes
                match constrainRes with
                | Some x ->
                    unfinished.Remove(node) |> ignore
                    goOn <- true
                    match x with
                    | Constrained _, _ -> constrained.Add(node)
                    | UnificationError _, _ -> errors.Add(node)
                | _ -> ()

            if not goOn then
                { constrainedNodes = constrained |> List.ofSeq
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
        
        let res = constrainNodes (ResizeArray nodes) (ResizeArray()) (ResizeArray())
        
        let allAsts =
            [ for n in nodes do
                match n.data with
                | Ast ast -> Some (ast, n.constr)
                | _ -> None
            ]
            |> List.choose id

        let doIt f =
            [ for ast,constr in allAsts do
                let x = f ast.exp
                match x with
                | Some x -> Some (x, constr.Value)
                | _ -> None
            ]
            |> List.choose id 
            |> Map.ofListUnique

        let envConstraintStates =
            doIt (function | IExp exp -> Some exp | _ -> None)

        let exprConstraintStates =
            doIt (function | TExp exp -> Some exp | _ -> None)

        { res with
            exprConstraintStates = exprConstraintStates
            envConstraintStates = envConstraintStates }
