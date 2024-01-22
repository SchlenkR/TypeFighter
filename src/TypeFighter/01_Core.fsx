
type TVar = int
type Typ =
    | TVar of TVar
    | TApp of name: string * typArgs: Typ list
    | TFun of funTyp: Typ * argTyp: Typ
    | TTuple of Typ list
    | TRecord of TRecordFields
        override this.ToString() = ShowTyp.ToString(this)

and TRecordField = { fname: string; typ: Typ }
and TRecordFields = Set<TRecordField>

and ShowTyp =
    static member ToString(t: Typ) =
        let tvarToString (x: TVar) = $"tv{x}"

        match t with
        | TVar x -> 
            tvarToString x
        | TApp (name, typArgs) ->
            match typArgs with
            | [] -> name
            | _ ->
                let typArgs = typArgs |> List.map _.ToString() |> String.concat ", "
                $"{name}<{typArgs}>"
        | TFun (t1, t2) ->
            $"({t1} -> {t2})"
        | TTuple taus ->
            taus |> List.map _.ToString() |> String.concat " * " |> sprintf "(%s)"
        | TRecord fields ->
            [ for f in fields do $"{f.fname}: {f.typ}" ] |> String.concat "; " |> sprintf "{ %s }"

type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit

type Exp<'anno> =
    | Lit of {| value: Lit; tanno: 'anno |}
    | Var of {| ident: string; tanno: 'anno |}
    | App of {| func: Exp<'anno>; arg: Exp<'anno>; tanno: 'anno |}
    | Fun of {| ident: Ident<'anno>; body: Exp<'anno>; tanno: 'anno |}
    | Let of {| ident: Ident<'anno>; value: Exp<'anno>; body: Exp<'anno>; tanno: 'anno |}
    | PropAcc of {| ident: Ident<'anno>; source: Exp<'anno>; tanno: 'anno |}
    | Tuple of {| exprs: Exp<'anno> list; tanno: 'anno |}
    | Record of {| fields: Field<'anno> list; tanno: 'anno |}
        member this.tanno =
            match this with
            | Lit x -> x.tanno
            | Var x -> x.tanno
            | App x -> x.tanno
            | Fun x -> x.tanno
            | Let x -> x.tanno
            | PropAcc x -> x.tanno
            | Tuple x -> x.tanno
            | Record x -> x.tanno
and Field<'anno> = { name: Ident<'anno>; value: Exp<'anno>; tanno: 'anno }
and Ident<'anno> = { nane: string; tanno: 'anno }

[<AutoOpen>]
module Helper =
    type Counter() =
        let mutable currVar = 0
        member this.next() =
            currVar <- currVar + 1
            currVar
    
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
        | LUnit -> TypeNames.unit

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some(Intern tyvar))
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound. Env: {env}"
        | Some t -> t
    let getInterns (env: Env) =
        env 
        |> Map.toList
        |> List.map (fun (ident,v) -> match v with | Intern tyvar -> Some (ident,tyvar) | _ -> None)
        |> List.choose id
        |> Map.ofListUnique

module Typ =
    let getTyp = function Constrained t -> t | _ -> failwith "TODO: unconstrained!"

    let getGenVars (t: Typ) =
        let rec getGenVars (t: Typ) =
            let map (proj: Typ -> TVar list) (typ: Typ) =
                match typ with
                | TVar v -> [v]
                | TApp (_, typs) -> typs |> List.collect proj
                | TFun (t1, t2) -> [ yield! proj t1; yield! proj t2 ]
                | TTuple typs -> typs |> List.collect proj
                | TRecord fields -> [ for field in fields do yield! proj field.typ ]
            t |> map getGenVars
        getGenVars t |> set


type AnnotationResult =
    { 
        newGenVar: Counter
        root : Exp
        allExpressions: List<Exp>
        allEnvVars: Map<TVar, Ident * Exp>
    }

module ConstraintGraph =

    type Ast = { term: Term; inc: Node }
    and Term =
        | ExprTerm of Exp
        | IdentTerm of Ident
    and MakeFun = { inc1: Node; inc2: Node }
    and ArgOp = In | Out // TODO: In braucht man nicht mehr
    and Arg = { argOp: ArgOp; inc: Node }
    and Unify = { inc1: Node; inc2: Node }
    and GetProp = { field: string; inc: Node }
    and MakeTuple = { incs: Node list }
    and MakeRecord = { fields: string list; incs: Node list }
    and Inst = { scope: string; inc: Node }

    and NodeData =
        | Source of Typ
        | Ast of Ast
        | MakeFun of MakeFun
        | GetProp of GetProp
        | MakeTuple of MakeTuple
        | MakeRecord of MakeRecord
        | Arg of Arg
        | Unify of Unify
        | Inst of Inst
    
    // TODO: maybe get rid of this and make everything immutable
    and Node (data: NodeData, constr: (ConstraintState * Set<Subst>) option) =
        member this.data = data
        member val constr = constr with get, set

    type SolveResult =
        { 
            constrainedNodes: List<Node>
            errorNodes: List<Node>
            unfinishedNodes: List<Node>
            allNodes: List<Node>
            exprConstraintStates: Map<Exp, ConstraintState * Set<Subst>>
            envConstraintStates: Map<Ident, ConstraintState * Set<Subst>>
            annotationResult: AnnotationResult
            success: bool 
        }

    module SolveResult =
        let findTypsAndSubsts (exp: Exp) sr =
            sr.exprConstraintStates
            |> Map.find exp
            |> fun (cs,substs) ->
                let tau = Typ.getTyp cs
                tau,substs
        let findTau (exp: Exp) sr = findTypsAndSubsts exp sr |> fst

    module Unification =
        let empty : Set<Subst> = Set.empty

        let subst (substs: Set<Subst>) tau =
            let rec substRec (substs: Set<Subst>) tau =
                let substVarInAwithB (tvar: TVar) (a: Typ) (b: Typ) =
                    let rec subst typ =
                        match typ with
                        | TVar var -> if var = tvar then b else typ
                        | TApp (ident, typs) -> TApp (ident, [ for t in typs do subst t ])
                        | TFun (t1, t2) -> TFun (subst t1, subst t2)
                        | TTuple typs -> TTuple [ for t in typs do subst t ]
                        | TRecord fields -> TRecord (set [ for field in fields do { fname = field.fname; typ = subst field.typ} ])
                    subst a
                match substs |> Set.toList with
                | [] -> tau
                | x :: xs ->
                    let taus = substVarInAwithB x.genTyVar tau x.substitute
                    let substs = xs |> List.map (fun next ->
                        let substitute = substVarInAwithB x.genTyVar next.substitute x.substitute
                        { genTyVar = next.genTyVar; substitute = substitute })
                    substRec (set substs) taus
            substRec substs tau

        let rec flatten (substs: Set<Subst>) =

            //Ok substs

            // TODO: Does this terminate?

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
        
        and unify (a: Typ) (b: Typ) =           
            let rec unify1 t1 t2 =
                let error (msg: string) = Error $"""Cannot unify types "{t1}" and "{t2}": {msg}"""
                match t1,t2 with
                | x,y when x = y ->
                    Ok (x, empty)
                | TVar x, y
                | y, TVar x ->
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
                let rec allOk (taus: Typ list) (allSubsts: Set<Subst>) remainingTaus =
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
        | Arg x -> [ x.inc ]
        | Unify x -> [ x.inc1; x.inc2 ]
        | Inst x -> [ x.inc ]
        
    // TODO: Rückgabe als annoRes + nodes, damit man besser pipen kann
    let create (annoRes: AnnotationResult) =
        let nodes = ResizeArray()

        let addNode n constr =
            let node = Node(n, constr)
            do nodes.Add node
            node
        let source tau = addNode (Source tau) (Some (Constrained tau, Unification.empty))
        let newGenVarSource() = source (TVar (annoRes.newGenVar.next()))
        let ast exp constr inc = addNode (Ast { term = exp; inc = inc }) constr
        let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) None
        let getProp field inc = addNode (GetProp { field = field; inc = inc }) None
        let makeTuple incs = addNode (MakeTuple { incs = incs }) None
        let makeRecord fields incs = addNode (MakeRecord { fields = fields; incs = incs }) None
        let arg op inc = addNode (Arg { argOp = op; inc = inc }) None
        let argIn = arg In
        let argOut = arg Out
        let unify inc1 inc2 = addNode (Unify { inc1 = inc1; inc2 = inc2 }) None
        let inst scope inc = addNode (Inst { scope = scope; inc = inc }) None

        let rec generateGraph (exp: Exp) (inc: Node option) =
            let ( ==> ) (n1, maybeN2) f =
                match maybeN2 with
                | None -> f n1
                | Some n2 -> unify n1 n2 |> f
            
            let nthis parent =
                let constr = exp.tanno.explicitConstr |> Option.map (fun t -> Constrained t, Unification.empty)
                ast (ExprTerm exp) constr parent
            
            let res =
                match exp with
                | Lit x ->
                    let nsource = source (TApp(Lit.getTypeName x, []))
                    (nsource, inc) ==> nthis
                | Var x ->
                    let nsource =
                        match Env.resolve x.ident.nane exp.tanno.env with
                        | Intern tyvarIdent ->
                            nodes |> Seq.find (fun n ->
                                match n.data with
                                | Ast { term = TExp { meta = meta } }
                                | Ast { term = IExp { meta = meta } }
                                  when meta.tyvar = tyvarIdent ->
                                    true
                                | _ ->
                                    false)
                        | Extern c -> source c
                    (nsource, inc) ==> nthis
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
                | Fun (ident, body) ->
                    let nident = ast (IExp ident) None (newGenVarSource())
                    let nbody = generateGraph body None
                    let nfunc = makeFunc nident nbody
                    (nfunc, inc) ==> nthis
                | Let (ident, e, body) ->
                    do
                        (generateGraph e None, None) ==> ast (IExp ident) None
                        |> ignore
                    (generateGraph body None, inc) ==> nthis
                | PropAcc (ident, e) -> 
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
            res |> inst (Format.getUnionCaseName exp.exp)

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
            | Ast { term = exp; inc = Cons(t,s) } ->
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
            | Arg { argOp = In; inc = Cons(TFun(t1,_), s) } ->
                Constrained t1, s
            | Arg { argOp = Out; inc = Cons(TFun(_,t2), s) } ->
                Constrained t2, s
            | Arg { argOp = argOp; inc = Cons (t,s) } ->
                UnificationError(Origin $"Function type expected ({argOp}), but was: {Format.tau t}"), s
            | Unify { inc1 = Cons (t1,s1); inc2 = Cons (t2,s2) } ->
                // TODO: unify2Types is not distributiv (macht aber auch nichts, oder?)
                match Unification.unify t1 t2 with
                | Error msg ->
                    UnificationError(Origin msg), Unification.empty
                | Ok (tres,sres) ->
                    Constrained tres, (sres + s1 + s2)
            | Inst { inc = Cons(t, s) } ->
                let ftv =
                    let tgenvars = Typ.getGenVars t
                    let substsGenVars = s |> Set.map (fun x -> x.genTyVar)
                    tgenvars - substsGenVars
                let instances = ftv |> Set.map (fun x -> 
                    { Subst.genTyVar = x; Subst.substitute = TVar (annoRes.newGenVar.next()) })
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
                            match ast.term with
                            | TExp _ -> inh
                            | IExp _ -> err
                            // TODO??
                            //let isEnvVar = annoRes.allEnvVars |> Map.containsKey x.tyvar
                            //if isEnvVar then err else inh
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
                let x = f ast.term
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
