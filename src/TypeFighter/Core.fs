namespace TypeFighter

type Ident = string
type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit
type Exp =
    | Lit of Lit
    | Var of Ident
    | App of Exp * Exp
    | Abs of Ident * Exp
    | Let of Ident * Exp * Exp
    | Tuple of Exp list
type Env = Map<Ident, Exp>

type TyVar = int

type TypeNode =
    | Var of int
    | Fun of {| n1: TypeNode; n2: TypeNode |}
    | Gen of {| n: TypeNode; gens: Set<TypeNode> |}

type Edge =
    | Unify of {| a: TypeNode; b: TypeNode |}
    | Inst of {| |}
    
// TODO: maybe get rid of this and make everything immutable
and Node (data: NodeData, constr: (ConstraintState * Set<Instanciation> * Set<Subst>) option) =
    member this.data = data
    member val constr = constr with get, set

type SolveResult =
    { constrainedNodes: List<Node>
      errorNodes: List<Node>
      unfinishedNodes: List<Node>
      allNodes: List<Node>
      exprConstraintStates: Map<TExp, ConstraintState * Set<Instanciation> * Set<Subst>>
      envConstraintStates: Map<IExp, ConstraintState * Set<Instanciation> * Set<Subst>>
      annotationResult: Annotation.AnnotationResult
      success: bool }

module Instanciation =
    let empty = Set.empty<Instanciation>

module Env =
    let empty : Env = Map.empty
    let bind (ident: Ident) (exp: Exp) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some exp)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound. Env: {env}"
        | Some t -> t

module Subst =
    let empty : Set<Subst> = Set.empty

    let instanciate (insts: Set<Instanciation>) (t: Tau) =
        (t, insts) ||> Set.fold (fun t inst ->
            let rec instanciate (t: Tau) =
                t |> Tau.mapTree instanciate (fun var ->
                    let res = if var = inst.oldVar then inst.newVar else var
                    TVar res)
            instanciate t)

    let subst (substs: Set<Subst>) tau =
        let rec substRec (substs: Set<Subst>) tau =
            // TODO: quite similar with remapGenVars
            let substVarInAwithB (tvar: TyVar) (a: Tau) (b: Tau) =
                let rec substTau tau =
                    tau |> Tau.mapTree substTau (fun var -> if var = tvar then b else tau)
                substTau a
            match substs |> Set.toList with
            | [] -> tau
            | x :: xs ->
                let taus = substVarInAwithB x.tyvar tau x.substitute
                let substs = xs |> List.map (fun next ->
                    let substitute = substVarInAwithB x.tyvar next.substitute x.substitute
                    { tyvar = next.tyvar; substitute = substitute })
                substRec (set substs) taus
        substRec substs tau

    // TODO: Does this terminate?
    let rec flatten (substs: Set<Subst>) =
        let varGroups = substs |> Set.toList |> List.groupBy (fun x -> x.tyvar)
            
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
                        { Subst.tyvar = var; Subst.substitute = utau }
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
            | TVar x, y
            | y, TVar x ->
                Ok (y, set [ { tyvar = x; substitute = y } ])
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
    | MakeTuple x -> x.incs
    | ArgOut x -> [ x.inc ]
    | Unify x -> [ x.inc1; x.inc2 ]
    | Inst x -> [ x.inc ]
        
// TODO: Rückgabe als annoRes + nodes, damit man besser pipen kann
let create (annoRes: Annotation.AnnotationResult) =
    let nodes = ResizeArray()

    let addNode n constr =
        let node = Node(n, constr)
        do nodes.Add node
        node
    let source tau = addNode (Source tau) (Some (Constrained tau, Instanciation.empty, Subst.empty))
    let newGenVarSource() = source (TVar (annoRes.newGenVar.next()))
    let ast exp constr inc = addNode (Ast { exp = exp; inc = inc }) constr
    let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) None
    let makeTuple incs = addNode (MakeTuple { incs = incs }) None
    let argOut inc = addNode (ArgOut { inc = inc }) None
    let unify inc1 inc2 = addNode (Unify { inc1 = inc1; inc2 = inc2 }) None
    let inst scope inc = addNode (Inst { scope = scope; inc = inc }) None

    let rec generateGraph (exp: TExp) (inc: Node option) =
        let ( ==> ) (n1, maybeN2) f =
            match maybeN2 with
            | None -> f n1
            | Some n2 -> unify n1 n2 |> f
            
        let nthis parent =
            let constr = 
                exp.meta.initialConstr 
                |> Option.map (fun t -> Constrained t, Instanciation.empty, Subst.empty)
            ast (SynExp exp) constr parent
            
        let res =
            match exp.exp with
            | Lit x ->
                let nsource = source (TApp(Lit.getTypeName x, []))
                (nsource, inc) ==> nthis
            | Var ident ->
                let nsource =
                    match Env.resolve ident exp.meta.env with
                    | Intern exp ->
                        nodes |> Seq.find (fun n ->
                            match n.data with
                            | Ast { exp = EnvExp envExp }
                                when envExp = exp ->
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
                let nident = ast (EnvExp ident) None (newGenVarSource())
                let nbody = generateGraph body None
                let nfunc = makeFunc nident nbody
                (nfunc, inc) ==> nthis
            | Let (ident, e, body) ->
                do
                    (generateGraph e None, None) ==> ast (EnvExp ident) None
                    |> ignore
                (generateGraph body None, inc) ==> nthis
            | Tuple es ->
                let ntuple = [ for e in es do generateGraph e None ] |> makeTuple
                (ntuple, inc) ==> nthis
        res

    do generateGraph (annoRes.root) None |> ignore
    nodes |> Seq.toList
    
// test
let solve (annoRes: Annotation.AnnotationResult) (nodes: Node list) =

    // TODO: Hier ggf. was machen
    let (|Cons|Err|Init|) (node: Node) =
        match node.constr with
        | Some (Constrained tau, insts, substs) -> Cons (tau,insts,substs)
        | Some (UnificationError e, insts, substs) -> Err e
        | None -> Init

    let (|AnyError|AnyInitial|AllConstrained|) (nodes: Node list) =
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
        | Ast { inc = Cons(t,i,s) } ->
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
                let substsGenVars = s |> Set.map (fun x -> x.tyvar)
                tgenvars - substsGenVars
            let instances = ftv |> Set.map (fun x -> 
                { oldVar = x
                    newVar = annoRes.newGenVar.next() })
            // TODO: Braucht man replacedTau wirklich?
            let t = Subst.instanciate instances t
            let t = Subst.subst s t
            Constrained t, i + instances, s
        | _ ->
            // for now, let's better fail here so that we can detect valid error cases and match them
            failwith $"Invalid graph at node: {nodeData}", Instanciation.empty, Subst.empty

    let rec constrainNodes
        (unfinished: ResizeArray<Node>) 
        (constrained: ResizeArray<Node>) 
        (errors: ResizeArray<Node>)
        =
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
                    let inh = Some(UnificationError Inherit, Instanciation.empty, Subst.empty)
                    let err = Some(UnificationError err, Instanciation.empty, Subst.empty)
                    let allIncomingsAreOperations = 
                        let isOp = function | Source _ | Ast _ -> false | _ -> true
                        incomingNodes |> List.map (fun n -> isOp n.data)|> List.contains false |> not
                    match allIncomingsAreOperations,node.data with
                    | true, _ -> err
                    | false, Ast ast ->
                        match ast.exp with
                        | SynExp _ -> inh
                        | EnvExp _ -> err
                    | false, _ -> inh

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
        doIt (function | EnvExp exp -> Some exp | _ -> None)

    let exprConstraintStates =
        doIt (function | SynExp exp -> Some exp | _ -> None)

    { res with
        exprConstraintStates = exprConstraintStates
        envConstraintStates = envConstraintStates }
