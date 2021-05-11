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
    | App of MetaExp<'meta> * MetaExp<'meta>
    | Abs of MetaIdent<'meta> * MetaExp<'meta>
    | Let of Ident * MetaExp<'meta> * MetaExp<'meta>
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
type TExp = Meta<Exp<Anno>, Anno>
type IExp = Meta<Exp<Anno>, Anno>

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
    let bind ident (tyvar: TyVar) (env: Env) : Env =
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
        |> Map.ofList

type Counter(exclSeed) =
    let mutable varCounter = exclSeed
    member this.next() = varCounter <- varCounter + 1; varCounter

module AnnotatedAst =

    type AnnotationResult =
        { newGenVar: Counter
          root : TExp
          allExpressions: List<TExp>
          allEnvVars: Map<TyVar, Ident * TExp> }

    let private remapGenVars (env: Env) : Counter * Env =
        let newGenVar = Counter(0)
        let mutable varMap = Map.empty<GenTyVar, GenTyVar>
        newGenVar, env |> Map.map (fun _ v ->
            match v with
            | Intern _ -> v
            | Extern tau ->
                let rec remap tau =
                    match tau with
                    | TGenVar var ->
                        varMap
                        |> Map.tryFind var
                        |> Option.defaultWith (fun () ->
                            let newVar = newGenVar.next()
                            varMap <- varMap |> Map.add var newVar
                            newVar)
                        |> TGenVar
                    | TApp (ident, taus) -> TApp (ident, [ for t in taus do remap t ])
                    | TFun (t1, t2) -> TFun (remap t1, remap t2)
                    | TTuple taus -> TTuple [ for t in taus do remap t ]
                    | TRecord fields -> TRecord (Set.ofList [ for f,t in fields do f, remap t ])
                Extern (remap tau))

    let create (env: Env) (exp: UExp) =
        let newGenVar,env = remapGenVars env
        let newTyVar = Counter(0)
        let mutable allExp = []
        let mutable allEnvVars = Map.empty
        let rec annotate (env: Env) (exp: UExp) =
            let texp =
                { meta =
                    { tyvar = newTyVar.next()
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
                        let tyvarIdent = newTyVar.next()
                        let newEnv = env |> Env.bind ident.exp tyvarIdent
                        let annotatedIdent =
                            { meta =
                                { tyvar = tyvarIdent
                                  env = env
                                  initialConstr = None }
                              exp = ident.exp }
                        Abs (annotatedIdent, annotate newEnv body)
                    | Let (ident, e, body) ->
                        let newEnv = env |> Env.bind ident (newTyVar.next())
                        Let (ident, annotate env e, annotate newEnv body)
                    | Prop (ident, e) -> 
                        Prop (ident, annotate env e)
                    | Tuple es -> 
                        Tuple (es |> List.map (annotate env))
                    | Record fields -> 
                        Record [ for ident,e in fields do ident, annotate env e ]
                }
            do allExp <- texp :: allExp
            do
                let newStuff = 
                    Env.getInterns env
                    |> Map.toSeq
                    |> Seq.filter (fun (ident,tyvar) -> not (allEnvVars |> Map.containsKey tyvar))
                for ident,tyvar in newStuff do
                    allEnvVars <- allEnvVars |> Map.add tyvar (ident, texp)
            texp
        let res = annotate env exp
        { newGenVar = newGenVar
          root = res
          allExpressions = allExp |> Seq.toList
          allEnvVars = allEnvVars }

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

    type Subst = { genTyVar: GenTyVar; substitute: Tau }

    type Ast = { tyvar: TyVar; inc1: Node option; inc2: Node option }
    type MakeFun = { inc1: Node; inc2: Node }
    type ArgOp = In | Out
    type Arg = { argOp: ArgOp; inc: Node }
    type UnifySubst = { substSource: Node; substIn: Node; applyTo: Node }
    type GetProp = { field: string; inc: Node }
    type MakeTuple = { incs: Node list }
    type MakeRecord = { fields: string list; incs: Node list }

    type NodeData =
        | Source of Tau
        | Ast of Ast
        | MakeFun of MakeFun
        | GetProp of GetProp
        | MakeTuple of MakeTuple
        | MakeRecord of MakeRecord
        | Arg of Arg
        | UnifySubst of UnifySubst
    
    // TODO: maybe get rid of this and make everything immutable
    type Node (data: NodeData, constr: ConstraintState option) =
        member this.data = data
        member val constr = constr with get, set

    type SolveResult =
        { constrainedNodes: List<Node>
          errorNodes: List<Node>
          unfinishedNodes: List<Node>
          allNodes: List<Node>
          substs: Set<Subst>
          varsAndConstraints: Map<TyVar, ConstraintState>
          constrainedVars: Map<TyVar, Tau>
          annotationResult: AnnotatedAst.AnnotationResult
          success: bool }

    module SolveResult =
        let findTau (exp: TExp) sr = sr.constrainedVars |> Map.find exp.meta.tyvar

    module Subst =
        let empty : Subst list = []

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
        let source tau = addNode (Source tau) (Some (Constrained tau))
        let ast tyvar constr inc1 inc2 = addNode (Ast { tyvar = tyvar; inc1 = inc1; inc2 = inc2 }) constr
        let makeFunc inc1 inc2 = addNode (MakeFun { inc1 = inc1; inc2 = inc2 }) None
        let getProp field inc = addNode (GetProp { field = field; inc = inc }) None
        let makeTuple incs = addNode (MakeTuple { incs = incs }) None
        let makeRecord fields incs = addNode (MakeRecord { fields = fields; incs = incs }) None
        let arg op inc = addNode (Arg { argOp = op; inc = inc }) None
        let argIn = arg In
        let argOut = arg Out
        let applySubst substSource substIn apply =
            addNode (UnifySubst { substSource = substSource; substIn = substIn; applyTo = apply }) None

        let rec generateGraph (exp: TExp) (inc: Node option) =
            let ( => ) x f = Some x |> f
            let ( ==> ) (x,y) f = (Some x,y) ||> f
            
            let nthis = ast exp.meta.tyvar (exp.meta.initialConstr |> Option.map Constrained)
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
                //     (ALT: subst: getSubst (argIn e1) e2 |> applySubst (argOut e1) ==> app)
                //     -- oder (NEU) --
                //     subst: applySubst e2 (argIn e1) (argOut e1) ==> app
                // infer: argIn(e1) ==> e2
                let ne1 = None |> generateGraph e1
                // Info: 
                // gegeben ist (\id. id 4, id "xxx")(\x.x)
                // Da wir von (argIn ne1) nach ne2 gehen, wird also das Argument (rechts im Tree)
                // von der Funktion bestimmt. Das könnte man prinzipiell auch anders machen; das
                // geht aber mit dem aktuellen Vokabular nicht.
                let ne2 = (argIn ne1) => generateGraph e2 
                (applySubst ne2 (argIn ne1) (argOut ne1), inc) ==> nthis
                //(argOut ne1, inc) ==> var exp.tyvar
            | Abs (ident, body) ->
                let nfunc = makeFunc (ast ident.meta.tyvar None None None) (generateGraph body None)
                (nfunc, inc) ==> nthis
            | Let (ident, e, body) ->
                do
                    // TODO: why do we have this exception? Can we express that let bound idents are always intern?
                    match Env.resolve ident body.meta.env with
                    | Intern tyvarIdent ->
                        (generateGraph e None, None) ==> ast tyvarIdent None
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
        nodes |> Seq.toList
    
    let rec unify2Types (a: Tau) (b: Tau) =
        let error (msg: string) = Error $"""Cannot unify types "{Format.tau a}" and "{Format.tau b}": {msg}"""
        let rec unify1 t1 t2 =
            match t1,t2 with
            | x,y when x = y ->
                Ok (x, Subst.empty)
            | TGenVar x, y
            | y, TGenVar x ->
                Ok (y, [ { genTyVar = x; substitute = y } ])
            | TApp (_, taus1), TApp (_, taus2) when taus1.Length <> taus2.Length ->
                error "Generic argument count mismatch"
            | TApp (n1,_), TApp (n2, _) when n1 <> n2 ->
                error "Type mismatch"
            | TApp (name, taus1), TApp (_, taus2) ->
                let res = unifyManyTypes taus1 taus2
                match res with
                | Ok (taus, substs) ->
                    Ok (TApp (name, taus), substs)
                | Error e -> 
                    Error e
            | TFun (ta1, ta2), TFun (tb1, tb2) ->
                match unifyManyTypes [ta1; ta2] [tb1; tb2] with
                | Ok ([tres1; tres2], substs) ->
                    Ok (TFun (tres1, tres2), substs)
                | Error e ->
                    Error e
                | _ ->
                    failwith $"inconsistent TFun unification: {t1} <> {t2}"
            | _ ->
                error "Unspecified cases"
        // TODO: muss das sein? "and" -> kann das nicht wieder nach innen?
        and unifyManyTypes taus1 taus2 =
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
    
    let rec substMany (substs: Subst list) taus =
        // TODO: quite similar with remapGenVars
        // TODO: Dieses "for" pattern haben wir ständig mit den Taus
        let substVarInAwithB (tvar: GenTyVar) (a: Tau) (b: Tau) =
            let rec substTau tau =
                match tau with
                | TGenVar i when i = tvar -> b
                | TGenVar _ -> tau
                | TFun (t1, t2) -> TFun (substTau t1, substTau t2)
                | TApp (name, taus) -> TApp(name, [ for tau in taus do substTau tau ])
                | TTuple taus -> TTuple [ for tau in taus do substTau tau ]
                | TRecord fields -> TRecord (Set.ofList [ for name,tau in fields do name,substTau tau ])
            substTau a
        match substs with
        | [] -> taus
        | x :: xs ->
            let taus = [ for tau in taus do substVarInAwithB x.genTyVar tau x.substitute ]
            let substs = xs |> List.map (fun next ->
                let substitute = substVarInAwithB x.genTyVar next.substitute x.substitute
                { genTyVar = next.genTyVar; substitute = substitute })
            substMany substs taus

    let solve (annoRes: AnnotatedAst.AnnotationResult) (nodes: Node list) =
        let (|Tau|Err|Init|) (node: Node) =
            match node.constr with
            | Some (Constrained tau) -> Tau tau
            | Some (UnificationError e) -> Err e
            | None -> Init

        let (|AnyError|AnyInitial|AllConstrained|) (nodes: Node list) =
            let cs = nodes |> List.choose (function | Tau x -> Some x | _ -> None)
            let es = nodes |> List.choose (function | Err x -> Some x | _ -> None)
            let ns = nodes |> List.choose (function | Init x -> Some x | _ -> None)
            match cs,es,ns with
            | _,es::_,_ -> AnyError es
            | _,_,ns::_ -> AnyInitial ns
            | cs,[],[] -> AllConstrained cs

        let constrainNodeData nodeData =
            match nodeData with
            | Source tau ->
                Constrained tau, Subst.empty
            // The next 2 are edge cases. we could also model inc1 and inc2 as not optional
            // and normalize them with a forall source node. But this would
            // lead to a blown up graph with much more nodes and gen vars.
            | Ast { tyvar = _; inc1 = None; inc2 = None } ->
                Constrained(TGenVar(annoRes.newGenVar.next())), Subst.empty
            | Ast { tyvar = _; inc1 = Some(Tau tau); inc2 = None }
            | Ast { tyvar = _; inc1 = None; inc2 = Some(Tau tau) } ->
                Constrained(tau), Subst.empty
            | Ast { tyvar = _; inc1 = Some(Tau t1); inc2 = Some(Tau t2) } ->
                match unify2Types t1 t2 with
                | Error msg -> UnificationError(Origin msg), Subst.empty
                | Ok (unifiedTau,substs) -> Constrained unifiedTau, substs
            | MakeFun { inc1 = Tau t1; inc2 = Tau t2 } ->
                Constrained(TFun (t1, t2)), Subst.empty
            | GetProp { field = field; inc = Tau(TRecord recordFields) } ->
                let res =
                    recordFields 
                    |> Set.toArray
                    |> Array.tryFind (fun (n,_) -> n = field)
                    |> Option.map snd
                    |> Option.map Constrained
                    |> Option.defaultValue (UnificationError(Origin $"Field not found: {field}"))
                res, Subst.empty
            | MakeTuple { incs = AllConstrained incs } ->
                Constrained(TTuple incs), Subst.empty
            | MakeRecord { fields = fields; incs = AllConstrained incs } ->
                let fields = List.zip fields incs
                Constrained(TRecord (Set.ofList fields)), Subst.empty
            | Arg { argOp = In; inc = Tau(TFun(t1,_)) } ->
                Constrained t1, Subst.empty
            | Arg { argOp = Out; inc = Tau(TFun(_,t2)) } ->
                Constrained t2, Subst.empty
            //| Arg { argOp = argOp; inc = Tau (TGenVar genvar) } ->
            //    let t1 = TGenVar (annoRes.newGenVar.next())
            //    let t2 = TGenVar (annoRes.newGenVar.next())
            //    let tfun = TFun (t1, t2)
            //    let c =
            //        match argOp with
            //        | In -> t1
            //        | Out -> t2
            //    let subst = { genTyVar = genvar; substitute = tfun }
            //    Constrained c, [subst]
            | Arg { argOp = argOp; inc = Tau x } ->
                UnificationError(Origin $"Function type expected ({argOp}), but was: {Format.tau x}"), Subst.empty
            | UnifySubst { substSource = Tau substSource; substIn = Tau substIn; applyTo = Tau applyTo } ->
                // TODO: it matters if we use "b a " or "a b", but we have to know that :(
                match unify2Types substIn substSource with
                | Error msg -> 
                    UnificationError(Origin msg), Subst.empty
                | Ok (_,substs) ->
                    match substs with
                    | [] as x ->
                        Constrained applyTo, x
                    | substs ->
                        // TODO: shouldn't we take the substituted substs from substMany instead of the "untouched" ones?
                        // OR: Do we need them at all?
                        let taus = substMany substs [applyTo]
                        Constrained (taus |> List.exactlyOne), substs
            | _ ->
                // for now, let's better fail here so that we can detect valid error cases and match them
                failwith $"Invalid graph at node: {nodeData}", Subst.empty

        let rec constrainNodes 
                (unfinished: ResizeArray<Node>) 
                (constrained: ResizeArray<Node>) 
                (errors: ResizeArray<Node>)
                (allSubsts: ResizeArray<Subst>) =
            let mutable goOn = false
            for node in unfinished |> Seq.toArray do
                let c,newSubsts = 
                    let incomingNodes = getIncomingNodes node
                    match incomingNodes with
                    | AnyInitial _ -> None, Subst.empty
                    | AnyError es ->
                        let allIncomingsAreOperations = 
                            let isOp = function | Source _ | Ast _ -> false | _ -> true
                            incomingNodes |> List.map (fun n -> isOp n.data)|> List.contains false |> not
                        match allIncomingsAreOperations with
                        | true -> Some(UnificationError Inherit), Subst.empty
                        | false -> Some(UnificationError es), Subst.empty
                    | _ ->
                        let c,substs = constrainNodeData node.data
                        Some c, substs
                allSubsts.AddRange(newSubsts)
                node.constr <- c
                match c with
                | Some (Constrained _) ->
                    goOn <- true
                    unfinished.Remove(node) |> ignore
                    constrained.Add(node)
                | Some (UnificationError _) ->
                    goOn <- true
                    unfinished.Remove(node) |> ignore
                    errors.Add(node)
                | _ -> ()
            if not goOn then
                { constrainedNodes = constrained |> List.ofSeq
                  errorNodes = errors |> List.ofSeq
                  unfinishedNodes = unfinished |> List.ofSeq
                  allNodes = [
                    yield! constrained
                    yield! unfinished
                    yield! errors ]
                  substs = allSubsts |> Set.ofSeq
                  varsAndConstraints = Map.empty
                  constrainedVars = Map.empty
                  annotationResult = annoRes
                  success = errors.Count = 0 && unfinished.Count = 0 }
            else
                constrainNodes unfinished constrained errors allSubsts
        
        let res = constrainNodes (ResizeArray nodes) (ResizeArray()) (ResizeArray()) (ResizeArray())
        
        // final generic param substitution
        do
            let finalSubsts =
                res.substs |> Set.toList |> List.choose (
                    function
                    | { genTyVar = _; substitute = TGenVar _ } as x -> Some x
                    | _ -> None)
            for n in res.constrainedNodes do
                match n.constr with
                | Some (Constrained c) -> 
                    let csubst = substMany finalSubsts [ c ]
                    n.constr <- Some (Constrained (csubst |> List.exactlyOne))
                | _ -> ()

        let varsAndCs =
            [ for n in res.allNodes do
                match n.data, n.constr with
                | Ast ast, Some c -> Some (ast.tyvar, c)
                | _ -> ()
            ]
            |> List.choose id
        let constrainedVars =
            [ for tyvar,constr in varsAndCs do
                match constr with
                | Constrained tau -> Some (tyvar,tau)
                | _ -> None
            ]
            |> List.choose id
            |> Map.ofList

        { res with
            varsAndConstraints = Map.ofList varsAndCs
            constrainedVars = constrainedVars }
