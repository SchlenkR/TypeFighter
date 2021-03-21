
type Lit = { typeName: string; value: string }

type Exp =
    | ELit of Lit
    | EVar of string
    | EApp of Exp * Exp
    | EFun of string * Exp
    | ELet of string * Exp * Exp

type TypeVar = int

type Ident = string

type Annotated<'a> =
    { annotated: 'a
      tvar: TypeVar }
type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of Annotated<TExp> * Annotated<TExp>
    | TEFun of Annotated<Ident> * Annotated<TExp> // TODO: Wieso hier IdentAnno und nicht nur Ident?
    | TELet of Ident * Annotated<TExp> * Annotated<TExp>

type Mono =
    | MVar of TypeVar
    | MBase of string
    | MFun of Mono * Mono
    
and Poly =
    { t: Mono
      tvars: TypeVar list }

type Env = Map<Ident, Poly>

type Subst =
    { desc: string
      tvar: TypeVar
      right: Mono }

type Unifyable =
    { left: Mono
      right: Mono }

module Poly =
    let poly t targs = { t = t; tvars = targs }
    let mono t = poly t []

module Env =
    let empty : Env = Map.empty
    let bind ident (p: Poly) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some p)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound."
        | Some t -> t

type Ftv =
    static member get (t: Mono) : Set<TypeVar> =
        match t with
        | MBase _ -> Set.empty
        | MVar varName -> Set.singleton varName
        | MFun (t1, t2) -> Ftv.get t1 + Ftv.get t2
    static member get (p: Poly) : Set<TypeVar> =
        Ftv.get p.t - Set.ofList p.tvars
    static member get (env: Env) : Set<TypeVar> =
        let typesBoundInEnv = env |> Map.toList |> List.map snd
        let typeVarsBoundInEnv =
            typesBoundInEnv
            |> List.map Ftv.get
            |> List.fold Set.union Set.empty
        typeVarsBoundInEnv

module Subst =
    let create(desc, tvar, r) = { desc = desc; tvar = tvar; right = r; }

type Newvar() =
    let mutable varCounter = -1
    member this.fresh() =
        varCounter <- varCounter + 1
        varCounter

module Infer =

    let annoExp (newvar: Newvar) (exp: Exp) =
        let rec annoExp (exp: Exp) =
            let texp =
                match exp with
                | ELit x ->
                    TELit x
                | EVar ident ->
                    TEVar ident
                | EApp (e1, e2) ->
                    TEApp (annoExp e1, annoExp e2)
                | EFun (ident, body) ->
                    let annotatedIdent = { annotated = ident; tvar = newvar.fresh() }
                    TEFun (annotatedIdent, annoExp body)
                | ELet (ident, e, body) ->
                    TELet (ident, annoExp e, annoExp body)
            { annotated = texp
              tvar = newvar.fresh () }
        annoExp exp

    let constrain (newvar: Newvar) (env: Env) (annoExp: Annotated<TExp>) : Subst list =
        let inst (p: Poly) : Mono =
            let rec replace (t: Mono) (freeVar: TypeVar) =
                match t with
                | MVar tvar when tvar = freeVar ->
                    let instanciatedVar = newvar.fresh()
                    printfn $"Inst {tvar} -> {instanciatedVar}"
                    MVar freeVar
                | MFun (t1, t2) ->
                    MFun (replace t1 freeVar, replace t2 freeVar)
                | _ -> t
            p.tvars |> List.fold replace p.t

        let gen (t: Mono) (env: Env) =
            let freeVars = Ftv.get t - Ftv.get env
            printfn $"GEN: {freeVars}"
            Poly.poly t (Set.toList freeVars)

        let rec constrain (env: Env) (annoExp: Annotated<TExp>) = [
            match annoExp.annotated with
            | TELit x ->
                yield Subst.create($"Lit {x.typeName}", annoExp.tvar, MBase x.typeName)
            | TEVar ident ->
                let varType = Env.resolve ident env |> inst
                yield Subst.create($"Var-Expr {ident}", annoExp.tvar, varType)
            | TEApp (e1, e2) ->
                yield Subst.create("App (e1 = e2)", e1.tvar, MFun(MVar e2.tvar, MVar annoExp.tvar))
                yield! constrain env e2
                yield! constrain env e1
            | TEFun (ident, body) ->
                let newEnv = Env.bind ident.annotated (Poly.mono (MVar ident.tvar)) env
                yield Subst.create("Fun-Expr", annoExp.tvar, MFun(MVar ident.tvar, MVar body.tvar))
                yield! constrain newEnv body
            | TELet (ident, e, body) ->
                let newEnv = Env.bind ident (gen (MVar e.tvar) env) env
                yield Subst.create($"Let-Expr {ident}", annoExp.tvar, MVar body.tvar)
                yield! constrain env e
                yield! constrain newEnv body
            ]
        constrain env annoExp

    let solve (eqs: Subst list) =
        let rec unify (t1: Mono) (t2: Mono) : Unifyable list =
            [
                match t1, t2 with
                | MFun (l,r), MFun (l',r') ->
                    yield! unify l l'
                    yield! unify r r'
                | MVar _, _ ->
                    yield { left = t1; right = t2 }
                | _, MVar _ ->
                    yield { left = t2; right = t1 }
                | a,b when a = b ->
                    ()
                | _ ->
                    failwith $"type error: expedted: {t2}, given: {t1}"
            ]

        let rec subst (t: Mono) (tvar: TypeVar) (dest: Mono) =
            match t with
            | MVar i when i = tvar -> dest
            | MFun (t1, t2) -> MFun (subst t1 tvar dest, subst t2 tvar dest)
            | _ -> t

        let substMany (eqs: Unifyable list) (tvar: TypeVar) (dest: Mono) : Unifyable list =
            eqs |> List.map (fun eq ->
                { left = subst eq.left tvar dest
                  right = subst eq.right tvar dest } )

        let rec solve (eqs: Unifyable list) (solution: Unifyable list) : Unifyable list =            
            match eqs with
            | [] -> solution
            | eq :: eqs ->
                match eq.left, eq.right with
                | MVar tvar, t
                | t, MVar tvar ->
                    // substitute
                    let newEqs = substMany eqs tvar t
                    let newSolution = substMany solution tvar t
                    solve newEqs (eq :: newSolution)
                | t1, t2 ->
                    // gen new constraints and solve
                    let newConstraints = unify t1 t2
                    let newEqs = eqs @ newConstraints
                    let newSolution = solution
                    solve newEqs newSolution

        let unifiedEqs =
            eqs
            |> List.map (fun eq -> { left = MVar eq.tvar; right = eq.right })
            |> List.sortByDescending (fun e -> e.left)
        
        solve unifiedEqs []
        |> List.map (fun s ->
            match s.left, s.right with
            | MVar tvar, t
            | t, MVar tvar ->
                { desc = "solution"; tvar = tvar; right = t }
            | _ ->
                failwith $"TODO: Unsolvable: {s.left} != {s.right}"
        )
    
    let infer env exp =
        let newvar = Newvar()

        let annotatedAst = annoExp newvar exp
        let constraintSet = constrain newvar env annotatedAst
        let solutionMap = solve constraintSet

        let findVar var =
            // TODO: err can happen
            let res =
                solutionMap
                |> List.choose (fun x ->
                    match x.tvar = var with
                    | true -> Some x.right
                    | false -> None)
                |> List.tryExactlyOne
            match res with
            | Some x -> x
            | None -> failwith $"Var not found: {var}"

        {| annotatedAst = annotatedAst
           constraintSet = constraintSet
           solutionMap = solutionMap
           finalType = findVar annotatedAst.tvar |}
