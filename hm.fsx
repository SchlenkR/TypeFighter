type Lit = { typeName: string; value: string }

type Exp =
    | ELit of Lit
    | EVar of string
    | EApp of Exp * Exp
    | EFun of string * Exp
    | ELet of string * Exp * Exp

// make SCDU fot them
type TypeVar = int

type Ident = string

type Mono =
    | MVar of TypeVar
    | MBase of string
    | MFun of Mono * Mono
    
and Poly =
    { t: Mono
      tvars: TypeVar list }

type Env = Map<Ident, Poly>

type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of TAnno * TAnno
    | TEFun of IdentAnno * TAnno // TODO: Wieso hier IdentAnno und nicht nur Ident?
    | TELet of Ident * TAnno * TAnno
and IdentAnno =
    { ident: Ident
      tvar: TypeVar }
and TAnno =
    { texp: TExp
      tvar: TypeVar }

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

type VarGen() =
    let mutable varCounter = -1
    member this.fresh() =
        varCounter <- varCounter + 1
        varCounter

module Infer =
        
    let generalize (t: Mono) (env: Env) =
        let freeVars = Ftv.get t - Ftv.get env
        // printfn $"GEN: {vars}"
        Poly.poly t (Set.toList freeVars)

    let inst (vargen: VarGen) (p: Poly) : Mono =
        // printfn $"Inst {p}"
        let rec replace (t: Mono) (freeVar: TypeVar) =
            match t with
            | MVar tvar when tvar = freeVar -> MVar (vargen.fresh())
            | MFun (t1, t2) -> MFun (replace t1 freeVar, replace t2 freeVar)
            | _ -> t
        p.tvars |> List.fold replace p.t

    let annoExp (vargen: VarGen) (exp: Exp) =
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
                    let annotatedIdent = { ident = ident;  tvar = vargen.fresh() }
                    TEFun (annotatedIdent, annoExp body)
                | ELet (ident, e, body) ->
                    TELet (ident, annoExp e, annoExp body)
            { texp = texp
              tvar = vargen.fresh () }
        annoExp exp

    let constrain (vargen: VarGen) (env: Env) (tanno: TAnno) =
        let rec constrain (env: Env) (tanno: TAnno) = [
            match tanno.texp with
            | TELit x ->
                yield Subst.create($"Lit {x.typeName}", tanno.tvar, MBase x.typeName)
            | TEVar ident ->
                let varType = Env.resolve ident env |> inst vargen
                yield Subst.create($"Var {ident}", tanno.tvar, varType)
            | TEApp (e1, e2) ->
                yield Subst.create("App", e1.tvar, MFun(MVar e2.tvar, MVar tanno.tvar))
                yield! constrain env e2
                yield! constrain env e1
            | TEFun (ident, body) ->
                let newEnv = Env.bind ident.ident (Poly.mono (MVar ident.tvar)) env
                yield Subst.create("Fun", tanno.tvar, MFun(MVar ident.tvar, MVar body.tvar))
                yield! constrain newEnv body
            | TELet (ident, e, body) ->
                let newEnv = Env.bind ident (generalize (MVar e.tvar) env) env
                yield Subst.create($"Let {ident}", tanno.tvar, MVar body.tvar)
                yield! constrain env e
                yield! constrain newEnv body
            ]
        constrain env tanno

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
        let vargen = VarGen()

        let annotatedAst = annoExp vargen exp
        let constraintSet = constrain vargen env annotatedAst
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
