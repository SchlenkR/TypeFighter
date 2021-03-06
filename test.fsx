#if INTERACTIVE
fsi.PrintWidth <- 250
#endif

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
      targs: TypeVar list }

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
    let poly t targs = { t = t; targs = targs }
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
        Ftv.get p.t - Set.ofList p.targs
    static member get (env: Env) : Set<TypeVar> =
        let typesBoundInEnv = env |> Map.toList |> List.map snd
        let typeVarsBoundInEnv =
            typesBoundInEnv
            |> List.map Ftv.get
            |> List.fold Set.union Set.empty
        typeVarsBoundInEnv

module Subst =
    let create(desc, tvar, r) = { desc = desc; tvar = tvar; right = r; }

type Infer() =
        
    let mutable varCounter = -1
    let newVar() =
        varCounter <- varCounter + 1
        varCounter

    let generalize (t: Mono) (env: Env) =
        Poly.poly t ((Ftv.get t - Ftv.get env) |> Set.toList)

    let inst (p: Poly) : Mono =
        let rec replace (t: Mono) (tvar: TypeVar) =
            match t with
            | MBase _ -> t
            | MVar _ -> t
            | MFun (t1, t2) -> MFun (replace t1 tvar, replace t2 tvar)
        p.targs |> List.fold replace p.t

    let rec annoExp (exp: Exp) =
        let annoIdent ident =
            { ident = ident
              tvar = newVar () }
        let texp =
            match exp with
            | ELit x ->
                TELit x
            | EVar ident ->
                TEVar ident
            | EApp (e1, e2) ->
                TEApp (annoExp e1, annoExp e2)
            | EFun (ident, body) ->
                TEFun (annoIdent ident, annoExp body)
            | ELet (ident, e, body) ->
                TELet (ident, annoExp e, annoExp body)
        { texp = texp
          tvar = newVar () }

    let rec constrain (env: Env) (tanno: TAnno) = [
        match tanno.texp with
        | TELit x ->
            yield Subst.create($"Lit {x.typeName}", tanno.tvar, MBase x.typeName)
        | TEVar ident ->
            let varType = Env.resolve ident env |> inst
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
            | MFun (m, n) -> MFun (subst m tvar dest, subst n tvar dest)
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
        let annotatedAst = annoExp exp
        let constraintSet = constrain env annotatedAst
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
        findVar annotatedAst.tvar

    member this.AnnoExp = annoExp
    member this.Constrain = constrain
    member this.Solve = solve
    member this.Infer = infer

module Debug =

    let printEquations (eqs: Subst list) =
        eqs
        |> List.sortBy (fun x -> x.tvar)
        |> List.iter (fun x -> printfn "%-20s    %A = %A" x.desc x.tvar x.right)



///////// Test

module Dsl =
    
    let knownBaseTypes =
        {| int = "Int"
           float = "Float"
           string = "String" |}
           
    let tint = MBase knownBaseTypes.int
    let tfloat = MBase knownBaseTypes.float
    let tstring = MBase knownBaseTypes.string
    let tfun(a, b) = MFun(a, b)

    let xint (x: int) = ELit { typeName = knownBaseTypes.int; value = string x }
    let xfloat (x: float) = ELit { typeName = knownBaseTypes.float; value = string x }
    let xstr (x: string) = ELit { typeName = knownBaseTypes.string; value = x }

open Dsl

let env : Env = Map.ofList [
    "libcall_add", tfun(tint, tfun(tint, tint)) |> Poly.mono
    ]
    
// let ftvOfE (e: Exp) =
//     let te = Infer.annotate Env.empty e
//     te.t, Ftv.get te.t
let printConstraints =
    let inf = Infer()
    inf.AnnoExp >> inf.Constrain env >> Debug.printEquations
let printSolution =
    let inf = Infer()
    inf.AnnoExp >> inf.Constrain env >> inf.Solve >> Debug.printEquations
let infer = Infer().Infer env







Ftv.get (tfun(tint, tfun(tint, MVar 2)))
// Infer.annotate env idExp
// ftvOfE idExp



let idExp = EFun("x", EVar "x")

infer <| xint 43
infer <| idExp
infer <| ELet("hurz", xint 43, xstr "sss")
infer <| ELet("id", idExp, EApp(EVar "id", xstr "sss"))
infer <| EFun("x", xstr "klököl")
infer <| EApp(EFun("x", EVar "x"), xint 2)
infer <| EApp(EFun("x", EVar "x"), xstr "Hello")

// unbound var "y":
infer <| EFun("x", EFun("y", EVar "x"))

infer <| ELet("k", EFun("x", ELet("f", EFun("y", EVar "x"), EVar "f")), EVar "k")
infer <| ELet("k", xint 43, ELet("k", xstr "sss", EVar "k"))



let expr1 = xint 42
let expr2 = ELet("hurz", xint 43, xint 32)
let expr3 =
    let addA = EFun("a", EApp(EVar "libcall_add", EVar "a"))
    let addB = EFun("b", EApp(addA, EVar "b"))
    ELet("hurz", xint 43, ELet("f", addB, EApp(EApp(EVar "f", EVar "hurz"), xint 99)))

printConstraints expr3
printSolution idExp
printSolution expr3

infer expr3
infer expr1
infer expr2


// Errors
(*
solve <| EApp (EVar "libcall_add") (xstr "lklö")
*)


(*
// Der Typ von "f" ist _kein_ Polytyp
(fun f -> f "as", f 99) id

// Der Typ von "f" ist ein Polytyp
let f = id in f "as", f 99
*)

