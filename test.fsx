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

type TypeVar = int

type Mono =
    | MBase of string
    | MFun of Mono * Mono
    | MVar of TypeVar
    | TypeError of string

//type Poly =
//    { variables: string list
//      mono: Mono }

type Env = Map<string, Mono>

type TExp =
    | TELit of Lit
    | TEVar of string
    | TEApp of {| target: TExpAnno; arg: TExpAnno |}
    | TEFun of {| ident: string; body: TExpAnno |}
    | TELet of {| ident: string; assignment: TExpAnno; body: TExpAnno |}

and TExpAnno = { texp: TExp; tvar: TypeVar; typ: Mono; env: Env }

type Subst = { desc: string; tvar: TypeVar; right: Mono }
type Unifyable = { left: Mono; right: Mono }

module Subst =
    let create(desc, tvar: TypeVar, r: Mono) = { desc = desc; tvar = tvar; right = r; }

module Infer =

    let annotate (env: Env) (exp: Exp) =
        
        let mutable varCounter = -1
        let newVar() =
            varCounter <- varCounter + 1
            varCounter

        let rec annotate env exp =
            let addToEnv ident x env = env |> Map.change ident (fun _ -> Some x)
            let texp =
                match exp with
                | ELit x -> TELit x
                | EVar ident -> TEVar ident
                | EApp (target, arg) ->
                    TEApp {| target = annotate env target; arg = annotate env arg |}
                | EFun (ident, body) ->
                    let tvar = MVar (newVar())
                    let newEnv = env |> addToEnv ident tvar
                    TEFun {| ident = ident; body = annotate newEnv body |}
                | ELet (ident, assignment, body) ->
                    let tyanno = annotate env assignment
                    let newEnv = env |> addToEnv ident tyanno.typ
                    TELet {| ident = ident; assignment = tyanno; body = annotate newEnv body |}
            let tvar = newVar()
            let annotation = MVar tvar
            { texp = texp; tvar = tvar; typ = annotation; env = env }
        annotate env exp

    let constrain (typExpAnno: TExpAnno) =
        let resolveVar env tvar =
            match env |> Map.tryFind tvar with
                | None -> TypeError $"Identifier {tvar} is undefined."
                | Some ta -> ta
        
        let rec genConstraints typExpAnno =
            [
                match typExpAnno.texp with
                | TELit x -> yield Subst.create($"Lit {x.typeName}", typExpAnno.tvar, MBase x.typeName)
                | TEVar tvar ->
                    let varType = resolveVar typExpAnno.env tvar
                    yield Subst.create($"Var {tvar}", typExpAnno.tvar, varType)
                | TEApp tapp ->
                    yield Subst.create("App", tapp.target.tvar, MFun(tapp.arg.typ, typExpAnno.typ))
                    yield! genConstraints tapp.arg
                    yield! genConstraints tapp.target
                | TEFun tfun ->
                    let varType = resolveVar tfun.body.env tfun.ident
                    yield Subst.create("Fun", typExpAnno.tvar, MFun(varType, tfun.body.typ))
                    yield! genConstraints tfun.body
                | TELet tlet ->
                    yield Subst.create($"Let {tlet.ident}", typExpAnno.tvar, tlet.body.typ)
                    yield! genConstraints tlet.assignment
                    yield! genConstraints tlet.body
            ]

        genConstraints typExpAnno

    let solve (eqs: Subst list) =

        let rec unify (m1: Mono) (m2: Mono) : Unifyable list =
            [
                match m1,m2 with
                | MFun (l,r), MFun (l',r') ->
                    yield! unify l l'
                    yield! unify r r'
                | MVar _, _ ->
                    yield { left = m1; right = m2 }
                | _, MVar _ ->
                    yield { left = m2; right = m1 }
                | a,b when a = b -> ()
                | _ -> failwith $"type error: expedted: {m2}, given: {m1}"
            ]

        let rec subst (t: Mono) (tvar: TypeVar) (dest: Mono) =
            match t with
            | MVar i when i = tvar -> dest
            | MFun (m, n) -> MFun (subst m tvar dest, subst n tvar dest)
            | _ -> t

        let substMany (eqs: Unifyable list) (tvar: TypeVar) (dest: Mono) : Unifyable list =
            [
                for eq in eqs do
                    { left = subst eq.left tvar dest
                      right = subst eq.right tvar dest }
            ]

        let rec solve (eqs: Unifyable list) (solution: Unifyable list) : Unifyable list =            
            match eqs with
            | [] -> solution
            | eq :: eqs ->
                match eq.left, eq.right with
                | TypeError e, _
                | _, TypeError e -> failwith $"TODO: TypeError {e}"
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
            | TypeError e, _
            | _, TypeError e ->
                failwith $"TODO: TypeError {e}"
            | MVar tvar, t
            | t, MVar tvar ->
                { desc = "solution"; tvar = tvar; right = t }
            | _ ->
                failwith $"TODO: Unsolvable: {s.left} != {s.right}"
        )
    
    
    let infer env exp =
        let annotatedAst = annotate env exp
        let constraintSet = constrain annotatedAst
        let solutionMap = solve constraintSet
        let typedAst =
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
                
            let rec applySolution (texp: TExpAnno) =
                let finalExp =
                    match texp.texp with
                    | TELit _
                    | TEVar _ -> texp.texp
                    | TEApp tapp -> TEApp {| tapp with target = applySolution tapp.target; arg = applySolution tapp.arg |}
                    | TEFun tfun -> TEFun {| tfun with body = applySolution tfun.body |}
                    | TELet tlet -> TELet {| tlet with assignment = applySolution tlet.assignment; body = applySolution tlet.body |}
                { texp with texp = finalExp; typ = findVar texp.tvar }
            applySolution annotatedAst
        typedAst


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
    let xvar ident = EVar (ident)
    let xlet ident e1 e2 = ELet (ident, e1, e2)
    let xfun ident e = EFun (ident, e)
    let xapp e1 e2 = EApp (e1, e2)

open Dsl

let env =
    [
        "libcall_add", tfun(tint, tfun(tint, tint))
    ]
    |> Map.ofList
    

let expr1 = xint 42
let expr2 = xlet "hurz" (xint 43) (xint 32)
let addA = xfun "a" (xapp (xvar "libcall_add") (xvar "a"))
let addB = xfun "b" (xapp addA (xvar "b"))
let expr3 = xlet "hurz" (xint 43) (xlet "f" addB (xapp (xapp (xvar "f") (xvar "hurz")) (xint 99)))

let idExp = xfun "x" (xvar "x")

let printConstraints = Infer.annotate env >> Infer.constrain >> Debug.printEquations
let printSolution = Infer.annotate env >> Infer.constrain >> Infer.solve >> Debug.printEquations
let infer = Infer.infer env
let solve = Infer.infer env >> fun x -> x.typ


solve <| idExp


(*

printConstraints expr3
printSolution idExp
printSolution expr3


infer expr3
solve expr1
solve expr2
solve <| xint 43
solve <| xlet "hurz" (xint 43) (xstr "sss")
solve <| idExp
solve <| xfun "x" (xstr "klököl")
solve <| xapp (xfun "x" (xvar "x")) (xint 2)
solve <| xapp (xfun "x" (xvar "x")) (xstr "Hello")

// unbound var "y":
infer <| xfun "x" (xfun "y" (xvar "x"))

solve <| xlet "k" (xfun "x" (xlet "f" (xfun "y" (xvar "x")) (xvar "f"))) (xvar "k")


solve <| xlet "k" (xint 43) (xlet "k" (xstr "sss") (xvar "k"))



// Errors
(*
solve <| xapp (xvar "libcall_add") (xstr "lklö")
*)


(*
// Der Typ von "f" ist _kein_ Polytyp
(fun f -> f "as", f 99) id

// Der Typ von "f" ist ein Polytyp
let f = id in f "as", f 99
*)
*)