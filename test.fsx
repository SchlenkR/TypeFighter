#if INTERACTIVE
fsi.PrintWidth <- 250
#endif

type Lit =
    | LInt of int
    | LFloat of float
    | LString of string
    
type Exp =
    | ELit of Lit
    | EVar of string
    | EApp of Exp * Exp
    | EFun of string * Exp
    | ELet of string * Exp * Exp

type Mono =
    | MBase of string
    | MFun of Mono * Mono
    | Var of int
    | TypeError of string

// type Poly<'TypeVar> =
//     | MBase of string
//     | MFun of 'TypeVar * 'TypeVar

type Env = Map<string, Mono>

type TExp =
    | TELit of Lit
    | TEVar of string
    | TEApp of {| target: TExpAnno; arg: TExpAnno |}
    | TEFun of {| ident: Ident; body: TExpAnno |}
    | TELet of {| ident: string; assignment: TExpAnno; body: TExpAnno |}
and TExpAnno = { texp: TExp; annotation: Mono; env: Env }
and Ident = { name: string; tvar: Mono }

type Equation = { desc: string; left: Mono; right: Mono }

let knownBaseTypes =
    {| int = "Int"
       float = "Float"
       string = "String" |}

module Infer =
        
    let emptyEnv: Env = Map.empty
        
    let annotate (env: Env) (exp: Exp) =
        
        let mutable tyCounter = -1
        let newTVar() =
            tyCounter <- tyCounter + 1
            Var tyCounter            

        let rec annotate env exp =
            let texp =
                match exp with
                | ELit x ->
                    match x with
                    | LInt _ -> TELit x
                    | LFloat _ -> TELit x
                    | LString _ -> TELit x
                | EVar ident -> TEVar ident
                | EApp (target, arg) ->
                    TEApp {| target = annotate env target; arg = annotate env arg |}
                | EFun (ident, body) ->
                    let tvar = newTVar()
                    let newEnv = env |> Map.change ident (fun _ -> Some tvar)
                    TEFun {| ident = { name = ident; tvar = tvar }; body = annotate newEnv body |}
                | ELet (ident, assignment, body) ->
                    let tyanno = annotate env assignment
                    let env = env |> Map.change ident (fun _ -> Some tyanno.annotation)
                    TELet {| ident = ident; assignment = tyanno; body = annotate env body |}
            { texp = texp; annotation = newTVar(); env = env }
        annotate env exp

    let constrain (typExpAnno: TExpAnno) =
        let genc(desc, l, r) = { desc = desc; left = l; right = r; }
        
        let rec genConstraints (typExpAnno: TExpAnno) =
            [
                match typExpAnno.texp with
                | TELit tlit ->
                    match tlit with
                    | LInt x -> yield genc($"Int {x}", typExpAnno.annotation, MBase knownBaseTypes.int)
                    | LFloat x -> yield genc($"Float {x}", typExpAnno.annotation, MBase knownBaseTypes.float)
                    | LString x -> yield genc($"String {x}", typExpAnno.annotation, MBase knownBaseTypes.string)
                | TEVar tvar ->
                    let newEnv =
                        match typExpAnno.env |> Map.tryFind tvar with
                        | None -> TypeError $"Identifier {tvar} is undefined."
                        | Some ta -> ta
                    yield genc($"Var {tvar}", typExpAnno.annotation, newEnv)
                | TEApp tapp ->
                    yield genc("App", tapp.target.annotation, MFun(tapp.arg.annotation, typExpAnno.annotation))
                    yield! genConstraints tapp.arg
                    yield! genConstraints tapp.target
                | TEFun tfun ->
                    yield genc("Fun", typExpAnno.annotation, MFun(tfun.ident.tvar, tfun.body.annotation))
                    yield! genConstraints tfun.body
                | TELet tlet ->
                    yield genc($"Let {tlet.ident}", typExpAnno.annotation, tlet.body.annotation)
                    yield! genConstraints tlet.assignment
                    yield! genConstraints tlet.body
            ]

        genConstraints typExpAnno

    let solve (eqs: Equation list) =
        
        let subst (eqs: Equation list) (varNr: int) (dest: Mono) =
            let substTerm (tvar: Mono) =
                let rec subst (tvar: Mono) =
                    match tvar with
                    | Var i when i = varNr ->
                        dest
                    | MFun (m, n) -> MFun (subst m, subst n)
                    | _ -> tvar
                subst tvar
            eqs |> List.map (fun eq -> { eq with left = substTerm eq.left; right = substTerm eq.right })

        //  TODO: Is this MGU?
        let rec unify m1 m2 =
            match m1,m2 with
            | MFun (l,r), MFun (l',r') ->
                [
                    yield! unify l l'
                    yield! unify r r'
                ]
            | Var _, _ ->
                [ { desc = "unification"; left = m1; right = m2 } ]
            | _, Var _ ->
                [ { desc = "unification"; left = m2; right = m1 } ]
            | a,b when a = b ->
                []
            | _ -> failwith $"type error: expedted: {m2}, given: {m1}"

        let rec solve (eqs: Equation list) (solution: Equation list) =            
            match eqs with
            | [] -> solution
            | eq :: eqs ->
                match eq.left, eq.right with
                | TypeError _, _
                | _, TypeError _ ->
                    failwith "TODO: Unresolvable"
                | Var a, x
                | x, Var a ->
                    // substitute
                    let newEqs = subst eqs a x
                    let newSolution = subst solution a x
                    solve newEqs (eq :: newSolution)
                | a, b ->
                    // gen new constraints and solve
                    let newConstraints = unify a b
                    let newEqs = eqs @ newConstraints
                    let newSolution = solution
                    solve newEqs newSolution
        
        solve (eqs |> List.sortByDescending (fun e -> e.left)) []
    
    let typeAst env exp =
        let annotatedAst = annotate env exp
        let constraintSet = constrain annotatedAst
        let solutionMap = solve constraintSet
        let typedAst =
            let find var =
                // TODO: err can happen
                solutionMap
                |> List.choose (fun x ->
                    match x.left, x.right with
                    | l,r | r,l when l = var -> Some r
                    | _ -> None)
                |> List.exactlyOne
            let rec applySolution (texp: TExpAnno<TExp>) =
                let finalExp =
                    match texp.texp with
                    | TELit _ | TEVar _ -> texp.texp
                    | TEApp tapp -> TEApp {| tapp with target = applySolution tapp.target; arg = applySolution tapp.arg |}
                    | TEFun tfun -> TEFun {| tfun with body = applySolution tfun.body |}
                    | TELet tlet -> TELet {| tlet with assignment = applySolution tlet.assignment; body = applySolution tlet.body |}
                { texp = finalExp; annotation = find texp.annotation; env = emptyEnv }
            applySolution annotatedAst
        typedAst


module Debug =

    let printEquations (eqs: Equation list) =
        eqs
        |> List.map (fun e ->
            let l,r =
                match e.left,e.right with
                | Var a,x
                | x, Var a -> Var a,x
                | x, y -> x,y
            l, r, e.desc)
        |> List.sortBy (fun (x,_,_) -> x)
        |> List.iter (fun (l,r,desc) -> printfn "%-20s    %A = %A" desc l r)



///////// Test

module Dsl =
    let tint = MBase knownBaseTypes.int
    let tfloat = MBase knownBaseTypes.float
    let tstring = MBase knownBaseTypes.string
    let tfun(a, b) = MFun(a, b)

    let xint x = ELit(LInt x)
    let xfloat x = ELit(LFloat x)
    let xstr x = ELit(LString x)
    let xvar ident = EVar(ident)
    let xlet ident e1 e2 = ELet(ident, e1, e2)
    let xfun ident e = EFun(ident, e)
    let xapp e1 e2 = EApp(e1, e2)

open Dsl

let env =
    [
        "libcall_add", tfun(tint, tfun(tint, tint))
    ]
    |> Map.ofList
    
        
let expr1 = xint 42
let expr2 = xlet "hurz" (xint 43) (xint 32)
let add =
    let addA =
        xfun "a" (xapp (xvar "libcall_add") (xvar "a"))
    xfun "b" (xapp addA (xvar "b"))
let expr3 =
    xlet "hurz" (xint 43) (xlet "f" add (xapp (xapp (xvar "f") (xvar "hurz")) (xint 99)))

let idExp = xfun "x" (xvar "x")

let printConstraints = Infer.annotate env >> Infer.constrain >> Debug.printEquations
let printSolution = Infer.annotate env >> Infer.constrain >> Infer.solve >> Debug.printEquations
let solve = Infer.typeAst env >> fun x -> x.annotation


printConstraints expr3
printSolution idExp

solve expr3
solve expr1
solve expr2
solve <| xlet "hurz" (xint 43) (xstr "sss")
solve <| idExp
solve <| xfun "x" (xstr "klököl")
solve <| xapp (xfun "x" (xvar "x")) (xint 2)
solve <| xapp (xfun "x" (xvar "x")) (xstr "Hello")

// Error
//solve <| xapp (xvar "libcall_add") (xstr "lklö")



// module X =
//     
//     let libcall_add a b = a + b
//     let app f x = f x
//     
//     let hurz = 43
//     let f = fun b -> fun a -> (libcall_add a) b
//     (f hurz) 99
//

