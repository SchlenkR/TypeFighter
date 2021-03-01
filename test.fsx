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

type TypeError = string

type Mono<'TypeVar> =
    | MInt
    | MFloat
    | MString
    | MFun of 'TypeVar * 'TypeVar

type Type =
    | Unresolvable of TypeError
    | Constr of Mono<Type>
    | Var of int

type Env = Map<string, Type>

type TExpAnno<'Exp> = { texp: 'Exp; annotation: Type; env: Env }

type TExp =
    | TELit of Lit
    | TEVar of string
    | TEApp of {| target: TExpAnno<TExp>; arg: TExpAnno<TExp> |}
    | TEFun of {| ident: Ident; body: TExpAnno<TExp> |}
    | TELet of {| ident: string; assignment: TExpAnno<TExp>; body: TExpAnno<TExp> |}
and Ident = { name: string; tvar: Type }

type Equation = { desc: string; left: Type; right: Type }

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

    let constrain (typExpAnno: TExpAnno<TExp>) =
        let rec genConstraints (typExpAnno: TExpAnno<TExp>) =
            let constrain(desc, l, r) = { desc = desc; left = l; right = r; }
            
            [
                match typExpAnno.texp with
                | TELit tlit ->
                    match tlit with
                    | LInt x -> yield constrain($"Int {x}", typExpAnno.annotation, Constr MInt)
                    | LFloat x -> yield constrain($"Float {x}", typExpAnno.annotation, Constr MFloat)
                    | LString x -> yield constrain($"String {x}", typExpAnno.annotation, Constr MString)
                | TEVar tvar ->
                    let newEnv =
                        match typExpAnno.env |> Map.tryFind tvar with
                        | None -> Unresolvable $"Identifier {tvar} is undefined."
                        | Some ta -> ta
                    yield constrain($"Var {tvar}", typExpAnno.annotation, newEnv)
                | TEApp tapp ->
                    yield constrain("App", tapp.target.annotation, Constr(MFun(tapp.arg.annotation, typExpAnno.annotation)))
                    yield! genConstraints tapp.arg
                    yield! genConstraints tapp.target
                | TEFun tfun ->
                    yield constrain("Fun", typExpAnno.annotation, Constr(MFun(tfun.ident.tvar, tfun.body.annotation)))
                    yield! genConstraints tfun.body
                | TELet tlet ->
                    yield constrain($"Let {tlet.ident}", typExpAnno.annotation, tlet.body.annotation)
                    yield! genConstraints tlet.assignment
                    yield! genConstraints tlet.body
            ]
        genConstraints typExpAnno

    let solve (eqs: Equation list) =
        
        let subst (eqs: Equation list) (varNr: int) (dest: Type) =
            let substTerm (tvar: Type) =
                let rec subst (tvar: Type) =
                    match tvar with
                    | Var i when i = varNr ->
                        dest
                    | Constr typ ->
                        match typ with
                        | MFun (m, n) -> Constr (MFun (subst m, subst n))
                        | _ -> tvar
                    | _ -> tvar
                subst tvar
            eqs |> List.map (fun eq -> { eq with left = substTerm eq.left; right = substTerm eq.right })

        /// unifies 2 types
        let unifyT tx ty =
            match tx,ty with
            | Var _, _
            | _, Var _ -> { desc = "unification"; left = tx; right = ty }
            | Constr _, Constr _ -> { desc = "unification"; left = tx; right = ty }
            | _ -> failwith "TODO: type error"

        /// unifies 2 mono types
        let unifyM ma mb =
            match ma,mb with
            | MFun (m,n), MFun (o,p) ->
                [
                    yield unifyT m o
                    yield unifyT n p
                ]
            | a,b when a = b -> []
            | _ -> failwith $"type error: expedted: {mb}, given: {ma}"

        let rec solve (eqs: Equation list) (solution: Equation list) =            
            match eqs with
            | [] -> solution
            | eq :: eqs ->
                match eq.left, eq.right with
                | Var a, x
                | x, Var a ->
                    // substitute
                    let newEqs = subst eqs a x
                    let newSolution = subst solution a x
                    solve newEqs (eq :: newSolution)
                | Constr a, Constr b ->
                    // gen new constraints and solve
                    let newConstraints = unifyM a b
                    let newEqs = eqs @ newConstraints
                    let newSolution = solution
                    solve newEqs newSolution
                | _ ->
                    failwith "TODO: Unresolvable"
        
        solve (eqs |> List.sortByDescending (fun e -> e.left)) []
    
    let typeAst lib exp =
        let annotatedAst = annotate lib exp
        let constraintSet = constrain annotatedAst
        let solutionMap = solve constraintSet
        let typedAst =
            let find var =
                // TODO: err can happen
                solutionMap
                |> List.choose (fun x ->
                    match x.left, x.right with
                    | a,b
                    | b,a when a = var -> Some b
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
        for e in eqs |> List.sortByDescending (fun e -> e.left) do
            printfn "%-20s    %A = %A" e.desc e.left e.right



///////// Test

module Dsl =
    let intTyp = Constr MInt
    let floatTyp = Constr MFloat
    let stringTyp = Constr MString
    let funTyp(a, b) = Constr (MFun(a, b))

    let xint x = ELit(LInt x)
    let xfloat x = ELit(LFloat x)
    let xstr x = ELit(LString x)
    let xvar ident = EVar(ident)
    let xlet ident e1 e2 = ELet(ident, e1, e2)
    let xfun ident e = EFun(ident, e)
    let xapp e1 e2 = EApp(e1, e2)

open Dsl

let lib =
    [
        "libcall_add", funTyp(intTyp, funTyp(intTyp, intTyp))
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


expr3 |> Infer.annotate lib |> Infer.constrain |> Debug.printEquations
expr3 |> Infer.annotate lib |> Infer.constrain |> Infer.solve |> Debug.printEquations

let solve = Infer.typeAst lib >> fun x -> x.annotation

solve expr3
solve expr1
solve expr2
solve <| xlet "hurz" (xint 43) (xstr "sss")
solve <| xfun "x" (xvar "x")
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

