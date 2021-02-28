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

type ErrorMsg = string

type MType =
    | MInt
    | MFloat
    | MString
    | MFun of TVar * TVar
and TVar =
    | Unresolvable of ErrorMsg
    | Det of MType
    | Free of int
and TExp =
    | TLit of Lit
    | TVar of string
    | TApp of {| target: TExpAnno; arg: TExpAnno |}
    | TFun of {| ident: Ident; body: TExpAnno |}
    | TLet of {| ident: string; assignment: TExpAnno; body: TExpAnno |}
and TExpAnno = { exp: TExp; annotation: TVar; env: Env }
and Ident = { name: string; tvar: TVar }
and Env = Map<string, TVar>

type Equation = { desc: string; left: TVar; right: TVar }

let emptyEnv: Env = Map.empty

module Infer =

    type VarTableEntry = { desc: string; nr: int }

    type TVarGen() =
        let mutable tyCounter = -1
        let table = ResizeArray<VarTableEntry>()
        member this.newTVar(desc) =
            tyCounter <- tyCounter + 1
            let entry = { desc = desc; nr = tyCounter; }
            do table.Add entry
            Free tyCounter
        member this.newTyExpAnno(desc, env, exp) =
            let tvar = this.newTVar(desc)
            { exp = exp; annotation = tvar; env = env }
        member this.Table = table |> Seq.toList
        
    let annotate (env: Env) (exp: Exp) =
        let tvarGen = TVarGen()

        let rec annotate env exp =
            match exp with
            | ELit x ->
                match x with
                | LInt _ -> tvarGen.newTyExpAnno("Int", env, TLit x)
                | LFloat _ -> tvarGen.newTyExpAnno("Float", env, TLit x)
                | LString _ -> tvarGen.newTyExpAnno("String", env, TLit x)
            | EVar ident -> tvarGen.newTyExpAnno("Var", env, TVar ident)
            | EApp (target, arg) ->
                let texp = TApp {| target = annotate env target; arg = annotate env arg |}
                tvarGen.newTyExpAnno("App", env, texp)
            | EFun (ident, body) ->
                let tvar = tvarGen.newTVar("FunIdent")
                let env = env |> Map.change ident (fun _ -> Some tvar)
                let texp = TFun {| ident = { name = ident; tvar = tvar }; body = annotate env body |}
                tvarGen.newTyExpAnno("Fun", env, texp)
            | ELet (ident, assignment, body) ->
                let tyAss = annotate env assignment
                let env = env |> Map.change ident (fun _ -> Some tyAss.annotation)
                let texp = TLet {| ident = ident; assignment = tyAss; body = annotate env body |}
                tvarGen.newTyExpAnno("Let", env, texp)

        
        let res = annotate env exp
        (res, tvarGen.Table)

    let genConstraints (typExpAnno: TExpAnno) =
        let constrain(desc, l, r) = { desc = desc; left = l; right = r; }
        
        let rec genConstraints (typExpAnno: TExpAnno) =
            [
                match typExpAnno.exp with
                | TLit tlit ->
                    match tlit with
                    | LInt x -> yield constrain($"Int {x}", typExpAnno.annotation, Det MInt)
                    | LFloat x -> yield constrain($"Float {x}", typExpAnno.annotation, Det MFloat)
                    | LString x -> yield constrain($"String {x}", typExpAnno.annotation, Det MString)
                | TVar tvar ->
                    let newEnv =
                        match typExpAnno.env |> Map.tryFind tvar with
                        | None -> Unresolvable $"Identifier {tvar} is undefined."
                        | Some ta -> ta
                    yield constrain($"Var {tvar}", typExpAnno.annotation, newEnv)
                | TApp tapp ->
                    // res = add 20 -> we know something about "add":
                    // It is a function that goes from int to whatever 'res' is
                    yield constrain("App", tapp.target.annotation, Det(MFun(tapp.arg.annotation, typExpAnno.annotation)))
                    yield! genConstraints tapp.arg
                    yield! genConstraints tapp.target
                | TFun tfun ->
                    yield constrain("Fun", typExpAnno.annotation, Det(MFun(tfun.ident.tvar, tfun.body.annotation)))
                    yield! genConstraints tfun.body
                | TLet tlet ->
                    yield constrain($"Let {tlet.ident}", typExpAnno.annotation, tlet.body.annotation)
                    yield! genConstraints tlet.assignment
                    yield! genConstraints tlet.body
            ]
        genConstraints typExpAnno

    let constrain lib exp =
        annotate lib exp |> fst |>  genConstraints

    let solveEquations (eqs: Equation list) =
        let subst (eqs: Equation list) (varNr: int) (dest: TVar) =
            let substTerm (tvar: TVar) =
                let rec subst (tvar: TVar) =
                    match tvar with
                    | Free i when i = varNr ->
                        dest
                    | Det typ ->
                        match typ with
                        | MFun (m, n) -> Det (MFun (subst m, subst n))
                        | _ -> tvar
                    | _ -> tvar
                subst tvar
            eqs |> List.map (fun eq -> { eq with left = substTerm eq.left; right = substTerm eq.right })

        let unify a b =
            match a,b with
            | MFun (m,n), MFun (o,p) ->
                let unify x y =
                    match x,y with
                    | Free _, _
                    | _, Free _ -> { desc = "unified"; left = x; right = y }
                    | Det _, Det _ -> { desc = "unified"; left = x; right = y }
                    | _ -> failwith "TODO: type error"
                [
                    yield unify m o
                    yield unify n p
                ]
            | a,b when a = b ->
                []
            | _ ->
                failwith $"type error: expedted: {b}, given: {a}"

        let rec solve (eqs: Equation list) (solution: Equation list) =            
            match eqs with
            | [] -> solution
            | eq :: eqs ->
                match eq.left, eq.right with
                | Free a, x
                | x, Free a ->
                    // substitute
                    let newEqs = subst eqs a x
                    let newSolution = subst solution a x
                    solve newEqs (eq :: newSolution)
                | Det a, Det b ->
                    // gen new constraints and solve
                    let newConstraints = unify a b
                    let newEqs = eqs @ newConstraints
                    let newSolution = solution
                    solve newEqs newSolution
                | _ ->
                    failwith "TODO: Unresolvable"
        
        solve (eqs |> List.sortByDescending (fun e -> e.left)) []
    
    let solve lib exp =
        let annotatedAst = annotate lib exp |> fst
        let constraintSet = genConstraints annotatedAst
        let solutionMap = solveEquations constraintSet
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
            let rec applySolution (texp: TExpAnno) =
                let finalExp =
                    match texp.exp with
                    | TLit _
                    | TVar _ ->
                        texp.exp
                    | TApp tapp ->
                        TApp {| tapp with target = applySolution tapp.target; arg = applySolution tapp.arg |}
                    | TFun tfun ->
                        TFun {| tfun with body = applySolution tfun.body |}
                    | TLet tlet ->
                        TLet {| tlet with assignment = applySolution tlet.assignment; body = applySolution tlet.body |}
                { exp = finalExp
                  annotation = find texp.annotation
                  env = emptyEnv }
            applySolution annotatedAst
        typedAst


module Debug =
    let printVarTable (table: Infer.VarTableEntry list) =
        for e in table |> List.sortByDescending (fun e -> e.nr) do
            printfn "%-15s %-5d" e.desc e.nr

    let printEquations (eqs: Equation list) =
        for e in eqs |> List.sortByDescending (fun e -> e.left) do
            printfn "%-20s    %A = %A" e.desc e.left e.right



///////// Test

module Dsl =
    let intTyp = Det MInt
    let floatTyp = Det MFloat
    let stringTyp = Det MString
    let funTyp(a, b) = Det (MFun(a, b))

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


expr3 |> Infer.annotate lib |> snd |> Debug.printVarTable
expr3 |> Infer.constrain lib |> Debug.printEquations

let solve = Infer.solve lib >> fun x -> x.annotation

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

