#if INTERACTIVE
fsi.PrintWidth <- 250
#endif

type Lit
    = LInt of int
    | LFloat of float
    | LString of string
type Exp
    = ELit of Lit
    | EVar of string
    | EApp of Exp * Exp
    | EFun of string * Exp
    | ELet of string * Exp * Exp

type ErrorMsg = string

type MType
    = MInt
    | MFloat
    | MString
    | MFun of TVar * TVar
and TVar
    = Free of int
    | Det of MType
    | Unresolvable of ErrorMsg
and TExp
    = TLit of Lit
    | TVar of ident: string
    | TLet of {| ident: string; assignment: TExpAnno; body: TExpAnno |}
    | TFun of {| ident: Ident; body: TExpAnno |}
    | TApp of {| target: TExpAnno; arg: TExpAnno |}
and Ident = { name: string; tvar: TVar }
and TExpAnno = { expr: TExp; annotation: TVar; env: Env }
and Env = Map<string, TVar>


module Infer =
    
    type Equation = { left: TVar;  right: TVar }
    
    type ConstraintTableEntry = { desc: string; constr: Equation }
    type VarTableEntry = { desc: string; nr: int; value: obj }

    type private TVarGen() =
        let mutable tyCounter = -1
        let table = ResizeArray<VarTableEntry>()
        member this.newTVar(desc, value) =
            tyCounter <- tyCounter + 1
            let entry = { desc = desc; nr = tyCounter; value = value }
            do table.Add entry
            Free tyCounter
        member this.newTyExpAnno(desc, value: obj, env, expr) =
            let tvar = this.newTVar(desc, value)
            { expr = expr; annotation = tvar; env = env }
        member this.Table = table |> Seq.toList
        
    type private ConstrGen() =
        let table = ResizeArray<ConstraintTableEntry>()
        member this.constrain(desc, l, r) =
            let c = { left = l; right = r; }
            let ce = { desc = desc; constr = c }
            table.Add ce
            c
        member this.Table = table |> Seq.toList

    module Map =
        let set (ident: string) (typAnno: TVar) map =
            map |> Map.change ident (fun _ -> Some typAnno)
        let resolveAnnotation (ident: string) (map: Map<string, TVar>) =
            match map |> Map.tryFind ident with
            | None -> Unresolvable $"Identifier {ident} is undefined."
            | Some ta -> ta

    let annotate (env: Env) (expr: Exp) =
        let tvarGen = TVarGen()

        let rec annotate env expr =
            match expr with
            | ELit x ->
                match x with
                | LInt y -> tvarGen.newTyExpAnno("Int", y, env, TLit x)
                | LFloat y -> tvarGen.newTyExpAnno("Float", y, env, TLit x)
                | LString y -> tvarGen.newTyExpAnno("String", y, env, TLit x)
            | EVar ident -> tvarGen.newTyExpAnno("Var", ident, env, TVar ident)
            | ELet (ident, assignment, body) ->
                let tyAss = annotate env assignment
                let env = env |> Map.set ident tyAss.annotation
                let texp = TLet {| ident = ident
                                   assignment = tyAss
                                   body = annotate env body |}
                tvarGen.newTyExpAnno("Let", ident, env, texp)
            | EFun (ident, body) ->
                let tvar = tvarGen.newTVar("FunIdent", ident)
                let env = env |> Map.set ident tvar
                let texp = TFun {| ident = { name = ident; tvar = tvar }
                                   body = annotate env body |}
                tvarGen.newTyExpAnno("Fun", body, env, texp)
            | EApp (target, arg) ->
                let texp = TApp {| target = annotate env target
                                   arg = annotate env arg |}
                tvarGen.newTyExpAnno("App", target, env, texp)
        
        let res = annotate env expr
        (res, tvarGen.Table)

    let private genConstraintSet (typExpr: TExpAnno) =
        let cgen = ConstrGen()
        
        let rec genConstraints (typExpr: TExpAnno) =
            [
                match typExpr.expr with
                | TLit lit ->
                    match lit with
                    | LInt x -> yield cgen.constrain($"Int {x}", typExpr.annotation, Det MInt)
                    | LFloat x -> yield cgen.constrain($"Float {x}", typExpr.annotation, Det MFloat)
                    | LString x -> yield cgen.constrain($"String {x}", typExpr.annotation, Det MString)
                | TVar var ->
                    yield cgen.constrain($"Var {var}", typExpr.annotation, typExpr.env |> Map.resolveAnnotation var)
                | TApp appExpr ->
                    // res = add 20 -> we know something about "add":
                    // It is a function that goes from int to whatever 'res' is
                    yield cgen.constrain("App", appExpr.target.annotation, Det(MFun(appExpr.arg.annotation, typExpr.annotation)))
                    yield! genConstraints appExpr.arg
                    yield! genConstraints appExpr.target
                | TFun funExpr ->
                    yield cgen.constrain("Fun", typExpr.annotation, Det(MFun(funExpr.ident.tvar, funExpr.body.annotation)))
                    yield! genConstraints funExpr.body
                | TLet letExpr ->
                    yield cgen.constrain($"Let {letExpr.ident}", typExpr.annotation, letExpr.body.annotation)
                    yield! genConstraints letExpr.assignment
                    yield! genConstraints letExpr.body
            ]
        let res = genConstraints typExpr
        (res, cgen.Table)

    let constrain lib expr =
        let a,_ = annotate lib expr
        let c,d = genConstraintSet a
        (c,d)

    type TraceResult
        = Cycle
        | Resolved of MType
        | NotFound
        | Inherited

    type VarTreeNode = { var: int; result: TraceResult; children: VarTreeNode list }

    let rec collectFreeVars (t: TVar) =
        match t with
        | Det m ->
            match m with
            | MInt | MFloat | MString -> []
            | MFun (m, n) ->
                [ yield! collectFreeVars m
                  yield! collectFreeVars n ]
        | Free x -> [ x ]
        | Unresolvable msg -> failwith $"Unresolvable: {msg}"
        
    let traceVar (var: int) (equations: Equation list)  =
        let rec doTraceVar (var: int) (visited: Set<int>) =
            let findEq i = equations |> List.tryFind (fun eq -> match eq.left with | Free x -> x = i | _ -> false)
            
            match Set.contains var visited with
            | true -> { var = var; result = Cycle; children = [] }
            | false ->
                match findEq var with
                | None -> { var = var; result = NotFound; children = [] }
                | Some v ->
                    let newVisited = Set.add var visited
                    let res =
                        collectFreeVars v.right
                        |> List.map (fun v -> doTraceVar v newVisited)
                    { var = var; result = Inherited; children = res }
        doTraceVar var Set.empty
        

module Helper =
    let printVarTable (_, table: Infer.VarTableEntry list) =
        for e in table |> List.sortByDescending (fun e -> e.nr) do
            printfn "%-15s %-5d %A" e.desc e.nr e.value
    
    let printConstrTable (_, table: Infer.ConstraintTableEntry list) =
        for e in table |> List.sortByDescending (fun e -> e.constr.left) do
            printfn "%-20s    %A = %A" e.desc e.constr.left e.constr.right



///////// Test

let intTyp = Det MInt
let floatTyp = Det MFloat
let stringTyp = Det MString
let funTyp(a, b) = Det (MFun(a, b))

let lib =
    [
        "libcall_add", funTyp(intTyp, funTyp(intTyp, intTyp))
    ]
    |> Map.ofList
    
        
let expr1 = LInt 42
let expr2 = ELet("hurz", ELit(LInt 43), ELit(LInt 32))
let add =
    let addA =
        EFun("a", EApp(EVar "libcall_add", EVar "a"))
    EFun("b", EApp(addA, EVar "b"))
let expr3 =
    ELet("hurz", ELit(LInt 43), ELet("f", add, EApp(EApp(EVar "f", EVar "hurz"), ELit(LInt 99))))


expr3 |> Infer.annotate lib |> Helper.printVarTable
expr3 |> Infer.constrain lib |> Helper.printConstrTable
expr3 |> Infer.constrain lib |> fst |> Infer.traceVar 3


Infer.collectFreeVars (Det (MFun (Free 13, Free 14)))

// module X =
//     
//     let libcall_add a b = a + b
//     let app f x = f x
//     
//     let hurz = 43
//     let f = fun b -> fun a -> (libcall_add a) b
//     (f hurz) 99
//

