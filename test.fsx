#if INTERACTIVE
fsi.PrintWidth <- 250
#endif

type Lit =
    | Int of int
    | Float of float
    | String of string
type Expr =
    | ELit of Lit
    | EVar of ident: string
    | ELet of ident: string * assignment: Expr * body: Expr
    | EFun of ident: string * body: Expr
    | EApp of target: Expr * arg: Expr
// TODO: Bool,Ctor,If

type MType =
    | MInt
    | MFloat
    | MString
    | MFun of TyAnnotation * TyAnnotation
and TypedExpr =
    | TLit of Lit
    | TVar of ident: string
    | TLet of {| ident: string; 
                 assignment: AnnotatedTy
                 body: AnnotatedTy |}
    | TFun of {| ident: Ident
                 body: AnnotatedTy |}
    | TApp of {| target: AnnotatedTy
                 arg: AnnotatedTy |}
and Ident =
    { name: string
      tyAnno: TyAnnotation }
and Scope = Map<string, TyAnnotation>
and AnnotatedTy =
    { expr: TypedExpr
      annotation: TyAnnotation
      scope: Scope }
and TyAnnotation =
    | Det of MType
    | Open of int
    | Unresolvable

module Infer =
    
    type Constraint =
        { left: TyAnnotation
          right: TyAnnotation }

    let constrain hint l r =
        printfn "%-10s    %A = %A" hint l r
        { left = l; right = r; }
    
    module Map =
        let set (ident: string) (typAnno: TyAnnotation) map =
            map |> Map.change ident (fun _ -> Some typAnno)
        let resolveAnnotation (ident: string) (map: Map<string, TyAnnotation>) =
            match map |> Map.tryFind ident with
            | None -> Unresolvable
            | Some ta -> ta

    let annotate (scope: Scope) (expr: Expr) =
        
        let newTyVar,tyExpr =
            let mutable tyCounter = -1
            let newTyVar (hint1, hint2: obj) =
                tyCounter <- tyCounter + 1
                printfn "%-15s %-5d %A" hint1 tyCounter hint2
                Open tyCounter
            newTyVar, fun (hint1, hint2: obj) scope expr ->
                let tyVar = newTyVar (hint1, hint2)
                { expr = expr; annotation = tyVar; scope = scope }

        let rec annotate scope expr =
            match expr with
            | ELit x ->
                match x with
                | Int y -> tyExpr ("Int", y) scope (TLit x)
                | Float y -> tyExpr ("Float", y) scope (TLit x)
                | String y -> tyExpr ("String", y) scope (TLit x)
            | EVar ident -> tyExpr ("Var", ident) scope (TVar ident)
            | ELet (ident, assignment, body) ->
                let tyAss = annotate scope assignment
                let scope = scope |> Map.set ident tyAss.annotation
                TLet {| ident = ident
                        assignment = tyAss
                        body = annotate scope body |}
                |> tyExpr ("Let", ident) scope
            | EFun (ident, body) ->
                let identTyAnno = newTyVar ("FunIdent", ident)
                let scope = scope |> Map.set ident identTyAnno
                TFun {| ident = { name = ident; tyAnno = identTyAnno }
                        body = annotate scope body |}
                |> tyExpr ("Fun", body) scope
            | EApp (target, arg) ->
                TApp {| target = annotate scope target
                        arg = annotate scope arg |}
                |> tyExpr ("App", target) scope
        annotate scope expr

    let genConstraintSet (typExpr: AnnotatedTy) =
        let rec genConstraints (typExpr: AnnotatedTy) =
            [
                match typExpr.expr with
                | TLit lit ->
                    match lit with
                    | Int _ -> yield constrain "Int" typExpr.annotation (Det MInt)
                    | Float _ -> yield constrain "Float" typExpr.annotation (Det MFloat)
                    | String _ -> yield constrain "String" typExpr.annotation (Det MString)
                | TVar var ->
                    yield constrain "Var" typExpr.annotation (typExpr.scope |> Map.resolveAnnotation var)
                | TApp appExpr ->
                    // res = add 20 -> we know something about "add":
                    // It is a function that goes from int to whatever 'res' is
                    yield constrain "App   " appExpr.target.annotation (Det(MFun(appExpr.arg.annotation, typExpr.annotation)))
                    yield! genConstraints appExpr.arg
                    yield! genConstraints appExpr.target
                | TFun funExpr ->
                    yield constrain "Fun" typExpr.annotation (Det(MFun(funExpr.ident.tyAnno, funExpr.body.annotation)))
                    yield! genConstraints funExpr.body
                | TLet letExpr ->
                    yield constrain "Let" typExpr.annotation (letExpr.body.annotation)
                    yield! genConstraints letExpr.assignment
                    yield! genConstraints letExpr.body
            ]

        genConstraints typExpr |> Set.ofList
        


///////// Test

let intTyp = Det MInt
let floatTyp = Det MFloat
let stringTyp = Det MString
let funTyp(a, b) = Det (MFun(a, b))

let printTExpr (expr: AnnotatedTy) =
    let doIndent indent = indent + "    "
    let doIndent2 indent = indent + indent + "    "
    let newline indent = "\n" + indent + indent
    let rec print (expr: AnnotatedTy) (indent: string) =
         match expr.expr with
         | TLit lit ->
             match lit with
             | Int x -> $"{x}"
             | Float x -> $"{x}"
             | String x -> $"{x}"
         | TVar x ->
            $"{x}"
         | TLet letExpr ->
             $"let ({letExpr.ident}:{expr.scope |> Infer.Map.resolveAnnotation letExpr.ident}) = ({print letExpr.assignment indent}) in ({print letExpr.body indent})"
         | TFun funExpr ->
             $"fun ({funExpr.ident.name}[{funExpr.ident.tyAnno}] -> ({print funExpr.body indent})"
         | TApp appExpr ->
             $"{print appExpr.target System.String.Empty} {print appExpr.arg (doIndent indent)}"
    print expr ""

let lib =
    [
        "libcall_add", funTyp(intTyp, funTyp(intTyp, intTyp))
    ]
    |> Map.ofList

let annotate expr =
    Infer.annotate lib expr
    |> printTExpr

let constrain expr =
    let printConstraints (constraints: Set<Infer.Constraint>) =
        for c in constraints |> Set.toList do
            printfn "%A = %A" c.left c.right
    
    Infer.annotate lib expr
    |> Infer.genConstraintSet
    // |> printConstraints
    |> ignore

        
let expr1 = Int 42
let expr2 = ELet("hurz", ELit(Int 43), ELit(Int 32))
let add =
    let addA =
        EFun("a", EApp(EVar "libcall_add", EVar "a"))
    EFun("b", EApp(addA, EVar "b"))
let expr3 =
    ELet("hurz", ELit(Int 43), ELet("f", add, EApp(EApp(EVar "f", EVar "hurz"), ELit(Int 99))))


expr3 |> annotate
expr3 |> constrain


module X =
    
    let libcall_add a b = a + b
    let app f x = f x
    
    let hurz = 43
    let f = fun b -> fun a -> (libcall_add a) b
    (f hurz) 99


