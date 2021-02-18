#if INTERACTIVE
fsi.PrintWidth <- 250
#endif

type Expr =
    | EInt of int
    | EFloat of float
    | EString of string
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
    | TInt of int
    | TFloat of float
    | TString of string
    | TVar of ident: string
    | TLet of {| ident: string
                 assignment: AnnotatedTy
                 body: AnnotatedTy |}
    | TFun of {| ident: string
                 identTyAnno: TyAnnotation
                 body: AnnotatedTy |}
    | TApp of {| target: AnnotatedTy
                 arg: AnnotatedTy |}
and AnnotatedTy =
    { typedExpr: TypedExpr
      tyAnno: TyAnnotation }
and TyAnnotation =
    | Det of MType
    | Open of int
    | Unresolvable


module Infer =
    
    type Constraint =
        { hint: string
          left: TyAnnotation
          right: TyAnnotation }
    
    module private Map =
        let set (ident: string) (typAnno: TyAnnotation) map =
            map |> Map.change ident (fun _ -> Some typAnno)
        let resolveAnnotation (ident: string) (map: Map<string, TyAnnotation>) =
            match map |> Map.tryFind ident with
            | None -> Unresolvable
            | Some ta -> ta

    let annotate (expr: Expr) =
        
        let mutable tyCounter = -1
        let newTyVar () =
            tyCounter <- tyCounter + 1
            Open tyCounter
        let tyExpr expr = { typedExpr = expr; tyAnno = newTyVar() }

        let rec gen expr =
            match expr with
            | EInt x -> tyExpr (TInt x)
            | EFloat x -> tyExpr (TFloat x)
            | EString x -> tyExpr (TString x)
            | EVar ident -> tyExpr (TVar ident)
            | ELet (ident, assignment, body) ->
                tyExpr
                    (TLet {| ident = ident
                             assignment = gen assignment
                             body = gen body |})
            | EFun (ident, body) ->
                tyExpr
                    (TFun {| ident = ident
                             identTyAnno = newTyVar ()
                             body = gen body |})
            | EApp (target, arg) ->
                tyExpr
                    (TApp {| target = gen target
                             arg = gen arg |})
        gen expr

    let genConstraintSet (identMap: Map<string, TyAnnotation>) (typExpr: AnnotatedTy) =
        let rec genConstraints (typExpr: AnnotatedTy) (identMap: Map<string, TyAnnotation>) =
            [
                match typExpr.typedExpr with
                | TInt _ ->
                    yield { hint = "TInt"
                            left = typExpr.tyAnno
                            right = Det MInt }
                | TFloat _ ->
                    yield { hint = "TFloat"
                            left = typExpr.tyAnno
                            right = Det MFloat }
                | TString _ ->
                    yield { hint = "TString"
                            left = typExpr.tyAnno
                            right = Det MString }
                | TVar ident ->
                    yield { hint = $"TVar {ident}"
                            left = typExpr.tyAnno
                            right = Map.resolveAnnotation ident identMap }
                | TLet letExpr ->
                    yield { hint = "TLet"
                            left = typExpr.tyAnno
                            right = letExpr.body.tyAnno }
                    yield! genConstraints letExpr.assignment identMap
                    yield! genConstraints letExpr.body (identMap |> Map.set letExpr.ident letExpr.assignment.tyAnno)
                | TFun funExpr ->
                    yield { hint = "TFun"
                            left = typExpr.tyAnno
                            right = Det(MFun(funExpr.identTyAnno, funExpr.body.tyAnno )) }
                    yield! genConstraints funExpr.body (Map.set funExpr.ident funExpr.identTyAnno identMap)
                | TApp appExpr ->
                    // res = add 20 -> we know something about "add":
                    // It is a function that goes from int to whatever 'res' is
                    yield { hint = "TApp_target"
                            left = appExpr.target.tyAnno
                            right = Det(MFun(appExpr.arg.tyAnno, typExpr.tyAnno )) }
                    yield! genConstraints appExpr.arg identMap
                    yield! genConstraints appExpr.target identMap
            ]

        genConstraints typExpr identMap |> Set.ofList
        


///////// Test

[<AutoOpen>]
module Test =
    let intTyp = Det MInt
    let floatTyp = Det MFloat
    let stringTyp = Det MString
    let funTyp(a, b) = Det (MFun(a, b))

    let printTExpr (expr: AnnotatedTy) =
        let doIndent indent = indent + "    "
        let rec print (expr: AnnotatedTy) (indent: string) =
             printf $"{indent}({expr.tyAnno}) : "
             match expr.typedExpr with
             | TInt x ->
                 printfn "INT %d" x
             | TFloat x ->
                 printfn "FLOAT %f" x
             | TString x ->
                 printfn "STRING %s" x
             | TVar x ->
                 printfn "VAR %s" x
             | TLet letExpr ->
                 printfn "LET %s [assignment, body]" letExpr.ident
                 print letExpr.assignment (doIndent indent)
                 print letExpr.body (doIndent indent)
             | TFun funExpr ->
                 printfn "FUN %s:(%A) [body]" funExpr.ident funExpr.identTyAnno
                 print funExpr.body (doIndent indent)
             | TApp appExpr ->
                 printfn "APP [target, arg]"
                 print appExpr.target (doIndent indent)
                 print appExpr.arg (doIndent indent)
        print expr ""    

    let printConstraints (constraints: Set<Infer.Constraint>) =
        for c in constraints |> Set.toList do
            printfn "%A (%s) = %A" c.left c.hint c.right
    
    let lib =
        [
            "libcall_add", funTyp(intTyp, funTyp(intTyp, intTyp))
        ]

    let annotate expr =
        Infer.annotate expr
        |> printTExpr

    let constrain expr =
        Infer.annotate expr
        |> Infer.genConstraintSet (Map.ofList lib)
        |> printConstraints

        
let expr1 = EInt 42
let expr2 = ELet("hurz", EInt 43, EInt 32)
let add =
    let addA =
        EFun("a", EApp(EVar "libcall_add", EVar "a"))
    EFun("b", EApp(addA, EVar "b"))
let expr3 =
    ELet("hurz", EInt 43, ELet("f", add, EApp(EApp(EVar "f", EVar "hurz"), EInt 99)))


expr3 |> annotate
expr3 |> constrain


module X =
    
    let libcall_add a b = a + b
    let app f x = f x
    
    let hurz = 43 in
        let f =
            fun b ->
                (fun a -> app (app libcall_add a) b) in
                    app (app f hurz) 99


