#if INTERACTIVE
fsi.PrintWidth <- 250
#endif

type Expr =
    | Int of int
    | Float of float
    | String of string
    | Var of ident: string
    | Let of ident: string * assignment: Expr * body: Expr
    | Fun of ident: string * body: Expr
    | App of target: Expr * arg: Expr
// TODO: Bool,Ctor,If

type Typ =
    | IntTyp
    | FloatTyp
    | StringTyp
    | FunTyp of TypAnno * TypAnno
and TExpr =
    | TInt of int
    | TFloat of float
    | TString of string
    | TVar of ident: string
    | TLet of {| ident: string
                 assignment: TypExpr
                 body: TypExpr |}
    | TFun of {| ident: string
                 identTypAnno: TypAnno
                 body: TypExpr |}
    | TApp of {| target: TypExpr
                 arg: TypExpr |}
and TypExpr =
    { expr: TExpr
      typAnno: TypAnno }
and TypAnno =
    | Det of Typ
    | Open of int
    | Unresolvable


module Infer =
    
    type Constraint =
        { hint: string
          left: TypAnno
          right: TypAnno }
    
    module private Map =
        let set (ident: string) (typAnno: TypAnno) map =
            map |> Map.change ident (fun _ -> Some typAnno)
        let resolveTypAnno (ident: string) (map: Map<string, TypAnno>) =
            match map |> Map.tryFind ident with
            | None -> Unresolvable
            | Some ta -> ta

    let annotate (expr: Expr) =
        
        let mutable typeCounter = -1
        let newTypeVar () =
            typeCounter <- typeCounter + 1
            Open typeCounter
        let typExpr expr =
            { expr = expr
              typAnno = newTypeVar() }

        let rec gen expr =
            match expr with
            | Int x ->
                typExpr (TInt x)
            | Float x ->
                typExpr (TFloat x)
            | String x ->
                typExpr (TString x)
            | Var ident ->
                typExpr (TVar ident)
            | Let (ident, assignment, body) ->
                typExpr
                    (TLet {| ident = ident
                             assignment = gen assignment
                             body = gen body |})
            | Fun (ident, body) ->
                typExpr
                    (TFun {| ident = ident
                             identTypAnno = newTypeVar ()
                             body = gen body |})
            | App (target, arg) ->
                typExpr
                    (TApp {| target = gen target
                             arg = gen arg |})
        gen expr

    let genConstraintSet (identMap: Map<string, TypAnno>) (typExpr: TypExpr) =
        let rec genConstraints (typExpr: TypExpr) (identMap: Map<string, TypAnno>) =
            [
                match typExpr.expr with
                | TInt _ ->
                    yield { hint = "TInt"
                            left = typExpr.typAnno
                            right = Det IntTyp }
                | TFloat _ ->
                    yield { hint = "TFloat"
                            left = typExpr.typAnno
                            right = Det FloatTyp }
                | TString _ ->
                    yield { hint = "TString"
                            left = typExpr.typAnno
                            right = Det StringTyp }
                | TVar ident ->
                    yield { hint = $"TVar {ident}"
                            left = typExpr.typAnno
                            right = Map.resolveTypAnno ident identMap }
                | TLet letExpr ->
                    yield { hint = "TLet"
                            left = typExpr.typAnno
                            right = letExpr.body.typAnno }
                    yield! genConstraints letExpr.assignment identMap
                    yield! genConstraints letExpr.body (identMap |> Map.set letExpr.ident letExpr.assignment.typAnno)
                | TFun funExpr ->
                    yield { hint = "TFun"
                            left = typExpr.typAnno
                            right = Det(FunTyp(funExpr.identTypAnno, funExpr.body.typAnno )) }
                    yield! genConstraints funExpr.body (Map.set funExpr.ident funExpr.identTypAnno identMap)
                | TApp appExpr ->
                    // res = add 20 -> we know something about "add":
                    // It is a function that goes from int to whatever 'res' is
                    yield { hint = "TApp_target"
                            left = appExpr.target.typAnno
                            right = Det(FunTyp(appExpr.arg.typAnno, typExpr.typAnno )) }
                    yield! genConstraints appExpr.arg identMap
                    yield! genConstraints appExpr.target identMap
            ]

        genConstraints typExpr identMap |> Set.ofList
        


///////// Test

[<AutoOpen>]
module Test =
    let intTyp = Det IntTyp
    let floatTyp = Det FloatTyp
    let stringTyp = Det StringTyp
    let funTyp(a, b) = Det (FunTyp(a, b))

    let printTExpr (expr: TypExpr) =
        let doIndent indent = indent + "    "
        let rec print (expr: TypExpr) (indent: string) =
             printf $"{indent}({expr.typAnno}) : "
             match expr.expr with
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
                 printfn "FUN %s:(%A) [body]" funExpr.ident funExpr.identTypAnno
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

        
let expr1 = Int 42
let expr2 = Let("hurz", Int 43, Int 32)
let add =
    let addA =
        Fun("a", App(Var "libcall_add", Var "a"))
    Fun("b", App(addA, Var "b"))
let expr3 =
    Let("hurz", Int 43, Let("f", add, App(App(Var "f", Var "hurz"), Int 99)))


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


