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
                 body: TypExpr |}
    | TApp of {| target: TypExpr
                 arg: TypExpr |}
and TypExpr =
    { expr: TExpr
      typAnno: TypAnno
      identMap: Map<string, TypAnno> }
and TypAnno =
    | Det of Typ
    | Open of int


module Infer =
    
    type Constraint =
        { hint: string
          left: TypAnno
          right: TypAnno }
    
    module private Map =
        let set key value map =
            map |> Map.change key (fun _ -> Some value)

    let annotate (identMap: Map<string, TypAnno>) (expr: Expr) =
        let mutable typeCounter = -1

        let newTypeVar () =
            typeCounter <- typeCounter + 1
            Open typeCounter

        let typExpr expr identMap =
            { expr = expr
              typAnno = newTypeVar ()
              identMap = identMap }

        let rec gen (identMap: Map<string, TypAnno>) = function
            | Int x -> typExpr (TInt x) identMap
            | Float x -> typExpr (TFloat x) identMap
            | String x -> typExpr (TString x) identMap
            | Var ident -> typExpr (TVar ident) identMap
            | Let (ident, assignment, body) ->
                let identMap = identMap |> Map.set ident (newTypeVar ())
                typExpr
                    (TLet {| ident = ident
                             assignment = gen identMap assignment
                             body = gen identMap body |})
                    identMap
            | Fun (ident, body) ->
                let identMap = identMap |> Map.set ident (newTypeVar ())
                typExpr
                    (TFun {| ident = ident
                             body = gen identMap body |})
                    identMap
            | App (target, arg) ->
                typExpr
                    (TApp {| target = gen identMap target
                             arg = gen identMap arg |})
                   identMap

        gen identMap expr

    let genConstraintSet (typExpr: TypExpr) =
        let rec genConstraints (typExpr: TypExpr) =
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
                            right = typExpr.identMap.[ident] }
                | TLet letExpr ->
                    yield { hint = "TLet_ident"
                            left = typExpr.identMap.[letExpr.ident]
                            right = letExpr.assignment.typAnno }
                    yield { hint = "TLet_itself"
                            left = typExpr.typAnno
                            right = letExpr.body.typAnno }
                    yield! genConstraints letExpr.assignment
                    yield! genConstraints letExpr.body
                | TFun funExpr ->
                    yield { hint = "TFun"
                            left = typExpr.typAnno
                            right = Det(FunTyp(typExpr.identMap.[funExpr.ident], funExpr.body.typAnno )) }
                    yield! genConstraints funExpr.body
                | TApp appExpr ->
                    // res = add 20 -> we know something about "add":
                    // It is a function that goes from int to whatever 'res' is
                    yield { hint = "TApp_target"
                            left = appExpr.target.typAnno
                            right = Det(FunTyp(appExpr.arg.typAnno, typExpr.typAnno )) }
                    yield! genConstraints appExpr.arg
                    yield! genConstraints appExpr.target
            ]
        genConstraints typExpr |> Set.ofList
        


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
             | TInt x -> printfn "%d" x
             | TFloat x -> printfn "%f" x
             | TString x -> printfn "%s" x
             | TVar x -> printfn "%s" x
             | TLet letExpr ->
                 printfn "LET %s [assignment, body]" letExpr.ident
                 print letExpr.assignment (doIndent indent)
                 print letExpr.body (doIndent indent)
             | TFun funExpr ->
                 printfn "FUN %s [body]" funExpr.ident
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
        Infer.annotate (lib |> Map.ofList) expr
        |> printTExpr

    let constrain expr =
        Infer.annotate (lib |> Map.ofList) expr
        |> Infer.genConstraintSet
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
