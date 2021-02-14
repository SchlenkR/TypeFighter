#if INTERACTIVE
fsi.PrintWidth <- 500
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
    | FunTyp of Typ * Typ

type TExpr =
    | TInt of TypExpr<int>
    | TFloat of TypExpr<float>
    | TString of TypExpr<string>
    | TVar of TypExpr<{| ident: string |}>
    | TLet of
        TypExpr<{| ident: string
                   assignment: TExpr
                   body: TExpr |}>
    | TFun of TypExpr<{| ident: string; body: TExpr |}>
    | TApp of TypExpr<{| target: TExpr; arg: TExpr |}>
and TypExpr<'a> =
    { typeInfo: TypInfo
      value: 'a
      identMap: Map<string, TypInfo> }
and TypInfo =
    | Det of Typ
    | Open of TypVar
and TypVar = TypVar of int

// module Print =
//     let printTExpr (expr: TExpr) =
//         match expr with
//         | TInt x ->
//         | TFloat x
//         | TString x
//         | TVar of ident: string
//         | TLet of ident: string * assignment: TypExpr * body: TypExpr
//         | TFun of ident: string * body: TypExpr
//         | TApp of target: TypExpr * arg: TypExpr


module Infer =
    
    type Constraint =
        { left: TypInfo
          right: TypInfo }
    
    module private Map =
        let set key value map =
            map |> Map.change key (fun _ -> Some value)

    let toTypableAst (identMap: Map<string, TypInfo>) (expr: Expr) =
        let mutable typeCounter = -1

        let newTypeVar () =
            typeCounter <- typeCounter + 1
            Open(TypVar typeCounter)

        let typExpr value identMap =
            { typeInfo = newTypeVar ()
              value = value
              identMap = identMap }

        let rec gen (identMap: Map<string, TypInfo>) = function
            | Int x -> TInt(typExpr x identMap)
            | Float x -> TFloat(typExpr x identMap)
            | String x -> TString(typExpr x identMap)
            | Var ident -> TVar(typExpr {| ident = ident |} identMap)
            | Let (ident, assignment, body) ->
                let identMap =
                    identMap |> Map.set ident (newTypeVar ())
                TLet (
                    typExpr
                        {| ident = ident
                           assignment = gen identMap assignment
                           body = gen identMap body |}
                        identMap)
            | Fun (ident, body) ->
                let identMap =
                    identMap |> Map.set ident (newTypeVar ())
                TFun (
                    typExpr
                        {| ident = ident
                           body = gen identMap body |}
                        identMap)
            | App (target, arg) ->
                TApp (
                    typExpr
                        {| target = gen identMap target
                           arg = gen identMap arg |}
                       identMap)

        gen identMap expr

    let genConstraintSet (expr: TExpr) =
        let right =
            match expr with
            | TInt x -> Det IntTyp
            | TFloat x -> Det FloatTyp
            | TString x -> Det StringTyp
            | TVar x -> x.identMap.[x.value.ident]
            | TLet x -> Det IntTyp
            | TFun x -> Det IntTyp
            | TApp x -> Det IntTyp
        { left = expr. }
       



///////// Test

let expr1 = Int 42

let expr2 = Let("hurz", Int 43, Int 32)

let add =
    let addA =
        Fun("a", App(Var "libcall_add", Var "a"))

    Fun("b", App(addA, Var "b"))

let expr3 =
    Let("hurz", Int 43, Let("f", add, App(App(Var "f", Var "hurz"), Int 99)))




let test expr =
    let lib =
        [ "libcall_add", Det(TypFun(TypInt, TypFun(TypInt, TypInt))) ]

    Infer.toTypableAst (lib |> Map.ofList) expr

test expr3
