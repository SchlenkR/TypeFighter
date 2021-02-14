#if INTERACTIVE
fsi.PrintWidth <- 500
#endif

type Expr =
    | Int of int
    | Float of float
    | String of string
    | Var of ident:string
    | Let of ident:string * assignment:Expr * body:Expr
    | Fun of ident:string * body:Expr
    | App of target:Expr * arg:Expr
    // TODO: Bool,Ctor,If

type Typ =
    | TypInt
    | TypFloat
    | TypString
    | TypFun of Typ * Typ

type TExpr =
    | TInt of TypExpr<int>
    | TFloat of TypExpr<float>
    | TString of TypExpr<string>
    | TVar of TypExpr<{| ident: string |}>
    | TLet of TypExpr<{| ident:string; assignment:TExpr; body:TExpr |}>
    | TFun of TypExpr<{| ident:string; body:TExpr |}>
    | TApp of TypExpr<{| target:TExpr; arg:TExpr |}>
and TypExpr<'a> = { typeInfo: TypeInfo; value: 'a }
and TypeInfo =
    | Typed of Typ
    | Untyped of typeVarName: string

let typExpr (typeInfo, value) = { typeInfo = typeInfo; value = value }
    
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
    module private Map =
        let set key value map = map |> Map.change key (fun _ -> Some value)

    let toTypableAst (identMap: Map<string, TypeInfo>) (expr: Expr) =
        let mutable typeCounter = -1
        let makeUntyped() =
            typeCounter <- typeCounter + 1
            Untyped $"t_{typeCounter}"
        let rec gen (identMap: Map<string, TypeInfo>) (expr: Expr) =
            match expr with
            | Int x ->
                TInt (typExpr (Typed TypInt, x))
            | Float x ->
                TFloat (typExpr (Typed TypFloat, x))
            | String x ->
                TString (typExpr (Typed TypString, x))
            | Var ident ->
                let typeInfo = makeUntyped()
                TVar (typExpr (typeInfo, {| ident = ident |}))
            | Let (ident, assignment, body) ->
                let identMap = identMap |> Map.set ident (makeUntyped())
                TLet (
                    typExpr (
                        makeUntyped(),
                        {| ident = ident
                           assignment = gen identMap assignment
                           body = gen identMap body |}))
            | Fun (ident, body) ->
                let identMap = identMap |> Map.set ident (makeUntyped())
                TFun (
                    typExpr (
                        makeUntyped(),
                        {| ident = ident
                           body = gen identMap body |}))
            | App (target, arg) ->
                TApp (
                    typExpr (
                        makeUntyped(),
                        {| target = gen identMap target
                           arg = gen identMap arg |}))
        
        gen identMap expr

///////// Test

let expr1 = Int 42

let expr2 = Let ("hurz", Int 43, Int 32)

let add =
    let addA = Fun("a", App(Var "libcall_add", Var "a"))
    Fun("b", App(addA, Var "b"))

let expr3 =
    Let ("hurz", Int 43,
        Let ("f", add,
            App (App (Var "f", Var "hurz"), Int 99)))




let test expr =
    let lib =
        [
            "libcall_add", Typed (TypFun (TypInt, TypFun (TypInt, TypInt)))
        ]
    Infer.toTypableAst (lib |> Map.ofList) expr

test expr3
