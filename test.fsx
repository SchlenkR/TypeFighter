#if INTERACTIVE
fsi.PrintWidth <- 500
#endif

type Typ =
    | TInt
    | TFloat
    | TString
    | TFun of Typ * Typ

type Expr =
    | Int of int
    | Float of float
    | String of string
    | Var of ident:string
    | Let of ident:string * value:Expr * body:Expr
    | Fun of paramName:string * body:Expr
    | App of target:Expr * arg:Expr

type TypeInfo =
    | Typed of Typ
    | Untyped of typeVarName:string


module Infer =
    module private Map =
        let set key value map = map |> Map.change key (fun _ -> Some value)

    let genConstraintSet (idents: Map<string, TypeInfo>) (expr: Expr) =
        let rec gen (typedExpressions: Map<Expr, TypeInfo>) (idents: Map<string, TypeInfo>) (expr: Expr) =
            match expr with
            | Int x -> typedExpressions |> Map.add expr (Typed TInt), idents
            | Float x -> typedExpressions |> Map.add expr (Typed TFloat), idents
            | String x -> typedExpressions |> Map.add expr (Typed TString), idents
            | Var ident ->
                let identTypeVarName = $"t_{typedExpressions.Count + idents.Count}"
                let idents = idents |> Map.set ident (Untyped identTypeVarName)
                
                typedExpressions,idents
            | Let (ident, value, body) ->
                let identTypeVarName = $"t_{typedExpressions.Count + idents.Count}"
                let idents = idents |> Map.set ident (Untyped identTypeVarName)
                
                let exprTypeVarName = $"t_{typedExpressions.Count + idents.Count + 1}"
                let typedExpressions =
                    typedExpressions
                    |> Map.add expr (Untyped exprTypeVarName)
                    |> Map.add value (Untyped exprTypeVarName)
                    
                gen typedExpressions idents body
            | Fun (paramName, body) ->
                let identTypeVarName = $"t_{typedExpressions.Count + idents.Count}"
                let idents = idents |> Map.set paramName (Untyped identTypeVarName)
                
                let exprTypeVarName = $"t_{typedExpressions.Count + idents.Count + 1}"
                let typedExpressions =
                    typedExpressions
                    |> Map.add expr (Untyped exprTypeVarName)
                    
                gen typedExpressions idents body
            | App (target, arg) ->
                match idents |> Map.tryFind ident with
                | None -> failwith $"function application failed: iedntifier '{ident}' not found."
                | Some typeInfo ->
                    
        gen Map.empty idents expr


///////// Test

let expr1 = Int 42

let expr2 = Let ("hurz", Int 43, Int 32)

let res = Infer.genConstraintSet Map.empty expr2
