
type Lit = { typeName: string; value: string }

type Exp =
    | ELit of Lit
    | EVar of string
    | EApp of Exp * Exp
    | EFun of string * Exp
    | ELet of string * Exp * Exp

type TyVar = int

type Ident = string

type Annotated<'var, 'expr> =
    { annotated: 'expr
      tvar: 'var }
type TExp =
    | TELit of Lit
    | TEVar of Ident
    | TEApp of Annotated<TyVar, TExp> * Annotated<TyVar, TExp>
    | TEFun of Annotated<TyVar, Ident> * Annotated<TyVar, TExp> // TODO: Wieso hier IdentAnno und nicht nur Ident?
    | TELet of Ident * Annotated<TyVar, TExp> * Annotated<TyVar, TExp>

type Constraint =
    | Class of {| name: string; constraints: Constraint list |}
    | Func of Constraint * Constraint
    
type Env = Map<Ident, TyVar>

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some tyvar)
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound."
        | Some t -> t

module Infer =
    type Newvar() =
        let mutable varCounter = -1
        member this.fresh() =
            varCounter <- varCounter + 1
            varCounter

    let annoExp (newvar: Newvar) (exp: Exp) =
        let rec annoExp (exp: Exp) =
            let texp =
                match exp with
                | ELit x ->
                    TELit x
                | EVar ident ->
                    TEVar ident
                | EApp (e1, e2) ->
                    TEApp (annoExp e1, annoExp e2)
                | EFun (ident, body) ->
                    let annotatedIdent = { annotated = ident; tvar = newvar.fresh() }
                    TEFun (annotatedIdent, annoExp body)
                | ELet (ident, e, body) ->
                    TELet (ident, annoExp e, annoExp body)
            { annotated = texp
              tvar = newvar.fresh () }
        annoExp exp

    //let constrain (env: Env) (annoExp: Annotated<TyVar, TExp>) : Subst list =
    //    let rec constrain (env: Env) (annoExp: Annotated<TyVar, TExp>) = [
    //        match annoExp.annotated with
    //        | TELit x ->
    //            yield Subst.create($"Lit {x.typeName}", annoExp.tvar, MBase x.typeName)
    //        | TEVar ident ->
    //            let tyvar = Env.resolve ident env
    //            yield Subst.create($"Var-Expr {ident}", annoExp.tvar, MVar tyvar)
    //        | TEApp (e1, e2) ->
    //            yield Subst.create("App (e1 = e2)", e1.tvar, MFun(MVar e2.tvar, MVar annoExp.tvar))
    //            yield! constrain env e2
    //            yield! constrain env e1
    //        | TEFun (ident, body) ->
    //            let newEnv = env |> Env.bind ident.annotated ident.tvar
    //            yield Subst.create("Fun-Expr", annoExp.tvar, MFun(MVar ident.tvar, MVar body.tvar))
    //            yield! constrain newEnv body
    //        | TELet (ident, e, body) ->
    //            let newEnv = env |> Env.bind ident e.tvar
    //            yield Subst.create($"Let-Expr {ident}", annoExp.tvar, MVar body.tvar)
    //            yield! constrain env e
    //            yield! constrain newEnv body
    //        ]
    //    constrain env annoExp
