
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Let
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Let bindings
// -----------------------------------------------------------------
// `let` introduces a name with two key properties:
//   1. Scope — the name is in scope only within the body.
//   2. Generalization — if the bound value has free type variables,
//      those are quantified, and every use of the name instantiates
//      the polytype fresh. This is "let-polymorphism".
// =================================================================


(*
    Env:
      add : Number -> Number -> Number

    Source:    let x = 10 in
               let y = 20 in
               add x y
    Inferred:  Number
*)
// Reading tip: `let name = value in body` introduces `name` so that
// `body` can refer to it. The name's *scope* is the body — outside
// the `let`, the name doesn't exist. Nesting is how you bind several
// names one after the other, each visible to everything to its right.
let [<Test>] ``nested lets introduce scoped names`` () =
    X.Let (X.Ident "x") (X.Lit 10) (
        X.Let (X.Ident "y") (X.Lit 20) (
            X.App (X.App (X.Var "add") (X.Var "x")) (X.Var "y")))
    |> solve
        [ "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number) ]
        None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Source:    let id = fun x -> x in
               id 42
    Inferred:  Number
*)
// A sanity check: binding the identity function to a name and using
// it via that name should work just like using the lambda inline.
// Without this baseline holding, nothing else in `Let.fs` would be
// meaningful — `let` must at least preserve the type of what it binds.
let [<Test>] ``let-bound identity can be applied`` () =
    X.Let (X.Ident "id") (X.Fun (X.Ident "x") (X.Var "x")) (
        X.App (X.Var "id") (X.Lit 42))
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Source:    let id = fun x -> x in
               let n = id 42 in
               let s = id "hello" in
               s
    Inferred:  String  (once let-polymorphism is implemented)
*)
// *Let-polymorphism* means: at the moment a `let` binds a value, any
// still-free type variables become universally quantified, and every
// later use of the name picks its own fresh copy of the type. In
// plainer words — after `let id = fun x -> x`, the two calls `id 42`
// and `id "hello"` get *independent* type variables, so they don't
// have to agree. Without this, the first use would pin `id`'s type
// and the second would be rejected.
//
// NOT YET IMPLEMENTED: the inferencer currently binds a let-ident to
// a raw TVar (EnvItem.Internal), not to a generalized polytype, so
// the second use unifies with the first. See Lang.fs Expr.Let branch
// in generateConstraints. Env-provided polytypes (EnvItem.External)
// already instantiate fresh — see Polymorphism.fs.
let [<Test; Ignore("let-polymorphism not yet implemented for AST-level let bindings")>]
    ``let-polymorphism allows use at different types`` () =
    X.Let (X.Ident "id") (X.Fun (X.Ident "x") (X.Var "x")) (
        X.Let (X.Ident "n") (X.App (X.Var "id") (X.Lit 42)) (
            X.Let (X.Ident "s") (X.App (X.Var "id") (X.Lit "hello")) (
                X.Var "s")))
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    let x = 1 in
               let x = "hello" in
               x
    Inferred:  String
*)
// *Shadowing*: a binding with the same name as an outer one hides
// the outer one inside its own scope. It's a reuse of the name, not a
// mutation — the outer `x` is still a `Number`, it's just unreachable
// from here. Different types on the two bindings is fine.
let [<Test>] ``inner let shadows the outer binding`` () =
    X.Let (X.Ident "x") (X.Lit 1) (
        X.Let (X.Ident "x") (X.Lit "hello") (
            X.Var "x"))
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    let identity = fun x -> x in
               identity
    Inferred:  forall a. a -> a
*)
// Returning a polymorphic let-bound value preserves its polytype.
let [<Test>] ``let-bound value keeps its polymorphic type`` () =
    X.Let (X.Ident "identity") (X.Fun (X.Ident "x") (X.Var "x")) (
        X.Var "identity")
    |> solve [] None
    |> shouldSolveType (TDef.Generalize (%0 ^-> %0))
