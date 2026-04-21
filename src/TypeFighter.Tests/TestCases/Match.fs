
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Match
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Expr.Match  (Step 6 of TypeSyntaxWithSets)
// -----------------------------------------------------------------
// Patterns supported in this first pass:
//   - Literal     (`| 0 ->`, `| "yes" ->`, `| true ->`)
//   - Var         (catch-all that binds the scrutinee to a name)
//   - Wildcard    (catch-all that ignores the scrutinee)
//
// Record / positional patterns will follow once the demand is real.
// Narrowing happens implicitly: literal patterns emit CHasMember
// against the scrutinee, so the scrutinee's type closes to the
// UnionTyp of the literals.
// =================================================================


(*
    Source:    fun x -> match x with | 0 -> "zero" | 1 -> "one"
    Inferred:  {0 | 1} -> String
*)
let [<Test>] ``match on numeric literals narrows scrutinee to union`` () =
    X.Fun (X.Ident "x") (
        X.Match (X.Var "x") [
            X.Arm (X.PatLit 0) (X.Lit "zero")
            X.Arm (X.PatLit 1) (X.Lit "one")
        ])
    |> solve [] None
    |> shouldSolveType (
        Mono (
            UnionTyp (set [ LiteralTyp (Number 0.0); LiteralTyp (Number 1.0) ])
            ^-> BuiltinTypes.string))


(*
    Source:    fun x -> match x with | true -> 1 | false -> 0
    Inferred:  Bool -> Number
    (set { true, false } collapses to Bool per TDef.RecordDefWith / literal-set semantics)
*)
let [<Test>] ``match on booleans covers both arms`` () =
    X.Fun (X.Ident "x") (
        X.Match (X.Var "x") [
            X.Arm (X.PatLit true) (X.Lit 1)
            X.Arm (X.PatLit false) (X.Lit 0)
        ])
    |> solve [] None
    |> shouldSolveType (
        Mono (BuiltinTypes.boolean ^-> BuiltinTypes.number))


(*
    Source:    fun x -> match x with | 42 -> "yes" | y -> "other"
    Inferred:  {42} -> String
    A Var arm is a catch-all — it binds but adds no constraint on the
    scrutinee's type. So the scrutinee only has the literal's member
    constraint and narrows accordingly. Explicit widening (via a type
    annotation on `x`) would change this.
*)
let [<Test>] ``match with catch-all Var pattern binds scrutinee`` () =
    X.Fun (X.Ident "x") (
        X.Match (X.Var "x") [
            X.Arm (X.PatLit 42) (X.Lit "yes")
            X.Arm (X.PatVar "y") (X.Lit "other")
        ])
    |> solve [] None
    |> shouldSolveType (
        Mono (
            UnionTyp (set [ LiteralTyp (Number 42.0) ])
            ^-> BuiltinTypes.string))


(*
    Source:    fun x -> match x with | y -> y
    Inferred:  forall a. a -> a
    (pure catch-all: the scrutinee is returned as-is)
*)
let [<Test>] ``match with single Var arm is identity`` () =
    X.Fun (X.Ident "x") (
        X.Match (X.Var "x") [
            X.Arm (X.PatVar "y") (X.Var "y")
        ])
    |> solve [] None
    |> shouldSolveType (TDef.Generalize (%1 ^-> %1))


(*
    Source:    match 5 with | 5 -> "five" | _ -> "other"
    Inferred:  String
*)
let [<Test>] ``match with wildcard as default arm`` () =
    X.Match (X.Lit 5) [
        X.Arm (X.PatLit 5) (X.Lit "five")
        X.Arm X.PatWild (X.Lit "other")
    ]
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    match 5 with | 5 -> "five" | 6 -> 42
    Error:     arm body types do not unify (String vs Number)
*)
let [<Test>] ``match arm body type mismatch fails`` () =
    X.Match (X.Lit 5) [
        X.Arm (X.PatLit 5) (X.Lit "five")
        X.Arm (X.PatLit 6) (X.Lit 42)
    ]
    |> solve [] None
    |> shouldFail
