
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Functions
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Functions
// -----------------------------------------------------------------
// Lambda abstractions, application, partial application, and the
// shape of higher-order types. A lambda without usage constraints
// generalizes to a polymorphic type at its enclosing let binding
// (or at the top level, if there is no such binding).
// =================================================================


(*
    Source:    fun x -> x
    Inferred:  forall a. a -> a
*)
// The identity function has no constraints on `x`, so its type is
// quantified over all `a`. Read `forall a. a -> a` as *"for any type `a`
// you pick, this is a function that takes an `a` and returns an `a`"* —
// so `id` can be called on a number and return a number, on a string
// and return a string, etc. The `a` is chosen fresh at every use site.
// The `->` arrow reads "takes … returns".
let [<Test>] ``identity generalizes to forall a. a -> a`` () =
    X.Fun (X.Ident "x") (X.Var "x")
    |> solve [] None
    |> shouldSolveType (TDef.Generalize (%0 ^-> %0))


(*
    Source:    (fun x -> x) "hello"
    Inferred:  String
*)
// The companion to the generalization test: now that identity has the
// polytype `forall a. a -> a`, applying it to a string should pin `a`
// to `String` at this call. This is the first time we see the
// instantiation step at work — the generic `a` gets fixed from the
// argument type flowing in.
let [<Test>] ``identity applied to String returns String`` () =
    X.App (X.Fun (X.Ident "x") (X.Var "x")) (X.Lit "hello")
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    (fun x -> x) 42
    Inferred:  Number
*)
// Same instantiation trick, now with a number. Having the two cases
// side by side (String and Number) is the whole point — it shows
// that each application picks its own type for `a` independently,
// which is what polymorphism promises.
let [<Test>] ``identity applied to Number returns Number`` () =
    X.App (X.Fun (X.Ident "x") (X.Var "x")) (X.Lit 42)
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Env:
      add : Number -> Number -> Number

    Source:    add 100 10
    Inferred:  Number
*)
// Arrows are right-associative: `Number -> Number -> Number` reads as
// `Number -> (Number -> Number)`. So `add` takes a number and returns
// *another function* that takes a number and returns a number. Applying
// both arguments here therefore gives back a `Number`.
let [<Test>] ``fully applied binary function`` () =
    X.App (X.App (X.Var "add") (X.Lit 100)) (X.Lit 10)
    |> solve
        [ "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number) ]
        None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Env:
      add : Number -> Number -> Number

    Source:    add 100
    Inferred:  Number -> Number
*)
// Partial application returns a function waiting for the remaining args.
let [<Test>] ``partial application yields a function`` () =
    X.App (X.Var "add") (X.Lit 100)
    |> solve
        [ "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number) ]
        None
    |> shouldSolveType (Mono (BuiltinTypes.number ^-> BuiltinTypes.number))


(*
    Source:    fun f x -> f x
    Inferred:  forall a b. (a -> b) -> a -> b
*)
// The apply-combinator. A *higher-order function* is one that takes
// another function as an argument (or returns one) — here `f` is a
// parameter that gets applied to `x`. A *combinator* is just a
// self-contained function that refers only to its parameters (no
// outside names). From `f x`, the inferencer learns that `f` must
// accept whatever type `x` has and produce *some* return — so
// `f : a -> b`, with `a` and `b` both free. The whole expression
// generalizes over both.
let [<Test>] ``apply combinator generalizes to (a -> b) -> a -> b`` () =
    X.Fun (X.Ident "f")
        (X.Fun (X.Ident "x")
            (X.App (X.Var "f") (X.Var "x")))
    |> solve [] None
    |> shouldSolveType (TDef.Generalize ((%0 ^-> %1) ^-> %0 ^-> %1))


(*
    Source:    fun f x -> f (f x)
    Inferred:  forall a. (a -> a) -> a -> a
*)
// Feeding `f x` back into `f` forces input and output of `f` to
// have the same type — constraining to `a -> a`.
let [<Test>] ``double application constrains f to a -> a`` () =
    X.Fun (X.Ident "f")
        (X.Fun (X.Ident "x")
            (X.App (X.Var "f") (X.App (X.Var "f") (X.Var "x"))))
    |> solve [] None
    |> shouldSolveType (TDef.Generalize ((%0 ^-> %0) ^-> %0 ^-> %0))


(*
    Env:
      add : Number -> Number -> Number

    Source:    add 10 "hello"
    Error:     Can't unify Number and String
*)
// "Unify" is type-checker jargon for "make these two types agree".
// When the inferencer meets `add`'s signature (second arg: Number)
// and a string literal at that position, it tries to unify `Number`
// with `String` — and fails, because they're plainly different.
let [<Test>] ``function rejects mismatched argument type`` () =
    X.App (X.App (X.Var "add") (X.Lit 10)) (X.Lit "hello")
    |> solve
        [ "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number) ]
        None
    |> shouldFail
