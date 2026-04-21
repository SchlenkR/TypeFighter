
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Rows
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Rows (row-polymorphic record access)
// -----------------------------------------------------------------
// When a function accesses fields of a record it receives as an
// argument, the inferencer collects the *required* fields and
// reconstructs a record type that contains exactly those fields.
// This is Rémy-style row polymorphism (here, closed at the end).
// =================================================================


let envWithArithmetic =
    [
        "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
    ]


(*
    Source:    fun r -> r.name
    Inferred:  forall a. { name: a } -> a
*)
// *Row polymorphism* is the trick that makes this work. The body
// `r.name` tells the inferencer "whatever `r` is, it must have a
// `name` field". That requirement is called a *row constraint*. The
// inferred parameter type is the smallest record shape that satisfies
// it — `{ name: a }`, with `a` left free for whatever type `name`
// turns out to have at each call site.
let [<Test>] ``single field access`` () =
    X.Fun (X.Ident "r") (X.PropAcc (X.Var "r") "name")
    |> solve [] None
    |> shouldSolveType (
        TDef.Generalize (TDef.RecordWith [ "name", %0 ] ^-> %0))


(*
    Env:
      add : Number -> Number -> Number

    Source:    fun r -> add r.x r.y
    Inferred:  { x: Number; y: Number } -> Number
*)
// Two field accesses constrain both fields to Number.
let [<Test>] ``two field accesses combine into one record type`` () =
    X.Fun (X.Ident "r")
        (X.App
            (X.App (X.Var "add") (X.PropAcc (X.Var "r") "x"))
            (X.PropAcc (X.Var "r") "y"))
    |> solve envWithArithmetic None
    |> shouldSolveType (
        Mono (
            TDef.RecordWith [
                "x", BuiltinTypes.number
                "y", BuiltinTypes.number
            ]
            ^-> BuiltinTypes.number))


(*
    Env:
      add : Number -> Number -> Number

    Source:    fun r -> add r.x r.x
    Inferred:  { x: Number } -> Number
*)
// Using the same field twice is fine — no duplicate fields, just
// one Number-typed `x`.
let [<Test>] ``same field used twice produces one field constraint`` () =
    X.Fun (X.Ident "r")
        (X.App
            (X.App (X.Var "add") (X.PropAcc (X.Var "r") "x"))
            (X.PropAcc (X.Var "r") "x"))
    |> solve envWithArithmetic None
    |> shouldSolveType (
        Mono (
            TDef.RecordWith [ "x", BuiltinTypes.number ]
            ^-> BuiltinTypes.number))


(*
    Env:
      order : { customer: { name: String } }

    Source:    order.customer.name
    Inferred:  String
*)
// Chained property access builds a chain of row constraints.
let [<Test>] ``nested property access`` () =
    X.PropAccN [ "order"; "customer"; "name" ]
    |> solve
        [
            "order", Mono (TDef.NamedRecordWith (NameHint.Given "Order") [
                "customer", TDef.NamedRecordWith (NameHint.Given "Customer") [
                    "name", BuiltinTypes.string
                ]
            ])
        ]
        None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    let useName = fun r -> r.name in
               useName { name = "Alice"; age = 30 }
    Inferred:  String
*)
// Structural compatibility: the lambda only requires `name`, and the
// concrete record has extra fields — the inferencer discards them
// because only `name` is used.
let [<Test>] ``lambda requiring one field accepts record with extra fields`` () =
    X.Let (X.Ident "useName") (X.Fun (X.Ident "r") (X.PropAcc (X.Var "r") "name")) (
        X.App (X.Var "useName")
            (X.MkRecord [
                X.Property "name" (X.Lit "Alice")
                X.Property "age" (X.Lit 30)
            ]))
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    let getName = fun r -> r.name in
               [ getName { name = "a"; age = 1 };
                 getName { name = "b"; city = "NYC" } ]
    Inferred:  Array<String>   (once let-polymorphism is implemented)
*)
// Two different record shapes share the same row constraint. Each
// call site needs a fresh instantiation — this requires let-polymorphism
// on AST-level let, which is not yet implemented.
//
// NOT YET IMPLEMENTED: see Let.fs for the underlying limitation.
// Today the first call site closes the row to `{ name: String; age: Number }`,
// then the second record `{ name: String; city: String }` fails to unify.
let [<Test; Ignore("requires AST-level let-polymorphism — see Let.fs")>]
    ``same row-polymorphic function used on two different record shapes`` () =
    X.Let (X.Ident "getName") (X.Fun (X.Ident "r") (X.PropAcc (X.Var "r") "name")) (
        X.MkArray [
            X.App (X.Var "getName")
                (X.MkRecord [
                    X.Property "name" (X.Lit "a")
                    X.Property "age" (X.Lit 1)
                ])
            X.App (X.Var "getName")
                (X.MkRecord [
                    X.Property "name" (X.Lit "b")
                    X.Property "city" (X.Lit "NYC")
                ])
        ])
    |> solve [] None
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.string))


(*
    Source:    fun r -> r.greet r.name
    Inferred:  forall a b. { greet: a -> b; name: a } -> b
*)
// A field is used as a function applied to another field — constraints
// flow between fields via the function's argument/result types.
let [<Test>] ``field used as function applied to another field`` () =
    X.Fun (X.Ident "r")
        (X.App
            (X.PropAcc (X.Var "r") "greet")
            (X.PropAcc (X.Var "r") "name"))
    |> solve [] None
    |> shouldSolveType (
        TDef.Generalize (
            TDef.RecordWith [
                "greet", %0 ^-> %1
                "name", %0
            ]
            ^-> %1))


(*
    Env:
      add : Number -> Number -> Number

    Source:    fun r -> { doubled = add r.x r.x }
    Inferred:  { x: Number } -> { doubled: Number }
*)
// A lambda taking a record and returning a newly constructed record.
let [<Test>] ``lambda returns record built from input fields`` () =
    X.Fun (X.Ident "r")
        (X.MkRecord [
            X.Property "doubled"
                (X.App
                    (X.App (X.Var "add") (X.PropAcc (X.Var "r") "x"))
                    (X.PropAcc (X.Var "r") "x"))
        ])
    |> solve envWithArithmetic None
    |> shouldSolveType (
        Mono (
            TDef.RecordWith [ "x", BuiltinTypes.number ]
            ^-> TDef.NamedRecordWith (NameHint.Given "Result") [
                "doubled", BuiltinTypes.number
            ]))


(*
    Source:    let useName = fun r -> r.name in
               useName { age = 30 }
    Error:     Member 'name' is missing in record type { age: Number }
*)
// Row constraints must be satisfied at the call site. Missing a
// required field is a type error.
let [<Test>] ``error - applying row-polymorphic function to record missing a field`` () =
    X.Let (X.Ident "useName") (X.Fun (X.Ident "r") (X.PropAcc (X.Var "r") "name")) (
        X.App (X.Var "useName")
            (X.MkRecord [
                X.Property "age" (X.Lit 30)
            ]))
    |> solve [] None
    |> shouldFail


(*
    Env:
      EQUALS : forall a. a -> a -> Bool

    Source:    fun a b -> EQUALS a.x b.x
    Inferred:  forall t. { x: t } -> { x: t } -> Bool
*)
// Two records must each have field `x`, and the two `x` fields must
// unify to the same type (driven by EQUALS's signature). The two
// surrounding record shapes are otherwise independent.
let [<Test>] ``two record parameters linked via a shared field`` () =
    X.Fun (X.Ident "a")
        (X.Fun (X.Ident "b")
            (X.App
                (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "a") "x"))
                (X.PropAcc (X.Var "b") "x")))
    |> solve
        [ "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean) ]
        None
    |> shouldSolveType (
        TDef.Generalize (
            TDef.RecordWith [ "x", %0 ]
            ^-> TDef.RecordWith [ "x", %0 ]
            ^-> BuiltinTypes.boolean))
