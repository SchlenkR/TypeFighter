
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Records
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Records
// -----------------------------------------------------------------
// This file covers record *creation* and *flat property access* on
// fully-known records. For row-polymorphic access through lambda
// parameters, see Rows.fs.
// =================================================================


let envWithArithmetic =
    [
        BuiltinValues.unitValueIdent, Mono BuiltinTypes.unit
        "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
    ]


(*
    Source:    {
                   age = 22
                   name = "John"
                   address = "123 Main St"
               }
    Inferred:  { age: Number; name: String; address: String }
*)
// The most direct way to produce a record: build it inline from
// literals. The result type is a *structural* record — one field per
// definition, each carrying the type of its value. Notably, no
// record *name* was declared anywhere; the type is inferred purely
// from the shape. This is the headline feature demonstrated at its
// simplest.
let [<Test>] ``record creation from literals`` () =
    X.MkRecord [
        X.Property "age" (X.Lit 22)
        X.Property "name" (X.Lit "John")
        X.Property "address" (X.Lit "123 Main St")
    ]
    |> solve [] None
    |> shouldSolveType (
            Mono (TDef.RecordWith [
                "age", BuiltinTypes.number
                "name", BuiltinTypes.string
                "address", BuiltinTypes.string
            ]))


(*
    Env:
      add   : Number -> Number -> Number
      order : { quantity: Number }

    Source:    add 10 order.quantity
    Inferred:  Number
*)
// Property access on a record bound in the environment.
let [<Test>] ``property access on environment-provided record`` () =
    X.App (X.App (X.Var "add") (X.Lit 10)) (X.PropAcc (X.Var "order") "quantity")
    |> solve
        [
            yield! envWithArithmetic
            yield "order", Mono (TDef.RecordWith [
                "quantity", BuiltinTypes.number
            ])
        ]
        None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Source:    let myRecord =
                   {
                       age = 22
                       name = "John"
                   }
               myRecord
    Inferred:  { age: Number; name: String }
*)
// Round-trips a record through a `let` binding and returns it
// unchanged. The test confirms that `let` doesn't lose information —
// the outer expression's inferred type is exactly the record's type,
// fields and all. A type checker that "forgot" fields at a binding
// boundary would be visibly broken here.
let [<Test>] ``let-bound record keeps its fields`` () =
    X.Let (X.Ident "myRecord") (
        X.MkRecord [
            X.Property "age" (X.Lit 22)
            X.Property "name" (X.Lit "John")
        ]
    ) (X.Var "myRecord")
    |> solve [] None
    |> shouldSolveType (
            Mono (TDef.RecordWith [
                "age", BuiltinTypes.number
                "name", BuiltinTypes.string
            ]))


(*
    Source:    let myRecord = { age = 22; name = "John" } in
               myRecord.name
    Inferred:  String
*)
// Combines `let` with `.`-field access — both covered elsewhere in
// isolation — and checks the two interact correctly. After binding
// the record, reading `name` should yield that field's type. This is
// the everyday shape of record usage, so it better just work.
let [<Test>] ``property access on a let-bound record`` () =
    X.Let (X.Ident "myRecord") (
        X.MkRecord [
            X.Property "age" (X.Lit 22)
            X.Property "name" (X.Lit "John")
        ]
    ) (
        X.PropAcc (X.Var "myRecord") "name"
    )
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    let myRecord = { age = 22; name = "John" } in
               myRecord.age
    Inferred:  Number
*)
// Same expression, different field. Together with the previous test
// this confirms the inferencer returns *the right* field type per
// access, not just the first one it finds. It's easy to write a
// naïve implementation that always gets one access correct and
// quietly confuses the other.
let [<Test>] ``property access on a let-bound record (2)`` () =
    X.Let (X.Ident "myRecord") (
        X.MkRecord [
            X.Property "age" (X.Lit 22)
            X.Property "name" (X.Lit "John")
        ]
    ) (
        X.PropAcc (X.Var "myRecord") "age"
    )
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Source:    let myRecord = { age = 22; name = "John" } in
               myRecord.xxxxxxxx
    Error:     Member 'xxxxxxxx' is missing in record type { age: Number; name: String }
*)
// Asking for a field that isn't there must be a static error. The
// record's full shape is known at this point, so a missing-member
// problem can (and should) be caught before the program runs.
// Dynamic languages surface this as a runtime crash — TypeFighter
// refuses to typecheck the program at all.
let [<Test>] ``accessing a non-existing field fails`` () =
    X.Let (X.Ident "myRecord") (
        X.MkRecord [
            X.Property "age" (X.Lit 22)
            X.Property "name" (X.Lit "John")
        ]
    ) (
        X.PropAcc (X.Var "myRecord") "xxxxxxxx"
    )
    |> solve [] None
    |> shouldFail


(*
    Env:
      EQUALS : forall a. a -> a -> Bool

    Source:    let r = { IntField = 3; BooleanField = true } in
               EQUALS r.BooleanField r.IntField
    Error:     Can't unify Bool and Number
*)
// Both arguments to EQUALS must be the same type, but the accessed
// fields disagree.
let [<Test>] ``comparing two differently-typed fields fails`` () =
    let env =
        [ "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean) ]

    X.Let
        (X.Ident "r")
        (X.MkRecord [
            X.Property "IntField" (X.Lit 3)
            X.Property "BooleanField" (X.Lit true)
        ])
        (X.App
            (X.App (X.Var("EQUALS")) (X.PropAcc (X.Var "r") "BooleanField"))
            (X.PropAcc (X.Var "r") "IntField"))
    |> solve env None
    |> shouldFail


(*
    Env:
      AND    : Bool -> Bool -> Bool
      EQUALS : forall a. a -> a -> Bool

    Source:    let r = { IntField = 3; BooleanField = true } in
               AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
    Inferred:  Bool
*)
// Accessing multiple fields, each compared against a matching literal.
let [<Test>] ``using multiple fields in a boolean expression`` () =
    let env =
        [
            "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]

    let left =
        X.App
            (X.App (X.Var("EQUALS")) (X.PropAcc (X.Var "r") "IntField"))
            (X.Lit 3)
    let right =
        X.App
            (X.App (X.Var("EQUALS")) (X.PropAcc (X.Var "r") "BooleanField"))
            (X.Lit true)
    X.Let
        (X.Ident "r")
        (X.MkRecord [
            X.Property "IntField" (X.Lit 3)
            X.Property "BooleanField" (X.Lit true)
        ])
        (X.App
            (X.App (X.Var "AND") left)
            right)
    |> solve env None
    |> shouldSolveType (Mono(BuiltinTypes.boolean))


(*
    Source:    { 42 }
    Inferred:  { Number }
*)
// Positional record item — Option Z's second row in action.
// The record carries a single positional of type Number and no
// named fields.
let [<Test>] ``record with single positional literal`` () =
    X.MkRecord [ X.Positional (X.Lit 42) ]
    |> solve [] None
    |> shouldSolveType (
        Mono (TDef.RecordWithItems [] [ BuiltinTypes.number ]))


(*
    Source:    { "Circle" & radius = 3 }
    Inferred:  { String & radius: Number }
*)
// Mix of positional and named items. The positional literal widens
// to `String` at the term level (literal types only appear via
// type-annotations, not from expression literals) — this is the
// same behavior as numeric / string literals everywhere else.
let [<Test>] ``record with positional string and named field`` () =
    X.MkRecord [
        X.Positional (X.Lit "Circle")
        X.Property "radius" (X.Lit 3)
    ]
    |> solve [] None
    |> shouldSolveType (
        Mono (TDef.RecordWithItems
                [ "radius", BuiltinTypes.number ]
                [ BuiltinTypes.string ]))
