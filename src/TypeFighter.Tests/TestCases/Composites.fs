
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Composites
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Composites — putting the pieces together
// -----------------------------------------------------------------
// Everywhere else in the test suite, each file isolates one feature
// (records, matches, let-binding, etc.). Real programs combine them.
// These tests show the *set-algebra* surface in action: discriminated
// unions expressed as `{ "Tag" & ...fields }`, literal-union refinement,
// and narrowing via `match` — all composed without any language
// feature beyond the ones already demonstrated.
// =================================================================


// -----------------------------------------------------------------
// Option<T> — the canonical discriminated union.
// -----------------------------------------------------------------
// At the type level:
//
//     Option<T> = { "None" } | { "Some" & T }
//
// Each arm is a record-set whose positional tag is a string literal.
// The tag narrows the arm; the rest of the record carries the payload.
// These tests build the *values* — type-level `Option<T>` parsing lives
// in the parser's Types.fs ("discriminated union - Option Some or None").


(*
    Source:    { "None" }
    Inferred:  { "None" }    (a record with one positional String literal)
*)
// The None arm of Option. The positional is a String literal at the
// term level — it widens to String in the inferred type, so the
// resulting record has shape `{ String }`. At the *type* level we
// would write `{ "None" }` using literal types.
let [<Test>] ``composite - Option.None value`` () =
    X.MkRecord [ X.Positional (X.Lit "None") ]
    |> solve [] None
    |> shouldSolveType (
        Mono (TDef.RecordWithItems [] [ BuiltinTypes.string ]))


(*
    Source:    { "Some" & 42 }
    Inferred:  { String & Number }
*)
// The Some arm carries a payload as a second positional. Two positionals,
// no named fields — a clean instance of the heterogeneous-set record.
let [<Test>] ``composite - Option.Some wrapping a number`` () =
    X.MkRecord [
        X.Positional (X.Lit "Some")
        X.Positional (X.Lit 42)
    ]
    |> solve [] None
    |> shouldSolveType (
        Mono (TDef.RecordWithItems
                []
                [ BuiltinTypes.string; BuiltinTypes.number ]))


// -----------------------------------------------------------------
// Shape — a discriminated union whose arms carry *named* payload.
// -----------------------------------------------------------------
// At the type level:
//
//     Shape = { "Circle" & radius: Number }
//           | { "Square" & side:   Number }
//
// Each value mixes a positional tag with a named field. The tag is a
// positional String literal; the named field is a Property. This is
// exactly the composition covered individually in Records.fs, applied
// to a real-world shape.


(*
    Source:    { "Circle" & radius = 3 }
    Inferred:  { String & radius: Number }
*)
let [<Test>] ``composite - Shape.Circle value`` () =
    X.MkRecord [
        X.Positional (X.Lit "Circle")
        X.Property "radius" (X.Lit 3)
    ]
    |> solve [] None
    |> shouldSolveType (
        Mono (TDef.RecordWithItems
                [ "radius", BuiltinTypes.number ]
                [ BuiltinTypes.string ]))


(*
    Source:    { "Square" & side = 5 }
    Inferred:  { String & side: Number }
*)
let [<Test>] ``composite - Shape.Square value`` () =
    X.MkRecord [
        X.Positional (X.Lit "Square")
        X.Property "side" (X.Lit 5)
    ]
    |> solve [] None
    |> shouldSolveType (
        Mono (TDef.RecordWithItems
                [ "side", BuiltinTypes.number ]
                [ BuiltinTypes.string ]))


// -----------------------------------------------------------------
// HttpStatus — a literal-type union, narrowed by `match`.
// -----------------------------------------------------------------
// `HttpStatus = 200 | 404 | 500` is just a union of Number literal
// types. Pattern-matching arms emit CHasMember constraints; the
// scrutinee's type closes to exactly the set of matched literals.


(*
    Source:    fun s -> match s with
                        | 200 -> "ok"
                        | 404 -> "missing"
                        | 500 -> "boom"
    Inferred:  {200 | 404 | 500} -> String
*)
// Three numeric literal arms narrow the function parameter to the
// union `{200 | 404 | 500}`. Each arm body is a String, so the
// return type closes to String. A caller passing `999` would fail
// at unification — that's the whole point of literal-union refinement.
let [<Test>] ``composite - HttpStatus classifier narrows to literal union`` () =
    X.Fun (X.Ident "s") (
        X.Match (X.Var "s") [
            X.Arm (X.PatLit 200) (X.Lit "ok")
            X.Arm (X.PatLit 404) (X.Lit "missing")
            X.Arm (X.PatLit 500) (X.Lit "boom")
        ])
    |> solve [] None
    |> shouldSolveType (
        Mono (
            UnionTyp (set [
                LiteralTyp (Number 200.0)
                LiteralTyp (Number 404.0)
                LiteralTyp (Number 500.0) ])
            ^-> BuiltinTypes.string))


// -----------------------------------------------------------------
// TrafficLight — a String literal union narrowed by the literal arms.
// -----------------------------------------------------------------
// Each literal pattern emits a CHasMember constraint on the scrutinee,
// so the parameter's type closes to exactly the union of those
// literals. A trailing wildcard *catches* leftover values at runtime
// but adds no type constraint of its own — the inferred parameter
// type is still the narrow literal union.


(*
    Source:    fun s -> match s with
                        | "red"   -> 0
                        | "green" -> 1
                        | _       -> 2
    Inferred:  {"red" | "green"} -> Number
*)
// Despite the wildcard, the parameter narrows to `"red" | "green"` —
// the only literals the pattern arms saw. All three arm bodies are
// numbers, so the result type is Number.
let [<Test>] ``composite - TrafficLight with wildcard keeps literal narrowing`` () =
    X.Fun (X.Ident "s") (
        X.Match (X.Var "s") [
            X.Arm (X.PatLit "red")   (X.Lit 0)
            X.Arm (X.PatLit "green") (X.Lit 1)
            X.Arm X.PatWild          (X.Lit 2)
        ])
    |> solve [] None
    |> shouldSolveType (
        Mono (
            UnionTyp (set [
                LiteralTyp (String "red")
                LiteralTyp (String "green") ])
            ^-> BuiltinTypes.number))


// -----------------------------------------------------------------
// Nested record with let-threading
// -----------------------------------------------------------------
// A let-bound outer record carrying an inner record, then pulling a
// specific nested field out. Exercises record inference + let-binding
// + multi-level property access together.


(*
    Source:    let user = { profile: { name: "Ada", age: 30 }, active: true } in
               user.profile.name
    Inferred:  String
*)
let [<Test>] ``composite - nested record field access through let`` () =
    X.Let (X.Ident "user") (
        X.MkRecord [
            X.Property "profile" (
                X.MkRecord [
                    X.Property "name" (X.Lit "Ada")
                    X.Property "age"  (X.Lit 30)
                ])
            X.Property "active" (X.Lit true)
        ]
    ) (
        X.PropAccN [ "user"; "profile"; "name" ]
    )
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)
