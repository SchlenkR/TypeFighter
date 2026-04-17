
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Literals
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Literals
// -----------------------------------------------------------------
// Numbers, strings, and booleans are the primitive literal forms.
// Booleans are modeled set-theoretically as the union of the two
// literal values `true` and `false` — so every boolean expression
// has type `Bool = true | false`.
// =================================================================


(*
    Source:    42
    Inferred:  Number
*)
// Reading tip: "solves to" is how we say "the type checker worked out
// its type to be …". A *type* is a compact description of what a value
// can be — `Number` is the set of all numbers. Every expression in
// TypeFighter has a type; the job of the inference engine is to figure
// that type out, usually without you writing it down.
let [<Test>] ``number literal solves to Number`` () =
    X.Lit 42
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Source:    "hello"
    Inferred:  String
*)
// Mirrors the number case: a string literal is self-evidently a
// `String`. These baseline cases are the anchors the rest of inference
// builds on — if literals can't be trusted, nothing built from them
// can be either.
let [<Test>] ``string literal solves to String`` () =
    X.Lit "hello"
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.string)


(*
    Source:    true
    Inferred:  Bool
*)
// `Bool` is *not* a primitive here — it's the set-theoretic union
// `true | false`. Each literal (`true`, `false`) is already a type on
// its own, and `Bool` is the union of those two literal types. So
// when we say the literal `true` "solves to `Bool`", we mean the
// inferencer widened its exact literal type to the enclosing union.
// (Numeric and string literals, by contrast, collapse straight to
// `Number`/`String` — only booleans are modelled this way today.)
let [<Test>] ``true literal solves to Bool`` () =
    X.Lit true
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.boolean)


(*
    Source:    false
    Inferred:  Bool
*)
// The companion to the `true` test. Confirms the `Bool = true | false`
// modelling is symmetric — both literals widen to the same union
// type. Without this, there'd be a suspicion that the encoding only
// works for one side.
let [<Test>] ``false literal solves to Bool`` () =
    X.Lit false
    |> solve [] None
    |> shouldSolveType (Mono BuiltinTypes.boolean)
