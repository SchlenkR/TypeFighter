
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.TypeHierarchies
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Type hierarchies / implicit conversions
// -----------------------------------------------------------------
// Scenarios where one type could be coerced to another — for example
// where a `String` is expected but a `Number` is given. Currently
// unsupported: unification rejects the mismatch outright. Adding
// support would require either explicit sub-typing relations or some
// form of conversion constraint. (TypeFighter does *not* do anything
// magical here — there's no automatic `ToString` or similar. The
// hypothetical behavior below is purely what the test *would* expect
// if conversions were wired in.)
// =================================================================


let typeHierarchyEnv =
    [
        "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        "concat", Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string)
    ]


(*
    Env:
      concat : String -> String -> String

    Source:    concat 10 "Hello"
    Inferred:  String   (IF implicit Number → String conversion were allowed)
*)
// NOT YET IMPLEMENTED: implicit conversions. Today this fails with
// "Can't unify Number and String". Kept as an ignored test to mark
// the intended behavior.
let [<Test; Ignore("Acceptance invalid")>] ``implicit conversion from number to string`` () =
    X.App (X.App (X.Var "concat") (X.Lit 10)) (X.Lit "Hello")
    |> solve typeHierarchyEnv None
    |> shouldSolveType (Mono BuiltinTypes.string)
