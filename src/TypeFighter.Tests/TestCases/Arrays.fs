
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Arrays
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Arrays
// -----------------------------------------------------------------
// Arrays are homogeneous — all elements must unify to a single
// element type. Mixing incompatible element types is a static error.
// =================================================================


(*
    Source:    [ 1; 2; 3 ]
    Inferred:  Array<Number>
*)
// *Homogeneous* means all elements share one type. `Array<Number>` is
// "an array whose elements are numbers, all of them". Languages that
// allow `[1, "a", true]` either have a dynamic type or an implicit
// union — TypeFighter has neither, so mixing is a static error (see
// the failing test further down).
let [<Test>] ``array literal number`` () =
    X.MkArray [ X.Lit 1; X.Lit 2; X.Lit 3 ]
    |> solve [] None
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.number))


(*
    Source:    [ "a"; "b"; "c" ]
    Inferred:  Array<String>
*)
// Same story as the number array, with strings. Each element unifies
// against a shared element-type variable; because all three agree on
// `String`, the array's full type drops out as `Array<String>`.
let [<Test>] ``array literal string`` () =
    X.MkArray [ X.Lit "a"; X.Lit "b"; X.Lit "c" ]
    |> solve [] None
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.string))


(*
    Source:    [ "a"; 1; "c" ]
    Error:     Can't unify String and Number
*)
// Mixing a string and a number in the same array forces unification
// of `String` with `Number`, which fails. TypeFighter has no
// automatic widening to a common union here, so the mismatch is a
// hard static error. The test pins down the desired "fail early"
// behaviour — silently producing `Array<Unknown>` would be much worse.
let [<Test>] ``error - non-homogeneous arrays`` () =
    X.MkArray [ X.Lit "a"; X.Lit 1; X.Lit "c" ]
    |> solve [] None
    |> shouldFail


(*
    Env:
      MkThing : forall a. String -> a

    Source:    [
                   { validFrom = MkThing "foo1" };
                   { validFrom = MkThing "foo2" };
               ]
    Inferred:  forall a. Array<{ validFrom: a }>
*)
// Two array elements are records whose `validFrom` value comes from
// calling a polymorphic function. The inferencer has to see both
// calls as fresh instantiations, unify the two resulting record
// shapes, and then notice that nothing pins down the return type —
// so the result is an array of records over a still-free variable.
// This scenario is interesting because it combines polymorphism,
// record inference, and array homogeneity in one expression.
let [<Test>] ``array with multiple record elements having field value from function app`` () =

    let defaultTcEnv =
        [
            "MkThing", TDef.Generalize (BuiltinTypes.string ^-> %1)
        ]

    let ast =
        X.MkArray
            [
                X.MkRecord [
                    X.Property
                        "validFrom"
                        (X.App
                          (X.Var "MkThing")
                          (X.Lit "foo1"))
                ]
                X.MkRecord [
                    X.Property
                        "validFrom"
                        (X.App
                          (X.Var "MkThing")
                          (X.Lit "foo2"))
                ]
            ]

    ast
    |> solve defaultTcEnv None
    |> shouldSolveType (TDef.Generalize (BuiltinTypes.array (TDef.NamedRecordWith (NameHint.Given "Record") [ "validFrom", %10 ])))
