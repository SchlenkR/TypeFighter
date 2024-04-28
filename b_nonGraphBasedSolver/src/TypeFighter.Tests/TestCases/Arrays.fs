
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Arrays
#endif

open TypeFighter.Tests.TestHelper

// do this _after_ opening TestHelper
#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter.Lang


// ------------------------------------------
// What's important here: The tests shall be executable
// via dotnet test (this project), but also via FSI.
// ------------------------------------------




// we don't use nil+cons for lists, but "MkArray" as syntax sugar in the language
// let typeHierarchyParamPolyEnv =
//     [
//         "nil", TGen (BuiltinTypes.array %1)
//         "cons", TGen (%1 ^-> BuiltinTypes.array %1 ^-> BuiltinTypes.array %1)
//         "concat", Mono (BuiltinTypes.array BuiltinTypes.string ^-> BuiltinTypes.string)
//     ]



(*
    [ 1; 2; 3 ]
*)
let [<Test>] ``array literal number`` () =

    let x = ExprCtx()

    x.MkArray [ x.Lit "1"; x.Lit "2"; x.Lit "3" ]
    |> solve []
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.number))


(*
    [ "a"; "b"; "c" ]
*)
let [<Test>] ``array literal string`` () =

    let x = ExprCtx()

    x.MkArray [ x.Lit "a"; x.Lit "b"; x.Lit "c" ]
    |> solve []
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.string))



(*
    [ "a"; 1; "c" ]
*)
// ERROR: Can't unify String and Number
let [<Test>] ``error - non-homogeneous arrays`` () =

    let x = ExprCtx()

    x.MkArray [ x.Lit "a"; x.Lit "1"; x.Lit "c" ]
    |> solve []
    |> shouldFail

