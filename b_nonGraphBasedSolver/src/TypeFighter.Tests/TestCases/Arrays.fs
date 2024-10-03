
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
//         "nil", TDef.Generalize (BuiltinTypes.array %1)
//         "cons", TDef.Generalize (%1 ^-> BuiltinTypes.array %1 ^-> BuiltinTypes.array %1)
//         "concat", Mono (BuiltinTypes.array BuiltinTypes.string ^-> BuiltinTypes.string)
//     ]



(*
    [ 1; 2; 3 ]
*)
let [<Test>] ``array literal number`` () =

    X.MkArray [ X.Lit "1"; X.Lit "2"; X.Lit "3" ]
    |> solve []
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.number))


(*
    [ "a"; "b"; "c" ]
*)
let [<Test>] ``array literal string`` () =

    X.MkArray [ X.Lit "a"; X.Lit "b"; X.Lit "c" ]
    |> solve []
    |> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.string))



(*
    [ "a"; 1; "c" ]
*)
// ERROR: Can't unify String and Number
let [<Test>] ``error - non-homogeneous arrays`` () =

    X.MkArray [ X.Lit "a"; X.Lit "1"; X.Lit "c" ]
    |> solve []
    |> shouldFail




(*
    [
        { validFrom = MkThing "foo1" };
        { validFrom = MkThing "foo2" };
    ]
*)
let [<Test>] ``array with multiple record elements having field value from function app`` () =

    let defaultTcEnv =
        [
            "MkThing", TDef.Generalize (BuiltinTypes.string ^-> %1)
        ]
 
    let ast =
        X.MkArray
            [
                X.MkRecord [
                    X.Field
                        "validFrom"
                        (X.App
                          (X.Var "MkThing")
                          (X.Lit "foo1")) 
                ]
                X.MkRecord [
                    X.Field
                        "validFrom"
                        (X.App
                          (X.Var "MkThing")
                          (X.Lit "foo2")) 
                ]
            ]
 
    // TODO: Comparison of poly types according to poly type params naming/numbering has to be implemented correctly
    //       + apply reindexing of vars (see reindex vars for external envs)
    //       %14 - that's the thing here.
    solve defaultTcEnv ast
    |> shouldSolveType (TDef.Generalize (BuiltinTypes.array (TDef.NamedRecordWith (NameHint.Given "Record") [ "validFrom", %14 ])))
