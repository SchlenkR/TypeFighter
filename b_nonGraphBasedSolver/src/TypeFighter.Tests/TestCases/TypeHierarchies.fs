
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.TypeHierarchies
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



let typeHierarchyEnv =
    [
        "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
        "concat", Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string)
    ]





(*
    // Works: implicit conversion from number to string
    concat 10 "Hello"
*)
let [<Test; Ignore("Acceptance invalid")>] ``implicit conversion from number to string`` () =

    let x = ExprCtx()

    x.App (x.App (x.Var "concat") (x.Lit "10")) (x.Lit "Hello")
    |> solve typeHierarchyEnv
    |> shouldSolveType (Mono BuiltinTypes.string)




