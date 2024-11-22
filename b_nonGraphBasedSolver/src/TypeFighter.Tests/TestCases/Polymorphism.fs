
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Polymorphism
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




(*
    (log "Hello")
    (log 88)
    42
*)
let [<Test>] ``polymorphic "log"`` () =

    X.Do (X.App (X.Var "log") (X.Lit "Hello")) (
        X.Do (X.App (X.Var "log") (X.Lit 88)) (
            X.Lit 42
        )
    )
    |> solve
        [
            yield "log", TDef.Generalize (%1 ^-> BuiltinTypes.unit)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    { 
        r1 = mkPoly true
        r2 = mkPoly 33
    }
*)
let [<Test>] ``polymorphic "mkPoly" with infered record`` () =

    X.MkRecord [
        X.Field "r1" (X.App (X.Var "mkPoly") (X.Lit true))
        X.Field "r2" (X.App (X.Var "mkPoly") (X.Lit 33))
    ]
    |> solve
        [
            "mkPoly", TDef.Generalize (%1 ^-> %1)
        ]
    |> shouldSolveType (
        Mono (TDef.RecordWith [
            "r1", BuiltinTypes.boolean
            "r2", BuiltinTypes.number
        ]))
    



