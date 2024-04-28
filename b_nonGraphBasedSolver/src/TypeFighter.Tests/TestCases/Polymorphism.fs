
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

    let x = ExprCtx()

    x.Do (x.App (x.Var "log") (x.Lit "Hello")) (
        x.Do (x.App (x.Var "log") (x.Lit "88")) (
            x.Lit "42"
        )
    )
    |> solve
        [
            yield "log", TGen (%1 ^-> BuiltinTypes.unit)
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    { 
        r1 = mkPoly true
        r2 = mkPoly 33
    }
*)
let [<Test>] ``polymorphic "mkPoly" with infered record`` () =

    let x = ExprCtx()

    x.MkRecord [
        x.Field "r1" (x.App (x.Var "mkPoly") (x.Lit "true"))
        x.Field "r2" (x.App (x.Var "mkPoly") (x.Lit "33"))
    ]
    |> solve
        [
            "mkPoly", TGen (%1 ^-> %1)
        ]
    |> shouldSolveType (
        Mono (TProvideMembersWith NameHint.Empty [
            "r1", BuiltinTypes.boolean
            "r2", BuiltinTypes.number
        ]))
    



