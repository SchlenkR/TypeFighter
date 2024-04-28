
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Misc
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
    SetDataField Object.ShouldBeComboxVariable Environment.AuthUser.Username
*)
// ERROR Date != String
let [<Test>] ``error - SetDataField wrong types`` () =

    let x = ExprCtx()

    x.App
        (x.App (x.Var "SetDataField") (x.PropAccN [ "Object"; "ShouldBeComboxVariable" ]))
        (x.PropAccN [ "Environment"; "AuthUser"; "Username" ])
    |> solve
        [
            "SetDataField", Mono (%1 ^-> %1 ^-> BuiltinTypes.unit)
            "Object", Mono (TProvideMembersWith (Named "ContextObject") [
                "ShouldBeComboxVariable", BuiltinTypes.date
            ])
            "Environment", Mono (TProvideMembersWith (Named "Environment") [ 
                "AuthUser", (TProvideMembersWith (Named "User") [ 
                    "Username", BuiltinTypes.string 
                ])
            ])
        ]
    |> shouldFail

