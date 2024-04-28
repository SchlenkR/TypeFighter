
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.IntersectionTypes
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




// TODO: SharedProp with different type

let TOneAndTwo =
    IntersectionTyp [
        TRecordWith (NameHint.Given "One") [
            "sharedProp", BuiltinTypes.number
            "sharedPropDiffType", BuiltinTypes.date
            "oneProp", BuiltinTypes.string
        ]
        TRecordWith (NameHint.Given "Two") [
            "sharedProp", BuiltinTypes.number
            "sharedPropDiffType", BuiltinTypes.number
            "twoProp", BuiltinTypes.string
        ]
    ]

let env = [
    "oneAndTwo", Mono TOneAndTwo
]





(*
    oneAndTwo.sharedProp
*)
let [<Test>] ``Shared prop on intersection type`` () =

    let x = ExprCtx()

    x.PropAccN [ "oneAndTwo"; "sharedProp" ]
    |> solve env
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    oneAndTwo.oneProp
*)
let [<Test>] ``Disjoint prop on intersection type`` () =

    let x = ExprCtx()

    x.PropAccN [ "oneAndTwo"; "oneProp" ]
    |> solve env
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    oneAndTwo.sharedPropDiffType
*)
let [<Test>] ``Shared prop, diff type on intersection type`` () =

    // TODO: Better error message
    
    let x = ExprCtx()

    x.PropAccN [ "oneAndTwo"; "sharedPropDiffType" ]
    |> solve env
    |> shouldSolveType (Mono BuiltinTypes.number)


