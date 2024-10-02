
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
        TDef.RecordDefWith (NameHint.Given "One") [
            "sharedProp", BuiltinTypes.number
            "sharedPropDiffType", BuiltinTypes.date
            "oneProp", BuiltinTypes.string
        ]
        TDef.RecordDefWith (NameHint.Given "Two") [
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
let [<Test; Ignore("")>] ``Shared prop on intersection type`` () =

    let x = ExprCtx()

    x.PropAccN [ "oneAndTwo"; "sharedProp" ]
    |> solve env
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    oneAndTwo.oneProp
*)
let [<Test>] ``Disjoint prop on intersection type`` () =

    let x = ExprCtx()

    // 2 Possibilities:
    //    Fail
    //    return an empty set / bottom type
    x.PropAccN [ "oneAndTwo"; "oneProp" ]
    |> solve env
    |> shouldFail




(*
    oneAndTwo.sharedPropDiffType
*)
let [<Test>] ``Shared prop, diff type on intersection type`` () =

    // TODO: Better error message
    
    let x = ExprCtx()

    // 2 possibilities:
    //     Find a common base type for both (in this case: object - if there was one)
    //     Fail when both types are not equal
    x.PropAccN [ "oneAndTwo"; "sharedPropDiffType" ]
    |> solve env
    |> shouldFail
    // |> shouldSolveType (Mono BuiltinTypes.number)


