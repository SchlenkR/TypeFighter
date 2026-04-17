
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Intersections
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Intersection types
// -----------------------------------------------------------------
// An intersection `R1 & R2` behaves as a value that conforms to
// *both* record shapes. Field access is only allowed when both
// sides agree on the field name and its type.
// =================================================================


let TOneAndTwo =
    IntersectionTyp [
        TDef.RecordDefWith (NameHint.Given "One") [
            "sharedProp", BuiltinTypes.number
            "sharedPropDiffType", BuiltinTypes.boolean
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
    Env:
      oneAndTwo : { sharedProp: Number; sharedPropDiffType: Bool; oneProp: String }
                & { sharedProp: Number; sharedPropDiffType: Number; twoProp: String }

    Source:    oneAndTwo.sharedProp
    Inferred:  Number   (field present in both, same type)
*)
// NOT YET IMPLEMENTED: intersection field resolution.
let [<Test; Ignore("")>] ``Shared prop on intersection type`` () =
    X.PropAccN [ "oneAndTwo"; "sharedProp" ]
    |> solve env None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Env:
      oneAndTwo : { sharedProp: Number; sharedPropDiffType: Bool; oneProp: String }
                & { sharedProp: Number; sharedPropDiffType: Number; twoProp: String }

    Source:    oneAndTwo.oneProp
    Error:     'oneProp' only exists on one side of the intersection
*)
// Open design question: should this fail, or return a bottom-type?
let [<Test>] ``Disjoint prop on intersection type`` () =
    X.PropAccN [ "oneAndTwo"; "oneProp" ]
    |> solve env None
    |> shouldFail


(*
    Env:
      oneAndTwo : { sharedProp: Number; sharedPropDiffType: Bool; oneProp: String }
                & { sharedProp: Number; sharedPropDiffType: Number; twoProp: String }

    Source:    oneAndTwo.sharedPropDiffType
    Error:     Field present on both sides but with incompatible types (Bool vs Number)
*)
// Open design question: could be widened to a common base type if
// one existed (e.g. `Object`). For now we fail.
let [<Test>] ``Shared prop, diff type on intersection type`` () =
    X.PropAccN [ "oneAndTwo"; "sharedPropDiffType" ]
    |> solve env None
    |> shouldFail
