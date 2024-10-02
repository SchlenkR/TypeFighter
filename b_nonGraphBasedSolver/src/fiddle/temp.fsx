#load "../_fsiBase.fsx"
open ``_fsiBase``

    
open TypeFighter.Lang
open TypeFighter.Lang.Services



// ------------------------------------------
// Poly
// ------------------------------------------






// ------------------------------------------
// Some cases for tracking source expressions in error cases
// ------------------------------------------

(*
    SetField Object.ShouldBeComboxVariable Environment.AuthUser.Username
*)

do
    let x = ExprCtx()

    x.App
        (x.App (x.Var "SetField") (x.PropAccN [ "Object"; "ShouldBeComboxVariable" ]))
        (x.PropAccN [ "Environment"; "AuthUser"; "Username" ])
    |> solve
        [
            "SetField", Mono (%3 ^-> %3 ^-> BuiltinTypes.unit)
            "Object", Mono (TRecordWith [
                "ShouldBeComboxVariable", BuiltinTypes.date
            ])
            "Environment", Mono (TRecordWith [ 
                "AuthUser", TRecordWith [ 
                    "Username", BuiltinTypes.string 
                ]
            ])
        ]
    |> ignore



// let id = fun x -> x in
//     let res1 = id 100
//     let res2 = id "sdf"
//     ()
