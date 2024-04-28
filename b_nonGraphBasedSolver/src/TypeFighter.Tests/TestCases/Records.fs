
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Records
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



let envWithAdd = 
    [
        BuiltinValues.unitValueIdent, Mono BuiltinTypes.unit
        "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
    ]





(*
    add 10 order.quantity
*)
let [<Test>] ``app with property access`` () =

    let x = ExprCtx()
    
    x.App (x.App (x.Var "add") (x.Lit "10")) (x.PropAcc (x.Var "order") "quantity")
    |> solve
        [
            yield! envWithAdd
            yield "order", Mono (TProvideMembersWith (NameHint.Given "Order") [ "quantity", BuiltinTypes.number ])
        ]
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    let myRecord = 
        { 
            age = 22
            name = "John"
        }
    myRecord
*)
let [<Test>] ``let bound record`` () =

    let x = ExprCtx()

    x.Let (x.Ident "myRecord") (
        x.MkRecord [
            x.Field "age" (x.Lit "22")
            x.Field "name" (x.Lit "John")
        ]
    ) (x.Var "myRecord")
    |> solve []
    |> shouldSolveType (
            Mono (TProvideMembersWith (NameHint.Given "Person") [
                "age", BuiltinTypes.number
                "name", BuiltinTypes.string
            ]))






(*
    let myRecord = 
        { 
            age = 22
            name = "John"
        }
    myRecord.name
*)
let [<Test>] ``let bound record and access prop`` () =

    let x = ExprCtx()

    x.Let (x.Ident "myRecord") (
        x.MkRecord [
            x.Field "age" (x.Lit "22")
            x.Field "name" (x.Lit "John")
        ]
    ) (
        x.PropAcc (x.Var "myRecord") "name"
    )
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.string)






(*
    let myRecord = 
        { 
            age = 22
            name = "John"
        }
    myRecord.age
*)
let [<Test>] ``let bound record and access prop 2`` () =

    let x = ExprCtx()

    x.Let (x.Ident "myRecord") (
        x.MkRecord [
            x.Field "age" (x.Lit "22")
            x.Field "name" (x.Lit "John")
        ]
    ) (
        x.PropAcc (x.Var "myRecord") "age"
    )
    |> solve []
    |> shouldSolveType (Mono BuiltinTypes.number)




(*
    let myRecord = 
        { 
            age = 22
            name = "John"
        }
    myRecord.xxxxxxxx
*)
// ERROR: Member 'xxxxxxxx' is missing in type { age: Number; name: String }
let [<Test>] ``error - let bound record and access non existing prop`` () =

    let x = ExprCtx()
    
    x.Let (x.Ident "myRecord") (
        x.MkRecord [
            x.Field "age" (x.Lit "22")
            x.Field "name" (x.Lit "John")
        ]
    ) (
        x.PropAcc (x.Var "myRecord") "xxxxxxxx"
    )
    |> solve []
    |> shouldFail

