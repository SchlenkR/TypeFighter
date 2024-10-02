
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
            yield "order", Mono (TDef.NamedRecordWith (NameHint.Given "Order") [ "quantity", BuiltinTypes.number ])
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

    let ast =
        x.Let (x.Ident "myRecord") (
            x.MkRecord [
                x.Field "age" (x.Lit "22")
                x.Field "name" (x.Lit "John")
            ]
        ) (x.Var "myRecord")
    
    ast
    |> solve []
    |> shouldSolveType (
            Mono (TDef.NamedRecordWith (NameHint.Given "Person") [
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

    let ast =
        x.Let (x.Ident "myRecord") (
            x.MkRecord [
                x.Field "age" (x.Lit "22")
                x.Field "name" (x.Lit "John")
            ]
        ) (
            x.PropAcc (x.Var "myRecord") "name"
        )
    
    ast
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

    let ast =
        x.Let (x.Ident "myRecord") (
            x.MkRecord [
                x.Field "age" (x.Lit "22")
                x.Field "name" (x.Lit "John")
            ]
        ) (
            x.PropAcc (x.Var "myRecord") "age"
        )
    
    ast
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
    
    let ast =
        x.Let (x.Ident "myRecord") (
            x.MkRecord [
                x.Field "age" (x.Lit "22")
                x.Field "name" (x.Lit "John")
            ]
        ) (
            x.PropAcc (x.Var "myRecord") "xxxxxxxx"
        )
    
    ast
    |> solve []
    |> shouldFail




(*
    let r = { IntField = 3, BooleanField = true }
    r.BooleanField = r.IntField
*)
let [<Test>] ``comparing two ununifiable record fields from one record should fail`` () =

    let defaultTcEnv =
        [
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()

    let ast =
        x.Let
            (x.Ident "r")
            (x.MkRecord [
                x.Field "IntField" (x.Lit "3")
                x.Field "BooleanField" (x.Lit "true")
            ])
            (x.App
                (x.App (x.Var("EQUALS")) (x.PropAcc (x.Var "r") "BooleanField"))
                (x.PropAcc (x.Var "r") "IntField"))
 
    ast
    |> solve defaultTcEnv 
    |> shouldFail




(*
    let r = { IntField = 3, BooleanField = true }
    AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
*)
let [<Test>] ``accessing two record fields in boolean expression should solve`` () =

    let defaultTcEnv =
        [
            "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()
 
    let left =
        x.App
            (x.App (x.Var("EQUALS")) (x.PropAcc (x.Var "r") "IntField"))
            (x.Lit("3"))
    let right =
        x.App
            (x.App (x.Var("EQUALS")) (x.PropAcc (x.Var "r") "BooleanField"))
            (x.Lit("true"))
    let ast =
        x.Let
            (x.Ident "r")
            (x.MkRecord [
                x.Field "IntField" (x.Lit "3")
                x.Field "BooleanField" (x.Lit "true")
            ])
            (x.App
                (x.App (x.Var "AND") left)
                right)

    solve defaultTcEnv ast 
    |> shouldSolveType (Mono(BuiltinTypes.boolean))



(*
    let plusOne x = x + 1
    [
        { myfield = 1 };
        { myfield = plusOne 1 }
    ]
*)

let [<Test>] ``array with multiple record elements should solve`` () =

    let defaultTcEnv =
        [
            "ADD", TDef.Generalize (%1 ^-> %1 ^-> %1)
        ]
 
    let x = ExprCtx()
    let body =
        x.MkArray
            [
                x.MkRecord [
                    x.Field "myfield" (x.Lit "1")
                ]
                x.MkRecord [
                   x.Field
                        "myfield"
                        (x.App
                          (x.Var "plusOne")
                          (x.Lit "1")) 
                ]
            ]
    let plusOneFunc =
        x.Let
            (x.Ident "plusOne")
            (x.Fun (x.Ident "x")
                (x.App
                  (x.App
                    (x.Var "ADD")
                    (x.Var "x"))
                  (x.Lit "1")))
            body
 
    let ast = plusOneFunc
 
    solve defaultTcEnv ast
    |> shouldSolveType (Mono (
        BuiltinTypes.array (TDef.RecordWith [ "myfield", BuiltinTypes.number ])))



(*
    let myFunc =
        fun r ->
            AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
    myFunc { IntField = 3, BooleanField = true }
*)
let [<Test>] ``function with record argument should solve`` () =

    let defaultTcEnv =
        [
            "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()
 
    let ast =
        x.Let
            (x.Ident "myFunc")
            (x.Fun (x.Ident "r")
                (x.App
                    (x.App
                        (x.Var "AND")
                        (x.App
                            (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "IntField"))
                            (x.Lit "3")))
                    (x.App
                        (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "BooleanField"))
                        (x.Lit "true"))))
            (x.App
                (x.Var "myFunc")
                (x.MkRecord [
                    x.Field "IntField" (x.Lit "3")
                    x.Field "BooleanField" (x.Lit "true")
                ]))

    solve defaultTcEnv ast
    |> shouldSolveType (Mono BuiltinTypes.boolean)




(*
    fun r -> EQUALS r.IntField 3
*)
let [<Test>] ``lambda taking record infers correct type`` () =

    let defaultTcEnv =
        [
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()
 
    let ast =
        x.Fun (x.Ident "r")
            (x.App
                (x.App
                    (x.Var "EQUALS")
                    (x.PropAcc (x.Var "r") "IntField"))
                (x.Lit "3"))
 
    ast 
    |> solve defaultTcEnv
    |> shouldSolveType (
        Mono (TDef.RecordWith [ "IntField", BuiltinTypes.number ] ^-> BuiltinTypes.boolean))





(*
    fun r ->
        AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
*)
let [<Test>] ``anonymous function taking record solves on correct field usage`` () =

    let defaultTcEnv =
        [
            "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()
 
    let ast =
        x.Fun (x.Ident "r")
            (x.App
                (x.App
                    (x.Var "AND")
                    (x.App
                        (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "IntField"))
                        (x.Lit "3")))
                (x.App
                    (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "BooleanField"))
                    (x.Lit "true")))
 
    ast 
    |> solve defaultTcEnv 
    |> shouldSolveType (
        Mono (
            (
                TDef.RecordWith [
                    "IntField", BuiltinTypes.number
                    "BooleanField", BuiltinTypes.boolean
                ]
            )
            ^-> BuiltinTypes.boolean
        ))








(*
    let myFunc =
        fun r -> EQUALS r.IntField r.BooleanField
    myFunc { IntField = 3, BooleanField = true }
*)
let [<Test>] ``function with record argument comparing 2 different typed fields should fail`` () =

    let defaultTcEnv =
        [
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()

    let ast =
        x.Let
            (x.Ident "myFunc")
            (x.Fun (x.Ident "r")
                (x.App
                    (x.App
                        (x.Var "EQUALS")
                        (x.PropAcc (x.Var "r") "IntField"))
                    (x.PropAcc (x.Var "r") "BooleanField")))
            (x.App
                (x.Var "myFunc")
                (x.MkRecord [
                    x.Field "IntField" (x.Lit "3")
                    x.Field "BooleanField" (x.Lit "true")
                ]))

    ast
    |> solve defaultTcEnv 
    |> shouldFail




(*
    fun a ->
        AND (EQUALS a 3) (EQUALS a true)
*)
let [<Test>] ``xxxxxxxx`` () =

    let defaultTcEnv =
        [
            "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()
 
    let ast =
        x.Fun (x.Ident "a")
            (x.App
                (x.App
                    (x.Var "AND")
                    (x.App
                        (x.App (x.Var "EQUALS") (x.Var "a"))
                        (x.Lit "3")))
                (x.App
                    (x.App (x.Var "EQUALS") (x.Var "a"))
                    (x.Lit "true")))
 
    ast
    |> solve defaultTcEnv 
    |> shouldFail





(*
    let inst = { IntField = 3, BooleanField = true }
    let myFunc = 
        fun r ->
            AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
    myFunc inst
*)
let [<Test>] ``function with record argument comparing 2 different typed fields should solve`` () =

    let defaultTcEnv =
        [
            "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
        ]
    let x = ExprCtx()

    let ast =
        x.Let
            (x.Ident "inst")
            (x.MkRecord [
                x.Field "IntField" (x.Lit "3")
                x.Field "BooleanField" (x.Lit "true")
            ])
            (x.Let
                (x.Ident "myFunc")
                (x.Fun (x.Ident "r")
                    (x.App
                        (x.App
                            (x.Var "AND")
                            (x.App
                                (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "IntField"))
                                (x.Lit "3")))
                            (x.App
                                (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "BooleanField"))
                                (x.Lit "true"))))
                (x.App
                    (x.Var "myFunc")
                    (x.Var "inst")))

    ast
    |> solve defaultTcEnv
    |> shouldSolveType (Mono BuiltinTypes.boolean)

  