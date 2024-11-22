
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

    X.App (X.App (X.Var "add") (X.Lit "10")) (X.PropAcc (X.Var "order") "quantity")
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

    let ast =
        X.Let (X.Ident "myRecord") (
            X.MkRecord [
                X.Field "age" (X.Lit "22")
                X.Field "name" (X.Lit "John")
            ]
        ) (X.Var "myRecord")
    
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

    let ast =
        X.Let (X.Ident "myRecord") (
            X.MkRecord [
                X.Field "age" (X.Lit "22")
                X.Field "name" (X.Lit "John")
            ]
        ) (
            X.PropAcc (X.Var "myRecord") "name"
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

    let ast =
        X.Let (X.Ident "myRecord") (
            X.MkRecord [
                X.Field "age" (X.Lit "22")
                X.Field "name" (X.Lit "John")
            ]
        ) (
            X.PropAcc (X.Var "myRecord") "age"
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

    let ast =
        X.Let (X.Ident "myRecord") (
            X.MkRecord [
                X.Field "age" (X.Lit "22")
                X.Field "name" (X.Lit "John")
            ]
        ) (
            X.PropAcc (X.Var "myRecord") "xxxxxxxx"
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

    let ast =
        X.Let
            (X.Ident "r")
            (X.MkRecord [
                X.Field "IntField" (X.Lit "3")
                X.Field "BooleanField" (X.Lit "true")
            ])
            (X.App
                (X.App (X.Var("EQUALS")) (X.PropAcc (X.Var "r") "BooleanField"))
                (X.PropAcc (X.Var "r") "IntField"))
 
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
 
    let left =
        X.App
            (X.App (X.Var("EQUALS")) (X.PropAcc (X.Var "r") "IntField"))
            (X.Lit("3"))
    let right =
        X.App
            (X.App (X.Var("EQUALS")) (X.PropAcc (X.Var "r") "BooleanField"))
            (X.Lit("true"))
    let ast =
        X.Let
            (X.Ident "r")
            (X.MkRecord [
                X.Field "IntField" (X.Lit "3")
                X.Field "BooleanField" (X.Lit "true")
            ])
            (X.App
                (X.App (X.Var "AND") left)
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
 
    let body =
        X.MkArray
            [
                X.MkRecord [
                    X.Field "myfield" (X.Lit "1")
                ]
                X.MkRecord [
                   X.Field
                        "myfield"
                        (X.App
                          (X.Var "plusOne")
                          (X.Lit "1")) 
                ]
            ]
    let plusOneFunc =
        X.Let
            (X.Ident "plusOne")
            (X.Fun (X.Ident "x")
                (X.App
                  (X.App
                    (X.Var "ADD")
                    (X.Var "x"))
                  (X.Lit "1")))
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
 
    let ast =
        X.Let
            (X.Ident "myFunc")
            (X.Fun (X.Ident "r")
                (X.App
                    (X.App
                        (X.Var "AND")
                        (X.App
                            (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "r") "IntField"))
                            (X.Lit "3")))
                    (X.App
                        (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "r") "BooleanField"))
                        (X.Lit "true"))))
            (X.App
                (X.Var "myFunc")
                (X.MkRecord [
                    X.Field "IntField" (X.Lit "3")
                    X.Field "BooleanField" (X.Lit "true")
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
 
    let ast =
        X.Fun (X.Ident "r")
            (X.App
                (X.App
                    (X.Var "EQUALS")
                    (X.PropAcc (X.Var "r") "IntField"))
                (X.Lit "3"))
 
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
 
    let ast =
        X.Fun (X.Ident "r")
            (X.App
                (X.App
                    (X.Var "AND")
                    (X.App
                        (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "r") "IntField"))
                        (X.Lit "3")))
                (X.App
                    (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "r") "BooleanField"))
                    (X.Lit "true")))
 
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

    let ast =
        X.Let
            (X.Ident "myFunc")
            (X.Fun (X.Ident "r")
                (X.App
                    (X.App
                        (X.Var "EQUALS")
                        (X.PropAcc (X.Var "r") "IntField"))
                    (X.PropAcc (X.Var "r") "BooleanField")))
            (X.App
                (X.Var "myFunc")
                (X.MkRecord [
                    X.Field "IntField" (X.Lit "3")
                    X.Field "BooleanField" (X.Lit "true")
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
 
    let ast =
        X.Fun (X.Ident "a")
            (X.App
                (X.App
                    (X.Var "AND")
                    (X.App
                        (X.App (X.Var "EQUALS") (X.Var "a"))
                        (X.Lit "3")))
                (X.App
                    (X.App (X.Var "EQUALS") (X.Var "a"))
                    (X.Lit "true")))
 
    ast
    |> solve defaultTcEnv 
    |> shouldFail



type T = { IntField: int; BooleanField: bool }
let AND a b = a && b
let EQUALS a b = a = b


let test () =
    let myRec = { IntField = 3; BooleanField = true }
    let myFunc = 
        fun r ->
            AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
    myFunc myRec

let test2 () =
    let myRec = { IntField = 3; BooleanField = true }
    let myFunc = 
        fun r ->
            AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
    myFunc myRec


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

    let ast =
        X.Let
            (X.Ident "inst")
            (X.MkRecord [
                X.Field "IntField" (X.Lit "3")
                X.Field "BooleanField" (X.Lit "true")
            ])
            (X.Let
                (X.Ident "myFunc")
                (X.Fun (X.Ident "r")
                    (X.App
                        (X.App
                            (X.Var "AND")
                            (X.App
                                (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "r") "IntField"))
                                (X.Lit "3")))
                            (X.App
                                (X.App (X.Var "EQUALS") (X.PropAcc (X.Var "r") "BooleanField"))
                                (X.Lit "true"))))
                (X.App
                    (X.Var "myFunc")
                    (X.Var "inst")))

    ast
    |> solve defaultTcEnv
    |> shouldSolveType (Mono BuiltinTypes.boolean)

  