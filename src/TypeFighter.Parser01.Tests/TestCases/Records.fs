module TypeFighter.Parser01.Tests.Records

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


// ---- Record literals --------------------------------------------

[<Test>]
let ``empty record`` () =
    """{}""" |> shouldParseTo (X.MkRecord [])

[<Test>]
let ``record with two named fields`` () =
    """{ age: 22, name: "John" }"""
    |> shouldParseTo
        (X.MkRecord
            [
                X.Property "age"  (X.Lit 22)
                X.Property "name" (X.Lit "John")
            ])

[<Test>]
let ``single-positional record`` () =
    """{ 42 }"""
    |> shouldParseTo (X.MkRecord [ X.Positional (X.Lit 42) ])

[<Test>]
let ``parens are grouping`` () =
    """( 42 )"""
    |> shouldParseTo (X.Lit 42)

[<Test>]
let ``empty parens produce unit/empty record`` () =
    """()""" |> shouldParseTo (X.MkRecord [])

[<Test>]
let ``call with named arg sugars to record arg`` () =
    """f(name: "Ada")"""
    |> shouldParseTo
        (X.App (X.Var "f")
            (X.MkRecord [ X.Property "name" (X.Lit "Ada") ]))

[<Test>]
let ``call with record literal as arg`` () =
    """f({name: "Ada"})"""
    |> shouldParseTo
        (X.App (X.Var "f")
            (X.MkRecord [ X.Property "name" (X.Lit "Ada") ]))


// ---- Property access --------------------------------------------

[<Test>]
let ``simple prop access`` () =
    """order.quantity"""
    |> shouldParseTo (X.PropAcc (X.Var "order") "quantity")

[<Test>]
let ``chained prop access`` () =
    """a.b.c"""
    |> shouldParseTo
        (X.PropAcc (X.PropAcc (X.Var "a") "b") "c")
