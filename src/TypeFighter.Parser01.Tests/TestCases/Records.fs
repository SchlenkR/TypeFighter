module TypeFighter.Parser01.Tests.Records

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


// ---- Record literals --------------------------------------------

[<Test>]
let ``empty record`` () =
    """()""" |> shouldParseTo (X.MkRecord [])

[<Test>]
let ``record with two fields`` () =
    """( age: 22, name: "John" )"""
    |> shouldParseTo
        (X.MkRecord
            [
                X.Property "age"  (X.Lit 22)
                X.Property "name" (X.Lit "John")
            ])

[<Test>]
let ``single-positional record requires trailing comma`` () =
    """( 42, )"""
    |> shouldParseTo (X.MkRecord [ X.Positional (X.Lit 42) ])

[<Test>]
let ``parens without trailing comma are grouping`` () =
    """( 42 )"""
    |> shouldParseTo (X.Lit 42)


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
