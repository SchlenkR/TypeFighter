module TypeFighter.Parser01.Tests.Basics

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


[<Test>]
let ``bare identifier`` () =
    """foo""" |> shouldParseTo (X.Var "foo")

[<Test>]
let ``identifier with underscores and digits`` () =
    """foo_bar42""" |> shouldParseTo (X.Var "foo_bar42")

[<Test>]
let ``reserved word cannot be used as identifier`` () =
    shouldFailToParse """let"""

[<Test>]
let ``whitespace is tolerated around tokens`` () =
    """   42   """ |> shouldParseTo (X.Lit 42)

[<Test>]
let ``parenthesized expression is transparent`` () =
    """(42)""" |> shouldParseTo (X.Lit 42)
