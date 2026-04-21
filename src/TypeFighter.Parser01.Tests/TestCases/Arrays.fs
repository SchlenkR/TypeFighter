module TypeFighter.Parser01.Tests.Arrays

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


[<Test>]
let ``empty array`` () =
    """[]""" |> shouldParseTo (X.MkArray [])

[<Test>]
let ``array of numbers`` () =
    """[1, 2, 3]"""
    |> shouldParseTo (X.MkArray [ X.Lit 1; X.Lit 2; X.Lit 3 ])

[<Test>]
let ``array mixing literal types parses fine (types are checked later)`` () =
    """[1, "a", true]"""
    |> shouldParseTo (X.MkArray [ X.Lit 1; X.Lit "a"; X.Lit true ])
