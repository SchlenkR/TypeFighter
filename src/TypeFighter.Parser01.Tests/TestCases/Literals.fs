module TypeFighter.Parser01.Tests.Literals

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


[<Test>]
let ``number literal`` () =
    """42""" |> shouldParseTo (X.Lit 42)

[<Test>]
let ``string literal`` () =
    """ "hello" """ |> shouldParseTo (X.Lit "hello")

[<Test>]
let ``true literal`` () =
    """true""" |> shouldParseTo (X.Lit true)

[<Test>]
let ``false literal`` () =
    """false""" |> shouldParseTo (X.Lit false)
