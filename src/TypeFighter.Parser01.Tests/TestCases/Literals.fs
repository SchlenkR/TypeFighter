module TypeFighter.Parser01.Tests.Literals

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


[<Test>]
let ``number literal`` () =
    """42""" |> shouldParseTo (X.Lit 42)

[<Test>]
let ``float literal`` () =
    """3.14""" |> shouldParseTo (X.Lit 3.14)

[<Test>]
let ``float literal with leading zero`` () =
    """0.5""" |> shouldParseTo (X.Lit 0.5)

[<Test>]
let ``string literal`` () =
    """ "hello" """ |> shouldParseTo (X.Lit "hello")

[<Test>]
let ``true literal`` () =
    """true""" |> shouldParseTo (X.Lit true)

[<Test>]
let ``false literal`` () =
    """false""" |> shouldParseTo (X.Lit false)
