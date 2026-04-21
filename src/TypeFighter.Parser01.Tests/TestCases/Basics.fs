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

// ---- Line comments ---------------------------------------------
// `// …` to end-of-line is skipped like whitespace — so comments can
// appear anywhere a space could.

[<Test>]
let ``line comment at start of source`` () =
    "// the answer\n42" |> shouldParseTo (X.Lit 42)

[<Test>]
let ``line comment after expression`` () =
    "42 // trailing" |> shouldParseTo (X.Lit 42)

[<Test>]
let ``line comment without trailing newline at eof`` () =
    "42 // no newline after this" |> shouldParseTo (X.Lit 42)

[<Test>]
let ``line comment between statements`` () =
    "let x = 1; // bind\nx" |> shouldParseTo
        (X.Let (X.Ident "x") (X.Lit 1) (X.Var "x"))
