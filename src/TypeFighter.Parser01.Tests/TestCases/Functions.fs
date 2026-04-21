module TypeFighter.Parser01.Tests.Functions

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


// ---- Function calls --------------------------------------------

[<Test>]
let ``single-argument call`` () =
    """f(42)"""
    |> shouldParseTo (X.App (X.Var "f") (X.Lit 42))

[<Test>]
let ``two-argument call is curried`` () =
    """add(1, 2)"""
    |> shouldParseTo
        (X.App (X.App (X.Var "add") (X.Lit 1)) (X.Lit 2))

[<Test>]
let ``zero-argument call desugars to applying UnitValue`` () =
    """now()"""
    |> shouldParseTo (X.App (X.Var "now") (X.Var "UnitValue"))

[<Test>]
let ``call followed by prop access`` () =
    """f(1).x"""
    |> shouldParseTo
        (X.PropAcc (X.App (X.Var "f") (X.Lit 1)) "x")


// ---- Arrow functions -------------------------------------------

[<Test>]
let ``single-param arrow`` () =
    """x => x"""
    |> shouldParseTo (X.Fun (X.Ident "x") (X.Var "x"))

[<Test>]
let ``multi-param arrow curries`` () =
    """(x, y) => x"""
    |> shouldParseTo
        (X.Fun (X.Ident "x") (X.Fun (X.Ident "y") (X.Var "x")))
