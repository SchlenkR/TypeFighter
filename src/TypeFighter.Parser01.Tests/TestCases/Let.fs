module TypeFighter.Parser01.Tests.Let

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


// ---- Let bindings ----------------------------------------------

[<Test>]
let ``let binding`` () =
    """let x = 1; x"""
    |> shouldParseTo
        (X.Let (X.Ident "x") (X.Lit 1) (X.Var "x"))

[<Test>]
let ``const binding is equivalent to let`` () =
    """const x = 1; x"""
    |> shouldParseTo
        (X.Let (X.Ident "x") (X.Lit 1) (X.Var "x"))

[<Test>]
let ``two nested lets`` () =
    let source = """
        let x = 1;
        let y = 2;
        x
    """
    source
    |> shouldParseTo
        (X.Let (X.Ident "x") (X.Lit 1)
            (X.Let (X.Ident "y") (X.Lit 2)
                (X.Var "x")))

[<Test>]
let ``expression statement becomes a Do`` () =
    """f(1); x"""
    |> shouldParseTo
        (X.Do
            (X.App (X.Var "f") (X.Lit 1))
            (X.Var "x"))

// ---- Trailing semicolons ---------------------------------------
// A trailing `;` after the final expression should be tolerated —
// it's an ergonomic expectation coming from C-family languages.

[<Test>]
let ``trailing semicolon after final expression`` () =
    """42;""" |> shouldParseTo (X.Lit 42)

[<Test>]
let ``trailing semicolon after let block`` () =
    """let x = 1; x;"""
    |> shouldParseTo
        (X.Let (X.Ident "x") (X.Lit 1) (X.Var "x"))

[<Test>]
let ``multiple trailing semicolons`` () =
    """42;;;""" |> shouldParseTo (X.Lit 42)
