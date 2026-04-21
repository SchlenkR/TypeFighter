
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Emit
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// JS emitter
// -----------------------------------------------------------------
// Golden tests for TypeFighter → JavaScript string emission. The
// emitter doesn't depend on the solver — it's a pure AST walker —
// so these tests construct expressions directly via the X.* DSL.
// =================================================================


let private jsOf expr = Emit.toJs expr

let [<Test>] ``literal number`` () =
    jsOf (X.Lit 42) |> shouldEqual "42"

let [<Test>] ``literal float`` () =
    jsOf (X.Lit 3.5) |> shouldEqual "3.5"

let [<Test>] ``literal string escapes quotes and backslashes`` () =
    jsOf (X.Lit "he said \"hi\"\\n") |> shouldEqual "\"he said \\\"hi\\\"\\\\n\""

let [<Test>] ``literal string with real newline escaped`` () =
    jsOf (X.Lit "line1\nline2") |> shouldEqual "\"line1\\nline2\""

let [<Test>] ``literal booleans`` () =
    jsOf (X.Lit true) |> shouldEqual "true"
    jsOf (X.Lit false) |> shouldEqual "false"

let [<Test>] ``variable identity`` () =
    jsOf (X.Var "greet") |> shouldEqual "greet"

let [<Test>] ``curried application`` () =
    let expr = X.App (X.App (X.Var "add") (X.Lit 1)) (X.Lit 2)
    jsOf expr |> shouldEqual "add(1)(2)"

let [<Test>] ``lambda single arg`` () =
    let expr = X.Fun (X.Ident "x") (X.Var "x")
    jsOf expr |> shouldEqual "(x) => x"

let [<Test>] ``lambda application needs paren on callee`` () =
    let expr = X.App (X.Fun (X.Ident "x") (X.Var "x")) (X.Lit 5)
    jsOf expr |> shouldEqual "((x) => x)(5)"

let [<Test>] ``let binds as IIFE`` () =
    let expr = X.Let (X.Ident "x") (X.Lit 5) (X.Var "x")
    jsOf expr |> shouldEqual "((x) => x)(5)"

let [<Test>] ``property access`` () =
    let expr = X.PropAcc (X.Var "r") "name"
    jsOf expr |> shouldEqual "r.name"

let [<Test>] ``record with named and positional items`` () =
    let expr =
        X.MkRecord [
            X.Property "age" (X.Lit 22)
            X.Positional (X.Lit "Tag")
            X.Property "name" (X.Lit "John")
        ]
    jsOf expr |> shouldEqual "{age: 22, _0: \"Tag\", name: \"John\"}"

let [<Test>] ``array literal`` () =
    let expr = X.MkArray [ X.Lit 1; X.Lit 2; X.Lit 3 ]
    jsOf expr |> shouldEqual "[1, 2, 3]"

let [<Test>] ``match with literal arms and wildcard`` () =
    let expr =
        X.Fun (X.Ident "n") (
            X.Match (X.Var "n") [
                X.Arm (X.PatLit 0) (X.Lit "zero")
                X.Arm (X.PatLit 1) (X.Lit "one")
                X.Arm X.PatWild (X.Lit "other")
            ])
    let expected =
        "(n) => ((__s) => { if (__s === 0) return \"zero\"; "
        + "if (__s === 1) return \"one\"; return \"other\"; })(n)"
    jsOf expr |> shouldEqual expected

let [<Test>] ``match with var pattern binds scrutinee`` () =
    let expr =
        X.Match (X.Lit 42) [
            X.Arm (X.PatVar "x") (X.Var "x")
        ]
    jsOf expr |> shouldEqual "((__s) => { return ((x) => x)(__s); })(42)"

let [<Test>] ``do action body uses comma operator`` () =
    let expr = X.Do (X.App (X.Var "log") (X.Lit "side")) (X.Lit 7)
    jsOf expr |> shouldEqual "(log(\"side\"), 7)"

let [<Test>] ``toJsModule wires imports and exports default`` () =
    let expr = X.App (X.Var "log") (X.Lit "hi")
    let result =
        Emit.toJsModule
            [ "log", "(s) => console.log(s)" ]
            expr
    let expected = "const log = (s) => console.log(s);\nexport default log(\"hi\");\n"
    result |> shouldEqual expected
