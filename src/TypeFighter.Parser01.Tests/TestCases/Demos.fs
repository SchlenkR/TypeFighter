module TypeFighter.Parser01.Tests.Demos

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


// ---- Composite tests -------------------------------------------

[<Test>]
let ``let bound identity applied`` () =
    let source = """
        let id = x => x;
        id(42)
    """
    source
    |> shouldParseTo
        (X.Let (X.Ident "id")
            (X.Fun (X.Ident "x") (X.Var "x"))
            (X.App (X.Var "id") (X.Lit 42)))

[<Test>]
let ``record creation and property access round trip`` () =
    let source = """
        let p = { age: 22, name: "John" };
        p.name
    """
    source
    |> shouldParseTo
        (X.Let (X.Ident "p")
            (X.MkRecord
                [
                    X.Field "age"  (X.Lit 22)
                    X.Field "name" (X.Lit "John")
                ])
            (X.PropAcc (X.Var "p") "name"))


// ---- Demo programs ---------------------------------------------
//
// These only check that a realistic JS-like program parses
// successfully. They are intentionally not asserting the exact AST
// so we can iterate on parser details without churn here.

[<Test>]
let ``demo - nested record literal`` () =
    let source = """
        {
            user: { id: 1, name: "Ada" },
            active: true
        }
    """
    shouldParseSuccessfully source

[<Test>]
let ``demo - array of records`` () =
    let source = """
        [
            { x: 1, y: 2 },
            { x: 3, y: 4 },
            { x: 5, y: 6 }
        ]
    """
    shouldParseSuccessfully source

[<Test>]
let ``demo - currying via call chain`` () =
    let source = """add(1)(2)(3)"""
    shouldParseSuccessfully source

[<Test>]
let ``demo - method-like chaining via PropAcc and call`` () =
    let source = """obj.handler.run(42)"""
    shouldParseSuccessfully source

[<Test>]
let ``demo - small pipeline-style program`` () =
    let source = """
        let data = [1, 2, 3];
        let mapper = x => x;
        map(mapper, data)
    """
    shouldParseSuccessfully source

[<Test>]
let ``demo - configuration-style nested object`` () =
    let source = """
        let config = {
            server: { host: "localhost", port: 8080 },
            retries: 3,
            debug: true
        };
        config.server.port
    """
    shouldParseSuccessfully source

[<Test>]
let ``demo - mixed statements with sequencing`` () =
    let source = """
        log("starting");
        let x = compute(1, 2);
        log("done");
        x
    """
    shouldParseSuccessfully source

[<Test>]
let ``demo - higher-order function definition`` () =
    let source = """
        let compose = (f, g) => x => f(g(x));
        compose(double, inc)(1)
    """
    shouldParseSuccessfully source

[<Test>]
let ``demo - immediately invoked arrow`` () =
    let source = """(x => x)(42)"""
    shouldParseSuccessfully source
