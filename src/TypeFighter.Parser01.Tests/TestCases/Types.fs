module TypeFighter.Parser01.Tests.Types

open NUnit.Framework

open TypeFighter
open TypeFighter.Parser01.Tests.TestHelper


// =================================================================
// Type-expression parser
// -----------------------------------------------------------------
// Grammar (low → high precedence):
//
//   typExpr = altTyp { "|" altTyp }
//   altTyp  = primTyp { "&" primTyp }          (Step 3)
//   primTyp = literalTyp | identTyp | applied | "(" typExpr ")" | "{…}" (Step 3)
//
// Step 2 covers literal types, identifiers (incl. built-ins),
// applied types, parens, and `|`. `&` / `{…}` land in Step 3.
// =================================================================


// ---- Literal types ----

[<Test>]
let ``literal type - integer`` () =
    "42" |> shouldParseTypTo (LiteralTyp (Number 42.0))

[<Test>]
let ``literal type - string`` () =
    """ "hello" """ |> shouldParseTypTo (LiteralTyp (String "hello"))

[<Test>]
let ``literal type - true`` () =
    "true" |> shouldParseTypTo (LiteralTyp (Boolean true))

[<Test>]
let ``literal type - false`` () =
    "false" |> shouldParseTypTo (LiteralTyp (Boolean false))


// ---- Identifier / built-in types ----

[<Test>]
let ``built-in - Number`` () =
    "Number" |> shouldParseTypTo BuiltinTypes.number

[<Test>]
let ``built-in - String`` () =
    "String" |> shouldParseTypTo BuiltinTypes.string

[<Test>]
let ``built-in - Bool`` () =
    "Bool" |> shouldParseTypTo BuiltinTypes.boolean

[<Test>]
let ``user-defined type - zero args`` () =
    "MyThing" |> shouldParseTypTo (TDef.SaturatedWith "MyThing" [])


// ---- Applied types ----

[<Test>]
let ``applied - Array of Number`` () =
    "Array<Number>"
    |> shouldParseTypTo (BuiltinTypes.array BuiltinTypes.number)

[<Test>]
let ``applied - nested`` () =
    "Array<Array<String>>"
    |> shouldParseTypTo
        (BuiltinTypes.array (BuiltinTypes.array BuiltinTypes.string))

[<Test>]
let ``applied - multiple args`` () =
    "Map<String, Number>"
    |> shouldParseTypTo
        (TDef.SaturatedWith "Map" [ BuiltinTypes.string; BuiltinTypes.number ])


// ---- Union (`|`) ----

[<Test>]
let ``union of two literals`` () =
    "0 | 1"
    |> shouldParseTypTo
        (UnionTyp (set [ LiteralTyp (Number 0.0); LiteralTyp (Number 1.0) ]))

[<Test>]
let ``union of three literals`` () =
    """ "red" | "yellow" | "green" """
    |> shouldParseTypTo
        (UnionTyp (set [
            LiteralTyp (String "red")
            LiteralTyp (String "yellow")
            LiteralTyp (String "green") ]))

[<Test>]
let ``true | false reconstructs Bool`` () =
    "true | false" |> shouldParseTypTo BuiltinTypes.boolean

[<Test>]
let ``union with built-in types`` () =
    "Number | String"
    |> shouldParseTypTo
        (UnionTyp (set [ BuiltinTypes.number; BuiltinTypes.string ]))

[<Test>]
let ``union flattens nested unions`` () =
    // `A | (B | C)` and `A | B | C` should both produce the same flat union
    "1 | (2 | 3)"
    |> shouldParseTypTo
        (UnionTyp (set [
            LiteralTyp (Number 1.0)
            LiteralTyp (Number 2.0)
            LiteralTyp (Number 3.0) ]))

[<Test>]
let ``union collapses duplicates (set semantics)`` () =
    "1 | 1"
    |> shouldParseTypTo (LiteralTyp (Number 1.0))


// ---- Parens ----

[<Test>]
let ``parens preserve nesting`` () =
    "(Number)" |> shouldParseTypTo BuiltinTypes.number


// ---- Errors ----

[<Test>]
let ``error - trailing pipe`` () =
    "Number |" |> shouldFailToParseTyp

[<Test>]
let ``error - leading pipe`` () =
    "| Number" |> shouldFailToParseTyp

[<Test>]
let ``error - empty input`` () =
    "" |> shouldFailToParseTyp
