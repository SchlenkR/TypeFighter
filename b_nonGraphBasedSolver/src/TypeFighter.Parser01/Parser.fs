module TODO

()
// module TypeFighter.Parser01

// open TypeFighter
// open TypeFighter.Lang
// open FParsec

// // Helper parser for whitespace
// let ws = spaces
// let ws1 = spaces1

// // Parser for identifiers
// let identParser: Parser<string, unit> =
//     let isIdentifierFirstChar c = isLetter c || c = '_'
//     let isIdentifierChar c = isLetter c || isDigit c || c = '_'
//     many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws

// // Parsers for literals
// let integerLiteral: Parser<Expr, unit> =
//     pint32 .>> ws |>> fun n -> Expr.Lit {| value = string n; tvar = 0 |}

// let stringLiteral: Parser<Expr, unit> =
//     between (pchar '"') (pchar '"') (manyChars (noneOf "\"")) .>> ws |>> fun s ->
//         Lit {| value = s; tvar = 0 |}

// let literal: Parser<Expr, unit> =
//     attempt stringLiteral <|> integerLiteral

// // Parser for variables
// let variable: Parser<Expr, unit> =
//     identParser |>> fun id -> Var {| ident = id; tvar = 0 |}

// // Forward declarations for recursive parsers
// let exprParser, exprParserRef = createParserForwardedToRef<Expr, unit>()

// // Parser for function definitions
// let functionDef: Parser<Expr, unit> =
//     pipe3
//         (pstring "fun" >>. ws1 >>. identParser)
//         (ws >>. pstring "->" >>. ws >>. exprParser)
//         (preturn 0)
//         (fun id body tvar -> Fun {| ident = id; body = body; tvar = tvar |})

// // Parser for let bindings
// let letParser: Parser<Expr, unit> =
//     pipe4
//         (pstring "let" >>. ws1 >>. identParser)
//         (ws >>. pchar '=' >>. ws >>. exprParser)
//         (ws >>. pstring "in" >>. ws >>. exprParser)
//         (preturn 0)
//         (fun id value body tvar -> Let {| ident = id; value = value; body = body; tvar = tvar |})

// // Parser for do expressions
// let doParser: Parser<Expr, unit> =
//     pipe3
//         (pstring "do" >>. ws1 >>. exprParser)
//         (ws1 >>. exprParser)
//         (preturn 0)
//         (fun action body tvar -> Do {| action = action; body = body; tvar = tvar |})

// // Parser for match expressions
// let caseParser: Parser<UnionCase, unit> =
//     pipe2
//         (pchar '|' >>. ws >>. identParser)
//         (ws >>. pstring "->" >>. ws >>. exprParser)
//         (fun pat expr -> Case(pat, expr))

// let matchParser: Parser<Expr, unit> =
//     pipe3
//         (pstring "match" >>. ws1 >>. exprParser)
//         (ws >>. pstring "with" >>. ws >>. many1 caseParser)
//         (preturn 0)
//         (fun expr cases tvar -> Match {| expr = expr; cases = cases; tvar = tvar |})

// // Parser for record construction
// let fieldParser: Parser<Field, unit> =
//     pipe2
//         (identParser)
//         (ws >>. pchar '=' >>. ws >>. exprParser)
//         (fun name expr -> Field(name, expr))

// let recordParser: Parser<Expr, unit> =
//     between
//         (pchar '{' >>. ws)
//         (ws >>. pchar '}')
//         (sepBy fieldParser (pchar ';' >>. ws))
//     |>> fun fields -> MkRecord {| fields = fields; tvar = 0 |}

// // Parser for array construction
// let arrayParser: Parser<Expr, unit> =
//     between
//         (pchar '[' >>. ws)
//         (ws >>. pchar ']')
//         (sepBy exprParser (pchar ';' >>. ws))
//     |>> fun exprs -> MkArray {| values = exprs; tvar = 0 |}

// // Parser for atoms (basic expressions)
// let atom: Parser<Expr, unit> =
//     choice [
//         literal
//         variable
//         recordParser
//         arrayParser
//         between (pchar '(' >>. ws) (ws >>. pchar ')') exprParser
//     ]

// // Parser for property access (e.g., source.ident)
// let propAccParser: Parser<Expr -> Expr, unit> =
//     pchar '.' >>. identParser |>> fun ident source ->
//         PropAcc {| source = source; ident = ident; tvar = 0 |}

// // Parser for postfix operators (e.g., property access)
// let postfixOperatorParser: Parser<Expr -> Expr, unit> =
//     choice [
//         propAccParser
//     ]

// let postfixExpr: Parser<Expr, unit> =
//     let rec loop acc =
//         (attempt postfixOperatorParser >>= fun f -> loop (f acc))
//         <|> preturn acc
//     atom >>= loop

// // Parser for function application
// let applicationExpr: Parser<Expr, unit> =
//     let rec loop acc =
//         (postfixExpr .>> ws >>= fun arg ->
//             loop (App {| func = acc; arg = arg; tvar = 0 |})
//         )
//         <|> preturn acc
//     postfixExpr .>> ws >>= loop

// // Define the main expression parser
// do exprParserRef :=
//     choice [
//         functionDef
//         letParser
//         doParser
//         matchParser
//         applicationExpr
//     ]

// // Example usage:
// // let parseExpression input = run exprParser input