module PureState.TypeFighter.Tests.BuildExprParserTests

open System
open NUnit.Framework
open FsUnit
open TheBlunt

// Toy AST and helpers: we evaluate operator parses to int so the assertions
// stay terse. Each test builds a tiny grammar + table to exercise one aspect
// of buildExprParser.

let private number : Parser<int> =
    parse {
        let! d = many1Str (pchar Char.IsDigit (fun _ -> "digit"))
        return { range = d.range; result = int d.result }
    }

let private opSurf (s: string) : Parser<unit> = pstr s |> map ignore

let private runOk parser input =
    match run input parser with
    | POk pv -> pv.result
    | PError e -> failwithf "expected ok, got error at %d: %s" e.idx e.message

// --- Single level, left-associative -----------------------------------------

[<Test>]
let ``left-assoc: a-b-c parses as (a-b)-c`` () =
    let p =
        buildExprParser opSurf number [
            [ Infix("-", Left, fun a b -> a - b) ]
        ]
    // 10 - 4 - 2 :  (10-4)-2 = 4   vs right-assoc 10-(4-2) = 8
    runOk p "10-4-2" |> should equal 4

[<Test>]
let ``left-assoc: chain of plus folds left-to-right`` () =
    let p =
        buildExprParser opSurf number [
            [ Infix("+", Left, (+)) ]
        ]
    runOk p "1+2+3+4" |> should equal 10

// --- Single level, right-associative ----------------------------------------

[<Test>]
let ``right-assoc: a^b^c parses as a^(b^c)`` () =
    let pow a b = pown a b
    let p =
        buildExprParser opSurf number [
            [ Infix("^", Right, pow) ]
        ]
    // 2 ^ 3 ^ 2 :  2^(3^2) = 2^9 = 512   vs left-assoc (2^3)^2 = 64
    runOk p "2^3^2" |> should equal 512

// --- Mixed precedence -------------------------------------------------------

[<Test>]
let ``mixed precedence: 1+2*3 = 7 (mul binds tighter)`` () =
    let p =
        buildExprParser opSurf number [
            [ Infix("+", Left, (+)) ]
            [ Infix("*", Left, ( * )) ]
        ]
    runOk p "1+2*3" |> should equal 7
    runOk p "2*3+1" |> should equal 7

[<Test>]
let ``mixed precedence: same operator on different levels still respects order`` () =
    let p =
        buildExprParser opSurf number [
            [ Infix("+", Left, (+)) ]
            [ Infix("-", Left, (-)) ]
            [ Infix("*", Left, ( * )) ]
        ]
    // 1 + 2 - 3 * 2  →  1 + (2 - (3*2)) = 1 + -4 = -3
    runOk p "1+2-3*2" |> should equal -3

// --- Surface conflicts ------------------------------------------------------

[<Test>]
let ``surface conflict: '<=' is preferred over '<' on the same level`` () =
    // We use sentinel results so the assertion captures which operator matched.
    let bin name a b = sprintf "(%s %d %d)" name a b
    let p : Parser<string> =
        let toStr = number |> map string
        buildExprParser opSurf toStr [
            [ Infix("<=", Left, fun a b -> bin "<=" (int a) (int b))
              Infix("<",  Left, fun a b -> bin "<" (int a) (int b)) ]
        ]
    runOk p "1<=2" |> should equal "(<= 1 2)"
    runOk p "1<2"  |> should equal "(< 1 2)"

[<Test>]
let ``surface alias: two surfaces map to the same canonical name via combine`` () =
    let bin name a b = sprintf "(%s %d %d)" name a b
    let toStr = number |> map string
    let p =
        buildExprParser opSurf toStr [
            [ Infix("<>", Left, fun a b -> bin "<>" (int a) (int b))
              Infix("!=", Left, fun a b -> bin "<>" (int a) (int b)) ]
        ]
    runOk p "1<>2" |> should equal "(<> 1 2)"
    runOk p "1!=2" |> should equal "(<> 1 2)"

// --- Prefix / postfix unary operators ---------------------------------------

[<Test>]
let ``prefix: --x parses as -(-(x))`` () =
    let p =
        buildExprParser opSurf number [
            [ Prefix("-", fun a -> -a) ]
        ]
    runOk p "--5" |> should equal 5
    runOk p "---5" |> should equal -5

[<Test>]
let ``postfix: x?? applies twice (right-most postfix wraps last)`` () =
    // Encode each `?` application as +10 so we can tell order from arithmetic.
    let p =
        buildExprParser opSurf number [
            [ Postfix("?", fun a -> a + 10) ]
        ]
    runOk p "5"   |> should equal 5
    runOk p "5?"  |> should equal 15
    runOk p "5??" |> should equal 25

[<Test>]
let ``prefix + infix on different levels: -2 * -3 = 6`` () =
    let p =
        buildExprParser opSurf number [
            [ Infix("*", Left, ( * )) ]
            [ Prefix("-", fun a -> -a) ]
        ]
    runOk p "-2*-3" |> should equal 6

// --- Single atom edge case --------------------------------------------------

[<Test>]
let ``empty operator table reduces to the atom parser`` () =
    let p = buildExprParser opSurf number []
    runOk p "42" |> should equal 42

[<Test>]
let ``atom-only input still works with a non-empty table`` () =
    let p =
        buildExprParser opSurf number [
            [ Infix("+", Left, (+)) ]
            [ Infix("*", Left, ( * )) ]
        ]
    runOk p "7" |> should equal 7
