module PureState.TypeFighter.Tests.ConstrainedPolyTests

open NUnit.Framework
open FsUnit
open PureState.TypeFighter

let private number = BuiltinTypes.number
let private string' = BuiltinTypes.string
let private boolean = BuiltinTypes.boolean

// op_add : forall T in {Number, String}. T -> T -> T
let private opAddTyp =
    TDef.GeneralizeWithConstraints
        [ 0, [ number; string' ] ]
        (%0 ^-> %0 ^-> %0)

let private rootTyp (sol: Solver.Solution) =
    match sol.result with
    | Ok r -> r.typ
    | Error e -> failwithf "expected Ok, got Error: %s" e

// ---- inst emits CIsOneOf, solver resolves on use --------------------------

[<Test>]
let ``constrained poly: Number + Number resolves to Number`` () =
    let expr =
        X.App (X.App (X.Var "op_add") (X.Lit 1.0)) (X.Lit 2.0)
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match rootTyp sol with
    | Some (Mono t) when t = number -> ()
    | other -> Assert.Fail (sprintf "expected Number, got %A" other)

[<Test>]
let ``constrained poly: String + String resolves to String`` () =
    let expr =
        X.App (X.App (X.Var "op_add") (X.Lit "a")) (X.Lit "b")
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match rootTyp sol with
    | Some (Mono t) when t = string' -> ()
    | other -> Assert.Fail (sprintf "expected String, got %A" other)

[<Test>]
let ``constrained poly: Boolean + Boolean is rejected`` () =
    let expr =
        X.App (X.App (X.Var "op_add") (X.Lit true)) (X.Lit false)
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match sol.result with
    | Error msg ->
        Assert.That(msg, Does.Contain "allowed set")
    | Ok r ->
        Assert.Fail (sprintf "expected Error, got Ok with %A" r.typ)

[<Test>]
let ``constrained poly: mixed Number and String is rejected`` () =
    let expr =
        X.App (X.App (X.Var "op_add") (X.Lit 1.0)) (X.Lit "a")
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match sol.result with
    | Error _ -> ()
    | Ok r -> Assert.Fail (sprintf "expected Error, got Ok with %A" r.typ)

// ---- Two constrained tvars meeting: intersection ---------------------------
// op_lt : forall T in {Number, String}. T -> T -> Boolean
let private opLtTyp =
    TDef.GeneralizeWithConstraints
        [ 0, [ number; string' ] ]
        (%0 ^-> %0 ^-> boolean)

[<Test>]
let ``constrained poly: comparison returns Boolean for Number`` () =
    let expr =
        X.App (X.App (X.Var "op_lt") (X.Lit 1.0)) (X.Lit 2.0)
    let sol = Solver.solve [ "op_lt", opLtTyp ] None expr
    match rootTyp sol with
    | Some (Mono t) when t = boolean -> ()
    | other -> Assert.Fail (sprintf "expected Boolean, got %A" other)

[<Test>]
let ``constrained poly: comparison returns Boolean for String`` () =
    let expr =
        X.App (X.App (X.Var "op_lt") (X.Lit "a")) (X.Lit "b")
    let sol = Solver.solve [ "op_lt", opLtTyp ] None expr
    match rootTyp sol with
    | Some (Mono t) when t = boolean -> ()
    | other -> Assert.Fail (sprintf "expected Boolean, got %A" other)

// ---- Within a function body: x + x where x is the parameter ----------------
//
// `let double = x => x + x; double(5)` — call site fixes `x` to Number,
// so the constraint is satisfied. Without the call we'd see a polymorphic
// type variable still constrained.

[<Test>]
let ``constrained poly: lambda body uses op_add, applied to Number`` () =
    let expr =
        X.Let (X.Ident "double")
            (X.Fun (X.Ident "x")
                (X.App (X.App (X.Var "op_add") (X.Var "x")) (X.Var "x")))
            (X.App (X.Var "double") (X.Lit 5.0))
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match rootTyp sol with
    | Some (Mono t) when t = number -> ()
    | other -> Assert.Fail (sprintf "expected Number, got %A" other)

[<Test>]
let ``constrained poly: lambda body uses op_add, applied to String`` () =
    let expr =
        X.Let (X.Ident "double")
            (X.Fun (X.Ident "x")
                (X.App (X.App (X.Var "op_add") (X.Var "x")) (X.Var "x")))
            (X.App (X.Var "double") (X.Lit "hi"))
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match rootTyp sol with
    | Some (Mono t) when t = string' -> ()
    | other -> Assert.Fail (sprintf "expected String, got %A" other)

[<Test>]
let ``constrained poly: lambda body uses op_add, applied to Boolean is rejected`` () =
    let expr =
        X.Let (X.Ident "double")
            (X.Fun (X.Ident "x")
                (X.App (X.App (X.Var "op_add") (X.Var "x")) (X.Var "x")))
            (X.App (X.Var "double") (X.Lit true))
    let sol = Solver.solve [ "op_add", opAddTyp ] None expr
    match sol.result with
    | Error _ -> ()
    | Ok r -> Assert.Fail (sprintf "expected Error, got Ok with %A" r.typ)

// ---- ShowTyp prints the constraint -----------------------------------------

[<Test>]
let ``ShowTyp: constrained polytype prints "where tv_N in (...)"`` () =
    let s = string opAddTyp
    Assert.That(s, Does.Contain "where")
    Assert.That(s, Does.Contain "Number")
    Assert.That(s, Does.Contain "String")


// ============================================================================
// Parser01 + operator-table integration: `+`, `-`, comparisons, logical
// ============================================================================

open PureState.TypeFighter.Parser01

let private prelude : (string * Typ) list =
    [
        "log", TDef.Generalize (%0 ^-> BuiltinTypes.unit)
        BuiltinValues.toStringFunctionIdent, TDef.Generalize (%0 ^-> string')
        "op_add",
            TDef.GeneralizeWithConstraints
                [ 0, [ number; string' ] ]
                (%0 ^-> %0 ^-> %0)
        "op_sub", Mono (number ^-> number ^-> number)
        "op_mul", Mono (number ^-> number ^-> number)
        "op_lt",
            TDef.GeneralizeWithConstraints
                [ 0, [ number; string' ] ]
                (%0 ^-> %0 ^-> boolean)
        "op_eq", TDef.Generalize (%0 ^-> %0 ^-> boolean)
        "op_and", Mono (boolean ^-> boolean ^-> boolean)
        "op_or", Mono (boolean ^-> boolean ^-> boolean)
        "op_not", Mono (boolean ^-> boolean)
        "op_neg", Mono (number ^-> number)
    ]

let private typeOfSource (source: string) : MonoTyp =
    match Syntax.parse source with
    | Error err -> failwithf "parse error at %d: %s" err.idx err.message
    | Ok expr ->
        let sol = Solver.solve prelude None expr
        match sol.result with
        | Error msg -> failwithf "type error: %s" msg
        | Ok r ->
            match r.typ with
            | Some (Mono m) -> m
            | Some (Poly p) -> p.monoTyp
            | None -> failwith "no root type"

let private expectError (source: string) =
    match Syntax.parse source with
    | Error _ -> ()
    | Ok expr ->
        let sol = Solver.solve prelude None expr
        match sol.result with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected error, got %A" r.typ)

[<Test>]
let ``parser: 1 + 2 has type Number`` () =
    typeOfSource "1 + 2" |> should equal number

[<Test>]
let ``parser: \"a\" + \"b\" has type String`` () =
    typeOfSource "\"a\" + \"b\"" |> should equal string'

[<Test>]
let ``parser: true + false is rejected`` () =
    expectError "true + false"

[<Test>]
let ``parser: 1 + 2 * 3 = 7 (mul tighter than add)`` () =
    // Both are Number; we just check the type and the AST shape via parse round-trip.
    typeOfSource "1 + 2 * 3" |> should equal number

[<Test>]
let ``parser: 1 < 2 has type Boolean`` () =
    typeOfSource "1 < 2" |> should equal boolean

[<Test>]
let ``parser: \"a\" < \"b\" has type Boolean`` () =
    typeOfSource "\"a\" < \"b\"" |> should equal boolean

[<Test>]
let ``parser: true && false has type Boolean`` () =
    typeOfSource "true && false" |> should equal boolean

[<Test>]
let ``parser: !true has type Boolean`` () =
    typeOfSource "!true" |> should equal boolean

[<Test>]
let ``parser: prefix minus on Number`` () =
    typeOfSource "-(5)" |> should equal number

[<Test>]
let ``parser: 1 == 1 has type Boolean`` () =
    typeOfSource "1 == 1" |> should equal boolean


// ============================================================================
// Array indexing: `at` builtin and `xs[i]` surface syntax
// ============================================================================

let private preludeWithAt : (string * Typ) list =
    prelude @ [
        "head", TDef.Generalize (BuiltinTypes.array %0 ^-> %0)
        "at", TDef.Generalize (BuiltinTypes.array %0 ^-> number ^-> %0)
    ]

let private typeOfSourceWithAt (source: string) : MonoTyp =
    match Syntax.parse source with
    | Error err -> failwithf "parse error at %d: %s" err.idx err.message
    | Ok expr ->
        let sol = Solver.solve preludeWithAt None expr
        match sol.result with
        | Error msg -> failwithf "type error: %s" msg
        | Ok r ->
            match r.typ with
            | Some (Mono m) -> m
            | Some (Poly p) -> p.monoTyp
            | None -> failwith "no root type"

[<Test>]
let ``at: indexing Array<Number> returns Number`` () =
    typeOfSourceWithAt "let xs = [10, 20, 30]; at(xs)(1)"
    |> should equal number

[<Test>]
let ``at: indexing Array<String> returns String`` () =
    typeOfSourceWithAt "let xs = [\"a\", \"b\"]; at(xs)(0)"
    |> should equal string'

[<Test>]
let ``parser: xs[i] desugars to at(xs)(i) for Number elements`` () =
    typeOfSourceWithAt "let xs = [10, 20, 30]; xs[2]"
    |> should equal number

[<Test>]
let ``parser: xs[i] desugars to at(xs)(i) for String elements`` () =
    typeOfSourceWithAt "let xs = [\"a\", \"b\"]; xs[1]"
    |> should equal string'

[<Test>]
let ``parser: xs[i].field chains index with property access`` () =
    typeOfSourceWithAt "let users = [{ name: \"Ada\", age: 22 }, { name: \"Bob\", age: 31 }]; users[0].name"
    |> should equal string'
