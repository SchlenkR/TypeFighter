module PureState.TypeFighter.Tests.StepRecordTests

open NUnit.Framework
open FsUnit
open PureState.TypeFighter

let private number = BuiltinTypes.number
let private boolean = BuiltinTypes.boolean
let private unit_ = BuiltinTypes.unit

// Mirror of the app-demo prelude entries the emit pass cares about. Kept
// here so the test stays in the core TypeFighter project — no app-demo
// dependency needed to validate inference. `useMemo` / `getCtx` are
// library functions defined in the test stdlib below, NOT prelude entries.
let private prelude : (string * Typ) list =
    [
        "log", TDef.Generalize (%0 ^-> unit_)
        "ToString", TDef.Generalize (%0 ^-> BuiltinTypes.string)
        BuiltinValues.unitValueIdent, Mono unit_
        "op_add",
            TDef.GeneralizeWithConstraints
                [ 0, [ number; BuiltinTypes.string ] ]
                (%0 ^-> %0 ^-> %0)
        "op_mul", Mono (number ^-> number ^-> number)
        "op_gt",
            TDef.GeneralizeWithConstraints
                [ 0, [ number; BuiltinTypes.string ] ]
                (%0 ^-> %0 ^-> boolean)
        "cond",
            TDef.Generalize (boolean ^-> %0 ^-> %0 ^-> %0)
    ]

// Stream-ops as plain library functions producing the structural step-record
// the emit recognises. Same body as the app-demo stdlib.
let private testStdlib =
    "let useMemo = a => { __machine: { init: a, step: s => { val: a, nextState: a } } };\n"
    + "let count = { __machine: { init: 0, step: s => { val: s, nextState: s + 1 } } };\n"

let private rootTyp (source: string) : MonoTyp =
    match Syntax.parse (testStdlib + source) with
    | Error err -> failwithf "parse error: %s" err.message
    | Ok expr ->
        let sol = Solver.solve prelude None expr
        match sol.result with
        | Error msg -> failwithf "type error: %s" msg
        | Ok r ->
            match r.typ with
            | Some (Mono m) -> m
            | Some (Poly p) -> p.monoTyp
            | None -> failwith "no root type"

let private isStepRecord (t: MonoTyp) : bool =
    BuiltinTypes.isStepRecord t

[<Test>]
let ``useMemo(42) infers as step-record`` () =
    let t = rootTyp "useMemo(42)"
    isStepRecord t |> should equal true

[<Test>]
let ``useMemo(42) emits a Number`` () =
    match BuiltinTypes.stepRecordVS (rootTyp "useMemo(42)") with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``count infers as a step-record yielding Number`` () =
    match BuiltinTypes.stepRecordVS (rootTyp "count") with
    | Some (v, s) ->
        v |> should equal number
        s |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``a let-block ending in a stream value is itself a stream`` () =
    let t = rootTyp "let x = useMemo(42); x"
    isStepRecord t |> should equal true


// ============================================================================
// State-machine emit: step-record block -> flat C# class with tick loop
// ============================================================================

// Returns the numbered AST, the resolved root type, and a tvar→MonoTyp
// resolver matching the production pipeline in app-demo's Api.fs.
let private numberedExprFor
    (source: string)
    : Expr<VarNum> * MonoTyp * (VarNum -> MonoTyp option)
    =
    match Syntax.parse (testStdlib + source) with
    | Error err -> failwithf "parse: %s" err.message
    | Ok expr ->
        let sol = Solver.solve prelude None expr
        match sol.result with
        | Error msg -> failwithf "solve: %s" msg
        | Ok r ->
            let mono =
                match r.typ with
                | Some (Mono m) -> m
                | Some (Poly p) -> p.monoTyp
                | None -> failwith "no root type"
            let tvarToMono : VarNum -> MonoTyp option =
                let m =
                    r.substitutions
                    |> List.choose (fun s ->
                        match s.typ with
                        | Mono mono -> Some (s.tvar, mono)
                        | Poly p -> Some (s.tvar, p.monoTyp))
                    |> Map.ofList
                fun tvar -> Map.tryFind tvar m
            sol.numberedExpr, mono, tvarToMono

[<Test>]
let ``emit: useMemo(42) produces state-machine class`` () =
    let expr, rootType, tvarToMono = numberedExprFor "useMemo(42)"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None for step-record root"
    | Some cs ->
        Assert.That(cs, Does.Contain "_init = false")
        Assert.That(cs, Does.Contain "for (int __i")
        Assert.That(cs, Does.Contain "__step_val_")
        Assert.That(cs, Does.Contain "((object)42D)")
        Assert.That(cs, Does.Contain "IReadOnlyList<double> Eval")
        Assert.That(cs, Does.Contain "new List<double>()")
        Assert.That(cs, Does.Contain "return __results;")

[<Test>]
let ``emit: count yields tick-incrementing values`` () =
    let expr, rootType, tvarToMono = numberedExprFor "count"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None for step-record root"
    | Some cs ->
        // val = s, nextState = s + 1; destructured into __step_val_*.
        Assert.That(cs, Does.Contain "__step_val_")
        Assert.That(cs, Does.Contain "TfBuiltins.op_add")
        Assert.That(cs, Does.Contain "var @count")

[<Test>]
let ``emit: returns None for non-stream root types`` () =
    let expr, rootType, tvarToMono = numberedExprFor "1 + 2"
    EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr |> should equal None

[<Test>]
let ``sampler emit: count produces a stateful Step class (no tick loop)`` () =
    let expr, rootType, tvarToMono = numberedExprFor "count"
    match EmitStreamMachine.tryEmitSampler "TfSampler" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmitSampler returned None for step-record root"
    | Some cs ->
        // Instance class with state fields + a single Step(now, ...) method,
        // no fixed tick loop, no result list.
        Assert.That(cs, Does.Contain "public class TfSampler")
        Assert.That(cs, Does.Contain "object Step(DateTimeOffset __now")
        Assert.That(cs, Does.Contain "private")
        Assert.That(cs, Does.Contain "return (object)")
        Assert.That(cs, Does.Not.Contain "for (int __i")
        Assert.That(cs, Does.Not.Contain "IReadOnlyList")

[<Test>]
let ``sampler emit: returns None for non-stream root (use stateless fallback)`` () =
    let expr, rootType, tvarToMono = numberedExprFor "1 + 2"
    EmitStreamMachine.tryEmitSampler "TfSampler" [] [] rootType tvarToMono expr |> should equal None

[<Test>]
let ``stateless sampler: plain expression re-evaluated per Step`` () =
    let expr, _, tvarToMono = numberedExprFor "1 + 2"
    let cs = EmitStreamMachine.emitStatelessSampler "TfSampler" [] [] tvarToMono expr
    Assert.That(cs, Does.Contain "public class TfSampler")
    Assert.That(cs, Does.Contain "object Step(DateTimeOffset __now")
    Assert.That(cs, Does.Contain "return (object)")

[<Test>]
let ``emit: let-chain produces sequential steps`` () =
    let expr, rootType, tvarToMono = numberedExprFor "let x = useMemo(7); x"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None"
    | Some cs ->
        // useMemo step bound to x via the destructured step value.
        Assert.That(cs, Does.Contain "var @x = __step_val_")
        Assert.That(cs, Does.Contain "__results.Add")


// ============================================================================
// Implicit monadic lifting: in a stream-block, `let x = streamOp` makes `x`
// the inner V (not the full step-record wrapper); pure expressions over x stay
// pure; the block-type is inferred as step-record.
// ============================================================================

[<Test>]
let ``lift: let x = useMemo(7); x has inferred type step-record<Number,_>`` () =
    isStepRecord (rootTyp "let x = useMemo(7); x") |> should equal true

[<Test>]
let ``lift: let x = useMemo(7); x + 10 typechecks as step-record<Number,_>`` () =
    let t = rootTyp "let x = useMemo(7); x + 10"
    isStepRecord t |> should equal true
    match BuiltinTypes.stepRecordVS t with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``lift: end-to-end via emit pass — stream-let yields ten ticks`` () =
    let expr, rootType, tvarToMono = numberedExprFor "let x = useMemo(7); x + 10"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None"
    | Some cs ->
        // useMemo as state slot, body computes x+10 each tick
        Assert.That(cs, Does.Contain "_s0_init")
        Assert.That(cs, Does.Contain "for (int __i")


// ============================================================================
// Composition: multiple stream-steps + pure intermediates in one block
// ============================================================================

[<Test>]
let ``compose: two useMemo steps share the same C and produce step-record`` () =
    let t = rootTyp "let a = useMemo(3); let b = useMemo(4); a + b"
    isStepRecord t |> should equal true
    match BuiltinTypes.stepRecordVS t with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``compose: ctx + useMemo combine into a step-record block`` () =
    isStepRecord (rootTyp "let i = count; let m = useMemo(100); m + i") |> should equal true

[<Test>]
let ``compose: pure intermediate let inside a stream block stays pure`` () =
    // doubled = i * 2 is a plain Number binding even though the enclosing
    // block is a step-record. The emit pass turns it into a regular C# local.
    let t = rootTyp "let i = count; let doubled = i * 2; doubled + 1"
    isStepRecord t |> should equal true

[<Test>]
let ``compose: emit pass handles pure intermediates`` () =
    let expr, rootType, tvarToMono =
        numberedExprFor "let i = count; let doubled = i * 2; doubled + 1"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None"
    | Some cs ->
        // doubled is a plain local, no state slot
        Assert.That(cs, Does.Contain "var @doubled")

[<Test>]
let ``compose: emit pass escapes C# reserved words for binding names`` () =
    // `base` is a C# keyword; the emit must prefix user-bound identifiers
    // so Roslyn doesn't choke.
    let expr, rootType, tvarToMono =
        numberedExprFor "let base = useMemo(10); base + 1"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None"
    | Some cs ->
        Assert.That(cs, Does.Contain "var @base = __step_val_")
        Assert.That(cs, Does.Not.Contain "var base ")

// ============================================================================
// Pure functions over stream-lifted values
// ============================================================================

[<Test>]
let ``stream-fn: pure function applied to lifted value typechecks as step-record`` () =
    // `inc` is a pure `Number -> Number`. Applying it to a lifted stream
    // value keeps the call pure, but the enclosing block is a step-record
    // because of the `useMemo` step.
    let t = rootTyp "let inc = (a) => a + 1; let x = useMemo(7); inc(x)"
    isStepRecord t |> should equal true
    match BuiltinTypes.stepRecordVS t with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``stream-fn: emit pass handles pure function applied to stream value`` () =
    let expr, rootType, tvarToMono =
        numberedExprFor "let inc = (a) => a + 1; let x = useMemo(7); inc(x)"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None"
    | Some cs ->
        // inc is a plain user function; lives as a local Func-shaped value.
        Assert.That(cs, Does.Contain "var @inc")
        Assert.That(cs, Does.Contain "_s0_init")
        // Result list typed by V (Number → double).
        Assert.That(cs, Does.Contain "IReadOnlyList<double> Eval")

[<Test>]
let ``stream-fn: pure binary function over two lifted values`` () =
    // Both `a` and `b` are lifted to Number; `add(a)(b)` is a pure call.
    let t = rootTyp "let add = (x) => (y) => x + y; let a = useMemo(3); let b = useMemo(4); add(a)(b)"
    isStepRecord t |> should equal true


// ============================================================================
// User-defined stream-producing functions
// ============================================================================
// Detection is structural and shape-based: any binding whose value walks
// down to a record-literal carrying the `__machine` marker is treated
// like a prelude stream-op. No special status for `useMemo`/`getCtx`.

// ============================================================================
// Custom step-records — purely structural, one reserved field
// ============================================================================
// The compiler recognises any record carrying `__machine` as a state-machine
// step. Defining a stream from scratch (without useMemo or getCtx) compiles
// to the same flat state-machine.

[<Test>]
let ``custom step record: typechecks as step-record`` () =
    let t = rootTyp "let myMemo = v => { __machine: { init: v, step: s => { val: v, nextState: v } } }; myMemo(42)"
    isStepRecord t |> should equal true
    match BuiltinTypes.stepRecordVS t with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``custom step record: emit produces state-machine without naming useMemo`` () =
    let expr, rootType, tvarToMono =
        numberedExprFor "let myMemo = v => { __machine: { init: v, step: s => { val: v, nextState: v } } }; myMemo(42)"
    match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono expr with
    | None -> Assert.Fail "tryEmit returned None"
    | Some cs ->
        // Slot allocated, init seeded, val destructured — no Func chain.
        Assert.That(cs, Does.Contain "_s0_init")
        Assert.That(cs, Does.Contain "__step_val_0")
        Assert.That(cs, Does.Contain "double _s0")

[<Test>]
let ``user stream-fn: typechecks as step-record`` () =
    let t = rootTyp "let mkStream = (a) => useMemo(a); let x = mkStream(99); x + 1"
    isStepRecord t |> should equal true
    match BuiltinTypes.stepRecordVS t with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

// ============================================================================
// `cond` — branching as a typed builtin, lowered to a C# ternary
// ============================================================================

[<Test>]
let ``cond: typechecks as the branch type`` () =
    let t = rootTyp "cond(true, 1, 2)"
    t |> should equal number

[<Test>]
let ``cond: branches must agree on type`` () =
    match Syntax.parse "cond(true, 1, \"x\")" with
    | Error _ -> Assert.Fail "parse should succeed"
    | Ok expr ->
        let sol = Solver.solve prelude None expr
        match sol.result with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected unify error, got %A" r.typ)

[<Test>]
let ``cond: emit lowers to a C# ternary, not a curried Func chain`` () =
    let expr =
        match Syntax.parse "cond(true, 1, 2)" with
        | Error _ -> failwith "parse"
        | Ok e -> e
    let sol = Solver.solve prelude None expr
    let tvarToMono : VarNum -> MonoTyp option =
        match sol.result with
        | Ok r ->
            let m =
                r.substitutions
                |> List.choose (fun s ->
                    match s.typ with
                    | Mono mono -> Some (s.tvar, mono)
                    | Poly p -> Some (s.tvar, p.monoTyp))
                |> Map.ofList
            fun tvar -> Map.tryFind tvar m
        | Error _ -> fun _ -> None
    let cs = EmitCSharp.toCSharp tvarToMono sol.numberedExpr
    Assert.That(cs, Does.Contain "?")
    Assert.That(cs, Does.Contain ":")
    Assert.That(cs, Does.Not.Contain "(@cond)")

[<Test>]
let ``cond: works inside a stream block`` () =
    // Branches over a lifted stream value — typed ternary on `double`.
    let t = rootTyp "let x = useMemo(7); cond(x > 3, x, 0)"
    isStepRecord t |> should equal true


[<Test>]
let ``limitation: nested useMemo fails with the new step-record useMemo`` () =
    // With `useMemo = a => { __machine: { init: a, step: ... { val: a, ... } } }`
    // the inner type is forced to V = T. Calling useMemo on a step-record
    // would require unifying Number (the val type expected by the outer)
    // with the inner step-record — type error. Documents the boundary.
    match Syntax.parse (testStdlib + "useMemo(useMemo(7))") with
    | Error _ -> Assert.Fail "parse should succeed"
    | Ok expr ->
        let sol = Solver.solve prelude None expr
        match sol.result with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected type error, got %A" r.typ)

// ============================================================================
// Desugar pipeline: stream-argument hoisting, UFCS, wrap (return/pure),
// scoped shadowing, opaque (imported) streams
// ============================================================================

// Prelude with one imported stream block, mirroring app-demo's DSP
// imports: declared purely by type (stream param, stream return). The
// block exists bare (`emaT`, legacy tests), as a namespaced flat
// (`__tmath_emaT`) and via the `tmath` namespace record.
let private emaTTyp =
    TDef.Generalize
        (BuiltinTypes.stepRecord number %0 ^-> number
            ^-> BuiltinTypes.stepRecord number %1)

let private importedPrelude : (string * Typ) list =
    prelude @ [
        "emaT", emaTTyp
        "__tmath_emaT", emaTTyp
        "tmath",
            Mono (TDef.RecordWith [
                "emaT", (match emaTTyp with Mono m -> m | Poly p -> p.monoTyp)
            ])
    ]

// Mirror of app-demo's Builtins.namespaceMembers.
let private testNamespaces : Map<string, Map<string, string>> =
    Map.ofList [ "tmath", Map.ofList [ "emaT", "__tmath_emaT" ] ]

// Mirror of app-demo's index construction (Api.mkPreludeIndex).
let private preludeIndexFor (prelude: (string * Typ) list) : Desugar.PreludeIndex =
    let monoOf t = match t with Mono m -> m | Poly p -> p.monoTyp
    { kinds =
        prelude
        |> List.choose (fun (n, t) ->
            match StepRecord.bindingKindOfTyp t with
            | StepRecord.BindingKind.Plain -> None
            | kind -> Some (n, kind))
        |> Map.ofList
      callables =
        prelude
        |> List.choose (fun (n, t) -> match monoOf t with FunTyp _ -> Some n | _ -> None)
        |> Set.ofList
      streamParams =
        prelude
        |> List.choose (fun (n, t) ->
            let rec walk t i acc =
                match t with
                | FunTyp (p, ret) ->
                    let acc = if BuiltinTypes.isStepRecord p then Set.add i acc else acc
                    walk ret (i + 1) acc
                | _ -> acc
            let positions = walk (monoOf t) 0 Set.empty
            if positions.IsEmpty then None else Some (n, positions))
        |> Map.ofList
      namespaceMembers = testNamespaces }

// Full production pipeline: parse -> desugar -> solve.
let private desugaredSolve (source: string) =
    match Syntax.parse (testStdlib + source) with
    | Error err -> failwithf "parse: %s" err.message
    | Ok expr ->
        let d = Desugar.run (preludeIndexFor importedPrelude) expr
        Solver.solve importedPrelude None d.expr

let private desugaredRootTyp (source: string) : MonoTyp =
    let sol = desugaredSolve source
    match sol.result with
    | Error msg -> failwithf "solve: %s" msg
    | Ok r ->
        match r.typ with
        | Some (Mono m) -> m
        | Some (Poly p) -> p.monoTyp
        | None -> failwith "no root type"

let private desugaredEmit (source: string) : string =
    let sol = desugaredSolve source
    match sol.result with
    | Error msg -> failwithf "solve: %s" msg
    | Ok r ->
        let rootType =
            match r.typ with
            | Some (Mono m) -> m
            | Some (Poly p) -> p.monoTyp
            | None -> failwith "no root type"
        let tvarToMono : VarNum -> MonoTyp option =
            let m =
                r.substitutions
                |> List.choose (fun s ->
                    match s.typ with
                    | Mono mono -> Some (s.tvar, mono)
                    | Poly p -> Some (s.tvar, p.monoTyp))
                |> Map.ofList
            fun tvar -> Map.tryFind tvar m
        match EmitStreamMachine.tryEmit "TfTest" [] [] rootType tvarToMono sol.numberedExpr with
        | Some cs -> cs
        | None -> failwith "tryEmit returned None"

[<Test>]
let ``hoist: stream argument to a value-typed callee becomes a lifted let`` () =
    // count is a stream; inc expects a value -> the arg is hoisted and the
    // block becomes a stream of the result.
    let t = desugaredRootTyp "let inc = a => a + 1; inc(count)"
    isStepRecord t |> should equal true
    match BuiltinTypes.stepRecordVS t with
    | Some (v, _) -> v |> should equal number
    | None -> Assert.Fail "expected step-record"

[<Test>]
let ``hoist: stream call inline in an operator expression`` () =
    let t = desugaredRootTyp "useMemo(7) + 1"
    isStepRecord t |> should equal true

[<Test>]
let ``ufcs: method-call syntax on a lifted per-tick value`` () =
    // i.double() -> double(i); zero-arg form drops the synthetic unit.
    let t = desugaredRootTyp "let double = x => x + x; let i = count; i.double()"
    isStepRecord t |> should equal true

[<Test>]
let ``ufcs: call result of a known callable is an eligible receiver`` () =
    desugaredRootTyp "let inc = x => x + 1; let double = x => x + x; inc(5).double()"
    |> should equal number

[<Test>]
let ``ufcs: chains recurse through method-call receivers`` () =
    desugaredRootTyp "let inc = x => x + 1; inc(1).inc().inc()"
    |> should equal number

[<Test>]
let ``ufcs: binding of a callable call result is an eligible receiver`` () =
    desugaredRootTyp "let inc = x => x + 1; let y = inc(5); y.inc()"
    |> should equal number

[<Test>]
let ``ufcs: aliases inherit receiver eligibility`` () =
    desugaredRootTyp "let inc = x => x + 1; let y = inc(5); let z = y; z.inc()"
    |> should equal number

[<Test>]
let ``ufcs: lambda parameters are method receivers`` () =
    // Scope rule: receiver shape is irrelevant, so method syntax works on
    // function parameters (chains are abstractable).
    desugaredRootTyp "let inc = x => x + 1; let apply = v => v.inc(); apply(5)"
    |> should equal number

[<Test>]
let ``ufcs: a visible callable shadows a record field in call position`` () =
    // Shadowing rule (documented edge case): `inc` is visible, so
    // `r.inc(3)` resolves as inc(r)(3) -- a type error here, not a silent
    // field call.
    match (desugaredSolve "let inc = x => x + 1; let r = { inc: x => x }; r.inc(3)").result with
    | Error _ -> ()
    | Ok _ -> Assert.Fail "expected the visible callable to shadow the field"

[<Test>]
let ``ufcs: field access without call parens is untouched`` () =
    // Plain `r.inc` stays a field read even when `inc` is a visible
    // callable; only the CALL form applies the scope rule.
    desugaredRootTyp "let inc = x => x + 1; let r = { inc: x => x }; let g = r.inc; g(3)"
    |> should equal number

[<Test>]
let ``wrap: plain value into a declared stream parameter (return/pure)`` () =
    let t = desugaredRootTyp "let e = emaT(1 + 2, 0.5); e"
    isStepRecord t |> should equal true

[<Test>]
let ``wrap: AST stream into a declared stream parameter is hoisted and re-wrapped`` () =
    let t = desugaredRootTyp "let e = emaT(count, 0.5); e"
    isStepRecord t |> should equal true

[<Test>]
let ``shadowing: rebinding a stream name makes it a plain value`` () =
    desugaredRootTyp "let count = 5; let x = count; x" |> should equal number

[<Test>]
let ``alias: re-binding a lifted value stays a plain let`` () =
    // Was a latent bug: the flat streamReturningBindings set lifted the
    // alias a second time and failed to unify.
    let t = desugaredRootTyp "let i = count; let x = i; x"
    isStepRecord t |> should equal true

[<Test>]
let ``emit: beta-reduction freshens binders so user idents are not captured`` () =
    // `s` collides with useMemo's `step: s => ...` binder; without
    // alpha-renaming the user `s` was rewritten to the state slot.
    let cs = desugaredEmit "let s = 5; let m = useMemo(s + 1); m"
    Assert.That(cs, Does.Contain "@s")

[<Test>]
let ``emit: opaque (imported) stream is driven via the runtime step protocol`` () =
    let cs = desugaredEmit "let e = emaT(count, 0.5); e"
    Assert.That(cs, Does.Contain "__rec_")
    Assert.That(cs, Does.Contain "[\"__machine\"]")
    Assert.That(cs, Does.Contain "[\"init\"]")
    Assert.That(cs, Does.Contain "[\"nextState\"]")

[<Test>]
let ``do statement inside a stream block stays a stream`` () =
    let t = desugaredRootTyp "let i = count; log(i); i * 2"
    isStepRecord t |> should equal true

[<Test>]
let ``namespace: ns.member call resolves to the flat import`` () =
    let t = desugaredRootTyp "let e = tmath.emaT(count, 0.5); e"
    isStepRecord t |> should equal true

[<Test>]
let ``namespace: ufcs method resolves through the namespace index`` () =
    let t = desugaredRootTyp "let i = count; i.emaT(0.5)"
    isStepRecord t |> should equal true

[<Test>]
let ``namespace: user binding shadows the namespace name`` () =
    desugaredRootTyp "let tmath = 5; tmath + 1" |> should equal number

[<Test>]
let ``namespace: first-class member access typechecks as a function`` () =
    match desugaredRootTyp "let f = tmath.emaT; f" with
    | FunTyp _ -> ()
    | t -> Assert.Fail (sprintf "expected a function type, got %A" t)
