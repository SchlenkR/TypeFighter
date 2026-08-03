// Compile-time desugaring of step-record-typed TypeFighter blocks to a flat
// C# state-machine. Steps are recognised STRUCTURALLY: any `let x = expr`
// whose value beta-reduces to a record literal `{ __machine: .. }` becomes
// one state slot. There's no magic for `useMemo`/`count` — they're plain
// library definitions in the stdlib that happen to construct such records.
//
// Per call site:
//   1. Beta-reduce the RHS (substituting through user-defined wrapper fns,
//      with alpha-freshened binders so user idents can't be captured)
//      until a record literal surfaces. A beta-reduced body may itself be
//      a let-chain (a stream block, e.g. built by the desugar pass inside
//      a fn body); its inner lets are SPLICED into the outer step list.
//   2. Use `init`'s value-expression to seed the slot, guarded inside
//      the tick body (so inits may reference earlier per-tick values).
//   3. Inline the `step` lambda's body into the tick body, substituting
//      its state param with the slot variable.
//   4. The body terminates in a record literal `{ val, nextState }` —
//      destructure inline so no per-tick record allocation happens.
module PureState.TypeFighter.EmitStreamMachine

open System.Text
open PureState.TypeFighter
open PureState.TypeFighter.Parser01
// Structural step-record helpers (shape tests, fieldOf, beta-reducer,
// the shared stream classifier) live in `module StepRecord` in Lang.fs.
open PureState.TypeFighter.StepRecord

module SF = BuiltinTypes.StepFields

// Fixed number of ticks the generated state-machine runs before returning
// its result list. Hard-wired for the preview/eval use case: a stream is
// sampled exactly this many times. Make this a parameter once stream
// programs need a caller-controlled horizon.
let private nTicks = 10

let isStepRecordRoot (rootType: MonoTyp) : bool =
    BuiltinTypes.isStepRecord rootType

let private emitExprText (tvarToMono: VarNum -> MonoTyp option) (e: Expr<VarNum>) =
    EmitCSharp.toCSharp tvarToMono e

// Top-level statement chain: bindings and Do-actions, in order.
type private ChainEntry =
    | EBind of name: string * rhs: Expr<VarNum>
    | EStmt of action: Expr<VarNum>

let private collectChain (expr: Expr<VarNum>) =
    let entries = ResizeArray<ChainEntry>()
    let rec walk = function
        | Expr.Let l ->
            entries.Add(EBind (l.ident.identName, l.value))
            walk l.body
        | Expr.Do d ->
            entries.Add(EStmt d.action)
            walk d.body
        | other -> other
    let finalExpr = walk expr
    List.ofSeq entries, finalExpr

// Result of beta-reducing an expression down to its stream structure:
// inner chain entries to splice (from let-chain bodies) plus either the
// step-record literal or an opaque per-tick expression.
type private Final =
    | FRecord of RecordItem<VarNum> list
    | FPlain of Expr<VarNum>

type private Resolved =
    { spliced: ChainEntry list
      final: Final }

// One entry of the fully flattened state machine, in execution order.
type private FlatStep =
    // A state slot: optional per-tick binding name exposing `val`.
    | FSlot of slotIdx: int * bindName: string option * items: RecordItem<VarNum> list
    // A step whose record is only known at RUNTIME (e.g. built by an
    // imported .NET stream block from the prelude): the rhs evaluates to a
    // step-record VALUE. The slot holds its state; the record itself is
    // rebuilt per tick (cheap closures, so per-tick context like host values stays
    // fresh) while `init` seeds the slot once.
    | FOpaque of slotIdx: int * bindName: string option * rhs: Expr<VarNum> * valType: string
    // A plain per-tick binding (aliases, pure values, fn closures).
    | FAlias of name: string * rhs: Expr<VarNum>
    // A Do-statement, evaluated per tick for its effect.
    | FAction of action: Expr<VarNum>

// Two emit shapes share all the step-resolution and slot logic below:
//   TickLoop n : a `static IReadOnlyList<V> Eval(...)` running n fixed ticks
//                (the /eval preview path).
//   Sampler    : a `public class` with state in INSTANCE fields and a single
//                `object Step(DateTimeOffset __now, ...)` (one tick per call,
//                state persists across calls) -- for VirtualCurves sampling.
type private EmitMode =
    | TickLoop of nTicks: int
    | Sampler

let private emitCore
    (mode: EmitMode)
    (className: string)
    (csImports: (string * string) list)
    (extraParams: (string * string) list)
    (rootType: MonoTyp)
    (tvarToMono: VarNum -> MonoTyp option)
    (expr: Expr<VarNum>)
    : string option
    =
    if not (isStepRecordRoot rootType) then None else
    let emitText = emitExprText tvarToMono

    let entries, finalExpr = collectChain expr

    // Fresh names for alpha-renaming beta-reduced fn bodies; must not
    // collide with anything in the program.
    let used = usedIdents expr
    let mutable freshIdx = 0
    let freshName () =
        let mutable n = sprintf "__fb%d" freshIdx
        freshIdx <- freshIdx + 1
        while used.Contains n do
            n <- sprintf "__fb%d" freshIdx
            freshIdx <- freshIdx + 1
        n

    // Classify the top-level chain with the same shared classifier the
    // solver uses, so emit and solver can never drift. Stream-fn DEFINITIONS
    // (StreamFn) have no runtime presence: call sites beta-reduce them.
    let kinds, fnDefs =
        let mutable kinds : StreamKinds = Map.empty
        let fnDefs = System.Collections.Generic.Dictionary<string, Expr<VarNum>>()
        for entry in entries do
            match entry with
            | EBind (name, rhs) ->
                let kind =
                    if isStreamExpr kinds rhs then BindingKind.Plain
                    else bindingKindOfRhs kinds rhs
                kinds <- kinds |> Map.add name kind
                match kind with
                | BindingKind.StreamFn _ -> fnDefs.[name] <- rhs
                | _ -> fnDefs.Remove name |> ignore
            | EStmt _ -> ()
        kinds, fnDefs

    let isStreamFnDef (name: string) =
        match kinds.TryFind name with
        | Some (BindingKind.StreamFn _) -> true
        | _ -> false

    // Beta-reduce `e` down to its stream structure. Memoized by reference
    // so slot discovery and emission see the same freshened result.
    let resolveCache = System.Collections.Generic.Dictionary<obj, Resolved>(HashIdentity.Reference)
    let rec resolve (e: Expr<VarNum>) : Resolved =
        match resolveCache.TryGetValue(box e) with
        | true, r -> r
        | _ ->
            let r =
                match e with
                | Expr.MkRecord r when hasStepShape r.items ->
                    { spliced = []; final = FRecord r.items }
                | Expr.App _ ->
                    let head, args = collectAppChain e
                    match head with
                    | Expr.Var v when fnDefs.ContainsKey v.ident ->
                        let freshened = freshenBinders freshName fnDefs.[v.ident]
                        resolve (applyLambda freshened args)
                    | _ -> { spliced = []; final = FPlain e }
                | Expr.Let l ->
                    let inner = resolve l.body
                    { inner with spliced = EBind (l.ident.identName, l.value) :: inner.spliced }
                | Expr.Do d ->
                    let inner = resolve d.body
                    { inner with spliced = EStmt d.action :: inner.spliced }
                | _ -> { spliced = []; final = FPlain e }
            resolveCache.[box e] <- r
            r

    // Flatten the whole program into execution-ordered FlatSteps, assigning
    // slot indices positionally (immune to shadowed binding names).
    let mutable slotCount = 0
    // (idx, Some initExpr) = step-record slot typed off its `init`;
    // (idx, None) = opaque object slot for an imported block.
    let slotInits = ResizeArray<int * Expr<VarNum> option>()
    let newSlot (initExpr: Expr<VarNum> option) =
        let idx = slotCount
        slotCount <- slotCount + 1
        slotInits.Add(idx, initExpr)
        idx

    // An opaque stream: the expression's SOLVED type is a step-record, but
    // its head is not a chain binding the emit can beta-reduce -- e.g. a
    // prelude block backed by a real .NET implementation. Purely
    // type-directed; no per-import emit knowledge needed.
    let (|OpaqueStream|_|) (e: Expr<VarNum>) =
        let head, _ = collectAppChain e
        match head with
        | Expr.Var v when not (kinds.ContainsKey v.ident) ->
            match tvarToMono e.TVar with
            | Some t when BuiltinTypes.isStepRecord t ->
                let valType =
                    BuiltinTypes.stepRecordV t
                    |> Option.bind EmitCSharp.csTypeOfMono
                    |> Option.defaultValue "object"
                Some valType
            | _ -> None
        | _ -> None

    let rec flattenEntry (entry: ChainEntry) : FlatStep list =
        match entry with
        | EStmt action -> [ FAction action ]
        | EBind (name, _) when isStreamFnDef name -> []
        | EBind (name, rhs) ->
            let resolved = resolve rhs
            let spliced = resolved.spliced |> List.collect flattenEntry
            match resolved.final with
            | FRecord items ->
                let idx = newSlot (machineFieldOf SF.init items)
                spliced @ [ FSlot (idx, Some name, items) ]
            | FPlain (OpaqueStream valType & e) ->
                let idx = newSlot None
                spliced @ [ FOpaque (idx, Some name, e, valType) ]
            | FPlain e -> spliced @ [ FAlias (name, e) ]

    let flatSteps = entries |> List.collect flattenEntry

    // The final expression: spliced lets become trailing steps; a final
    // step-record (AST-level or opaque) gets its own slot, a plain final
    // is evaluated directly.
    let finalResolved = resolve finalExpr
    let finalSpliced = finalResolved.spliced |> List.collect flattenEntry
    let finalStep =
        match finalResolved.final with
        | FRecord items ->
            let idx = newSlot (machineFieldOf SF.init items)
            Some (idx, FSlot (idx, None, items))
        | FPlain (OpaqueStream valType & e) ->
            let idx = newSlot None
            Some (idx, FOpaque (idx, None, e, valType))
        | FPlain _ -> None

    // Extract V from rootType for the result list type.
    let vType =
        BuiltinTypes.stepRecordV rootType
        |> Option.bind EmitCSharp.csTypeOfMono
        |> Option.defaultValue "object"

    // Slot C# types, derived from each `init` expression's resolved type;
    // imported blocks hold their opaque runtime object.
    let slotTypes =
        [ for idx, initExpr in slotInits ->
            let slotType =
                initExpr
                |> Option.bind (fun (e: Expr<VarNum>) -> tvarToMono e.TVar)
                |> Option.bind EmitCSharp.csTypeOfMono
                |> Option.defaultValue "object"
            let slotDef =
                match slotType with
                | "double" -> "0D"
                | "bool"   -> "false"
                | _        -> "null"
            idx, slotType, slotDef ]

    let slotTypeOf =
        let m = System.Collections.Generic.Dictionary<int, string>()
        for idx, slotType, _ in slotTypes do m.[idx] <- slotType
        fun idx -> m.[idx]

    let sb = StringBuilder()
    let line (s: string) = sb.AppendLine s |> ignore
    line "using System;"
    line "using System.Collections.Generic;"
    line "using System.IO;"
    line ""

    // Emit one flattened step into the tick body (12-space indent). For
    // slots: guarded init (inits may reference earlier per-tick locals),
    // inlined step body, inline destructure of `{ val, nextState }`.
    let emitFlatStep (step: FlatStep) =
        match step with
        | FAction action ->
            sb.AppendLine (sprintf "            _ = (object)(%s);" (emitText action)) |> ignore
        | FAlias (name, rhs) ->
            sb.AppendLine (sprintf "            var @%s = (object)(%s);" name (emitText rhs)) |> ignore
        | FOpaque (idx, bindName, rhs, valType) ->
            // Runtime step protocol: the rhs evaluates to a step-record
            // VALUE (IDictionary). Rebuilt per tick so closures over
            // per-tick context stay fresh; state lives in the slot.
            let dictT = "System.Collections.Generic.IDictionary<string, object>"
            let rhsCs = emitText rhs
            sb.AppendLine (sprintf "            var __outer_%d = (%s)((object)(%s));" idx dictT rhsCs) |> ignore
            sb.AppendLine (sprintf "            var __rec_%d = (%s)__outer_%d[\"%s\"];" idx dictT idx SF.machine) |> ignore
            sb.AppendLine
                (sprintf "            if (!_s%d_init) { _s%d = __rec_%d[\"%s\"]; _s%d_init = true; }"
                    idx idx idx SF.init idx)
            |> ignore
            sb.AppendLine
                (sprintf "            var __res_%d = (%s)(((Func<object, object>)__rec_%d[\"%s\"])(_s%d));"
                    idx dictT idx SF.step idx)
            |> ignore
            sb.AppendLine (sprintf "            %s __step_val_%d = (%s)__res_%d[\"%s\"];" valType idx valType idx SF.value) |> ignore
            sb.AppendLine (sprintf "            _s%d = __res_%d[\"%s\"];" idx idx SF.nextState) |> ignore
            match bindName with
            | Some name ->
                sb.AppendLine (sprintf "            var @%s = __step_val_%d;" name idx) |> ignore
            | None -> ()
        | FSlot (idx, bindName, items) ->
            let slotType = slotTypeOf idx
            // Step's `val` type — derived from the val-field's resolved
            // tvar so we can declare a typed local for it.
            let valType =
                match machineFieldOf SF.step items with
                | Some (Expr.Fun outer) ->
                    match outer.body with
                    | Expr.MkRecord resR when hasResultShape resR.items ->
                        match fieldOf SF.value resR.items with
                        | Some v ->
                            tvarToMono v.TVar
                            |> Option.bind EmitCSharp.csTypeOfMono
                            |> Option.defaultValue "object"
                        | None -> "object"
                    | _ -> "object"
                | _ -> "object"

            match machineFieldOf SF.init items with
            | Some initExpr ->
                let initCs = emitText initExpr
                sb.AppendLine
                    (sprintf "            if (!_s%d_init) { _s%d = (%s)(%s); _s%d_init = true; }"
                        idx idx slotType initCs idx)
                |> ignore
            | None -> ()

            match machineFieldOf SF.step items with
            | Some (Expr.Fun outer) ->
                let sParam = outer.ident.identName
                let slotVar = sprintf "_s%d" idx
                let withSlot =
                    substVar sParam
                        (Expr.Var {| ident = slotVar; tvar = outer.ident.tvar |})
                        outer.body
                match withSlot with
                | Expr.MkRecord resR when hasResultShape resR.items ->
                    let valExpr = fieldOf SF.value resR.items |> Option.get
                    let nextExpr = fieldOf SF.nextState resR.items |> Option.get
                    let valCs = emitText valExpr
                    let nextCs = emitText nextExpr
                    sb.AppendLine (sprintf "            %s __step_val_%d = (%s)(%s);" valType idx valType valCs) |> ignore
                    sb.AppendLine (sprintf "            %s = (%s)(%s);" slotVar slotType nextCs) |> ignore
                    match bindName with
                    | Some name ->
                        sb.AppendLine (sprintf "            var @%s = __step_val_%d;" name idx) |> ignore
                    | None -> ()
                | _ ->
                    let bodyCs = emitText withSlot
                    sb.AppendLine (sprintf "            var __step_r_%d = ((System.Collections.Generic.IDictionary<string, object>)((object)(%s)));" idx bodyCs) |> ignore
                    sb.AppendLine (sprintf "            %s __step_val_%d = (%s)__step_r_%d[\"val\"];" valType idx valType idx) |> ignore
                    match bindName with
                    | Some name ->
                        sb.AppendLine (sprintf "            var @%s = __step_val_%d;" name idx) |> ignore
                    | None -> ()
                    sb.AppendLine (sprintf "            %s = (%s)__step_r_%d[\"nextState\"];" slotVar slotType idx) |> ignore
            | _ -> ()

    // Run every step once, then hand the final value to `emitFinal` (the
    // only place the two modes differ at the body level: collect-into-list
    // vs. return).
    let emitStepsAndFinal (emitFinal: string -> unit) =
        for step in flatSteps do emitFlatStep step
        for step in finalSpliced do emitFlatStep step
        match finalStep with
        | Some (idx, step) ->
            emitFlatStep step
            emitFinal (sprintf "__step_val_%d" idx)
        | None ->
            match finalResolved.final with
            | FPlain e -> emitFinal (sprintf "(%s)(%s)" vType (emitText e))
            | FRecord _ -> ()

    match mode with
    | TickLoop ticks ->
        line (sprintf "public static class %s" className)
        line "{"
        let paramSig =
            ("__out", "TextWriter") :: extraParams
            |> List.map (fun (n, t) -> sprintf "%s %s" t n)
            |> String.concat ", "
        line (sprintf "    public static IReadOnlyList<%s> Eval(%s)" vType paramSig)
        line "    {"
        // `__now` advances one second per preview tick so time-based
        // streams (sine, timeSince, ...) show movement in the 10-tick
        // preview; the sampler path sees real sample instants instead.
        line "        var __now0 = DateTimeOffset.UtcNow;"
        for name, body in csImports do
            line (sprintf "        var %s = %s;" name body)
        for idx, slotType, slotDef in slotTypes do
            line (sprintf "        %s _s%d = %s;" slotType idx slotDef)
            line (sprintf "        bool _s%d_init = false;" idx)
        line (sprintf "        const int __ticks = %d;" ticks)
        line (sprintf "        var __results = new List<%s>();" vType)
        line "        for (int __i = 0; __i < __ticks; __i++) {"
        line "            var __now = __now0.AddSeconds(__i);"
        emitStepsAndFinal (fun v -> line (sprintf "            __results.Add(%s);" v))
        line "        }"
        line "        return __results;"
        line "    }"
        line "}"
    | Sampler ->
        // State lives in instance fields so it persists across Step calls
        // (one Step = one tick at a real sample time). No tick loop.
        line (sprintf "public class %s" className)
        line "{"
        for idx, slotType, slotDef in slotTypes do
            line (sprintf "    private %s _s%d = %s;" slotType idx slotDef)
            line (sprintf "    private bool _s%d_init = false;" idx)
        let paramSig =
            ("__now", "DateTimeOffset") :: extraParams
            |> List.map (fun (n, t) -> sprintf "%s %s" t n)
            |> String.concat ", "
        line (sprintf "    public object Step(%s)" paramSig)
        line "    {"
        // `log` in a sampler discards to TextWriter.Null (no console in this path).
        line "        var __out = System.IO.TextWriter.Null;"
        for name, body in csImports do
            line (sprintf "        var %s = %s;" name body)
        emitStepsAndFinal (fun v -> line (sprintf "            return (object)(%s);" v))
        line "    }"
        line "}"

    Some (sb.ToString())

/// Emit the fixed-tick `Eval` preview class (the /eval path).
let tryEmit className csImports extraParams rootType tvarToMono expr =
    emitCore (TickLoop nTicks) className csImports extraParams rootType tvarToMono expr

/// Emit a stateful `Step(now, ...)` sampler class for VirtualCurves: one tick
/// per call, state persisted in instance fields. None if root isn't a step-record.
let tryEmitSampler className csImports extraParams rootType tvarToMono expr =
    emitCore Sampler className csImports extraParams rootType tvarToMono expr

/// Emit a STATELESS `Step(now, ...)` sampler: the expression is plain (not a
/// step-record), so every sample instant just re-evaluates it (typically over
/// the host values at `now`). No persisted state. Used as the fallback when
/// `tryEmitSampler` returns None.
let emitStatelessSampler
    (className: string)
    (csImports: (string * string) list)
    (extraParams: (string * string) list)
    (tvarToMono: VarNum -> MonoTyp option)
    (expr: Expr<VarNum>)
    : string
    =
    let sb = StringBuilder()
    let line (s: string) = sb.AppendLine s |> ignore
    line "using System;"
    line "using System.Collections.Generic;"
    line "using System.IO;"
    line ""
    line (sprintf "public class %s" className)
    line "{"
    let paramSig =
        ("__now", "DateTimeOffset") :: extraParams
        |> List.map (fun (n, t) -> sprintf "%s %s" t n)
        |> String.concat ", "
    line (sprintf "    public object Step(%s)" paramSig)
    line "    {"
    line "        var __out = System.IO.TextWriter.Null;"
    for name, body in csImports do
        line (sprintf "        var %s = %s;" name body)
    line (sprintf "        return (object)(%s);" (EmitCSharp.toCSharp tvarToMono expr))
    line "    }"
    line "}"
    sb.ToString()
