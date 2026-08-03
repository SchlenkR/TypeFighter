module PureState.TypeFighter.Web.Api

open PureState.TypeFighter
open PureState.TypeFighter.Parser01

// Hover types displayed for hugely-fielded records get truncated to keep
// responses small (otherwise a 3k-field record blows the response to MB).
do ShowTyp.AddPrinter (fun typ ->
    match typ with
    | RecordTyp r when r.fields.Count > 30 ->
        Some (sprintf "{ <%d fields> }" r.fields.Count)
    | _ -> None)

// Step-records read as `Stream<V>` in hovers/signatures instead of the raw
// `{ __machine: ... }` shape -- that is what they MEAN to users.
do ShowTyp.AddPrinter (fun typ ->
    if BuiltinTypes.isStepRecord typ then
        match BuiltinTypes.stepRecordV typ with
        | Some v -> Some (sprintf "Stream<%s>" (ShowTyp.Show v))
        | None -> Some "Stream"
    else None)


type PreludeEntry =
    { name: string
      typ: string
      fields: string array }

type HoverInfo =
    { startIdx: int
      endIdx: int
      name: string
      typ: string }

type CompletionEntry =
    { name: string
      typ: string
      kind: string }

type ErrorRange =
    { startIdx: int; endIdx: int }

type CompileInfo =
    { error: string
      errorRange: ErrorRange option
      hovers: HoverInfo array
      completions: CompletionEntry array
      // The whole program's inferred result type, when type-checking
      // succeeds — empty string on parse / type errors.
      resultType: string }

type EvalInfo =
    { csharp: string
      result: string
      output: string
      error: string }


// ─── Stdlib (TypeFighter source, prepended invisibly to user input) ──
// Stream-ops live here as ordinary library functions, NOT as solver
// builtins. They produce a structural record `{ __machine: .. }` that
// the emit recognises as a state-machine step. Anything that returns
// the same record shape is compiled the same way — useMemo and getCtx
// have no special status beyond what's defined here.
//
// Position offset: stdlib occupies characters 0..stdlibLen-1 in the
// augmented source. User-visible positions are shifted by stdlibLen.
// Errors/hovers are translated back before reaching the frontend.
// `val: a` (instead of `val: s`) ties the `step`'s value type to the
// initial-value type, so `useMemo(42)` correctly resolves V to Number
// rather than leaving it as a free variable. Semantically identical for
// useMemo (state never changes, so `s` and the captured `a` are equal).
//
// `count` is a plain stream value (not a function) — `i = count` lifts
// `i` to Number and yields 0, 1, 2, ... per tick.
//
// `now` is a stream value yielding the current sample time (seconds since
// the Unix epoch) per tick; in a VirtualCurves sampler each tick is a real
// sample instant, so `let t0 = useMemo(now); now - t0` gives elapsed seconds.
//
// stdlib carries explicit `;` (no preprocessing) so position offsets to
// user source are stable; user source is line-based and gets newlines
// translated to `;` by `preprocessNewlines` below.
let private stdlibSource = """useMemo = a => { __machine: { init: a, step: s => { val: s, nextState: s } } };
count = { __machine: { init: 0, step: s => { val: s, nextState: s + 1 } } };
now = { __machine: { init: 0, step: s => { val: __nowSeconds(UnitValue), nextState: 0 } } };
"""

let private stdlibLen = stdlibSource.Length

let private prependStdlib (source: string) =
    stdlibSource + Parser01.Syntax.preprocessNewlines source

// The stdlib prefix, PARSED: binding names and count come from the source
// itself (single source of truth -- editors must not duplicate the list).
// The desugar pass prunes unused bindings per request (the solver is
// superlinear in constraint count, so an unpruned stdlib costs whole
// seconds per keystroke once the block library is in).
let private stdlibBindingNames =
    match Parser01.Syntax.parse (stdlibSource + "0") with
    | Ok expr -> Desugar.topLetNames expr
    | Error err -> failwithf "stdlib does not parse: %s" err.message

let private stdlibBindingCount = List.length stdlibBindingNames

// Build the desugar oracle from the prelude: stream classification per
// binding, callable names (for UFCS), record bindings with known fields
// (host records), and declared step-record parameter positions (the "don't hoist"
// exception for stream-consuming prelude fns).
let private mkOracle (prelude: (string * Typ) list) : Desugar.Oracle =
    let monoOf typ = match typ with Mono m -> m | Poly p -> p.monoTyp
    let kinds =
        prelude
        |> List.choose (fun (name, typ) ->
            match StepRecord.bindingKindOfTyp typ with
            | StepRecord.BindingKind.Plain -> None
            | kind -> Some (name, kind))
        |> Map.ofList
    let callables =
        prelude
        |> List.choose (fun (name, typ) ->
            match monoOf typ with FunTyp _ -> Some name | _ -> None)
        |> Set.ofList
    let streamParams =
        prelude
        |> List.choose (fun (name, typ) ->
            let rec walk t i acc =
                match t with
                | FunTyp (p, ret) ->
                    let acc = if BuiltinTypes.isStepRecord p then Set.add i acc else acc
                    walk ret (i + 1) acc
                | _ -> acc
            let positions = walk (monoOf typ) 0 Set.empty
            if positions.IsEmpty then None else Some (name, positions))
        |> Map.ofList
    { kinds = kinds
      callables = callables
      streamParams = streamParams
      namespaceMembers = Web.Builtins.namespaceMembers }

// Apply the desugar pass (UFCS + stream-argument hoisting) and stdlib
// pruning to a parsed augmented chain. Returns the rewritten expr, the
// number of kept stdlib bindings, and a range resolver that follows the
// (new -> old) node remap so squiggles/hovers keep working.
let private desugarAndPrune
    (prelude: (string * Typ) list)
    (expr: Expr<unit>)
    (resolveRange: Expr<unit> -> TheBlunt.Range option)
    =
    let desugared = Desugar.run (mkOracle prelude) expr
    let pruned, keptStdlibCount =
        Desugar.pruneStdlibBindings stdlibBindingCount desugared.remap desugared.expr
    let resolveRange' (e: Expr<unit>) =
        let mutable cur = box e
        let mutable guard = 0
        while desugared.remap.ContainsKey cur && guard < 64 do
            cur <- desugared.remap.[cur]
            guard <- guard + 1
        resolveRange (cur :?> Expr<unit>)
    pruned, keptStdlibCount, resolveRange'

// ─── Prelude ──────────────────────────────────────────────────────
// Shipped with the TF language. The built-in set (types + runtime closures)
// is defined once in `Builtins`; hosts inject extra bindings via the
// `extraPrelude` parameter (e.g. a host injects `values`). useMemo / getCtx are
// NOT here -- they live in the stdlib below as ordinary library functions
// producing a step-record the emit recognises structurally.

let private fullPrelude (extraPrelude: (string * Typ) list) : (string * Typ) list =
    Web.Builtins.prelude @ extraPrelude

let private fieldsOf (typ: Typ) : string array =
    let mono =
        match typ with
        | Mono m -> m
        | Poly p -> p.monoTyp
    match mono with
    | RecordTyp r ->
        r.fields |> Seq.map (fun f -> f.fname) |> Seq.sortBy id |> Array.ofSeq
    | _ -> [||]

let preludeInfo (extraPrelude: (string * Typ) list) : PreludeEntry array =
    [|
        for name, typ in fullPrelude extraPrelude ->
            { name = name; typ = string typ; fields = fieldsOf typ }
        // stdlib bindings (useMemo, count, now, ...): names straight from the
        // parsed stdlib prefix; their types are inference results, so the typ
        // field carries the marker editors key off.
        for name in stdlibBindingNames ->
            { name = name; typ = "(stdlib)"; fields = [||] }
    |]


/// Field names accessed via `<bindingName>.<field>` anywhere in the source.
/// Walks the parsed AST -- no regex, no string scanning. Empty list if the
/// source doesn't parse. The source gets the same newline-to-`;` translation
/// as the real compile pipeline -- multi-statement sources without explicit
/// semicolons would otherwise fail the parse and silently yield [].
let extractFieldsAccessedOn (bindingName: string) (source: string) : string list =
    match Parser01.Syntax.parseWithSpans (Parser01.Syntax.preprocessNewlines source) with
    | Error _ -> []
    | Ok (expr, _) ->
        IdentInfo.collectVarPropAccs expr
        |> List.filter (fun (varName, _) -> varName = bindingName)
        |> List.map snd
        |> List.distinct


let private spanName (sk: Parser01.Syntax.SpanKind) =
    match sk with
    | Parser01.Syntax.VarRef n
    | Parser01.Syntax.BindingDef n
    | Parser01.Syntax.FunParam n
    | Parser01.Syntax.PropField n -> n


type private AnalysisResult =
    { expr: Expr<VarNum> option
      rootType: MonoTyp option
      // VarNum -> resolved monotyp; lets the emitter pick typed C# code paths
      // (e.g. direct `TfBuiltins.op_add(double, double)` instead of curried
      // Func<object,object>).
      tvarToMono: VarNum -> MonoTyp option
      error: string
      errorRange: ErrorRange option
      hovers: HoverInfo array
      completions: CompletionEntry array }

let private analyze (extraPrelude: (string * Typ) list) (source: string) : AnalysisResult =
    let prelude = fullPrelude extraPrelude
    let preludeMap =
        prelude |> List.map (fun (name, typ) -> name, string typ) |> Map.ofList
    let preludeCompletions =
        [| for name, typ in prelude ->
             { name = name; typ = string typ; kind = "prelude" } |]

    // Translate positions from augmented source back to the user's view.
    // Anything that landed inside the stdlib prefix is hidden from the UI.
    let toUserIdx (augIdx: int) = max 0 (augIdx - stdlibLen)
    let inUserRegion (augIdx: int) = augIdx >= stdlibLen
    let augSource = prependStdlib source

    match Parser01.Syntax.parseWithSpansAndRanges augSource with
    | Error err ->
        // Parse error in the stdlib would be a compiler bug — surface it.
        let userIdx = toUserIdx err.idx
        let endIdx = min (userIdx + 1) source.Length
        { expr = None
          rootType = None
          tvarToMono = fun _ -> None
          error = err.message
          errorRange = Some { startIdx = userIdx; endIdx = endIdx }
          hovers = [||]
          completions = preludeCompletions }
    | Ok (expr, spans, resolveRange) ->
        // Desugar (UFCS + stream-argument hoisting) and prune unused stdlib
        // bindings; solver AND emit both consume the rewritten chain.
        let expr, keptStdlibCount, resolveRange = desugarAndPrune prelude expr resolveRange
        let env = [ for name, typ in prelude -> name, typ ]
        // Solver throws on unresolved identifiers (e.g. user just typed
        // `a` and `a` isn't bound). Treat that as a normal type error.
        let solveResult =
            try Ok (Solver.solveWithRanges env None expr resolveRange)
            with ex -> Error ex.Message
        match solveResult with
        | Error err ->
            { expr = None
              rootType = None
              tvarToMono = fun _ -> None
              error = sprintf "Type error: %s" err
              errorRange = None
              hovers = [||]
              completions = preludeCompletions }
        | Ok solution ->

        let tvarToTyp : VarNum -> string option =
            match solution.result with
            | Ok r ->
                let m =
                    r.substitutions
                    |> List.map (fun s -> s.tvar, string s.typ)
                    |> Map.ofList
                fun tvar -> Map.tryFind tvar m
            | Error _ -> fun _ -> None

        let tvarToMono : VarNum -> MonoTyp option =
            match solution.result with
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

        // User-region occurrences only: the stdlib prefix (kept bindings
        // included) is skipped so its identifiers can't contaminate the
        // order-based span-to-occurrence pairing below.
        let occurrences = IdentInfo.collectSkippingTopLets keptStdlibCount solution.numberedExpr
        let occBuckets =
            occurrences
            |> List.groupBy (fun o -> o.name, o.kind)
            |> List.map (fun (k, xs) -> k, ref xs)
            |> Map.ofList

        let pickOccurrence (name: string) (kind: IdentInfo.IdentKind) =
            match Map.tryFind (name, kind) occBuckets with
            | Some cell ->
                match cell.Value with
                | head :: rest ->
                    cell.Value <- rest
                    Some head
                | [] -> None
            | None -> None

        let orderedSpans =
            spans |> List.sortBy (fun s -> s.range.startIdx, s.range.endIdx)

        // Range-based fallback: nodes the desugar pass rewrote (namespace
        // members like `math.clamp` -> Var __math_clamp) have no occurrence
        // for the original span, but the rewritten node's tvar carries the
        // ORIGINAL range via the remap. Smallest containing range wins.
        let rangedTvars =
            solution.tvarRanges
            |> Map.toList
            |> List.map (fun (tv, r) -> r.startIdx, r.endIdx, tv)
        let typByRange (s: Parser01.Syntax.Span) =
            rangedTvars
            |> List.filter (fun (lo, hi, _) -> lo <= s.range.startIdx && s.range.endIdx <= hi)
            |> List.sortBy (fun (lo, hi, _) -> hi - lo)
            |> List.tryPick (fun (_, _, tv) -> tvarToTyp tv)

        let hovers =
            orderedSpans
            |> List.filter (fun s -> inUserRegion s.range.startIdx)
            |> List.map (fun s ->
                let name = spanName s.kind
                let kindOpt =
                    match s.kind with
                    | Parser01.Syntax.VarRef _ -> Some IdentInfo.Var
                    | Parser01.Syntax.BindingDef _ -> Some IdentInfo.Binding
                    | Parser01.Syntax.FunParam _ -> Some IdentInfo.Param
                    | Parser01.Syntax.PropField _ -> Some IdentInfo.Field
                let solvedTyp =
                    match kindOpt |> Option.bind (pickOccurrence name) with
                    | Some occ -> tvarToTyp occ.tvar
                    | None -> None
                let typ =
                    match solvedTyp with
                    | Some t -> t
                    | None ->
                        match s.kind with
                        | Parser01.Syntax.VarRef n ->
                            defaultArg (Map.tryFind n preludeMap) ""
                        | _ -> defaultArg (typByRange s) ""
                { startIdx = toUserIdx s.range.startIdx
                  endIdx = toUserIdx s.range.endIdx
                  name = name
                  typ = typ })
            |> Array.ofList

        let userBindings =
            spans
            |> List.choose (fun s ->
                match s.kind with
                | Parser01.Syntax.BindingDef name -> Some name
                | _ -> None)
            |> List.distinct
            |> List.filter (fun n -> not (Map.containsKey n preludeMap))
            |> List.map (fun name ->
                let typ =
                    occurrences
                    |> List.tryFind (fun o -> o.name = name && o.kind = IdentInfo.Binding)
                    |> Option.bind (fun o -> tvarToTyp o.tvar)
                    |> Option.defaultValue ""
                { name = name; typ = typ; kind = "binding" })
            |> Array.ofList

        let completions = Array.append preludeCompletions userBindings

        // Solver tracks the offending tvar on a unification error; combined
        // with the parser's per-tvar range map this gives us a precise editor
        // squiggle position for type errors.
        let typeErrorRange =
            solution.errorTVar
            |> Option.bind (fun tv -> Map.tryFind tv solution.tvarRanges)
            |> Option.filter (fun (r: TheBlunt.Range) -> inUserRegion r.startIdx)
            |> Option.map (fun (r: TheBlunt.Range) ->
                { startIdx = toUserIdx r.startIdx; endIdx = toUserIdx r.endIdx })

        // Root mono type for downstream emit decisions (e.g. step-record-typed
        // blocks get the state-machine emit path).
        let rootType =
            match solution.result with
            | Ok r ->
                match r.typ with
                | Some (Mono m) -> Some m
                | Some (Poly p) -> Some p.monoTyp
                | None -> None
            | Error _ -> None

        match solution.result with
        | Error err ->
            { expr = None
              rootType = rootType
              tvarToMono = tvarToMono
              error = sprintf "Type error: %s" err
              errorRange = typeErrorRange
              hovers = hovers
              completions = completions }
        | Ok _ ->
            { expr = Some solution.numberedExpr
              rootType = rootType
              tvarToMono = tvarToMono
              error = ""
              errorRange = None
              hovers = hovers
              completions = completions }


let compileInfo (extraPrelude: (string * Typ) list) (source: string) : CompileInfo =
    let r = analyze extraPrelude source
    let resultType =
        r.rootType
        |> Option.map (fun t -> string t)
        |> Option.defaultValue ""
    { error = r.error
      errorRange = r.errorRange
      hovers = r.hovers
      completions = r.completions
      resultType = resultType }


// Pulls the receiver chain (`<expr>` left of the last `.`) and the
// statement prefix (everything up to the last `;`) out of source-up-to-
// cursor. Returns None if the cursor isn't in `<expr>.<tail>` position.
let private extractDotContext (sourceUpToCursor: string) : (string * string) option =
    let lastSemi = sourceUpToCursor.LastIndexOf(';')
    let stmtPrefix =
        if lastSemi >= 0 then sourceUpToCursor.Substring(0, lastSemi + 1) else ""
    let stmt =
        if lastSemi >= 0 then sourceUpToCursor.Substring(lastSemi + 1) else sourceUpToCursor
    // Greedy chain of `id` or backtick-quoted `id`, then required `.`,
    // then optional partial (word chars or backtick-prefix). Partial must
    // be glued to the cursor — no whitespace/newlines.
    let pattern =
        @"((?:[A-Za-z_]\w*|`[^`]+`)(?:\.(?:[A-Za-z_]\w*|`[^`]+`))*)\.(`[^`]*|\w*)$"
    let m = System.Text.RegularExpressions.Regex.Match(stmt, pattern)
    if m.Success then Some (stmtPrefix, m.Groups.[1].Value) else None

let completionAt
    (extraPrelude: (string * Typ) list)
    (stmtPrefix: string)
    (baseExpr: string)
    : string array
    =
    // Through the same pipeline as analyze: stdlib prefix (so a stmtPrefix
    // using stream fns still solves), desugar, prune. Offsets don't matter
    // here -- only field names are returned.
    let synthSource =
        prependStdlib (sprintf "%s let __probe = %s; __probe" stmtPrefix baseExpr)
    match Parser01.Syntax.parseWithSpans synthSource with
    | Error _ -> [||]
    | Ok (expr, _) ->
        let prelude = fullPrelude extraPrelude
        let desugared = Desugar.run (mkOracle prelude) expr
        let expr, _ =
            Desugar.pruneStdlibBindings stdlibBindingCount desugared.remap desugared.expr
        let env = [ for name, typ in prelude -> name, typ ]
        let solveResult = try Ok (Solver.solve env None expr) with _ -> Error ()
        match solveResult with
        | Error _ -> [||]
        | Ok solution ->
        match solution.result with
        | Error _ -> [||]
        | Ok r ->
            let probeTvar =
                IdentInfo.collect solution.numberedExpr
                |> List.tryFindBack (fun o -> o.name = "__probe" && o.kind = IdentInfo.Var)
                |> Option.map (fun o -> o.tvar)
            match probeTvar with
            | None -> [||]
            | Some tvar ->
                let resolved =
                    r.substitutions
                    |> List.tryFind (fun s -> s.tvar = tvar)
                    |> Option.map (fun s -> s.typ)
                let mono =
                    match resolved with
                    | Some (Mono m) -> Some m
                    | Some (Poly p) -> Some p.monoTyp
                    | None -> None
                match mono with
                | Some (RecordTyp record) ->
                    record.fields
                    |> Seq.map (fun f -> f.fname)
                    |> Seq.sort
                    |> Array.ofSeq
                | _ -> [||]


// (stmtPrefix, baseExpr) -> field array. Solver cost is dominated by the
// 13650-field record reindex; same key hit twice = same answer. Process-
// lifetime cache keyed by reference identity of `extraPrelude` so a host
// rebuild (e.g. a host value refresh) automatically invalidates.
let private completionCache =
    System.Collections.Concurrent.ConcurrentDictionary<obj * string * string, string array>(
        HashIdentity.Structural)

/// Single entry point used by the controller: takes raw source + cursor
/// offset, decides whether the cursor is in dot-position, and returns
/// the field list. No frontend logic, no host-specific shortcuts.
let completionAtCursor
    (extraPrelude: (string * Typ) list)
    (source: string)
    (offset: int)
    : string array
    =
    let cursor = max 0 (min offset source.Length)
    let before = source.Substring(0, cursor)
    match extractDotContext before with
    | None -> [||]
    | Some (stmtPrefix, baseExpr) ->
        let key = (extraPrelude :> obj), stmtPrefix, baseExpr
        match completionCache.TryGetValue(key) with
        | true, fs -> fs
        | _ ->
            let fs = completionAt extraPrelude stmtPrefix baseExpr
            completionCache.[key] <- fs
            fs


/// Manual eval. Caller supplies any host-specific runtime bindings.
let evalSource
    (extraPrelude: (string * Typ) list)
    (extraBindings: Eval.ExtraBinding list)
    (source: string)
    : EvalInfo
    =
    let r = analyze extraPrelude source
    match r.expr, r.rootType with
    | None, _ | _, None ->
        { csharp = ""; result = ""; output = ""; error = r.error }
    | Some expr, Some rootType ->
        let evald = Eval.evalExpr extraBindings source rootType r.tvarToMono expr
        { csharp = evald.csharp
          result = evald.result
          output = evald.output
          error = evald.error }


/// Compile `source` ONCE to a stateful sampler for VirtualCurves. The host
/// passes the values binding shape via `extraBindings`; the host values are fed
/// per `Step` call (they change per sample instant). `Step` is null on error.
let compileSamplerFromSource
    (extraPrelude: (string * Typ) list)
    (extraBindings: Eval.ExtraBinding list)
    (source: string)
    : Eval.CompiledSampler
    =
    let r = analyze extraPrelude source
    match r.expr, r.rootType with
    | None, _ | _, None ->
        { csharp = ""; step = null; error = r.error }
    | Some expr, Some rootType ->
        Eval.compileSampler extraBindings source rootType r.tvarToMono expr
