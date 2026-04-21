module TypeFighter.Web.Api

open TypeFighter
open TypeFighter.Parser01

/// Result of a single compile attempt. One field carries the
/// emitted JavaScript, the other the error message — exactly one
/// of them is non-null at a time. The shape is chosen for easy
/// JavaScript consumption (no discriminated unions at the boundary).
type CompileResult =
    { js: string
      error: string }

/// User-facing description of one prelude binding — what name is in
/// scope and what its type signature looks like when rendered.
type PreludeEntry =
    { name: string
      typ: string }

/// Hover info for one source span. `startIdx`/`endIdx` are character
/// offsets into the source string; the UI converts them to line/column.
/// `typ` is the rendered type signature when available, otherwise "".
type HoverInfo =
    { startIdx: int
      endIdx: int
      name: string
      typ: string }

/// Entry offered by completion. `kind` is "prelude" or "binding" so the
/// UI can show an icon. `typ` is the rendered type (empty for bindings
/// until we have externally-computed types).
type CompletionEntry =
    { name: string
      typ: string
      kind: string }

/// Richer compile result with IntelliSense data alongside the emitted JS.
/// `hovers` carries per-identifier type info; `completions` carries
/// names in scope anywhere in the program (prelude + user bindings).
/// Types for user bindings/usages come from zipping parser spans (source
/// ranges) with the numbered-AST walk from `IdentInfo.collect` — both
/// lists are produced in document order, so matching by
/// `(name, kind, nth-by-name)` is unambiguous even with shadowing.
type CompileInfo =
    { js: string
      error: string
      hovers: HoverInfo array
      completions: CompletionEntry array }


// ─── Prelude ──────────────────────────────────────────────────────

let private prelude : (string * Typ * string) list =
    [
        "log",
        TDef.Generalize (%0 ^-> BuiltinTypes.unit),
        "(s) => (console.log(s), undefined)"

        "concat",
        Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string),
        "(a) => (b) => a + b"

        BuiltinValues.unitValueIdent,
        Mono BuiltinTypes.unit,
        "undefined"

        BuiltinValues.toStringFunctionIdent,
        TDef.Generalize (%0 ^-> BuiltinTypes.string),
        "(x) => String(x)"
    ]


/// The identifiers that every compiled program has in scope, together
/// with their rendered type signatures. Exposed so the UI can surface
/// them without hard-coding the list on the JS side (which would rot).
let preludeInfo () : PreludeEntry array =
    [| for name, typ, _ in prelude ->
        { name = name; typ = string typ } |]


/// Compile a TypeFighter source string to a self-contained JavaScript
/// module. On type or parse error returns `{ js = ""; error = msg }`.
let compile (source: string) : CompileResult =
    match Parser01.Syntax.parse source with
    | Error err ->
        { js = ""; error = err }
    | Ok expr ->
        let env = [ for name, typ, _ in prelude -> name, typ ]
        let solution = Solver.solve env None expr
        match solution.result with
        | Error err ->
            { js = ""; error = sprintf "Type error: %s" err }
        | Ok _ ->
            let imports = [ for name, _, js in prelude -> name, js ]
            { js = Emit.toJsModule imports expr; error = "" }


// Span kinds (parser) and IdentInfo.IdentKind (AST walker) carry the
// same four semantic categories — we just need a way to match them up
// when zipping spans to occurrences.
let private spanKindMatches (sk: Parser01.Syntax.SpanKind) (ik: IdentInfo.IdentKind) =
    match sk, ik with
    | Parser01.Syntax.VarRef _, IdentInfo.Var -> true
    | Parser01.Syntax.BindingDef _, IdentInfo.Binding -> true
    | Parser01.Syntax.FunParam _, IdentInfo.Param -> true
    | Parser01.Syntax.PropField _, IdentInfo.Field -> true
    | _ -> false

let private spanName (sk: Parser01.Syntax.SpanKind) =
    match sk with
    | Parser01.Syntax.VarRef n
    | Parser01.Syntax.BindingDef n
    | Parser01.Syntax.FunParam n
    | Parser01.Syntax.PropField n -> n

/// Compile with accompanying IntelliSense data. Always returns hover
/// and completion arrays (possibly empty) so the UI can wire providers
/// unconditionally.
///
/// Hover: for every ident span we look up the occurrence's TVar in the
/// solver's substitutions to get a real type. Prelude names fall back
/// to their declared type signatures.
///
/// Completions: the prelude + every `BindingDef` name seen in the
/// source (deduped). No scope-awareness yet; the list is returned flat.
let compileWithInfo (source: string) : CompileInfo =
    let preludeMap =
        prelude
        |> List.map (fun (name, typ, _) -> name, string typ)
        |> Map.ofList

    let preludeCompletions =
        [| for name, typ, _ in prelude ->
             { name = name; typ = string typ; kind = "prelude" } |]

    match Parser01.Syntax.parseWithSpans source with
    | Error err ->
        { js = ""
          error = err
          hovers = [||]
          completions = preludeCompletions }
    | Ok (expr, spans) ->
        let env = [ for name, typ, _ in prelude -> name, typ ]
        let solution = Solver.solve env None expr

        // Substitution lookup: TVar → rendered Typ string. Only present
        // when the solver succeeded; a type error leaves us with just
        // the prelude-derived fallback.
        let tvarToTyp : VarNum -> string option =
            match solution.result with
            | Ok r ->
                let m =
                    r.substitutions
                    |> List.map (fun s -> s.tvar, string s.typ)
                    |> Map.ofList
                fun tvar -> Map.tryFind tvar m
            | Error _ -> fun _ -> None

        // Walk the numbered AST in document order. Bucket occurrences
        // by (name, kind) and hand them out in order when zipping with
        // spans, so shadowing and repeated usage stay unambiguous.
        let occurrences = IdentInfo.collect solution.numberedExpr
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

        // Spans may have been recorded in non-source order due to parser
        // backtracking; resort so the bucket-consumption order matches
        // the AST walk's document order.
        let orderedSpans =
            spans |> List.sortBy (fun s -> s.range.startIdx, s.range.endIdx)

        let hovers =
            orderedSpans
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
                        // Fall back to prelude signature for VarRefs of
                        // prelude names (handy when solver didn't run or
                        // didn't bind this specific usage's tvar).
                        match s.kind with
                        | Parser01.Syntax.VarRef n ->
                            defaultArg (Map.tryFind n preludeMap) ""
                        | _ -> ""
                { startIdx = s.range.startIdx
                  endIdx = s.range.endIdx
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
                // Look up the binding's inferred type via its Let-ident
                // tvar. Walker stored one Binding occurrence per let —
                // any remaining entry for this name gives us the type.
                let typ =
                    occurrences
                    |> List.tryFind (fun o -> o.name = name && o.kind = IdentInfo.Binding)
                    |> Option.bind (fun o -> tvarToTyp o.tvar)
                    |> Option.defaultValue ""
                { name = name; typ = typ; kind = "binding" })
            |> Array.ofList

        let completions = Array.append preludeCompletions userBindings

        match solution.result with
        | Error err ->
            { js = ""
              error = sprintf "Type error: %s" err
              hovers = hovers
              completions = completions }
        | Ok _ ->
            let imports = [ for name, _, js in prelude -> name, js ]
            { js = Emit.toJsModule imports expr
              error = ""
              hovers = hovers
              completions = completions }
