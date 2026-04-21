module TypeFighter.Web.Api

open TypeFighter

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


// ─── Prelude ──────────────────────────────────────────────────────

let private prelude : (string * Typ * string) list =
    [
        "log",
        Mono (BuiltinTypes.string ^-> BuiltinTypes.unit),
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
