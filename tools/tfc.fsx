// TypeFighter → JavaScript transpiler (CLI).
//
// Usage:
//   dotnet fsi tools/tfc.fsx <input.tf> [--out <output.js>]
//
// Reads a TypeFighter source file, runs the type checker, and emits a
// self-contained JavaScript module. Without --out, writes to stdout.
//
// Requires TypeFighter and TypeFighter.Parser01 to be built first:
//   dotnet build TypeFighter.sln

#r "../src/TypeFighter/bin/Debug/net10.0/TypeFighter.dll"
#r "../src/TypeFighter.Parser01/bin/Debug/net10.0/TypeFighter.Parser01.dll"

open System
open System.IO
open TypeFighter


// ─── Prelude ──────────────────────────────────────────────────────
//
// The minimal env that `console.log`-style programs need. Each entry
// is a triple of (name, TypeFighter type, JS implementation string).
// The type is consumed by the solver; the JS string is wired in by
// the emitter via `Emit.toJsModule`.

let prelude : (string * Typ * string) list =
    [
        // log : String -> Unit
        "log",
        Mono (BuiltinTypes.string ^-> BuiltinTypes.unit),
        "(s) => (console.log(s), undefined)"

        // concat : String -> String -> String
        "concat",
        Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string),
        "(a) => (b) => a + b"

        // UnitValue : Unit
        BuiltinValues.unitValueIdent,
        Mono BuiltinTypes.unit,
        "undefined"

        // toString : forall a. a -> String
        BuiltinValues.toStringFunctionIdent,
        TDef.Generalize (%0 ^-> BuiltinTypes.string),
        "(x) => String(x)"
    ]


// ─── Args ─────────────────────────────────────────────────────────

type Args =
    { Input: string
      Output: string option }

let parseArgs (argv: string[]) =
    let rec loop i (input: string option) (output: string option) =
        if i >= argv.Length then
            match input with
            | Some path -> Ok { Input = path; Output = output }
            | None -> Error "No input file given."
        elif argv.[i] = "--out" || argv.[i] = "-o" then
            if i + 1 >= argv.Length then Error "--out requires a path."
            else loop (i + 2) input (Some argv.[i + 1])
        elif argv.[i].StartsWith "--" then
            Error (sprintf "Unknown flag: %s" argv.[i])
        else
            match input with
            | None -> loop (i + 1) (Some argv.[i]) output
            | Some _ -> Error (sprintf "Unexpected extra argument: %s" argv.[i])
    loop 0 None None


// ─── Pipeline ─────────────────────────────────────────────────────

let compile (source: string) =
    match Parser01.Syntax.parse source with
    | Error err -> Error err
    | Ok expr ->
        let env =
            [ for name, typ, _ in prelude -> name, typ ]
        let solution = Solver.solve env None expr
        match solution.result with
        | Error err -> Error (sprintf "Type error: %s" err)
        | Ok _ ->
            let imports =
                [ for name, _, jsImpl in prelude -> name, jsImpl ]
            Ok (Emit.toJsModule imports expr)


// ─── Main ─────────────────────────────────────────────────────────

// Skip the script path AND any FSI-injected flags (e.g. --preferreduilang).
let argv =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> Array.filter (fun a -> not (a.StartsWith "--preferreduilang"))

match parseArgs argv with
| Error msg ->
    eprintfn "%s" msg
    eprintfn "Usage: dotnet fsi tools/tfc.fsx <input.tf> [--out <output.js>]"
    exit 1
| Ok args ->
    if not (File.Exists args.Input) then
        eprintfn "Input file not found: %s" args.Input
        exit 1
    let source = File.ReadAllText args.Input
    match compile source with
    | Error err ->
        eprintfn "%s" err
        exit 1
    | Ok js ->
        match args.Output with
        | Some path ->
            File.WriteAllText(path, js)
            eprintfn "Wrote %s (%d bytes)" path js.Length
        | None ->
            printfn "%s" js
