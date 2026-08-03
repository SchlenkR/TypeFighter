// Copyright (c) Ronald Schlenker
// Evaluation use only. Commercial use prohibited.

// Roslyn pipeline shared by every C# emit backend: wrap a C# source string,
// compile it in-memory, and hand back the `Eval` MethodInfo. Carries no
// TypeFighter AST knowledge -- emit modules build the source, this runs it.
module PureState.TypeFighter.Roslyn

open System
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

type CompileException(diagnostics: string) =
    inherit Exception(diagnostics)

let private parseOpts = CSharpParseOptions(LanguageVersion.Latest)

// concurrentBuild=false: Roslyn's parallel emit blocks on monitors, which
// throws on single-threaded hosts (browser WASM); sources are tiny anyway.
let private compileOpts =
    CSharpCompilationOptions(
        OutputKind.DynamicallyLinkedLibrary,
        optimizationLevel = OptimizationLevel.Release,
        concurrentBuild = false)

// Hosts without a real file system (browser WASM: Assembly.Location is empty)
// install a resolver that yields ALL references up front; the file-based
// defaults below then step aside. Configuration point, analog to the editor
// package's injected transport.
let mutable private referenceOverride : (unit -> MetadataReference list) option = None

let setReferenceOverride (f: unit -> MetadataReference list) =
    referenceOverride <- Some f

/// Default BCL refs every compile needs (System.Runtime, System.Linq, ...).
let baseReferences () : MetadataReference list =
    match referenceOverride with
    | Some f -> f ()
    | None ->
        let coreAsm = typeof<obj>.Assembly
        let trustedDir = Path.GetDirectoryName(coreAsm.Location)
        [
            "System.Runtime.dll"; "System.Private.CoreLib.dll"
            "netstandard.dll"; "System.Linq.dll"
        ]
        |> List.choose (fun n ->
            let p = Path.Combine(trustedDir, n)
            if File.Exists p then Some (MetadataReference.CreateFromFile p :> MetadataReference)
            else None)

/// Add `MetadataReference`s for every assembly hosting the supplied types.
/// Use this from callers to pin the runtime types the generated code needs.
/// With an active override the pool already covers everything -> empty.
let referencesFor (types: Type list) : MetadataReference list =
    match referenceOverride with
    | Some _ -> []
    | None ->
        types
        |> List.map (fun t -> MetadataReference.CreateFromFile t.Assembly.Location :> MetadataReference)

/// Stable per-source class name (32-bit hash of the source string).
let classNameFor (prefix: string) (source: string) : string =
    let h = uint32 (source.GetHashCode()) |> sprintf "%08x"
    prefix + "_" + h

/// Compile a full C# source string and return the named class's `Type`.
/// Throws `CompileException` on diagnostics.
let compileType
    (className: string)
    (source: string)
    (references: MetadataReference list)
    : System.Type
    =
    let tree = CSharpSyntaxTree.ParseText(source, parseOpts)
    let asmName = "TFGen_" + className
    let compilation =
        CSharpCompilation.Create(asmName, [ tree ], references, compileOpts)
    use ms = new MemoryStream()
    let result = compilation.Emit ms
    if not result.Success then
        let msg =
            result.Diagnostics
            |> Seq.filter (fun d -> d.Severity = DiagnosticSeverity.Error)
            |> Seq.map (fun d -> d.ToString())
            |> String.concat "\n"
        raise (CompileException("Roslyn compile failed:\n" + msg + "\n----- source -----\n" + source))
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    let asm = Assembly.Load(ms.ToArray())
    let typ = asm.GetType(className)
    if isNull typ then failwithf "compiled type '%s' not found" className
    typ

/// Compile a full C# source string and return the `Eval` MethodInfo on the
/// class named `className`. Throws `CompileException` on diagnostics.
let compileFunction
    (className: string)
    (source: string)
    (references: MetadataReference list)
    : MethodInfo
    =
    (compileType className source references).GetMethod("Eval")
