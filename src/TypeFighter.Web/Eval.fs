// Compile a TypeFighter program to C#, run it through Roslyn, capture
// stdout, return the result value plus any output. Pure server-side.
// Generic: callers inject host-specific bindings (e.g. host value records).
module PureState.TypeFighter.Web.Eval

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open PureState.TypeFighter

type EvalResult =
    {
        csharp: string
        result: string
        output: string
        error: string
    }

/// One host-injected binding. The user-side ident is bound inside the
/// compiled C# body via `csInitExpr`; the C# class's `Eval` method gets
/// an extra parameter `csParamName: csParamType`; at invoke time the
/// `runtimeValue` is passed in for that parameter.
type ExtraBinding =
    {
        name: string
        csParamName: string
        csParamType: string
        csInitExpr: string
        runtimeValue: obj
    }

// Typed builtins library, compiled alongside every user program. The emitter's
// typed fast-path (`EmitCSharp`) routes fully-applied, type-resolved
// builtin calls straight to these named overloads (e.g. `TfBuiltins.op_add(
// double, double)`) instead of the curried Func<object,object> closures.
// AggressiveInlining lets the JIT fold them into the generated `Eval` body.
// The curried closures (see `Builtins.csImports`) remain the fallback for
// partial application, unresolved operand types, and first-class refs.
//
// Fully-qualified names throughout -- this source is appended to the user
// class which already has the usual `using` directives at the top, and C#
// disallows `using` after type declarations.
let private tfBuiltinsSource = """
public static class TfBuiltins
{
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static double op_add(double a, double b) => a + b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static string op_add(string a, string b) => a + b;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static double op_sub(double a, double b) => a - b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static double op_mul(double a, double b) => a * b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static double op_div(double a, double b) => a / b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static double op_neg(double a) => -a;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_lt(double a, double b) => a < b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_lt(string a, string b) => string.Compare(a, b) < 0;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_gt(double a, double b) => a > b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_gt(string a, string b) => string.Compare(a, b) > 0;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_le(double a, double b) => a <= b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_le(string a, string b) => string.Compare(a, b) <= 0;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_ge(double a, double b) => a >= b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_ge(string a, string b) => string.Compare(a, b) >= 0;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_eq<T>(T a, T b) => System.Collections.Generic.EqualityComparer<T>.Default.Equals(a, b);
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_neq<T>(T a, T b) => !System.Collections.Generic.EqualityComparer<T>.Default.Equals(a, b);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_and(bool a, bool b) => a && b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_or(bool a, bool b) => a || b;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool op_not(bool a) => !a;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static string concat(string a, string b) => a + b;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static string ToString<T>(T x) => x is null ? "" : x.ToString();

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T head<T>(System.Collections.Generic.IReadOnlyList<T> xs) => xs[0];
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static double length<T>(System.Collections.Generic.IReadOnlyList<T> xs) => (double)xs.Count;
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T at<T>(System.Collections.Generic.IReadOnlyList<T> xs, double i) => xs[(int)i];

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static object log<T>(System.IO.TextWriter @out, T s)
    {
        @out.WriteLine(s is null ? "" : s.ToString());
        return null;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T cond<T>(bool b, T t, T e) => b ? t : e;
}
"""

// Built-in runtime closures live in `Builtins` alongside their type
// signatures (single source of truth). `op_add` and the comparisons
// dispatch on the actual runtime type (Number|String).
let private defaultCsImports : (string * string) list = Web.Builtins.csImports

// Lazy: hosts may install Roslyn.setReferenceOverride before the first
// compile (browser WASM); an eager module value would resolve too early.
let private references : Lazy<Microsoft.CodeAnalysis.MetadataReference list> =
    lazy (
        Roslyn.baseReferences ()
        @ Roslyn.referencesFor [
            typeof<obj>
            typeof<Dictionary<string, obj>>
            typeof<TextWriter>
            typeof<Func<obj, obj>>
            // imported stream blocks: the generated C# calls the DspRt
            // adapter (this assembly), which fronts the vendored DSP
            typeof<DspRt>
          ])

let private renderResult (v: obj) : string =
    match v with
    | null -> "null"
    | :? string as s -> s
    | :? double as d -> d.ToString(System.Globalization.CultureInfo.InvariantCulture)
    | :? bool as b -> if b then "true" else "false"
    | :? IDictionary<string, obj> as dict ->
        dict |> Seq.map (fun kv -> sprintf "%s: %A" kv.Key kv.Value)
             |> String.concat ", " |> sprintf "{ %s }"
    | :? (obj[]) as arr ->
        arr |> Array.map (fun x -> sprintf "%A" x) |> String.concat ", " |> sprintf "[%s]"
    | :? IReadOnlyList<obj> as xs ->
        xs |> Seq.map (fun x -> sprintf "%A" x) |> String.concat ", " |> sprintf "[%s]"
    // Catch-all for value-type lists like IReadOnlyList<double>, which
    // aren't covariant with IReadOnlyList<obj>. Without this, F#'s default
    // `%A` printer kicks in and truncates the seq to 4 elements + ellipsis.
    | :? System.Collections.IEnumerable as e ->
        let items =
            [ for x in e -> sprintf "%A" x ]
        items |> String.concat ", " |> sprintf "[%s]"
    | other -> sprintf "%A" other

let evalExpr
    (extras: ExtraBinding list)
    (source: string)
    (rootType: MonoTyp)
    (tvarToMono: VarNum -> MonoTyp option)
    (expr: Expr<VarNum>)
    : EvalResult
    =
    // Builtins that the emit fully dispatches into TfBuiltins (e.g. op_add
    // when both operands are Number) don't need a Func<object,object> shim
    // in the prelude header. Keep the shim only when the name appears as a
    // first-class value (passed as an argument, returned, stored, etc.).
    let neededBuiltinRefs =
        EmitCSharp.collectFirstClassRefs tvarToMono expr
    let prunedDefaults =
        defaultCsImports
        |> List.filter (fun (name, _) -> Set.contains name neededBuiltinRefs)
    let csImports = prunedDefaults @ [ for x in extras -> x.name, x.csInitExpr ]
    let extraParams = [ for x in extras -> x.csParamName, x.csParamType ]
    let className = Roslyn.classNameFor "TfWeb" source
    // Step-record-typed roots get a flat C# state-machine; everything else
    // falls through to the standard expression-bodied emit.
    let userClass =
        match EmitStreamMachine.tryEmit className csImports extraParams rootType tvarToMono expr with
        | Some sm -> sm
        | None -> EmitCSharp.toCSharpClass className csImports extraParams tvarToMono expr
    // Append the typed builtins library so Roslyn compiles both classes into
    // the same assembly; the user class calls `TfBuiltins.op_*` directly on the
    // typed fast-path. Appended (not prepended) because C# disallows `using`
    // directives after type declarations.
    let csharp = userClass + "\n" + tfBuiltinsSource
    try
        let mi = Roslyn.compileFunction className csharp references.Value
        use sw = new StringWriter()
        let invokeArgs : obj array =
            [|
                yield box (sw :> TextWriter)
                for x in extras -> x.runtimeValue
            |]
        let value =
            try
                mi.Invoke(null, invokeArgs)
            with
            | :? TargetInvocationException as ex when not (isNull ex.InnerException) ->
                raise ex.InnerException
        {
            csharp = csharp
            result = renderResult value
            output = sw.ToString()
            error = ""
        }
    with
    | :? Roslyn.CompileException as ex ->
        { csharp = csharp; result = ""; output = ""; error = ex.Message }
    | ex ->
        { csharp = csharp; result = ""; output = ""; error = ex.Message }


/// A compiled, STATEFUL sampler. `Step.Invoke(now, hostValues)` advances the stream
/// by one tick at sample time `now` (with the host values supplied for that
/// instant) and yields the emitted value (null = gap). State persists across
/// calls in the underlying class instance, so each call is one tick at a real
/// sample time. `Step` is null when `error` is non-empty.
type CompiledSampler =
    {
        csharp: string
        step: Func<DateTimeOffset, IDictionary<string, obj>, Nullable<double>>
        error: string
    }

/// Compile a step-record TypeFighter program ONCE to a reusable sampler.
/// The root must be a stream (step-record); otherwise returns an error.
/// `extras` carry the host bindings' compile-time shape (csParamName/Type/Init);
/// their `runtimeValue` is ignored here -- the host values dictionary is passed per
/// `step` call instead, because it changes per sample instant.
let compileSampler
    (extras: ExtraBinding list)
    (source: string)
    (rootType: MonoTyp)
    (tvarToMono: VarNum -> MonoTyp option)
    (expr: Expr<VarNum>)
    : CompiledSampler
    =
    let neededBuiltinRefs =
        EmitCSharp.collectFirstClassRefs tvarToMono expr
    let prunedDefaults =
        defaultCsImports
        |> List.filter (fun (name, _) -> Set.contains name neededBuiltinRefs)
    let csImports = prunedDefaults @ [ for x in extras -> x.name, x.csInitExpr ]
    let extraParams = [ for x in extras -> x.csParamName, x.csParamType ]
    let className = Roslyn.classNameFor "TfSampler" source
    let noStep : Func<DateTimeOffset, IDictionary<string, obj>, Nullable<double>> = null
    // Step-record root -> stateful state-machine sampler; any other expression
    // -> stateless sampler that re-evaluates over the host values at each instant.
    let userClass =
        match EmitStreamMachine.tryEmitSampler className csImports extraParams rootType tvarToMono expr with
        | Some sm -> sm
        | None -> EmitStreamMachine.emitStatelessSampler className csImports extraParams tvarToMono expr
    let csharp = userClass + "\n" + tfBuiltinsSource
    try
        let typ = Roslyn.compileType className csharp references.Value
        let instance = System.Activator.CreateInstance typ
        let stepMi = typ.GetMethod "Step"
        let step =
            Func<DateTimeOffset, IDictionary<string, obj>, Nullable<double>>(fun now hostValues ->
                // A missing input / runtime error at one instant is a gap in the
                // curve, not a crash of the whole sampling loop.
                try
                    match stepMi.Invoke(instance, [| box now; box hostValues |]) with
                    | :? double as d -> if Double.IsNaN d then Nullable() else Nullable d
                    | :? int as i -> Nullable(double i)
                    | _ -> Nullable()
                with _ -> Nullable())
        { csharp = csharp; step = step; error = "" }
    with
    | :? Roslyn.CompileException as ex -> { csharp = csharp; step = noStep; error = ex.Message }
    | ex -> { csharp = csharp; step = noStep; error = ex.Message }
