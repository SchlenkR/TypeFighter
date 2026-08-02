using System.Runtime.InteropServices.JavaScript;
using System.Runtime.Versioning;
using System.Text.Json;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using TfApi = PureState.TypeFighter.Web.Api;
using TfEval = PureState.TypeFighter.Web.Eval;
using TfRoslyn = PureState.TypeFighter.Roslyn;

namespace PureState.TypeFighter.Wasm;

// JS-facing facade over the TypeFighter compiler. Everything crosses the
// boundary as JSON strings whose shapes are IDENTICAL to the HTTP endpoints
// of AppDemoTypeFighterController (cSharp casing, errorRange: null) so the
// editor package works against either transport unchanged.
[SupportedOSPlatform("browser")]
public static partial class TypeFighterInterop
{
    static readonly JsonSerializerOptions jsonOptions = new(JsonSerializerDefaults.Web);

    // One static instance each: completionAtCursor caches per extraPrelude
    // REFERENCE, so the empty prelude must stay identity-stable.
    static readonly FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>> emptyPrelude =
        FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>>.Empty;
    static readonly FSharpList<TfEval.ExtraBinding> emptyBindings =
        FSharpList<TfEval.ExtraBinding>.Empty;

    record TfErrorRangeDto(int StartIdx, int EndIdx);
    record TfHoverDto(int StartIdx, int EndIdx, string Name, string Typ);
    record TfCompletionDto(string Name, string Typ, string Kind);
    record TfCompileInfoDto(
        string Error,
        TfErrorRangeDto? ErrorRange,
        IReadOnlyList<TfHoverDto> Hovers,
        IReadOnlyList<TfCompletionDto> Completions,
        string ResultType);
    record TfEvalDto(string CSharp, string Result, string Output, string Error);
    record TfPreludeEntryDto(string Name, string Typ, IReadOnlyList<string> Fields);
    record TfInitDto(string Version, int ReferenceCount, double WarmupMs, string WarmupResult, string Error);

    static string Json<T>(T value) => JsonSerializer.Serialize(value, jsonOptions);

    // Reference pool + override + warm-up compile. The warm-up uses a stream
    // builtin so it exercises Roslyn AND the LocStaRt runtime path once.
    [JSExport]
    internal static string Init()
    {
        try
        {
            var pool = ReferencePool.Init();
            TfRoslyn.setReferenceOverride(
                FuncConvert.FromFunc<Unit, FSharpList<Microsoft.CodeAnalysis.MetadataReference>>(
                    _ => pool));

            var sw = System.Diagnostics.Stopwatch.StartNew();
            var warmup = TfApi.evalSource(emptyPrelude, emptyBindings, "math.ema(gen.counter(0, 1), 0.5)");
            sw.Stop();

            var version = typeof(TfApi).Assembly.GetName().Version?.ToString(3) ?? "?";
            return Json(new TfInitDto(version, pool.Length, sw.Elapsed.TotalMilliseconds, warmup.result, warmup.error));
        }
        catch (Exception ex)
        {
            return Json(new TfInitDto("?", 0, 0, "", ex.ToString()));
        }
    }

    [JSExport]
    internal static string CompileInfo(string source)
    {
        try
        {
            var info = TfApi.compileInfo(emptyPrelude, source ?? "");
            var range = OptionModule.IsSome(info.errorRange)
                ? new TfErrorRangeDto(info.errorRange.Value.startIdx, info.errorRange.Value.endIdx)
                : null;
            return Json(new TfCompileInfoDto(
                info.error,
                range,
                info.hovers.Select(h => new TfHoverDto(h.startIdx, h.endIdx, h.name, h.typ)).ToList(),
                info.completions.Select(c => new TfCompletionDto(c.name, c.typ, c.kind)).ToList(),
                info.resultType));
        }
        catch (Exception ex)
        {
            return Json(new TfCompileInfoDto(ex.Message, null, [], [], ""));
        }
    }

    [JSExport]
    internal static string Eval(string source)
    {
        try
        {
            var info = TfApi.evalSource(emptyPrelude, emptyBindings, source ?? "");
            return Json(new TfEvalDto(info.csharp, info.result, info.output, info.error));
        }
        catch (Exception ex)
        {
            return Json(new TfEvalDto("", "", "", ex.Message));
        }
    }

    [JSExport]
    internal static string Prelude()
    {
        try
        {
            var entries = TfApi.preludeInfo(emptyPrelude)
                .Select(p => new TfPreludeEntryDto(p.name, p.typ, p.fields))
                .ToList();
            return Json(entries);
        }
        catch
        {
            return "[]";
        }
    }

    [JSExport]
    internal static string CompletionAt(string source, int offset)
    {
        try
        {
            return Json(TfApi.completionAtCursor(emptyPrelude, source ?? "", offset));
        }
        catch
        {
            return "[]";
        }
    }
}
