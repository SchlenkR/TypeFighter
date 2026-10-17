using System.Runtime.InteropServices.JavaScript;
using System.Runtime.Versioning;
using System.Text.Json;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using TfApi = PureState.TypeFighter.Web.Api;
using TfEval = PureState.TypeFighter.Web.Eval;
using TfRoslyn = PureState.TypeFighter.Roslyn;

namespace PureState.TypeFighter.Wasm;

// JS-facing facade over the TypeFighter compiler.
[SupportedOSPlatform("browser")]
public static partial class TypeFighterInterop
{
    static readonly JsonSerializerOptions jsonOptions = new(JsonSerializerDefaults.Web);

    // The host prelude stays identity-stable for completion caching.
    static readonly FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>> emptyPrelude =
        FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>>.Empty;
    static readonly FSharpList<TfEval.ExtraBinding> emptyBindings =
        FSharpList<TfEval.ExtraBinding>.Empty;

    // The full record, as the host declared it. Completion needs every field --
    // that IS the list it offers.
    static FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>> hostPrelude = emptyPrelude;

    // The same records as name -> field set, for narrowing (below).
    static IReadOnlyList<(string Name, HashSet<string> Fields)> hostRecords = [];

    // Type-checking pays for the SIZE of the record it carries: a host record of
    // some thousand fields costs hundreds of milliseconds per solve, while the same
    // source narrowed to the two fields it actually reads costs single digits.
    //
    // So every call that only type-checks gets a record holding exactly the fields
    // the source names. Intersected with the declared ones, so a field that does
    // not exist is still absent and still an error.
    //
    // Memoised by source: `CompileInfo` and `Graph` arrive for the same keystroke,
    // and the analysis cache behind them keys on prelude IDENTITY -- a fresh list
    // per call would make it miss and buy back the second solve.
    static string? narrowedSource;
    static FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>> narrowedPrelude = emptyPrelude;

    static FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>> PreludeFor(string source)
    {
        if (hostRecords.Count == 0) return hostPrelude;
        if (narrowedSource == source) return narrowedPrelude;

        // A record handed around as a whole - `a = values`, passed to a function -
        // says nothing about which fields will be wanted. Then it goes in full,
        // slow and correct, rather than narrow and wrong.
        var narrowable = hostRecords
            .Select(record => (record.Name, Fields: TfApi.narrowableFieldsOf(record.Name, source)))
            .ToList();

        if (narrowable.Any(entry => OptionModule.IsNone(entry.Fields)))
        {
            narrowedPrelude = hostPrelude;
            narrowedSource = source;
            return narrowedPrelude;
        }

        var known = hostRecords.ToDictionary(record => record.Name, record => record.Fields);

        var narrowed = narrowable.Select(entry =>
        {
            var touched = entry.Fields.Value
                .Where(known[entry.Name].Contains)
                .Distinct(StringComparer.Ordinal)
                .Select(field => Tuple.Create(field, global::PureState.TypeFighter.BuiltinTypes.number));

            return Tuple.Create(
                entry.Name,
                global::PureState.TypeFighter.Typ.NewMono(
                    global::PureState.TypeFighter.TDef.RecordWith(ListModule.OfSeq(touched))));
        });

        narrowedPrelude = ListModule.OfSeq(narrowed);
        narrowedSource = source;
        return narrowedPrelude;
    }

    record TfNumberRecordDto(string Name, IReadOnlyList<string> Fields);
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
    record TfCompletionAtDto(bool IsMemberAccess, IReadOnlyList<string> Fields);
    record TfInitDto(string Version, int ReferenceCount, double WarmupMs, string WarmupResult, string Error);
    record TfGraphNodeDto(
        string Id,
        string Kind,
        string Label,
        string Detail,
        string Typ,
        string Binds,
        string Group,
        int StartIdx,
        int EndIdx);
    record TfGraphEdgeDto(string FromId, string ToId, string Port, string Typ);
    record TfGraphGroupDto(string Id, string Label, string Parent);
    record TfGraphDto(
        string Error,
        IReadOnlyList<TfGraphNodeDto> Nodes,
        IReadOnlyList<TfGraphEdgeDto> Edges,
        IReadOnlyList<TfGraphGroupDto> Groups,
        string RootId);

    static string Json<T>(T value) => JsonSerializer.Serialize(value, jsonOptions);

    [JSExport]
    internal static string Init(string numberRecordsJson)
    {
        try
        {
            hostPrelude = NumberRecordPrelude(numberRecordsJson);
            hostRecords = NumberRecordFields(numberRecordsJson);
            narrowedSource = null;

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
            var text = source ?? "";
            var info = TfApi.compileInfo(PreludeFor(text), text);
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
            var text = source ?? "";
            var info = TfApi.evalSource(PreludeFor(text), emptyBindings, text);
            return Json(new TfEvalDto(info.csharp, info.result, info.output, info.error));
        }
        catch (Exception ex)
        {
            return Json(new TfEvalDto("", "", "", ex.Message));
        }
    }

    [JSExport]
    internal static string Graph(string source)
    {
        try
        {
            var text = source ?? "";
            var info = TfApi.graphInfo(PreludeFor(text), text);
            var g = info.graph;
            return Json(new TfGraphDto(
                info.error,
                g.nodes.Select(n => new TfGraphNodeDto(
                    n.id, n.kind, n.label, n.detail, n.typ, n.binds, n.group, n.startIdx, n.endIdx)).ToList(),
                g.edges.Select(e => new TfGraphEdgeDto(e.fromId, e.toId, e.port, e.typ)).ToList(),
                g.groups.Select(gr => new TfGraphGroupDto(gr.id, gr.label, gr.parent)).ToList(),
                g.rootId));
        }
        catch (Exception ex)
        {
            return Json(new TfGraphDto(ex.Message, [], [], [], ""));
        }
    }

    [JSExport]
    internal static string Prelude()
    {
        try
        {
            var entries = TfApi.preludeInfo(hostPrelude)
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
            var completion = TfApi.completionAtCursor(hostPrelude, source ?? "", offset);
            return Json(new TfCompletionAtDto(completion.isMemberAccess, completion.fields));
        }
        catch
        {
            return Json(new TfCompletionAtDto(false, []));
        }
    }

    static IReadOnlyList<(string Name, HashSet<string> Fields)> NumberRecordFields(
        string numberRecordsJson)
    {
        var records = JsonSerializer.Deserialize<IReadOnlyList<TfNumberRecordDto>>(
            numberRecordsJson,
            jsonOptions) ?? [];

        return records
            .Select(it => (it.Name, new HashSet<string>(it.Fields, StringComparer.Ordinal)))
            .ToList();
    }

    static FSharpList<Tuple<string, global::PureState.TypeFighter.Typ>> NumberRecordPrelude(
        string numberRecordsJson)
    {
        var records = JsonSerializer.Deserialize<IReadOnlyList<TfNumberRecordDto>>(
            numberRecordsJson,
            jsonOptions) ?? [];

        return ListModule.OfSeq(records.Select(it =>
        {
            var fields = ListModule.OfSeq(
                it.Fields
                    .Distinct(StringComparer.Ordinal)
                    .Select(field => Tuple.Create(field, global::PureState.TypeFighter.BuiltinTypes.number)));
            var record = global::PureState.TypeFighter.TDef.RecordWith(fields);

            return Tuple.Create(it.Name, global::PureState.TypeFighter.Typ.NewMono(record));
        }));
    }
}
