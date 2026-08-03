using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.InteropServices;
using Microsoft.CodeAnalysis;
using Microsoft.FSharp.Collections;

namespace PureState.TypeFighter.Wasm;

// Browser WASM has no runtime directory on disk, so the file-based reference
// lookup in Roslyn.fs cannot work. Instead the already-loaded assemblies are
// tapped in-memory via TryGetRawMetadata (pattern proven in Pxl.Studio.Wasm).
internal static class ReferencePool
{
    static FSharpList<MetadataReference>? references;

    // typeof() forces the runtime to actually load these assemblies before we
    // enumerate the AppDomain (assemblies load lazily under WASM).
    static readonly Type[] anchorTypes =
    [
        typeof(object),
        typeof(System.Linq.Enumerable),
        typeof(Dictionary<string, object>),
        typeof(System.IO.TextWriter),
        typeof(Func<object, object>),
        typeof(Microsoft.FSharp.Core.Unit),
        typeof(global::PureState.TypeFighter.Web.DspRt),
    ];

    // Facade assemblies carry no obvious anchor type; load them by name.
    static readonly string[] facadeNames =
    [
        "System.Runtime",
        "System.Runtime.Extensions",
        "netstandard",
        "System.Linq",
        "System.ObjectModel",
        "System.Collections",
        "DSP",
    ];

    // Fail fast if anything the generated C# needs is missing.
    static readonly string[] requiredNames =
    [
        "System.Private.CoreLib",
        "System.Runtime",
        "netstandard",
        "System.Linq",
        "System.Collections",
        "FSharp.Core",
        "PureState.TypeFighter",
        "PureState.TypeFighter.Web",
        "DSP",
    ];

    internal static FSharpList<MetadataReference> References => references ?? Init();

    internal static FSharpList<MetadataReference> Init()
    {
        if (references is not null)
            return references;

        var refsByName = new Dictionary<string, MetadataReference>();

        void add(Assembly asm)
        {
            var name = asm.GetName().Name ?? asm.FullName ?? "";
            if (refsByName.ContainsKey(name))
                return;
            var reference = TryGetReference(asm);
            if (reference is not null)
                refsByName[name] = reference;
        }

        foreach (var type in anchorTypes)
            add(type.Assembly);

        foreach (var facadeName in facadeNames)
        {
            try { add(Assembly.Load(facadeName)); }
            catch (Exception ex) { Console.WriteLine($"[ReferencePool] Facade '{facadeName}' not loadable: {ex.Message}"); }
        }

        foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            add(asm);

        var missing = requiredNames.Where(n => !refsByName.ContainsKey(n)).ToList();
        if (missing.Count > 0)
            throw new InvalidOperationException($"ReferencePool init failed - required assemblies missing: {string.Join(", ", missing)}");

        references = ListModule.OfSeq(refsByName.Values);
        Console.WriteLine($"[ReferencePool] {refsByName.Count} metadata references collected");
        return references;
    }

    static unsafe MetadataReference? TryGetReference(Assembly asm)
    {
        if (asm.IsDynamic)
            return null;
        try
        {
            if (asm.TryGetRawMetadata(out byte* blob, out int length))
            {
                var moduleMetadata = ModuleMetadata.CreateFromMetadata((nint)blob, length);
                return AssemblyMetadata.Create(moduleMetadata).GetReference();
            }
        }
        catch
        {
        }
        return null;
    }
}
