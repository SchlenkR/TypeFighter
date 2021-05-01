namespace TypeFighter.CodeGen

#if INTERACTIVE
#r "nuget: Basic.Reference.Assemblies"
#r "nuget: Microsoft.CSharp"
#r "nuget: Microsoft.CodeAnalysis"
#r "nuget: Microsoft.CodeAnalysis.CSharp"
# load "Core.fs"
#endif

type Query<'context> = 'context -> obj

open System.IO
open System.Reflection
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open TypeFighter

module rec DotNetCodeModel =
    
    type CapturedField =
        { name: string
          typ: Tau }

    type DispalyClass =
        { fields: CapturedField
          invokeReturnTyp: Tau
          inputTyp: Tau
          body: CodeExp }

    type CodeExp =
        | DisplayClass of DispalyClass



//module Compiler =
//    let contextArgName = "context"

//    let transpile (exp: TExp) =
//        let quote = "\""
//        let csTrue = "true"
//        let csFalse = "false"

//        let rec toCode (exp: TExp) =
//            [
//                match exp.exp with
//                | Lit l ->
//                    match l with
//                    | LString x -> quote + x + quote
//                    | LNumber x -> x.ToString(System.Globalization.CultureInfo.InvariantCulture) + "d"
//                    | LBool x -> if x then csTrue else csFalse
//                    | LUnit -> $"{nameof(CoreLib.Unit)}.{nameof(CoreLib.Unit.Instance)}"
//                | Var ident ->ident
//                | App (e1, e2) ->
//                    $"{toCode e1}({toCode e2})"
//                | Abs (ident, body) ->
//                    $"({ident} => ({toCode body}))"
//                | Let (ident, e, body) ->
//                    $"{nameof(CoreLib.CompilerHelper.Exp)}(() => {{ var {ident} = {toCode e}; return {toCode body}; }})"
//                | Prop (name, e) ->
//                    $"{toCode e}.{name}"
//                | Tuple es ->
//                    es |> List.map toCode |> String.concat ", "
//                | Record fields ->
//                    let assExp =
//                        [ for l,e in fields do $"{l} = ({toCode e})" ]
//                        |> String.concat ", "
//                    $"new {{ {assExp} }}"
//            ]
//            |> String.concat " "
//        toCode exp

//    let compileCode (code: string) (assemblies: seq<Assembly>) =
//        printfn "Code:\\n\n%s\n\n\n" code

//        let syntaxTree = CSharpSyntaxTree.ParseText(code)
//        let res =
//            CSharpCompilation.Create(
//                "dynamic",
//                [ syntaxTree ],
//                references = [
//                    yield! 
//                        Basic.Reference.Assemblies.ReferenceAssemblies.NetStandard20 
//                        |> Seq.map (fun x -> x :> MetadataReference)
//                    for asm in assemblies do 
//                        yield MetadataReference.CreateFromFile(asm.Location) :> MetadataReference
//                ],
//                options = CSharpCompilationOptions(
//                    OutputKind.DynamicallyLinkedLibrary,
//                    optimizationLevel = OptimizationLevel.Debug))

//        use ms = new MemoryStream()
//        let emitRes = res.Emit(ms)
//        if not emitRes.Success then
//            let failures =
//                [
//                    for d in emitRes.Diagnostics do
//                        if d.IsWarningAsError || d.Severity = DiagnosticSeverity.Error then
//                            yield $"    {d.Id} ({d.Location}): {d.GetMessage()}"
//                ]
//                |> String.concat "\n"
//            failwith $"Compilation Error:\n {failures}"
//        do ms.Seek(0L, SeekOrigin.Begin) |> ignore
//        let bytes = ms.ToArray()
//        Assembly.Load(bytes)

//    let compileExp<'context> (assemblies: seq<Assembly>) (exp: Exp) : Query<'context> =
//        let generatedClassName = "Query"
//        let queryMethodName = "Eval"
//        let code =
//            @$"
//                using System;
//                using CoreLib;

//                using static {nameof(CoreLib)}.{nameof(CoreLib.CompilerHelper)};
//                using static {nameof(CoreLib)}.{nameof(CoreLib.Exports)};

//                public static class {generatedClassName}
//                {{
//                    public static object {queryMethodName}(
//                        {typeof<'context>.FullName} {contextArgName},
//                        {nameof(CoreLib)}.{nameof(CoreLib.ViewCache)} {viewCacheArgName})
//                    {{
//                        return {transpile exp};
//                    }}
//                }}
//            "

//        let asm = compileCode code [
//                //typeof<CoreLib.CompilerHelper>.Assembly
//                //typeof<TestData.Context>.Assembly
//                yield! assemblies
//            ]
            
//        let rootType = asm.GetTypes() |> Seq.find (fun t -> t.Name = generatedClassName)
//        let queryMethod = rootType.GetMethod queryMethodName
//        let query = fun (c: 'context) -> queryMethod.Invoke(null, [| c |])
//        query

//    let run x (q: Query<_>) = q x
    
//    let compileAndRun context assemblies exp = compileExp assemblies exp |> run context
