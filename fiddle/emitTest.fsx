
open System
open System.Collections.Generic
open System.Linq
open System.Reflection
open System.Reflection.Emit


let aname = AssemblyName("Test1")
let assemblyb = AssemblyBuilder.DefineDynamicAssembly(aname, AssemblyBuilderAccess.Run)
let moduleb = assemblyb.DefineDynamicModule("Main")
let typeb = moduleb.DefineType("Module1", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

let methodName = "Map"
let methodb =
    typeb.DefineMethod
        (
            methodName,
            MethodAttributes.Public ||| MethodAttributes.Static,
            typeof<IEnumerable<string>>,
            [|
                typeof<IEnumerable<string>>
                typeof<Func<string, string>>
            |]
        )


// we know it's the first overload :)
let selectMethod =
    typeof<Enumerable>.GetMethods()
    |> Array.filter (fun m -> m.Name = "Select")
    |> Array.head
    |> fun m -> m.MakeGenericMethod ([| typeof<string>; typeof<string> |])


// Get an ILGenerator and emit a body for the dynamic method,
// using a stream size larger than the IL that will be
// emitted.
let il = methodb.GetILGenerator(256)

// Load the first argument, which is a string, onto the stack.
il.Emit(OpCodes.Ldarg_0)
il.Emit(OpCodes.Ldarg_1)
il.Emit(OpCodes.Call, selectMethod)
il.Emit(OpCodes.Ret)


let map = typeb.CreateType().GetMethod(methodName)

map.Invoke(
    null,
    [|
        ["Hello"; "World"] |> Seq.ofList
        Func<string, string>(fun s -> s + "XXX")
    |])
:?> IEnumerable<string>
|> Seq.toList

