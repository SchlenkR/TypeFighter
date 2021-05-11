
open System
open System.Reflection
open System.Reflection.Emit

let asmName = AssemblyName("DemoMethodBuilder1")
let domain = AppDomain.CurrentDomain

let demoAssembly = domain.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.RunAndCollect)

// Define the module that contains the code. For an 
// assembly with one module, the module name is the 
// assembly name plus a file extension.
let demoModule = demoAssembly.DefineDynamicModule(asmName.Name, asmName.Name + ".dll")
let demoType = demoModule.DefineType("DemoType", TypeAttributes.Public)
let methodBuilder = demoType.DefineMethod("Factory", MethodAttributes.Public ||| MethodAttributes.Static)

methodBuilder.SetParameters [| typeof<int> |]
methodBuilder.SetReturnType typeof<int>

let ilgen = methodBuilder.GetILGenerator()

let input = ilgen.DeclareLocal(typeof<int>)

ilgen.Emit(OpCodes.Ldarg_0)
//ilgen.Emit(OpCodes.Stloc_S, input)
ilgen.Emit(OpCodes.Newobj)
ilgen.Emit(OpCodes.Throw)


[ for numbers in [ [1;2]; [3;4;5] ] do
    let newNumbers =
        for number in numbers do
            yield number + 1
    newNumbers
]

