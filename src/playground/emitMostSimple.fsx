
open System
open System.Reflection
open System.Reflection.Emit

let aname = AssemblyName("Test1")
let ab = AssemblyBuilder.DefineDynamicAssembly(aname, AssemblyBuilderAccess.Run)
let mb = ab.DefineDynamicModule("MainModule")

// let getIntMethod = DynamicMethod("GetInt", typeof<string>, [| typeof<string> |], typeof<string>)
let getIntMethod = DynamicMethod("GetInt", typeof<string>, [| typeof<string> |], typeof<string>)

// Create an array that specifies the parameter types of the
// overload of Console.WriteLine to be used in Hello.
let writeStringArgs = [| typeof<string> |]
// Get the overload of Console.WriteLine that has one
// String parameter.
let writeString = typeof<Console>.GetMethod("WriteLine", writeStringArgs)

// Get an ILGenerator and emit a body for the dynamic method,
// using a stream size larger than the IL that will be
// emitted.
let il = getIntMethod.GetILGenerator()

// Load the first argument, which is a string, onto the stack.
il.Emit(OpCodes.Ldarg_0)
// Call the overload of Console.WriteLine that prints a string.
il.EmitCall(OpCodes.Call, writeString, null)

// The Hello method returns the value of the second argument;
// to do this, load the onto the stack and return.
il.Emit(OpCodes.Ldarg_0)
il.Emit(OpCodes.Ret)


// Add parameter information to the dynamic method. (This is not
// necessary, but can be useful for debugging.) For each parameter,
// identified by position, supply the parameter attributes and a
// parameter name.
getIntMethod.DefineParameter(1, ParameterAttributes.In, "myNumber")


let getInt =
 getIntMethod.CreateDelegate(typeof<Func<string, string>>)
 :?> Func<string, string>

getInt.Invoke("23")
