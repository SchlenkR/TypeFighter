
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


let replace (a: 'a) (s: seq<'a>) = s
let x = replace 3 [4]
let y = replace "3" ["4"]
















open System

let tryParse (s: string) = Nullable(5.0)
let div (a: float) (b: float) = Nullable 10.0
let add a b = a + b


let num1 = tryParse "5.0"
if num1.HasValue then
    let num2 = tryParse "10.0"
    if num2.HasValue then
        let res = div num1.Value num2.Value
        res
    else Nullable()
else Nullable()


let hasValue (n: Nullable<_>) = n.HasValue
let getValue (n: Nullable<_>) = n.Value

let num1' = tryParse "5.0"
let num2' = tryParse "10.0"
let res = div num1' num2'
res

(tryParse "5.0") |> (fun num1' ->
    (tryParse "10.0") |> (fun num2' ->
        (div num1' num2') |> (fun res ->
            res
        )
    )
)

let failwith msg = raise (Exception msg)

type MyClass(a: string, b: string) =
    member this.Name = a + " " + b
    member this.Destroy() = failwith "blah"


let bind f m = 
    if hasValue m then f (getValue m) else Nullable()
let ret a = 
    Nullable a

(tryParse "5.0") |> bind (fun num1' ->
    (tryParse "10.0") |> bind (fun num2' ->
        (div num1' num2') |> bind (fun res ->
            ret res
        )
    )
)




tryParse "5.0" |> bind (fun num1' ->
tryParse "10.0" |> bind (fun num2' ->
div num1' num2' |> bind (fun res ->
ret res
)))

let ( >=> ) m f = bind f m

tryParse "5.0" >=> (fun num1' ->
tryParse "10.0" >=> (fun num2' ->
div num1' num2' >=> (fun res ->
ret res
)))


type NullableMonad() =
    member this.Bind(m, f) = bind f m
    member this.Return(x) = ret x
let maybe = NullableMonad()

maybe {
    let! num1' = tryParse "5.0"
    let! num2' = tryParse "10.0"
    let! res = div num1' num2'
    return res
}






[
    1
    2
    3
]

[
    yield 1
    yield 2
    yield 3
]

let a = 112

[
    for x in 10..20 do
        if x % 2 = 0 then
            yield x + 10
]




let minus a b = a-b
let ( -/- ) a b = minus a b
5 -/- 2








let a = 10 in a + 1
//  x   e1    --e2-

10 |> (fun a -> a + 1)

(*
((a) => a + 1)(10);
*)

let a = 10
a + 1











let sayName (first: string) (last: string) = $"Hello, {first} {last}"

sayName "Sefa" "Ilkimen"

"Ilkimen" |> sayName "Sefa"












(*

// (number * number) -> string
const doNumbers = (a, b) => (a + b).toString();

*)


