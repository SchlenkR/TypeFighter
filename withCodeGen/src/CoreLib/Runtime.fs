namespace CoreLib

open System
open System.Collections.Generic
open System.Linq

type Unit() =
    static member Instance = Unit()

type ViewCache() =
    let views = Dictionary<string, obj>()
    member this.GetOrUpdate<'elem>(name, factory: Func<IEnumerable<'elem>>) : IEnumerable<'elem> =
        let succ,res = views.TryGetValue(name)
        let coll = 
            if succ then res
            else
                let view = factory.Invoke()
                let materializedView =
                    match view with
                    | :? Array -> view
                    | :? ResizeArray<_> -> view
                    | _ -> view.ToList().AsEnumerable()
                views.[name] <- materializedView
                materializedView :> obj
        coll :?> IEnumerable<'elem>


[<AbstractClass; Sealed>]
type CompilerHelper =
    static member Exp(del: Func<_>) = del.Invoke()
    static member Fun(f: Func<_>) = f
    static member Fun(f: Func<_,_>) = f
    static member Fun(f: Func<_,_,_>) = f

[<AbstractClass; Sealed>]
type Exports =
    static member toString(x) = x.ToString()
    
    // map cannot be curried - C# is too stupid
    //static member map<'t, 'u>(that: IEnumerable<'t>) =
    //    CompilerHelper.Fun(fun (proj: Func<'t, 'u>) -> that.Select(proj))
    static member map(that: IEnumerable<_>, proj: Func<_,_>) = that.Select(proj)
    
    // always use floats for now in the API
    static member skip(that: IEnumerable<_>, count: float) = that.Skip(int count)
    static member take(that: IEnumerable<_>, count: float) = that.Take(int count)
    
    static member read(_: Unit) = 34.0
    
    static member add(a: float, b: float) = a + b
    // we could use currying here, but for now, we won't
    //static member add(a: float) =
    //    CompilerHelper.Fun(fun (b: float) -> a + b)
    //static member add(a: int) = 
    //    CompilerHelper.Fun(fun (b: int) -> a + b)
