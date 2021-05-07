namespace FSharpPlayground

module MyModule =
    let getResult myNumber =
        let f x = x + myNumber
        let g myFun x = myFun x
        g (fun x -> x + 23) (f myNumber)
