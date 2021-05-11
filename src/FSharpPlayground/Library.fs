namespace FSharpPlayground

module MyModule =
    let it =
        let id = fun x -> x
        let add = fun a -> fun b -> {| a = a; b = b |}
        add "Hello" (id 42.0)
