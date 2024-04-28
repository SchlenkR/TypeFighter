

let ( ^-> ) a b =
    printfn $"b: {b}"
    printfn $"a: {a}"
    a,b

1 ^-> 2 ^-> 3 ^-> 4
