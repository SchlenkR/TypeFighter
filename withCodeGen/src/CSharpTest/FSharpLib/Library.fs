namespace FSharpLib

module Say =
    let doIt() =
        List.map (fun x ->
            let id = fun x -> x
            let a = 20
            (id a) + x
        ) [1;2;3]

module Hello =
    let doIt(v: int) =
        let x =
            let x = 10 * v
            let y = 200 * v
            x + y
        x + 99

module World =
    let doIt() =
        [1] |> List.map (fun x ->
            [1] |> List.map (fun y ->
                [1] |> List.map (fun z ->
                    x + y + z)))
        

