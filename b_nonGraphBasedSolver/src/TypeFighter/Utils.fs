namespace TypeFighter.Utils

module List =
    let findErr (f: 'T -> bool) msg (xs: 'T list) =
        try xs |> List.find f
        with _ -> failwith msg

module Set =
    let tryFind (f: 'T -> bool) (set: Set<'T>) =
        set |> Set.toSeq |> Seq.tryFind f