[<AutoOpen>]
module Helper

type Counter(exclSeed) =
    let mutable varCounter = exclSeed
    member this.next() = varCounter <- varCounter + 1; varCounter

module Map =
    let private xtract f (m: Map<_,_>) = m |> Seq.map f |> Seq.toList
    let keys (m: Map<_,_>) = xtract (fun kvp -> kvp.Key) m
    let values (m: Map<_,_>) = xtract (fun kvp -> kvp.Value) m
    let ofListUnique l =
        let rec build l map =
            match l with
            | [] -> map
            | (k,v) :: xs ->
                if map |> Map.containsKey k then
                    failwith $"Key already in map: {k}"
                let map = map |> Map.add k v
                build xs map
        build l Map.empty
