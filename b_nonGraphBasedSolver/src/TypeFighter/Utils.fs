namespace TypeFighter.Utils

module List =
    let findErr (f: 'T -> bool) msg (xs: 'T list) =
        try xs |> List.find f
        with _ -> failwith msg

type Fifo<'a when 'a: equality>(curr) =
    let mutable list = curr |> Option.defaultValue []
    member _.Append(x: 'a) = list <- x :: list
    member _.Replace(oldX, newX) =
        list <-
            [
                for y in list do
                    if y = oldX then newX else y
            ]
    member _.Values = list |> List.rev

type OneToMany<'k,'v when 'k: comparison and 'v: comparison>(existing) =
    let mutable map = existing |> Option.defaultValue Map.empty<'k, Set<'v>>
    member _.Add(k, v) =
        map <-
            let newValues = 
                match map |> Map.tryFind k with
                | Some curr -> curr |> Set.add v
                | None -> Set.singleton v
            map |> Map.add k newValues
    member _.Replace(k, vs) =
        map <- map |> Map.add k (set vs)
    member _.TryFind(k) = map |> Map.tryFind k
    member _.Find(k) = 
        match map |> Map.tryFind k with
        | Some v -> v
        | None -> failwithf $"Key '{k}' not found"
    member _.Remove(k) = map <- map |> Map.remove k
    member _.Values = map

module Mutable =
    let fifo existing = new Fifo<_>(existing)
    let oneToMany existing = new OneToMany<_,_>(existing)
