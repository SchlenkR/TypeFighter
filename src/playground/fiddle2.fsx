
type Option<'a> =
    | Some of 'a
    | None
type Lit =
    | Num of float
    | Str of string
type Exp =
    | Lit of Lit
    | Var of string 
    | Tuple of List<Exp>
    | App of Exp * Exp * Option<Exp>


let analyze exp =
    match exp with
    | Lit (Num x) when x < 0.0 ->
        "Warning: negative number is used!"
    | App (_, _, Some (Tuple t)) ->
        match t with
        | [a;b;c] -> "..."
        | [] -> "..."
        | x::xs -> "..."
    | _ -> ""




let id = fun x -> x in
    let res1 = id 4 in
        let res2 = id "Hallo" in
            res2
