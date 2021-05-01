
type Exp<'a> =
    | Lam of string * N<'a>
and N<'a> = Exp<'a> * 'a

type UExp = Exp<unit>


type XData = { a: string; b: int }
type T =
    | X of XData

let x t =
    match t with
    | X { a = "a"; b = _ } -> "It's a!"
    | _ -> ":("
