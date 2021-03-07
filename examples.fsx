
#load "hm.fsx"
open Hm

type Show =
    static member show(t: Mono) =
        match t with
        | MBase tname -> tname
        | MVar tvar -> string tvar
        | MFun (m, n) -> $"{Show.show m} -> {Show.show n}"
    static member show(eqs: Subst list) =
        eqs
        |> List.sortBy (fun x -> x.tvar)
        |> List.map (fun x -> $"%-20s{x.desc}    {x.tvar} = {Show.show x.right}")
        |> String.concat "\n"
        |> fun x -> "\n\n----------Substs:\n" + x + "\n----------"

#if INTERACTIVE
fsi.PrintWidth <- 250
fsi.AddPrinter (fun (x: Mono) -> Show.show x)
fsi.AddPrinter (fun (x: Subst list) -> Show.show x)
#endif


[<AutoOpen>]
module Dsl =
    let knownBaseTypes =
        {| int = "Int"
           float = "Float"
           string = "String" |}
           
    let tint = MBase knownBaseTypes.int
    let tfloat = MBase knownBaseTypes.float
    let tstring = MBase knownBaseTypes.string
    let tfun(a, b) = MFun(a, b)

    let cint (x: int) = ELit { typeName = knownBaseTypes.int; value = string x }
    let cfloat (x: float) = ELit { typeName = knownBaseTypes.float; value = string x }
    let cstr (x: string) = ELit { typeName = knownBaseTypes.string; value = x }

let env : Env = Map.ofList [
    "libcall_add", tfun(tint, tfun(tint, tint)) |> Poly.mono
    ]

// let ftvOfE (e: Exp) =
//     let te = Infer.annotate Env.empty e
//     te.t, Ftv.get te.t
let inferType =
    Infer.infer env
    >> fun res -> res.finalType
let constr =
    Infer.infer env
    >> fun x -> x.constraintSet
let solve =
    Infer.infer env
    >> fun x -> x.solutionMap
let constrAndSolve x =
    Infer.infer env x
    |> fun x -> x.constraintSet, x.solutionMap







Ftv.get (tfun(tint, tfun(tint, MVar 2)))
// Infer.annotate env idExp
// ftvOfE idExp



let idExp = EFun("x", EVar "x")

// polymorphic let
ELet("f", idExp,
    ELet("res1", EApp(EVar "f", cint 99),
        ELet("res2", EApp(EVar "f", cstr "HelloWorld"),
            EVar("res2")
)))
|> constrAndSolve


constrAndSolve idExp

inferType <| cint 43
inferType <| idExp
inferType <| ELet("hurz", cint 43, cstr "sss")
inferType <| ELet("id", idExp, EApp(EVar "id", cstr "sss"))
inferType <| EFun("x", cstr "klököl")
inferType <| EApp(EFun("x", EVar "x"), cint 2)
inferType <| EApp(EFun("x", EVar "x"), cstr "Hello")

// unbound var "y":
inferType <| EFun("x", EFun("y", EVar "x"))

inferType <| ELet("k", EFun("x", ELet("f", EFun("y", EVar "x"), EVar "f")), EVar "k")
inferType <| ELet("k", cint 43, ELet("k", cstr "sss", EVar "k"))


let expr1 = cint 42
let expr2 = ELet("hurz", cint 43, cint 32)
let expr3 =
    let addA = EFun("a", EApp(EVar "libcall_add", EVar "a"))
    let addB = EFun("b", EApp(addA, EVar "b"))
    ELet("hurz", cint 43, ELet("f", addB, EApp(EApp(EVar "f", EVar "hurz"), cint 99)))

constr expr3
solve idExp
solve expr3

inferType expr3
inferType expr1
inferType expr2


// Errors
(*
solve <| EApp (EVar "libcall_add") (xstr "lklö")
*)


(*
// Der Typ von "f" ist _kein_ Polytyp
(fun f -> f "as", f 99) id

// Der Typ von "f" ist ein Polytyp
let f = id in f "as", f 99
*)

