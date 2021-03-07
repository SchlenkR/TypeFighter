#load "test.fsx"

open Test

module Dsl =
    
    let knownBaseTypes =
        {| int = "Int"
           float = "Float"
           string = "String" |}
           
    let tint = MBase knownBaseTypes.int
    let tfloat = MBase knownBaseTypes.float
    let tstring = MBase knownBaseTypes.string
    let tfun(a, b) = MFun(a, b)

    let xint (x: int) = ELit { typeName = knownBaseTypes.int; value = string x }
    let xfloat (x: float) = ELit { typeName = knownBaseTypes.float; value = string x }
    let xstr (x: string) = ELit { typeName = knownBaseTypes.string; value = x }

open Dsl

let env : Env = Map.ofList [
    "libcall_add", tfun(tint, tfun(tint, tint)) |> Poly.mono
    ]
    
// let ftvOfE (e: Exp) =
//     let te = Infer.annotate Env.empty e
//     te.t, Ftv.get te.t
let printConstraints =
    let inf = Infer()
    inf.AnnoExp >> inf.Constrain env >> Debug.printEquations
let printSolution =
    let inf = Infer()
    inf.AnnoExp >> inf.Constrain env >> inf.Solve >> Debug.printEquations
let inferType x = Infer().Infer env x |> fun res -> res.finalType







Ftv.get (tfun(tint, tfun(tint, MVar 2)))
// Infer.annotate env idExp
// ftvOfE idExp



let idExp = EFun("x", EVar "x")

inferType <| xint 43
inferType <| idExp
inferType <| ELet("hurz", xint 43, xstr "sss")
inferType <| ELet("id", idExp, EApp(EVar "id", xstr "sss"))
inferType <| EFun("x", xstr "klököl")
inferType <| EApp(EFun("x", EVar "x"), xint 2)
inferType <| EApp(EFun("x", EVar "x"), xstr "Hello")

// unbound var "y":
inferType <| EFun("x", EFun("y", EVar "x"))

inferType <| ELet("k", EFun("x", ELet("f", EFun("y", EVar "x"), EVar "f")), EVar "k")
inferType <| ELet("k", xint 43, ELet("k", xstr "sss", EVar "k"))



let expr1 = xint 42
let expr2 = ELet("hurz", xint 43, xint 32)
let expr3 =
    let addA = EFun("a", EApp(EVar "libcall_add", EVar "a"))
    let addB = EFun("b", EApp(addA, EVar "b"))
    ELet("hurz", xint 43, ELet("f", addB, EApp(EApp(EVar "f", EVar "hurz"), xint 99)))

printConstraints expr3
printSolution idExp
printSolution expr3

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

