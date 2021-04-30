
#load "visuWrapper.fsx"
open Main
open VisuWrapper

[<AutoOpen>]
module Dsl =
    let mu exp = { exp = exp; meta = () }

    let Str x = Lit (LString x) |> mu
    let Num x = Lit (LNumber x) |> mu
    let Bool x = Lit (LBool x) |> mu
    let Unit : UExp = Lit LUnit |> mu

    let Var ident = Var ident |> mu
    let App e1 e2 = App (e1, e2) |> mu
    let Abs ident e = Abs (mu ident, e) |> mu
    let Let ident e1 e2 = Let (ident, e1, e2) |> mu
    let Prop ident e = Prop (ident, e) |> mu
    let Tuple es = Tuple es |> mu
    let Record fields = Record fields |> mu

    // convenience

    let Appn e es =
        let rec apply current es =
            match es with
            | [] -> current
            | [x] -> App current x
            | x :: xs -> apply (App current x) xs
        apply e es

    let private listOp name seq lam = Appn (Var name) [ seq; lam ]
    
    let MapExp seq projection = listOp "map" seq projection
    let FilterExp seq predicate = listOp "filter" seq predicate



//let env = AnnotatedAst.Env.empty
// TODO: convenience for importing .Net methods
module EnvCfg =
    let numberTyp = TApp(KnownTypeNames.number, [])
    let stringTyp = TApp(KnownTypeNames.string, [])
    let unitTyp = TApp(KnownTypeNames.unit, [])

    // a small DSL for type definitions
    type AppT = AppT with
        static member inline ($) (AppT, x: string) = TApp(x, [])
        static member inline ($) (AppT, x: int) = TGenVar x
    let inline tapp x = (($) AppT) x
    let inline (~%) x = tapp x
    let ( ** ) name arg = TApp(name, [arg])
    let ( * ) x arg =
        match x with
        | TApp (name, args) -> TApp(name, args @ [arg])
        | _ -> failwith "Operator '*' works only on TApp."
    let ( ^-> ) t1 t2 = TFun(t1, t2)
    let import(name, t) = name, Extern t

    // Example:
    //  Dictionary  <    string,  'a > ->   string   -> 'a
    // "Dictionary" ** %"string" * %1 ^-> %"string" ^-> %1

    let add = import("add", numberTyp ^-> numberTyp ^-> numberTyp)
    let read = import("read", unitTyp ^-> numberTyp)
    let map = import("map", KnownTypeNames.seq ** %1 ^-> (%1 ^-> %2) ^-> KnownTypeNames.seq ** %2)
    let take = import("take", KnownTypeNames.seq ** %1 ^-> numberTyp ^-> %1)
    let skip = import("skip", KnownTypeNames.seq ** %1 ^-> numberTyp ^-> %1)
    
    let numbers = import("Numbers", KnownTypeNames.seq ** numberTyp)

open EnvCfg

let smallEnv = Map.ofList [ add; read ]
let fullEnv = Map.ofList [ add; read; map; take; skip; numbers ]


(*
let x = 10.0
map Numbers (\number ->
    add number x)
*)

(Let "x" (Num 10.0)
(Appn (Var "map") [ Var "Numbers"; Abs "number"
(Appn (Var "add") [ Var "number"; Var "x" ] )] ))
|> showLightAst fullEnv
|> showAnnotatedAst fullEnv
|> showConstraintGraph fullEnv
|> showSolvedGraph fullEnv
|> showSolvedAst fullEnv




(*
let x = { a = 5.0; b = "hello" }
x.b
*)

(Let "x" (Record [ ("a", Num 5.0); ("b", Str "hello") ])
(Prop "b" (Var "x")))
|> showSolvedAst fullEnv



//let idExp = Abs "x" (Var "x")
//// polymorphic let
//(*
//let id = fun x -> x in
//    let f = id in
//        let res1 = f 99 in
//            let res2 = f "Hello World" in
//                res2
//*)
//Let "f" idExp
//(Let("res1", App(Var "f", Num 99.0),
//    Let("res2", App(Var "f", Str "HelloWorld"),
//        Var("res2")
//)))
////|> annotate env |> createConstraintGraph
////|> showAst
//|> showConstraintGraph EnvCfg.fullEnv

