
#load "visuWrapper.fsx"
open Main
open VisuWrapper


//let env = AnnotatedAst.Env.empty
// TODO: convenience for importing .Net methods
module Builtins =
    let env x : Env = Map.ofList x

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
    
    let numberTyp = %Types.number
    let boolTyp = %Types.bool
    let stringTyp = %Types.string
    let unitTyp = %Types.unit
    let seqTyp = %Types.seq

    let add = import("add", numberTyp ^-> numberTyp ^-> numberTyp)
    let tostring = import("tostring", %1 ^-> stringTyp)
    let read = import("read", unitTyp ^-> numberTyp)
    let map = import("map", seqTyp * %1 ^-> (%1 ^-> %2) ^-> seqTyp * %2)
    let mapp = import("mapp", (%1 ^-> %2) ^-> seqTyp * %1 ^-> seqTyp * %2)
    let filter = import("filter", seqTyp * %1 ^-> (%1 ^-> boolTyp) ^-> seqTyp * %2)
    let filterp = import("filter", (%1 ^-> boolTyp) ^-> seqTyp * %1 ^-> seqTyp * %1)
    let take = import("take", seqTyp * %1 ^-> numberTyp ^-> %1)
    let skip = import("skip", seqTyp * %1 ^-> numberTyp ^-> %1)
    
    let emptyList = import("emptyList", seqTyp * %1)
    let cons = import("cons", %1 ^-> seqTyp * %1 ^-> seqTyp * %1)

    let numbers = import("Numbers", seqTyp * numberTyp)


[<AutoOpen>]
module Dsl =
    let mu exp = { exp = exp; meta = () }

    let Str x = Lit (LString x) |> mu
    let Num x = Lit (LNumber x) |> mu
    let Bool x : UExp = Lit (LBool x) |> mu
    let True = Bool true
    let False = Bool false
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

    let private seqOp name seq lam = Appn (Var name) [ seq; lam ]

    //let Pipe x f = App f x
    let FComp f g = Abs "x" (App (App f (Var "x")) g)

    let Map seq projection = seqOp (fst Builtins.map) seq projection
    let MapP projection = App (Var(fst Builtins.mapp)) projection
    let Filter seq predicate = seqOp (fst Builtins.filter) seq predicate
    let FilterP predicate = App (Var(fst Builtins.filterp)) predicate
    let NewList es =
        let rec makeList es =
            match es with
            | [] -> Var (fst Builtins.emptyList)
            | e :: es -> Appn (Var (fst Builtins.cons)) [ e; makeList es ]
        makeList es





let env1 = Builtins.env [ Builtins.map; Builtins.add; Builtins.numbers ]

(*
let x = 10.0
map Numbers (number ->
    add number x)
*)

(Let "x" (Num 10.0)
(Map (Var "Numbers") (Abs "number"
    (Appn (Var "add") [ Var "number"; Var "x" ] ))))
|> showSolvedAst env1

|> showLightAst env1
|> showAnnotatedAst env1
|> showConstraintGraph env1
|> showSolvedGraph env1
|> showSolvedAst env1




let env2 = Builtins.env [ ]
(*
let x = { a = 5.0; b = "hello" }
x.b
*)

(Let "x" (Record [ ("a", Num 5.0); ("b", Str "hello") ])
(Prop "b" (Var "x")))
|> showSolvedAst env2




let env3 = Builtins.env [ Builtins.cons; Builtins.emptyList ]
(*
[ 1.0; 2.0; 3.0 ]
*)

// this should fail
NewList [ Num 1.0; Num 2.0; Str "xxx"  ]
|> showSolvedAst env3

// this should work
NewList [ Num 1.0; Num 2.0; Num 3.0  ]
|> showSolvedAst env3







let env4 = Builtins.env [ 
    Builtins.add
    Builtins.tostring
    Builtins.mapp
    Builtins.filterp
    Builtins.cons
    Builtins.emptyList
]

(*
[ 1.0 ]
|> map (fun x -> tostring x)
*)

//(Pipe
//(NewList [ Num 1.0 ])
//(MapP (Abs "x" (App (Var "tostring") (Var "x") )))
//)
//|> showSolvedAst env4

(App 
    (FComp
        (MapP (Abs "x" (App (Var "tostring") (Var "x") )))
        (FilterP (Abs "x" True ))
        )
    (NewList [ Num 1.0 ])
)
|> showSolvedGraph env4
|> showSolvedAst env4

|> showLightAst env4
|> showAnnotatedAst env4


MapP (Abs "x" (App (Var "tostring") (Var "x")))
|> showSolvedAst env4 |> fun x -> x.substs


(Abs "x" (App (Var "tostring") (Var "x")))
|> showSolvedAst env4 |> fun x -> x.substs




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

