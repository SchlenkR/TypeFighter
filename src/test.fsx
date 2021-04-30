
#load "visuWrapper.fsx"
open Main
open VisuWrapper


//let env = AnnotatedAst.Env.empty
// TODO: convenience for importing .Net methods
module Builtins =
    let env x : Env = Map.ofList x

    // a small DSL for type definitions
    // TODO: Test the type DSL
    let inline (~%) x = TGenVar x
    let ( * ) x y =
        match x with
        | TTuple taus -> TTuple (taus @ [y])
        | _ -> TTuple [x;y]
    let ( ^-> ) t1 t2 = TFun(t1, t2)
    let t0 x = TApp (x, [])
    let t1 x a = TApp (x, [a])
    let t2 x a b = TApp (x, [a;b])
    let t3 x a b c = TApp (x, [a;b;c])
    let import(name, t) = name, Extern t

    let numberTyp = t0 Types.number
    let boolTyp = t0 Types.bool
    let stringTyp = t0 Types.string
    let unitTyp = t0 Types.unit
    let seqOf arg = t1 Types.seq arg

    let add = import("add", numberTyp ^-> numberTyp ^-> numberTyp)
    let tostring = import("tostring", %1 ^-> stringTyp)
    let read = import("read", unitTyp ^-> numberTyp)
    let map = import("map", seqOf %1 ^-> (%1 ^-> %2) ^-> seqOf %2)
    let mapp = import("mapp", (%1 ^-> %2) ^-> seqOf %1 ^-> seqOf %2)
    let filter = import("filter", seqOf %1 ^-> (%1 ^-> boolTyp) ^-> seqOf %2)
    let filterp = import("filter", (%1 ^-> boolTyp) ^-> seqOf %1 ^-> seqOf %1)
    let take = import("take", seqOf %1 ^-> numberTyp ^-> %1)
    let skip = import("skip", seqOf %1 ^-> numberTyp ^-> %1)
    
    let emptyList = import("emptyList", seqOf %1)
    let cons = import("cons", %1 ^-> seqOf %1 ^-> seqOf %1)

    let numbers = import("Numbers", seqOf numberTyp)


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


module Test =
    let run env exp =
        let annoRes = AnnotatedAst.create env exp
        let res = 
            annoRes.resultExp
            |> ConstraintGraph.create
            |> ConstraintGraph.solve annoRes.newGenVar
        do ConstraintGraph.applyResult annoRes.resultExp res.allNodes
        annoRes.resultExp.meta.constr
    let error name expected actual = failwith $"Failed '{name}'\nExpected: {expected}\nActual:   {actual}"
    let assertType name env typ exp =
        let error actual = error name (Format.tau typ) actual
        match run env exp with
        | Constrained c -> if c = typ  then () else error  (Format.tau c)
        | UnificationError e -> error $"ERROR ({e})"
        | Initial -> error "Initial"
    let assertError name env exp =
        let error actual = error name "ERROR" actual
        match run env exp with
        | Constrained c -> error (Format.tau c)
        | UnificationError e -> ()
        | Initial -> error "Initial"

open Builtins



let env1 = env [ map; add; numbers ]

(*
let x = 10.0
map Numbers (number ->
    add number x)
*)

(Let "x" (Num 10.0)
(Map (Var "Numbers") (Abs "number"
    (Appn (Var "add") [ Var "number"; Var "x" ] ))))
|> Test.assertType "map numbers by add" env1 (seqOf numberTyp)
//|> showSolvedAst env1
//|> showLightAst env1
//|> showAnnotatedAst env1
//|> showConstraintGraph env1
//|> showSolvedGraph env1




let env2 = env [ ]
(*
let x = { a = 5.0; b = "hello" }
x.b
*)

(Let "x" (Record [ ("a", Num 5.0); ("b", Str "hello") ])
(Prop "b" (Var "x")))
|> Test.assertType "Get record property" env2 stringTyp
//|> showSolvedAst env2




let env3 = env [ cons; emptyList ]
(*
[ 1.0; 2.0; 3.0 ]
*)

// this should fail
NewList [ Num 1.0; Num 2.0; Str "xxx"  ]
|> Test.assertError "Disjunct list element types" env3
//|> showSolvedAst env3

// this should work
NewList [ Num 1.0; Num 2.0; Num 3.0  ]
|> Test.assertType "Num list" env3 (seqOf numberTyp)
//|> showSolvedAst env3







let env4 = env [ add; tostring; mapp; filterp; cons; emptyList ]

(*
[ 1.0 ] |> map (fun x -> tostring x)
*)

//(Pipe
//(NewList [ Num 1.0 ])
//(MapP (Abs "x" (App (Var "tostring") (Var "x") )))
//)
//|> showSolvedAst env4

//(App 
//    (FComp
//        (MapP (Abs "x" (App (Var "tostring") (Var "x") )))
//        (FilterP (Abs "x" True ))
//        )
//    (NewList [ Num 1.0 ])
//)
//|> showSolvedGraph env4
//|> showSolvedAst env4

//|> showLightAst env4
//|> showAnnotatedAst env4


MapP (Abs "x" (App (Var "tostring") (Var "x")))
|> Test.assertType "Lambda applied to MapP" env4 (seqOf %1 ^-> seqOf stringTyp)
//|> showSolvedAst env4 |> fun x -> x.substs


(Abs "x" (App (Var "tostring") (Var "x")))
|> Test.assertType "Lambda with anon type" env4 (%1 ^-> stringTyp)
//|> showSolvedAst env4 |> fun x -> x.substs



// polymorphic let
(*
let id = fun x -> x
(id "Hello World", id 42.0)
*)

(Let "id" (Abs "x" (Var "x"))
(Tuple [ App (Var "id") (Str "Hello World"); App (Var "id") (Num 42.0) ])
)
|> Test.assertType "Polymorphic let" (env []) (stringTyp * numberTyp)




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

