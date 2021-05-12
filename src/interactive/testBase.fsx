
#load "../TypeFighter/Core.fs"
#load "../TypeFighter/Runtime.fs"
#load "../TypeFighter/DotNetCodeGen.fs"

open TypeFighter

[<AutoOpen>]
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

    let numberTyp = t0 TypeNames.number
    let boolTyp = t0 TypeNames.bool
    let stringTyp = t0 TypeNames.string
    let unitTyp = t0 TypeNames.unit
    let seqOf arg = t1 TypeNames.seq arg

    let binOpNumType = numberTyp ^-> numberTyp ^-> numberTyp
    let add = import("add", binOpNumType)
    let sub = import("sub", binOpNumType)
    let mul = import("mul", binOpNumType)
    let div = import("div", binOpNumType)

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

    let Pipe seq projection = seqOp (fst Builtins.map) seq projection
    let MapX seq projection = seqOp (fst Builtins.map) seq projection
    let MapP projection = App (Var(fst Builtins.mapp)) projection
    let FilterX seq predicate = seqOp (fst Builtins.filter) seq predicate
    let FilterP predicate = App (Var(fst Builtins.filterp)) predicate
    let NewList es =
        let rec makeList es =
            match es with
            | [] -> Var (fst emptyList)
            | e :: es -> Appn (Var (fst cons)) [ e; makeList es ]
        makeList es

module Test =
    let private testPassed name = $"TEST PASSES: {name}"
    let private error name expected actual = failwith $"Failed '{name}'\nExpected: {expected}\nActual:   {actual}"
    let private run env exp =
        let annoRes = AnnotatedAst.create env exp
        let res = annoRes |> ConstraintGraph.create |> ConstraintGraph.solve annoRes
        res.exprConstraintStates |> Map.find res.annotationResult.root
    let isOfType (name: string) env typ exp =
        let error actual = error name (Format.tau typ) actual
        match run env exp with
        | Constrained c,_ -> 
            if c = typ
            then testPassed name
            else error (Format.tau c)
        | UnificationError e,_ -> error $"ERROR ({e})"
    let isError (name: string) env exp =
        let error actual = error name "ERROR" actual
        match run env exp with
        | Constrained c,_ -> error (Format.tau c)
        | UnificationError _,_ -> testPassed name

