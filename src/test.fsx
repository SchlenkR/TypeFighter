#load "main.fsx"
open Main

module Format =
    let tyvar (ident: string) (x: string) =
        $"'{ident}' : {x}"

    let texpName (exp: Exp<_>) =
        match exp with
        | Lit _ -> "Lit"
        | Var _ -> "Var"
        | App _ -> "App"
        | Abs _ -> "Fun"
        | Let _ -> "Let"

    let constraintState cs =
        match cs with
        | Constrained t -> Format.tau t
        | UnificationError e -> $"ERROR: {e}"
        | Initial -> "???"

    let envItem ident envItem =
        match envItem with
        | Intern tv -> $"{tyvar ident (string tv)}"
        | Extern t -> $"{tyvar ident (Format.tau t)}"

    let expTyvar exp = $"var = {exp.tyvar}"

    let env exp =
        let envVars =
            match exp.env |> Map.toList with
            | [] -> "[ ]"
            | [(ident, item)] ->
                $"[ {envItem ident item} ]"
            | _ ->
                [ for x in exp.env do $"-  {envItem x.Key x.Value}" ]
                |> String.concat "\n"
                |> fun s -> $"\n{s}"
        "env = " + envVars

#load "./visu/visu.fsx"
module Visu =
    open Visu
    
    let writeAnnotatedAst (showVar: bool) (showEnv: bool) (showConstraint: bool) (exp: TExp) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
        let rec createNodes (exp: TExp) =
            let details =
                [
                    if showVar then yield Format.expTyvar exp.meta
                    if showConstraint then yield Format.constraintState exp.meta.constr
                    if showEnv then yield Format.env exp.meta
                ]
                |> String.concat "\n"
            match exp.exp with
            | Lit x ->
                Tree.var $"Lit ({Lit.getValue x})" details []
            | Var ident ->
                Tree.var $"Var ({ident})" details []
            | App (e1, e2) ->
                Tree.var "App" details [ createNodes e1; createNodes e2 ]
            | Abs (ident, body) ->
                Tree.var $"Fun ({ident.exp})" details [ createNodes body ]
            | Let (ident, e, body) ->
                Tree.var $"Let {ident}" details [ createNodes e; createNodes body ]
        createNodes exp |> flatten |> Tree.write

    let writeConstraintGraph (allAnnoExp: TExp list) (nodes: ConstraintGraph.Node seq) =
        let indexedNodes = nodes |> Seq.indexed |> Seq.toList
        let jsLinks =
            [ 
                let nodesLookup = indexedNodes |> List.map (fun (a,b) -> b,a) |> readOnlyDict
                for n in nodes do
                    for i in ConstraintGraph.getIncomingNodes n do
                        { Visu.JsLink.fromNode = nodesLookup.[i]
                          Visu.JsLink.toNode = nodesLookup.[n] }
            ]
        let jsNodes =
            [ for i,x in indexedNodes do
                let name, layout =
                    match x.data with
                    | ConstraintGraph.Source _ -> "SOURCE", NodeTypes.op
                    | ConstraintGraph.Ast x ->
                        let expName =
                            match allAnnoExp |> List.tryFind (fun a -> a.meta.tyvar = x.tyvar) with
                            | None -> "Env"
                            | Some x -> Format.texpName x.exp
                        $"{x.tyvar} ({expName})", NodeTypes.var
                    | ConstraintGraph.MakeFun _ -> "MakeFun", NodeTypes.op
                    | ConstraintGraph.Arg { argOp = x; inc = _ } -> $"Arg {x}", NodeTypes.op
                    | ConstraintGraph.UnifySubst _ -> $"ApplySubst", NodeTypes.op
                { key = i
                  name = name
                  desc = Format.constraintState x.constr
                  layout = layout }
            ]
        Graph.write jsNodes jsLinks
    
    let private showAst env (showVar: bool) (showEnv: bool) (showConstraint: bool) exp =
        let annoExp = AnnotatedAst.create env exp |> fst
        do annoExp |> writeAnnotatedAst showVar showEnv showConstraint
        exp

    let showLightAst env exp = showAst env false false false exp
    let showAnnotatedAst env exp = showAst env true true false exp
    let showConstraintGraph env exp =
        let annoExp,allAnnoExp = AnnotatedAst.create env exp
        do annoExp |> ConstraintGraph.create |> writeConstraintGraph allAnnoExp
        exp
    let showSolvedGraph env exp =
        let annoExp,allAnnoExp = AnnotatedAst.create env exp
        let nodes = annoExp |> ConstraintGraph.create
        let res = ConstraintGraph.solve nodes
        do res.allNodes |> writeConstraintGraph allAnnoExp
        exp
    let showSolvedAst env exp =
        let annoExp,_ = AnnotatedAst.create env exp
        let nodes = annoExp |> ConstraintGraph.create
        let res = ConstraintGraph.solve nodes
        do ConstraintGraph.applyResult annoExp res.allNodes
        do annoExp |> writeAnnotatedAst false false true


[<AutoOpen>]
module Dsl =
    let mu exp = { exp = exp; meta = () }

    let Str x = Lit (LString x) |> mu
    let Num x = Lit (LNumber x) |> mu
    let Bool x = Lit (LBool x) |> mu
    let Unit : UExp = Lit LUnit |> mu

    let Var x = Var x |> mu
    let App e1 e2 = App (e1, e2) |> mu
    let Abs (x: Ident) e = Abs (mu x, e) |> mu
    let Let x e1 e2 = Let (x, e1, e2) |> mu

    // convenience
    let Appn e es =
        let rec apply current es =
            match es with
            | [] -> current
            | [x] -> App current x
            | x :: xs ->
                let current = App current x
                apply current xs
        apply e es



//let env = AnnotatedAst.Env.empty
// TODO: convenience for importing .Net methods
module EnvCfg =
    let numberTyp = TApp(KnownTypeNames.number, [])
    let unitTyp = TApp(KnownTypeNames.unit, [])

    let add =
        "add",
        let typ =
            let typ = TFun(numberTyp, TFun(numberTyp, numberTyp))
            Extern(typ)
        in typ
    let read =
        "read",
        let typ =
            let typ = TFun(unitTyp, numberTyp)
            Extern(typ)
        in typ
    let map =
        "map",
        let typ =
            let v1,v2 = ConstraintGraph.newGenVar(), ConstraintGraph.newGenVar()
            let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
            let projTyp = TFun(TGenVar v1, TGenVar v2)
            let retTyp = TApp(KnownTypeNames.seq, [TGenVar v2])
            let typ = TFun(seqTyp, TFun(projTyp, retTyp))
            Extern(typ)
        in typ
    let take =
        "take",
        let typ =
            let v1 = ConstraintGraph.newGenVar()
            let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
            let retTyp = TGenVar v1
            let typ = TFun(seqTyp, TFun(numberTyp, retTyp))
            Extern(typ)
        in typ
    let skip =
        "skip",
        let typ =
            let v1 = ConstraintGraph.newGenVar()
            let seqTyp = TApp(KnownTypeNames.seq, [TGenVar v1])
            let retTyp = TGenVar v1
            let typ = TFun(seqTyp, TFun(numberTyp, retTyp))
            Extern(typ)
        in typ
    let numbers =
        "Numbers",
        let typ =
            let typ = TApp(KnownTypeNames.seq, [ TApp(KnownTypeNames.number, []) ])
            Extern(typ)
        in typ

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
|> Visu.showLightAst fullEnv
|> Visu.showAnnotatedAst fullEnv
|> Visu.showConstraintGraph fullEnv
|> Visu.showSolvedGraph fullEnv
|> Visu.showSolvedAst fullEnv



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

