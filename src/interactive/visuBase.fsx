
#load "testBase.fsx"
open TypeFighter
open TypeFighter.DotNetCodeGen

#load "visu/visu.fsx"
open Visu

module Format =
    let getUnionCaseName x =
        match Reflection.FSharpValue.GetUnionFields(x, x.GetType()) with
        | c, _ -> c.Name

    let tyvar (ident: string) (x: string) =
        $"'{ident}' : {x}"

    let recordFieldNames fields = fields |> String.concat "; " |> sprintf "{ %s }"
    let recordFields fields = fields |> List.map fst |> recordFieldNames

    let constraintState cs =
        match cs with
        | Some (Constrained t) -> Format.tau t
        | Some (UnificationError (Origin e)) -> $"ERROR: {e}"
        | Some (UnificationError Inherit) -> $"ERROR (inherited)"
        | None -> "???"

    let envItem ident envItem =
        match envItem with
        | Intern tv -> $"{tyvar ident (string tv)}"
        | Extern t -> $"{tyvar ident (Format.tau t)}"

    let env exp =
        match exp.env |> Map.toList with
        | [] -> "[ ]"
        | [(ident, item)] ->
            $"[ {envItem ident item} ]"
        | _ ->
            [ for x in exp.env do $"-  {envItem x.Key x.Value}" ]
            |> String.concat "\n"
            |> fun s -> $"\n{s}"
    
let writeAnnotatedAst 
        (showVar: bool) 
        (showEnv: bool) 
        (showConstraint: bool) 
        (res: AnnotatedAst.AnnotationResult)
        (constraints: Map<TyVar, ConstraintState>)
        =
    let rec flatten (node: Tree.Node) =
        [
            yield node
            for c in node.children do
                yield! flatten c
        ]
    let rec createNodes (exp: TExp) =
        let details =
            [
                if showVar then yield $"var = {exp.meta.tyvar}"
                if showConstraint then
                    let constr = constraints |> Map.tryFind exp.meta.tyvar 
                    yield $"type = {Format.constraintState constr}"
                if showEnv then yield $"env = {Format.env exp.meta}"
            ]
            |> String.concat "\n"
        match exp.exp with
        | Lit x ->
            let value =
                match x with
                | LString x -> x :> obj
                | LNumber x -> x :> obj
                | LBool x -> x :> obj
                | LUnit -> "()" :> obj
            Tree.var $"Lit ({value})" details []
        | Var ident ->
            Tree.var $"Var ({ident})" details []
        | App (e1, e2) ->
            Tree.var "App" details [ createNodes e1; createNodes e2 ]
        | Abs (ident, body) ->
            Tree.var $"Fun ({ident.exp})" details [ createNodes body ]
        | Let (ident, e, body) ->
            Tree.var $"Let {ident}" details [ createNodes e; createNodes body ]
        | Prop (ident, e) ->
            Tree.var $"Prop {ident}" details [ createNodes e ]
        | Tuple es ->
            Tree.var $"Tuple" details [ for e in es do createNodes e ]
        | Record fields ->
            let fieldNames = Format.recordFields fields
            let details = $"fields = {fieldNames}\n{details}"
            Tree.var $"Record" details [ for _,e in fields do createNodes e ]
    createNodes res.root |> flatten |> Tree.write

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
                        | Some x -> Format.getUnionCaseName x.exp
                    $"{x.tyvar} ({expName})", NodeTypes.var
                | ConstraintGraph.MakeFun _ -> "MakeFun", NodeTypes.op
                | ConstraintGraph.Arg x -> $"Arg {x.argOp}", NodeTypes.op
                | ConstraintGraph.GetProp x -> $"GetProp ({x.field})", NodeTypes.op
                | ConstraintGraph.MakeRecord x -> $"MakeRecord ({Format.recordFieldNames x.fields})", NodeTypes.op
                | _ -> Format.getUnionCaseName x.data, NodeTypes.op
            { key = i
              name = name
              desc = Format.constraintState x.constr
              layout = layout }
        ]
    Graph.write jsNodes jsLinks
    
let private showAst env (showVar: bool) (showEnv: bool) (showConstraint: bool) exp constraints =
    let annoRes = AnnotatedAst.create env exp
    do writeAnnotatedAst showVar showEnv showConstraint annoRes constraints
    exp

let showLightAst env exp = showAst env false false false exp Map.empty
let showAnnotatedAst env exp = showAst env true true false exp Map.empty
let showConstraintGraph env exp =
    let annoRes = AnnotatedAst.create env exp
    do annoRes |> ConstraintGraph.create |> writeConstraintGraph annoRes.allExpressions
    exp
let solve env exp =
    let annoRes = AnnotatedAst.create env exp
    let nodes = annoRes |> ConstraintGraph.create
    ConstraintGraph.solve annoRes nodes
let showSolvedGraph env exp =
    let res = solve env exp
    do res.allNodes |> writeConstraintGraph res.annotationResult.allExpressions
    res
let showSolvedAst env exp =
    let res = solve env exp
    do writeAnnotatedAst true false true res.annotationResult res.varsAndConstraints
    res



let renderDisplayClasses env exp =
    //let exp = App (Abs "__" exp) (Num 0.0)
    exp
    |> solve env 
    |> fun res -> renderDisplayClasses (RecordCache()) res
    |> fun res ->
        printfn ""
        printfn ""
        printfn "%s" res
        printfn ""
        printfn ""
let render env exp =
    exp
    |> solve env 
    |> fun res -> render res
    |> fun res ->
        printfn "------------------"
        printfn ""
        printfn "%s" res.records
        printfn ""
        printfn "%s" res.body
        printfn ""
        printfn "------------------"
