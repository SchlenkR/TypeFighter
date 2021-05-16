
#load "testBase.fsx"
open TypeFighter
//open TypeFighter.DotNetCodeGen

#load "visu/visu.fsx"
open Visu


module Format =
    let tyvar (ident: string) (x: string) =
        $"'{ident}' : {x}"

    let recordFieldNames fields = fields |> String.concat "; " |> sprintf "{ %s }"
    let recordFields fields = fields |> List.map fst |> recordFieldNames

    let constraintState cs =
        match cs with
        | Some(Constrained t) -> Format.tau t
        | Some(UnificationError (Origin e)) -> $"ERROR: {e}"
        | Some(UnificationError Inherit) -> $"ERROR (inherited)"
        | None -> "???"

    let items items fmt =
        match items with
        | [] -> "[ ]"
        | [x] ->
            $"[ {fmt x} ]"
        | xs ->
            [ for x in xs do $"-  {fmt x}" ]
            |> String.concat "\n"
            |> fun s -> $"\n{s}"

    let substs (substs: Set<Subst>) =
        let fmt s = $"{Format.genVar s.genTyVar} = {Format.tau s.substitute}"
        items (substs |> Set.toList) fmt

    let env exp (envCs: Map<IExp, ConstraintState * Set<Subst>>) =
        let fmt (ident,item) =
            match item with
            | Extern t ->
                $"{tyvar ident (Format.tau t)}"
            | Intern (tv,parentTv) ->
                let tvstring = $"(var={tv};parent={parentTv})"
                let csstring =
                    envCs
                    |> Seq.tryFind (fun x -> x.Key.meta.tyvar = tv)
                    |> Option.map (fun x -> x.Value)
                    |> Option.map fst
                    |> constraintState
                let content = $"{tvstring} {csstring}"
                $"{tyvar ident content}"
        items (exp.env |> Map.toList) fmt


[<AutoOpen>]
module Show =
    open ConstraintGraph

    let writeAnnotatedAst 
            (showVar: bool) 
            (showEnv: bool) 
            (showConstraint: bool)
            (showSubsts: bool)
            (res: AnnotatedAst.AnnotationResult)
            (exprConstraintStates: Map<TExp, ConstraintState * Set<Subst>>)
            (envConstraintStates: Map<IExp, ConstraintState * Set<Subst>>)
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
                    let constrSubsts = lazy (exprConstraintStates |> Map.find exp)

                    if showVar then yield $"var = {exp.meta.tyvar}"
                    if showConstraint then
                        yield $"type = {Format.constraintState (Some (fst constrSubsts.Value))}"
                    if showSubsts then
                        yield $"substs = {Format.substs (snd constrSubsts.Value)}"
                    if showEnv then yield $"env = {Format.env exp.meta envConstraintStates}"
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
                Tree.var $"Fun ({ident.exp}) ->" details [ createNodes body ]
            | Let (ident, e, body) ->
                Tree.var $"Let {ident.exp} = ..." details [ createNodes e; createNodes body ]
            | Prop (ident, e) ->
                Tree.var $"Prop {ident}" details [ createNodes e ]
            | Tuple es ->
                Tree.var $"Tuple" details [ for e in es do createNodes e ]
            | Record fields ->
                let fieldNames = Format.recordFields fields
                let details = $"fields = {fieldNames}\n{details}"
                Tree.var $"Record" details [ for _,e in fields do createNodes e ]
        createNodes res.root |> flatten |> Tree.write

    let writeConstraintGraph (nodes: Node seq) =
        let indexedNodes = nodes |> Seq.indexed |> Seq.toList
        let jsLinks =
            [ 
                let nodesLookup = indexedNodes |> List.map (fun (a,b) -> b,a) |> readOnlyDict
                for n in nodes do
                    for i in getIncomingNodes n do
                        { Visu.JsLink.fromNode = nodesLookup.[i]
                          Visu.JsLink.toNode = nodesLookup.[n] }
            ]
        let jsNodes =
            [ for i,n in indexedNodes do
                let name,layout =
                    match n.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Ast { exp = TExp exp } ->
                        let expName = Format.getUnionCaseName exp.exp
                        let name = $"{exp.meta.tyvar} ({expName})"
                        name,NodeTypes.var
                    | Ast { exp = IExp exp } ->
                        let expName = $"Env ({exp.exp})"
                        let name = $"{exp.meta.tyvar} ({expName})"
                        name,NodeTypes.var
                    | MakeFun _ -> "MakeFun", NodeTypes.op
                    | GetProp x -> $"GetProp ({x.field})", NodeTypes.op
                    | MakeRecord x -> $"MakeRecord ({Format.recordFieldNames x.fields})", NodeTypes.op
                    | Inst x -> $"Inst ({x.scope})", NodeTypes.op
                    | _ -> Format.getUnionCaseName n.data, NodeTypes.op
                { key = i
                  name = name
                  desc = 
                    [
                        yield Format.constraintState (n.constr |> Option.map fst)
                        yield 
                            n.constr 
                            |> Option.map snd 
                            |> Option.map (fun substs -> $"substs = {Format.substs substs}")
                            |> Option.defaultValue ""
                    ]
                    |> String.concat "\n"
                  layout = layout }
            ]
        Graph.write jsNodes jsLinks
    
    let private showAst env showVar showEnv showConstraint showSubsts exp exprCs envCs =
        let annoRes = AnnotatedAst.create (Map.ofList env) exp
        do writeAnnotatedAst showVar showEnv showConstraint showSubsts annoRes exprCs envCs
        annoRes

    let showLightAst env exp = showAst env false false false false exp Map.empty Map.empty
    let showAnnotatedAst env exp = showAst env true true false false exp Map.empty Map.empty
    let showConstraintGraph env exp =
        let annoRes = AnnotatedAst.create (Map.ofList env) exp
        do annoRes |> ConstraintGraph.create |> writeConstraintGraph
        annoRes
    let solve env exp =
        let annoRes = AnnotatedAst.create (Map.ofList env) exp
        let nodes = annoRes |> ConstraintGraph.create
        ConstraintGraph.solve annoRes nodes
    let showSolvedGraph env exp =
        let res = solve env exp
        do res.allNodes |> writeConstraintGraph
        res
    let showSolvedAst env exp =
        let res = solve env exp
        do writeAnnotatedAst true false true true res.annotationResult res.exprConstraintStates res.envConstraintStates
        res
    let showSolvedAstWEnv env exp =
        let res = solve env exp
        do writeAnnotatedAst true true true true res.annotationResult res.exprConstraintStates res.envConstraintStates
        res

[<AutoOpen>]
module CodeGen =
    open DotNetCodeGen

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
