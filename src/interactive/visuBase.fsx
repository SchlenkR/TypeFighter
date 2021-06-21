
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

    let insts (insts: Set<Instanciation>) =
        let fmt i = $"{Format.genVar i.oldVar} = {Format.genVar i.newVar}"
        items (insts |> Set.toList) fmt

    let env exp (envBoundValues: Map<IExp, TExp option>) (envCs: Map<IExp, ConstraintState * Set<Instanciation> * Set<Subst>>) =
        let fmt (ident,item) =
            match item with
            | Extern t ->
                tyvar ident (Format.tau t)
            | Intern exp ->
                let tvstring = $"tv={exp.meta.tyvar}"
                let csstring =
                    envCs
                    |> Seq.tryFind (fun x -> x.Key = exp)
                    |> Option.map (fun x -> x.Value)
                    |> Option.map (fun (x,_,_) -> x)
                    |> constraintState
                let boundValue =
                    envBoundValues
                    |> Map.find exp
                    |> Option.map (fun x -> string x.meta.tyvar)
                    |> Option.defaultValue "-"
                    |> sprintf "ref=%s"
                let content = $"{csstring} | {tvstring} {boundValue}"
                tyvar ident content
        items (exp.env |> Map.toList) fmt


[<AutoOpen>]
module Show =
    open Annotation
    open ConstraintGraph

    let writeAnnotatedAst 
            (showVar: bool) 
            (showEnv: bool) 
            (showConstraint: bool)
            (showSubsts: bool)
            (res: AnnotationResult) 
            (exprConstraintStates: Map<TExp, ConstraintState * Set<Instanciation> * Set<Subst>>)
            (envConstraintStates: Map<IExp, ConstraintState * Set<Instanciation> * Set<Subst>>)
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
                        yield $"type = {Format.constraintState (Some (constrSubsts.Value |> fun (x,_,_) -> x))}"
                    if showSubsts then
                        yield $"insts = {Format.insts (constrSubsts.Value |> fun (_,x,_) -> x)}"
                        yield $"substs = {Format.substs (constrSubsts.Value |> fun (_,_,x) -> x)}"
                    if showEnv then yield $"env = {Format.env exp.meta res.identLinks envConstraintStates}"
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
            | Tuple es ->
                Tree.var $"Tuple" details [ for e in es do createNodes e ]
        createNodes res.root |> flatten |> Tree.write

    let writeConstraintGraph (graph: Graph) =
        let jsLinks =
            [ 
                for n in graph.nodes do
                    for i in getIncomingNodeIds n.Value do
                        { Visu.JsLink.fromNode = i
                          Visu.JsLink.toNode = n.Key }
            ]
        let jsNodes =
            [ for kvp in graph.nodes do
                let i = kvp.Key
                let n = kvp.Value
                let name,layout =
                    match n.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | TAst ast ->
                        let expName = Format.getUnionCaseName ast.exp.exp
                        let name = $"{ast.exp.meta.tyvar} ({expName})"
                        name,NodeTypes.var
                    | IAst ast ->
                        let expName = $"Env ({ast.exp.exp})"
                        let name = $"{ast.exp.meta.tyvar} ({expName})"
                        name,NodeTypes.var
                    | MakeFun _ -> "MakeFun", NodeTypes.op
                    | Inst x -> $"Inst ({x.scope})", NodeTypes.op
                    | _ -> Format.getUnionCaseName n.data, NodeTypes.op
                { key = i
                  name = name
                  desc = 
                    [
                        yield Format.constraintState (n.constr |> Option.map (fun (x,_,_) -> x))
                        match n.constr with
                        | None -> ()
                        | Some constr ->
                            let _,insts,substs = constr
                            yield $"insts = {Format.insts insts}"
                            yield $"substs = {Format.substs substs}"
                    ]
                    |> String.concat "\n"
                  layout = layout }
            ]
        Graph.write jsNodes jsLinks
    
    let private showAst env showVar showEnv showConstraint showSubsts exp exprCs envCs =
        let annoRes = annotate (Map.ofList env) exp
        do writeAnnotatedAst showVar showEnv showConstraint showSubsts annoRes exprCs envCs
        annoRes

    let showLightAst env exp = showAst env false false false false exp Map.empty Map.empty
    let showAnnotatedAst env exp = showAst env true true false false exp Map.empty Map.empty
    let showConstraintGraph env exp =
        let annoRes = annotate (Map.ofList env) exp
        do annoRes |> createGraph |> writeConstraintGraph
        annoRes
    let solve env exp =
        let annoRes = annotate (Map.ofList env) exp
        let nodes = annoRes |> createGraph
        solve annoRes nodes
    let showSolvedGraph env exp =
        let res = solve env exp
        do res.graph |> writeConstraintGraph
        res
    let showSolvedAst env exp =
        let res = solve env exp
        do writeAnnotatedAst true false true true res.annotationResult res.exprConstraintStates res.envConstraintStates
        res
    let showSolvedAstWEnv env exp =
        let res = solve env exp
        do writeAnnotatedAst true true true true res.annotationResult res.exprConstraintStates res.envConstraintStates
        res

//[<AutoOpen>]
//module CodeGen =
//    open DotNetCodeGen

//    let renderDisplayClasses env exp =
//        //let exp = App (Abs "__" exp) (Num 0.0)
//        exp
//        |> solve env 
//        |> fun res -> renderDisplayClasses (RecordCache()) res
//        |> fun res ->
//            printfn ""
//            printfn ""
//            printfn "%s" res
//            printfn ""
//            printfn ""

//    let render env exp =
//        exp
//        |> solve env 
//        |> fun res -> render res
//        |> fun res ->
//            printfn "------------------"
//            printfn ""
//            printfn "%s" res.records
//            printfn ""
//            printfn "%s" res.body
//            printfn ""
//            printfn "------------------"
