
#load "testBase.fsx"
open TypeFighter

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

    let env (exp: TExp) (envCs: Map<IExp, ConstraintState * Set<Instanciation> * Set<Subst>>) =
        let fmt (ident,item) =
            match item with
            | Extern t ->
                tyvar ident (Format.tau t)
            | Intern tv ->
                let tvstring = $"tv={tv}"
                //let csstring =
                //    envCs
                //    |> Seq.tryFind (fun x -> x.Key = exp)
                //    |> Option.map (fun x -> x.Value)
                //    |> Option.map (fun (x,_,_) -> x)
                //    |> constraintState
                //let content = $"{csstring} | {tvstring}"
                let content = tvstring
                tyvar ident content
        items (exp.meta.env |> Map.toList) fmt


[<AutoOpen>]
module Show =
    open Annotation
    open NewStuff

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
                    if showEnv then yield $"env = {Format.env exp envConstraintStates}"
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

    let writeConstraintGraph (nodes: Node list) =
        let jsLinks =
            [ 
                for n in nodes do
                    for i in getIncomingNodeIds n do
                        { Visu.JsLink.fromNode = i
                          Visu.JsLink.toNode = n.id }
            ]
        let jsNodes =
            [ for n in nodes do
                let name,layout =
                    match n.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Ast ast ->
                        $"AST (tv={n.id})", NodeTypes.var
                    | MakeFun _ -> "MakeFun", NodeTypes.op
                { key = n.id
                  name = name
                  desc = n.desc
                    //[
                    //    yield Format.constraintState (n.constr |> Option.map (fun (x,_,_) -> x))
                    //    match n.constr with
                    //    | None -> ()
                    //    | Some constr ->
                    //        let _,insts,substs = constr
                    //        yield $"insts = {Format.insts insts}"
                    //        yield $"substs = {Format.substs substs}"
                    //]
                    //|> String.concat "\n"
                  layout = layout }
            ]
        Graph.write jsNodes jsLinks
    
    let private showAst env showVar showEnv showConstraint showSubsts exp exprCs envCs =
        let annoRes = annotate (Map.ofList env) exp
        do writeAnnotatedAst showVar showEnv showConstraint showSubsts annoRes exprCs envCs
        annoRes

    let showLightAst env exp = showAst env false false false false exp Map.empty Map.empty
    let showAnnotatedAst env exp = showAst env true true false false exp Map.empty Map.empty
    let showConstraints env exp =
        let annoRes = annotate (Map.ofList env) exp
        let constraints = annoRes |> createGraph
        for n in constraints do
            let right =
                match n.data with
                | Source s -> Format.tau s
                | Ast ast -> ast.incs |> List.map string |> String.concat ","
                | MakeFun f -> $"{f.inc1} -> {f.inc2}"
            printfn $"{n.id} = {right}"
    let showConstraintGraph env exp =
        let annoRes = annotate (Map.ofList env) exp
        do annoRes |> createGraph |> writeConstraintGraph
        annoRes
    //let solve env exp =
    //    let annoRes = annotate (Map.ofList env) exp
    //    let nodes = annoRes |> createGraph
    //    solve annoRes nodes
    //let showSolvedGraph env exp =
    //    let res = solve env exp
    //    do res.graph |> writeConstraintGraph
    //    res
    //let showSolvedAst env exp =
    //    let res = solve env exp
    //    do writeAnnotatedAst true false true true res.annotationResult res.exprConstraintStates res.envConstraintStates
    //    res
    //let showSolvedAstWEnv env exp =
    //    let res = solve env exp
    //    do writeAnnotatedAst true true true true res.annotationResult res.exprConstraintStates res.envConstraintStates
    //    res

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
