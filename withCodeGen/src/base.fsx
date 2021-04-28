
#r "nuget: Basic.Reference.Assemblies"
#r "nuget: Microsoft.CSharp"
#r "nuget: Microsoft.CodeAnalysis"
#r "nuget: Microsoft.CodeAnalysis.CSharp"
#r "./CoreLib/bin/Debug/net5.0/CoreLib.dll"
#r "./TestData/bin/Debug/net5.0/TestData.dll"
#load "./visu/visu.fsx"



type Lit =
    | LString of string
    | LNumber of float
    | LBool of bool
    | LUnit
type Exp =
    | Lit of Lit
    | Var of string
    | App of Exp * Exp
    | Abs of string * Exp
    | Let of string * Exp * Exp
    | Prop of string * Exp
    | Tup of List<Exp>
    | Record of List<string * Exp>
    | View of string * Exp

type GenVar = int
type Tau =
    | TGenVar of GenVar
    | TApp of string * Tau list
    | TFun of Tau * Tau
    | TTup of List<Tau>
    | TRecord of List<string * Tau>
type Forall = GenVar list * Tau
type Sigma =
    | Forall of Forall
type Constraint =
    | CTau of Tau
    | CSigma of Sigma

type Ident = string
type TyVar = int
type EnvItem =
    | Extern of Constraint
    | Intern of TyVar
type Env = Map<Ident, EnvItem>

module KnownTypeNames =
    let string = "String"
    let number = "Number"
    let bool = "Bool"
    let unit = "Unit"
    let seq = "Seq"

module Lit =
    let getTypeName (l: Lit) =
        match l with
        | LString _ -> KnownTypeNames.string
        | LNumber _ -> KnownTypeNames.number
        | LBool _ -> KnownTypeNames.bool
        | LUnit _ -> KnownTypeNames.unit

    let getValue (l: Lit) =
        match l with
        | LString x -> x :> obj
        | LNumber x -> x :> obj
        | LBool x -> x :> obj
        | LUnit -> "()" :> obj

module Env =
    let empty : Env = Map.empty
    let bind ident (tyvar: TyVar) (env: Env) : Env =
        env |> Map.change ident (fun _ -> Some(Intern tyvar))
    let resolve varName (env: Env) =
        match env |> Map.tryFind varName with
        | None -> failwith $"Variable '{varName}' is unbound. Env: {env}"
        | Some t -> t

module Count =
    let create f =
        let mutable varCounter = 0
        fun () ->
            varCounter <- f varCounter 1
            varCounter
    let up () = create (+)
    let down () = create (-)

module AnnotatedAst =

    type Annotated<'expr> =
        { annotated: 'expr
          tyvar: TyVar
          env: Env }
    type TExp =
        | TELit of Lit
        | TEVar of Ident
        | TEApp of Annotated<TExp> * Annotated<TExp>
        | TEAbs of Annotated<Ident> * Annotated<TExp>
        | TELet of Ident * Annotated<TExp> * Annotated<TExp>
        // syntax sugar
        | TEProp of string * Annotated<TExp>
        | TETup of List<Annotated<TExp>>
        | TERecord of List<string * Annotated<TExp>>
        | TEView of string * Annotated<TExp>
    
    let create (env: Env) (exp: Exp) =
        let newvar = Count.up()
        let rec annotate (env: Env) (exp: Exp) =
            let thisTyVar = newvar()
            let annotatedExp =
                match exp with
                | Lit x ->
                    TELit x
                | Var ident ->
                    TEVar ident
                | Abs (ident, body) ->
                    let tyvarIdent = newvar()
                    let newEnv = env |> Env.bind ident tyvarIdent
                    let annotatedIdent = { annotated = ident; tyvar = tyvarIdent; env = env }
                    TEAbs (annotatedIdent, annotate newEnv body)
                | App (e1, e2) ->
                    TEApp (annotate env e1, annotate env e2)
                | Let (ident, e, body) ->
                    let newEnv = env |> Env.bind ident (newvar())
                    TELet (ident, annotate env e, annotate newEnv body)
                | Prop (name, e) ->
                    TEProp (name, annotate env e)
                | Tup es ->
                    let annotatedEs = es |> List.map (annotate env)
                    TETup annotatedEs
                | Record fields ->
                    let annotatedFields = fields |> List.map (fun (name, e) -> name, annotate env e)
                    TERecord annotatedFields
                | View (name, e) ->
                    TEView (name, annotate env e)
            { tyvar = thisTyVar
              annotated = annotatedExp
              env = env }
        annotate env exp

module ConstraintGraph =
    open AnnotatedAst

    type ConstraintState =
        | UnificationError of string
        | Constrained of Constraint
    
    type Op =
        | MakeFun
        | ApplyFun
    type NodeData =
        | Source of Constraint
        | Var of TyVar
        | Op of Op
    type [<ReferenceEquality>] Node =
        { data: NodeData
          mutable constr: ConstraintState option
          mutable incoming: Edge list
          mutable outgoing: Edge list }
    and [<CustomEquality; CustomComparison>] Edge =
        { fromNode: Node
          toNode: Node }
            interface System.IComparable with
                member this.CompareTo(other) =
                    this.GetHashCode().CompareTo((other :?> Edge).GetHashCode())
            override this.Equals(other) =
                let other = other :?> Edge
                this.fromNode = other.fromNode && this.toNode = other.toNode
            override this.GetHashCode() = hash (this.fromNode, this.toNode)
    and Graph =
        { root: Node 
          nodes: ResizeArray<Node> }
    
    module Node =
        let connectNodes (fromNode: Node) (toNode: Node) =
            let edge = { fromNode = fromNode; toNode = toNode }
            do
                fromNode.outgoing <- edge :: fromNode.outgoing
                toNode.incoming <- edge :: toNode.incoming
    
    module Graph =
        let addNode n (nodes: ResizeArray<Node>) =
            let node =
                { data = n
                  constr = match n with | Source c -> Some(Constrained c) | _ -> None
                  incoming = []
                  outgoing = [] }
            do
                nodes.Add node
            node
        let addVarNode n nodes = addNode (Var n) nodes
        let addFuncNode n1 n2 ntarget nodes =
            let nfunc = nodes |> addNode (Op MakeFun)
            do
                Node.connectNodes n1 nfunc
                Node.connectNodes n2 nfunc
                Node.connectNodes nfunc ntarget
        let addApplyFuncNode nsource ntarget nodes =
            let napp = nodes |> addNode (Op ApplyFun)
            do
                Node.connectNodes nsource napp
                Node.connectNodes napp ntarget
        let findNode (tyvar: TyVar) (nodes: Node seq) =
            nodes |> Seq.find (fun n ->
                match n.data with
                | Var v when v = tyvar -> true
                | _ -> false)
    
    let create (exp: Annotated<TExp>) =
        let nodes = ResizeArray()
        let rec generateGraph (exp: Annotated<TExp>) =
            match exp.annotated with
            | TELit x ->
                let node = nodes |> Graph.addVarNode exp.tyvar
                let nsource = nodes |> Graph.addNode (Source(CTau(TApp(Lit.getTypeName x, []))))
                do
                    Node.connectNodes nsource node
                node
            | TEVar ident ->
                let node = nodes |> Graph.addVarNode exp.tyvar
                let identEnvItem = Env.resolve ident exp.env
                match identEnvItem with
                | Intern tyvarIdent ->
                    Node.connectNodes (nodes |> Graph.findNode tyvarIdent) node
                | Extern c -> 
                    let nsource = nodes |> Graph.addNode (Source c)
                    Node.connectNodes nsource node
                node
            | TEApp (e1, e2) ->
                let ne1 = generateGraph e1
                let ne2 = generateGraph e2
                let napp = nodes |> Graph.addVarNode exp.tyvar
                do
                    nodes |> Graph.addFuncNode ne2 napp ne1
                    nodes |> Graph.addApplyFuncNode ne1 napp
                napp
            | TEAbs (ident, body) ->
                let nident = nodes |> Graph.addVarNode ident.tyvar
                let nfun = nodes |> Graph.addVarNode exp.tyvar
                do
                    nodes |> Graph.addFuncNode nident (generateGraph body) nfun
                nfun
            | TELet (ident, e, body) ->
                let nident =
                    let identEnvItem = body.env |> Env.resolve ident
                    match identEnvItem with
                    | Intern tyvarIdent ->
                        nodes |> Graph.addVarNode tyvarIdent
                    | Extern c -> 
                        nodes |> Graph.addNode (Source c)
                let nlet = nodes |> Graph.addVarNode exp.tyvar
                do
                    Node.connectNodes (generateGraph body) nlet
                    Node.connectNodes (generateGraph e) nident
                nlet
            | TEProp (name, e) ->
                let nprop = nodes |> Graph.addVarNode exp.tyvar
                Node.connectNodes (generateGraph e) nprop
                nprop
            | TETup es ->
                // TODO: Tuple type
                let ntup = nodes |> Graph.addVarNode exp.tyvar
                for e in es do
                    Node.connectNodes (generateGraph e) ntup
                ntup
            | TERecord fields ->
                // TODO: Record type
                let nrecord = nodes |> Graph.addVarNode exp.tyvar
                for fname,e in fields do
                    Node.connectNodes (generateGraph e) nrecord
                nrecord
            | TEView (name, e) ->
                let nview = nodes |> Graph.addVarNode exp.tyvar
                Node.connectNodes (generateGraph e) nview
                nview
        let rootNode = generateGraph exp
        { nodes = nodes; root = rootNode }

module Compiler =
    open System.IO
    open System.Reflection
    open Microsoft.CodeAnalysis
    open Microsoft.CodeAnalysis.CSharp
    
    type Query<'context> = ('context * CoreLib.ViewCache) -> obj
    
    let contextArgName = "context"
    let viewCacheArgName = "viewCache"

    let transpile (exp: Exp) =
        let quote = "\""
        let csTrue = "true"
        let csFalse = "false"

        let rec toCode (exp: Exp) =
            [
                match exp with
                | Lit l ->
                    match l with
                    | LString x -> quote + x + quote
                    | LNumber x -> x.ToString(System.Globalization.CultureInfo.InvariantCulture) + "d"
                    | LBool x -> if x then csTrue else csFalse
                    | LUnit -> $"{nameof(CoreLib.Unit)}.{nameof(CoreLib.Unit.Instance)}"
                | Var ident ->ident
                | App (e1, e2) ->
                    $"{toCode e1}({toCode e2})"
                | Abs (ident, body) ->
                    $"({ident} => ({toCode body}))"
                | Let (ident, e, body) ->
                    $"{nameof(CoreLib.CompilerHelper.Exp)}(() => {{ var {ident} = {toCode e}; return {toCode body}; }})"
                | Prop (name, e) ->
                    $"{toCode e}.{name}"
                | Tup es ->
                    es |> List.map toCode |> String.concat ", "
                | Record fields ->
                    let assExp =
                        [ for l,e in fields do $"{l} = ({toCode e})" ]
                        |> String.concat ", "
                    $"new {{ {assExp} }}"
                | View (name, e) ->
                    $"""{viewCacheArgName}.GetOrUpdate("{name}", () => {{ return ({toCode e}); }})"""
            ]
            |> String.concat " "
        toCode exp

    let compileCode (code: string) (assemblies: seq<Assembly>) =
        printfn "Code:\\n\n%s\n\n\n" code

        let syntaxTree = CSharpSyntaxTree.ParseText(code)
        let res =
            CSharpCompilation.Create(
                "dynamic",
                [ syntaxTree ],
                references = [
                    yield! 
                        Basic.Reference.Assemblies.ReferenceAssemblies.NetStandard20 
                        |> Seq.map (fun x -> x :> MetadataReference)
                    for asm in assemblies do 
                        yield MetadataReference.CreateFromFile(asm.Location) :> MetadataReference
                ],
                options = CSharpCompilationOptions(
                    OutputKind.DynamicallyLinkedLibrary,
                    optimizationLevel = OptimizationLevel.Debug))

        use ms = new MemoryStream()
        let emitRes = res.Emit(ms)
        if not emitRes.Success then
            let failures =
                [
                    for d in emitRes.Diagnostics do
                        if d.IsWarningAsError || d.Severity = DiagnosticSeverity.Error then
                            yield $"    {d.Id} ({d.Location}): {d.GetMessage()}"
                ]
                |> String.concat "\n"
            failwith $"Compilation Error:\n {failures}"
        do ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let bytes = ms.ToArray()
        Assembly.Load(bytes)

    let compileExp<'context> (assemblies: seq<Assembly>) (exp: Exp) : Query<'context> =
        let generatedClassName = "Query"
        let queryMethodName = "Eval"
        let code =
            @$"
                using System;
                using CoreLib;

                using static {nameof(CoreLib)}.{nameof(CoreLib.CompilerHelper)};
                using static {nameof(CoreLib)}.{nameof(CoreLib.Exports)};

                public static class {generatedClassName}
                {{
                    public static object {queryMethodName}(
                        {typeof<'context>.FullName} {contextArgName},
                        {nameof(CoreLib)}.{nameof(CoreLib.ViewCache)} {viewCacheArgName})
                    {{
                        return {transpile exp};
                    }}
                }}
            "

        let asm = compileCode code [
                typeof<CoreLib.CompilerHelper>.Assembly
                typeof<TestData.Context>.Assembly
                yield! assemblies
            ]
            
        let rootType = asm.GetTypes() |> Seq.find (fun t -> t.Name = generatedClassName)
        let queryMethod = rootType.GetMethod queryMethodName
        let query = fun (c: 'context, viewCache: CoreLib.ViewCache) -> queryMethod.Invoke(null, [| c; viewCache |])
        query

    let run x (q: Query<_>) = q x
    let compileAndRun context assemblies exp = compileExp assemblies exp |> run context

let printSeq (s: obj) =
    s
    :?> System.Collections.IEnumerable
    |> System.Linq.Enumerable.Cast<obj>
    |> Seq.iter (printfn "%A")



module Visu =
    open Visu

    let showUntypedAst (exp: Exp) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
    
        let rec createNodes (exp: Exp) =
            match exp with
            | Lit x ->
                Tree.var $"Lit" $"{Lit.getValue x} : {(Lit.getTypeName x)}" []
            | Var ident ->
                Tree.var $"Var" ident []
            | App (e1, e2) ->
                Tree.var $"App" "" [ createNodes e1; createNodes e2 ]
            | Abs (ident, body) ->
                Tree.var $"Abs {ident} -> e" "" [ createNodes body ]
            | Let (ident, e, body) ->
                Tree.var $"Let" ident [ createNodes e; createNodes body ]
            | Prop (name, e) ->
                Tree.var $"Prop" name [ createNodes e ]
            | Tup es ->
                Tree.var $"Tup" "" [ for e in es do createNodes e ]
            | Record fields ->
                let labels = fields |> List.map fst |> String.concat ", "
                Tree.var $"Record" labels [ for _,e in fields do createNodes e ]
            | View (name, e) ->
                Tree.var $"View" name [ createNodes e ]

        createNodes exp |> flatten |> Tree.write

    open AnnotatedAst
    
    let formatTyvar (ident: string) (x: string) =
        $"'{ident}' : {x}"

    let rec formatTau (tau: Tau) =
        match tau with
        | TGenVar genVar ->
            $"'{char (genVar + 96)}"
        | TApp (name, args) ->
            match args with
            | [] -> name
            | _ ->
                let args = args |> List.map formatTau |> String.concat ", "
                $"{name}<{args}>"
        | TFun (t1, t2) ->
            $"({formatTau t1} -> {formatTau t2})"
        | TTup taus ->
            let taus = taus |> List.map formatTau |> String.concat " * "
            $"({taus})"
        | TRecord fields ->
            let fields = [ for f,e in fields do $"{f}: {formatTau e}" ] |> String.concat "; "
            $"{{| {fields} |}}"

    let formatConstraint (c: Constraint) =
        match c with
        | CTau t -> formatTau t
        | CSigma(Forall (vars,tau)) -> formatTau tau
        
    let formatEnvItem ident envItem =
        match envItem with
        | Intern tyvar ->
            $"{formatTyvar ident (string tyvar)}"
        | Extern c ->
            $"{formatTyvar ident (formatConstraint c)}"

    let formatTyvarAndEnv exp =
        let envVars =
            match exp.env |> Map.toList with
            | [] -> "[ ]"
            | [(ident, envItem)] ->
                $"[ {formatEnvItem ident envItem} ]"
            | _ ->
                [ for x in exp.env do $"-  {formatEnvItem x.Key x.Value}" ]
                |> String.concat "\n"
                |> fun s -> $"\n{s}"
        ($"var = {exp.tyvar}") + "\nenv = " + envVars

    let showAnnotatedAst (exp: Annotated<TExp>) =
        let rec flatten (node: Tree.Node) =
            [
                yield node
                for c in node.children do
                    yield! flatten c
            ]
    
        let rec createNodes (exp: Annotated<TExp>) =
            match exp.annotated with
            | TELit x ->
                Tree.var $"Lit ({Lit.getValue x}: {Lit.getTypeName x})" (formatTyvarAndEnv exp) []
            | TEVar ident ->
                let envItem = Env.resolve ident exp.env                
                Tree.var $"Var {formatEnvItem ident envItem}" (formatTyvarAndEnv exp) []
            | TEApp (e1, e2) ->
                Tree.var $"App" (formatTyvarAndEnv exp) [ createNodes e1; createNodes e2 ]
            | TEAbs (ident, body) ->
                Tree.var
                    $"""fun {formatTyvar ident.annotated (string ident.tyvar)} -> {formatTyvar "e" (string body.tyvar)}"""
                    (formatTyvarAndEnv exp)
                    [ createNodes body ]
            | TELet (ident, e, body) ->
                Tree.var
                    $"""let {ident} = {formatTyvar "e1" (string e.tyvar)} in {formatTyvar "e2" (string body.tyvar)}"""
                    (formatTyvarAndEnv exp)
                    [ createNodes e; createNodes body ]
            | TEProp (name, e) ->
                Tree.var
                    $"Prop '{name}'"
                    (formatTyvarAndEnv exp)
                    [ createNodes e ]
            | TETup es ->
                Tree.var
                    $"Tuple"
                    (formatTyvarAndEnv exp)
                    (es |> List.map createNodes)
            | TERecord fields ->
                let fieldNames = fields |> List.map fst |> String.concat "; "
                Tree.var
                    $"Record ({fieldNames})"
                    (formatTyvarAndEnv exp)
                    (fields |> List.map snd |> List.map createNodes)
            | TEView (name, e) -> failwith "View are not implemented."

        createNodes exp |> flatten |> Tree.write

    open ConstraintGraph

    let showConstraintGraph (graph: Graph) =
        let edges =
            [ for n in graph.nodes do
              yield! n.incoming
              yield! n.outgoing ]
            |> List.distinct
        let indexedNodes = graph.nodes |> Seq.indexed |> Seq.toList
        let nodesLookup = indexedNodes |> List.map (fun (a,b) -> b,a) |> readOnlyDict
        let jsLinks =
            edges
            |> List.map (fun edge ->
                { Visu.JsLink.fromNode = nodesLookup.[edge.fromNode]
                  Visu.JsLink.toNode = nodesLookup.[edge.toNode] })
        let jsNodes =
            [ for i,x in indexedNodes do
                let name, layout =
                    match x.data with
                    | Source _ -> "SOURCE", NodeTypes.op
                    | Var tyvar -> string tyvar, NodeTypes.var
                    | Op op -> string op, NodeTypes.op
                { key = i
                  name = name
                  desc =
                    match x.constr with
                    | Some (Constrained c) -> formatConstraint c
                    | Some (UnificationError e) -> $"ERROR: {e}"
                    | None -> "???"
                  layout = layout }
            ]

        Graph.write jsNodes jsLinks



[<AutoOpen>]
module Dsl =
    let Str x = Lit (LString x)
    let Num x = Lit (LNumber x)
    let Bool x = Lit (LBool x)
    let Unit = Lit LUnit

    let Var x = Var x
    let App e1 e2 = App (e1, e2)
    let Abs x e = Abs (x, e)
    let Let x e1 e2 = Let (x, e1, e2)
    let Prop e x = Prop (x, e)
    let Tup es = Tup es
    let Record es = Record es
    let View name e = View (name, e)

    // convenience
    let Appn e (es: List<Exp>) =
        let rec apply (current: Exp) (es: List<Exp>) =
            match es with
            | [] -> current
            | [x] -> App current x
            | x :: xs ->
                let current = App current x
                apply current xs
        apply e es
    let Appt e (es: List<Exp>) =
        let t = Tup es
        App e t


//let env = AnnotatedAst.Env.empty

// TODO: convenience for importing .Net methods
module EnvCfg =
    let numberTyp = TApp(KnownTypeNames.number, [])
    let unitTyp = TApp(KnownTypeNames.unit, [])

    let add =
        "add",
        let typ =
            let typ = TFun(TTup([numberTyp; numberTyp]), numberTyp)
            Extern(CTau(typ))
        in typ
    let read =
        "read",
        let typ =
            let typ = TFun(unitTyp, numberTyp)
            Extern(CTau(typ))
        in typ
    let map =
        "map",
        let typ =
            let seqTyp = TApp(KnownTypeNames.seq, [TGenVar 1])
            let projTyp = TFun(TGenVar 1, TGenVar 2)
            let retTyp = TApp(KnownTypeNames.seq, [TGenVar 2])
            let typ = Forall([1; 2], TFun(TTup [seqTyp; projTyp], retTyp))
            Extern(CSigma(typ))
        in typ
    let take =
        "take",
        let typ =
            let seqTyp = TApp(KnownTypeNames.seq, [TGenVar 1])
            let retTyp = TGenVar 1
            let typ = Forall([1], TFun(TTup [seqTyp; numberTyp], retTyp))
            Extern(CSigma(typ))
        in typ
    let skip =
        "skip",
        let typ =
            let seqTyp = TApp(KnownTypeNames.seq, [TGenVar 1])
            let retTyp = TGenVar 1
            let typ = Forall([1], TFun(TTup [seqTyp; numberTyp], retTyp))
            Extern(CSigma(typ))
        in typ
    let demoContext =
        Compiler.contextArgName,
        let typ =
            let r = TRecord [
                "BlockEdges", TApp(KnownTypeNames.seq, [ TApp("BlockEdge", []) ])
            ]
            Extern(CTau(r))
        in typ

    let smallEnv = Map.ofList [ add; read ]
    let fullEnv = Map.ofList [ add; read; map; take; skip; demoContext ]

let showUntypedAst exp =
    do Visu.showUntypedAst exp
    exp
let showAnnotatedAst env exp = 
    do AnnotatedAst.create env exp |> Visu.showAnnotatedAst
    exp
let showConstraintGraph env exp =
    do AnnotatedAst.create env exp |> ConstraintGraph.create |> Visu.showConstraintGraph
    exp






(*
    let x = 10.0
    let y = read ()
    add x y
*)

(Let "x" (Num 10.0)
(Let "y" (App (Var "read") Unit)
(Appt (Var "add") [ Var "x"; Var "y" ])))
|> showUntypedAst
|> showAnnotatedAst EnvCfg.smallEnv
|> showConstraintGraph EnvCfg.smallEnv
|> Compiler.compileAndRun (CoreLib.Unit.Instance, CoreLib.ViewCache()) []



(*
    let x = 10.0
    map c.BlockEdges (\be ->
        add be.Id x)
*)

(Let "x" (Num 10.0)
(Appt (Var "map") [ Prop (Var "context") "BlockEdges"; Abs "be"
    (Appt (Var "add") [ Prop (Var "be") "Id"; Var "x" ] )] ))
//|> showUntypedAst
//|> showAnnotatedAst EnvCfg.fullEnv
//|> showConstraintGraph EnvCfg.fullEnv
|> Compiler.compileAndRun (TestData.Context(), CoreLib.ViewCache()) []



(*
    let x = 10.0
    take(
        map c.BlockEdges (be =>
            let id = be.Id
            new
            {
                oldId = id
                newId = add id x
            }),
        2)
*)

(Let "x" (Num 4.0)
(Appt (Var "take") [
    (Appt (Var "map") [ Prop (Var "context") "BlockEdges"; Abs "be"
        (Let "id" (Prop (Var "be") "Id")
        (Record
            [
                "oldId", (Var "id")
                "newId", Appt (Var "add") [ Var "id"; Var "x" ]
            ]))])
    Num 2.0]))
//|> showUntypedAst
//|> showAnnotatedAst EnvCfg.fullEnv
//|> showConstraintGraph EnvCfg.fullEnv
|> (Compiler.compileAndRun (TestData.Context(), CoreLib.ViewCache()) [] >> printSeq)



let viewCache = CoreLib.ViewCache()
(*
    view("superBlockEdges", c.RandomNumbers)
*)

// WITHOUT ViewCache
Prop (Var "context") "RandomNumbers"
|> (Compiler.compileAndRun (TestData.Context(), viewCache) [] >> printSeq)

// WITH ViewCache
View "superBlockEdges" (Prop (Var "context") "RandomNumbers")
|> (Compiler.compileAndRun (TestData.Context(), viewCache) [] >> printSeq)

