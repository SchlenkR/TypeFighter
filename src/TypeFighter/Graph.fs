/// Dataflow graph of a solved expression: every block the source names becomes
/// a node, every argument an edge. Read off the numbered AST -- never off text --
/// so what the picture shows and what the compiler sees cannot drift apart.
///
/// The graph is a DAG, not a tree: a `let` bound once and used twice is ONE node
/// with two outgoing edges. That sharing is the whole point of drawing it.
module PureState.TypeFighter.Graph

open PureState.TypeFighter

/// What a block is. The drawing picks its shape from this, so the set is small
/// and every member means something visually different.
module NodeKind =
    /// a constant written in the source
    let literal = "literal"
    /// a free name that carries a value of its own (`count`, `now`)
    let source = "source"
    /// `expr.field` -- a host value read is this
    let field = "field"
    /// an `op_*` application: the arithmetic and comparisons
    let operator = "operator"
    /// any other named function applied to arguments
    let call = "call"
    /// a lambda's parameter, standing at the top of its group
    let param = "param"
    /// a choice between two ways - `if c then a else b`
    let decision = "decision"
    /// an application whose head is not a plain name
    let apply = "apply"
    let array = "array"
    let record = "record"
    let matchOn = "match"

type GraphNode =
    {
        id: string
        kind: string
        /// what is written across the block
        label: string
        /// the small line under it: the operator's word, the namespace, the raw name
        detail: string
        /// inferred type, empty when the solver had nothing to say
        typ: string
        /// the name a `let` put on what this block hands out, empty when anonymous
        binds: string
        /// the lambda this block stands inside, empty at the top level
        group: string
        /// source range in the user's coordinates, -1 when the node has none
        startIdx: int
        endIdx: int
    }

type GraphEdge =
    {
        fromId: string
        toId: string
        /// which argument of the target this fills: "1", "2", or a field name
        port: string
        typ: string
    }

/// A lambda, drawn as an area around the blocks that belong to it.
type GraphGroup =
    {
        id: string
        label: string
        /// the group this one sits inside, empty at the top level
        parent: string
    }

type ExprGraph =
    {
        nodes: GraphNode array
        edges: GraphEdge array
        groups: GraphGroup array
        /// the block whose value the whole expression is
        rootId: string
    }

let empty = { nodes = [||]; edges = [||]; groups = [||]; rootId = "" }

/// The words the operator bindings are drawn with. `op_add` says nothing on a
/// block; `+` says everything.
let private operatorSymbols =
    Map.ofList [
        "op_add", "+"
        "op_sub", "-"
        "op_mul", "*"
        "op_div", "/"
        "op_lt", "<"
        "op_gt", ">"
        "op_le", "<="
        "op_ge", ">="
        "op_eq", "=="
        "op_neq", "!="
        "op_and", "&&"
        "op_or", "||"
        "op_not", "!"
        "op_neg", "-"
    ]

/// Calls whose arguments mean something a number cannot say. `cond` IS the
/// `if c then a else b` of the surface syntax, so its wires are named the way the
/// source names them - and it is drawn as a choice, not as a call.
let private namedPorts =
    Map.ofList [
        "cond", [ "if"; "then"; "else" ]
    ]

let private portOf (head: string) (index: int) =
    match Map.tryFind head namedPorts with
    | Some names when index < names.Length -> names.[index]
    | _ -> string (index + 1)

/// Namespace members reach the solver flattened (`math.ema` -> `__math_ema`).
/// The picture shows what was written, so the flattening is undone here.
let private unflatten (name: string) =
    if not (name.StartsWith "__") then name
    else
        let rest = name.Substring 2
        match rest.IndexOf '_' with
        | -1 -> name
        | at -> rest.Substring(0, at) + "." + rest.Substring(at + 1)

/// Names the desugar pass invented for hoisted stream arguments. They are real
/// bindings in the tree but nothing the user wrote, so no wire carries them.
let private isSynthetic (name: string) = name.StartsWith "__tfv"

/// The desugar pass lifts a plain value used where a stream is wanted into a
/// constant-state machine - the monadic `return`. That wrapper is plumbing the
/// user never wrote, so the picture shows the value inside it instead.
///
/// Matched on its exact shape, not on "has a __machine field": a hand-written
/// step record carries real state, and this one is recognisable by having none -
/// UnitValue in and out, around a step function with a name the desugar invented.
let private unlifted (e: Expr<VarNum>) : Expr<VarNum> option =
    let isUnit ex =
        match ex with
        | Expr.Var v -> v.ident = BuiltinValues.unitValueIdent
        | _ -> false

    match e with
    | Expr.MkRecord outer when outer.items.Length = 1 ->
        match
            StepRecord.machineFieldOf BuiltinTypes.StepFields.init outer.items,
            StepRecord.machineFieldOf BuiltinTypes.StepFields.step outer.items
        with
        | Some init, Some (Expr.Fun stepFn) when isUnit init && isSynthetic stepFn.ident.identName ->
            match stepFn.body with
            | Expr.MkRecord result when result.items.Length = 2 ->
                match
                    StepRecord.fieldOf BuiltinTypes.StepFields.value result.items,
                    StepRecord.fieldOf BuiltinTypes.StepFields.nextState result.items
                with
                | Some wrapped, Some next when isUnit next -> Some wrapped
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let private literalText (value: Literal) =
    match value with
    | Literal.Number n ->
        if System.Double.IsInteger n then sprintf "%d" (int64 n) else string n
    // A block is one line. What a string does with newlines and runs of spaces is
    // its own business; on the picture it reads as one line or not at all.
    | Literal.String s ->
        let flat = System.Text.RegularExpressions.Regex.Replace(s, @"\s+", " ")
        sprintf "\"%s\"" flat
    | Literal.Boolean b -> if b then "true" else "false"

type private Builder(typeOf: VarNum -> string option, rangeOf: VarNum -> (int * int) option) =
    let nodes = ResizeArray<GraphNode>()
    let edges = ResizeArray<GraphEdge>()
    let groups = ResizeArray<GraphGroup>()
    let byId = System.Collections.Generic.Dictionary<string, int>()
    let mutable groupSeq = 0

    let idOf (VarNum n) = sprintf "n%d" n

    member _.Nodes = nodes
    member _.Edges = edges
    member _.Groups = groups

    member _.TypeOf(tvar) = typeOf tvar |> Option.defaultValue ""

    member _.AddGroup(label: string, parent: string) =
        let id = sprintf "g%d" groupSeq
        groupSeq <- groupSeq + 1
        groups.Add { id = id; label = label; parent = parent }
        id

    member this.Add(tvar: VarNum, kind: string, label: string, detail: string, group: string) =
        let id = idOf tvar
        let startIdx, endIdx =
            match rangeOf tvar with
            | Some (s, e) -> s, e
            | None -> -1, -1
        let node =
            {
                id = id
                kind = kind
                label = label
                detail = detail
                typ = this.TypeOf tvar
                binds = ""
                group = group
                startIdx = startIdx
                endIdx = endIdx
            }
        // A tvar can be reached twice (a Var resolving to an already-drawn node);
        // the first drawing wins so the picture stays stable.
        match byId.TryGetValue id with
        | true, _ -> id
        | _ ->
            byId.[id] <- nodes.Count
            nodes.Add node
            id

    member _.Connect(fromId: string, toId: string, port: string, typ: string) =
        if fromId <> "" && toId <> "" then
            edges.Add { fromId = fromId; toId = toId; port = port; typ = typ }

    /// Hangs a `let` name on the block that produces the value. Several names on
    /// one block is possible (`let a = 5; let b = a`) - all of them are shown.
    member _.Bind(id: string, name: string) =
        match byId.TryGetValue id with
        | true, at ->
            let node = nodes.[at]
            let binds =
                if node.binds = "" then name
                elif node.binds.Split(", ") |> Array.contains name then node.binds
                else node.binds + ", " + name
            nodes.[at] <- { node with binds = binds }
        | _ -> ()

/// Builds the graph of a solved expression.
///
/// `skipLets` drops that many leading bindings of the statement chain - a host
/// prepends its stdlib there, and none of it is the user's picture.
let build
    (skipLets: int)
    (typeOf: VarNum -> string option)
    (rangeOf: VarNum -> (int * int) option)
    (expr: Expr<VarNum>)
    : ExprGraph
    =
    let builder = Builder(typeOf, rangeOf)

    let rec skip n (e: Expr<VarNum>) =
        if n <= 0 then e
        else
            match e with
            | Expr.Let l -> skip (n - 1) l.body
            | _ -> e

    /// `env` maps a bound name to the block that produces its value, which is how
    /// a name used twice becomes one block with two wires instead of two blocks.
    let rec emit (env: Map<string, string>) (group: string) (e: Expr<VarNum>) : string =
        match unlifted e with
        | Some inner -> emit env group inner
        | None ->

        match e with
        | Expr.Lit x ->
            builder.Add(x.tvar, NodeKind.literal, literalText x.value, "", group)

        | Expr.Var x ->
            match Map.tryFind x.ident env with
            | Some id -> id
            | None ->
                let shown = unflatten x.ident
                builder.Add(x.tvar, NodeKind.source, shown, "", group)

        | Expr.App _ ->
            let head, args = StepRecord.collectAppChain e
            let headName =
                match head with
                | Expr.Var v when Map.containsKey v.ident env |> not -> v.ident
                | _ -> ""

            let id =
                match head with
                | Expr.Var v when Map.containsKey v.ident env |> not ->
                    match Map.tryFind v.ident operatorSymbols with
                    | Some symbol ->
                        builder.Add(e.TVar, NodeKind.operator, symbol, v.ident.Substring 3, group)
                    | None when Map.containsKey v.ident namedPorts ->
                        builder.Add(e.TVar, NodeKind.decision, "if", "", group)
                    | None ->
                        let shown = unflatten v.ident
                        let short =
                            match shown.LastIndexOf '.' with
                            | -1 -> shown
                            | at -> shown.Substring(at + 1)
                        let detail = if short = shown then "" else shown.Substring(0, shown.Length - short.Length - 1)
                        builder.Add(e.TVar, NodeKind.call, short, detail, group)

                // A field read applied to arguments, and anything else in head
                // position: the head is a value like any other and gets a wire in.
                | _ ->
                    let id = builder.Add(e.TVar, NodeKind.apply, "apply", "", group)
                    let headId = emit env group head
                    builder.Connect(headId, id, "fn", builder.TypeOf head.TVar)
                    id

            args
            |> List.iteri (fun index arg ->
                let argId = emit env group arg
                builder.Connect(argId, id, portOf headName index, builder.TypeOf arg.TVar))

            id

        | Expr.Fun x ->
            let inner = builder.AddGroup(x.ident.identName + " =>", group)
            let paramId = builder.Add(x.ident.tvar, NodeKind.param, x.ident.identName, "", inner)
            emit (Map.add x.ident.identName paramId env) inner x.body

        | Expr.Let x ->
            let valueId = emit env group x.value
            if not (isSynthetic x.ident.identName) then
                builder.Bind(valueId, x.ident.identName)
            emit (Map.add x.ident.identName valueId env) group x.body

        | Expr.Do x ->
            emit env group x.action |> ignore
            emit env group x.body

        | Expr.PropAcc x ->
            // `values.cpu` is one block, not a 3000-field record feeding a
            // reader. Only a field read off something computed gets its own wire.
            match x.source with
            | Expr.Var v when Map.containsKey v.ident env |> not ->
                builder.Add(
                    e.TVar,
                    NodeKind.field,
                    v.ident + "." + x.ident.identName,
                    "",
                    group)
            | _ ->
                let sourceId = emit env group x.source
                let id = builder.Add(e.TVar, NodeKind.field, "." + x.ident.identName, "", group)
                builder.Connect(sourceId, id, "1", builder.TypeOf x.source.TVar)
                id

        | Expr.MkArray x ->
            let id = builder.Add(e.TVar, NodeKind.array, "[ ]", sprintf "%d" x.values.Length, group)
            x.values
            |> List.iteri (fun index value ->
                let valueId = emit env group value
                builder.Connect(valueId, id, string (index + 1), builder.TypeOf value.TVar))
            id

        | Expr.MkRecord x ->
            let id = builder.Add(e.TVar, NodeKind.record, "{ }", sprintf "%d" x.items.Length, group)
            x.items
            |> List.iteri (fun index item ->
                let value = item.Value
                let valueId = emit env group value
                let port =
                    match item with
                    | RecordItem.Property f -> f.fname
                    | RecordItem.Positional _ -> string (index + 1)
                builder.Connect(valueId, id, port, builder.TypeOf value.TVar))
            id

        | Expr.Match x ->
            let id = builder.Add(e.TVar, NodeKind.matchOn, "match", sprintf "%d" x.arms.Length, group)
            let scrutineeId = emit env group x.scrutinee
            builder.Connect(scrutineeId, id, "on", builder.TypeOf x.scrutinee.TVar)
            x.arms
            |> List.iteri (fun index arm ->
                let armEnv =
                    match arm.pattern with
                    | MatchPattern.Var ident ->
                        let patternId = builder.Add(ident.tvar, NodeKind.param, ident.identName, "", group)
                        Map.add ident.identName patternId env
                    | _ -> env
                let bodyId = emit armEnv group arm.body
                builder.Connect(bodyId, id, sprintf "arm %d" (index + 1), builder.TypeOf arm.body.TVar))
            id

    let rootId = emit Map.empty "" (skip skipLets expr)

    {
        nodes = builder.Nodes |> Array.ofSeq
        edges = builder.Edges |> Array.ofSeq
        groups = builder.Groups |> Array.ofSeq
        rootId = rootId
    }
