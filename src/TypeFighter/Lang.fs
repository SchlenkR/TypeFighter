// Copyright (c) Ronald Schlenker
// Evaluation use only. Commercial use prohibited.

namespace PureState.TypeFighter

open TheBlunt

type VarNum = VarNum of int
    with override this.ToString() = let (VarNum v) = this in $"tv_{v}"

module NumGen =
    let mkGenerator() =
        let mutable currVar = -1
        fun () ->
            currVar <- currVar + 1
            VarNum currVar

type Literal =
    | Number of float
    | String of string
    | Boolean of bool

// A constraint-shaped type; some variants (e.g. TVar) only appear
// mid-inference and never in a final solution.
type MonoTyp =
    | TVar of VarNum
    | SaturatedTyp of {| name: string; args: MonoTyp list |}
    | FunTyp of MonoTyp * MonoTyp
    | RecordTyp of RecordDefinition
    | LiteralTyp of Literal
    | UnionTyp of Set<MonoTyp>
    override this.ToString() = ShowTyp.Show(this)

and PolyTyp =
    {
        vars: Set<VarNum>
        // Per-tvar membership constraints (`T ∈ {Number, String}`). Each entry
        // limits the named tvar to the listed monotypes; empty map = unconstrained.
        // Re-emitted as CIsOneOf when the polytype is instantiated.
        constraints: Map<VarNum, Set<MonoTyp>>
        monoTyp: MonoTyp
    }
    override this.ToString() = ShowTyp.Show(this)

and Typ =
    | Mono of MonoTyp
    | Poly of PolyTyp
    override this.ToString() = ShowTyp.Show(this)

and RecordDefinition =
    {
        fields: Set<FieldDefinition>
        // Bag-semantics at the type level; order only kept for pretty-
        // printing. See docs/design/RecordsAsHeterogeneousSets.md §4.
        positionals: MonoTyp list
    }
    override this.ToString() = ShowTyp.Show(this)

and FieldDefinition =
    {
        fname: string
        typ: MonoTyp
    }
    override this.ToString() = ShowTyp.Show(this)

and ShowTyp =
    static let printers = ResizeArray()
    static let printOrShow (typ: MonoTyp) defaultShow =
        match
            printers
            |> Seq.map (fun p -> p typ)
            |> Seq.choose id
            |> Seq.tryHead
        with
        | Some printed -> printed
        | None -> defaultShow ()
    static member AddPrinter printer = printers.Add printer
    static member Show (field: FieldDefinition) =
        $"{field.fname}: {field.typ}"
    static member Show (fields: Set<FieldDefinition>, positionals: MonoTyp list) =
        let namedParts =
            fields
            |> Set.map ShowTyp.Show
        let positionalParts =
            positionals
            |> List.map ShowTyp.Show
        let allParts =
            [
                yield! positionalParts
                yield! namedParts
            ]
            |> String.concat " & "
        $"{{ {allParts} }}"
    static member Show (record: RecordDefinition) =
        ShowTyp.Show(record.fields, record.positionals)
    static member Show (typ: MonoTyp) =
        printOrShow typ (fun () ->
            match typ with
            | TVar x -> x.ToString()
            | SaturatedTyp x ->
                match x.args with
                | [] -> x.name
                | args ->
                    let printedArgs = [ for a in args -> ShowTyp.Show a ] |> String.concat ", "
                    $"{x.name}<{printedArgs}>"
            | FunTyp (t1, t2) -> $"({t1} -> {t2})"
            | RecordTyp record -> ShowTyp.Show record
            | LiteralTyp lit ->
                match lit with
                | Number n -> n.ToString()
                | String s -> $"\"{s}\""
                | Boolean b -> b.ToString()
            | UnionTyp members ->
                members
                |> Set.toList
                |> List.map ShowTyp.Show
                |> String.concat " | "
        )
    
    static member Show (typ: PolyTyp) =
        let printedVars = [ for v in typ.vars -> v.ToString() ] |> String.concat ", "
        let printedConstraints =
            if Map.isEmpty typ.constraints then ""
            else
                let parts =
                    typ.constraints
                    |> Map.toList
                    |> List.map (fun (v, allowed) ->
                        let allowedStr =
                            allowed
                            |> Set.toList
                            |> List.map ShowTyp.Show
                            |> String.concat " | "
                        $"{v} in ({allowedStr})")
                    |> String.concat ", "
                $" where {parts}"
        $"<{printedVars}>{printedConstraints}.{ShowTyp.Show typ.monoTyp}"
    
    static member Show (typ: Typ) =
        match typ with
        | Mono typ -> ShowTyp.Show typ
        | Poly typ -> ShowTyp.Show typ

type Env = Map<string, EnvItem>
and [<RequireQualifiedAccess>] EnvItem =
    | Internal of VarNum 
    | External of Typ

[<RequireQualifiedAccess>]
type Expr<'noneOrVarnum> =
    // many exprs are non-elementary, but we don't care about that here,
    // since we aim for expressiveness when working with the AST
    internal
    | Lit of {| value: Literal; tvar: 'noneOrVarnum |}                                                                             // "foo" oder 2323
    | Var of {| ident: string; tvar: 'noneOrVarnum |}                                                                             // ident
    | App of {| func: Expr<'noneOrVarnum>; arg: Expr<'noneOrVarnum>; tvar: 'noneOrVarnum |}                                       // func arg
    | Fun of {| ident: Ident<'noneOrVarnum>; body: Expr<'noneOrVarnum>; tvar: 'noneOrVarnum |}                                    // fun ident -> body
    | Let of {| ident: Ident<'noneOrVarnum>; value: Expr<'noneOrVarnum>; body: Expr<'noneOrVarnum>; tvar: 'noneOrVarnum |}        // let ident = value in body
    | Do of {| action: Expr<'noneOrVarnum>; body: Expr<'noneOrVarnum>; tvar: 'noneOrVarnum |}                                     // do value body
    | PropAcc of {| source: Expr<'noneOrVarnum>; ident: Ident<'noneOrVarnum>; tvar: 'noneOrVarnum |}
    | MkArray of {| values: Expr<'noneOrVarnum> list; tvar: 'noneOrVarnum |}
    | MkRecord of {| items: RecordItem<'noneOrVarnum> list; tvar: 'noneOrVarnum |}
    | Match of {| scrutinee: Expr<'noneOrVarnum>; arms: MatchArm<'noneOrVarnum> list; tvar: 'noneOrVarnum |}

    member this.TVar =
        match this with
        | Lit x -> x.tvar
        | Var x -> x.tvar
        | App x -> x.tvar
        | Fun x -> x.tvar
        | Let x -> x.tvar
        | Do x -> x.tvar
        | PropAcc x -> x.tvar
        | MkArray x -> x.tvar
        | MkRecord x -> x.tvar
        | Match x -> x.tvar
    override this.ToString() =
        ShowExpr.Expr(this)

and Ident<'noneOrVarnum> = internal { identName: string; tvar: 'noneOrVarnum }

// One entry of a record-set: either named (Property) or positional.
// See docs/design/RecordsAsHeterogeneousSets.md.
and [<RequireQualifiedAccess>] RecordItem<'noneOrVarnum> =
    internal
    | Property of {| fname: string; value: Expr<'noneOrVarnum> |}
    | Positional of Expr<'noneOrVarnum>

    member this.Value =
        match this with
        | Property x -> x.value
        | Positional v -> v

    member this.TryAsPositional =
        match this with
        | Positional v -> Some v
        | _ -> None

// Pattern + body arm for `match`. Literal / Var / Wildcard patterns
// today; record destructure is a follow-up.
and MatchArm<'noneOrVarnum> =
    internal { pattern: MatchPattern<'noneOrVarnum>; body: Expr<'noneOrVarnum> }

and [<RequireQualifiedAccess>] MatchPattern<'noneOrVarnum> =
    internal
    | Literal of {| value: Literal; tvar: 'noneOrVarnum |}
    | Var of Ident<'noneOrVarnum>
    | Wildcard of {| tvar: 'noneOrVarnum |}

    member this.TVar =
        match this with
        | Literal x -> x.tvar
        | Var x -> x.tvar
        | Wildcard x -> x.tvar

and ShowExpr =
    static member Expr (expr: Expr<'noneOrVarnum>) =
        let printIdent (ident: Ident<'noneOrVarnum>) = ident.identName
        let printItem (item: RecordItem<'noneOrVarnum>) =
            match item with
            | RecordItem.Property f -> $"{f.fname}: {f.value}"
            | RecordItem.Positional v -> ShowExpr.Expr v
        match expr with
        | Expr.Lit x -> $"LIT := {x.value}"
        | Expr.Var x -> $"VAR := {x.ident}"
        | Expr.App x -> $"APP := ({x.func}) ({x.arg})"
        | Expr.Fun x -> $"FUN := ({printIdent x.ident}) => ... {x.body.TVar}"
        | Expr.Let x -> $"LET := {printIdent x.ident} = {x.value}) in {x.body}"
        | Expr.Do x -> $"DO := {x.action} {x.body}"
        | Expr.PropAcc x -> $"PropAcc {x.source}.{x.ident}"
        | Expr.MkArray x ->
            let values = [ for x in x.values -> ShowExpr.Expr x ] |> String.concat "; "
            $"MkARRAY := [ {values} ]"
        | Expr.MkRecord x ->
            let parts =
                x.items
                |> List.map printItem
                |> String.concat ", "
            $"{{ {parts} }}"
        | Expr.Match x ->
            let printPat (p: MatchPattern<_>) =
                match p with
                | MatchPattern.Literal l -> $"{l.value}"
                | MatchPattern.Var v -> v.identName
                | MatchPattern.Wildcard _ -> "_"
            let arms =
                x.arms
                |> List.map (fun a -> $"| {printPat a.pattern} -> {ShowExpr.Expr a.body}")
                |> String.concat " "
            $"MATCH := {ShowExpr.Expr x.scrutinee} with {arms}"

// TODO: Replace this (and throwing) with a "TError"
type UnificationError(source: Expr<VarNum>, t1: MonoTyp, t2: MonoTyp, reason: string option) =
    inherit System.Exception(
        [
            $"Can't unify"
            $"    {t1}"
            $"  and"
            $"    {t2}"
            $"""Reason: {match reason with None -> "UNKNOWN" | Some message -> message}"""
            $"Source Expression: {source}"
            $"Source TVar: {source.TVar}"
            ""
        ]
        |> String.concat "\n"
    )
    member _.T1 = t1
    member _.T2 = t2
    member _.Reason = reason
    member _.SourceTVar : VarNum = source.TVar

module Expr =

    let toNumberedExpr (expr: Expr<unit>) newVar : Expr<VarNum> =
        let numberIdent (ident: Ident<unit>) : Ident<VarNum> =
            {
                identName = ident.identName
                tvar = newVar ()
            }

        let rec loop (expr: Expr<Unit>) =
            match expr with
            | Expr.Lit x -> Expr.Lit {| value = x.value; tvar = newVar () |}
            | Expr.Var x -> Expr.Var {| ident = x.ident; tvar = newVar () |} 
            | Expr.App x -> Expr.App {| func = loop x.func; arg = loop x.arg; tvar = newVar () |}
            | Expr.Fun x -> Expr.Fun {| ident = numberIdent x.ident; body = loop x.body; tvar = newVar () |}
            | Expr.Let x -> Expr.Let {| ident = numberIdent x.ident; value = loop x.value; body = loop x.body; tvar = newVar () |}
            | Expr.Do x -> Expr.Do {| action = loop x.action; body = loop x.body; tvar = newVar () |}
            | Expr.PropAcc x ->
                Expr.PropAcc 
                    {| 
                        source = loop x.source
                        ident = { identName = x.ident.identName; tvar = newVar () }
                        tvar = newVar ()
                    |}
            | Expr.MkArray x -> 
                Expr.MkArray 
                    {| 
                        values = [ for v in x.values -> loop v ]
                        tvar = newVar ()
                    |}
            | Expr.MkRecord x ->
                Expr.MkRecord
                    {|
                        items = [
                            for item in x.items do
                                match item with
                                | RecordItem.Property f ->
                                    RecordItem.Property {| fname = f.fname; value = loop f.value |}
                                | RecordItem.Positional v ->
                                    RecordItem.Positional (loop v)
                            ]
                        tvar = newVar ()
                    |}
            | Expr.Match x ->
                let numberPattern (p: MatchPattern<unit>) : MatchPattern<VarNum> =
                    match p with
                    | MatchPattern.Literal l ->
                        MatchPattern.Literal {| value = l.value; tvar = newVar () |}
                    | MatchPattern.Var v ->
                        MatchPattern.Var (numberIdent v)
                    | MatchPattern.Wildcard _ ->
                        MatchPattern.Wildcard {| tvar = newVar () |}
                Expr.Match
                    {|
                        scrutinee = loop x.scrutinee
                        arms =
                            [ for a in x.arms ->
                                { pattern = numberPattern a.pattern; body = loop a.body } ]
                        tvar = newVar ()
                    |}
        loop expr

    let collectTVars (expr: Expr<VarNum>) =
        let rec loop (expr: Expr<VarNum>) (acc: VarNum list) =
            match expr with
            | Expr.Lit x -> x.tvar :: acc
            | Expr.Var x -> x.tvar :: acc
            | Expr.App x -> 
                [ 
                    yield x.tvar
                    yield! loop x.func acc
                    yield! loop x.arg acc
                ]
            | Expr.Fun x ->
                [
                    yield x.tvar
                    yield! loop x.body acc
                ]
            | Expr.Let x ->
                [
                    yield x.tvar
                    yield! loop x.value acc
                    yield! loop x.body acc
                ]
            | Expr.Do x ->
                [
                    yield x.tvar
                    yield! loop x.action acc
                    yield! loop x.body acc
                ]
            | Expr.PropAcc x ->
                [
                    yield x.tvar
                    yield! loop x.source acc
                ]
            | Expr.MkArray x ->
                [
                    yield x.tvar
                    for v in x.values do
                        yield! loop v acc
                ]
            | Expr.MkRecord x ->
                [
                    yield x.tvar
                    for item in x.items do
                        yield! loop item.Value acc
                ]
            | Expr.Match x ->
                [
                    yield x.tvar
                    yield! loop x.scrutinee acc
                    for a in x.arms do
                        yield a.pattern.TVar
                        match a.pattern with
                        | MatchPattern.Var ident -> yield ident.tvar
                        | _ -> ()
                        yield! loop a.body acc
                ]

        loop expr []
        |> List.map (fun (VarNum v) -> v)
        |> List.distinct

    let maxVar (expr: Expr<VarNum>) =
        0 :: collectTVars expr |> List.max

module Typ =
    let collectTVars (typ: Typ) =
        let rec loop (typ: Typ) (acc: VarNum list) =
            match typ with
            | Mono typ -> loopMono typ acc
            | Poly typ -> loopPoly typ acc
        and loopRecord (record: RecordDefinition) (acc: VarNum list) =
            [
                for f in record.fields do
                    yield! loopMono f.typ acc
                for p in record.positionals do
                    yield! loopMono p acc
            ]
        and loopMono (typ: MonoTyp) (acc: VarNum list) =
            match typ with
            | TVar x -> x :: acc
            | SaturatedTyp x ->
                [
                    for arg in x.args do
                        yield! loopMono arg acc
                ]
            | FunTyp (t1, t2) ->
                [
                    yield! loopMono t1 acc
                    yield! loopMono t2 acc
                ]
            | RecordTyp record ->
                loopRecord record acc
            | LiteralTyp _ -> acc
            | UnionTyp members ->
                [
                    for m in members do
                        yield! loopMono m acc
                ]
        and loopPoly (typ: PolyTyp) (acc: VarNum list) =
            [
                yield! typ.vars
                yield! loopMono typ.monoTyp acc
            ]

        loop typ []
        |> List.map (fun (VarNum v) -> v)
        |> List.distinct

    let maxVar (typ: Typ) =
        0 :: collectTVars typ |> List.max

    // Canonically renumbers bound TVars (0, 1, ...) so alpha-equivalent
    // polytypes compare equal under F# structural equality.
    let gen (typ: MonoTyp) =
        let tvars = collectTVars (Mono typ) |> List.map VarNum
        match tvars with
        | [] -> Mono typ
        | tvars ->
            let renaming =
                tvars
                |> List.mapi (fun i v -> v, VarNum i)
                |> Map.ofList
            let rec renameMono t =
                match t with
                | TVar v -> TVar (Map.find v renaming)
                | SaturatedTyp x ->
                    SaturatedTyp {| name = x.name; args = x.args |> List.map renameMono |}
                | FunTyp (a, b) -> FunTyp (renameMono a, renameMono b)
                | RecordTyp r -> RecordTyp (renameRecord r)
                | LiteralTyp l -> LiteralTyp l
                | UnionTyp ms -> UnionTyp (ms |> Set.map renameMono)
            and renameRecord (r: RecordDefinition) : RecordDefinition =
                { r with
                    fields =
                        r.fields
                        |> Set.map (fun f -> { f with typ = renameMono f.typ })
                    positionals =
                        r.positionals
                        |> List.map renameMono
                }
            let canonicalVars = renaming |> Map.toSeq |> Seq.map snd |> Set.ofSeq
            Poly { vars = canonicalVars; constraints = Map.empty; monoTyp = renameMono typ }

module Env =
    let collectTVars (env: Env) =
        [
            for x in env.Values do
                match x with
                | EnvItem.Internal (VarNum tvar) -> yield tvar
                | EnvItem.External typ -> yield! Typ.collectTVars typ
        ]
        |> List.distinct
    let maxVar (env: Env) =
        0 :: collectTVars env |> List.max

[<AutoOpen>]
module TDefAutoOps =
    let ( ~% ) x = TVar (VarNum x)

    // CAREFUL HERE: -> is right-associative
    let ( ^-> ) t1 t2 = FunTyp (t1, t2)

[<RequireQualifiedAccess>]
module TDef =

    // TODO: no capitals

    let Poly (vars: int list) (monoTyp: MonoTyp) =
        let vars = vars |> List.map VarNum |> set
        Poly { vars = vars; constraints = Map.empty; monoTyp = monoTyp }

    /// Generalize with per-tvar membership constraints. Each `(varIdx, allowed)`
    /// pair limits `tv_varIdx` to one of the listed monotypes. The constraints
    /// flow through `inst`/solve so polymorphic operators (e.g. `op_add` over
    /// Number|String) work without ad-hoc dispatch.
    let PolyWithConstraints
        (vars: int list)
        (constraints: (int * MonoTyp list) list)
        (monoTyp: MonoTyp)
        =
        let varSet = vars |> List.map VarNum |> set
        let cmap =
            constraints
            |> List.map (fun (v, allowed) -> VarNum v, Set.ofList allowed)
            |> Map.ofList
        Typ.Poly { vars = varSet; constraints = cmap; monoTyp = monoTyp }

    let GeneralizeWithConstraints
        (constraints: (int * MonoTyp list) list)
        (monoTyp: MonoTyp)
        =
        let tvars = Typ.collectTVars (Mono monoTyp)
        PolyWithConstraints tvars constraints monoTyp
    
    let RecordDefWith (fields: (string * MonoTyp) list) =
        let fields =
            [ for (fname, typ) in fields do { fname = fname; typ = typ } ]
            |> set
        { fields = fields; positionals = [] }

    let RecordDefWithItems (fields: (string * MonoTyp) list) (positionals: MonoTyp list) =
        let fields =
            [ for (fname, typ) in fields do { fname = fname; typ = typ } ]
            |> set
        { fields = fields; positionals = positionals }

    let RecordWithItems (fields: (string * MonoTyp) list) (positionals: MonoTyp list) =
        RecordTyp (RecordDefWithItems fields positionals)

    let RecordWith (fields: (string * MonoTyp) list) =
        RecordTyp (RecordDefWith fields)

    let SaturatedWith (name: string) (args: MonoTyp list) =
        SaturatedTyp {| name = name; args = args |}

    let Generalize (monoTyp: MonoTyp) =
        Typ.gen monoTyp

module BuiltinTypes =
    module Names =
        let [<Literal>] unit = "Unit"
        let [<Literal>] number = "Number"
        let [<Literal>] string = "String"
        let [<Literal>] date = "Date"
        let [<Literal>] array = "Array"
        let [<Literal>] bool = "Bool"

    let unit = TDef.SaturatedWith Names.unit []
    let boolean = UnionTyp (set [ LiteralTyp (Boolean true); LiteralTyp (Boolean false) ])
    let number = TDef.SaturatedWith Names.number []
    let string = TDef.SaturatedWith Names.string []

    let array elemTyp = TDef.SaturatedWith Names.array [ elemTyp ]

    // `__machine` is the ONE reserved field name in the language: carrying it
    // is what makes a record a state-machine step. The names inside it are
    // ordinary fields, interpreted only in this position, so they can never
    // collide with user code. Single source of truth for the type-side
    // helpers below, the solver's step-let detection and the emit pass.
    module StepFields =
        let [<Literal>] machine = "__machine"
        let [<Literal>] init = "init"
        let [<Literal>] step = "step"
        let [<Literal>] value = "val"
        let [<Literal>] nextState = "nextState"

    // Step-record shape: a value of this type is a state-machine step.
    // The emit pass recognises the marker structurally and compiles each
    // let-binding into a flat C# state slot: no nominal "Stream" type,
    // no implicit context, just records.
    //
    //   { __machine: { init: S; step: S -> { val: V, nextState: S } } }
    let stepResult v s =
        TDef.RecordWith [
            StepFields.value, v
            StepFields.nextState, s
        ]
    let stepMachine v s =
        TDef.RecordWith [
            StepFields.init, s
            StepFields.step, FunTyp (s, stepResult v s)
        ]
    let stepRecord v s =
        TDef.RecordWith [
            StepFields.machine, stepMachine v s
        ]

    // Fields of the inner machine record, if the marker is present.
    let private machineFieldsOf (typ: MonoTyp) =
        match typ with
        | RecordTyp r ->
            r.fields
            |> Seq.tryFind (fun f -> f.fname = StepFields.machine)
            |> Option.bind (fun f ->
                match f.typ with
                | RecordTyp m -> Some m.fields
                | _ -> None)
        | _ -> None

    // Structural test: is this MonoTyp a step-record? Carrying the marker is
    // the whole criterion -- its payload may still be an unsolved tvar.
    let isStepRecord (typ: MonoTyp) =
        match typ with
        | RecordTyp r -> r.fields |> Seq.exists (fun f -> f.fname = StepFields.machine)
        | _ -> false

    // Extract the V (val) component from a step-record's `step` return
    // type: used by the emit to type the result list.
    let stepRecordV (typ: MonoTyp) : MonoTyp option =
        machineFieldsOf typ
        |> Option.bind (fun fields ->
            fields
            |> Seq.tryFind (fun f -> f.fname = StepFields.step)
            |> Option.bind (fun f ->
                match f.typ with
                | FunTyp (_, RecordTyp resR) ->
                    resR.fields
                    |> Seq.tryFind (fun rf -> rf.fname = StepFields.value)
                    |> Option.map (fun rf -> rf.typ)
                | _ -> None))

    // Extract V and S from a step-record. Returns None if the shape doesn't
    // match: useful for tests and diagnostics.
    let stepRecordVS (typ: MonoTyp) : (MonoTyp * MonoTyp) option =
        machineFieldsOf typ
        |> Option.bind (fun fields ->
            let init = fields |> Seq.tryFind (fun f -> f.fname = StepFields.init)
            let step = fields |> Seq.tryFind (fun f -> f.fname = StepFields.step)
            match init, step with
            | Some i, Some s ->
                match s.typ with
                | FunTyp (_, RecordTyp resR) ->
                    resR.fields
                    |> Seq.tryFind (fun rf -> rf.fname = StepFields.value)
                    |> Option.map (fun rf -> rf.typ, i.typ)
                | _ -> None
            | _ -> None)

    ShowTyp.AddPrinter(fun typ ->
        match typ with
        | t when t = boolean -> Some Names.bool
        | _ -> None
    )

// Structural step-record helpers on the AST level. Shared by the solver's
// step-let detection, the desugar pass and the state-machine emit so all
// agree on what counts as a step. Stateless; generic over the tvar tag
// (the desugar pass runs on `Expr<unit>`, solver/emit on `Expr<VarNum>`).
module StepRecord =

    // Property field names present in a record literal's items.
    let propFieldNames (items: RecordItem<'a> list) : Set<string> =
        items
        |> List.choose (fun item ->
            match item with
            | RecordItem.Property f -> Some f.fname
            | _ -> None)
        |> Set.ofList

    // Does a record literal carry the `__machine` marker?
    let hasStepShape (items: RecordItem<'a> list) : bool =
        Set.contains BuiltinTypes.StepFields.machine (propFieldNames items)

    // Does a record literal carry the `{ val, nextState }` result shape a
    // `step` body terminates in?
    let hasResultShape (items: RecordItem<'a> list) : bool =
        let names = propFieldNames items
        Set.contains BuiltinTypes.StepFields.value names
        && Set.contains BuiltinTypes.StepFields.nextState names

    // Value expression of a named property in a record literal, if present.
    let fieldOf (name: string) (items: RecordItem<'a> list) : Expr<'a> option =
        items
        |> List.tryPick (fun item ->
            match item with
            | RecordItem.Property f when f.fname = name -> Some f.value
            | _ -> None)

    // Items of the inner `{ init, step }` literal behind the marker.
    let machineItemsOf (items: RecordItem<'a> list) : RecordItem<'a> list option =
        fieldOf BuiltinTypes.StepFields.machine items
        |> Option.bind (fun e ->
            match e with
            | Expr.MkRecord r -> Some r.items
            | _ -> None)

    // Value expression of a field inside the marker, e.g. `init` or `step`.
    let machineFieldOf (name: string) (items: RecordItem<'a> list) : Expr<'a> option =
        machineItemsOf items |> Option.bind (fieldOf name)

    // Decompose a curried application into (head, args) in left-to-right order:
    // `App(App(f, a), b)` -> `(f, [a; b])`.
    let collectAppChain (e: Expr<'a>) : Expr<'a> * Expr<'a> list =
        let rec loop e args =
            match e with
            | Expr.App app -> loop app.func (app.arg :: args)
            | _ -> e, args
        loop e []

    // Head identifier of a curried application chain, if it is a Var.
    let headIdentOf (e: Expr<'a>) : string option =
        match collectAppChain e |> fst with
        | Expr.Var v -> Some v.ident
        | _ -> None

    // Beta-substitution: replace every unbound occurrence of `name` in `e`
    // with `replacement`, respecting shadowing in inner Fun / Let / Match-Var
    // binders. Capture-avoidance relies on `replacement` having no free vars
    // that collide with inner binders; the emit only substitutes fresh slot
    // identifiers, so that holds at its call sites.
    let rec substVar (name: string) (replacement: Expr<'a>) (e: Expr<'a>) : Expr<'a> =
        let go = substVar name replacement
        match e with
        | Expr.Var v when v.ident = name -> replacement
        | Expr.Var _ | Expr.Lit _ -> e
        | Expr.App app ->
            Expr.App {| func = go app.func; arg = go app.arg; tvar = app.tvar |}
        | Expr.Fun f when f.ident.identName = name -> e
        | Expr.Fun f ->
            Expr.Fun {| ident = f.ident; body = go f.body; tvar = f.tvar |}
        | Expr.Let l when l.ident.identName = name ->
            Expr.Let {| ident = l.ident; value = go l.value; body = l.body; tvar = l.tvar |}
        | Expr.Let l ->
            Expr.Let
                {|
                    ident = l.ident
                    value = go l.value
                    body  = go l.body
                    tvar  = l.tvar
                |}
        | Expr.Do d ->
            Expr.Do {| action = go d.action; body = go d.body; tvar = d.tvar |}
        | Expr.PropAcc p ->
            Expr.PropAcc {| source = go p.source; ident = p.ident; tvar = p.tvar |}
        | Expr.MkArray a ->
            Expr.MkArray {| values = a.values |> List.map go; tvar = a.tvar |}
        | Expr.MkRecord r ->
            let mapItem item =
                match item with
                | RecordItem.Property f ->
                    RecordItem.Property {| fname = f.fname; value = go f.value |}
                | RecordItem.Positional v -> RecordItem.Positional (go v)
            Expr.MkRecord {| items = r.items |> List.map mapItem; tvar = r.tvar |}
        | Expr.Match m ->
            let mapArm arm =
                let body =
                    match arm.pattern with
                    | MatchPattern.Var v when v.identName = name -> arm.body
                    | _ -> go arm.body
                { pattern = arm.pattern; body = body }
            Expr.Match
                {|
                    scrutinee = go m.scrutinee
                    arms      = m.arms |> List.map mapArm
                    tvar      = m.tvar
                |}

    // Apply a curried lambda chain to its argument list. Stops when either
    // runs out, returning whatever's left of the body (handy for partial app
    // in nested cases, though for our use we expect full application).
    let applyLambda (lambda: Expr<'a>) (args: Expr<'a> list) : Expr<'a> =
        let rec collectParams e acc =
            match e with
            | Expr.Fun f -> collectParams f.body (f.ident.identName :: acc)
            | _ -> List.rev acc, e
        let parameters, body = collectParams lambda []
        let paired = List.zip (List.truncate args.Length parameters) args
        paired |> List.fold (fun acc (n, r) -> substVar n r acc) body

    // Every identifier that occurs in `e` -- binders and references alike.
    // Used to pick fresh synthetic names that cannot collide.
    let usedIdents (e: Expr<'a>) : Set<string> =
        let acc = System.Collections.Generic.HashSet<string>()
        let rec go e =
            match e with
            | Expr.Lit _ -> ()
            | Expr.Var v -> acc.Add v.ident |> ignore
            | Expr.App a -> go a.func; go a.arg
            | Expr.Fun f -> acc.Add f.ident.identName |> ignore; go f.body
            | Expr.Let l -> acc.Add l.ident.identName |> ignore; go l.value; go l.body
            | Expr.Do d -> go d.action; go d.body
            | Expr.PropAcc p -> go p.source
            | Expr.MkArray a -> for v in a.values do go v
            | Expr.MkRecord r -> for item in r.items do go item.Value
            | Expr.Match m ->
                go m.scrutinee
                for arm in m.arms do
                    match arm.pattern with
                    | MatchPattern.Var v -> acc.Add v.identName |> ignore
                    | _ -> ()
                    go arm.body
        go e
        Set.ofSeq acc

    // Alpha-rename every Fun / Let / Match-Var binder in `e` to a fresh name.
    // The emit beta-reduces stream-fn bodies with RAW user argument
    // expressions; without freshening, a user identifier colliding with an
    // inner binder (e.g. user `s` vs `step: s => ...`) would be captured.
    let freshenBinders (fresh: unit -> string) (e: Expr<'a>) : Expr<'a> =
        let rec go e =
            match e with
            | Expr.Lit _ | Expr.Var _ -> e
            | Expr.App a ->
                Expr.App {| func = go a.func; arg = go a.arg; tvar = a.tvar |}
            | Expr.Fun f ->
                let n = fresh ()
                let renamed =
                    substVar f.ident.identName
                        (Expr.Var {| ident = n; tvar = f.ident.tvar |}) f.body
                Expr.Fun {| ident = { identName = n; tvar = f.ident.tvar }; body = go renamed; tvar = f.tvar |}
            | Expr.Let l ->
                let n = fresh ()
                let renamed =
                    substVar l.ident.identName
                        (Expr.Var {| ident = n; tvar = l.ident.tvar |}) l.body
                Expr.Let {| ident = { identName = n; tvar = l.ident.tvar }; value = go l.value; body = go renamed; tvar = l.tvar |}
            | Expr.Do d ->
                Expr.Do {| action = go d.action; body = go d.body; tvar = d.tvar |}
            | Expr.PropAcc p ->
                Expr.PropAcc {| source = go p.source; ident = p.ident; tvar = p.tvar |}
            | Expr.MkArray a ->
                Expr.MkArray {| values = a.values |> List.map go; tvar = a.tvar |}
            | Expr.MkRecord r ->
                let mapItem item =
                    match item with
                    | RecordItem.Property f ->
                        RecordItem.Property {| fname = f.fname; value = go f.value |}
                    | RecordItem.Positional v -> RecordItem.Positional (go v)
                Expr.MkRecord {| items = r.items |> List.map mapItem; tvar = r.tvar |}
            | Expr.Match m ->
                let mapArm (arm: MatchArm<'a>) =
                    match arm.pattern with
                    | MatchPattern.Var v ->
                        let n = fresh ()
                        let renamed =
                            substVar v.identName
                                (Expr.Var {| ident = n; tvar = v.tvar |}) arm.body
                        { pattern = MatchPattern.Var { identName = n; tvar = v.tvar }; body = go renamed }
                    | _ -> { pattern = arm.pattern; body = go arm.body }
                Expr.Match
                    {|
                        scrutinee = go m.scrutinee
                        arms      = m.arms |> List.map mapArm
                        tvar      = m.tvar
                    |}
        go e

    // ─── Shared stream-binding classification ─────────────────────────
    // One classifier used by the desugar pass, the solver's step-let
    // detection and the emit's stream-fn registration, so the three can
    // never drift apart. Scoped: shadowing a name (e.g. `count = 5`)
    // reclassifies it for the rest of the chain.
    //
    //   StreamValue : the name holds a step-record itself (count, now,
    //                 custom `{ __machine: ... }` literals)
    //   StreamFn n  : curried fn of arity n ultimately producing a step-record
    //   Plain       : everything else -- INCLUDING stream-let bindings,
    //                 whose name is lifted to the per-tick value V.
    [<RequireQualifiedAccess>]
    type BindingKind =
        | StreamValue
        | StreamFn of arity: int
        | Plain

    type StreamKinds = Map<string, BindingKind>

    // Classify a prelude (External) entry by its declared type.
    let bindingKindOfTyp (typ: Typ) : BindingKind =
        let mono = match typ with Mono m -> m | Poly p -> p.monoTyp
        let rec walk t n =
            match t with
            | FunTyp (_, ret) -> walk ret (n + 1)
            | other ->
                if BuiltinTypes.isStepRecord other then
                    if n = 0 then BindingKind.StreamValue else BindingKind.StreamFn n
                else BindingKind.Plain
        walk mono 0

    // Is `e` a stream-typed expression under `kinds`: a Var naming a
    // StreamValue, or a saturated call of a StreamFn? This is exactly the
    // let-lift trigger -- and the hoist trigger in the desugar pass. A
    // step-record LITERAL is deliberately not one: `count = { __machine: .. }`
    // DEFINES a StreamValue, it doesn't get lifted.
    let isStreamExpr (kinds: StreamKinds) (e: Expr<'a>) : bool =
        match e with
        | Expr.Var v -> kinds.TryFind v.ident = Some BindingKind.StreamValue
        | Expr.App _ ->
            match headIdentOf e with
            | Some name ->
                match kinds.TryFind name with
                | Some (BindingKind.StreamFn arity) ->
                    let argCount = collectAppChain e |> snd |> List.length
                    argCount >= arity
                | _ -> false
            | None -> false
        | _ -> false

    // Classify what a let-binding's NAME means downstream, given its RHS.
    // A stream-typed RHS means the let is LIFTED (name = per-tick V = Plain).
    let rec bindingKindOfRhs (kinds: StreamKinds) (rhs: Expr<'a>) : BindingKind =
        match rhs with
        | Expr.MkRecord r when hasStepShape r.items -> BindingKind.StreamValue
        | Expr.Fun _ ->
            let rec arityAndBody e n =
                match e with
                | Expr.Fun f -> arityAndBody f.body (n + 1)
                | _ -> e, n
            let body, arity = arityAndBody rhs 0
            if chainIsStream kinds body then BindingKind.StreamFn arity
            else BindingKind.Plain
        | _ -> BindingKind.Plain

    // Does a statement chain (Let/Do spine) produce a step-record: any
    // stream-let inside it, or a terminal stream expression / step literal.
    and chainIsStream (kinds: StreamKinds) (e: Expr<'a>) : bool =
        match e with
        | Expr.Let l ->
            isStreamExpr kinds l.value
            || chainIsStream (kinds |> Map.add l.ident.identName (bindingKindOfRhs kinds l.value)) l.body
        | Expr.Do d -> chainIsStream kinds d.body
        | Expr.MkRecord r -> hasStepShape r.items
        | e -> isStreamExpr kinds e

module BuiltinValues =
    let [<Literal>] unitValueIdent = "UnitValue"
    let [<Literal>] toStringFunctionIdent = "ToString"

module TypeSystem =
    open PureState.TypeFighter.Utils

    /// A constraint represents a relation between types that the solver must satisfy.
    /// Each variant is a different relation; the solver dispatches over them.
    type ConstraintKind =
        | CEq of MonoTyp * MonoTyp
        | CHasField of {| row: MonoTyp; fname: string; ftyp: MonoTyp |}
        | CHasMember of {| row: MonoTyp; memberTyp: MonoTyp |}
        // Restricts a tvar to a finite set of monotypes. Emitted by `inst`
        // for polytypes that carry per-tvar constraints. Solved by checking
        // membership when the tvar gets unified with a concrete type, or by
        // intersecting allowed sets when two constrained tvars meet.
        | CIsOneOf of {| tvar: VarNum; allowed: Set<MonoTyp> |}

    type Constraint =
        { triviaSource: Expr<VarNum>; kind: ConstraintKind }

    type Substitution = { tvar: VarNum; typ: Typ }
    type MonoSubstitution = { tvar: VarNum; monoTyp: MonoTyp }
    type SubstThis = SubstThis of MonoTyp
    type SubstWith = SubstWith of MonoTyp
    type SubstIn = SubstIn of MonoTyp

    type RecordRefs = Map<int, Set<FieldDefinition>>
    type MemberRefs = Map<int, Set<MonoTyp>>
    // Per-tvar membership constraints accumulated during solving. Closed
    // out at the end as a UnionTyp if the tvar didn't get unified concretely.
    type MembershipRefs = Map<int, Set<MonoTyp>>

    type SolverRun =
        {
            cycle: int
            constraints: Constraint list
            recordRefs: RecordRefs
            memberRefs: MemberRefs
            memberships: MembershipRefs
            substitutions: Substitution list
        }

    let rec substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn inTyp) =
        let substRecord record =
            let fields = [ for f in record.fields do { f with typ = substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn f.typ) }]
            let positionals =
                [ for p in record.positionals ->
                    substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn p) ]
            { record with fields = set fields; positionals = positionals }
        match inTyp with
        | t when t = typToReplace -> withTyp
        | TVar _ -> inTyp
        | SaturatedTyp app ->
            let substitutedArgs = [ for arg in app.args do substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn arg) ]
            SaturatedTyp {| app with args = substitutedArgs |}
        | FunTyp (t1, t2) ->
            let t1 = substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn t1)
            let t2 = substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn t2)
            FunTyp (t1, t2)
        | RecordTyp record ->
            RecordTyp (substRecord record)
        | LiteralTyp _ -> inTyp
        | UnionTyp members ->
            UnionTyp (set [
                for m in members do
                    substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn m)
            ])
    
    let rec substVarWithTypInTyp (tvarToReplace: VarNum) withTyp inTyp =
        substTypWithTypInTyp (SubstThis (TVar tvarToReplace)) withTyp inTyp

    let generateConstraints (env: Env) (expr: Expr<VarNum>) newVar =
        let constraints = Mutable.fifo None

        let trace = System.Text.StringBuilder()

        let mutable exprToEnv = Map.empty
        let addEnv (expr: Expr<VarNum>) (env: Env) =
            do exprToEnv <- exprToEnv |> Map.add expr env

        // Scoped stream classification (StreamValue / StreamFn / Plain) for
        // every binding in scope, threaded exactly like `env` so shadowing
        // (e.g. `count = 5`) reclassifies a name for the rest of the chain.
        // Built by the shared classifier in `module StepRecord`.
        let initialKinds : StepRecord.StreamKinds =
            env
            |> Map.toSeq
            |> Seq.choose (fun (name, item) ->
                match item with
                | EnvItem.External typ ->
                    match StepRecord.bindingKindOfTyp typ with
                    | StepRecord.BindingKind.Plain -> None
                    | kind -> Some (name, kind)
                | EnvItem.Internal _ -> None)
            |> Map.ofSeq

        let rec generateConstraints (env: Env) (kinds: StepRecord.StreamKinds) (expr: Expr<VarNum>) =

            let appendConstraint (tvar: VarNum) (typ: MonoTyp) =
                trace.AppendLine($"{expr}    :::     {TVar tvar} := {typ}") |> ignore
                do constraints.Append({ triviaSource = expr; kind = CEq (TVar tvar, typ) })

            let inst (typ: Typ) =
                match typ with
                | Mono mono ->
                    mono
                | Poly poly ->
                    // Build a renaming map orig -> fresh so per-tvar constraints
                    // can be re-emitted against the fresh tvars.
                    let renaming =
                        poly.vars
                        |> Set.toList
                        |> List.map (fun v -> v, newVar ())
                        |> Map.ofList
                    let mutable substed = poly.monoTyp
                    for KeyValue (origVar, freshVar) in renaming do
                        substed <- substVarWithTypInTyp origVar (SubstWith (TVar freshVar)) (SubstIn substed)
                    for KeyValue (origVar, allowed) in poly.constraints do
                        match Map.tryFind origVar renaming with
                        | Some freshVar ->
                            do constraints.Append
                                {
                                    triviaSource = expr
                                    kind = CIsOneOf {| tvar = freshVar; allowed = allowed |}
                                }
                        | None -> ()
                    substed

            do addEnv expr env

            match expr with

            | Expr.Lit x ->
                let litTyp =
                    match x.value with
                    | Boolean _ -> BuiltinTypes.boolean
                    | Number _ -> BuiltinTypes.number
                    | String _ -> BuiltinTypes.string
                appendConstraint x.tvar litTyp

            | Expr.Var x ->
                (*
                    S: ident
                    P: env has ident:t
                    C: expr:t
                *)
                let resolvedIdent =
                    match env |> Map.tryFind x.ident with
                    | Some (EnvItem.Internal tvar) ->
                        TVar tvar
                    | Some (EnvItem.External typ) ->
                        // INST - here's where it happens :)
                        inst typ
                    | None ->
                        failwith $"Unresolved identifier: {x.ident}"
                appendConstraint x.tvar resolvedIdent

            | Expr.App x ->
                (*
                    S: func arg
                    P: func: t1 -> t2, arg:t1
                    C: expr:t2
                *)
                generateConstraints env kinds x.func
                generateConstraints env kinds x.arg
                appendConstraint x.func.TVar (FunTyp (TVar x.arg.TVar, TVar x.tvar))

            | Expr.Fun x ->
                (*
                    S:fun ident -> body
                    P: ident:t1, body:t2
                    C: expr: t1 -> t2
                *)
                generateConstraints
                    (env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar))
                    (kinds |> Map.add x.ident.identName StepRecord.BindingKind.Plain)
                    x.body

                appendConstraint x.tvar (FunTyp (TVar x.ident.tvar, TVar x.body.TVar))

            | Expr.Let x ->
                (*
                    S: let ident = value in body
                    P: value:t1, body:t2
                    C: expr:t2

                    Step-let detection: if `value` is a stream expression (a
                    step-record literal, a StreamValue var or a saturated
                    StreamFn call -- see StepRecord.isStreamExpr), lift this
                    Let: bind `ident` to the inner V (the per-tick value) and
                    tag the enclosing block as itself a step-record. The body
                    is the continuation, so every step in a block composes
                    into one state-machine. Falls back to plain-let otherwise.

                    Detection is purely structural and runs before types are
                    solved, via the shared scoped classifier in `StepRecord`.
                    A value whose step-record nature only emerges from
                    inference (e.g. arriving via a function parameter) is
                    not lifted.
                *)
                generateConstraints env kinds x.value

                let isStreamRhs = StepRecord.isStreamExpr kinds x.value

                // The lifted name is the per-tick value -- Plain, never a
                // stream. Only non-lifted bindings can classify as
                // StreamValue (record literal) or StreamFn (fn chain).
                let identKind =
                    if isStreamRhs then StepRecord.BindingKind.Plain
                    else StepRecord.bindingKindOfRhs kinds x.value

                let bodyKinds = kinds |> Map.add x.ident.identName identKind

                if isStreamRhs then
                    let s1 = newVar ()
                    appendConstraint x.value.TVar
                        (BuiltinTypes.stepRecord (TVar x.ident.tvar) (TVar s1))
                    // Body chain containing a stream-let or ending in a
                    // stream expression is itself a step-record: passthrough,
                    // don't double-wrap.
                    if StepRecord.chainIsStream bodyKinds x.body then
                        let bodyV = newVar ()
                        let bodyS = newVar ()
                        appendConstraint x.body.TVar
                            (BuiltinTypes.stepRecord (TVar bodyV) (TVar bodyS))
                        appendConstraint x.tvar (TVar x.body.TVar)
                    else
                        // Body is pure — wrap it as the inner V of the block.
                        let s2 = newVar ()
                        appendConstraint x.tvar
                            (BuiltinTypes.stepRecord (TVar x.body.TVar) (TVar s2))
                else
                    appendConstraint x.ident.tvar (TVar x.value.TVar)
                    appendConstraint x.tvar (TVar x.body.TVar)

                generateConstraints
                    (env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar))
                    bodyKinds
                    x.body

            | Expr.Do x ->
                (*
                    S: do action in body
                    P: action:unit, body:t2
                    C: expr:t2
                *)
                generateConstraints env kinds x.body
                generateConstraints env kinds x.action

                appendConstraint x.tvar (TVar x.body.TVar)
                appendConstraint x.action.TVar BuiltinTypes.unit

            | Expr.PropAcc x ->
                (*
                    S: source.ident
                    P: source:t1, t1 is a record in the ctx, t1 has field "ident", ident:t2
                    C: expr:t2
                *)

                generateConstraints env kinds x.source

                do constraints.Append
                    {
                        triviaSource = expr
                        kind = CHasField {| row = TVar x.source.TVar
                                            fname = x.ident.identName
                                            ftyp = TVar x.ident.tvar |}
                    }

                appendConstraint x.tvar (TVar x.ident.tvar)

            | Expr.MkArray x ->
                let elemTyp = TVar (newVar ())

                for v in x.values do
                    appendConstraint v.TVar elemTyp
                    generateConstraints env kinds v

                appendConstraint x.tvar (BuiltinTypes.array elemTyp)

            | Expr.MkRecord x ->
                for item in x.items do
                    generateConstraints env kinds item.Value

                // Named items -> named row, positionals -> second row.
                // TODO: field names must be distinct
                let namedFields =
                    [ for item in x.items do
                        match item with
                        | RecordItem.Property f -> yield f.fname, TVar f.value.TVar
                        | RecordItem.Positional _ -> () ]
                    |> List.sortBy fst
                let positionals =
                    [ for item in x.items do
                        match item with
                        | RecordItem.Positional v -> yield TVar v.TVar
                        | RecordItem.Property _ -> () ]
                appendConstraint
                    x.tvar
                    (RecordTyp
                        {
                            fields = set [ for n, t in namedFields -> { fname = n; typ = t } ]
                            positionals = positionals
                        })

            | Expr.Match x ->
                (*
                    S: match scrutinee with | pat_i -> body_i
                    P:  scrutinee : tS
                        each pattern constrains tS:
                          - Literal l : emit CHasMember { row=tS, memberTyp=LiteralTyp l }
                          - Var v     : bind v to tS in arm env (catch-all)
                          - Wildcard  : no constraint on tS (catch-all)
                        all body_i : tR
                    C: expr : tR
                *)
                generateConstraints env kinds x.scrutinee
                let scrutineeTyp = TVar x.scrutinee.TVar
                for a in x.arms do
                    let armEnv, armKinds =
                        match a.pattern with
                        | MatchPattern.Literal l ->
                            do constraints.Append
                                {
                                    triviaSource = expr
                                    kind = CHasMember {|
                                        row = scrutineeTyp
                                        memberTyp = LiteralTyp l.value |}
                                }
                            appendConstraint l.tvar (LiteralTyp l.value)
                            env, kinds
                        | MatchPattern.Var ident ->
                            // Catch-all binder: bind `ident` to the
                            // scrutinee's type within this arm's body.
                            appendConstraint ident.tvar scrutineeTyp
                            env |> Map.add ident.identName (EnvItem.Internal ident.tvar),
                            kinds |> Map.add ident.identName StepRecord.BindingKind.Plain
                        | MatchPattern.Wildcard w ->
                            appendConstraint w.tvar scrutineeTyp
                            env, kinds
                    generateConstraints armEnv armKinds a.body
                    appendConstraint x.tvar (TVar a.body.TVar)

        do generateConstraints env initialKinds expr

        constraints.Values, exprToEnv, trace.ToString()

    /// Closes row vars that accumulated CHasField / CHasMember constraints
    /// but never got pinned: into RecordTyp and UnionTyp respectively.
    let finalizeSubstitutions
        (substitutions: MonoSubstitution list)
        (pendingFields: RecordRefs)
        (pendingMembers: MemberRefs)
        (pendingMemberships: MembershipRefs)
        =
        let closeRecord (subs: MonoSubstitution list) (varNum: int, fields: Set<FieldDefinition>) =
            let recordTyp =
                RecordTyp { fields = fields; positionals = [] }
            let alpha = VarNum varNum
            let substituted =
                [ for s in subs ->
                    { s with monoTyp = substVarWithTypInTyp alpha (SubstWith recordTyp) (SubstIn s.monoTyp) } ]
            { tvar = alpha; monoTyp = recordTyp } :: substituted
        let closeUnion (subs: MonoSubstitution list) (varNum: int, members: Set<MonoTyp>) =
            let unionTyp = UnionTyp members
            let alpha = VarNum varNum
            let substituted =
                [ for s in subs ->
                    { s with monoTyp = substVarWithTypInTyp alpha (SubstWith unionTyp) (SubstIn s.monoTyp) } ]
            { tvar = alpha; monoTyp = unionTyp } :: substituted
        [
            let withRecords = pendingFields |> Map.toList |> List.fold closeRecord substitutions
            let withMembers = pendingMembers |> Map.toList |> List.fold closeUnion withRecords
            // Tvars that retained CIsOneOf constraints without ever being unified
            // to a single concrete type close as a UnionTyp of the allowed set.
            let closed = pendingMemberships |> Map.toList |> List.fold closeUnion withMembers
            for s in closed do
                { tvar = s.tvar; typ = Typ.gen s.monoTyp }
        ]

    let solveConstraints (constraints: Constraint list) maxSolverRuns =
        let mutable solverRuns = ResizeArray()

        // Keyed by row-TVar, accumulating CHasField requirements as they
        // come in. Internal index; not exposed in MonoTyp.
        let rec solve
            (constraints: Constraint list)
            (pendingFields: RecordRefs)
            (pendingMembers: MemberRefs)
            (pendingMemberships: MembershipRefs)
            (substitutions: MonoSubstitution list)
            =
            do
                let substitutions =
                    [
                        for msi in substitutions do
                            { tvar = msi.tvar; typ = Mono msi.monoTyp }
                    ]
                let solverRun =
                    {
                        cycle = solverRuns.Count
                        constraints = constraints
                        recordRefs = pendingFields
                        memberRefs = pendingMembers
                        memberships = pendingMemberships
                        substitutions = substitutions
                    }
                solverRuns.Add(solverRun)

            let continueSolve =
                match maxSolverRuns with
                | None -> true
                | Some maxSolverRuns when solverRuns.Count <= maxSolverRuns -> true
                | _ -> false

            if continueSolve then
                match constraints with
                | [] -> substitutions, pendingFields, pendingMembers, pendingMemberships
                | c :: constraints ->
                    let mutable constraints = constraints
                    let mutable substitutions = substitutions
                    let nextConstraints = Mutable.fifo None
                    let pendingFields = Mutable.oneToMany (Some pendingFields)
                    let pendingMembers = Mutable.oneToMany (Some pendingMembers)
                    let pendingMemberships = Mutable.oneToMany (Some pendingMemberships)

                    let applySubstToKind left right k =
                        match k with
                        | CEq (t1, t2) ->
                            CEq (
                                substVarWithTypInTyp left (SubstWith right) (SubstIn t1),
                                substVarWithTypInTyp left (SubstWith right) (SubstIn t2)
                            )
                        | CHasField r ->
                            CHasField {| r with
                                          row = substVarWithTypInTyp left (SubstWith right) (SubstIn r.row)
                                          ftyp = substVarWithTypInTyp left (SubstWith right) (SubstIn r.ftyp) |}
                        | CHasMember r ->
                            CHasMember {| r with
                                            row = substVarWithTypInTyp left (SubstWith right) (SubstIn r.row)
                                            memberTyp = substVarWithTypInTyp left (SubstWith right) (SubstIn r.memberTyp) |}
                        | CIsOneOf r ->
                            // Substitute inside the allowed set in case any element references `left`.
                            let substedAllowed =
                                r.allowed
                                |> Set.map (fun a -> substVarWithTypInTyp left (SubstWith right) (SubstIn a))
                            if r.tvar = left then
                                match right with
                                | TVar y ->
                                    CIsOneOf {| tvar = y; allowed = substedAllowed |}
                                | concrete ->
                                    // Re-target as membership check against a Union of allowed types.
                                    CHasMember {| row = UnionTyp substedAllowed; memberTyp = concrete |}
                            else
                                CIsOneOf {| tvar = r.tvar; allowed = substedAllowed |}

                    let rec unifyTypes (source: Expr<VarNum>) (t1: MonoTyp) (t2: MonoTyp) =
                        let throwUniError message = raise (UnificationError(source, t1, t2, Some message))

                        let unifyRecordFields (requiredFields: Set<FieldDefinition>) (providedFields: Set<FieldDefinition>) =
                            let unprovidedFields =
                                requiredFields
                                |> Set.filter (fun f -> not (providedFields |> Set.exists (fun pf -> pf.fname = f.fname)))
                            if unprovidedFields <> Set.empty then
                                throwUniError $"The following members are required, but missing in the provided record: {unprovidedFields}"
                            for requiredField in requiredFields do
                                let providedField =
                                    providedFields
                                    |> Set.toSeq
                                    |> Seq.tryFind (fun f -> f.fname = requiredField.fname)
                                match providedField with
                                | None ->
                                    throwUniError $"Member '{requiredField.fname}' is missing in type {providedFields}"
                                | Some existingField ->
                                    try
                                        do unifyTypes source requiredField.typ existingField.typ
                                    with
                                    | :? UnificationError as uniErr ->
                                        let detail = match uniErr.Reason with Some r -> $": {r}" | None -> ""
                                        raise (UnificationError(
                                            source,
                                            uniErr.T1,
                                            uniErr.T2,
                                            Some $"Type mismatch in record field '{requiredField.fname}'{detail}"))

                        match t1, t2 with
                        | t1, t2 when t1 = t2 -> ()
                        | t, TVar tvar
                        | TVar tvar, t -> nextConstraints.Append({ triviaSource = source; kind = CEq (TVar tvar, t) })
                        | SaturatedTyp app1, SaturatedTyp app2 when app1.name = app2.name ->
                            let rec loop funArgs1 funArgs2 =
                                match funArgs1, funArgs2 with
                                | [], [] -> ()
                                | a1 :: args1, a2 :: args2 ->
                                    do loop args1 args2
                                    do unifyTypes source a1 a2
                                | [], _
                                | _, [] -> throwUniError "Type parameters count mismatch."
                            do loop app1.args app2.args
                        | FunTyp (ta, tb), FunTyp (tc, td) ->
                            do unifyTypes source ta tc
                            do unifyTypes source tb td
                        | RecordTyp rec1, RecordTyp rec2 ->
                            unifyRecordFields rec1.fields rec2.fields
                            // Ordered-list unification for Step 4; bag
                            // semantics will land with pattern-matching.
                            if List.length rec1.positionals <> List.length rec2.positionals then
                                throwUniError
                                    $"Positional item count mismatch: {List.length rec1.positionals} vs {List.length rec2.positionals}"
                            for p1, p2 in List.zip rec1.positionals rec2.positionals do
                                unifyTypes source p1 p2
                        | LiteralTyp lit, UnionTyp members
                        | UnionTyp members, LiteralTyp lit ->
                            if not (Set.contains (LiteralTyp lit) members) then
                                throwUniError $"Literal '{LiteralTyp lit}' is not a member of union '{UnionTyp members}'"
                        | UnionTyp ms1, UnionTyp ms2 when ms1 = ms2 -> ()
                        | _, _ ->
                            throwUniError $"Can't unify types '{t1}' and '{t2}'"

                    match c.kind with

                    // ---- CHasField: accumulate or resolve ----
                    | CHasField req ->
                        match req.row with
                        | TVar (VarNum rowN) ->
                            // Add to pending; if same fname is already required with a
                            // different type, emit CEq to force the types to unify.
                            let current =
                                pendingFields.Values
                                |> Map.tryFind rowN
                                |> Option.defaultValue Set.empty
                            let existing =
                                current |> Set.toSeq |> Seq.tryFind (fun f -> f.fname = req.fname)
                            match existing with
                            | Some f when f.typ <> req.ftyp ->
                                nextConstraints.Append
                                    {
                                        triviaSource = c.triviaSource
                                        kind = CEq (req.ftyp, f.typ)
                                    }
                            | _ -> ()
                            let updated = Set.add { fname = req.fname; typ = req.ftyp } current
                            do pendingFields.Replace(rowN, updated)
                            for c' in constraints do
                                do nextConstraints.Append c'

                        | RecordTyp record ->
                            // Row is already a concrete record -- check field exists and emit CEq.
                            let found =
                                record.fields
                                |> Set.toSeq
                                |> Seq.tryFind (fun f -> f.fname = req.fname)
                            match found with
                            | Some f ->
                                nextConstraints.Append
                                    {
                                        triviaSource = c.triviaSource
                                        kind = CEq (req.ftyp, f.typ)
                                    }
                            | None ->
                                raise (UnificationError(
                                    c.triviaSource, req.row, req.ftyp,
                                    Some $"Member '{req.fname}' is missing in record type {req.row}"))
                            for c' in constraints do
                                do nextConstraints.Append c'

                        | other ->
                            raise (UnificationError(
                                c.triviaSource, other, req.ftyp,
                                Some $"Can't access field '{req.fname}' on non-record type {other}"))

                    // ---- CHasMember: accumulate or resolve ----
                    | CHasMember req ->
                        match req.row with
                        | TVar (VarNum rowN) ->
                            let current =
                                pendingMembers.Values
                                |> Map.tryFind rowN
                                |> Option.defaultValue Set.empty
                            do pendingMembers.Replace(rowN, Set.add req.memberTyp current)
                            for c' in constraints do
                                do nextConstraints.Append c'

                        | UnionTyp members ->
                            if not (Set.contains req.memberTyp members) then
                                raise (UnificationError(
                                    c.triviaSource, req.row, req.memberTyp,
                                    Some $"'{req.memberTyp}' is not a member of union '{req.row}'"))
                            for c' in constraints do
                                do nextConstraints.Append c'

                        | LiteralTyp _ when req.row = req.memberTyp ->
                            for c' in constraints do
                                do nextConstraints.Append c'

                        // `LiteralTyp 5 ∈ Number`, etc. -- literals belong
                        // to the built-in they widen to.
                        | builtin when
                            (match req.memberTyp with
                             | LiteralTyp (Number _) -> builtin = BuiltinTypes.number
                             | LiteralTyp (String _) -> builtin = BuiltinTypes.string
                             | LiteralTyp (Boolean _) -> builtin = BuiltinTypes.boolean
                             | _ -> false) ->
                            for c' in constraints do
                                do nextConstraints.Append c'

                        | other ->
                            raise (UnificationError(
                                c.triviaSource, other, req.memberTyp,
                                Some $"'{req.memberTyp}' is not a member of non-union type {other}"))

                    // ---- CIsOneOf: accumulate per-tvar membership constraint ----
                    | CIsOneOf req ->
                        let (VarNum tn) = req.tvar
                        let current =
                            pendingMemberships.Values
                            |> Map.tryFind tn
                        let intersected =
                            match current with
                            | Some existing -> Set.intersect existing req.allowed
                            | None -> req.allowed
                        if Set.isEmpty intersected then
                            raise (UnificationError(
                                c.triviaSource, TVar req.tvar, TVar req.tvar,
                                Some $"Membership constraint cannot be satisfied: empty intersection of allowed sets for {TVar req.tvar}"))
                        do pendingMemberships.Replace(tn, intersected)
                        for c' in constraints do
                            do nextConstraints.Append c'

                    // ---- CEq with TVar on the left: substitute everywhere ----
                    | CEq (TVar left, right) ->
                        let (VarNum leftN) = left

                        // Interact pending[left] with right BEFORE rewriting constraints.
                        let leftPending =
                            pendingFields.Values
                            |> Map.tryFind leftN
                            |> Option.defaultValue Set.empty

                        if not (Set.isEmpty leftPending) then
                            match right with
                            | TVar (VarNum rightN) ->
                                // Merge leftPending into rightPending; unify same-name fields.
                                let rightPending =
                                    pendingFields.Values
                                    |> Map.tryFind rightN
                                    |> Option.defaultValue Set.empty
                                let groupedByName =
                                    [ yield! leftPending; yield! rightPending ]
                                    |> List.groupBy _.fname
                                for _, fs in groupedByName do
                                    match fs with
                                    | [f1; f2] ->
                                        nextConstraints.Append
                                            {
                                                triviaSource = c.triviaSource
                                                kind = CEq (f1.typ, f2.typ)
                                            }
                                    | _ -> ()
                                let merged = set [ yield! leftPending; yield! rightPending ]
                                do pendingFields.Remove(leftN)
                                do pendingFields.Replace(rightN, merged)
                            | RecordTyp record ->
                                let pendingAsRecord =
                                    RecordTyp { fields = leftPending; positionals = [] }
                                do unifyTypes c.triviaSource pendingAsRecord (RecordTyp record)
                                do pendingFields.Remove(leftN)
                            | other ->
                                raise (UnificationError(
                                    c.triviaSource, TVar left, other,
                                    Some $"Row-constrained type var cannot be unified with non-record type {other}"))

                        // Interact pending member requirements with the resolved right.
                        let leftPendingMembers =
                            pendingMembers.Values
                            |> Map.tryFind leftN
                            |> Option.defaultValue Set.empty

                        if not (Set.isEmpty leftPendingMembers) then
                            match right with
                            | TVar (VarNum rightN) ->
                                let rightPending =
                                    pendingMembers.Values
                                    |> Map.tryFind rightN
                                    |> Option.defaultValue Set.empty
                                let merged = Set.union leftPendingMembers rightPending
                                do pendingMembers.Remove(leftN)
                                do pendingMembers.Replace(rightN, merged)
                            | _ ->
                                for m in leftPendingMembers do
                                    do nextConstraints.Append
                                        {
                                            triviaSource = c.triviaSource
                                            kind = CHasMember {| row = right; memberTyp = m |}
                                        }
                                do pendingMembers.Remove(leftN)

                        // Interact pending membership constraints with the resolved right.
                        let leftPendingMemberships =
                            pendingMemberships.Values
                            |> Map.tryFind leftN
                            |> Option.defaultValue Set.empty

                        if not (Set.isEmpty leftPendingMemberships) then
                            match right with
                            | TVar (VarNum rightN) ->
                                let rightPending =
                                    pendingMemberships.Values
                                    |> Map.tryFind rightN
                                let merged =
                                    match rightPending with
                                    | Some existing -> Set.intersect leftPendingMemberships existing
                                    | None -> leftPendingMemberships
                                if Set.isEmpty merged then
                                    raise (UnificationError(
                                        c.triviaSource, TVar left, right,
                                        Some "Membership constraints have no common type"))
                                do pendingMemberships.Remove(leftN)
                                do pendingMemberships.Replace(rightN, merged)
                            | concrete ->
                                if not (Set.contains concrete leftPendingMemberships) then
                                    let allowed =
                                        leftPendingMemberships
                                        |> Set.toList
                                        |> List.map string
                                        |> String.concat "; "
                                    raise (UnificationError(
                                        c.triviaSource, TVar left, concrete,
                                        Some $"Type {concrete} is not in allowed set [{allowed}]"))
                                do pendingMemberships.Remove(leftN)

                        // Rewrite remaining constraints with the substitution.
                        for c' in constraints do
                            do nextConstraints.Append
                                {
                                    triviaSource = c'.triviaSource
                                    kind = applySubstToKind left right c'.kind
                                }

                        for r in pendingFields.Values do
                            let newFields =
                                [ for f in r.Value ->
                                    { f with typ = substVarWithTypInTyp left (SubstWith right) (SubstIn f.typ) } ]
                            do pendingFields.Replace(r.Key, newFields)

                        for r in pendingMembers.Values do
                            let newMembers =
                                set [ for m in r.Value -> substVarWithTypInTyp left (SubstWith right) (SubstIn m) ]
                            do pendingMembers.Replace(r.Key, newMembers)

                        for r in pendingMemberships.Values do
                            let newAllowed =
                                set [ for a in r.Value -> substVarWithTypInTyp left (SubstWith right) (SubstIn a) ]
                            do pendingMemberships.Replace(r.Key, newAllowed)

                        do substitutions <-
                            [
                                { tvar = left; monoTyp = right }
                                for s in substitutions do
                                    {
                                        tvar = s.tvar
                                        monoTyp = substVarWithTypInTyp left (SubstWith right) (SubstIn s.monoTyp)
                                    }
                            ]

                    // ---- CEq with concrete left and right: structurally unify ----
                    | CEq (left, right) ->
                        do unifyTypes c.triviaSource left right
                        for c' in constraints do
                            do nextConstraints.Append(c')
                        do substitutions <- substitutions

                    solve nextConstraints.Values pendingFields.Values pendingMembers.Values pendingMemberships.Values substitutions
            else
                substitutions, pendingFields, pendingMembers, pendingMemberships

        let mutable errorTVar : VarNum option = None
        let result =
            try
                let substitutions, pendingFields, pendingMembers, pendingMemberships =
                    solve constraints Map.empty Map.empty Map.empty []
                Ok (finalizeSubstitutions substitutions pendingFields pendingMembers pendingMemberships)
            with
            | :? UnificationError as ue ->
                errorTVar <- Some ue.SourceTVar
                Error ue.Message
            | ex ->
                Error ex.Message

        {|
            substitutions = result
            errorTVar = errorTVar
            solverRuns = [ yield! solverRuns ]
        |}


[<RequireQualifiedAccess>]
module Solver =

    type Solution =
        {
            numberedExpr: Expr<VarNum>
            result:
                Result<
                    {|
                        substitutions: TypeSystem.Substitution list
                        typ: Typ option
                    |},
                    string
                >
            // The TVar of the AST node that triggered the unification failure
            // (set when result is Error from a UnificationError). Combined with
            // `tvarRanges` it lets callers point at the offending source range.
            errorTVar: VarNum option
            // Per-tvar source range, populated only via `solveWithRanges`.
            // Empty when the caller used `solve` (no parser range info).
            tvarRanges: Map<VarNum, Range>
            solverRuns: list<TypeSystem.SolverRun>
            exprToEnv: Map<Expr<VarNum>, Env>
            trace: string
        }

    /// Walk an `Expr<unit>` and produce `Expr<VarNum>` while recording each
    /// node's source range (when the parser tracked one). Returns the numbered
    /// expression and a `Map<VarNum, Range>`.
    let private numberWithRanges
        (expr: Expr<unit>)
        (newVar: unit -> VarNum)
        (resolveRange: Expr<unit> -> Range option)
        =
        let ranges = System.Collections.Generic.Dictionary<VarNum, Range>()
        let trackTvar (e: Expr<unit>) (tv: VarNum) =
            match resolveRange e with
            | Some r -> ranges.[tv] <- r
            | None -> ()
            tv
        let numberIdent (ident: Ident<unit>) : Ident<VarNum> =
            { identName = ident.identName; tvar = newVar () }
        let rec loop (e: Expr<unit>) =
            match e with
            | Expr.Lit x ->
                let v = trackTvar e (newVar ())
                Expr.Lit {| value = x.value; tvar = v |}
            | Expr.Var x ->
                let v = trackTvar e (newVar ())
                Expr.Var {| ident = x.ident; tvar = v |}
            | Expr.App x ->
                let func = loop x.func
                let arg = loop x.arg
                let v = trackTvar e (newVar ())
                Expr.App {| func = func; arg = arg; tvar = v |}
            | Expr.Fun x ->
                let body = loop x.body
                let v = trackTvar e (newVar ())
                Expr.Fun {| ident = numberIdent x.ident; body = body; tvar = v |}
            | Expr.Let x ->
                let value = loop x.value
                let body = loop x.body
                let v = trackTvar e (newVar ())
                Expr.Let {| ident = numberIdent x.ident; value = value; body = body; tvar = v |}
            | Expr.Do x ->
                let action = loop x.action
                let body = loop x.body
                let v = trackTvar e (newVar ())
                Expr.Do {| action = action; body = body; tvar = v |}
            | Expr.PropAcc x ->
                let source = loop x.source
                let identTvar = newVar ()
                let v = trackTvar e (newVar ())
                Expr.PropAcc {|
                    source = source
                    ident = { identName = x.ident.identName; tvar = identTvar }
                    tvar = v
                |}
            | Expr.MkArray x ->
                let values = [ for v in x.values -> loop v ]
                let v = trackTvar e (newVar ())
                Expr.MkArray {| values = values; tvar = v |}
            | Expr.MkRecord x ->
                let items =
                    [ for item in x.items do
                        match item with
                        | RecordItem.Property f ->
                            RecordItem.Property {| fname = f.fname; value = loop f.value |}
                        | RecordItem.Positional v ->
                            RecordItem.Positional (loop v) ]
                let v = trackTvar e (newVar ())
                Expr.MkRecord {| items = items; tvar = v |}
            | Expr.Match x ->
                let numberPattern (p: MatchPattern<unit>) : MatchPattern<VarNum> =
                    match p with
                    | MatchPattern.Literal l ->
                        MatchPattern.Literal {| value = l.value; tvar = newVar () |}
                    | MatchPattern.Var v ->
                        MatchPattern.Var (numberIdent v)
                    | MatchPattern.Wildcard _ ->
                        MatchPattern.Wildcard {| tvar = newVar () |}
                let scrut = loop x.scrutinee
                let arms =
                    [ for a in x.arms ->
                        { pattern = numberPattern a.pattern; body = loop a.body } ]
                let v = trackTvar e (newVar ())
                Expr.Match {| scrutinee = scrut; arms = arms; tvar = v |}
        let numbered = loop expr
        let tvarRanges = Map.ofSeq (seq { for kv in ranges -> kv.Key, kv.Value })
        numbered, tvarRanges

    // Shift every TVar in the prelude env by `varOffset` so its numbering
    // can't collide with the freshly-numbered expression's tvars. Shared by
    // `solve` and `solveWithRanges`.
    let private reindexEnv (varOffset: int) (env: (string * Typ) list) =
        [
            for ident, typ in env do
                let rec reindexedVarNums typ =
                    match typ with
                    | Mono typ -> Mono (reindexMono typ)
                    | Poly poly ->
                        let vars = [ for (VarNum v) in poly.vars do VarNum (v + varOffset) ]
                        let monoTyp = reindexMono poly.monoTyp
                        // Re-key constraints to the shifted tvars; allowed-sets contain
                        // monotypes (e.g. Number/String) that don't reference our tvars.
                        let constraints =
                            poly.constraints
                            |> Map.toList
                            |> List.map (fun (VarNum v, allowed) -> VarNum (v + varOffset), allowed)
                            |> Map.ofList
                        Poly { vars = vars |> Set.ofList; constraints = constraints; monoTyp = monoTyp }
                and reindexMono (typ: MonoTyp) =
                    let reindexRecord record =
                        let fields = [ for f in record.fields do { f with typ = reindexMono f.typ } ]
                        let positionals = [ for p in record.positionals -> reindexMono p ]
                        { record with fields = fields |> Set.ofList; positionals = positionals }
                    match typ with
                    | TVar (VarNum n) ->
                        TVar (VarNum (n + varOffset))
                    | SaturatedTyp app ->
                        SaturatedTyp {| app with args = app.args |> List.map reindexMono |}
                    | FunTyp (t1, t2) ->
                        FunTyp (reindexMono t1, reindexMono t2)
                    | RecordTyp record ->
                        RecordTyp (reindexRecord record)
                    | LiteralTyp _ -> typ
                    | UnionTyp members ->
                        UnionTyp (set [ for m in members -> reindexMono m ])

                ident, EnvItem.External (reindexedVarNums typ)
        ]
        |> Map.ofList

    let solve (env: (string * Typ) list) maxSolverRuns (expr: Expr<Unit>) =
        let newVar = NumGen.mkGenerator()

        let expr = Expr.toNumberedExpr expr newVar

        // TODO: In Env, it should be disallowed having unquantified TVars

        let env = reindexEnv (Expr.maxVar expr) env

        let constraints,exprToEnv,trace = TypeSystem.generateConstraints env expr newVar
        let sr = TypeSystem.solveConstraints constraints maxSolverRuns

        {
            numberedExpr = expr
            result =
                match sr.substitutions with
                | Ok substitutions ->
                    Ok {|
                        substitutions = substitutions
                        // we can have limited solver runs; so the result is partial
                        typ =
                            substitutions
                            |> List.tryFind (fun s -> s.tvar = expr.TVar)
                            |> Option.map (fun x -> x.typ)
                    |}
                | Error message -> Error message
            errorTVar = sr.errorTVar
            tvarRanges = Map.empty
            solverRuns = sr.solverRuns
            exprToEnv = exprToEnv
            trace = trace
        }

    /// Like `solve` but also tracks per-tvar source ranges (collected from the
    /// parser via the `resolveRange` callback). On a unification failure the
    /// returned solution exposes the offending tvar plus the range map, so
    /// callers can translate the error to a precise editor squiggle.
    let solveWithRanges
        (env: (string * Typ) list)
        maxSolverRuns
        (expr: Expr<Unit>)
        (resolveRange: Expr<unit> -> Range option)
        =
        let newVar = NumGen.mkGenerator()
        let numberedExpr, tvarRanges = numberWithRanges expr newVar resolveRange

        let env = reindexEnv (Expr.maxVar numberedExpr) env

        let constraints, exprToEnv, trace = TypeSystem.generateConstraints env numberedExpr newVar
        let sr = TypeSystem.solveConstraints constraints maxSolverRuns

        {
            numberedExpr = numberedExpr
            result =
                match sr.substitutions with
                | Ok substitutions ->
                    Ok {|
                        substitutions = substitutions
                        typ =
                            substitutions
                            |> List.tryFind (fun s -> s.tvar = numberedExpr.TVar)
                            |> Option.map (fun x -> x.typ)
                    |}
                | Error message -> Error message
            errorTVar = sr.errorTVar
            tvarRanges = tvarRanges
            solverRuns = sr.solverRuns
            exprToEnv = exprToEnv
            trace = trace
        }

/// Ident walker -- surfaces every identifier occurrence in a numbered
/// Expr together with its associated TVar so tools (hover, go-to-def)
/// can correlate source locations to inferred types.
/// Traversal order matches the surface-syntax order so callers can zip
/// (name, kind, nth-by-name) with parser spans.
[<RequireQualifiedAccess>]
module IdentInfo =

    type IdentKind =
        | Var       // identifier used in expression position
        | Binding   // `let NAME = ...` -- ident of a Let
        | Param     // `(NAME) => ...` -- ident of a Fun
        | Field     // `expr.NAME` -- ident of a PropAcc

    type Occurrence =
        {
            name: string
            kind: IdentKind
            tvar: VarNum
        }

    /// Walk the numbered Expr and emit every identifier occurrence in
    /// document (left-to-right, top-down) order.
    let collect (expr: Expr<VarNum>) : Occurrence list =
        let out = ResizeArray<Occurrence>()

        let rec walk (e: Expr<VarNum>) =
            match e with
            | Expr.Lit _ -> ()
            | Expr.Var x ->
                out.Add { name = x.ident; kind = Var; tvar = x.tvar }
            | Expr.App x ->
                walk x.func
                walk x.arg
            | Expr.Fun x ->
                out.Add { name = x.ident.identName; kind = Param; tvar = x.ident.tvar }
                walk x.body
            | Expr.Let x ->
                out.Add { name = x.ident.identName; kind = Binding; tvar = x.ident.tvar }
                walk x.value
                walk x.body
            | Expr.Do x ->
                walk x.action
                walk x.body
            | Expr.PropAcc x ->
                // The source expression contains the field owner; the
                // field name comes *after* in source order.
                walk x.source
                out.Add { name = x.ident.identName; kind = Field; tvar = x.ident.tvar }
            | Expr.MkArray x ->
                for v in x.values do walk v
            | Expr.MkRecord x ->
                for item in x.items do
                    match item with
                    | RecordItem.Property f -> walk f.value
                    | RecordItem.Positional v -> walk v
            | Expr.Match x ->
                walk x.scrutinee
                for arm in x.arms do
                    match arm.pattern with
                    | MatchPattern.Literal _ -> ()
                    | MatchPattern.Var ident ->
                        out.Add { name = ident.identName; kind = Binding; tvar = ident.tvar }
                    | MatchPattern.Wildcard _ -> ()
                    walk arm.body

        walk expr
        List.ofSeq out

    /// Like `collect`, but skips the leading `skipLets` Let bindings of the
    /// chain entirely (binder AND value). Used to collect occurrences of the
    /// user region only, so a host-injected stdlib prefix can't contaminate
    /// the span-to-occurrence pairing for hovers.
    let collectSkippingTopLets (skipLets: int) (expr: Expr<VarNum>) : Occurrence list =
        let rec skip n (e: Expr<VarNum>) =
            if n = 0 then e
            else
                match e with
                | Expr.Let l -> skip (n - 1) l.body
                | _ -> e
        collect (skip skipLets expr)

    /// Every `<varName>.<field>` PropAcc in the expression. Skips PropAccs
    /// whose source isn't a bare Var (so `r.x.y` would yield `(r, x)` only).
    let collectVarPropAccs (expr: Expr<'v>) : (string * string) list =
        let out = ResizeArray<string * string>()
        let rec walk (e: Expr<'v>) =
            match e with
            | Expr.Lit _ | Expr.Var _ -> ()
            | Expr.App x -> walk x.func; walk x.arg
            | Expr.Fun x -> walk x.body
            | Expr.Let x -> walk x.value; walk x.body
            | Expr.Do x -> walk x.action; walk x.body
            | Expr.PropAcc x ->
                match x.source with
                | Expr.Var v -> out.Add (v.ident, x.ident.identName)
                | _ -> walk x.source
            | Expr.MkArray x ->
                for v in x.values do walk v
            | Expr.MkRecord x ->
                for item in x.items do
                    match item with
                    | RecordItem.Property f -> walk f.value
                    | RecordItem.Positional v -> walk v
            | Expr.Match x ->
                walk x.scrutinee
                for arm in x.arms do walk arm.body
        walk expr
        List.ofSeq out

