namespace TypeFighter

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
        $"( {allParts} )"
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
        $"<{printedVars}>.{ShowTyp.Show typ.monoTyp}"
    
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
            $"( {parts} )"
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
type UnificationError(source: Expr<_>, t1: MonoTyp, t2: MonoTyp, reason: string option) =
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

    // Canonically renumbers bound TVars (0, 1, …) so alpha-equivalent
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
                        |> List.map renameMono }
            let canonicalVars = renaming |> Map.toSeq |> Seq.map snd |> Set.ofSeq
            Poly { vars = canonicalVars; monoTyp = renameMono typ }

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
        Poly { vars = vars; monoTyp = monoTyp }
    
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

    ShowTyp.AddPrinter(fun typ ->
        match typ with
        | t when t = boolean -> Some Names.bool
        | _ -> None
    )

module BuiltinValues =
    let [<Literal>] unitValueIdent = "UnitValue"
    let [<Literal>] toStringFunctionIdent = "ToString"

module TypeSystem =
    open TypeFighter.Utils

    /// A constraint represents a relation between types that the solver must satisfy.
    /// Each variant is a different relation; the solver dispatches over them.
    type ConstraintKind =
        | CEq of MonoTyp * MonoTyp
        | CHasField of {| row: MonoTyp; fname: string; ftyp: MonoTyp |}
        | CHasMember of {| row: MonoTyp; memberTyp: MonoTyp |}

    type Constraint =
        { triviaSource: Expr<VarNum>; kind: ConstraintKind }

    type Substitution = { tvar: VarNum; typ: Typ }
    type MonoSubstitution = { tvar: VarNum; monoTyp: MonoTyp }
    type SubstThis = SubstThis of MonoTyp
    type SubstWith = SubstWith of MonoTyp
    type SubstIn = SubstIn of MonoTyp

    type RecordRefs = Map<int, Set<FieldDefinition>>
    type MemberRefs = Map<int, Set<MonoTyp>>

    type SolverRun =
        {
            cycle: int
            constraints: Constraint list
            recordRefs: RecordRefs
            memberRefs: MemberRefs
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

        let rec generateConstraints (env: Env) (expr: Expr<VarNum>) =

            let appendConstraint (tvar: VarNum) (typ: MonoTyp) =
                trace.AppendLine($"{expr}    :::     {TVar tvar} := {typ}") |> ignore
                do constraints.Append({ triviaSource = expr; kind = CEq (TVar tvar, typ) })

            let inst (typ: Typ) =
                match typ with
                | Mono mono ->
                    mono
                | Poly poly ->
                    let rec substPoly remainingVars typ =
                        match remainingVars with
                        | [] -> typ
                        | v :: remainingVars ->
                            let substedTyp = substVarWithTypInTyp v (SubstWith (TVar (newVar ()))) (SubstIn typ)
                            substPoly remainingVars substedTyp
                    substPoly (poly.vars |> Set.toList) poly.monoTyp

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
                generateConstraints env x.func
                generateConstraints env x.arg
                appendConstraint x.func.TVar (FunTyp (TVar x.arg.TVar, TVar x.tvar))

            | Expr.Fun x ->
                (*
                    S:fun ident -> body 
                    P: ident:t1, body:t2
                    C: expr: t1 -> t2
                *)
                generateConstraints 
                    (env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar))
                    x.body

                appendConstraint x.tvar (FunTyp (TVar x.ident.tvar, TVar x.body.TVar))
            
            | Expr.Let x ->
                (*
                    S: let ident = value in body
                    P: value:t1, body:t2
                    C: expr:t2
                *)
                generateConstraints env x.value

                appendConstraint x.ident.tvar (TVar x.value.TVar)
                appendConstraint x.tvar (TVar x.body.TVar)

                generateConstraints
                    (env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar))
                    x.body

            | Expr.Do x ->
                (*  
                    S: do action in body
                    P: action:unit, body:t2
                    C: expr:t2
                *)
                generateConstraints env x.body
                generateConstraints env x.action

                appendConstraint x.tvar (TVar x.body.TVar)
                appendConstraint x.action.TVar BuiltinTypes.unit

            | Expr.PropAcc x ->
                (*
                    S: source.ident
                    P: source:t1, t1 is a record in the ctx, t1 has field "ident", ident:t2
                    C: expr:t2
                *)

                generateConstraints env x.source

                do constraints.Append
                    { triviaSource = expr
                      kind = CHasField {| row = TVar x.source.TVar
                                          fname = x.ident.identName
                                          ftyp = TVar x.ident.tvar |} }

                appendConstraint x.tvar (TVar x.ident.tvar)

            | Expr.MkArray x ->
                let elemTyp = TVar (newVar ())

                for v in x.values do
                    appendConstraint v.TVar elemTyp
                    generateConstraints env v

                appendConstraint x.tvar (BuiltinTypes.array elemTyp)

            | Expr.MkRecord x ->
                for item in x.items do
                    generateConstraints env item.Value

                // Named items → named row, positionals → second row.
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
                        { fields = set [ for n, t in namedFields -> { fname = n; typ = t } ]
                          positionals = positionals })

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
                generateConstraints env x.scrutinee
                let scrutineeTyp = TVar x.scrutinee.TVar
                for a in x.arms do
                    let armEnv =
                        match a.pattern with
                        | MatchPattern.Literal l ->
                            do constraints.Append
                                { triviaSource = expr
                                  kind = CHasMember {|
                                      row = scrutineeTyp
                                      memberTyp = LiteralTyp l.value |} }
                            appendConstraint l.tvar (LiteralTyp l.value)
                            env
                        | MatchPattern.Var ident ->
                            // Catch-all binder: bind `ident` to the
                            // scrutinee's type within this arm's body.
                            appendConstraint ident.tvar scrutineeTyp
                            env |> Map.add ident.identName (EnvItem.Internal ident.tvar)
                        | MatchPattern.Wildcard w ->
                            appendConstraint w.tvar scrutineeTyp
                            env
                    generateConstraints armEnv a.body
                    appendConstraint x.tvar (TVar a.body.TVar)

        do generateConstraints env expr

        constraints.Values, exprToEnv, trace.ToString()

    /// Closes row vars that accumulated CHasField / CHasMember constraints
    /// but never got pinned: into RecordTyp and UnionTyp respectively.
    let finalizeSubstitutions (substitutions: MonoSubstitution list) (pendingFields: RecordRefs) (pendingMembers: MemberRefs) =
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
            let closed = pendingMembers |> Map.toList |> List.fold closeUnion withRecords
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
                | [] -> substitutions, pendingFields, pendingMembers
                | c :: constraints ->
                    let mutable constraints = constraints
                    let mutable substitutions = substitutions
                    let nextConstraints = Mutable.fifo None
                    let pendingFields = Mutable.oneToMany (Some pendingFields)
                    let pendingMembers = Mutable.oneToMany (Some pendingMembers)

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
                                    { triviaSource = c.triviaSource
                                      kind = CEq (req.ftyp, f.typ) }
                            | _ -> ()
                            let updated = Set.add { fname = req.fname; typ = req.ftyp } current
                            do pendingFields.Replace(rowN, updated)
                            for c' in constraints do
                                do nextConstraints.Append c'

                        | RecordTyp record ->
                            // Row is already a concrete record — check field exists and emit CEq.
                            let found =
                                record.fields
                                |> Set.toSeq
                                |> Seq.tryFind (fun f -> f.fname = req.fname)
                            match found with
                            | Some f ->
                                nextConstraints.Append
                                    { triviaSource = c.triviaSource
                                      kind = CEq (req.ftyp, f.typ) }
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

                        // `LiteralTyp 5 ∈ Number`, etc. — literals belong
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
                                            { triviaSource = c.triviaSource
                                              kind = CEq (f1.typ, f2.typ) }
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
                                        { triviaSource = c.triviaSource
                                          kind = CHasMember {| row = right; memberTyp = m |} }
                                do pendingMembers.Remove(leftN)

                        // Rewrite remaining constraints with the substitution.
                        for c' in constraints do
                            do nextConstraints.Append
                                { triviaSource = c'.triviaSource
                                  kind = applySubstToKind left right c'.kind }

                        for r in pendingFields.Values do
                            let newFields =
                                [ for f in r.Value ->
                                    { f with typ = substVarWithTypInTyp left (SubstWith right) (SubstIn f.typ) } ]
                            do pendingFields.Replace(r.Key, newFields)

                        for r in pendingMembers.Values do
                            let newMembers =
                                set [ for m in r.Value -> substVarWithTypInTyp left (SubstWith right) (SubstIn m) ]
                            do pendingMembers.Replace(r.Key, newMembers)

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

                    solve nextConstraints.Values pendingFields.Values pendingMembers.Values substitutions
            else
                substitutions, pendingFields, pendingMembers

        let result =
            try
                let substitutions, pendingFields, pendingMembers = solve constraints Map.empty Map.empty []
                Ok (finalizeSubstitutions substitutions pendingFields pendingMembers)
            with ex ->
                Error ex.Message

        {|
            substitutions = result
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
            solverRuns: list<TypeSystem.SolverRun>
            exprToEnv: Map<Expr<VarNum>, Env>
            trace: string
        }

    let solve (env: (string * Typ) list) maxSolverRuns (expr: Expr<Unit>) =
        let newVar = NumGen.mkGenerator()

        let expr = Expr.toNumberedExpr expr newVar

        // TODO: In Env, it should be disallowed having unquantified TVars
        
        // reindex all TVars in env and expr
        let env =
            [
                let varOffset = Expr.maxVar expr
                for ident, typ in env do
                    let rec reindexedVarNums typ = 
                        match typ with
                        | Mono typ -> Mono (reindexMono typ)
                        | Poly poly ->
                            let vars = [ for (VarNum v) in poly.vars do VarNum (v + varOffset) ]
                            let monoTyp = reindexMono poly.monoTyp
                            Poly { vars = vars |> Set.ofList; monoTyp = monoTyp }
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
            solverRuns = sr.solverRuns
            exprToEnv = exprToEnv
            trace = trace
        }
