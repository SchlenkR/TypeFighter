namespace TypeFighter.Lang

(*
    - we only allow top-level parametric polymorphism
    - we don not generalize on let bindings
    - generalization can only happen outside (when passing env)
    - no soundness- and completeness-proofs yet
*)


type VarNum = VarNum of int
    with override this.ToString() = let (VarNum v) = this in $"tv_{v}"

module NumGen =
    let mkGenerator() =
        let mutable currVar = -1
        fun () ->
            currVar <- currVar + 1
            VarNum currVar

[<CustomEquality; CustomComparison; RequireQualifiedAccess>]
type NameHint =
    | Anonymous
    | Given of string
    override this.Equals(other) =
        match other with :? NameHint -> true | _ -> false
    override this.GetHashCode() = hash this
    interface System.IComparable with
        member _.CompareTo(other) = match other with :? NameHint -> 0 | _ -> -1
    interface System.IComparable<NameHint> with
        member _.CompareTo(_) = 0

// TODO: Set<TField> and Set<TCase> are not well-suited for the concrete constraints these types represent.

// This is not a type in the final sense, but a constraint.
// Some of the elements may not even appear in the final type
// (e.g. TVar, TRequireFields).
// TODO: Map to a final type.
type MonoTyp =
    | TVar of VarNum
    | LeafTyp of {| name: string; args: MonoTyp list |}
    | FunTyp of MonoTyp * MonoTyp
    | RecordTyp of RecordDefinition
    | IntersectionTyp of RecordDefinition list
    | RecordRefTyp of int
    | DiscriminatedUnionTyp of DiscriminatedUnionDefinition
        override this.ToString() = ShowTyp.Show(this)
and PolyTyp =
    { vars: Set<VarNum>; monoTyp: MonoTyp }
        override this.ToString() = ShowTyp.Show(this)
and Typ =
    | Mono of MonoTyp
    | Poly of PolyTyp
        override this.ToString() = ShowTyp.Show(this)
and RecordDefinition = 
    { nameHint: NameHint; fields: Set<FieldDefinition> }
        override this.ToString() = ShowTyp.Show(this)
and FieldDefinition =
    { fname: string; typ: MonoTyp }
        override this.ToString() = ShowTyp.Show(this)
and DiscriminatedUnionDefinition = 
    {
        nameHint: NameHint
        cases: Set<{| disc: string; payloadTyp: MonoTyp option |}>
    }

and ShowTyp =
    static let printers = ResizeArray()
    static let getNameHint nameHint =
        match nameHint with
        | NameHint.Anonymous -> ""
        | NameHint.Given name -> $" (name={name})"
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
    static member Show (nameHint: string, fields: Set<FieldDefinition>) =
        fields
        |> Set.map ShowTyp.Show
        |> String.concat "; "
        |> fun s -> $"{{{nameHint} {s} }}"
    static member Show (record: RecordDefinition) =
        ShowTyp.Show(getNameHint record.nameHint, record.fields)
    static member Show (typ: MonoTyp) =
        printOrShow typ (fun () ->
            match typ with
            | TVar x -> x.ToString()
            | LeafTyp x ->
                match x.args with
                | [] -> x.name
                | args ->
                    let printedArgs = [ for a in args -> ShowTyp.Show a ] |> String.concat ", "
                    $"{x.name}<{printedArgs}>"
            | FunTyp (t1, t2) -> $"({t1} -> {t2})"
            | RecordTyp record -> ShowTyp.Show record
            | IntersectionTyp records ->
                records
                |> List.map ShowTyp.Show
                |> String.concat " & "
            | RecordRefTyp recref ->
                $"recordRef_({recref})"
            | DiscriminatedUnionTyp union ->
                [
                    for c in union.cases do
                        let payload = c.payloadTyp |> Option.map (fun t -> $"({t})") |> Option.defaultValue "_"
                        $"{c.disc}: {payload}"
                ]
                |> String.concat "; "
                |> fun s -> 
                    $"{{{getNameHint union.nameHint} {s} }}"
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

type Literal = 
    | Number of float 
    | String of string 
    | Boolean of bool

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
    | Match of {| expr: Expr<'noneOrVarnum>; cases: UnionCase<'noneOrVarnum> list; tvar: 'noneOrVarnum |}                         // match expr with | cases
    | PropAcc of {| source: Expr<'noneOrVarnum>; ident: Ident<'noneOrVarnum>; tvar: 'noneOrVarnum |}
    | MkArray of {| values: Expr<'noneOrVarnum> list; tvar: 'noneOrVarnum |}
    | MkRecord of {| fields: Field<'noneOrVarnum> list; tvar: 'noneOrVarnum |}
    
    member this.TVar =
        match this with
        | Lit x -> x.tvar
        | Var x -> x.tvar
        | App x -> x.tvar
        | Fun x -> x.tvar
        | Let x -> x.tvar
        | Do x -> x.tvar
        | Match x -> x.tvar
        | PropAcc x -> x.tvar
        | MkArray x -> x.tvar
        | MkRecord x -> x.tvar
    override this.ToString() =
        ShowExpr.Expr(this)

and Ident<'noneOrVarnum> = internal { identName: string; tvar: 'noneOrVarnum }
and Field<'noneOrVarnum> = internal { fname: string; value: Expr<'noneOrVarnum>; tvar: 'noneOrVarnum }
and UnionCase<'noneOrVarnum> = internal { disc: string; ident: Ident<'noneOrVarnum> option; body: Expr<'noneOrVarnum> }

and ShowExpr =
    static member Expr (expr: Expr<'noneOrVarnum>) =
        let printIdent (ident: Ident<'noneOrVarnum>) = ident.identName
        let printField (f: Field<'noneOrVarnum>) = $"{f.fname}: {f.value}"
        let printUnionCase (c: UnionCase<'noneOrVarnum>) =
            let binding =
                match c.ident with
                | Some ident -> $"as {printIdent ident}"
                | None -> $""
            $"    | {c.disc} {binding}-> {c.body}"
        match expr with
        | Expr.Lit x -> $"Lit {x.value}"
        | Expr.Var x -> $"Var {x.ident}"
        | Expr.App x -> $"App {x.func} ..."
        | Expr.Fun x -> $"Fun {printIdent x.ident} -> (... {x.body.TVar})"
        | Expr.Let x -> $"Let {printIdent x.ident} = ({x.value}) in {x.body}"
        | Expr.Do x -> $"Do {x.action} {x.body}"
        | Expr.Match x ->
            let caseNames = [ for x in x.cases -> printUnionCase x ] |> String.concat " | "
            $"Match {x.expr} with | {caseNames})"
        | Expr.PropAcc x -> $"PropAcc {x.source}.{x.ident}"
        | Expr.MkArray x ->
            let values = [ for x in x.values -> ShowExpr.Expr x ] |> String.concat "; "
            $"MkArray [ {values} ]"
        | Expr.MkRecord x ->
            let fieldNames = 
                x.fields 
                |> List.map printField
                |> String.concat "; " 
                |> sprintf "{ %s }"
            $"{{ {fieldNames} }}"

type X =
    static member Ident value = { identName = value; tvar = () }
    static member Lit(value: string) = Expr.Lit {| value = String value; tvar = () |}
    static member Lit(value: int) = Expr.Lit {| value = Number value; tvar = () |}
    static member Lit(value: float) = Expr.Lit {| value = Number value; tvar = () |}
    static member Lit(value: bool) = Expr.Lit {| value = Boolean value; tvar = () |}
    static member Var ident = Expr.Var {| ident = ident; tvar = () |}
    static member App func arg = Expr.App {| func = func; arg = arg; tvar = () |}
    static member Fun ident body = Expr.Fun {| ident = ident; body = body; tvar = () |}
    static member Let ident value body = Expr.Let {| ident = ident; value = value; body = body; tvar = () |}
    static member Do action body = Expr.Do {| action = action; body = body; tvar = () |}
    static member Match expr cases = Expr.Match {| expr = expr; cases = cases; tvar = () |}
    static member PropAcc source ident = Expr.PropAcc {| source = source; ident = { identName = ident; tvar = () } ; tvar = () |}
    static member PropAccN segments =
        match segments with
        | [] -> failwith "At least one segment required."
        | x :: xs ->
            let source = X.Var x
            let rec loop source segments =
                match segments with
                | [] -> source
                | x :: xs -> loop (X.PropAcc source x) xs
            loop source xs
    static member MkArray values = Expr.MkArray {| values = values; tvar = ()  |}
    static member MkRecord fields = Expr.MkRecord {| fields = fields; tvar = ()  |}
    static member Field field value = { fname = field; value = value; tvar = () }
    static member Case disc ident body = { disc = disc; ident = ident; body = body }

// TODO: Replace this (and throwing) with a "TError"
type UnificationError(source: Expr<_>, t1: MonoTyp, t2: MonoTyp, reason: string option) =
    inherit System.Exception(
        [
            $"-----------------"
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
            | Expr.Match x -> 
                Expr.Match 
                    {|
                        expr = loop x.expr; 
                        cases = [ 
                            for c in x.cases do 
                                { 
                                    disc = c.disc
                                    ident = Option.map (fun i -> { identName = i.identName; tvar = newVar () }) c.ident
                                    body = loop c.body }
                            ]
                        tvar = newVar ()
                    |}
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
                        fields = [ 
                            for f in x.fields do 
                                { 
                                    fname = f.fname
                                    value = loop f.value
                                    tvar = newVar ()
                                } 
                            ]
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
            | Expr.Match x -> 
                [
                    yield x.tvar
                    yield! loop x.expr acc
                    for c in x.cases do
                        yield! loop c.body acc
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
                    for f in x.fields do
                        yield! loop f.value acc
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
            ]
        and loopMono (typ: MonoTyp) (acc: VarNum list) =
            match typ with
            | TVar x -> x :: acc
            | LeafTyp x ->
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
            | IntersectionTyp records ->
                [
                    for r in records do
                        yield! loopRecord r acc
                ]
            | RecordRefTyp _ -> []
            | DiscriminatedUnionTyp union ->
                [
                    for c in union.cases do
                        match c.payloadTyp with
                        | Some payloadTyp -> yield! loopMono payloadTyp acc
                        | None -> ()
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

    let gen (typ: MonoTyp) =
        let tvars = collectTVars (Mono typ) |> List.map (fun v -> VarNum v)
        match tvars with
        | [] -> Mono typ
        | tvars -> Poly { vars = set tvars; monoTyp = typ }

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
    
    let RecordDefWith nameHint (fields: (string * MonoTyp) list) =
        let fields =
            [ for (fname, typ) in fields do { fname = fname; typ = typ } ]
            |> set
        { nameHint = nameHint; fields = fields }
    
    let NamedRecordWith nameHint (fields: (string * MonoTyp) list) =
        RecordTyp (RecordDefWith nameHint fields)
    
    let RecordWith (fields: (string * MonoTyp) list) =
        RecordTyp (RecordDefWith NameHint.Anonymous fields)

    let DiscriminatedUnionWith nameHint (cases: (string * MonoTyp option) list) =
        let cases =
            [ for (disc, payloadTyp) in cases do {| disc = disc; payloadTyp = payloadTyp |} ]
            |> set
        DiscriminatedUnionTyp { nameHint = nameHint; cases = cases }
    
    let LeafWith (name: string) (args: MonoTyp list) =
        LeafTyp {| name = name; args = args |}

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

    let unit = TDef.LeafWith Names.unit []
    let boolean = TDef.DiscriminatedUnionWith (NameHint.Given Names.bool) [ "True", None; "False", None ]
    let number = TDef.LeafWith Names.number []
    let string = TDef.LeafWith Names.string []
    
    let array elemTyp = TDef.LeafWith Names.array [ elemTyp ]

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

    /// A constraint in the form: the right type must be assignable to the left type    
    type Constraint = { triviaSource: Expr<VarNum>; t1: MonoTyp; t2: MonoTyp }

    type SolutionItem = { tvar: VarNum; typ: Typ }
    type MSolutionItem = { tvar: VarNum; monoTyp: MonoTyp }
    type SubstThis = SubstThis of MonoTyp
    type SubstWith = SubstWith of MonoTyp
    type SubstIn = SubstIn of MonoTyp

    type RecordRefs = Map<int, Set<FieldDefinition>>

    type SolverRun =
        {
            cycle: int
            constraints: Constraint list
            recordRefs: RecordRefs
            solutionItems: MSolutionItem list
        }

    let rec substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn inTyp) =
        let substRecord record =
            let fields = [ for f in record.fields do { f with typ = substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn f.typ) }]
            { record with fields = set fields }
        match inTyp with
        | t when t = typToReplace -> withTyp
        | TVar _ -> inTyp
        | LeafTyp app ->
            let substitutedArgs = [ for arg in app.args do substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn arg) ]
            LeafTyp {| app with args = substitutedArgs |}
        | FunTyp (t1, t2) ->
            let t1 = substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn t1)
            let t2 = substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn t2)
            FunTyp (t1, t2)
        | RecordTyp record -> 
            RecordTyp (substRecord record)
        | IntersectionTyp records ->
            IntersectionTyp [ for record in records do substRecord record ]
        | RecordRefTyp recref ->
            RecordRefTyp recref
        | DiscriminatedUnionTyp union ->
            TDef.DiscriminatedUnionWith
                union.nameHint
                [ for c in union.cases do 
                    c.disc, 
                    c.payloadTyp |> Option.map (fun inTyp -> substTypWithTypInTyp (SubstThis typToReplace) (SubstWith withTyp) (SubstIn inTyp)) 
                ]
    
    let rec substVarWithTypInTyp (tvarToReplace: VarNum) withTyp inTyp =
        substTypWithTypInTyp (SubstThis (TVar tvarToReplace)) withTyp inTyp

    let generateConstraints (env: Env) (expr: Expr<VarNum>) newVar =
        let constraints = Mutable.fifo None
        let appendConstraint (triviaSource: Expr<VarNum>) (tvar: VarNum) (typ: MonoTyp) =
            do constraints.Append({ triviaSource = triviaSource; t1 = TVar tvar; t2 = typ })

        let recordRefs = Mutable.oneToMany None

        let mutable exprToEnv = Map.empty
        let addEnv (expr: Expr<VarNum>) (env: Env) =
            do exprToEnv <- exprToEnv |> Map.add expr env
        
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

        let rec generateConstraints (env: Env) (expr: Expr<VarNum>) =
            do addEnv expr env
            match expr with
            | Expr.Lit x ->
                let litTyp =
                    match x.value with
                    | Boolean _ -> BuiltinTypes.boolean
                    | Number _ -> BuiltinTypes.number
                    | String _ -> BuiltinTypes.string
                appendConstraint expr x.tvar litTyp
            | Expr.Var x ->
                (*
                    S: ident
                    P: env has ident:t
                    C: expr:t
                *)
                let resolvedIdent =
                    match env |> Map.tryFind x.ident with
                    | Some (EnvItem.Internal tvar) -> TVar tvar
                    | Some (EnvItem.External typ) ->
                        // INST - here's where it happens :)
                        inst typ
                    | None -> failwith $"Unresolved identifier: {x.ident}"
                appendConstraint expr x.tvar resolvedIdent
            | Expr.App x ->
                (*
                    S: func arg
                    P: func: t1 -> t2, arg:t1
                    C: expr:t2
                *)
                generateConstraints env x.arg
                generateConstraints env x.func

                appendConstraint expr x.func.TVar (FunTyp (TVar x.arg.TVar, TVar x.tvar))
            | Expr.Fun x ->
                (*
                    S:fun ident -> body 
                    P: ident:t1, body:t2
                    C: expr: t1 -> t2
                *)
                generateConstraints 
                    (env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar))
                    x.body

                appendConstraint expr x.tvar (FunTyp (TVar x.ident.tvar, TVar x.body.TVar))
            | Expr.Let x ->
                (*
                    S: let ident = value in body
                    P: value:t1, body:t2
                    C: expr:t2
                *)
                generateConstraints env x.value

                appendConstraint expr x.ident.tvar (TVar x.value.TVar)
                appendConstraint expr x.tvar (TVar x.body.TVar)

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

                appendConstraint expr x.tvar (TVar x.body.TVar)
                appendConstraint expr x.action.TVar BuiltinTypes.unit
            | Expr.PropAcc x ->
                (*
                    S: source.ident
                    P: source:t1, t1 is a record in the ctx, t1 has field "ident", ident:t2
                    C: expr:t2
                *)

                generateConstraints env x.source

                let (VarNum recref) = x.source.TVar
                recordRefs.Add(recref, { fname = x.ident.identName; typ = TVar x.ident.tvar })
                appendConstraint expr x.source.TVar (RecordRefTyp recref)

                appendConstraint expr x.tvar (TVar x.ident.tvar)
            | Expr.MkArray x ->
                let elemTyp = TVar (newVar ())

                for v in x.values do
                    appendConstraint expr v.TVar elemTyp
                    generateConstraints env v

                appendConstraint expr x.tvar (BuiltinTypes.array elemTyp)
            | Expr.MkRecord x ->
                for f in x.fields do
                    generateConstraints env f.value

                // TODO: field names must be distinct
                let fields =
                    x.fields
                    |> List.sortBy _.fname
                    |> List.map (fun f -> f.fname, TVar f.value.TVar)
                appendConstraint expr x.tvar (TDef.RecordWith fields)
            | Expr.Match x ->
                match x.cases with
                | [] -> failwith $"Match expression must have at least one case."
                | firstCase :: otherCases ->
                    for c in x.cases do
                        generateConstraints 
                            (
                                match c.ident with
                                | Some ident -> env |> Map.add ident.identName (EnvItem.Internal ident.tvar)
                                | None -> env
                            )
                            c.body

                        // the type of the case body must be the same as the type of the value expr
                        appendConstraint expr c.body.TVar (TVar firstCase.body.TVar)

                    appendConstraint expr x.tvar (TVar firstCase.body.TVar)

                generateConstraints env x.expr
                
                // match is exhaustive:
                // the type of the value expr that gets matched is a union type of all cases
                appendConstraint expr x.expr.TVar (TDef.DiscriminatedUnionWith NameHint.Anonymous [ for c in x.cases -> c.disc, None ])

        do generateConstraints env expr

        constraints.Values, exprToEnv, recordRefs.Values

    let finalizeSolution (solution: MSolutionItem list) (recordRefs: RecordRefs) =
        let rec substRecordRefInSolution solution remainingRecordRefs =
            match remainingRecordRefs with
            | [] -> solution
            | (recref, fields) :: remainingRecordRefs ->
                let rec substRecordRefInSolutionItem solution =
                    match solution with
                    | [] -> []
                    | s :: solution ->
                        let recordInfo = { nameHint = NameHint.Given $"RECORD_{recref}"; fields = fields }
                        let substTyp = substTypWithTypInTyp (SubstThis (RecordRefTyp recref)) (SubstWith (RecordTyp recordInfo)) (SubstIn s.monoTyp)
                        { s with monoTyp = substTyp } :: substRecordRefInSolutionItem solution
                substRecordRefInSolution (substRecordRefInSolutionItem solution) remainingRecordRefs
        [
            // TODO: Again, we need a kind-of "FinylTyp"
            let noMoreRecordRefsHere = substRecordRefInSolution solution (Map.toList recordRefs)
            for s in noMoreRecordRefsHere do
                { tvar = s.tvar; typ = Typ.gen s.monoTyp } 
        ]
                
    let solveConstraints (constraints: Constraint list) (recordRefs: RecordRefs) =
        let mutable solverRuns = []
   
        let rec solve (constraints: Constraint list) (recordRefs: RecordRefs) (solutionItems: MSolutionItem list) =
            do solverRuns <- 
                [
                    yield! solverRuns
                    yield { 
                        cycle = solverRuns.Length
                        constraints = constraints
                        recordRefs = recordRefs
                        solutionItems = solutionItems }
                ]

            match constraints with
            | [] -> solutionItems,recordRefs
            | c :: constraints ->
                let mutable constraints = constraints
                let mutable solutionItems = solutionItems
                let nextConstraints = Mutable.fifo None
                let recordRefs = Mutable.oneToMany (Some recordRefs)

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

                    match t1,t2 with
                    | t1,t2 when t1 = t2 -> ()
                    | t, TVar tvar
                    | TVar tvar, t -> nextConstraints.Append({ triviaSource = source; t1 = TVar tvar; t2 = t })
                    | LeafTyp app1, LeafTyp app2 when app1.name = app2.name ->
                        let rec loop funArgs1 funArgs2 =
                            match funArgs1, funArgs2 with
                            | [], [] -> ()
                            | a1 :: args1, a2 :: args2 ->
                                do loop args1 args2
                                do unifyTypes source a1 a2
                            | [], args
                            | args, [] -> throwUniError "Type parameters count mismatch."
                        do loop app1.args app2.args
                    | FunTyp (ta, tb), FunTyp (tc, td) ->
                        do unifyTypes source ta tc
                        do unifyTypes source tb td
                    | RecordRefTyp recref, RecordTyp providedRecord
                    | RecordTyp providedRecord, RecordRefTyp recref ->
                        let requiredFields = recordRefs.Find(recref)
                        do unifyRecordFields requiredFields providedRecord.fields
                    | RecordTyp _, IntersectionTyp _
                    | IntersectionTyp _, RecordTyp _ ->
                        failwith "TRecord and TIntersection: We need to tweak that, too."
                        // [
                        //     for providedRecord in providedRecords do
                        //         yield! unifyRecordFields requiredFields.fields providedRecord.fields
                        // ]
                    | RecordRefTyp recref1, RecordRefTyp recref2 ->
                        let fields1 = recordRefs.Find(recref1)
                        let fields2 = recordRefs.Find(recref2)

                        // unify the fields by name of of both records
                        let groupedFieldsByName =
                            [
                                yield! recordRefs.Find(recref1)
                                yield! recordRefs.Find(recref2)
                            ]
                            |> List.groupBy _.fname
                        let unifyableFields = groupedFieldsByName |> List.choose (fun (fname, fields) ->
                            match fields with
                            | [f1;f2] -> Some (fname, f1, f2)
                            | _ -> None)
                        for fname, f1, f2 in unifyableFields do
                            do unifyTypes source f1.typ f2.typ

                        // merge the fields of both records
                        let mergedFields = set [ yield! fields1; yield! fields2 ]
                        do recordRefs.Remove(recref1)
                        do recordRefs.Replace(recref2, mergedFields)

                        // Now, we also have to reset all references currently pointing to recref1 so that they point to recref2
                        // in all constraints and solutions
                        do constraints <-
                            [
                                for c in constraints do
                                    {
                                        triviaSource = c.triviaSource
                                        t1 = substTypWithTypInTyp (SubstThis (RecordRefTyp recref1)) (SubstWith (RecordRefTyp recref2)) (SubstIn c.t1)
                                        t2 = substTypWithTypInTyp (SubstThis (RecordRefTyp recref1)) (SubstWith (RecordRefTyp recref2)) (SubstIn c.t2)
                                    }
                            ]
                        do solutionItems <-
                            [
                                for s in solutionItems do
                                    {
                                        tvar = s.tvar
                                        monoTyp = substTypWithTypInTyp (SubstThis (RecordRefTyp recref1)) (SubstWith (RecordRefTyp recref2)) (SubstIn s.monoTyp)
                                    }
                            ]
                    | RecordTyp rec1, RecordTyp rec2 ->
                        unifyRecordFields rec1.fields rec2.fields
                    | t1, t2 ->
                        let getCaseLabel (du: 'T) =
                            let case, _ = Reflection.FSharpValue.GetUnionFields(du, typeof<'T>)
                            case.Name
                        throwUniError $"Unification cases are not handled. T1: {getCaseLabel t1}, T2: {getCaseLabel t2}"

                // we try to keep only the constraints mutable, because we could otherwise easily forget
                // adding elements in all branches (this happened right now, and it was a pain to find out).
                match c.t1, c.t2 with
                | TVar left, right ->
                    // replace tvar with t in all other constraints
                    // replace tvar with t in all solutions
                    // add a new solution to solutions
                    // continue to solve
                    for c in constraints do
                        do nextConstraints.Append
                            {
                                triviaSource = c.triviaSource //if c.t1 = otherC.t1 then otherC.source else c.source
                                t1 = substVarWithTypInTyp left (SubstWith right) (SubstIn c.t1)
                                t2 = substVarWithTypInTyp left (SubstWith right) (SubstIn c.t2)
                            }

                    for r in recordRefs.Values do
                        let newFields =
                            [ 
                                for f in r.Value do
                                    { f with typ = substVarWithTypInTyp left (SubstWith right) (SubstIn f.typ) }
                            ]
                        do recordRefs.Replace(r.Key, newFields)

                    do solutionItems <-
                        [
                            { tvar = left; monoTyp = right }
                            for s in solutionItems do
                                {
                                    tvar = s.tvar
                                    monoTyp = substVarWithTypInTyp left (SubstWith right) (SubstIn s.monoTyp)
                                }
                        ]

                | left, right ->
                    // unify left and right
                    // add new constraints to constraints
                    // continue to solve
                    do unifyTypes c.triviaSource left right
                    for c in constraints do
                        do nextConstraints.Append(c)
                    for r in recordRefs.Values do
                        do recordRefs.Replace(r.Key, r.Value)
                    do solutionItems <- solutionItems

                solve nextConstraints.Values recordRefs.Values solutionItems

        let solution =
            try 
                let solutionItems,recordRefs = solve constraints recordRefs []
                let finalizedSolution = finalizeSolution solutionItems recordRefs
                Ok finalizedSolution
            with ex -> Error ex.Message
        
        {|
            solution = solution
            solverRuns = solverRuns
        |}

module Services =

    type SolveResult = 
        {
            numberedExpr: Expr<VarNum>
            result: 
                Result<
                    {|
                        solution: TypeSystem.SolutionItem list
                        finalTyp: Typ
                    |},
                    string
                >
            solverRuns: list<TypeSystem.SolverRun>
            exprToEnv: Map<Expr<VarNum>, Env>
        }

    let solve (env: (string * Typ) list) (expr: Expr<Unit>) =
        let newVar = NumGen.mkGenerator()

        let expr = Expr.toNumberedExpr expr newVar

        // TODO: In Env, it should be disallowed having unquantified TVars
        
        // reindex all TVars in env and expr
        let env =
            [
                let varOffset = Expr.maxVar expr
                for (ident, typ) in env do
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
                            { record with fields = fields |> Set.ofList }
                        match typ with
                        | TVar (VarNum n) ->
                            TVar (VarNum (n + varOffset))
                        | LeafTyp app -> 
                            LeafTyp {| app with args = app.args |> List.map reindexMono |}
                        | FunTyp (t1, t2) -> 
                            FunTyp (reindexMono t1, reindexMono t2)
                        | RecordRefTyp _ ->
                            failwith "Record references should not appear in the external environment (see that 'FinalTyp' comments somewhere)."
                        | IntersectionTyp records -> 
                            IntersectionTyp [ for r in records do reindexRecord r ]
                        | RecordTyp record ->
                            RecordTyp (reindexRecord record)
                        | DiscriminatedUnionTyp union -> 
                            TDef.DiscriminatedUnionWith
                                union.nameHint
                                [ for c in union.cases do c.disc, c.payloadTyp |> Option.map reindexMono ]

                    ident, EnvItem.External (reindexedVarNums typ)
            ]
            |> Map.ofList

        let constraints,exprToEnv,recordRefs = TypeSystem.generateConstraints env expr newVar
        let sr = TypeSystem.solveConstraints constraints recordRefs

        {
            numberedExpr = expr
            result =
                match sr.solution with
                | Ok solution ->
                    Ok {|
                        solution = solution
                        finalTyp = solution |> List.find (fun s -> s.tvar = expr.TVar) |> (_.typ)
                    |}
                | Error message -> Error message
            solverRuns = sr.solverRuns
            exprToEnv = exprToEnv
        }
