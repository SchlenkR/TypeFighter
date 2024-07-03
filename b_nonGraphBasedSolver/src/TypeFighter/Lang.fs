namespace TypeFighter.Lang

open TypeFighter.Utils

(*
  - we only allow top-level parametric polymorphism
  - we don not generalize on let bindings
  - generalization can only happen outside (when passing env)
  - Maybe generalizing on every "let" is overrated, and it could be
    valuable to think about onyl generalizing at some "top-level".
*)

type VarNum = VarNum of int
    with override this.ToString() = let (VarNum v) = this in $"tv_{v}"

/// Names for types are really only hints for programmers, because
/// they have no meaning for structural typing.
[<CustomEquality; CustomComparison; RequireQualifiedAccess>]
type NameHint =
    | Empty
    | Given of string
    override _.Equals(other) =
        match other with :? NameHint -> true | _ -> false
    override this.GetHashCode() = hash this
    interface System.IComparable with
        member _.CompareTo(other) = match other with :? NameHint -> 0 | _ -> -1
    interface System.IComparable<NameHint> with
        member _.CompareTo(_) = 0

type MonoTyp =
    // TVar can occur in the following contexts :
    //   1. While solving, it denotes a specific, but not yet known type.
    //   2. The mono typ occurs in a poly typ, and the variable is a quantified variable.
    // In a final typ, TVar only occurs in the context of a poly typ.
    | TVar of VarNum
    | LeafTyp of {| name: string; args: MonoTyp list |}
    | FunTyp of MonoTyp * MonoTyp
    | RecordTyp of RecordDefinition
    | OrTyp of RecordDefinition list
    | AndTyp of RecordDefinition list
    // Since we currently only have "equivalence" in unification (correct term?),
    // we hack around some needs by introducing constraints in these forms.
    // These "types" will not occur in the final type.
    // TODO: Get rid of it, and extend the unification system.
    | RequireMemberConstraint of FieldDefinition
and PolyTyp =
    { 
        vars: Set<VarNum>
        monoTyp: MonoTyp
    }
and Typ =
    | Mono of MonoTyp
    | Poly of PolyTyp
and RecordDefinition =
    { 
        nameHint: NameHint
        fields: Set<FieldDefinition>
    }
// and FieldDefinition =
//     | NamedField of NamedFieldDefinition
//     | UnnamedConstantField of MonoTyp
and FieldDefinition =
    { 
        fname: string
        typ: MonoTyp 
    }

[<RequireQualifiedAccess>]
type Expr =
    // many exprs are non-elementary, but we don't care about that here,
    // since we aim for expressiveness when working with the AST
    internal
    | Lit of {| value: string; tvar: VarNum |}                            // "foo" oder 2323
    | Var of {| ident: string; tvar: VarNum |}                            // ident
    | App of {| func: Expr; arg: Expr; tvar: VarNum |}                    // func arg
    | Fun of {| ident: Ident; body: Expr; tvar: VarNum |}                 // fun ident -> body
    | Let of {| ident: Ident; value: Expr; body: Expr; tvar: VarNum |}    // let ident = value in body
    | Do of {| value: Expr; body: Expr; tvar: VarNum |}                   // do value body
    | Match of {| expr: Expr; cases: UnionCase list; tvar: VarNum |}      // match expr with | cases
    | PropAcc of {| source: Expr; ident: Ident; tvar: VarNum |}
    | MkArray of {| values: Expr list; tvar: VarNum |}
    | MkRecord of {| fields: Field list; tvar: VarNum |}
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
and Ident = internal { identName: string; tvar: VarNum }
and Field = internal { fname: string; value: Expr; tvar: VarNum }
and UnionCase = internal { disc: string; ident: Ident option; body: Expr }

module Show =
    let getNameHint nameHint =
        match nameHint with
        | NameHint.Empty -> ""
        | NameHint.Given name -> $" (name={name})"
    let fieldDef (field: FieldDefinition) =
        $"{field.fname}: {field.typ}"
    let recordDef (record: RecordDefinition) =
        record.fields
        |> Set.map fieldDef
        |> String.concat "; "
        |> fun s -> $"{{{getNameHint record.nameHint} {s} }}"
    let rec monoTyp(typ: MonoTyp) =
        match typ with
        | TVar x -> x.ToString()
        | LeafTyp x ->
            match x.args with
            | [] -> x.name
            | args ->
                let printedArgs = [ for a in args -> monoTyp a ] |> String.concat ", "
                $"{x.name}<{printedArgs}>"
        | FunTyp (t1, t2) -> $"({t1} -> {t2})"
        | RecordTyp record -> recordDef record
        | IntersectionTyp records ->
            records 
            |> List.map recordDef
            |> String.concat " & "
        | RequireMemberConstraint f -> $"req_member({fieldDef f})"
    let polyTyp(typ: PolyTyp) =
        let printedVars = [ for v in typ.vars -> v.ToString() ] |> String.concat ", "
        $"<{printedVars}>.{monoTyp typ.monoTyp}"
    let typ (typ: Typ) =
        match typ with
        | Mono typ -> monoTyp typ
        | Poly typ -> polyTyp typ

    let rec expression (expr: Expr) =
        let printIdent (ident: Ident) = ident.identName
        let printField (f: Field) = $"{f.fname}: {f.value}"
        let printUnionCase (c: UnionCase) =
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
        | Expr.Do x -> $"Do {x.value} {x.body}"
        | Expr.Match x ->
            let caseNames = [ for x in x.cases -> printUnionCase x ] |> String.concat " | "
            $"Match {x.expr} with | {caseNames})"
        | Expr.PropAcc x -> $"PropAcc {x.source}.{x.ident}"
        | Expr.MkArray x ->
            let values = [ for x in x.values -> expression x ] |> String.concat "; "
            $"MkArray [ {values} ]"
        | Expr.MkRecord x ->
            let fieldNames = 
                x.fields 
                |> List.map printField
                |> String.concat "; " 
                |> sprintf "{ %s }"
            $"{{ {fieldNames} }}"

type Env = Map<string, EnvItem>
and [<RequireQualifiedAccess>] EnvItem =
    | Internal of VarNum 
    | External of Typ

module Expr =
    let collectTVars (expr: Expr) =
        let rec loop (expr: Expr) (acc: VarNum list) =
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
                    yield! loop x.value acc
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

    let maxVar (expr: Expr) =
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
            | RequireMemberConstraint f ->
                loopMono f.typ acc
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
type ExprCtx() =
    let mutable currVar = 0
    let newTVar () =
        currVar <- currVar + 1
        VarNum currVar
    member _.VarCount = currVar
    member _.NewTVar() = newTVar ()
    member _.Ident value = { identName = value; tvar = newTVar () }
    member _.Lit value = Expr.Lit {| value = value; tvar = newTVar ()  |}
    member _.Var ident = Expr.Var {| ident = ident; tvar = newTVar ()  |}
    member _.App func arg = Expr.App {| func = func; arg = arg; tvar = newTVar ()  |}
    member _.Fun ident body = Expr.Fun {| ident = ident; body = body; tvar = newTVar ()  |}
    member _.Let ident value body = Expr.Let {| ident = ident; value = value; body = body; tvar = newTVar ()  |}
    member _.Do value body = Expr.Do {| value = value; body = body; tvar = newTVar ()  |}
    member _.Match expr cases = Expr.Match {| expr = expr; cases = cases; tvar = newTVar ()  |}
    member _.PropAcc source ident = Expr.PropAcc {| source = source; ident = { identName = ident; tvar = newTVar () } ; tvar = newTVar ()  |}
    member t.PropAccN segments =
        match segments with
        | [] -> failwith "At least one segment required."
        | x :: xs ->
            let source = t.Var x
            let rec loop source segments =
                match segments with
                | [] -> source
                | x :: xs -> loop (t.PropAcc source x) xs
            loop source xs
    member _.MkArray values = Expr.MkArray {| values = values; tvar = newTVar ()  |}
    member _.MkRecord fields = Expr.MkRecord {| fields = fields; tvar = newTVar ()  |}
    member _.Field field value = { fname = field; value = value; tvar = newTVar () }
    member _.Case disc ident body = { disc = disc; ident = ident; body = body }

[<AutoOpen>]
module TypDefHelper =

    let ( ~% ) x = TVar (VarNum x)

    let TPoly (vars: int list) (monoTyp: MonoTyp) =
        let vars = vars |> List.map VarNum |> set
        Poly { vars = vars; monoTyp = monoTyp }

    let TFunCurr (args: MonoTyp list) =
        let rec loop (args: MonoTyp list) =
            match args with
            | [] -> failwith "At least one argument and a return type required."
            | a1 :: a2 :: [] -> FunTyp (a1, a2)
            | a1 :: args -> FunTyp (a1, loop args)
        loop args
    
    // CAREFUL HERE: -> is right-associative
    let ( ^-> ) t1 t2 = FunTyp (t1, t2)
    
    let TRecordWith nameHint (fields: (string * MonoTyp) list) =
        let fields =
            [ for (fname, typ) in fields do { fname = fname; typ = typ } ]
            |> set
        { nameHint = nameHint; fields = fields }
    
    let TProvideMembersWith nameHint (fields: (string * MonoTyp) list) =
        RecordTyp (TRecordWith nameHint fields)

    let TAppWith (name: string) (args: MonoTyp list) =
        LeafTyp {| name = name; args = args |}

    let TConst (name: string) =
        TAppWith name []

    let TGen (monoTyp: MonoTyp) =
        Typ.gen monoTyp

module BuiltinTypes =
    module Names =
        let [<Literal>] unit = "Unit"
        let [<Literal>] number = "Number"
        let [<Literal>] string = "String"
        let [<Literal>] date = "Date"
        let [<Literal>] array = "Array"
        let [<Literal>] bool = "Bool"

    let unit = TConst Names.unit
    let boolean = failwith "Not implemented"
    let number = TConst Names.number
    let string = TConst Names.string
    let date = TConst Names.date
    
    let array elemTyp = TAppWith Names.array [ elemTyp ]

module BuiltinValues =
    let [<Literal>] unitValueIdent = "UnitValue"
    let [<Literal>] toStringFunctionIdent = "ToString"


module TypeSystem =

    /// A constraint in the form: the right type must be assignable to the left type    
    type Constraint = { triviaSource: Expr; t1: MonoTyp; t2: MonoTyp }

    type SolutionItem = { tvar: VarNum; typ: Typ }
    type MSolutionItem = { tvar: VarNum; monoTyp: MonoTyp }
    
    let rec substVarInTyp (tvarToReplace: VarNum) (withTyp: MonoTyp) (inTyp: MonoTyp) =
        let substRecord record =
            let fields = [ for f in record.fields do { f with typ = substVarInTyp tvarToReplace withTyp f.typ }]
            { record with fields = set fields }
        match inTyp with
        | TVar tvar when tvar = tvarToReplace -> withTyp
        | TVar _ -> inTyp
        | LeafTyp app ->
            let substitutedArgs = [ for arg in app.args do substVarInTyp tvarToReplace withTyp arg ]
            LeafTyp {| app with args = substitutedArgs |}
        | FunTyp (t1, t2) ->
            let t1 = substVarInTyp tvarToReplace withTyp t1
            let t2 = substVarInTyp tvarToReplace withTyp t2
            FunTyp (t1, t2)
        | RecordTyp record -> 
            RecordTyp (substRecord record)
        | IntersectionTyp records ->
            IntersectionTyp [ for record in records do substRecord record ]
        | RequireMemberConstraint f ->
            RequireMemberConstraint { f with typ = substVarInTyp tvarToReplace withTyp f.typ }

    let generateConstraints (env: Env) (expr: Expr) =
        let mutable constraints = []
        let addConstraint (triviaSource: Expr) (tvar: VarNum) (typ: MonoTyp) =
            constraints <-
                { 
                    triviaSource = triviaSource
                    t1 = TVar tvar
                    t2 = typ 
                }
                :: constraints

        let mutable currVar =
            let maxEnvVarNum = Env.maxVar env
            let maxExprVarNum = Expr.maxVar expr
            max maxEnvVarNum maxExprVarNum
        let newTVar () =
            currVar <- currVar + 1
            VarNum currVar
        
        let inst (typ: Typ) =
            match typ with
            | Mono mono ->
                mono
            | Poly poly ->
                let rec substPoly remainingVars typ =
                    match remainingVars with
                    | [] -> typ
                    | v :: remainingVars ->
                        let substedTyp = substVarInTyp v (TVar (newTVar ())) typ
                        substPoly remainingVars substedTyp
                substPoly (poly.vars |> Set.toList) poly.monoTyp

        let rec generateConstraints (env: Env) (expr: Expr) =
            match expr with
            | Expr.Lit x ->
                let guessedTyp =
                    let ci = System.Globalization.CultureInfo.InvariantCulture
                    let (|Boolean|_|) (input: string) =
                        match System.Boolean.TryParse(input) with
                        | true, result -> Some result
                        | _ -> None
                    let (|Number|_|) (input: string) =
                        match System.Double.TryParse(input, ci) with
                        | true, result -> Some result
                        | _ -> None
                    let (|Date|_|) (input: string) =
                        match System.DateTime.TryParse(input, ci) with
                        | true, result -> Some result
                        | _ -> None
                    match x.value with
                    | Boolean _ -> BuiltinTypes.boolean
                    | Number _ -> BuiltinTypes.number
                    | Date _ -> BuiltinTypes.date
                    | _ -> BuiltinTypes.string
                addConstraint expr x.tvar guessedTyp
            | Expr.Var x ->
                (*
                    ident   ||   t(ident) = tvar
                *)
                let resolvedIdent =
                    match env |> Map.tryFind x.ident with
                    | Some (EnvItem.Internal tvar) -> TVar tvar
                    | Some (EnvItem.External typ) ->
                        // INST - here's where it happens :)
                        inst typ
                    | None -> failwith $"Unresolved identifier: {x.ident}"
                addConstraint expr x.tvar resolvedIdent
            | Expr.App x ->
                (*
                    func arg   ||   t(func): t1 -> t2   ||   t(arg) = t1   ||   t(app): t2
                *)
                addConstraint expr x.func.TVar (FunTyp (TVar x.arg.TVar, TVar x.tvar))

                generateConstraints env x.func
                generateConstraints env x.arg
            | Expr.Fun x ->
                (*
                    fun ident -> body   ||   t(ident) = t1   ||   t(body) = t2   ||   t(fun): t1 -> t2
                *)
                addConstraint expr x.tvar (FunTyp (TVar x.ident.tvar, TVar x.body.TVar))

                let env = env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar)
                generateConstraints env x.body
            | Expr.Let x ->
                (*
                    let ident = value in body   ||   t(value) = t1   ||   t(body) = t2   ||   t(let): t2
                *)
                addConstraint expr x.ident.tvar (TVar x.value.TVar)
                addConstraint expr x.tvar (TVar x.body.TVar)

                generateConstraints env x.value
                
                let env = env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar)
                generateConstraints env x.body
            | Expr.Do x ->
                (*
                    do value body   ||   t(value) = t(unit)   ||   t(body) = t2   ||   t(do) = t2
                *)
                addConstraint expr x.value.TVar BuiltinTypes.unit
                addConstraint expr x.tvar (TVar x.body.TVar)

                generateConstraints env x.value
                generateConstraints env x.body
            | Expr.PropAcc x ->
                addConstraint expr x.source.TVar (RequireMemberConstraint { fname = x.ident.identName; typ = TVar x.tvar })
                generateConstraints env x.source
            | Expr.MkArray x ->
                let elemTyp = TVar (newTVar ())
                
                addConstraint expr x.tvar (BuiltinTypes.array elemTyp)
                for v in x.values do
                    addConstraint expr v.TVar elemTyp
                    generateConstraints env v
            | Expr.MkRecord x ->
                // TODO: field names must be distinct
                let fields =
                    x.fields
                    |> List.sortBy _.fname
                    |> List.map (fun f -> f.fname, TVar f.value.TVar)
                addConstraint expr x.tvar (TProvideMembersWith NameHint.Empty fields)

                for f in x.fields do
                    generateConstraints env f.value
            | _ -> failwith "TODO: Implement the rest of the cases."
            // | Expr.Match x ->
            //     // match is exhaustive:
            //     // the type of the value expr that gets matched is a union type of all cases
            //     addConstraint expr x.expr.TVar (TProvideCasesWith NameHint.Empty [ for c in x.cases -> c.disc, None ])

            //     generateConstraints env x.expr

            //     match x.cases with
            //     | [] -> failwith $"Match expression must have at least one case."
            //     | firstCase :: otherCases ->
            //         addConstraint expr x.tvar (TVar firstCase.body.TVar)
                    
            //         for c in x.cases do
            //             // the type of the case body must be the same as the type of the value expr
            //             addConstraint expr c.body.TVar (TVar firstCase.body.TVar)

            //             let env =
            //                 match c.ident with
            //                 | Some ident -> env |> Map.add ident.identName (EnvItem.Internal ident.tvar)
            //                 | None -> env
            //             generateConstraints env c.body

        do generateConstraints env expr

        constraints

    let finalizeSolution (solution: MSolutionItem list) =
        [
            for s in solution do
                {
                    tvar = s.tvar
                    typ = Typ.gen s.monoTyp
                }
        ]

    let solveConstraints (constraints: Constraint list) =
        let mutable solverRuns = []

        let throwUniError detail (source: Expr) (t1: MonoTyp) (t2: MonoTyp) =
            let detail = 
                match detail with
                | "" -> ""
                | _ -> $"\nReason: {detail}"
            [
                $"Unification Error"
                $"-----------------"
                $"Can't unify"
                $"    {t1}"
                $"  and"
                $"    {t2}"
                $"Source Expression: {source}"
                $"Source TVar: {source.TVar}"
                $"{detail}"
            ]
            |> String.concat "\n"
            |> failwith
   
        let rec unifyTypes (source: Expr) (t1: MonoTyp) (t2: MonoTyp) =
            let throwUniError message =
                throwUniError message source t1 t2

            let unifyRecordField (requiredField: FieldDefinition) (providedRecord: RecordDefinition) =
                let existingField = 
                    providedRecord.fields
                    |> Set.tryFind (fun f -> f.fname = requiredField.fname)
                match existingField with
                | Some existingField -> unifyTypes source requiredField.typ existingField.typ
                | None -> throwUniError $"Member '{requiredField.fname}' is missing in type {providedRecord}"

            match t1,t2 with
            | t1,t2 when t1 = t2 ->
                []
            | t, TVar tvar
            | TVar tvar, t ->
                [ tvar, t ]
            | LeafTyp app1, LeafTyp app2 when app1.name = app2.name ->
                let rec loop funArgs1 funArgs2 =
                    match funArgs1, funArgs2 with
                    | [], [] -> []
                    | a1 :: args1, a2 :: args2 ->
                        loop args1 args2 @ unifyTypes source a1 a2
                    | _ -> throwUniError ""
                loop app1.args app2.args
            | FunTyp (ta, tb), FunTyp (tc, td) ->
                [
                    yield! unifyTypes source ta tc
                    yield! unifyTypes source tb td
                ]
            | RequireMemberConstraint requiredField, RecordTyp providedRecord
            | RecordTyp providedRecord, RequireMemberConstraint requiredField ->
                unifyRecordField requiredField providedRecord
            | RequireMemberConstraint requiredField, IntersectionTyp providedRecords
            | IntersectionTyp providedRecords, RequireMemberConstraint requiredField ->
                [
                    for providedRecord in providedRecords do
                        yield! unifyRecordField requiredField providedRecord
                ]
            | _ ->
                throwUniError "" source t1 t2
        
        let rec solve (constraints: Constraint list) (solutions: MSolutionItem list) =
            do solverRuns <- solverRuns @ [ (constraints, solutions) ]

            match constraints with
            | [] -> solutions
            | c :: constraints ->
                match c.t1, c.t2 with
                | TVar tvar, tSubstitute ->
                    // replace tvar with t in all other constraints
                    let updatedConstraints =
                        [
                            for otherC in constraints do
                                {
                                    triviaSource = otherC.triviaSource //if c.t1 = otherC.t1 then otherC.source else c.source
                                    t1 = substVarInTyp tvar tSubstitute otherC.t1
                                    t2 = substVarInTyp tvar tSubstitute otherC.t2 
                                }
                        ]
                    // replace tvar with t in all solutions
                    let solutions =
                        [
                            for s in solutions do
                                {
                                    tvar = s.tvar
                                    monoTyp = substVarInTyp tvar tSubstitute s.monoTyp
                                }
                        ]
                    // add solution to solutions and continue to solve
                    let solutionItem = { tvar = tvar; monoTyp = tSubstitute }
                    solve updatedConstraints (solutions @ [ solutionItem ])
                | t1, t2 ->
                    // unify t1 and t2
                    let newConstraints = 
                        unifyTypes c.triviaSource t1 t2
                        |> List.map (fun (t1, t2) -> { triviaSource = c.triviaSource; t1 = TVar t1; t2 = t2 })
                    // add new constraints to constraints and continue to solve
                    solve (newConstraints @ constraints) solutions

        let solution =
            try Ok (finalizeSolution (solve constraints []))
            with ex -> Error ex.Message

        {|
            solution = solution
            solverRuns = solverRuns
        |}


module Services =

    type SolveResult = 
        {
            result: 
                Result<
                    {|
                        solution: TypeSystem.SolutionItem list
                        finalTyp: Typ
                    |},
                    string>
            solverRuns: (TypeSystem.Constraint list * TypeSystem.MSolutionItem list) list
        }

    let solve (env: (string * Typ) list) (expr: Expr) =
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
                        | TVar (VarNum n) -> TVar (VarNum (n + varOffset))
                        | LeafTyp app -> LeafTyp {| app with args = app.args |> List.map reindexMono |}
                        | FunTyp (t1, t2) -> FunTyp (reindexMono t1, reindexMono t2)
                        | RequireMemberConstraint field -> RequireMemberConstraint { field with typ = reindexMono field.typ }
                        | IntersectionTyp records -> 
                            IntersectionTyp [ for r in records do reindexRecord r ]
                        | RecordTyp record ->
                            RecordTyp (reindexRecord record)

                    ident, EnvItem.External (reindexedVarNums typ)
            ]
            |> Map.ofList

        let solution =
            expr
            |> TypeSystem.generateConstraints env
            |> TypeSystem.solveConstraints

        {
            result =
                match solution.solution with
                | Ok solution ->
                    Ok {| 
                        solution = solution
                        finalTyp = solution |> List.find (fun s -> s.tvar = expr.TVar) |> (_.typ)
                    |}
                | Error message -> Error message
            solverRuns = solution.solverRuns
        }
