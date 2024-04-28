namespace TypeFighter.Lang

open TypeFighter.Utils

// - we only allow top-level parametric polymorphism
// - we don not generalize on let bindings
// - generalization can only happen outside (when passing env)

type VarNum = VarNum of int
    with override this.ToString() = let (VarNum v) = this in $"tv_{v}"

[<CustomEquality; CustomComparison>]
type NameHint =
    | Anonymous
    | Named of string
    override _.Equals(other) =
        match other with :? NameHint -> true | _ -> false
    override this.GetHashCode() = hash this
    interface System.IComparable with
        member _.CompareTo(other) = match other with :? NameHint -> 0 | _ -> -1
    interface System.IComparable<NameHint> with
        member _.CompareTo(_) = 0

type Mono =
    | TVar of VarNum
    | TApp of TMonoApp
    | TFun of Mono * Mono
    | TProvideMembers of TRecord
    | TIntersectMembers of TRecord list
    | TRequireMember of TField
    | TProvideCases of TUnion
        override this.ToString() = ShowTyp.Show(this)
and Poly =
    { vars: Set<VarNum>; monoTyp: Mono }
        override this.ToString() = ShowTyp.Show(this)
and Typ =
    | Mono of Mono
    | Poly of Poly
        override this.ToString() = ShowTyp.Show(this)
and TRecord = 
    { nameHint: NameHint; fields: Set<TField>}
        override this.ToString() = ShowTyp.Show(this)
and TField =
    { fname: string; typ: Mono }
        override this.ToString() = ShowTyp.Show(this)
and TUnion = { nameHint: NameHint; cases: Set<TCase> }
and TCase = { disc: string; payloadTyp: Mono option }
and TMonoApp = { name: string; args: Mono list }

and ShowTyp =
    static let getNameHint nameHint =
        match nameHint with
        | Anonymous -> ""
        | Named name -> $" (name={name})"
    static member Show (field: TField) =
        $"{field.fname}: {field.typ}"
    static member Show (record: TRecord) =
        record.fields
        |> Set.map ShowTyp.Show
        |> String.concat "; "
        |> fun s -> $"{{{getNameHint record.nameHint} {s} }}"
    static member Show (typ: Mono) =
        match typ with
        | TVar x -> x.ToString()
        | TApp x ->
            match x.args with
            | [] -> x.name
            | args ->
                let printedArgs = [ for a in args -> ShowTyp.Show a ] |> String.concat ", "
                $"{x.name}<{printedArgs}>"
        | TFun (t1, t2) -> $"({t1} -> {t2})"
        | TProvideMembers record -> ShowTyp.Show record
        | TIntersectMembers records ->
            records 
            |> List.map ShowTyp.Show
            |> String.concat " & "
        | TRequireMember f -> $"req_member({ShowTyp.Show f})"
        | TProvideCases union ->
            [
                for c in union.cases do
                    let payload = c.payloadTyp |> Option.map (fun t -> $"({t})") |> Option.defaultValue "_"
                    $"{c.disc}: {payload}"
            ]
            |> String.concat "; "
            |> fun s -> 
                $"{{{getNameHint union.nameHint} {s} }}"
    
    static member Show (typ: Poly) =
        let printedVars = [ for v in typ.vars -> v.ToString() ] |> String.concat ", "
        $"<{printedVars}>.{ShowTyp.Show typ.monoTyp}"
    
    static member Show (typ: Typ) =
        match typ with
        | Mono typ -> ShowTyp.Show typ
        | Poly typ -> ShowTyp.Show typ

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
        override this.ToString() =
            ShowExpr.Expr(this)
and Ident = internal { identName: string; tvar: VarNum }
and Field = internal { fname: string; value: Expr; tvar: VarNum }
and UnionCase = internal { disc: string; ident: Ident option; body: Expr }

and ShowExpr =
    static member Expr (expr: Expr) =
        let printIdent (ident: Ident) = ident.identName
        let printField (f: Field) = $"{f.fname}: {f.value}"
        let printUnionCase (c: UnionCase) =
            let binding =
                match c.ident with
                | Some ident -> $"as {printIdent ident}"
                | None -> $""
            $"    | {c.disc} {binding}-> {c.body}"
        match expr with
        | Lit x -> $"Lit {x.value}"
        | Var x -> $"Var {x.ident}"
        | App x -> $"App {x.func} ..."
        | Fun x -> $"Fun {printIdent x.ident} -> (... {x.body.TVar})"
        | Let x -> $"Let {printIdent x.ident} = ({x.value}) in {x.body}"
        | Do x -> $"Do {x.value} {x.body}"
        | Match x ->
            let caseNames = [ for x in x.cases -> printUnionCase x ] |> String.concat " | "
            $"Match {x.expr} with | {caseNames})"
        | PropAcc x -> $"PropAcc {x.source}.{x.ident}"
        | MkArray x ->
            let values = [ for x in x.values -> ShowExpr.Expr x ] |> String.concat "; "
            $"MkArray [ {values} ]"
        | MkRecord x ->
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
            | Lit x -> x.tvar :: acc
            | Var x -> x.tvar :: acc
            | App x -> 
                [ 
                    yield x.tvar
                    yield! loop x.func acc
                    yield! loop x.arg acc
                ]
            | Fun x ->
                [
                    yield x.tvar
                    yield! loop x.body acc
                ]
            | Let x ->
                [
                    yield x.tvar
                    yield! loop x.value acc
                    yield! loop x.body acc
                ]
            | Do x ->
                [
                    yield x.tvar
                    yield! loop x.value acc
                    yield! loop x.body acc
                ]
            | Match x -> 
                [
                    yield x.tvar
                    yield! loop x.expr acc
                    for c in x.cases do
                        yield! loop c.body acc
                ]
            | PropAcc x ->
                [
                    yield x.tvar
                    yield! loop x.source acc
                ]
            | MkArray x ->
                [
                    yield x.tvar
                    for v in x.values do
                        yield! loop v acc
                ]
            | MkRecord x ->
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
        and loopRecord (record: TRecord) (acc: VarNum list) =
            [
                for f in record.fields do
                    yield! loopMono f.typ acc
            ]
        and loopMono (typ: Mono) (acc: VarNum list) =
            match typ with
            | TVar x -> x :: acc
            | TApp x ->
                [
                    for arg in x.args do
                        yield! loopMono arg acc
                ]
            | TFun (t1, t2) ->
                [
                    yield! loopMono t1 acc
                    yield! loopMono t2 acc
                ]
            | TProvideMembers record ->
                loopRecord record acc
            | TIntersectMembers records ->
                [
                    for r in records do
                        yield! loopRecord r acc
                ]
            | TRequireMember f ->
                loopMono f.typ acc
            | TProvideCases union ->
                [
                    for c in union.cases do
                        match c.payloadTyp with
                        | Some payloadTyp -> yield! loopMono payloadTyp acc
                        | None -> ()
                ]
        and loopPoly (typ: Poly) (acc: VarNum list) =
            [
                yield! typ.vars
                yield! loopMono typ.monoTyp acc
            ]
        
        loop typ []
        |> List.map (fun (VarNum v) -> v)
        |> List.distinct

    let maxVar (typ: Typ) =
        0 :: collectTVars typ |> List.max

    let gen (typ: Mono) =
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
    member _.Lit value = Lit {| value = value; tvar = newTVar ()  |}
    member _.Var ident = Var {| ident = ident; tvar = newTVar ()  |}
    member _.App func arg = App {| func = func; arg = arg; tvar = newTVar ()  |}
    member _.Fun ident body = Fun {| ident = ident; body = body; tvar = newTVar ()  |}
    member _.Let ident value body = Let {| ident = ident; value = value; body = body; tvar = newTVar ()  |}
    member _.Do value body = Do {| value = value; body = body; tvar = newTVar ()  |}
    member _.Match expr cases = Match {| expr = expr; cases = cases; tvar = newTVar ()  |}
    member _.PropAcc source ident = PropAcc {| source = source; ident = { identName = ident; tvar = newTVar () } ; tvar = newTVar ()  |}
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
    member _.MkArray values = MkArray {| values = values; tvar = newTVar ()  |}
    member _.MkRecord fields = MkRecord {| fields = fields; tvar = newTVar ()  |}
    member _.Field field value = { fname = field; value = value; tvar = newTVar () }
    member _.Case disc ident body = { disc = disc; ident = ident; body = body }

[<AutoOpen>]
module TypDefHelper =

    let ( ~% ) x = TVar (VarNum x)

    let TPoly (vars: int list) (monoTyp: Mono) =
        let vars = vars |> List.map VarNum |> set
        Poly { vars = vars; monoTyp = monoTyp }

    let TFunCurr (args: Mono list) =
        let rec loop (args: Mono list) =
            match args with
            | [] -> failwith "At least one argument and a return type required."
            | a1 :: a2 :: [] -> TFun (a1, a2)
            | a1 :: args -> TFun (a1, loop args)
        loop args
    
    // CAREFUL HERE: -> is right-associative
    let ( ^-> ) t1 t2 = TFun (t1, t2)
    
    let TRecordWith nameHint (fields: (string * Mono) list) =
        let fields =
            [ for (fname, typ) in fields do { fname = fname; typ = typ } ]
            |> set
        { nameHint = nameHint; fields = fields }
    
    let TProvideMembersWith nameHint (fields: (string * Mono) list) =
        TProvideMembers (TRecordWith nameHint fields)

    let TProvideCasesWith nameHint (cases: (string * Mono option) list) =
        let cases =
            [ for (disc, payloadTyp) in cases do { disc = disc; payloadTyp = payloadTyp } ]
            |> set
        TProvideCases { nameHint = nameHint; cases = cases }
    
    let TAppWith (name: string) (args: Mono list) =
        TApp { name = name; args = args }

    let TConst (name: string) =
        TAppWith name []

    let TGen (monoTyp: Mono) =
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
    let boolean = TProvideCasesWith (Named Names.bool) [ "True", None; "False", None ]
    let number = TConst Names.number
    let string = TConst Names.string
    let date = TConst Names.date
    
    let array elemTyp = TAppWith Names.array [ elemTyp ]

module BuiltinValues =
    let [<Literal>] unitValueIdent = "UnitValue"
    let [<Literal>] toStringFunctionIdent = "ToString"


module TypeSystem =

    /// A constraint in the form: the right type must be assignable to the left type    
    type Constraint = { triviaSource: Expr; t1: Mono; t2: Mono }

    type SolutionItem = { tvar: VarNum; typ: Typ }
    type MSolutionItem = { tvar: VarNum; monoTyp: Mono }
    
    let rec substVarInTyp (tvarToReplace: VarNum) (withTyp: Mono) (inTyp: Mono) =
        let substRecord record =
            let fields = [ for f in record.fields do { f with typ = substVarInTyp tvarToReplace withTyp f.typ }]
            { record with fields = set fields }
        match inTyp with
        | TVar tvar when tvar = tvarToReplace -> withTyp
        | TVar _ -> inTyp
        | TApp app ->
            let substitutedArgs = [ for arg in app.args do substVarInTyp tvarToReplace withTyp arg ]
            TApp { app with args = substitutedArgs }
        | TFun (t1, t2) ->
            let t1 = substVarInTyp tvarToReplace withTyp t1
            let t2 = substVarInTyp tvarToReplace withTyp t2
            TFun (t1, t2)
        | TProvideMembers record -> 
            TProvideMembers (substRecord record)
        | TIntersectMembers records ->
            TIntersectMembers [ for record in records do substRecord record ]
        | TRequireMember f ->
            TRequireMember { f with typ = substVarInTyp tvarToReplace withTyp f.typ }
        | TProvideCases union ->
            TProvideCasesWith
                union.nameHint
                [ for c in union.cases do c.disc, c.payloadTyp |> Option.map (substVarInTyp tvarToReplace withTyp) ]

    let generateConstraints (env: Env) (expr: Expr) =
        let mutable constraints = []
        let addConstraint (triviaSource: Expr) (tvar: VarNum) (typ: Mono) =
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
            | Lit x ->
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
            | Var x ->
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
            | App x ->
                (*
                    func arg   ||   t(func): t1 -> t2   ||   t(arg) = t1   ||   t(app): t2
                *)
                addConstraint expr x.func.TVar (TFun (TVar x.arg.TVar, TVar x.tvar))

                generateConstraints env x.func
                generateConstraints env x.arg
            | Fun x ->
                (*
                    fun ident -> body   ||   t(ident) = t1   ||   t(body) = t2   ||   t(fun): t1 -> t2
                *)
                addConstraint expr x.tvar (TFun (TVar x.ident.tvar, TVar x.body.TVar))

                let env = env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar)
                generateConstraints env x.body
            | Let x ->
                (*
                    let ident = value in body   ||   t(value) = t1   ||   t(body) = t2   ||   t(let): t2
                *)
                addConstraint expr x.ident.tvar (TVar x.value.TVar)
                addConstraint expr x.tvar (TVar x.body.TVar)

                generateConstraints env x.value
                
                let env = env |> Map.add x.ident.identName (EnvItem.Internal x.ident.tvar)
                generateConstraints env x.body
            | Do x ->
                (*
                    do value body   ||   t(value) = t(unit)   ||   t(body) = t2   ||   t(do) = t2
                *)
                addConstraint expr x.value.TVar BuiltinTypes.unit
                addConstraint expr x.tvar (TVar x.body.TVar)

                generateConstraints env x.value
                generateConstraints env x.body
            | PropAcc x ->
                addConstraint expr x.source.TVar (TRequireMember { fname = x.ident.identName; typ = TVar x.tvar })
                generateConstraints env x.source
            | MkArray x ->
                let elemTyp = TVar (newTVar ())
                
                addConstraint expr x.tvar (BuiltinTypes.array elemTyp)
                for v in x.values do
                    addConstraint expr v.TVar elemTyp
                    generateConstraints env v
            | MkRecord x ->
                // TODO: field names must be distinct
                let fields =
                    x.fields
                    |> List.sortBy _.fname
                    |> List.map (fun f -> f.fname, TVar f.value.TVar)
                addConstraint expr x.tvar (TProvideMembersWith Anonymous fields)

                for f in x.fields do
                    generateConstraints env f.value
            | Match x ->
                // match is exhaustive:
                // the type of the value expr that gets matched is a union type of all cases
                addConstraint expr x.expr.TVar (TProvideCasesWith Anonymous [ for c in x.cases -> c.disc, None ])

                generateConstraints env x.expr

                match x.cases with
                | [] -> failwith $"Match expression must have at least one case."
                | firstCase :: otherCases ->
                    addConstraint expr x.tvar (TVar firstCase.body.TVar)
                    
                    for c in x.cases do
                        // the type of the case body must be the same as the type of the value expr
                        addConstraint expr c.body.TVar (TVar firstCase.body.TVar)

                        let env =
                            match c.ident with
                            | Some ident -> env |> Map.add ident.identName (EnvItem.Internal ident.tvar)
                            | None -> env
                        generateConstraints env c.body

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

        let throwUniError detail (source: Expr) (t1: Mono) (t2: Mono) =
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
   
        let rec unifyTypes (source: Expr) (t1: Mono) (t2: Mono) =
            let throwUniError message =
                throwUniError message source t1 t2

            let unifyRecordField (requiredField: TField) (providedRecord: TRecord) =
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
            | TApp app1, TApp app2 when app1.name = app2.name ->
                let rec loop funArgs1 funArgs2 =
                    match funArgs1, funArgs2 with
                    | [], [] -> []
                    | a1 :: args1, a2 :: args2 ->
                        loop args1 args2 @ unifyTypes source a1 a2
                    | _ -> throwUniError ""
                loop app1.args app2.args
            | TFun (ta, tb), TFun (tc, td) ->
                [
                    yield! unifyTypes source ta tc
                    yield! unifyTypes source tb td
                ]
            | TRequireMember requiredField, TProvideMembers providedRecord
            | TProvideMembers providedRecord, TRequireMember requiredField ->
                unifyRecordField requiredField providedRecord
            | TRequireMember requiredField, TIntersectMembers providedRecords
            | TIntersectMembers providedRecords, TRequireMember requiredField ->
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
                    and reindexMono (typ: Mono) =
                        let reindexRecord record =
                            let fields = [ for f in record.fields do { f with typ = reindexMono f.typ } ]
                            { record with fields = fields |> Set.ofList }
                        match typ with
                        | TVar (VarNum n) -> TVar (VarNum (n + varOffset))
                        | TApp app -> TApp { app with args = app.args |> List.map reindexMono }
                        | TFun (t1, t2) -> TFun (reindexMono t1, reindexMono t2)
                        | TRequireMember field -> TRequireMember { field with typ = reindexMono field.typ }
                        | TIntersectMembers records -> 
                            TIntersectMembers [ for r in records do reindexRecord r ]
                        | TProvideMembers record ->
                            TProvideMembers (reindexRecord record)
                        | TProvideCases union -> 
                            TProvideCasesWith
                                union.nameHint
                                [ for c in union.cases do c.disc, c.payloadTyp |> Option.map reindexMono ]

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
