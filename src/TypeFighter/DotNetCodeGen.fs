module TypeFighter.CodeGen

open TypeFighter
open System.Collections.Generic
open System.Globalization
open System.Text

type ExpressionRendering =
    | Inline of code: string
    | Reference of varName: string

type RenderResult =
    { records: string
      body: string }

type RecordDefinition =
    { name: string
      fields: Set<string> }

let quote = "\""
let csTrue = "true"
let csFalse = "false"

let indent x = String.replicate x "    "

module Format =
    let number (f: float) = f.ToString(CultureInfo.InvariantCulture) + "d"
    
    // TODO
    let typeName (name: string) =
        match name with
        | TypeNames.string -> "string"
        | TypeNames.bool -> "bool"
        | TypeNames.number -> "double"
        | TypeNames.seq -> "IEnumerable"
        | TypeNames.unit -> "Unit"
        | x -> x

    // TODO: this is crap (again)
    let genVar (x: GenTyVar) = $"{char (x + 65)}"

    let genArgList (args: string list) =
        match args with
        | [] -> ""
        | args -> $"""<{args |> String.concat ", "}>"""

module Types =
    type RecordCache = Dictionary<Set<string>, string>

    let tau = function | Constrained t -> t | _ -> failwith "TODO: unconstrained!"

    let getGenVars (t: Tau) : GenTyVar list =
        let rec getGenVars (t: Tau) : GenTyVar list =
            match t with
            | TGenVar v -> [v]
            | TApp (_, vars) -> vars |> List.collect getGenVars
            | TFun (t1, t2) -> [ yield! getGenVars t1; yield! getGenVars t2 ]
            | TTuple taus -> taus |> List.collect getGenVars
            | TRecord fields -> [ for _,t in fields do yield! getGenVars t ]
        getGenVars t |> List.distinct
    
    let getGenericRecordDefinition (cachedRecords: RecordCache) (fields: TRecordFields) =
        let fieldNames = set [ for n,_ in fields do n ]
        let succ,recordName = cachedRecords.TryGetValue fieldNames
        let recordName =
            match succ with
            | true -> recordName
            | false ->
                let name = $"RECORD_{cachedRecords.Count}"
                do cachedRecords.Add(fieldNames, name)
                name
        recordName,fieldNames

    let renderTypeDeclaration (cachedRecords: RecordCache) (t: Tau) =
        let rec renderTypeDeclaration (t: Tau) =
            match t with
            | TGenVar v ->
                (Format.genVar v).ToUpperInvariant()
            | TApp (name, vars) ->
                let name = Format.typeName name
                let generics = Format.genArgList (vars |> List.map renderTypeDeclaration)
                $"{name}{generics}"
            | TFun (t1, t2) ->
                $"Func<{renderTypeDeclaration t1}, {renderTypeDeclaration t2}>"
            | TTuple taus ->
                let taus = [ for t in taus do renderTypeDeclaration t ] |> Format.genArgList
                $"ValueTuple{taus}"
            | TRecord fields ->
                let typeName,_ = getGenericRecordDefinition cachedRecords fields
                let recordArgs = [ for _,t in fields do renderTypeDeclaration t ] |> Format.genArgList
                $"%s{typeName}%s{recordArgs}"
        renderTypeDeclaration t

let renderRecords (cachedRecords: Types.RecordCache) (exp: TExp) =
    let rec collectedRecords =
        [ for e in Exp.collectAll exp do
            match e.meta.constr with
            | Constrained (TRecord trecord) -> yield trecord
            | _ -> ()
        ]
    let recordDefinitions = 
        collectedRecords 
        |> List.map (Types.getGenericRecordDefinition cachedRecords)
        |> List.distinct
    [ for rname,fields in recordDefinitions do
        let getArgName = sprintf "T%d"
        let indexedFields = fields |> Seq.indexed 
        let genArgs =
            indexedFields
            |> Seq.map fst 
            |> Seq.map getArgName
            |> Seq.toList 
            |> Format.genArgList
        yield $"public struct {rname}{genArgs} {{"
        for i,fname in indexedFields do
            yield $"{indent 1}public {getArgName i} {fname};"
        yield "}"
        yield ""
    ]
    |> String.concat "\n"

let rec renderBody (cachedRecords: Types.RecordCache) (exp: TExp) =
    let newLocal =
        let varCounter = Counter(0)
        fun () -> $"_loc_{varCounter.next()}"
    let emit =
        let body = StringBuilder()
        fun (indentation: int) line ->
            body.AppendLine (indent indentation + line) |> ignore
    let rec renderBody indentLevel (exp: TExp) : ExpressionRendering =
        match exp.exp with
        | Lit l ->
            let code =
                match l with
                | LString x -> quote + x + quote
                | LNumber x -> Format.number x
                | LBool x -> if x then csTrue else csFalse
                | LUnit -> $"{nameof(Unit)}.{nameof(Unit.Instance)}"
            Inline code
        | Var ident ->
            Inline ident
        | App (e1, e2) ->
            let e1res = renderBody indentLevel e1
            let e2res = renderBody indentLevel e2
            let retType = Types.renderTypeDeclaration cachedRecords (Types.tau exp.meta.constr)
            let renderApp a b = $"{a}({b})"
            let renderAppLocal local a b = $"{retType} {local} = {renderApp a b};"
            match e1res,e2res with
            | Reference v1, Reference v2 ->
                let local = newLocal()
                emit indentLevel (renderAppLocal local v1 v2)
                Reference local
            | Inline e1code, Reference v2 ->
                let local = newLocal()
                emit indentLevel (renderAppLocal local e1code v2)
                Reference local
            | Reference v1, Inline e2code ->
                let local = newLocal()
                emit indentLevel (renderAppLocal local v1 e2code)
                Reference local
            | Inline e1code, Inline e2code ->
                Inline (renderApp e1code e2code)
        | Abs (ident, body) ->
            let local = newLocal()
            let tau = Types.tau exp.meta.constr
            let (TFun (t1,t2)) = tau
            let inType = Types.renderTypeDeclaration cachedRecords t1
            let retType = Types.renderTypeDeclaration cachedRecords t2
            let genArgs = Types.getGenVars tau |> List.map Format.genVar |> Format.genArgList

            emit indentLevel $"{retType} {local}{genArgs}({inType} %s{ident.exp})"
            emit indentLevel "{"
            let bodyRes = renderBody (indentLevel + 1) body
            match bodyRes with
            | Reference v ->
                emit (indentLevel + 1) $"return {v};"
            | Inline bodyCode ->
                emit (indentLevel + 1) $"return {bodyCode};"
            emit indentLevel "}"

            Reference local
        | Let (ident, e, body) ->
            let local = newLocal()
            let identType = Types.renderTypeDeclaration cachedRecords (Types.tau e.meta.constr)
            let eres = renderBody indentLevel e
            let bodyRes = renderBody indentLevel body
            let retType = Types.renderTypeDeclaration cachedRecords (Types.tau exp.meta.constr)
            let decl s = $"{identType} {ident} = %s{s};"

            match eres with
            | Reference v ->
                emit indentLevel (decl v)
            | Inline ecode ->
                emit indentLevel (decl ecode)
                    
            let makeBody x = $"{retType} {local} = %s{x};"
            match bodyRes with
            | Reference v ->
                emit indentLevel (makeBody v)
            | Inline bodyCode ->
                emit indentLevel (makeBody bodyCode)

            Reference local
        | Prop (name, e) ->
            failwith "TODO: Prop"
        | Tuple es ->
            failwith "TODO: Tuple"
        | Record fields ->
            let tau = Types.tau exp.meta.constr
            // TODO: why can't we encode that the type must be TRecord?
            // TODO: also: this looks a bit delocated - we do many things more or less twice
            let (TRecord trecord) = tau
            let genArgs =
                [ for name,t in trecord do Types.renderTypeDeclaration cachedRecords t ] 
                |> Format.genArgList
            let recordName,_ = Types.getGenericRecordDefinition cachedRecords trecord
            let recordDecl = $"{recordName}{genArgs}"
            let local = newLocal()
            let renderedFieldExps =
                [ for fname,fe in fields do
                    fname, renderBody indentLevel fe ]
            emit indentLevel $"{recordDecl} {local} = new {recordDecl}();"
            for fname,e in renderedFieldExps do
                let renderFieldAss e = $"{local}.{fname} = %s{e};"
                match e with
                | Reference v -> emit indentLevel (renderFieldAss v)
                | Inline code -> emit indentLevel (renderFieldAss code)
                
            Reference local
    renderBody 0 exp

let rec render (exp: TExp) =
    let cachedRecords = Types.RecordCache()
    let renderedBody = renderBody cachedRecords exp
    let renderedRecords = renderRecords cachedRecords exp

    // TODO (in renderBody verschieben)
    let bodyCode =
        match renderedBody with
        | Inline x -> x
        | Reference x -> x
        
    { records = renderedRecords
      body = bodyCode }
