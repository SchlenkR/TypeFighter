namespace TypeFighter.CodeGen

type Query<'context> = 'context -> obj

open System.Collections.Generic
open System.Globalization
open TypeFighter

module rec DotNetCodeModel =
    
    type CapturedField =
        { name: string
          typ: Tau }

    type DispalyClass =
        { fields: CapturedField
          invokeReturnTyp: Tau
          inputTyp: Tau
          body: CodeExp }

    type CodeExp =
        | DisplayClass of DispalyClass


module CsCodeGen =

    type ExpressionRendering =
        | One of string
        | Many of string * (int * string) list

    type RenderResult =
        { records: string
          body: string }

    type RecordDefinition =
        { name: string
          fields: Set<string> }

    let quote = "\""
    let csTrue = "true"
    let csFalse = "false"

    let formatNumber (f: float) = f.ToString(CultureInfo.InvariantCulture) + "d"
    
    // TODO
    let resolveDotNetTypeName (name: string) =
        match name with
        | TypeNames.string -> "string"
        | TypeNames.bool -> "bool"
        | TypeNames.number -> "double"
        | TypeNames.seq -> "IEnumerable"
        | TypeNames.unit -> "Unit"
        | x -> x

    let rec render (exp: TExp) =

        let getRecordDefinition =
            let knownRecords = Dictionary<Set<string>, string>()
            fun (fields: TRecordFields) ->
                let fieldNames = set [ for n,_ in fields do n ]
                let succ,recordName = knownRecords.TryGetValue fieldNames
                let recordName =
                    match succ with
                    | true -> recordName
                    | false ->
                        let name = $"RECORD_{knownRecords.Count}"
                        do knownRecords.Add(fieldNames, name)
                        name
                recordName,fieldNames

        let getGenVars (t: Tau) : GenTyVar list =
            let rec getGenVars (t: Tau) : GenTyVar list =
                match t with
                | TGenVar v -> [v]
                | TApp (_, vars) -> vars |> List.collect getGenVars
                | TFun (t1, t2) -> [ yield! getGenVars t1; yield! getGenVars t2 ]
                | TTuple taus -> taus |> List.collect getGenVars
                | TRecord fields -> [ for _,t in fields do yield! getGenVars t ]
            getGenVars t |> List.distinct

        let renderGenArgs (args: string list) =
            match args with
            | [] -> ""
            | args -> $"""<{args |> String.concat ", "}>"""

        // TODO: value tuple vars limit
        let rec toDotNetTypeDeclaration (t: Tau) =
            match t with
            | TGenVar v ->
                (Format.genVar v).ToUpperInvariant()
            | TApp (name, vars) ->
                let name = resolveDotNetTypeName name
                let generics = renderGenArgs (vars |> List.map toDotNetTypeDeclaration)
                $"{name}{generics}"
            | TFun (t1, t2) ->
                $"Func<{toDotNetTypeDeclaration t1}, {toDotNetTypeDeclaration t2}>"
            | TTuple taus ->
                let taus = [ for t in taus do toDotNetTypeDeclaration t ] |> renderGenArgs
                $"ValueTuple{taus}"
            | TRecord fields ->
                let typeName,_ = getRecordDefinition fields
                let recordArgs = [ for _,t in fields do toDotNetTypeDeclaration t ] |> renderGenArgs
                $"%s{typeName}%s{recordArgs}"

        let newLocal =
            let varCounter = Counter(0)
            fun () -> $"_loc_{varCounter.next()}"

        let indent x = String.replicate x "    "

        // TODO
        let tau =
            function
            | Constrained t -> t
            | _ -> failwith "TODO: unconstrained!"

        let rec collectRecords (exp: TExp) =
            [ for e in Exp.collectAll exp do
                match e.meta.constr with
                | Constrained (TRecord trecord) -> yield trecord
                | _ -> ()
            ]

        let renderedRecords =
            let recordDefinitions = 
                collectRecords exp 
                |> List.map getRecordDefinition
                |> List.distinct
            [ for rname,fields in recordDefinitions do
                let getArgName = sprintf "T%d"
                let indexedFields = fields |> Seq.indexed 
                let genArgs =
                    indexedFields
                    |> Seq.map fst 
                    |> Seq.map getArgName
                    |> Seq.toList 
                    |> renderGenArgs
                yield $"public struct {rname}{genArgs} {{"
                for i,fname in indexedFields do
                    yield $"{indent 1}public {getArgName i} {fname};"
                yield "}"
                yield ""
            ]
            |> String.concat "\n"

        let rec renderBody indentLevel (exp: TExp) : ExpressionRendering =
            match exp.exp with
            | Lit l ->
                let code =
                    match l with
                    | LString x -> quote + x + quote
                    | LNumber x -> formatNumber x
                    | LBool x -> if x then csTrue else csFalse
                    | LUnit -> $"{nameof(Unit)}.{nameof(Unit.Instance)}"
                One code
            | Var ident ->
                One ident
            | App (e1, e2) ->
                let e1res = renderBody indentLevel e1
                let e2res = renderBody indentLevel e2
                let retType = toDotNetTypeDeclaration (tau exp.meta.constr)
                let renderApp a b = $"{a}.Invoke({b})"
                let renderAppLocal local a b = $"{retType} {local} = {renderApp a b};"
                match e1res,e2res with
                | Many (v1, e1code), Many (v2,e2code) ->
                    let local = newLocal()
                    Many (local, [
                        yield! e1code
                        yield! e2code
                        yield indentLevel, renderAppLocal local v1 v2
                    ])
                | One e1code, Many (v2,e2code) ->
                    let local = newLocal()
                    Many(local, [
                        yield! e2code
                        yield indentLevel, renderAppLocal local e1code v2
                    ])
                | Many (v1,e1code), One e2code ->
                    let local = newLocal()
                    Many (local, [
                        yield! e1code
                        yield indentLevel, renderAppLocal local v1 e2code
                    ])
                | One e1code, One e2code ->
                    One (renderApp e1code e2code)
            | Abs (ident, body) ->
                let local = newLocal()
                let bodyRes = renderBody (indentLevel + 1) body
                let funcType = toDotNetTypeDeclaration (tau exp.meta.constr)
                Many (local, [
                    yield indentLevel, $"{funcType} {local} = new {funcType}(%s{ident.exp} => {{"
                    match bodyRes with
                    | Many (v, bodyCode) ->
                        yield! bodyCode
                        yield indentLevel + 1, $"return {v};"
                    | One bodyCode ->
                        yield indentLevel + 1, $"return {bodyCode};"
                    yield indentLevel, $"}});"
                ])
            | Let (ident, e, body) ->
                let local = newLocal()
                let identType = toDotNetTypeDeclaration (tau e.meta.constr)
                let eres = renderBody indentLevel e
                let bodyRes = renderBody indentLevel body
                let retType = toDotNetTypeDeclaration (tau exp.meta.constr)
                let decl s = $"{identType} {ident} = %s{s};"
                Many (local, [
                    match eres with
                    | Many (v, ecode) ->
                        yield! ecode
                        yield indentLevel, decl v
                    | One ecode ->
                        yield indentLevel, decl ecode
                    
                    let makeBody x = $"{retType} {local} = %s{x};"
                    match bodyRes with
                    | Many (v, bodyCode) ->
                        yield! bodyCode
                        yield indentLevel, makeBody v
                    | One bodyCode ->
                        yield indentLevel, makeBody bodyCode
                ])
            | Prop (name, e) ->
                failwith "TODO: Prop"
            | Tuple es ->
                failwith "TODO: Tuple"
            | Record fields ->
                let tau = tau exp.meta.constr
                // TODO: why can't we encode that the type must be TRecord?
                // TODO: also: this looks a bit delocated - we do many things more or less twice
                let (TRecord trecord) = tau
                let genArgs = [ for name,t in trecord do toDotNetTypeDeclaration t ] |> renderGenArgs
                let recordName,_ = getRecordDefinition trecord
                let recordDecl = $"{recordName}{genArgs}"
                let local = newLocal()
                let renderedFieldExps = [ for fname,fe in fields do fname, renderBody indentLevel fe ]
                Many (local,
                    [ // first, yield the generated code of the field expressions
                      for _,e in renderedFieldExps do
                        match e with
                        | Many (_, code) -> yield! code
                        | _ -> ()
                      
                      yield indentLevel, $"{recordDecl} {local} = new {recordDecl}();"

                      // then, the field assignements
                      for fname,e in renderedFieldExps do
                        let renderFieldAss e = indentLevel, $"{local}.{fname} = %s{e};"
                        match e with
                        | Many (v,_) -> yield renderFieldAss v
                        | One code -> yield renderFieldAss code
                    ])


        let res = renderBody 0 exp
        let codeString =
            match res with
            | Many (v, code) ->
                code |> List.map (fun (i,s) -> $"{indent i}{s}") |>  String.concat "\n"
            | One code ->
                code
        
        { records = renderedRecords
          body = codeString }



