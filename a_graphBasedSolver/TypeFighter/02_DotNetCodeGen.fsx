module TypeFighter.DotNetCodeGen

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

type RecordCache = Dictionary<Set<string>, string>

let quote = "\""
let csTrue = "true"
let csFalse = "false"

let indent x = String.replicate x "    "
let indentLine x s = (indent x) + s

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
    let genArg (x: GenTyVar) = $"T{x}"

    let genArgList (args: string list) =
        match args with
        | [] -> ""
        | args -> $"""<{args |> String.concat ", "}>"""
    let genArgListVars vars =
         vars |> Set.map genArg |> Set.toList |> genArgList
    let genArgListTaus taus =
         taus |> Tau.geTDef.GeneralizeVarsMany |> genArgListVars
    let genArgListTau tau =
         genArgListTaus [tau]
    
    let geTDef.GeneralizeericRecordDefinition (cachedRecords: RecordCache) (fields: TRecordFields) =
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
            | TDef.GeneralizeVar v ->
                (genArg v).ToUpperInvariant()
            | TApp (name, vars) ->
                let name = typeName name
                let generics = [ for var in vars do renderTypeDeclaration var ] |> genArgList
                $"{name}{generics}"
            | TFun (t1, t2) ->
                $"Func<{renderTypeDeclaration t1}, {renderTypeDeclaration t2}>"
            | TTuple taus ->
                let taus = [ for t in taus do renderTypeDeclaration t ] |> genArgList
                $"ValueTuple{taus}"
            | TRecord fields ->
                let typeName,_ = geTDef.GeneralizeericRecordDefinition cachedRecords fields
                let recordArgs = [ for _,t in fields do renderTypeDeclaration t ] |> genArgList
                $"%s{typeName}%s{recordArgs}"
        renderTypeDeclaration t

open ConstraintGraph

type Emitter() =
    let allSbs = ResizeArray<StringBuilder>()
    let stack = Stack<StringBuilder>()
    let mutable currentSb = StringBuilder()
    member this.push() =
        do stack.Push currentSb
        let newSb = StringBuilder()
        do allSbs.Add(newSb)
        do currentSb <- newSb
    member this.pop() =
        currentSb <- stack.Pop()
    member this.emit indentation s =
        currentSb.AppendLine (indentLine indentation s) |> ignore
    member this.renderString() =
        [ for s in allSbs do s.ToString() ] |> String.concat "\n\n"

let renderDisplayClasses (cachedRecords: RecordCache) (solveRes: ConstraintGraph.SolveResult) =   
    let newName f =
        let counter = Counter(0)
        fun () -> f (counter.next())
    let newClassName = newName (sprintf "DisplayClass_%d")
    let newLocVarName = newName (sprintf "loc_%d")
    let emitter = Emitter()
    let rec walk indentation exp =
        let walkNext = walk (indentation + 1)
        match exp.exp with
        | Lit x ->
            let code =
                match x with
                | LString x -> quote + x + quote
                | LNumber x -> Format.number x
                | LBool x -> if x then csTrue else csFalse
                | LUnit -> $"{nameof(Unit)}.{nameof(Unit.Instance)}"
            Inline code
        | Var ident ->
            Inline ident
        | App (e1, e2) ->
            let t1 = SolveResult.findTau e1 solveRes
            let t2 = SolveResult.findTau e2 solveRes
            walkNext e1 |> ignore
            walkNext e2 |> ignore
            emitter.emit 0 $"t1: {Format.tau t1}   t2: {Format.tau t1}"
            Inline ""
        | Fun (ident, body) ->
            let tau = SolveResult.findTau exp solveRes
            let (TFun (t1,t2)) = tau
            let inType = Format.renderTypeDeclaration cachedRecords t1
            let retType = Format.renderTypeDeclaration cachedRecords t2
            let fields =
                exp.meta.env
                |> Env.getInterns
                |> Map.toSeq
                |> Seq.map (fun (ident,tyvar) ->
                    let tau = solveRes.envConstraintStates |> Map.find tyvar
                    ident,tau)
                |> Seq.toList
            let classGenArgs = 
                fields
                |> List.map snd
                |> List.map Tau.tau
                |> Tau.geTDef.GeneralizeVarsMany
            let invokeGenArgs = (Tau.geTDef.GeneralizeVars tau) - classGenArgs
                    
            let fieldsString =
                fields
                |> List.map (fun (ident,tau) ->
                    let typeDef =
                        // TODO: am Anfang mal eine Liste machen, so dass ConstrState zu Tau wird
                        match Tau.tau tau with
                        | TFun (t1,t2) -> $"DISPLAY_CLASS_<{Format.tau t1}|{Format.tau t2}>"
                        | tau -> tau |> Format.renderTypeDeclaration cachedRecords
                    $"public {typeDef} {ident};")
            let classGenArgsString = classGenArgs |> Format.genArgListVars
            let invokeGenArgsString = invokeGenArgs |> Format.genArgListVars

            do emitter.push()
            $"class {newClassName()}%s{classGenArgsString}" |> emitter.emit 0
            "{" |> emitter.emit 0
            for x in fieldsString do x |> emitter.emit 1
            $"public {retType} Invoke{invokeGenArgsString}({inType} {ident.exp})" |> emitter.emit 1
            "{" |> emitter.emit 1
            walkNext body |> ignore
            "}" |> emitter.emit 1
            "}" |> emitter.emit 0
            do emitter.pop()
            Inline ""
        | Let (ident, e, body) ->
            walkNext e |> ignore
            walkNext body |> ignore
            Inline ""
        | Prop (ident, e) ->
            walkNext e |> ignore
            Inline ""
        | Tuple es ->
            for e in es do walkNext e |> ignore
            Inline ""
        | Record fields ->
            for _,e in fields do walkNext e |> ignore
            Inline ""
    do walk 0 solveRes.annotationResult.root |> ignore
    emitter.renderString()
    
let renderRecords (cachedRecords: RecordCache) (solveRes: ConstraintGraph.SolveResult) =
    let rec collectedRecords =
        [ for exp in solveRes.exprConstraintStates |> Map.values do
            match fst exp |> Tau.tau with | TRecord trecord -> trecord | _ -> ()
        ]
    let recordDefinitions = 
        collectedRecords 
        |> List.map (Format.geTDef.GeneralizeericRecordDefinition cachedRecords)
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

let rec renderBody (cachedRecords: RecordCache) (solveRes: ConstraintGraph.SolveResult) =
    failwith "TODO"
    //let newLocal =
    //    let varCounter = Counter(0)
    //    fun () -> $"_loc_{varCounter.next()}"
    //let rec renderBody indentLevel (exp: TExp) : ExpressionRendering =
    //    match exp.exp with
    //    | Lit l ->
    //        let code =
    //            match l with
    //            | LString x -> quote + x + quote
    //            | LNumber x -> Format.number x
    //            | LBool x -> if x then csTrue else csFalse
    //            | LUnit -> $"{nameof(Unit)}.{nameof(Unit.Instance)}"
    //        Inline code
    //    | Var ident ->
    //        Inline ident
    //    | App (e1, e2) ->
    //        let e1res = renderBody indentLevel e1
    //        let e2res = renderBody indentLevel e2
    //        let retType = Format.renderTypeDeclaration cachedRecords (SolveResult.findTau exp solveRes)
    //        let renderApp a b = $"{a}({b})"
    //        let renderAppLocal local a b = $"{retType} {local} = {renderApp a b};"
    //        match e1res,e2res with
    //        | Reference v1, Reference v2 ->
    //            let local = newLocal()
    //            emit indentLevel (renderAppLocal local v1 v2)
    //            Reference local
    //        | Inline e1code, Reference v2 ->
    //            let local = newLocal()
    //            emit indentLevel (renderAppLocal local e1code v2)
    //            Reference local
    //        | Reference v1, Inline e2code ->
    //            let local = newLocal()
    //            emit indentLevel (renderAppLocal local v1 e2code)
    //            Reference local
    //        | Inline e1code, Inline e2code ->
    //            Inline (renderApp e1code e2code)
    //    | Fun (ident, body) ->
    //        let local = newLocal()
    //        let tau = SolveResult.findTau exp solveRes
    //        let (TFun (t1,t2)) = tau
    //        let inType = Format.renderTypeDeclaration cachedRecords t1
    //        let retType = Format.renderTypeDeclaration cachedRecords t2
    //        let genArgs = Format.genArgListTau tau

    //        emit indentLevel $"{retType} {local}{genArgs}({inType} %s{ident.exp})"
    //        emit indentLevel "{"
    //        let bodyRes = renderBody (indentLevel + 1) body
    //        match bodyRes with
    //        | Reference v ->
    //            emit (indentLevel + 1) $"return {v};"
    //        | Inline bodyCode ->
    //            emit (indentLevel + 1) $"return {bodyCode};"
    //        emit indentLevel "}"

    //        Reference local
    //    | Let (ident, e, body) ->
    //        let local = newLocal()
    //        let identType = Format.renderTypeDeclaration cachedRecords (SolveResult.findTau exp solveRes)
    //        let eres = renderBody indentLevel e
    //        let bodyRes = renderBody indentLevel body
    //        let retType = Format.renderTypeDeclaration cachedRecords (SolveResult.findTau exp solveRes)
    //        let decl s = $"{identType} {ident} = %s{s};"

    //        match eres with
    //        | Reference v ->
    //            emit indentLevel (decl v)
    //        | Inline ecode ->
    //            emit indentLevel (decl ecode)
                    
    //        let makeBody x = $"{retType} {local} = %s{x};"
    //        match bodyRes with
    //        | Reference v ->
    //            emit indentLevel (makeBody v)
    //        | Inline bodyCode ->
    //            emit indentLevel (makeBody bodyCode)

    //        Reference local
    //    | Prop (name, e) ->
    //        failwith "TODO: Prop"
    //    | Tuple es ->
    //        failwith "TODO: Tuple"
    //    | Record fields ->
    //        let tau = SolveResult.findTau exp solveRes
    //        // TODO: why can't we encode that the type must be TRecord?
    //        // TODO: also: this looks a bit delocated - we do many things more or less twice
    //        let (TRecord trecord) = tau
    //        let genArgs =
    //            trecord
    //            |> Set.toList
    //            |> List.map snd
    //            |> Format.genArgListTaus
    //        let recordName,_ = Format.geTDef.GeneralizeericRecordDefinition cachedRecords trecord
    //        let recordDecl = $"{recordName}{genArgs}"
    //        let local = newLocal()
    //        let renderedFieldExps =
    //            [ for fname,fe in fields do
    //                fname, renderBody indentLevel fe ]
    //        emit indentLevel $"{recordDecl} {local} = new {recordDecl}();"
    //        for fname,e in renderedFieldExps do
    //            let renderFieldAss e = $"{local}.{fname} = %s{e};"
    //            match e with
    //            | Reference v -> emit indentLevel (renderFieldAss v)
    //            | Inline code -> emit indentLevel (renderFieldAss code)
                
    //        Reference local
    //renderBody 0 solveRes.annotationResult.root

let rec render (solveRes: ConstraintGraph.SolveResult) =
    let cachedRecords = RecordCache()
    let renderedBody = renderBody cachedRecords solveRes
    let renderedRecords = renderRecords cachedRecords solveRes

    // TODO (in renderBody verschieben)
    let bodyCode =
        match renderedBody with
        | Inline x -> x
        | Reference x -> x
        
    { records = renderedRecords
      body = bodyCode }
