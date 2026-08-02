// C# emitter for the TypeFighter (Parser01) language.
// Every value boxed to `object`, functions as `Func<object, object>`,
// records as `IDictionary<string, object>`, arrays as `object[]`.
// Idents prefixed with `@` so they never clash with C# keywords.
//
// Iter B: when an App-chain is a fully-applied call to a known operator
// (op_add, op_sub, …) and the solver has resolved the operand type, we
// emit a direct typed call into the `TfBuiltins` static class instead of
// the curried `((Func<,>)((Func<,>)f)(a))(b)` chain. Eliminates closure
// allocation and runtime type-tag dispatch for the operator hot path.
module PureState.TypeFighter.Parser01.EmitCSharp

open System.Text
open PureState.TypeFighter

// Builtins with a typed `TfBuiltins.<name>(...)` counterpart. Each entry
// describes the call shape so the emitter can spell the right C# at the
// fully-applied call site. Partial applications still go through the
// curried Func<object,object> fallback (and trigger a csImport).
type private TypedCallSig =
    /// All operands are the same type, taken from the first arg's resolved
    /// MonoTyp (e.g. `op_add` — both args are Number or both String).
    | OpUniform of arity: int
    /// `log`: 1 user arg + implicit `__out` TextWriter from the method param.
    | LogLike
    /// `ToString`: 1 arg, generic; C# infers T.
    | ToStringLike
    /// `concat`: 2 string args.
    | ConcatLike
    /// `head` / `length`: 1 IReadOnlyList<object> arg.
    | HeadLike
    /// `at`: list + double index.
    | AtLike
    /// `cond`: bool predicate + 2 branch values of the same type T.
    /// T is taken from the second arg's resolved MonoTyp.
    | CondLike
    /// `__nowSeconds`: 1 Unit arg (ignored); reads the implicit `__now`
    /// (DateTimeOffset) method param and yields seconds since the Unix epoch.
    | NowLike

let private typedCalls : Map<string, TypedCallSig> =
    Map.ofList [
        "op_add", OpUniform 2; "op_sub", OpUniform 2; "op_mul", OpUniform 2
        "op_div", OpUniform 2; "op_neg", OpUniform 1
        "op_lt",  OpUniform 2; "op_gt",  OpUniform 2
        "op_le",  OpUniform 2; "op_ge",  OpUniform 2
        "op_eq",  OpUniform 2; "op_neq", OpUniform 2
        "op_and", OpUniform 2; "op_or",  OpUniform 2; "op_not", OpUniform 1
        "log",      LogLike
        "ToString", ToStringLike
        "concat",   ConcatLike
        "head",     HeadLike
        "length",   HeadLike
        "at",       AtLike
        "cond",     CondLike
        "__nowSeconds", NowLike
    ]

// Map a resolved MonoTyp to the C# type used at typed call-sites. None
// when the type isn't a primitive we know how to spell — caller falls
// back to `object`.
let csTypeOfMono (t: MonoTyp) : string option =
    match t with
    | SaturatedTyp app when app.name = BuiltinTypes.Names.number -> Some "double"
    | SaturatedTyp app when app.name = BuiltinTypes.Names.string -> Some "string"
    | UnionTyp members
        when members |> Set.forall (fun m ->
            match m with LiteralTyp (Boolean _) -> true | _ -> false) ->
        Some "bool"
    | _ -> None

let private collectAppChain (e: Expr<VarNum>) =
    let rec loop e args =
        match e with
        | Expr.App app -> loop app.func (app.arg :: args)
        | _ -> e, args
    loop e []

let private escapeString (s: string) =
    let sb = StringBuilder(s.Length + 2)
    sb.Append('"') |> ignore
    for c in s do
        match c with
        | '\\' -> sb.Append("\\\\") |> ignore
        | '"'  -> sb.Append("\\\"") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | c when int c < 0x20 -> sb.Append(sprintf "\\u%04x" (int c)) |> ignore
        | c -> sb.Append(c) |> ignore
    sb.Append('"') |> ignore
    sb.ToString()

let private emitNumber (n: float) =
    if System.Double.IsNaN n then "double.NaN"
    elif System.Double.IsPositiveInfinity n then "double.PositiveInfinity"
    elif System.Double.IsNegativeInfinity n then "double.NegativeInfinity"
    else sprintf "%sD" (n.ToString(System.Globalization.CultureInfo.InvariantCulture))

let private emitLiteral (lit: Literal) =
    match lit with
    | Number n -> sprintf "((object)%s)" (emitNumber n)
    | String s -> sprintf "((object)%s)" (escapeString s)
    | Boolean b -> if b then "((object)true)" else "((object)false)"

// Prefix all identifiers with `@` to dodge C# keyword collisions.
let private id (s: string) = "@" + s

// Wraps the body in extra parens so the cast is applied to the *whole*
// lambda: `((Func<,>)((object @x) => body))` instead of the syntactically
// invalid `((Func<,>)(object @x) => body)`.
let private fn = "((Func<object, object>)("
let private fnEnd = "))"

let rec private emitExpr (tvarToMono: VarNum -> MonoTyp option) (expr: Expr<VarNum>) : string =
    let recur = emitExpr tvarToMono
    match expr with
    | Expr.Lit x -> emitLiteral x.value
    | Expr.Var x -> id x.ident
    | Expr.App _ ->
        // Try the typed fast-path first: a fully-applied call to a known
        // operator routes directly to `TfBuiltins.<name>(...)`. Falls back
        // to the curried Func<object,object> chain when arity is partial
        // or the operand type can't be resolved to a primitive.
        let head, args = collectAppChain expr
        let typedCall =
            match head with
            | Expr.Var v when Map.containsKey v.ident typedCalls ->
                let sigInfo = Map.find v.ident typedCalls
                let expectedArity =
                    match sigInfo with
                    | OpUniform n -> n
                    | LogLike | ToStringLike | HeadLike | NowLike -> 1
                    | ConcatLike | AtLike -> 2
                    | CondLike -> 3
                if args.Length <> expectedArity then None
                else
                    match sigInfo with
                    | OpUniform _ ->
                        let opType =
                            args
                            |> List.tryHead
                            |> Option.bind (fun a -> tvarToMono a.TVar)
                            |> Option.bind csTypeOfMono
                        match opType with
                        | Some t ->
                            let argStrs =
                                args |> List.map (fun a -> sprintf "(%s)(%s)" t (recur a))
                            Some (sprintf "((object)TfBuiltins.%s(%s))" v.ident (String.concat ", " argStrs))
                        | None -> None
                    | LogLike ->
                        Some (sprintf "((object)TfBuiltins.log(__out, %s))" (recur args.[0]))
                    | NowLike ->
                        // Unit arg ignored; reads the implicit `__now` param.
                        Some "((object)(double)(__now - System.DateTimeOffset.UnixEpoch).TotalSeconds)"
                    | ToStringLike ->
                        Some (sprintf "((object)TfBuiltins.ToString(%s))" (recur args.[0]))
                    | ConcatLike ->
                        Some (sprintf "((object)TfBuiltins.concat((string)(%s), (string)(%s)))"
                                (recur args.[0]) (recur args.[1]))
                    | HeadLike ->
                        Some (sprintf "((object)TfBuiltins.%s((System.Collections.Generic.IReadOnlyList<object>)(%s)))"
                                v.ident (recur args.[0]))
                    | AtLike ->
                        Some (sprintf "((object)TfBuiltins.at((System.Collections.Generic.IReadOnlyList<object>)(%s), (double)(%s)))"
                                (recur args.[0]) (recur args.[1]))
                    | CondLike ->
                        // Branch type T comes from the second arg's resolved
                        // MonoTyp; without it we can't pick a uniform C#
                        // ternary type — fall back to the curried Func chain.
                        let branchType =
                            tvarToMono args.[1].TVar |> Option.bind csTypeOfMono
                        match branchType with
                        | Some t ->
                            Some (sprintf "((object)(((bool)(%s)) ? ((%s)(%s)) : ((%s)(%s))))"
                                    (recur args.[0]) t (recur args.[1]) t (recur args.[2]))
                        | None -> None
            | _ -> None
        match typedCall with
        | Some s -> s
        | None ->
            match expr with
            | Expr.App x ->
                sprintf "%s%s%s(%s)" fn (recur x.func) fnEnd (recur x.arg)
            | _ -> failwith "unreachable"
    | Expr.Fun x ->
        sprintf "%s(object %s) => %s%s" fn (id x.ident.identName) (recur x.body) fnEnd
    | Expr.Let x ->
        sprintf "%s(object %s) => %s%s(%s)"
            fn (id x.ident.identName) (recur x.body) fnEnd (recur x.value)
    | Expr.Do x ->
        // Sequence: evaluate `action` for side-effect, then return `body`.
        sprintf "((Func<object>)(() => { var __ = %s; return %s; }))()"
            (recur x.action) (recur x.body)
    | Expr.PropAcc x ->
        sprintf "((System.Collections.Generic.IDictionary<string, object>)(%s))[\"%s\"]"
            (recur x.source) x.ident.identName
    | Expr.MkArray x ->
        let items = x.values |> List.map recur |> String.concat ", "
        sprintf "((object)new object[] { %s })" items
    | Expr.MkRecord x ->
        let mutable posIdx = 0
        let parts =
            x.items
            |> List.map (fun item ->
                match item with
                | RecordItem.Property f ->
                    sprintf "[\"%s\"] = %s" f.fname (recur f.value)
                | RecordItem.Positional v ->
                    let name = sprintf "_%d" posIdx
                    posIdx <- posIdx + 1
                    sprintf "[\"%s\"] = %s" name (recur v))
            |> String.concat ", "
        sprintf "((object)new System.Collections.Generic.Dictionary<string, object> { %s })" parts
    | Expr.Match x ->
        let sb = StringBuilder()
        sb.Append(fn) |> ignore
        sb.Append("(object __s) => {") |> ignore
        for arm in x.arms do
            match arm.pattern with
            | MatchPattern.Literal l ->
                sb.Append(sprintf " if (object.Equals(__s, %s)) return %s;"
                            (emitLiteral l.value) (recur arm.body)) |> ignore
            | MatchPattern.Var v ->
                sb.Append(sprintf " { object %s = __s; return %s; }"
                            (id v.identName) (recur arm.body)) |> ignore
            | MatchPattern.Wildcard _ ->
                sb.Append(sprintf " return %s;" (recur arm.body)) |> ignore
        sb.Append(" return null;") |> ignore
        sb.Append("}") |> ignore
        sb.Append(fnEnd) |> ignore
        sb.Append(sprintf "(%s)" (recur x.scrutinee)) |> ignore
        sb.ToString()

/// Emit a single C# expression body.
let toCSharp (tvarToMono: VarNum -> MonoTyp option) (expr: Expr<VarNum>) : string =
    emitExpr tvarToMono expr

/// Free identifiers that need a runtime shim (i.e. are referenced as
/// first-class values rather than being fully-dispatched into TfBuiltins).
/// Used to prune the unconditional `var @log = (Func<object,object>)...`
/// header — if the program never uses `log` outside a fully-applied call
/// position, the shim is dead weight.
let collectFirstClassRefs
    (tvarToMono: VarNum -> MonoTyp option)
    (expr: Expr<VarNum>)
    : Set<string>
    =
    let acc = System.Collections.Generic.HashSet<string>()

    let isTypedCallDispatch (head: Expr<VarNum>) (args: Expr<VarNum> list) =
        match head with
        | Expr.Var v when Map.containsKey v.ident typedCalls ->
            let sigInfo = Map.find v.ident typedCalls
            let expectedArity =
                match sigInfo with
                | OpUniform n -> n
                | LogLike | ToStringLike | HeadLike | NowLike -> 1
                | ConcatLike | AtLike -> 2
                | CondLike -> 3
            if args.Length <> expectedArity then false
            else
                match sigInfo with
                | OpUniform _ ->
                    args
                    |> List.tryHead
                    |> Option.bind (fun a -> tvarToMono a.TVar)
                    |> Option.bind csTypeOfMono
                    |> Option.isSome
                | CondLike ->
                    tvarToMono args.[1].TVar
                    |> Option.bind csTypeOfMono
                    |> Option.isSome
                | _ -> true
        | _ -> false

    let rec walk (bound: Set<string>) e =
        match e with
        | Expr.Lit _ -> ()
        | Expr.Var v ->
            if not (Set.contains v.ident bound) then
                acc.Add v.ident |> ignore
        | Expr.App _ ->
            let head, args = collectAppChain e
            if not (isTypedCallDispatch head args) then
                walk bound head
            for a in args do walk bound a
        | Expr.Fun x -> walk (Set.add x.ident.identName bound) x.body
        | Expr.Let x ->
            walk bound x.value
            walk (Set.add x.ident.identName bound) x.body
        | Expr.Do x ->
            walk bound x.action
            walk bound x.body
        | Expr.PropAcc x -> walk bound x.source
        | Expr.MkArray x ->
            for v in x.values do walk bound v
        | Expr.MkRecord x ->
            for item in x.items do
                match item with
                | RecordItem.Property f -> walk bound f.value
                | RecordItem.Positional v -> walk bound v
        | Expr.Match x ->
            walk bound x.scrutinee
            for arm in x.arms do
                let armBound =
                    match arm.pattern with
                    | MatchPattern.Var v -> Set.add v.identName bound
                    | _ -> bound
                walk armBound arm.body
    walk Set.empty expr
    Set.ofSeq acc

/// Wrap the expression in a self-contained C# class.
/// `imports`: prelude bindings as `(preludeName, csExpr)` -- become local vars.
/// `extraParams`: extra method params as `(name, csType)` after `__out`.
let toCSharpClass
    (className: string)
    (imports: (string * string) list)
    (extraParams: (string * string) list)
    (tvarToMono: VarNum -> MonoTyp option)
    (expr: Expr<VarNum>)
    : string
    =
    let sb = StringBuilder()
    sb.AppendLine("using System;") |> ignore
    sb.AppendLine("using System.Collections.Generic;") |> ignore
    sb.AppendLine("using System.IO;") |> ignore
    sb.AppendLine() |> ignore
    sb.AppendLine(sprintf "public static class %s" className) |> ignore
    sb.AppendLine("{") |> ignore
    let paramSig =
        ("__out", "TextWriter") :: extraParams
        |> List.map (fun (n, t) -> sprintf "%s %s" t n)
        |> String.concat ", "
    sb.AppendLine(sprintf "    public static object Eval(%s)" paramSig) |> ignore
    sb.AppendLine("    {") |> ignore
    // `__now` so the stdlib `now` binding (emitted even when unused) compiles.
    sb.AppendLine("        var __now = DateTimeOffset.UtcNow;") |> ignore
    for name, body in imports do
        sb.AppendLine(sprintf "        var %s = %s;" (id name) body) |> ignore
    sb.AppendLine(sprintf "        return %s;" (emitExpr tvarToMono expr)) |> ignore
    sb.AppendLine("    }") |> ignore
    sb.AppendLine("}") |> ignore
    sb.ToString()
