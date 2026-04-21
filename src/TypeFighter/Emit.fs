namespace TypeFighter

/// JavaScript emitter: turns a TypeFighter AST into a runnable JS
/// expression string. Pure function, no I/O, Fable-compatible.
[<RequireQualifiedAccess>]
module Emit =

    open System.Text

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
            | c when int c < 0x20 ->
                sb.Append(sprintf "\\u%04x" (int c)) |> ignore
            | c -> sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()

    // JavaScript and .NET both accept the same shape of number literals
    // (no locale-sensitive decimal mark when running under invariant
    // culture / default JS locale). `string n` gives that shape on both
    // runtimes. The integer shortcut avoids trailing `.0` artifacts.
    let private emitNumber (n: float) =
        if System.Double.IsNaN n then "NaN"
        elif System.Double.IsPositiveInfinity n then "Infinity"
        elif System.Double.IsNegativeInfinity n then "-Infinity"
        elif n = floor n && abs n < 1e15 then
            sprintf "%d" (int64 n)
        else
            string n

    let private emitLiteral (lit: Literal) =
        match lit with
        | Number n -> emitNumber n
        | String s -> escapeString s
        | Boolean b -> if b then "true" else "false"

    /// Shapes whose rendered form never needs outer parens before `(`, `.` etc.
    let private isAtomic (expr: Expr<_>) =
        match expr with
        | Expr.Var _ | Expr.Lit _ | Expr.PropAcc _ | Expr.App _
        | Expr.MkArray _ | Expr.MkRecord _ -> true
        | _ -> false

    let rec private emitExpr (expr: Expr<'v>) : string =
        match expr with
        | Expr.Lit x -> emitLiteral x.value
        | Expr.Var x -> x.ident
        | Expr.App x ->
            let func = emitExpr x.func
            let arg = emitExpr x.arg
            let funcPart = if isAtomic x.func then func else sprintf "(%s)" func
            sprintf "%s(%s)" funcPart arg
        | Expr.Fun x ->
            sprintf "(%s) => %s" x.ident.identName (emitExpr x.body)
        | Expr.Let x ->
            sprintf "((%s) => %s)(%s)"
                x.ident.identName (emitExpr x.body) (emitExpr x.value)
        | Expr.Do x ->
            sprintf "(%s, %s)" (emitExpr x.action) (emitExpr x.body)
        | Expr.PropAcc x ->
            let src = emitExpr x.source
            let srcPart = if isAtomic x.source then src else sprintf "(%s)" src
            sprintf "%s.%s" srcPart x.ident.identName
        | Expr.MkArray x ->
            x.values
            |> List.map emitExpr
            |> String.concat ", "
            |> sprintf "[%s]"
        | Expr.MkRecord x ->
            let mutable posIdx = 0
            let parts =
                x.items
                |> List.map (fun item ->
                    match item with
                    | RecordItem.Property f ->
                        sprintf "%s: %s" f.fname (emitExpr f.value)
                    | RecordItem.Positional v ->
                        let name = sprintf "_%d" posIdx
                        posIdx <- posIdx + 1
                        sprintf "%s: %s" name (emitExpr v))
                |> String.concat ", "
            sprintf "{%s}" parts
        | Expr.Match x ->
            let sb = StringBuilder()
            sb.Append("((__s) => {") |> ignore
            for arm in x.arms do
                match arm.pattern with
                | MatchPattern.Literal l ->
                    sb.Append(sprintf " if (__s === %s) return %s;"
                                (emitLiteral l.value) (emitExpr arm.body)) |> ignore
                | MatchPattern.Var v ->
                    sb.Append(sprintf " return ((%s) => %s)(__s);"
                                v.identName (emitExpr arm.body)) |> ignore
                | MatchPattern.Wildcard _ ->
                    sb.Append(sprintf " return %s;" (emitExpr arm.body)) |> ignore
            sb.Append(" })(") |> ignore
            sb.Append(emitExpr x.scrutinee) |> ignore
            sb.Append(")") |> ignore
            sb.ToString()

    /// Emit a single JavaScript expression. Pure, side-effect-free string.
    let toJs (expr: Expr<'v>) : string =
        emitExpr expr

    /// Emit a self-contained JS module: `const`-bind each import, then
    /// `export default` the compiled expression. Imports are raw JS snippets
    /// (e.g. `("log", "(s) => console.log(s)")`) that become accessible
    /// inside the expression under the given TypeFighter identifier.
    let toJsModule (imports: (string * string) list) (expr: Expr<'v>) : string =
        let sb = StringBuilder()
        for name, body in imports do
            sb.Append(sprintf "const %s = %s;\n" name body) |> ignore
        sb.Append("export default ") |> ignore
        sb.Append(emitExpr expr) |> ignore
        sb.Append(";\n") |> ignore
        sb.ToString()
