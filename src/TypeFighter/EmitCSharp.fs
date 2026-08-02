// Copyright (c) Ronald Schlenker
// Evaluation use only. Commercial use prohibited.

module PureState.TypeFighter.EmitCSharp

// Legacy C#-side codegen for an external host: `emit` walks a typed
// `Expr<VarNum>` and produces a C# expression string that calls into a
// runtime `Ops` class supplied by the host (e.g. Host.Ops). The
// TypeFighter-Web language uses `Parser01.EmitCSharp` instead; the shared
// Roslyn pipeline (compile, references) lives in `PureState.TypeFighter.Roslyn`.

open System
open System.Text

// ---------------------------------------------------------------------------
// Source emit (typed Expr<VarNum> -> C# expression body)

let private esc (s: string) =
    s.Replace("\\", "\\\\").Replace("\"", "\\\"")

let private dToLit (d: double) =
    if Double.IsNaN d then "double.NaN"
    elif Double.IsPositiveInfinity d then "double.PositiveInfinity"
    elif Double.IsNegativeInfinity d then "double.NegativeInfinity"
    else
        let culture = Globalization.CultureInfo.InvariantCulture
        let s = d.ToString("R", culture)
        if s.IndexOfAny([| '.'; 'e'; 'E'; 'N'; 'I' |]) < 0 then s + "D" else s

let private opMethod = function
    | "+" -> Some "Add" | "-" -> Some "Sub" | "*" -> Some "Mul"
    | "/" -> Some "Div" | "^" -> Some "Pow" | "&" -> Some "Concat"
    | "=" -> Some "Eq" | "<>" -> Some "Ne"
    | "<" -> Some "Lt" | "<=" -> Some "Le"
    | ">" -> Some "Gt" | ">=" -> Some "Ge"
    | "neg" -> Some "Neg" | "uplus" -> Some "UPlus" | "percent" -> Some "Percent"
    | _ -> None

let private isLazyFn = function
    | "if" | "iferror" | "and" | "or" -> true
    | _ -> false

/// Decompose a curried application chain into (head, args) where args
/// are in left-to-right surface order: `App(App(f, a), b)` -> `(f, [a; b])`.
let private collectApp (e: Expr<VarNum>) =
    let rec loop e args =
        match e with
        | Expr.App app -> loop app.func (app.arg :: args)
        | _ -> e, args
    loop e []

/// Flatten nested binary `and`/`or` chains so we can emit a single
/// `EvalAnd`/`EvalOr` with all leaves. The translator right-folds these
/// as binary; the runtime helper takes an N-array.
let private flattenLazyBinary (name: string) (e: Expr<VarNum>) =
    let rec loop e acc =
        match collectApp e with
        | Expr.Var v, [a; b] when v.ident = name ->
            loop a (loop b acc)
        | _ -> e :: acc
    loop e []

/// Walk a typed TF expression and return a C# expression string. The
/// emitted code calls into an `Ops` static class with the conventions
/// listed at the top of this file.
let emit (expr: Expr<VarNum>) : string =
    let sb = StringBuilder()

    let rec emitExpr (e: Expr<VarNum>) =
        match e with
        | Expr.Lit lit ->
            match lit.value with
            | Number d ->
                sb.Append("Ops.Num(").Append(dToLit d).Append(')') |> ignore
            | String s ->
                sb.Append("Ops.Str(\"").Append(esc s).Append("\")") |> ignore
            | Boolean b ->
                sb.Append("Ops.Bool(").Append(if b then "true" else "false").Append(')') |> ignore
        | Expr.Var v when v.ident.StartsWith "__notimpl__:" ->
            // Range / wildcard / selector refs land here; produce a runtime
            // NotImplemented error instead of trying to resolve them.
            let raw = v.ident.Substring "__notimpl__:".Length
            sb.Append("Ops.RefNotImplemented(\"")
              .Append(esc raw).Append("\")") |> ignore
        | Expr.Var v when v.ident.StartsWith "//" ->
            sb.Append("Ops.RefRead(__resolver, \"")
              .Append(esc v.ident).Append("\")") |> ignore
        | Expr.Var v ->
            // Nullary library function, e.g. `pi()`, `true()`, `now()`.
            sb.Append("Ops.CallFn(__registry, \"")
              .Append(esc v.ident).Append("\", new CalcValue[] { })") |> ignore
        | Expr.App _ ->
            let head, args = collectApp e
            emitCall head args
        | other ->
            failwithf "CSharp.emit: unsupported expression: %A" other

    and emitCall (head: Expr<VarNum>) (args: Expr<VarNum> list) =
        match head with
        | Expr.Var v ->
            match opMethod v.ident with
            | Some opName -> emitOp opName args
            | None when isLazyFn v.ident -> emitLazy v.ident args
            | None -> emitGenericCall v.ident args
        | other ->
            failwithf "CSharp.emit: call head must be a Var, got %A" other

    and emitOp (opName: string) (args: Expr<VarNum> list) =
        sb.Append("Ops.").Append(opName).Append('(') |> ignore
        args |> List.iteri (fun i a ->
            if i > 0 then sb.Append(", ") |> ignore
            emitExpr a)
        sb.Append(')') |> ignore

    and emitLazy (name: string) (args: Expr<VarNum> list) =
        match name, args with
        | "if", [c; t; e] ->
            sb.Append("Ops.EvalIf(") |> ignore
            emitExpr c
            sb.Append(", () => ") |> ignore
            emitExpr t
            sb.Append(", () => ") |> ignore
            emitExpr e
            sb.Append(')') |> ignore
        | "iferror", [v; alt] ->
            sb.Append("Ops.EvalIfError(") |> ignore
            emitExpr v
            sb.Append(", () => ") |> ignore
            emitExpr alt
            sb.Append(')') |> ignore
        | "and", _ | "or", _ ->
            let parts =
                args
                |> List.collect (fun arg -> flattenLazyBinary name arg)
            let helper = if name = "and" then "EvalAnd" else "EvalOr"
            sb.Append("Ops.").Append(helper)
              .Append("(new System.Func<CalcValue>[] { ") |> ignore
            parts |> List.iteri (fun i p ->
                if i > 0 then sb.Append(", ") |> ignore
                sb.Append("() => ") |> ignore
                emitExpr p)
            sb.Append(" })") |> ignore
        | _ ->
            emitGenericCall name args

    and emitGenericCall (name: string) (args: Expr<VarNum> list) =
        sb.Append("Ops.CallFn(__registry, \"")
          .Append(esc name)
          .Append("\", new CalcValue[] { ") |> ignore
        args |> List.iteri (fun i a ->
            if i > 0 then sb.Append(", ") |> ignore
            emitExpr a)
        sb.Append(" })") |> ignore

    emitExpr expr
    sb.ToString()
