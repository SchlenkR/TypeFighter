namespace TypeFighter

/// Ident walker — surfaces every identifier occurrence in a numbered
/// Expr together with its associated TVar so tools (hover, go-to-def)
/// can correlate source locations to inferred types.
///
/// Lives in the TypeFighter assembly because `Expr`'s case constructors
/// are `internal`. Keeping the walker here avoids touching Lang.fs
/// while still giving external callers (TypeFighter.Web.Api) the
/// structural info they need.
///
/// Traversal order matches the surface-syntax order that the parser
/// sees when recording spans, so callers can zip the two lists (by
/// (name, kind, nth-by-name)) to pair a span with its TVar.
[<RequireQualifiedAccess>]
module IdentInfo =

    type IdentKind =
        | Var       // identifier used in expression position
        | Binding   // `let NAME = …` — ident of a Let
        | Param     // `(NAME) => …` — ident of a Fun
        | Field     // `expr.NAME` — ident of a PropAcc

    type Occurrence =
        { name: string
          kind: IdentKind
          tvar: VarNum }

    /// Walk the numbered Expr and emit every identifier occurrence in
    /// document (left-to-right, top-down) order.
    let collect (expr: Expr<VarNum>) : Occurrence list =
        let out = ResizeArray<Occurrence>()

        let rec walk (e: Expr<VarNum>) =
            match e with
            | Expr.Lit _ -> ()
            | Expr.Var x ->
                out.Add { name = x.ident; kind = Var; tvar = x.tvar }
            | Expr.App x ->
                walk x.func
                walk x.arg
            | Expr.Fun x ->
                out.Add { name = x.ident.identName; kind = Param; tvar = x.ident.tvar }
                walk x.body
            | Expr.Let x ->
                out.Add { name = x.ident.identName; kind = Binding; tvar = x.ident.tvar }
                walk x.value
                walk x.body
            | Expr.Do x ->
                walk x.action
                walk x.body
            | Expr.PropAcc x ->
                // The source expression contains the field owner; the
                // field name comes *after* in source order.
                walk x.source
                out.Add { name = x.ident.identName; kind = Field; tvar = x.ident.tvar }
            | Expr.MkArray x ->
                for v in x.values do walk v
            | Expr.MkRecord x ->
                for item in x.items do
                    match item with
                    | RecordItem.Property f -> walk f.value
                    | RecordItem.Positional v -> walk v
            | Expr.Match x ->
                walk x.scrutinee
                for arm in x.arms do
                    match arm.pattern with
                    | MatchPattern.Literal _ -> ()
                    | MatchPattern.Var ident ->
                        out.Add { name = ident.identName; kind = Binding; tvar = ident.tvar }
                    | MatchPattern.Wildcard _ -> ()
                    walk arm.body

        walk expr
        List.ofSeq out
