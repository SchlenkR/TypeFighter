namespace TypeFighter

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
    static member Field field value = { fname = field; value = value; }
    static member Case disc ident body = { disc = disc; ident = ident; body = body }
