#load "../TestHelperFsiOverrides.fsx"

open TestHelperFsiOverrides
open TypeFighter



(*
    let isPositive = ((x) => x > 0)
    let theNumber = 42
    [ isPositive theNumber; isPositive 0 ]
*)
let env =
    [
        "gt", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.boolean)
    ]

X.Let (X.Ident "isPositive") 
    (X.Fun (X.Ident "x") 
        (X.App (X.App (X.Var "gt") (X.Var "x")) (X.Lit 0))
    )
    (X.Let (X.Ident "theNumber") (X.Lit 42)
        (X.MkArray [
            X.App (X.Var "isPositive") (X.Var "theNumber")
            X.App (X.Var "isPositive") (X.Lit 0)
        ])
    )
|> solve env None

