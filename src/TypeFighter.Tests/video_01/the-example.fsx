#load "../TestHelperFsiOverrides.fsx"

open TypeFighter.Tests.TestHelper
open TestHelperFsiOverrides
open TypeFighter



(*
    ((x) => isPositive x) 42
*)
let env =
    [
        "isPositive", Mono (BuiltinTypes.number ^-> BuiltinTypes.boolean)
    ]
X.App
    (X.Fun (X.Ident "x")
        (X.App
            (X.Var "isPositive")
            (X.Var "x")))
    (X.Lit 42)
|> solve env None



(*
    let x = 42
    x > 0
*)
X.Let (X.Ident "x") (X.Lit 42)
    (X.App
        (X.App
            (X.Var "greaterThan")
            (X.Var "x"))
        (X.Lit 0))
|> solve
    [
        "greaterThan", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.boolean)
    ]
    None



(*
    ((x) => x > 0) 42
*)
X.App
    (X.Fun (X.Ident "x")
        (X.App
            (X.App
                (X.Var "greaterThan")
                (X.Var "x"))
            (X.Lit 0)))
    (X.Lit 42)
|> solve
    [
        "greaterThan", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.boolean)
    ]
    None



(*
    (x) => x
*)
X.Fun (X.Ident "x") (X.Var "x")
|> solve [] None




(*
    [ 1; 2; 3 ]
*)

X.MkArray [ X.Lit 1; X.Lit 2; X.Lit 3 ]
|> solve [] None
|> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.number))



(*
    [ "a"; "b"; "c" ]
*)

X.MkArray [ X.Lit "a"; X.Lit "b"; X.Lit "c" ]
|> solve [] None
|> shouldSolveType (Mono (BuiltinTypes.array BuiltinTypes.string))




(*
    [ "a"; 1; "c" ]
*)
// ERROR: Can't unify String and Number

X.MkArray [ X.Lit "a"; X.Lit 1; X.Lit "c" ]
|> solve [] None
|> shouldFail


(*
    let name = "Alice"
    let age = 30
    { name = name; age = age }
*)
X.Let (X.Ident "name") (X.Lit "Alice")
    (X.Let (X.Ident "age") (X.Lit 30)
        (X.MkRecord [
            X.Field "name" (X.Var "name")
            X.Field "age" (X.Var "age")
        ]))
|> solve [] None
|> shouldSolveType (Mono (TDef.NamedRecordWith (NameHint.Given "Record") [ "name", BuiltinTypes.string; "age", BuiltinTypes.number ]))



(*
addNumbers 41 "one"
*)
X.App
    (X.App
        (X.Var "addNumbers")
        (X.Lit 41))
    (X.Lit "one")
|> solve
    [
        "addNumbers", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
    ]
    None
|> shouldFail




(*
    [
        { validFrom = MkThing "foo1" };
        { validFrom = MkThing "foo2" };
    ]
*)

let defaultTcEnv =
    [
        "MkThing", TDef.Generalize (BuiltinTypes.string ^-> %1)
    ]

// TODO: Comparison of poly types according to poly type params naming/numbering has to be implemented correctly
//       + apply reindexing of vars (see reindex vars for external envs)
//       %14 - that's the thing here.
X.MkArray
    [
        X.MkRecord [
            X.Field
                "validFrom"
                (X.App
                    (X.Var "MkThing")
                    (X.Lit "foo1")) 
        ]
        X.MkRecord [
            X.Field
                "validFrom"
                (X.App
                    (X.Var "MkThing")
                    (X.Lit "foo2")) 
        ]
    ]
|> solve defaultTcEnv None
|> shouldSolveType (TDef.Generalize (BuiltinTypes.array (TDef.NamedRecordWith (NameHint.Given "Record") [ "validFrom", %10 ])))






(*
    let isPositive = ((x) => x > 0)
    let theNumber = 42
    [ isPositive theNumber; isPositive 0 ]
*)

X.Let (X.Ident "isPositive") 
    (X.Fun (X.Ident "x") 
        (X.App (X.App (X.Var "greaterThan") (X.Var "x")) (X.Lit 0))
    )
    (X.Let (X.Ident "theNumber") (X.Lit 42)
        (X.MkArray [
            X.App (X.Var "isPositive") (X.Var "theNumber")
            X.App (X.Var "isPositive") (X.Lit 0)
        ])
    )
|> solve 
    [
        "greaterThan", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.boolean)
    ]
    None



(*
    let x = 42
    toString x
*)

X.Let (X.Ident "x") (X.Lit 42)
    (X.App (X.Var "toString") (X.Var "x"))
|> solve
    [
        "toString", Mono (BuiltinTypes.number ^-> BuiltinTypes.string)
    ]
    None

    


(*
    map ((x => x > 0)) [1; 2; 3]
*)

X.App 
    (X.App 
        (X.Var "map")
        (X.Fun (X.Ident "x") 
            (X.App (X.App (X.Var "greaterThan") (X.Var "x")) (X.Lit 0))))
    (X.MkArray [ X.Lit 1; X.Lit 2; X.Lit 3 ])
|> solve
    [
        "map", TDef.Generalize ((%1 ^-> %2) ^-> BuiltinTypes.array %1 ^-> BuiltinTypes.array %2)
        "greaterThan", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.boolean)
    ]
    None


(*
    if (isPositive 42)
        return "yes";

    let x = "hello";
    return x + " world";
*)

// Desugared (early return becomes match on boolean):
// match (isPositive 42) with
// | True -> "yes"
// | False -> let x = "hello" in x + " world"

X.Match
    (X.App (X.Var "isPositive") (X.Lit 42))
    [
        X.Case "True" None (X.Lit "yes")
        X.Case "False" None 
            (X.Let (X.Ident "x") (X.Lit "hello")
                (X.App (X.App (X.Var "concat") (X.Var "x")) (X.Lit " world")))
    ]
|> solve
    [
        "isPositive", Mono (BuiltinTypes.number ^-> BuiltinTypes.boolean)
        "concat", Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string)
    ]
    None



(*
let identity = (x) => x
identity

Final type after generalization:
    Some(<tv_1>.(tv_1 -> tv_1))
*)

X.Let (X.Ident "identity")
    (X.Fun (X.Ident "x") (X.Var "x"))
    (X.Var "identity")
|> solve [] None
|> shouldSolveType (TDef.Generalize (%1 ^-> %1))
