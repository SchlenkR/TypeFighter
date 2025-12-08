#load "../TestHelperFsiOverrides.fsx"

open TypeFighter.Tests.TestHelper
open TestHelperFsiOverrides
open TypeFighter



(*
        
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
    (x) => x
*)
X.Fun (X.Ident "x") (X.Var "x")
|> solve [] None



(*

ERROR cases:
    - applying a string to the "twice" function

*)





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
    [
        { validFrom = MkThing "foo1" };
        { validFrom = MkThing "foo2" };
    ]
*)

let defaultTcEnv =
    [
        "MkThing", TDef.Generalize (BuiltinTypes.string ^-> %1)
    ]

let ast =
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

// TODO: Comparison of poly types according to poly type params naming/numbering has to be implemented correctly
//       + apply reindexing of vars (see reindex vars for external envs)
//       %14 - that's the thing here.
ast 
|> solve defaultTcEnv None
|> shouldSolveType (TDef.Generalize (BuiltinTypes.array (TDef.NamedRecordWith (NameHint.Given "Record") [ "validFrom", %12 ])))
