
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Polymorphism
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open TypeFighter


// =================================================================
// Polymorphism (via env-supplied polytypes)
// -----------------------------------------------------------------
// Polytypes provided through the environment are instantiated fresh
// at every use site — the same name can be used at different types.
// For AST-level `let` generalization, see Let.fs.
// =================================================================


(*
    Env:
      log : forall a. a -> Unit

    Source:    log "Hello";
               log 88;
               42
    Inferred:  Number
*)
// `log` is polymorphic, so its two usages can take different types.
// *Fresh instantiation*: every time a polytype like `forall a. a -> Unit`
// is looked up, the `a` gets replaced by a brand-new type variable —
// one for this use site, one for the next. That's what lets the first
// call fix `a` to `String` while the second fixes it to `Number`
// without the two use sites clashing.
let [<Test>] ``polymorphic "log"`` () =
    X.Do (X.App (X.Var "log") (X.Lit "Hello")) (
        X.Do (X.App (X.Var "log") (X.Lit 88)) (
            X.Lit 42
        )
    )
    |> solve
        [
            yield "log", TDef.Generalize (%1 ^-> BuiltinTypes.unit)
        ]
        None
    |> shouldSolveType (Mono BuiltinTypes.number)


(*
    Env:
      mkPoly : forall a. a -> a

    Source:    {
                   r1 = mkPoly true
                   r2 = mkPoly 33
               }
    Inferred:  { r1: Bool; r2: Number }
*)
// The more interesting shape of fresh instantiation: both uses of
// `mkPoly` appear inside the *same* record literal, side by side.
// Yet each call gets its own fresh instantiation — one at `Bool`,
// one at `Number` — and the record assembles their results into a
// heterogeneous shape. If instantiation weren't fresh per use, the
// two calls would have to agree and the record couldn't hold
// different types.
let [<Test>] ``polymorphic "mkPoly" with infered record`` () =
    X.MkRecord [
        X.Property "r1" (X.App (X.Var "mkPoly") (X.Lit true))
        X.Property "r2" (X.App (X.Var "mkPoly") (X.Lit 33))
    ]
    |> solve
        [
            "mkPoly", TDef.Generalize (%1 ^-> %1)
        ]
        None
    |> shouldSolveType (
        Mono (TDef.RecordWith [
            "r1", BuiltinTypes.boolean
            "r2", BuiltinTypes.number
        ]))
