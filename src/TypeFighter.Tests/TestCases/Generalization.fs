
#if INTERACTIVE
#load "../TestHelperFsiOverrides.fsx"
#else
module TypeFighter.Tests.Generalization
#endif

open TypeFighter.Tests.TestHelper

#if INTERACTIVE
open TestHelperFsiOverrides
#endif

open NUnit.Framework
open FsUnit
open TypeFighter


// =================================================================
// Generalization
// -----------------------------------------------------------------
// Turning a MonoTyp with free type variables into a PolyTyp binds
// those variables as forall-quantified.
//
// Alpha-equivalent polytypes (same shape, different var numbers)
// are normalized to a canonical numbering 0, 1, 2, … in traversal
// order, so structural F# equality on polytypes directly reflects
// type equality.
//
// These tests work on `Typ.gen` directly — no expression, no solver.
// =================================================================


(*
    Input:     Number
    Output:    Number           (stays Mono — no free vars to bind)
*)
// A few words of vocabulary:
//   • *Type variable* (written `%0`, `%1`, …): an unknown type, like a
//     placeholder `?`. The inferencer makes one whenever it can't yet
//     tell what a type should be.
//   • *Ground type*: a type with no variables in it — `Number`, `String`,
//     `Array<Bool>`. There's nothing left to fill in.
//   • *Mono* / *Poly*: `Mono τ` is a plain type. `Poly` wraps a type in
//     `forall …` quantifiers that declare which variables are free to
//     be chosen at each use site. `gen` is the function that *promotes*
//     a Mono to a Poly by quantifying its free variables.
// Since `Number` has nothing to quantify, `gen` returns it as-is.
let [<Test>] ``gen of a ground type stays Mono`` () =
    Typ.gen BuiltinTypes.number
    |> should equal (Mono BuiltinTypes.number)


(*
    Input:     %5 -> %5   and   %7 -> %7
    Output:   both canonicalize to   forall a. a -> a
*)
// *Alpha-equivalence*: two types that differ only in the *names* of
// their quantified variables are considered the same. `%5 -> %5` and
// `%7 -> %7` both describe "a function from some type to the same
// type" — the specific number attached to the variable is arbitrary
// label, not type content. `gen` canonicalises these labels so that
// structural equality on polytypes reflects real equivalence.
let [<Test>] ``alpha-equivalent polytypes are equal after gen`` () =
    let a = Typ.gen (%5 ^-> %5)
    let b = Typ.gen (%7 ^-> %7)
    a |> should equal b


(*
    Input:     %10 -> %5
    Output:    forall a b. a -> b
*)
// *Canonical numbering*: after generalization, the first type variable
// the traversal meets becomes 0, the next distinct one becomes 1, and
// so on. This is why two alpha-equivalent polytypes end up *literally
// identical* — they share the same numbering scheme.
let [<Test>] ``two distinct TVars are renumbered to 0 and 1`` () =
    Typ.gen (%10 ^-> %5)
    |> should equal (TDef.Generalize (%0 ^-> %1))


(*
    Input:     (%9 -> %4) -> %9 -> %4
    Output:    forall a b. (a -> b) -> a -> b
*)
// Canonical numbering still works when the same variables appear in
// several positions of a more complex type. The variable `%9` is
// seen first (inside the inner arrow and again in the second
// argument), so it becomes `a`; `%4` is seen second, so it becomes
// `b`. The repetition across positions is preserved — both
// occurrences of `%9` map to the *same* `a`.
let [<Test>] ``higher-order polytype canonicalizes`` () =
    Typ.gen ((%9 ^-> %4) ^-> %9 ^-> %4)
    |> should equal (TDef.Generalize ((%0 ^-> %1) ^-> %0 ^-> %1))
