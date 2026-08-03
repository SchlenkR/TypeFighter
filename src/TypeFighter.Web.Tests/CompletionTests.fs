module PureState.TypeFighter.Web.Tests.CompletionTests

open NUnit.Framework
open FsUnit
open PureState.TypeFighter.Web

// Cursor position is written as `|` in the source and stripped before the
// call, so the tests read like what a user actually types.
let private completionsAt (marked: string) : string list =
    let offset = marked.IndexOf '|'
    if offset < 0 then failwith "test source needs a | cursor marker"
    Api.completionAtCursor [] (marked.Replace("|", "")) offset |> List.ofArray


// ============================================================================
// Line-based source — what the editor actually sends
// ============================================================================

[<Test>]
let ``dot after a record binding lists its fields`` () =
    completionsAt "p = { x: 1, y: 2 }\np.|" |> should equal [ "x"; "y" ]

[<Test>]
let ``partial member keeps the full field list`` () =
    // Filtering by the typed prefix is the editor's job, not the solver's.
    completionsAt "p = { x: 1, y: 2 }\np.x|" |> should equal [ "x"; "y" ]

[<Test>]
let ``nested chain resolves the inner record`` () =
    completionsAt "u = { user: { id: 1, name: \"Ada\" } }\nu.user.|"
    |> should equal [ "id"; "name" ]

[<Test>]
let ``fields come back sorted`` () =
    completionsAt "p = { zeta: 1, alpha: 2, mid: 3 }\np.|"
    |> should equal [ "alpha"; "mid"; "zeta" ]

[<Test>]
let ``binding several lines above is still in scope`` () =
    completionsAt "a = 1\nb = 2\np = { x: 1, y: 2 }\np.|" |> should equal [ "x"; "y" ]


// ============================================================================
// Everything after the cursor is discarded
// ============================================================================

[<Test>]
let ``lines after the cursor are ignored`` () =
    completionsAt "p = { x: 1, y: 2 }\np.|\nq = 5\nq" |> should equal [ "x"; "y" ]

[<Test>]
let ``a syntax error after the cursor does not break completion`` () =
    completionsAt "p = { x: 1, y: 2 }\np.|\n???" |> should equal [ "x"; "y" ]

[<Test>]
let ``a binding defined after the cursor is not visible`` () =
    // Consequence of cutting at the cursor: `q` does not exist yet.
    completionsAt "p = { x: 1 }\nq.|\nq = { a: 1, b: 2 }" |> should be Empty


// ============================================================================
// Prelude namespaces
// ============================================================================

[<Test>]
let ``namespace lists its members`` () =
    let members = completionsAt "gen.|"
    members |> should contain "counter"
    members |> should contain "sineOsc"

[<Test>]
let ``namespace works with a statement prefix present`` () =
    completionsAt "a = 1\nmath.|" |> should contain "ema"


// ============================================================================
// Non-completion positions return nothing
// ============================================================================

[<Test>]
let ``no dot means no completion`` () =
    completionsAt "p = { x: 1 }\np|" |> should be Empty

[<Test>]
let ``unknown receiver yields nothing`` () =
    completionsAt "zzz.|" |> should be Empty

[<Test>]
let ``dot without a receiver yields nothing`` () =
    completionsAt ".|" |> should be Empty

[<Test>]
let ``empty source yields nothing`` () =
    completionsAt "|" |> should be Empty

[<Test>]
let ``non-record receiver yields nothing`` () =
    completionsAt "n = 42\nn.|" |> should be Empty

[<Test>]
let ``a lifted stream binding is a Number, not a record`` () =
    // `count` is a step-record, but the let-lift makes `i` the per-tick
    // value — so there are no protocol fields to offer.
    completionsAt "i = count\ni.|" |> should be Empty


// ============================================================================
// Semicolon form (stdlib and host-synthesised source)
// ============================================================================

[<Test>]
let ``semicolon-separated source works too`` () =
    completionsAt "p = { x: 1, y: 2 };p.|" |> should equal [ "x"; "y" ]

[<Test>]
let ``mixed separators resolve against the nearest statement`` () =
    completionsAt "a = 1;b = 2\np = { x: 1, y: 2 }\np.|" |> should equal [ "x"; "y" ]


// ============================================================================
// Cursor offset handling
// ============================================================================

[<Test>]
let ``offset zero yields nothing`` () =
    Api.completionAtCursor [] "p = { x: 1 }\np." 0 |> should be Empty

[<Test>]
let ``offset past the end is clamped to the end`` () =
    Api.completionAtCursor [] "p = { x: 1, y: 2 }\np." 9999
    |> List.ofArray
    |> should equal [ "x"; "y" ]

[<Test>]
let ``negative offset is clamped to zero`` () =
    Api.completionAtCursor [] "p = { x: 1 }\np." -5 |> should be Empty

[<Test>]
let ``cursor before the dot does not complete`` () =
    completionsAt "p = { x: 1, y: 2 }\np|." |> should be Empty


// ============================================================================
// Caching — repeated calls must agree
// ============================================================================

[<Test>]
let ``repeated call returns the same result`` () =
    let src = "p = { x: 1, y: 2 }\np."
    let first = Api.completionAtCursor [] src src.Length
    let second = Api.completionAtCursor [] src src.Length
    second |> should equal first

[<Test>]
let ``different receivers in the same source do not share a cache entry`` () =
    let src = "p = { x: 1 }\nq = { a: 1, b: 2 }\n"
    let forP = Api.completionAtCursor [] (src + "p.") (src.Length + 2) |> List.ofArray
    let forQ = Api.completionAtCursor [] (src + "q.") (src.Length + 2) |> List.ofArray
    forP |> should equal [ "x" ]
    forQ |> should equal [ "a"; "b" ]


// ============================================================================
// Known limits of the regex-based receiver detection
// ============================================================================

[<Test>]
let ``call result as receiver is not supported`` () =
    // The receiver pattern matches identifier chains only.
    completionsAt "f = a => { m: 1, n: 2 }\nf(0).|" |> should be Empty

[<Test>]
let ``record literal as receiver is not supported`` () =
    completionsAt "{ a: 1, b: 2 }.|" |> should be Empty
