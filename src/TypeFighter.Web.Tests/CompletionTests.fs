module PureState.TypeFighter.Web.Tests.CompletionTests

open NUnit.Framework
open FsUnit
open PureState.TypeFighter
open PureState.TypeFighter.Web

let private completionAt (marked: string) =
    let offset = marked.IndexOf '|'
    if offset < 0 then failwith "test source needs a | cursor marker"
    Api.completionAtCursor [] (marked.Replace("|", "")) offset

let private memberFieldsAt marked =
    let completion = completionAt marked
    completion.isMemberAccess |> should equal true
    completion.fields |> List.ofArray

let private shouldNotBeMemberAccess marked =
    let completion = completionAt marked
    completion.isMemberAccess |> should equal false
    completion.fields |> should be Empty

[<Test>]
let ``dot after a record binding lists its fields`` () =
    memberFieldsAt "p = { x: 1, y: 2 }\np.|" |> should equal [ "x"; "y" ]

[<Test>]
let ``partial member keeps the full field list`` () =
    // Filtering by the typed prefix is the editor's job, not the solver's.
    memberFieldsAt "p = { x: 1, y: 2 }\np.x|" |> should equal [ "x"; "y" ]

[<Test>]
let ``opening a quoted member keeps completion active`` () =
    memberFieldsAt "p = { `Field One`: 1, `Field Two`: 2 }\np.`|"
    |> should equal [ "Field One"; "Field Two" ]

[<Test>]
let ``partial quoted member keeps the full field list`` () =
    memberFieldsAt "p = { `Field One`: 1, `Field Two`: 2 }\np.`Field |"
    |> should equal [ "Field One"; "Field Two" ]

[<Test>]
let ``nested chain resolves the inner record`` () =
    memberFieldsAt "u = { user: { id: 1, name: \"Ada\" } }\nu.user.|"
    |> should equal [ "id"; "name" ]

[<Test>]
let ``fields come back sorted`` () =
    memberFieldsAt "p = { zeta: 1, alpha: 2, mid: 3 }\np.|"
    |> should equal [ "alpha"; "mid"; "zeta" ]

[<Test>]
let ``binding several lines above is still in scope`` () =
    memberFieldsAt "a = 1\nb = 2\np = { x: 1, y: 2 }\np.|" |> should equal [ "x"; "y" ]

[<Test>]
let ``lines after the cursor are ignored`` () =
    memberFieldsAt "p = { x: 1, y: 2 }\np.|\nq = 5\nq" |> should equal [ "x"; "y" ]

[<Test>]
let ``a syntax error after the cursor does not break completion`` () =
    memberFieldsAt "p = { x: 1, y: 2 }\np.|\n???" |> should equal [ "x"; "y" ]

[<Test>]
let ``a binding defined after the cursor is not visible`` () =
    memberFieldsAt "p = { x: 1 }\nq.|\nq = { a: 1, b: 2 }" |> should be Empty

[<Test>]
let ``namespace lists its members`` () =
    let members = memberFieldsAt "gen.|"
    members |> should contain "counter"
    members |> should contain "sineOsc"

[<Test>]
let ``namespace works with a statement prefix present`` () =
    memberFieldsAt "a = 1\nmath.|" |> should contain "ema"

[<Test>]
let ``host injected number record lists its fields`` () =
    let fields = [
        "CPU_LOAD", BuiltinTypes.number
        "MEMORY_USED", BuiltinTypes.number
    ]
    let extraPrelude = [ "values", Mono (TDef.RecordWith fields) ]
    let source = "values."
    let completion = Api.completionAtCursor extraPrelude source source.Length
    completion.isMemberAccess |> should equal true
    completion.fields
    |> List.ofArray
    |> should equal [ "CPU_LOAD"; "MEMORY_USED" ]

[<Test>]
let ``no dot means no completion`` () =
    shouldNotBeMemberAccess "p = { x: 1 }\np|"

[<Test>]
let ``unknown receiver yields nothing`` () =
    memberFieldsAt "zzz.|" |> should be Empty

[<Test>]
let ``dot without a receiver yields nothing`` () =
    shouldNotBeMemberAccess ".|"

[<Test>]
let ``empty source yields nothing`` () =
    shouldNotBeMemberAccess "|"

[<Test>]
let ``unclosed quoted identifier without a receiver is not member access`` () =
    shouldNotBeMemberAccess "`Field |"

[<Test>]
let ``non-record receiver yields nothing`` () =
    memberFieldsAt "n = 42\nn.|" |> should be Empty

[<Test>]
let ``a lifted stream binding is a Number, not a record`` () =
    memberFieldsAt "i = count\ni.|" |> should be Empty

[<Test>]
let ``semicolon-separated source works too`` () =
    memberFieldsAt "p = { x: 1, y: 2 };p.|" |> should equal [ "x"; "y" ]

[<Test>]
let ``mixed separators resolve against the nearest statement`` () =
    memberFieldsAt "a = 1;b = 2\np = { x: 1, y: 2 }\np.|" |> should equal [ "x"; "y" ]

[<Test>]
let ``offset zero yields nothing`` () =
    let completion = Api.completionAtCursor [] "p = { x: 1 }\np." 0
    completion.isMemberAccess |> should equal false
    completion.fields |> should be Empty

[<Test>]
let ``offset past the end is clamped to the end`` () =
    let completion = Api.completionAtCursor [] "p = { x: 1, y: 2 }\np." 9999
    completion.isMemberAccess |> should equal true
    completion.fields
    |> List.ofArray
    |> should equal [ "x"; "y" ]

[<Test>]
let ``negative offset is clamped to zero`` () =
    let completion = Api.completionAtCursor [] "p = { x: 1 }\np." -5
    completion.isMemberAccess |> should equal false
    completion.fields |> should be Empty

[<Test>]
let ``cursor before the dot does not complete`` () =
    shouldNotBeMemberAccess "p = { x: 1, y: 2 }\np|."

[<Test>]
let ``repeated call returns the same result`` () =
    let src = "p = { x: 1, y: 2 }\np."
    let first = Api.completionAtCursor [] src src.Length
    let second = Api.completionAtCursor [] src src.Length
    second |> should equal first

[<Test>]
let ``different receivers in the same source do not share a cache entry`` () =
    let src = "p = { x: 1 }\nq = { a: 1, b: 2 }\n"
    let forP = Api.completionAtCursor [] (src + "p.") (src.Length + 2)
    let forQ = Api.completionAtCursor [] (src + "q.") (src.Length + 2)
    forP.isMemberAccess |> should equal true
    forQ.isMemberAccess |> should equal true
    let forPFields = forP.fields |> List.ofArray
    let forQFields = forQ.fields |> List.ofArray
    forPFields |> should equal [ "x" ]
    forQFields |> should equal [ "a"; "b" ]

[<Test>]
let ``call result as receiver resolves its fields`` () =
    memberFieldsAt "f = a => { m: 1, n: 2 }\nf(0).|"
    |> should equal [ "m"; "n" ]

[<Test>]
let ``record literal as receiver resolves its fields`` () =
    memberFieldsAt "{ a: 1, b: 2 }.|" |> should equal [ "a"; "b" ]


// The property being completed can sit anywhere in an expression, not just at
// the end of a statement -- `values.A + values.` is the everyday case.

let private p = "p = { x: 1, y: 2 }\n"

[<Test>]
let ``second operand of an operator completes`` () =
    memberFieldsAt (p + "1 + p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``property after another property of the same record completes`` () =
    memberFieldsAt (p + "p.x + p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``partial property in an operand keeps the full field list`` () =
    memberFieldsAt (p + "p.x + p.y|") |> should equal [ "x"; "y" ]

[<Test>]
let ``argument of an unclosed call completes`` () =
    memberFieldsAt (p + "f = a => a\nf(p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``second argument of an unclosed call completes`` () =
    memberFieldsAt (p + "f = a => b => a\nf(1, p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``element of an unclosed array completes`` () =
    memberFieldsAt (p + "[1, p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``value of an unclosed record completes`` () =
    memberFieldsAt (p + "{ a: 1, b: p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``lambda body completes`` () =
    memberFieldsAt (p + "g = a => p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``value of a binding that is an expression completes`` () =
    memberFieldsAt (p + "q = 1 + p.|\nq") |> should equal [ "x"; "y" ]

[<Test>]
let ``then branch of an if without an else completes`` () =
    memberFieldsAt (p + "if true then p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``condition of an if without its branches completes`` () =
    memberFieldsAt (p + "if p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``else branch completes`` () =
    memberFieldsAt (p + "if true then 1 else p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``deeply nested position completes`` () =
    memberFieldsAt (p + "f = a => a\nf(1 + f((2 * p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``chain in an operand resolves the inner record`` () =
    memberFieldsAt "u = { user: { id: 1, name: \"Ada\" } }\n1 + u.user.|"
    |> should equal [ "id"; "name" ]

[<Test>]
let ``quoted member in an operand keeps completion active`` () =
    memberFieldsAt "p = { `Field One`: 1, `Field Two`: 2 }\n1 + p.`Field |"
    |> should equal [ "Field One"; "Field Two" ]

[<Test>]
let ``a type error elsewhere in the line does not break completion`` () =
    memberFieldsAt (p + "\"txt\" * 3 + p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``a bracket inside a string opens nothing`` () =
    memberFieldsAt (p + "s = \"a(b\"\n1 + p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``a bracket inside a comment opens nothing`` () =
    memberFieldsAt (p + "// f(x\n1 + p.|") |> should equal [ "x"; "y" ]

[<Test>]
let ``a bracket inside a quoted identifier opens nothing`` () =
    memberFieldsAt "p = { `A(1)`: 1, `B`: 2 }\n1 + p.|" |> should equal [ "A(1)"; "B" ]

[<Test>]
let ``cursor behind a finished property does not complete`` () =
    // The property ended a token ago -- what is being written there is new.
    shouldNotBeMemberAccess (p + "1 + p.x |")

[<Test>]
let ``a plain identifier in an operand is not member access`` () =
    shouldNotBeMemberAccess (p + "1 + p|")
