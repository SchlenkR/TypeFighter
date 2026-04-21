module TypeFighter.Web.Tests.ExamplesTests

open System.IO
open System.Text.Json
open NUnit.Framework

open TypeFighter.Web

// Smoke test that re-validates every playground example against the
// real compiler — the same code path the playground calls in the
// browser. Shares `examples.json` with the TS side (see
// `web/src/examples.ts`) so the examples can't drift from the
// advertised behavior.

type ExampleCase =
    { Category: string
      Title: string
      Source: string
      ExpectsError: bool }
    override this.ToString() =
        // NUnit uses ToString for the test-case label in the runner.
        sprintf "%s — %s" this.Category this.Title

// __SOURCE_DIRECTORY__ is captured at compile time; the JSON lives
// outside the src tree under web/src. Repo layout:
//   <repo>/src/TypeFighter.Web.Tests/   ← this file
//   <repo>/web/src/examples.json        ← shared examples
let private examplesPath =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "web", "src", "examples.json")

let private loadCases () : ExampleCase list =
    let json = File.ReadAllText(examplesPath)
    use doc = JsonDocument.Parse(json)
    [ for category in doc.RootElement.EnumerateArray() do
        let name = category.GetProperty("name").GetString()
        let expectsError =
            match category.TryGetProperty("expectsError") with
            | true, v -> v.GetBoolean()
            | _ -> false
        for example in category.GetProperty("examples").EnumerateArray() do
            yield
                { Category = name
                  Title = example.GetProperty("title").GetString()
                  Source = example.GetProperty("source").GetString()
                  ExpectsError = expectsError } ]

// NUnit requires TestCaseSource members to return boxed values.
let cases () : obj[] =
    loadCases ()
    |> List.map (fun c -> box c)
    |> Array.ofList

[<TestCaseSource(nameof cases)>]
let ``playground example compiles as advertised`` (case: ExampleCase) =
    let result = Api.compile case.Source
    if case.ExpectsError then
        if System.String.IsNullOrEmpty(result.error) then
            Assert.Fail(
                sprintf "Expected error from %s / %s, but compile succeeded."
                    case.Category case.Title)
    else
        if not (System.String.IsNullOrEmpty(result.error)) then
            Assert.Fail(
                sprintf "Expected %s / %s to compile, but got: %s"
                    case.Category case.Title result.error)
