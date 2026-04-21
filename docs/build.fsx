// Generates static HTML documentation from TypeFighter test files.
// Run via:  dotnet fsi docs/build.fsx

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let scriptDir = __SOURCE_DIRECTORY__
let repoRoot  = Path.GetFullPath(Path.Combine(scriptDir, ".."))
let testsDir  = Path.Combine(repoRoot, "src", "TypeFighter.Tests", "TestCases")
let outputDir = Path.Combine(scriptDir, "output")

// ─── Model ──────────────────────────────────────────────────────────

type TestCase = {
    Name: string
    DocBlock: string option          // the (* ... *) block above the test
    InlineComments: string list      // // ... lines between doc block and `let`
    Body: string                     // raw F# test body (dedented)
    Ignored: bool
    IgnoreReason: string option
}

type TestFile = {
    Slug: string                     // lower-cased file stem, used for URL
    Title: string                    // taken from banner comment
    Intro: string option             // banner intro paragraph
    Tests: TestCase list
}

// ─── Parser ─────────────────────────────────────────────────────────

let testPattern =
    Regex(@"^let\s+\[<Test(?:;\s*Ignore\(\""([^\""]*)\""\))?>\][^`]*``([^`]+)``",
          RegexOptions.Compiled)

let isBannerSeparator (line: string) =
    let t = line.TrimStart()
    if not (t.StartsWith "//") then false
    else
        let stripped = t.Substring(2).Trim()
        stripped.Length >= 3
        && stripped |> Seq.forall (fun c -> c = '=')

let isSubSeparator (line: string) =
    let t = line.TrimStart()
    if not (t.StartsWith "//") then false
    else
        let stripped = t.Substring(2).Trim()
        stripped.Length >= 3
        && stripped |> Seq.forall (fun c -> c = '-')

let stripLineComment (line: string) =
    let t = line.TrimStart()
    if t.StartsWith "//" then t.Substring(2).TrimStart()
    else t

let dedent (s: string) =
    let lines = s.Split('\n')
    let nonEmpty = lines |> Array.filter (fun l -> l.Trim() <> "")
    if nonEmpty.Length = 0 then s
    else
        let minIndent =
            nonEmpty
            |> Array.map (fun l -> l.Length - l.TrimStart(' ').Length)
            |> Array.min
        lines
        |> Array.map (fun l ->
            if l.Length >= minIndent && l.Substring(0, minIndent).Trim() = ""
            then l.Substring(minIndent)
            else l)
        |> String.concat "\n"

let parseFile (path: string) =
    let lines = File.ReadAllLines(path)
    let fileStem = Path.GetFileNameWithoutExtension(path)

    // ── banner extraction ──
    let mutable bannerTitle = fileStem
    let bannerIntro = ResizeArray<string>()
    let mutable bannerEnd = 0

    let firstSeparator =
        lines |> Array.tryFindIndex isBannerSeparator

    match firstSeparator with
    | None -> ()
    | Some start ->
        let mutable i = start + 1
        // title = first commented line that isn't a separator
        while i < lines.Length
              && (lines.[i].Trim() = "" || isSubSeparator lines.[i]) do
            i <- i + 1
        if i < lines.Length && lines.[i].TrimStart().StartsWith "//"
           && not (isBannerSeparator lines.[i]) then
            let t = stripLineComment lines.[i]
            if t <> "" then bannerTitle <- t
            i <- i + 1
        // skip separators
        while i < lines.Length
              && (lines.[i].Trim() = "" || isSubSeparator lines.[i]) do
            i <- i + 1
        // collect intro lines (// lines) until closing banner separator
        while i < lines.Length
              && not (isBannerSeparator lines.[i])
              && lines.[i].TrimStart().StartsWith "//" do
            bannerIntro.Add(stripLineComment lines.[i])
            i <- i + 1
        // consume closing separator(s)
        while i < lines.Length
              && (isBannerSeparator lines.[i] || lines.[i].Trim() = "") do
            i <- i + 1
        bannerEnd <- i

    // ── test collection ──
    let tests = ResizeArray<TestCase>()
    let mutable docBlock: string option = None
    let mutable inlineComments: string list = []
    let mutable bodyLines = ResizeArray<string>()
    let mutable currentTest: (string * bool * string option) option = None

    let finalize () =
        match currentTest with
        | Some (name, ignored, reason) ->
            let body =
                String.concat "\n" bodyLines
                |> fun s -> s.TrimEnd()
                |> dedent
            tests.Add({
                Name = name
                DocBlock = docBlock
                InlineComments = inlineComments
                Body = body
                Ignored = ignored
                IgnoreReason = reason
            })
            // only reset after actually closing a test — the pending
            // docBlock/inlineComments belong to the *next* test
            currentTest <- None
            docBlock <- None
            inlineComments <- []
            bodyLines <- ResizeArray<string>()
        | None -> ()

    let isTopLevel (line: string) =
        line.Length > 0 && line.[0] <> ' ' && line.[0] <> '\t'

    let mutable j = bannerEnd
    while j < lines.Length do
        let line = lines.[j]
        let trimmed = line.TrimStart()
        let topLevel = isTopLevel line

        if topLevel && line.StartsWith "let " then
            // A `let [<Test...>] ``name`` () =` declaration may wrap across
            // several lines when Ignore("…") carries a long reason.
            // Peek ahead joining lines until the regex matches (or we give up).
            let mutable buf = line
            let mutable declEnd = j
            let mutable m = testPattern.Match(buf)
            while not m.Success && declEnd + 1 < lines.Length
                  && not (isTopLevel lines.[declEnd + 1] && lines.[declEnd + 1].StartsWith "let ") do
                declEnd <- declEnd + 1
                buf <- buf + " " + lines.[declEnd]
                m <- testPattern.Match(buf)
            if m.Success then
                finalize ()
                let ignored = m.Groups.[1].Success
                let reason = if ignored then Some m.Groups.[1].Value else None
                let name = m.Groups.[2].Value
                currentTest <- Some (name, ignored, reason)
                for k in j .. declEnd do bodyLines.Add(lines.[k])
                j <- declEnd
            else
                // non-test top-level let (env setup) — close any pending test
                finalize ()
        elif topLevel && trimmed.StartsWith "(*" then
            // Top-level doc block — belongs to the next test; close the previous
            finalize ()
            let sb = StringBuilder()
            sb.AppendLine(line) |> ignore
            let mutable k = j
            let mutable closed = trimmed.Contains "*)"
            while not closed && k + 1 < lines.Length do
                k <- k + 1
                sb.AppendLine(lines.[k]) |> ignore
                if lines.[k].Contains "*)" then closed <- true
            docBlock <- Some (sb.ToString().TrimEnd())
            j <- k
        elif topLevel && trimmed.StartsWith "//" then
            // Top-level line comment — belongs to next test
            finalize ()
            inlineComments <- inlineComments @ [stripLineComment trimmed]
        elif topLevel && (line.StartsWith "open " || line.StartsWith "module "
                          || line.StartsWith "#" || line.StartsWith "namespace ") then
            // module-level directives — ignore
            finalize ()
        elif currentTest.IsSome then
            bodyLines.Add(line)
        // else: blank / indented line outside any test — ignore
        j <- j + 1
    finalize ()

    {
        Slug = fileStem.ToLowerInvariant()
        Title = bannerTitle
        Intro =
            if bannerIntro.Count = 0 then None
            else
                bannerIntro
                |> Seq.map (fun s -> s.TrimEnd())
                |> String.concat " "
                |> fun s -> s.Replace("  ", " ").Trim()
                |> fun s -> if s = "" then None else Some s
        Tests = List.ofSeq tests
    }

// ─── Rendering ──────────────────────────────────────────────────────

let htmlEscape (s: string) =
    s.Replace("&", "&amp;")
     .Replace("<", "&lt;")
     .Replace(">", "&gt;")
     .Replace("\"", "&quot;")

/// Render a short inline fragment with minimal markdown support:
///   `code`    → <code>code</code>
///   **bold**  → <strong>bold</strong>
///   *italic*  → <em>italic</em>
/// HTML is escaped first; code spans are lifted out before the
/// emphasis passes so asterisks inside code are left alone.
let mdInline (raw: string) =
    let escaped = htmlEscape raw
    let codeSpans = ResizeArray<string>()
    let stage1 =
        Regex.Replace(escaped, @"`([^`]+)`", fun m ->
            let i = codeSpans.Count
            codeSpans.Add(sprintf "<code>%s</code>" m.Groups.[1].Value)
            sprintf "\u0001%d\u0001" i)
    let stage2 = Regex.Replace(stage1, @"\*\*([^*]+?)\*\*", "<strong>$1</strong>")
    let stage3 = Regex.Replace(stage2, @"\*([^*\n]+?)\*", "<em>$1</em>")
    Regex.Replace(stage3, @"\u0001(\d+)\u0001", fun m ->
        codeSpans.[int m.Groups.[1].Value])

/// Strip `(*` / `*)` wrappers and leading indentation from a doc block.
let cleanDocBlock (raw: string) =
    let s = raw.Trim()
    let s = if s.StartsWith "(*" then s.Substring(2) else s
    let s = if s.EndsWith "*)" then s.Substring(0, s.Length - 2) else s
    s.Trim('\n', '\r') |> dedent

type DocSection = { Label: string; Content: string }

let private docLabelRx =
    Regex(@"^(Env|Source|Inferred|Error|Input|Output):(\s*)(.*)$",
          RegexOptions.Compiled)

let private docLabelOnlyRx =
    Regex(@"^(Env|Source|Inferred|Error|Input|Output):\s*$",
          RegexOptions.Compiled)

/// Normalize a doc block so that every label sits on its own line and all
/// content lines of a section share the same column offset — that way a
/// simple dedent per section yields clean content. A line like
/// `Source:    log "x"` becomes
/// `Source:` followed by `           log "x"` (column-preserving).
let private splitLabels (raw: string) =
    let lines = (cleanDocBlock raw).Split('\n')
    [ for line in lines do
        let m = docLabelRx.Match(line)
        if m.Success && m.Groups.[3].Value <> "" then
            let label = m.Groups.[1].Value
            let spaces = m.Groups.[2].Value
            let rest = m.Groups.[3].Value
            yield label + ":"
            yield String.replicate (label.Length + 1) " " + spaces + rest
        else
            yield line ]
    |> String.concat "\n"

let parseDocBlock (raw: string) : DocSection list =
    let normalized = splitLabels raw
    let lines = normalized.Split('\n')

    let sections = ResizeArray<string * ResizeArray<string>>()
    let mutable current: (string * ResizeArray<string>) option = None

    for line in lines do
        let m = docLabelOnlyRx.Match(line)
        if m.Success then
            let label = m.Groups.[1].Value
            let buf = ResizeArray<string>()
            let entry = (label, buf)
            sections.Add(entry)
            current <- Some entry
        else
            match current with
            | Some (_, buf) -> buf.Add(line)
            | None -> ()  // discard any junk before the first label

    [ for (label, buf) in sections ->
        let content =
            buf
            |> Seq.toArray
            |> String.concat "\n"
            |> dedent
            |> fun s -> s.Trim('\n').TrimEnd()
        { Label = label; Content = content } ]

let renderDocBlock (raw: string) =
    let sections = parseDocBlock raw
    if sections.IsEmpty then ""
    else
        let parts =
            sections
            |> List.map (fun s ->
                let cls =
                    match s.Label with
                    | "Env"      -> "doc-env"
                    | "Source"   -> "doc-source"
                    | "Inferred" -> "doc-inferred"
                    | "Error"    -> "doc-error"
                    | "Input"    -> "doc-source"
                    | "Output"   -> "doc-inferred"
                    | _          -> "doc-misc"
                let content = htmlEscape s.Content
                let highlight =
                    s.Label = "Env" || s.Label = "Source" || s.Label = "Inferred" || s.Label = "Input" || s.Label = "Output"
                let pre =
                    if highlight then
                        $"""<pre class="doc-value"><code class="language-fsharp">{content}</code></pre>"""
                    else
                        $"""<pre class="doc-value">{content}</pre>"""
                $"""    <div class="doc-section {cls}">
      <span class="doc-label">{s.Label}</span>
      {pre}
    </div>""")
            |> String.concat "\n"
        $"""<div class="test-doc">
{parts}
  </div>"""

/// Strip the `let [<Test>] \`\`name\`\` () =` declaration (may span several
/// lines for long Ignore reasons) and dedent the rest.
let extractTestBody (raw: string) =
    let lines = raw.Split('\n')
    let mutable skip = 0
    let mutable found = false
    while not found && skip < lines.Length do
        if lines.[skip].Contains "() =" then found <- true
        skip <- skip + 1
    if not found || skip >= lines.Length then raw
    else
        lines
        |> Array.skip skip
        |> String.concat "\n"
        |> dedent
        |> fun s -> s.TrimEnd()

let logoSvg = """<svg class="logo" viewBox="0 0 140 60" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
  <g stroke="currentColor" stroke-width="2" fill="none" stroke-linejoin="round" stroke-linecap="round">
    <path d="M 14 10 L 34 6 L 40 30 L 34 54 L 14 50 Z"/>
    <line x1="24" y1="8" x2="24" y2="52"/>
    <line x1="40" y1="30" x2="54" y2="30"/>
    <path d="M 126 10 L 106 6 L 100 30 L 106 54 L 126 50 Z"/>
    <line x1="116" y1="8" x2="116" y2="52"/>
    <line x1="100" y1="30" x2="86" y2="30"/>
  </g>
  <circle cx="70" cy="30" r="15" fill="#181825" stroke="currentColor" stroke-width="2"/>
  <text x="70" y="36" text-anchor="middle" font-size="17" font-family="'Times New Roman', serif" fill="currentColor" font-weight="700">&#8704;</text>
</svg>"""

let renderSidebar (files: TestFile list) (activeSlug: string option) =
    let conceptsActive = activeSlug = Some "concepts"
    let conceptsCls = if conceptsActive then "nav-item active" else "nav-item"
    let categoryLinks =
        files
        |> List.map (fun f ->
            let active = activeSlug = Some f.Slug
            let cls = if active then "nav-item active" else "nav-item"
            let name = htmlEscape f.Title
            $"""<a class="{cls}" href="{f.Slug}.html"><span class="nav-name">{name}</span><span class="nav-count">{f.Tests.Length}</span></a>""")
        |> String.concat "\n"
    $"""<aside class="sidebar">
  <div class="sidebar-head">
    <a class="brand" href="index.html">
      {logoSvg}
      <span class="brand-text">TypeFighter</span>
    </a>
    <div class="tagline">Test-driven type system docs</div>
  </div>
  <nav class="nav">
    <div class="nav-group-label">Try it</div>
    <a class="nav-item nav-item-playground" href="playground/"><span class="nav-name">Playground →</span></a>
    <div class="nav-group-label">Reference</div>
    <a class="{conceptsCls}" href="concepts.html"><span class="nav-name">Concepts</span></a>
    <div class="nav-group-label">Test categories</div>
{categoryLinks}
  </nav>
</aside>"""

let renderTestCard (t: TestCase) =
    let doc =
        match t.DocBlock with
        | Some b -> renderDocBlock b
        | None -> ""
    let inlineNote =
        if t.InlineComments.IsEmpty then ""
        else
            let joined =
                t.InlineComments
                |> List.filter (fun l -> l <> "")
                |> String.concat " "
            if joined = "" then ""
            else $"""<p class="test-note">{mdInline joined}</p>"""
    let body = extractTestBody t.Body
    let badge =
        if t.Ignored then
            let reason = t.IgnoreReason |> Option.defaultValue "not yet implemented"
            $"""<span class="badge badge-ignored" title="{htmlEscape reason}">Not yet implemented</span>"""
        elif t.Body.Contains "shouldFail" then
            """<span class="badge badge-error">Expected error</span>"""
        else
            """<span class="badge badge-ok">Inferred</span>"""
    let cardCls = if t.Ignored then " test-ignored" else ""
    let name = htmlEscape t.Name
    let bodyEsc = htmlEscape body
    $"""<article class="test-card{cardCls}">
  <header class="test-head">
    <h3 class="test-name">{name}</h3>
    {badge}
  </header>
  {doc}
  {inlineNote}
  <details class="test-src">
    <summary>F# test body</summary>
    <pre><code class="language-fsharp">{bodyEsc}</code></pre>
  </details>
</article>"""

let renderCategoryPage (files: TestFile list) (file: TestFile) =
    let tests =
        file.Tests
        |> List.map renderTestCard
        |> String.concat "\n"
    let intro =
        match file.Intro with
        | Some s -> $"""<p class="intro">{mdInline s}</p>"""
        | None -> ""
    let sidebar = renderSidebar files (Some file.Slug)
    let title = htmlEscape file.Title
    $"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <title>{title} — TypeFighter</title>
  <link rel="stylesheet" href="style.css">
  <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/highlight.min.js"></script>
  <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/languages/fsharp.min.js"></script>
  <script>document.addEventListener('DOMContentLoaded',()=>hljs.highlightAll());</script>
</head>
<body>
  <div class="page">
    {sidebar}
    <main class="main">
      <h1>{title}</h1>
      {intro}
      <div class="tests">
{tests}
      </div>
    </main>
  </div>
</body>
</html>
"""

let renderCatCard (f: TestFile) =
    let intro = f.Intro |> Option.defaultValue ""
    let snippet =
        if intro.Length <= 180 then intro
        else intro.Substring(0, 177).TrimEnd() + "…"
    let title = htmlEscape f.Title
    let snipEsc = mdInline snippet
    $"""<a class="cat-card" href="{f.Slug}.html">
  <div class="cat-head">
    <span class="cat-name">{title}</span>
    <span class="cat-count">{f.Tests.Length} tests</span>
  </div>
  <p class="cat-intro">{snipEsc}</p>
</a>"""

let renderConceptsPage (files: TestFile list) =
    let sidebar = renderSidebar files (Some "concepts")
    $"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <title>Concepts — TypeFighter</title>
  <link rel="stylesheet" href="style.css">
  <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/highlight.min.js"></script>
  <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/languages/fsharp.min.js"></script>
  <script>document.addEventListener('DOMContentLoaded',()=>hljs.highlightAll());</script>
</head>
<body>
  <div class="page">
    {sidebar}
    <main class="main concepts">
      <h1>Concepts</h1>
      <p class="intro">This page covers only features that are <strong>actually implemented and test-covered today</strong>. Design sketches and roadmap ideas live in <code>docs/design/</code> in the repo and intentionally don't appear here — everything you read on this page corresponds to running, passing code. Each section points to the test categories that exercise it.</p>

      <section class="concept">
        <h2>1. The type AST at a glance</h2>
        <p>A <code>MonoTyp</code> is one of six shapes — that's the entire universe of types the solver reasons about:</p>
        <ul>
          <li><strong>Type variable</strong> (<code>TVar</code>) — a placeholder that unification eventually pins down.</li>
          <li><strong>Saturated type</strong> (<code>SaturatedTyp</code>) — a named type applied to zero or more arguments: <code>Number</code>, <code>String</code>, <code>Array&lt;Number&gt;</code>, <code>Map&lt;String, Number&gt;</code>.</li>
          <li><strong>Function type</strong> (<code>FunTyp</code>) — <code>A -&gt; B</code>, right-associative by convention.</li>
          <li><strong>Record type</strong> (<code>RecordTyp</code>) — a set of named fields <em>and</em> a bag of positional items. Both rows live in one record; either can be empty.</li>
          <li><strong>Literal type</strong> (<code>LiteralTyp</code>) — a specific number, string, or boolean value treated <em>as</em> a type: <code>42</code>, <code>"red"</code>, <code>true</code>.</li>
          <li><strong>Union type</strong> (<code>UnionTyp</code>) — a set of alternatives: <code>true | false</code>, <code>200 | 404 | 500</code>, <code>Number | String</code>.</li>
        </ul>
        <p>There is <em>no</em> separate <code>IntersectionTyp</code>: <code>&amp;</code> between two record operands normalises at parse time into a single merged <code>RecordTyp</code>.</p>
      </section>

      <section class="concept">
        <h2>2. Set-theoretic type syntax</h2>
        <p>Three combinators on types, all usable from the text-level parser:</p>
        <table class="concept-table">
          <thead><tr><th>Surface</th><th>Meaning</th><th>Example</th></tr></thead>
          <tbody>
            <tr><td><code>A | B</code></td><td>disjunction — "is an A <em>or</em> a B"</td><td><code>200 | 404 | 500</code></td></tr>
            <tr><td><code>A &amp; B</code></td><td>conjunction at the record level — adds B's items to A</td><td><code>{{ "Circle" &amp; radius: Number }}</code></td></tr>
            <tr><td><code>{{ … }}</code></td><td>record-set bracket — packages named + positional items</td><td><code>{{ x: Number &amp; y: Number }}</code></td></tr>
            <tr><td><code>"foo"</code>, <code>42</code>, <code>true</code></td><td>literal types — TypeScript-style</td><td><code>"red" | "yellow" | "green"</code></td></tr>
            <tr><td><code>name: A</code></td><td>named property inside a record</td><td><code>{{ name: String }}</code></td></tr>
          </tbody>
        </table>
        <p><strong>Precedence.</strong> <code>&amp;</code> binds tighter than <code>|</code> (the universal "and binds tighter than or" convention). <code>A | B &amp; C</code> parses as <code>A | (B &amp; C)</code>. Parentheses override.</p>
        <p><strong>Two meanings of <code>|</code> around records.</strong></p>
        <ul>
          <li><code>{{ A }} | {{ B }}</code> — union of two <em>record</em> types; the value is <em>either</em> a record-with-A <em>or</em> a record-with-B.</li>
          <li><code>{{ A | B }}</code> — <em>one</em> record whose single slot has type <code>A | B</code>.</li>
        </ul>
        <p>Tests: <a href="literals.html">Literals</a>, and the parser's type-expression tests in <code>TypeFighter.Parser01.Tests/TestCases/Types.fs</code>.</p>
      </section>

      <section class="concept">
        <h2>3. Structural, heterogeneous records</h2>
        <p>A record is a <strong>set of items</strong>, not a <em>list of declared fields with a name on the envelope</em>. Two rows live on every record:</p>
        <ul>
          <li><strong>Named items</strong> (<em>properties</em>) — <code>x: Number</code>, <code>name: String</code>.</li>
          <li><strong>Positional items</strong> — anonymous types that sit in the record's bag, order-insensitive.</li>
        </ul>
        <p>Records are matched <em>structurally</em>: a function expecting <code>{{ name: String }}</code> accepts any record that has a <code>name</code> field of type <code>String</code>, regardless of what else the record carries. No <code>type</code> declaration is required anywhere.</p>
<pre><code class="language-fsharp">// Just a record expression — no declaration, no "class", no name.
X.MkRecord [
    X.Property "age"  (X.Lit 22)
    X.Property "name" (X.Lit "John")
]
// Inferred:  {{ age: Number; name: String }}
</code></pre>
        <p>Tests: <a href="records.html">Records</a>.</p>
      </section>

      <section class="concept">
        <h2>4. Row polymorphism</h2>
        <p>When a function reads fields from a record argument, the inferencer collects the <em>required</em> fields via <code>CHasField</code> constraints. The argument's type closes to a record containing exactly those fields — no more, no less — so the function works on any record <em>at least</em> that wide.</p>
<pre><code class="language-fsharp">fun r -&gt; r.name
// Inferred:  forall a. {{ name: a }} -&gt; a
</code></pre>
        <p>Pass it a <code>{{ name: String; age: Number }}</code> and it works; pass it a record without <code>name</code> and unification fails. This is "works on anything with field X" — it is <em>not</em> type-class polymorphism ("works on anything supporting operation X").</p>
        <p>Tests: <a href="rows.html">Rows</a>.</p>
      </section>

      <section class="concept">
        <h2>5. Pattern matching with narrowing</h2>
        <p><code>Expr.Match</code> supports three pattern shapes today:</p>
        <ul>
          <li><strong>Literal</strong> — <code>| 0 -&gt;</code>, <code>| "yes" -&gt;</code>, <code>| true -&gt;</code>. Emits a <code>CHasMember</code> constraint so the scrutinee's type closes to a union containing (at least) the matched literal.</li>
          <li><strong>Var</strong> — <code>| y -&gt;</code>. Catch-all that binds the scrutinee to a name in the arm body.</li>
          <li><strong>Wildcard</strong> — <code>| _ -&gt;</code>. Catch-all that adds no type constraint.</li>
        </ul>
        <p><strong>Narrowing.</strong> A match that consists only of literal arms closes the scrutinee into exactly the union of those literals:</p>
<pre><code class="language-fsharp">fun s -&gt; match s with
         | 200 -&gt; "ok"
         | 404 -&gt; "missing"
         | 500 -&gt; "boom"
// Inferred:  {{200 | 404 | 500}} -&gt; String
</code></pre>
        <p>Adding a wildcard arm does <em>not</em> widen the scrutinee — the literal arms still determine its type. Adding a var arm also catches-all at runtime but the var is free, so the scrutinee's type is driven by the literal arms only.</p>
        <p>Not yet: record / destructuring patterns, exhaustiveness checking.</p>
        <p>Tests: <a href="match.html">Match</a>.</p>
      </section>

      <section class="concept">
        <h2>6. Discriminated unions without <code>type</code></h2>
        <p>TypeFighter has no <code>type Shape = Circle of … | Square of …</code> syntax, and doesn't need one. A DU is just a <strong>union of tagged records</strong>:</p>
<pre><code>Option&lt;T&gt;   = {{ "None" }} | {{ "Some" &amp; T }}
Shape       = {{ "Circle" &amp; radius: Number }}
            | {{ "Square" &amp; side:   Number }}
Result&lt;T,E&gt; = {{ "Ok"  &amp; T }} | {{ "Err" &amp; E }}
</code></pre>
        <p>Each arm is a heterogeneous record whose tag is a positional <em>literal</em> type. Build a value of one arm with <code>X.MkRecord [ X.Positional (X.Lit "Circle"); X.Property "radius" (X.Lit 3) ]</code>.</p>
        <p>Matching on the tag uses the same literal-pattern machinery as numeric refinement — the wrapper record keeps its payload, the tag selects the arm. Narrowing across arm bodies (so the arm <em>sees</em> the right payload type) is the next step and is <em>not</em> wired up yet.</p>
        <p>Tests: <a href="composites.html">Composites</a>.</p>
      </section>

      <section class="concept">
        <h2>7. Polymorphism</h2>
        <p>Two flavors are supported:</p>
        <ul>
          <li><strong>Environment-supplied polytypes.</strong> An environment entry like <code>log : forall a. a -&gt; Unit</code> is instantiated fresh at every use site. Two calls get independent <code>a</code>s.</li>
          <li><strong>Top-level generalization.</strong> An expression whose inferred type has free type variables generalises to a polytype at the top level.</li>
        </ul>
        <p>Not yet: generalisation at <em>inner</em> <code>let</code> bindings. The relevant test is marked <code>[&lt;Ignore&gt;]</code> on <a href="let.html">Let bindings</a>.</p>
        <p>Tests: <a href="polymorphism.html">Polymorphism</a>, <a href="generalization.html">Generalization</a>, <a href="functions.html">Functions</a>.</p>
      </section>

      <section class="concept">
        <h2>8. Arrays</h2>
        <p>Arrays are <strong>homogeneous</strong>: all elements must unify to one element type. Mixing incompatible elements is a static error — TypeFighter does not auto-widen to <code>Array&lt;Number | String&gt;</code>.</p>
        <p>Tests: <a href="arrays.html">Arrays</a>.</p>
      </section>

      <section class="concept">
        <h2>9. Text parser (TypeFighter.Parser01)</h2>
        <p>A standalone parser project reads programs and type expressions from text into the same AST the tests construct by hand. Type-expression grammar (low → high precedence):</p>
<pre><code>typeExpr = altTyp {{ "|" altTyp }}
altTyp   = primTyp {{ "&amp;" primTyp }}
primTyp  = literalTyp | identTyp | applied | "(" typeExpr ")" | "{{" recordItems "}}"
</code></pre>
        <p>Top-level <code>&amp;</code> between two record operands normalises to a single merged <code>RecordTyp</code> at parse time; conflicting fields are rejected. Literals, built-in names (<code>Number</code>, <code>String</code>, <code>Bool</code>), applied types, unions, parenthesised grouping, and heterogeneous record literals are all supported.</p>
        <p>Tests: see the <code>TypeFighter.Parser01.Tests</code> project in the repo.</p>
      </section>

      <section class="concept">
        <h2>10. What's deliberately absent</h2>
        <ul>
          <li><strong>Nominal types.</strong> There is no way to declare <code>type Person = {{ name: String }}</code> and have <code>Person</code> be <em>distinct</em> from any other record with the same fields. Structural matching is the design.</li>
          <li><strong>Implicit conversions / subtyping.</strong> <code>Number</code> does not silently become <code>String</code>. If you pass the wrong type, unification fails immediately.</li>
          <li><strong>Traits / type classes.</strong> No <code>Show</code>-style constraints. See the "note on Traits" on the <a href="index.html">index page</a>.</li>
          <li><strong>Recursion.</strong> No <code>let rec</code>, no fix-point combinator.</li>
          <li><strong>Effects.</strong> Not tracked.</li>
        </ul>
      </section>
    </main>
  </div>
</body>
</html>
"""

let renderIndexPage (files: TestFile list) =
    let cards =
        files
        |> List.map renderCatCard
        |> String.concat "\n"
    let totalTests = files |> List.sumBy (fun f -> f.Tests.Length)
    let sidebar = renderSidebar files None
    $"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <title>TypeFighter</title>
  <link rel="stylesheet" href="style.css">
  <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/highlight.min.js"></script>
  <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/languages/fsharp.min.js"></script>
  <script>document.addEventListener('DOMContentLoaded',()=>hljs.highlightAll());</script>
</head>
<body>
  <div class="page">
    {sidebar}
    <main class="main">
      <h1>TypeFighter</h1>
      <p class="intro">TypeFighter is a small, experimental language built around a modern, inference-first type system. The headline feature is <strong>structural records instead of nominal ones</strong>: records are compared by the fields they actually have, not by a declared name — so a function that needs <code class="inline-code">{{ name: String }}</code> accepts <em>any</em> record with that field, no boilerplate declarations required. On top of that: set-theoretic type syntax (<code>|</code>, <code>&amp;</code>, <code>{{ … }}</code>, literal types), row polymorphism, pattern matching with narrowing, and classical polymorphic functions — figured out by the type checker with (almost) no annotations. This site is generated from the test suite: each test is a minimal example of what the type system can — and can't yet — do. For a narrative walkthrough of what's actually implemented, see <a href="concepts.html">Concepts</a>.</p>

      <section class="features">
        <h2>What's in, what's not</h2>
        <p>Every item in <em>In today</em> is covered by at least one passing test in this suite; items under <em>Not yet</em> either fail, are marked <code>Ignore</code>, or are absent by design. A quick comparison against concepts you'll recognize from Rust, Haskell, Scala, TypeScript, Swift, or F#:</p>
        <div class="feature-grid">
          <div class="feature-col has">
            <h3>In today</h3>
            <ul>
              <li><strong>Structural records</strong> — matched by fields, not by name</li>
              <li><strong>Heterogeneous records</strong> — named fields <em>and</em> positional items (<code>{{ "Tag" &amp; radius: Number }}</code>)</li>
              <li><strong>Row polymorphism</strong> — "has at least field X"</li>
              <li><strong>Literal types</strong> — <code>42</code>, <code>"red"</code>, <code>true</code> each have a type of their own</li>
              <li><strong>Union types</strong> <code>A | B</code> — booleans are the union <code>true | false</code>; numeric/string literal unions for refinement</li>
              <li><strong>Record-level <code>&amp;</code></strong> — merges two record operands into one structural record at the type level</li>
              <li><strong>Discriminated unions</strong> — express <code>Option</code>, <code>Result</code>, <code>Shape</code> via <code>{{ "Tag" &amp; fields }} | {{ "OtherTag" &amp; fields }}</code> without a separate <code>type</code> declaration</li>
              <li><strong>Pattern matching</strong> — <code>Expr.Match</code> with literal, variable, and wildcard patterns; literal arms <em>narrow</em> the scrutinee's type</li>
              <li><strong>Polymorphic functions</strong> — fresh instantiation at every use site</li>
              <li><strong>Higher-order &amp; partial application</strong></li>
              <li><strong>Arrays</strong> as a built-in type constructor</li>
              <li><strong>Text parser for type expressions</strong> — <code>TypeFighter.Parser01</code> reads the surface syntax from text</li>
              <li><strong>Inference with (almost) no annotations</strong></li>
            </ul>
          </div>
          <div class="feature-col lacks">
            <h3>Not yet (or absent by design)</h3>
            <ul>
              <li><strong>AST-level <code>let</code>-generalization</strong> — inner <code>let</code> bindings don't auto-generalize the way classical Hindley–Milner does (two <code>Ignore</code>d tests track this)</li>
              <li><strong>Record / destructuring patterns in <code>match</code></strong> — only literal / var / wildcard patterns today</li>
              <li><strong>Exhaustiveness checking for <code>match</code></strong> — not enforced</li>
              <li><strong>Traits / type classes / protocols</strong> — no ad-hoc polymorphism (see below)</li>
              <li><strong>Recursion</strong> — no <code>let rec</code> or fix-point combinator</li>
              <li><strong>Implicit conversions / subtyping</strong> — <code>Number</code> is never silently a <code>String</code> (by design)</li>
              <li><strong>Nominal types</strong> — everything is structural (by design, not a gap)</li>
              <li><strong>Effect system</strong> — effects aren't tracked</li>
            </ul>
          </div>
        </div>

        <div class="traits-note">
          <h3>A note on Traits</h3>
          <p>In languages like Rust (<em>traits</em>), Haskell (<em>type classes</em>), Swift (<em>protocols</em>), or Scala (<em>given</em>/implicits), you can write a function like <code>show : a -&gt; String</code> that works on <em>any</em> type <code>a</code> which implements a <code>Show</code> constraint. TypeFighter has no such mechanism — there is no way to say "any type that supports <code>toString</code>" or "any <code>Num</code>". The closest thing TypeFighter offers is <strong>row polymorphism</strong>: a function <code>fun r -&gt; r.name</code> works on any record that <em>structurally</em> has a <code>name</code> field. That gives you "works on anything with field X" — but not "works on anything supporting operation X".</p>
        </div>
      </section>

      <section class="about">
        <h2>How to read these pages</h2>
        <p>The site has two kinds of content:</p>
        <ul>
          <li><a href="concepts.html"><strong>Concepts</strong></a> — a narrative reference written by hand, covering only what's actually wired up and passing tests today. No roadmap speculation lives there.</li>
          <li><strong>Test categories</strong> (listed below) — one page per feature, auto-generated from the F# test files. Every test shows its doc block (<em>Env / Source / Inferred / Error</em>), an optional note, and its raw F# body.</li>
        </ul>
        <p>Tests marked <span class="badge badge-ignored">Not yet implemented</span> document features that are deliberately not supported today.</p>
      </section>

      <h2>Categories ({totalTests} tests total)</h2>
      <div class="cat-grid">
{cards}
      </div>
    </main>
  </div>
</body>
</html>
"""

// ─── CSS ────────────────────────────────────────────────────────────

let css = """
*, *::before, *::after { box-sizing: border-box; }

html, body {
    margin: 0;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
                 Helvetica, Arial, sans-serif;
    color: #2b2b2b;
    background: #f5f5f7;
}

a { color: inherit; text-decoration: none; }

.page {
    display: flex;
    min-height: 100vh;
}

/* ─── Sidebar ──────────────────────────────────────────────────── */

.sidebar {
    width: 280px;
    flex-shrink: 0;
    background: #1e1e2e;
    color: #cdd6f4;
    position: sticky;
    top: 0;
    height: 100vh;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
}

.sidebar-head {
    padding: 1.25rem 1.25rem 1rem;
    border-bottom: 1px solid #313244;
}

.brand {
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    gap: 0.4rem;
    color: #cba6f7;
    transition: color 0.2s;
}

.brand:hover { color: #f5c2e7; }

.logo {
    width: 100%;
    max-width: 160px;
    height: auto;
    display: block;
}

.brand-text {
    font-size: 1.25rem;
    font-weight: 700;
    letter-spacing: -0.01em;
}

.tagline {
    font-size: 0.75rem;
    color: #7f849c;
    margin-top: 0.2rem;
}

.nav {
    display: flex;
    flex-direction: column;
    padding: 0.5rem 0;
}

.nav-group-label {
    font-size: 0.65rem;
    font-weight: 700;
    letter-spacing: 0.12em;
    text-transform: uppercase;
    color: #7f849c;
    padding: 0.9rem 1.25rem 0.3rem;
}

.nav-group-label:first-child { padding-top: 0.4rem; }

.nav-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem 1.25rem;
    color: #a6adc8;
    font-size: 0.9rem;
    transition: background 0.15s, color 0.15s;
    border-left: 3px solid transparent;
}

.nav-item:hover {
    background: #313244;
    color: #cdd6f4;
}

.nav-item.active {
    background: #313244;
    color: #cdd6f4;
    border-left-color: #cba6f7;
}

.nav-count {
    font-size: 0.7rem;
    color: #7f849c;
    background: #181825;
    padding: 0.1rem 0.45rem;
    border-radius: 999px;
}

.nav-item.active .nav-count { color: #cdd6f4; }

.nav-item-playground {
    color: #f9e2af;
}

.nav-item-playground:hover {
    color: #fbe9b7;
}

/* ─── Main ─────────────────────────────────────────────────────── */

.main {
    flex: 1;
    padding: 2rem 2.5rem 4rem;
    max-width: 960px;
    min-width: 0;
}

.main h1 {
    margin: 0 0 0.5rem;
    font-size: 1.8rem;
    font-weight: 700;
    letter-spacing: -0.01em;
}

.main h2 {
    margin: 2rem 0 0.75rem;
    font-size: 1.15rem;
    font-weight: 600;
}

.intro {
    max-width: 750px;
    color: #555;
    font-size: 1rem;
    line-height: 1.55;
    margin: 0 0 1.25rem;
}

.about p, .about li {
    color: #444;
    font-size: 0.92rem;
    line-height: 1.55;
}

.about ul { padding-left: 1.25rem; margin: 0.5rem 0; }

/* ─── Feature comparison ──────────────────────────────────────── */

.features {
    margin-top: 1.5rem;
    max-width: 720px;
}

.features > p {
    color: #555;
    font-size: 0.95rem;
    line-height: 1.55;
    margin: 0 0 0.8rem;
}

.feature-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 0.75rem;
}

.feature-col {
    background: #fff;
    border: 1px solid #e4e4e8;
    border-radius: 10px;
    padding: 1rem 1.15rem 1.1rem;
}

.feature-col h3 {
    margin: 0 0 0.6rem;
    font-size: 0.95rem;
    font-weight: 700;
    letter-spacing: -0.005em;
}

.feature-col.has h3   { color: #1f7a36; }
.feature-col.lacks h3 { color: #6b6b7a; }

.feature-col ul {
    margin: 0;
    padding-left: 0;
    list-style: none;
}

.feature-col li {
    position: relative;
    padding: 0.3rem 0 0.3rem 1.3rem;
    font-size: 0.88rem;
    color: #333;
    line-height: 1.5;
}

.feature-col li::before {
    position: absolute;
    left: 0.15rem;
    top: 0.65rem;
    width: 0.55rem;
    height: 0.55rem;
    border-radius: 50%;
    content: "";
}

.feature-col.has   li::before { background: #2ea44f; }
.feature-col.lacks li::before { background: #d9d9e0; border: 1px solid #b8b8c0; }


.traits-note { margin-top: 1.25rem; }

.traits-note h3 {
    margin: 0 0 0.5rem;
    font-size: 1rem;
    font-weight: 700;
}

.traits-note p {
    margin: 0;
    max-width: 750px;
    font-size: 0.92rem;
    color: #444;
    line-height: 1.6;
}

/* ─── Concepts page ─────────────────────────────────────────── */

.concepts .concept {
    margin: 2.25rem 0 0;
    max-width: 780px;
}

.concepts .concept h2 {
    margin: 0 0 0.75rem;
    font-size: 1.2rem;
    font-weight: 700;
    letter-spacing: -0.005em;
    padding-bottom: 0.4rem;
    border-bottom: 1px solid #e4e4e8;
}

.concepts .concept p {
    margin: 0.6rem 0;
    font-size: 0.95rem;
    line-height: 1.65;
    color: #3a3a44;
}

.concepts .concept ul {
    margin: 0.75rem 0 1rem;
    padding-left: 1.4rem;
}

.concepts .concept li {
    margin: 0.4rem 0;
    font-size: 0.93rem;
    line-height: 1.65;
    color: #3a3a44;
}

.concepts .concept li::marker { color: #cba6f7; }

.concepts .concept pre {
    background: #1e1e2e;
    color: #cdd6f4;
    padding: 0.9rem 1.1rem;
    border-radius: 6px;
    font-family: "SF Mono", "Fira Code", "Cascadia Code", Consolas, monospace;
    font-size: 0.82rem;
    line-height: 1.6;
    overflow-x: auto;
    margin: 0.9rem 0 1rem;
}

.concepts .concept p code,
.concepts .concept li code {
    font-family: "SF Mono", "Fira Code", "Cascadia Code", Consolas, monospace;
    font-size: 0.86em;
    background: #f0eef7;
    padding: 0.05rem 0.32rem;
    border-radius: 3px;
    color: #3b3756;
}

.concept-table {
    width: 100%;
    border-collapse: collapse;
    margin: 0.9rem 0 1rem;
    font-size: 0.9rem;
    background: #fff;
    border: 1px solid #e4e4e8;
    border-radius: 8px;
    overflow: hidden;
}

.concept-table thead {
    background: #f5f3fa;
}

.concept-table th {
    text-align: left;
    padding: 0.6rem 0.9rem;
    font-size: 0.78rem;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
    color: #6d58a8;
    border-bottom: 1px solid #e4e4e8;
}

.concept-table td {
    padding: 0.65rem 0.9rem;
    border-top: 1px solid #eeecf3;
    vertical-align: top;
    line-height: 1.55;
    color: #3a3a44;
}

.concept-table tbody tr:first-child td { border-top: none; }

.concept-table td code {
    font-family: "SF Mono", "Fira Code", "Cascadia Code", Consolas, monospace;
    font-size: 0.86em;
    background: #f0eef7;
    padding: 0.05rem 0.32rem;
    border-radius: 3px;
    color: #3b3756;
}


/* ─── Index grid ───────────────────────────────────────────────── */

.cat-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
    gap: 0.85rem;
}

.cat-card {
    background: #fff;
    border: 1px solid #e4e4e8;
    border-radius: 10px;
    padding: 1rem 1.15rem;
    transition: border-color 0.15s, box-shadow 0.15s;
}

.cat-card:hover {
    border-color: #cba6f7;
    box-shadow: 0 3px 10px rgba(0,0,0,0.05);
}

.cat-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin-bottom: 0.3rem;
}

.cat-name {
    font-weight: 600;
    font-size: 1rem;
    color: #2b2b2b;
}

.cat-count {
    font-size: 0.72rem;
    color: #888;
}

.cat-intro {
    margin: 0;
    font-size: 0.85rem;
    color: #666;
    line-height: 1.45;
}

/* ─── Test cards ───────────────────────────────────────────────── */

.tests {
    display: flex;
    flex-direction: column;
    gap: 1rem;
}

.test-card {
    background: #fff;
    border: 1px solid #e4e4e8;
    border-radius: 10px;
    padding: 1rem 1.25rem 1.1rem;
}

.test-card.test-ignored {
    background: #fafafa;
    border-style: dashed;
    opacity: 0.88;
}

.test-head {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    gap: 0.75rem;
    margin-bottom: 0.6rem;
}

.test-name {
    margin: 0;
    font-size: 1rem;
    font-weight: 600;
    color: #2b2b2b;
    line-height: 1.35;
}

.badge {
    flex-shrink: 0;
    font-size: 0.7rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding: 0.22rem 0.55rem;
    border-radius: 999px;
    white-space: nowrap;
}

.badge-ok       { background: #e7f5ea; color: #1f7a36; }
.badge-error    { background: #fce8e8; color: #a63535; }
.badge-ignored  { background: #ecebf2; color: #6b6b7a; }

.test-doc {
    display: flex;
    flex-direction: column;
    gap: 0.45rem;
    background: #f5f3fa;
    border-left: 3px solid #cba6f7;
    padding: 0.75rem 0.9rem;
    border-radius: 4px;
    margin: 0 0 0.65rem;
    overflow-x: auto;
}

.doc-section {
    display: grid;
    grid-template-columns: 5.5rem 1fr;
    column-gap: 0.75rem;
    align-items: start;
}

.doc-label {
    font-size: 0.66rem;
    font-weight: 700;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    padding: 0.22rem 0.55rem;
    border-radius: 999px;
    text-align: center;
    line-height: 1;
    justify-self: start;
    white-space: nowrap;
    background: #e9e4f5;
    color: #6d58a8;
}

.doc-section.doc-env      .doc-label { background: #e4edfb; color: #2956a4; }
.doc-section.doc-source   .doc-label { background: #ece9f7; color: #5a4c92; }
.doc-section.doc-inferred .doc-label { background: #e0f1e6; color: #1f7a36; }
.doc-section.doc-error    .doc-label { background: #fbe4e4; color: #a63535; }

.doc-value {
    margin: 0;
    padding-top: 0.05rem;
    font-family: "SF Mono", "Fira Code", "Cascadia Code", Consolas, monospace;
    font-size: 0.82rem;
    line-height: 1.55;
    color: #3b3756;
    white-space: pre-wrap;
    word-break: break-word;
}


.test-note {
    margin: 0 0 0.6rem;
    font-size: 0.88rem;
    color: #555;
    line-height: 1.5;
}

.test-src {
    margin-top: 0.4rem;
    font-size: 0.85rem;
}

.test-src summary {
    cursor: pointer;
    color: #7c3aed;
    font-weight: 600;
    user-select: none;
}

.test-src summary:hover { color: #6d28d9; }

.test-src pre {
    margin: 0.5rem 0 0;
    background: #1e1e2e;
    color: #cdd6f4;
    padding: 0.85rem 1rem;
    border-radius: 6px;
    font-family: "SF Mono", "Fira Code", "Cascadia Code", Consolas, monospace;
    font-size: 0.8rem;
    line-height: 1.55;
    overflow-x: auto;
    white-space: pre;
}

/* ─── Inline code in prose ────────────────────────────────────── */

.intro code,
.cat-intro code,
.test-note code,
.feature-col code,
.traits-note code {
    font-family: "SF Mono", "Fira Code", "Cascadia Code", Consolas, monospace;
    font-size: 0.86em;
    background: #f0eef7;
    padding: 0.05rem 0.3rem;
    border-radius: 3px;
    color: #3b3756;
}

/* ─── Syntax highlighting ─────────────────────────────────────── */

.doc-value code.hljs,
.test-src pre code.hljs {
    padding: 0;
    background: transparent;
}

/* Light theme — doc-value (Env / Source / Inferred) */
.doc-value .hljs-keyword,
.doc-value .hljs-literal,
.doc-value .hljs-built_in          { color: #8250df; font-weight: 600; }
.doc-value .hljs-string            { color: #0a7d2c; }
.doc-value .hljs-number            { color: #0550ae; }
.doc-value .hljs-comment           { color: #6e7781; font-style: italic; }
.doc-value .hljs-type,
.doc-value .hljs-title.class_,
.doc-value .hljs-class .hljs-title { color: #953800; }
.doc-value .hljs-title.function_,
.doc-value .hljs-function .hljs-title { color: #0550ae; }
.doc-value .hljs-symbol,
.doc-value .hljs-operator          { color: #3b3756; }
.doc-value .hljs-variable,
.doc-value .hljs-attr              { color: #3b3756; }

/* Dark theme — F# test body (expanded details) */
.test-src .hljs-keyword,
.test-src .hljs-literal,
.test-src .hljs-built_in           { color: #cba6f7; }
.test-src .hljs-string             { color: #a6e3a1; }
.test-src .hljs-number             { color: #fab387; }
.test-src .hljs-comment            { color: #7f849c; font-style: italic; }
.test-src .hljs-type,
.test-src .hljs-title.class_,
.test-src .hljs-class .hljs-title  { color: #f9e2af; }
.test-src .hljs-title.function_,
.test-src .hljs-function .hljs-title { color: #89b4fa; }
.test-src .hljs-symbol,
.test-src .hljs-operator           { color: #cdd6f4; }
.test-src .hljs-variable,
.test-src .hljs-attr               { color: #cdd6f4; }

/* ─── Responsive ───────────────────────────────────────────────── */

@media (max-width: 960px) {
    .page { flex-direction: column; }
    .sidebar {
        width: 100%;
        height: auto;
        position: static;
    }
    .sidebar-head {
        padding: 1rem 1.25rem 0.9rem;
    }
    .brand { flex-direction: row; align-items: center; gap: 0.6rem; }
    .logo { max-width: 60px; }
    .tagline { display: none; }
    .nav {
        flex-direction: row;
        flex-wrap: wrap;
        padding: 0.5rem 0.75rem;
        gap: 0.25rem;
    }
    .nav-item {
        border-left: none;
        border-radius: 6px;
        padding: 0.35rem 0.7rem;
    }
    .nav-item.active { border-left: none; }
    .main { padding: 1.5rem 1.5rem 3rem; }
}

@media (max-width: 720px) {
    .feature-grid { grid-template-columns: 1fr; }
    .test-head {
        flex-wrap: wrap;
        gap: 0.4rem;
    }
    .cat-grid { grid-template-columns: 1fr; }
    .main { padding: 1.25rem 1rem 2.5rem; }
}

@media (max-width: 560px) {
    .main h1 { font-size: 1.45rem; }
    .main h2 { font-size: 1.05rem; }
    .doc-section {
        grid-template-columns: 1fr;
        row-gap: 0.25rem;
    }
    .test-card { padding: 0.85rem 1rem; }
    .test-doc { padding: 0.6rem 0.75rem; }
    .test-src pre { font-size: 0.75rem; padding: 0.7rem 0.8rem; }
    .intro { font-size: 0.95rem; }
}
"""

// ─── Main ───────────────────────────────────────────────────────────

let testFileOrder =
    [ "Literals"; "Functions"; "Let"; "Generalization"; "Arrays"
      "Records"; "Rows"; "Polymorphism"; "Match"; "Composites" ]

let readAllTestFiles () =
    let foundFiles =
        Directory.GetFiles(testsDir, "*.fs")
        |> Array.map (fun p -> Path.GetFileNameWithoutExtension p, p)
        |> Map.ofArray
    [ for name in testFileOrder do
        match Map.tryFind name foundFiles with
        | Some p -> yield parseFile p
        | None -> ()
      // any files not in the ordered list, appended alphabetically
      for KeyValue(name, p) in foundFiles do
        if not (List.contains name testFileOrder) then
            yield parseFile p ]

let main () =
    if not (Directory.Exists outputDir) then
        Directory.CreateDirectory outputDir |> ignore

    let files = readAllTestFiles ()
    File.WriteAllText(Path.Combine(outputDir, "style.css"), css)
    File.WriteAllText(Path.Combine(outputDir, "index.html"), renderIndexPage files)
    File.WriteAllText(Path.Combine(outputDir, "concepts.html"), renderConceptsPage files)
    for f in files do
        let html = renderCategoryPage files f
        File.WriteAllText(Path.Combine(outputDir, f.Slug + ".html"), html)

    let total = files |> List.sumBy (fun f -> f.Tests.Length)
    printfn "Generated %d category pages (%d tests) into %s"
        files.Length total outputDir

main ()
