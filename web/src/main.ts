import * as monaco from "monaco-editor";
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { compileWithInfo, preludeInfo } from "../fable-out/Api.js";
import { categories, type Example } from "./examples";
// Shared with the docs site — single source of truth lives in
// `assets/logo.svg`. Vite inlines the raw SVG text at build time.
import logoSvg from "../../assets/logo.svg?raw";

// Shapes returned by Api.compileWithInfo — mirrored here to get
// type-safety on the TS side. The F# record names stay in sync via the
// smoke test; if they diverge we'd get "undefined" at runtime quickly.
type HoverInfo = { startIdx: number; endIdx: number; name: string; typ: string };
type CompletionEntry = { name: string; typ: string; kind: string };

// Monaco loads language + editor functionality from web workers. Vite's
// `?worker` import turns each worker module into a constructor. We only
// register the core editor worker — no TS/JSON workers since we don't
// use those languages here. Without `MonacoEnvironment.getWorker` set,
// Monaco falls back to evaluating the worker inline, which CSP-aware
// setups reject and which defeats lazy-loading.
self.MonacoEnvironment = {
    getWorker: () => new editorWorker()
};

monaco.languages.register({ id: "typefighter" });
monaco.languages.setMonarchTokensProvider("typefighter", {
    keywords: ["let", "const", "true", "false"],
    operators: ["=>", "=", "+", "-", "*", "/", ";", ",", ".", ":"],
    tokenizer: {
        root: [
            // `//` line comments must precede the operator rule that
            // matches bare `/`, otherwise the first slash wins.
            [/\/\/.*$/, "comment"],
            [/[a-zA-Z_][\w]*/, {
                cases: {
                    "@keywords": "keyword",
                    "@default": "identifier"
                }
            }],
            [/"([^"\\]|\\.)*"/, "string"],
            [/\d+(\.\d+)?/, "number"],
            [/[{}()\[\]]/, "@brackets"],
            [/[;,.:]/, "delimiter"],
            [/=>|[=+\-*\/]/, "operator"],
            [/\s+/, "white"]
        ]
    }
});

// Language configuration: tells Monaco what the line-comment marker is
// so `Cmd+/` / `Ctrl+/` toggles `//` prefixes on the selected lines.
monaco.languages.setLanguageConfiguration("typefighter", {
    comments: { lineComment: "//" },
    brackets: [["{", "}"], ["[", "]"], ["(", ")"]],
    autoClosingPairs: [
        { open: "{", close: "}" },
        { open: "[", close: "]" },
        { open: "(", close: ")" },
        { open: "\"", close: "\"" }
    ]
});

const srcEditor = monaco.editor.create(document.getElementById("src-editor")!, {
    value: categories[0].examples[0].source,
    language: "typefighter",
    theme: "vs-dark",
    automaticLayout: true,
    minimap: { enabled: false },
    fontSize: 13,
    scrollBeyondLastLine: false,
    tabSize: 2
});

const outEditor = monaco.editor.create(document.getElementById("out-editor")!, {
    value: "",
    language: "javascript",
    theme: "vs-dark",
    automaticLayout: true,
    readOnly: true,
    minimap: { enabled: false },
    fontSize: 13,
    scrollBeyondLastLine: false
});

const status = document.getElementById("status")!;
const srcModel = srcEditor.getModel()!;

document.getElementById("brand-logo")!.innerHTML = logoSvg;

// Docs link target: in a production build the playground ships under
// `/TypeFighter/playground/` on GH Pages, so `../` reaches the docs
// root. In dev there is no local docs site (Vite serves only this app),
// so point at the live one instead to avoid the dead `/` → playground
// loop.
const docsLink = document.getElementById("docs-link") as HTMLAnchorElement;
docsLink.href = import.meta.env.PROD
    ? "../"
    : "https://schlenkr.github.io/TypeFighter/";

// Latest IntelliSense snapshot. Providers read from here; the compile
// callback refreshes it. Stale data during a failed parse still beats
// nothing — completions keep working while the user fixes a typo.
let currentHovers: HoverInfo[] = [];
let currentCompletions: CompletionEntry[] = [];

// ── Console pane ────────────────────────────────────────────────
// Captures console.log / console.error output from running the emitted
// JS. Kept as a running log across runs so users can see history while
// iterating; `clear` button empties it.

const consoleEl = document.getElementById("console")!;
const clearConsoleBtn = document.getElementById("clear-console")!;

type ConsoleKind = "log" | "error" | "meta";
function appendConsoleLine(kind: ConsoleKind, text: string) {
    const line = document.createElement("div");
    line.className = `line ${kind}`;
    line.textContent = text;
    consoleEl.appendChild(line);
    consoleEl.scrollTop = consoleEl.scrollHeight;
}
clearConsoleBtn.addEventListener("click", () => {
    consoleEl.innerHTML = "";
});

// Render an arbitrary JS value to a readable string for the console.
// Strings print raw (no surrounding quotes) to match Node/browser
// behavior; objects get JSON.stringify with a 2-space indent.
function formatValue(v: unknown): string {
    if (typeof v === "string") return v;
    if (v === undefined) return "undefined";
    if (v === null) return "null";
    if (typeof v === "function") return "[Function]";
    try {
        return JSON.stringify(v, null, 2);
    } catch {
        return String(v);
    }
}

// Execute the emitted JS and capture its console output. The emitter
// produces an ES module ending in `export default <expr>;` — for
// execution we strip that prefix so the final expression runs for its
// side effects. We intentionally use `new Function` (classic script
// scope) rather than `eval` so the snippet can't see our module-scoped
// vars. A fake `console` is the only injected dependency.
function executeJs(jsSource: string) {
    const script = jsSource.replace(/^export\s+default\s+/m, "");
    const fakeConsole = {
        log: (...args: unknown[]) =>
            appendConsoleLine("log", args.map(formatValue).join(" ")),
        error: (...args: unknown[]) =>
            appendConsoleLine("error", args.map(formatValue).join(" "))
    };
    try {
        new Function("console", script)(fakeConsole);
    } catch (e) {
        appendConsoleLine("error", (e as Error).message);
    }
}

function run() {
    const result = compileWithInfo(srcEditor.getValue());
    currentHovers = result.hovers ?? [];
    currentCompletions = result.completions ?? [];
    if (result.error) {
        status.textContent = result.error.replace(/\s+/g, " ").trim();
        status.classList.add("error");
        monaco.editor.setModelMarkers(srcModel, "typefighter", [{
            severity: monaco.MarkerSeverity.Error,
            startLineNumber: 1,
            startColumn: 1,
            endLineNumber: srcModel.getLineCount(),
            endColumn: srcModel.getLineMaxColumn(srcModel.getLineCount()),
            message: result.error
        }]);
    } else {
        status.textContent = `ok — ${result.js.length} chars emitted`;
        status.classList.remove("error");
        monaco.editor.setModelMarkers(srcModel, "typefighter", []);
        outEditor.setValue(result.js);
        appendConsoleLine("meta", "▸ run");
        executeJs(result.js);
    }
}

// ── Monaco providers: hover + completions ─────────────────────────
// The span data from the F# side is offset-based (startIdx/endIdx into
// the raw source string), but Monaco works in (lineNumber, column)
// pairs. `getOffsetAt` / `getPositionAt` bridge the two worlds.

monaco.languages.registerHoverProvider("typefighter", {
    provideHover(model, position) {
        const offset = model.getOffsetAt(position);
        const hit = currentHovers.find(
            h => offset >= h.startIdx && offset <= h.endIdx
        );
        if (!hit) return null;
        const start = model.getPositionAt(hit.startIdx);
        const end = model.getPositionAt(hit.endIdx);
        const contents = hit.typ
            ? [{ value: `**${hit.name}** : \`${hit.typ}\`` }]
            : [{ value: `**${hit.name}**` }];
        return {
            range: new monaco.Range(start.lineNumber, start.column, end.lineNumber, end.column),
            contents
        };
    }
});

monaco.languages.registerCompletionItemProvider("typefighter", {
    provideCompletionItems(model, position) {
        const word = model.getWordUntilPosition(position);
        const range = new monaco.Range(
            position.lineNumber, word.startColumn,
            position.lineNumber, word.endColumn
        );
        const suggestions = currentCompletions.map(c => ({
            label: c.name,
            kind: c.kind === "prelude"
                ? monaco.languages.CompletionItemKind.Function
                : monaco.languages.CompletionItemKind.Variable,
            insertText: c.name,
            detail: c.typ || c.kind,
            range
        }));
        return { suggestions };
    }
});

// Debounce so each keystroke doesn't trigger a full parse+typecheck+emit.
// 200ms feels instant but lets typing bursts settle.
let pending: number | undefined;
srcEditor.onDidChangeModelContent(() => {
    if (pending !== undefined) clearTimeout(pending);
    pending = window.setTimeout(run, 200);
});

// ── Sidebar: categorized examples ─────────────────────────────────
const sidebar = document.getElementById("sidebar")!;
let activeEl: HTMLElement | null = null;

function loadExample(example: Example, el: HTMLElement) {
    srcEditor.setValue(example.source);
    if (activeEl) activeEl.classList.remove("active");
    el.classList.add("active");
    activeEl = el;
}

for (const cat of categories) {
    const label = document.createElement("div");
    label.className = "category";
    label.textContent = cat.name;
    sidebar.appendChild(label);
    for (const ex of cat.examples) {
        const row = document.createElement("div");
        row.className = "example";
        row.textContent = ex.title;
        row.addEventListener("click", () => loadExample(ex, row));
        sidebar.appendChild(row);
    }
}
// Mark the first example active on boot since that's what we loaded.
const firstExample = sidebar.querySelector(".example");
if (firstExample) {
    firstExample.classList.add("active");
    activeEl = firstExample as HTMLElement;
}

// ── Prelude panel: names + type signatures in scope ────────────────
const preludePanel = document.getElementById("prelude")!;
const label = document.createElement("div");
label.className = "label";
label.textContent = "In scope";
preludePanel.appendChild(label);
for (const { name, typ } of preludeInfo()) {
    const row = document.createElement("div");
    row.className = "entry";
    row.innerHTML = `<span class="name">${name}</span><span class="sep"> : </span><span class="typ">${typ}</span>`;
    preludePanel.appendChild(row);
}

// ── Splitter: drag to resize source vs output panes ──────────────
// The CSS grid uses two named fractions — `--src-col` and `--out-col`
// — and this handler rewrites them as pixel widths during the drag.
// Monaco editors have `automaticLayout: true`, so they re-flow on their
// own. We clamp to a sane minimum so neither pane can collapse to 0.

const mainEl = document.querySelector("main") as HTMLElement;
const splitter = document.getElementById("splitter")!;
const SIDEBAR_PX = 240;
const SPLITTER_PX = 5;
const GAPS_PX = 3; // three 1px grid gaps
const MIN_PANE_PX = 200;

let dragging = false;
splitter.addEventListener("mousedown", (e) => {
    dragging = true;
    splitter.classList.add("dragging");
    document.body.style.cursor = "col-resize";
    // Suppress text selection while dragging.
    document.body.style.userSelect = "none";
    e.preventDefault();
});

window.addEventListener("mousemove", (e) => {
    if (!dragging) return;
    const rect = mainEl.getBoundingClientRect();
    const editorsWidth = rect.width - SIDEBAR_PX - SPLITTER_PX - GAPS_PX;
    const srcStart = rect.left + SIDEBAR_PX + 1; // +1 for grid gap
    let srcWidth = e.clientX - srcStart;
    srcWidth = Math.max(MIN_PANE_PX, Math.min(editorsWidth - MIN_PANE_PX, srcWidth));
    const outWidth = editorsWidth - srcWidth;
    mainEl.style.setProperty("--src-col", `${srcWidth}px`);
    mainEl.style.setProperty("--out-col", `${outWidth}px`);
});

window.addEventListener("mouseup", () => {
    if (!dragging) return;
    dragging = false;
    splitter.classList.remove("dragging");
    document.body.style.cursor = "";
    document.body.style.userSelect = "";
});

run();
