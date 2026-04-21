import * as monaco from "monaco-editor";
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { compileWithInfo, preludeInfo } from "../fable-out/Api.js";
import { categories, type Example } from "./examples";

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

// Latest IntelliSense snapshot. Providers read from here; the compile
// callback refreshes it. Stale data during a failed parse still beats
// nothing — completions keep working while the user fixes a typo.
let currentHovers: HoverInfo[] = [];
let currentCompletions: CompletionEntry[] = [];

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

run();
