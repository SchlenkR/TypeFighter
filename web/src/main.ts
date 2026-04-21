import * as monaco from "monaco-editor";
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { compile, preludeInfo } from "../fable-out/Api.js";
import { categories, type Example } from "./examples";

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
            [/\/\/.*$/, "comment"],
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

function run() {
    const result = compile(srcEditor.getValue());
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
