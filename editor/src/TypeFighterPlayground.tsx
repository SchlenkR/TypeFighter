import { useEffect, useRef, useState } from "react";
import * as monaco from "monaco-editor";
import { defaultCategories, type Category, type Example } from "./examples";
import { ExampleOverview } from "./ExampleOverview";
import TypeFighterEditor, { type TypeFighterEditorInstance } from "./TypeFighterEditor";
import { attachCompileInfo } from "./attachments";
import { fetchEval, fetchPrelude, type PreludeEntry } from "./api";
import "./TypeFighterPlayground.scss";

type Props = { categories?: Category[] };

export default function TypeFighterPlayground({ categories = defaultCategories }: Props) {
  const csContainerRef = useRef<HTMLDivElement>(null);
  const srcEditorRef = useRef<TypeFighterEditorInstance | null>(null);
  const csEditorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  const [activeExample, setActiveExample] = useState<string>(categories[0].examples[0].title);
  const [status, setStatus] = useState<{ msg: string; isError: boolean }>({ msg: "", isError: false });
  const [prelude, setPrelude] = useState<PreludeEntry[]>([]);
  const [result, setResult] = useState<string>("");
  const [output, setOutput] = useState<string>("");
  const [running, setRunning] = useState(false);
  const [hasError, setHasError] = useState(false);
  const [errorText, setErrorText] = useState<string>("");
  const [resultType, setResultType] = useState<string>("");

  const mainRef = useRef<HTMLDivElement>(null);
  const [dragging, setDragging] = useState(false);

  // Card overview as the entry screen; the editor stays mounted underneath,
  // so switching back and forth never loses editor state.
  const [view, setView] = useState<"overview" | "editor">("overview");

  useEffect(() => {
    fetchPrelude().then(setPrelude).catch((e) => {
      console.error("prelude fetch failed", e);
    });
  }, []);

  // Read-only pane for the emitted C#; plain Monaco, not a TypeFighter editor.
  useEffect(() => {
    if (!csContainerRef.current) return;
    const cs = monaco.editor.create(csContainerRef.current, {
      value: "",
      language: "csharp",
      theme: "vs-dark",
      automaticLayout: true,
      readOnly: true,
      minimap: { enabled: false },
      fontSize: 13,
      scrollBeyondLastLine: false,
    });
    csEditorRef.current = cs;
    return () => {
      cs.dispose();
      csEditorRef.current = null;
    };
  }, []);

  // Auto: parse + type-check + IntelliSense only. Never compiles or evaluates.
  const mountSrcEditor = (editor: TypeFighterEditorInstance) => {
    srcEditorRef.current = editor;
    const detach = attachCompileInfo(editor, {
      debounceMs: 250,
      onInfo: (r) => {
        setResultType(r.resultType ?? "");
        if (r.error) {
          setStatus({ msg: "type error", isError: true });
          setErrorText(r.error);
          setHasError(true);
        } else {
          setStatus({ msg: "ok - type-checks", isError: false });
          setErrorText("");
          setHasError(false);
        }
      },
    });
    return () => {
      detach();
      srcEditorRef.current = null;
    };
  };

  const loadExample = (ex: Example) => {
    setActiveExample(ex.title);
    srcEditorRef.current?.setValue(ex.source);
    setResult("");
    setOutput("");
    csEditorRef.current?.setValue("");
  };

  const runEval = async () => {
    const src = srcEditorRef.current;
    const cs = csEditorRef.current;
    if (!src || !cs) return;
    setRunning(true);
    try {
      const r = await fetchEval(src.getValue());
      cs.setValue(r.cSharp ?? "");
      if (r.error) {
        setResult("");
        setOutput(r.error);
      } else {
        setResult(r.result ?? "");
        setOutput(r.output ?? "");
      }
    } finally {
      setRunning(false);
    }
  };

  useEffect(() => {
    if (!dragging) return;
    const onMove = (e: MouseEvent) => {
      const main = mainRef.current;
      if (!main) return;
      const rect = main.getBoundingClientRect();
      const editorsWidth = rect.width - 240 - 5 - 3;
      const srcStart = rect.left + 240 + 1;
      const minPx = 200;
      let srcWidth = e.clientX - srcStart;
      srcWidth = Math.max(minPx, Math.min(editorsWidth - minPx, srcWidth));
      const outWidth = editorsWidth - srcWidth;
      main.style.setProperty("--tfp-src", `${srcWidth}px`);
      main.style.setProperty("--tfp-out", `${outWidth}px`);
    };
    const onUp = () => setDragging(false);
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
    document.body.style.cursor = "col-resize";
    document.body.style.userSelect = "none";
    return () => {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
      document.body.style.cursor = "";
      document.body.style.userSelect = "";
    };
  }, [dragging]);

  return (
    <div className="tfp">
      <header className="tfp__header">
        <span className="tfp__title">TypeFighter Playground</span>
        <button
          className="tfp__view-btn"
          onClick={() => setView(view === "overview" ? "editor" : "overview")}
          style={{
            background: "transparent", color: "#9d9d9d", border: "1px solid #3c3c3c",
            borderRadius: 4, padding: "3px 10px", fontSize: 12, cursor: "pointer", marginLeft: 10,
          }}
        >
          {view === "overview" ? "Editor" : "Examples"}
        </button>
        {view === "editor" && (
          <>
            <span className={`tfp__status${status.isError ? " tfp__status--error" : ""}`}>
              {status.msg}
            </span>
            <button
              className="tfp__run-btn"
              onClick={runEval}
              disabled={running || hasError}
              title={hasError ? "Fix type errors first" : "Compile + evaluate (Roslyn)"}
            >
              {running ? "Running..." : "Run"}
            </button>
          </>
        )}
      </header>

      <main className="tfp__main" ref={mainRef} style={{ position: "relative" }}>
        {view === "overview" && (
          <div style={{ position: "absolute", inset: 0, zIndex: 10, background: "#1e1e1e" }}>
            <ExampleOverview
              categories={categories}
              onPick={(ex) => {
                loadExample(ex);
                setView("editor");
              }}
            />
          </div>
        )}
        <aside className="tfp__sidebar">
          {categories.map((cat) => (
            <div key={cat.name}>
              <div className="tfp__category">{cat.name}</div>
              {cat.examples.map((ex) => (
                <div
                  key={ex.title}
                  className={`tfp__example${ex.title === activeExample ? " tfp__example--active" : ""}`}
                  onClick={() => loadExample(ex)}
                >
                  {ex.title}
                </div>
              ))}
            </div>
          ))}
        </aside>

        <section className="tfp__pane">
          <div className="tfp__pane-header">
            <span>Source</span>
            <button
              className="tfp__pane-run"
              onClick={runEval}
              disabled={running || hasError}
              title={hasError ? "Fix type errors first" : "Run"}
              aria-label="Run"
            >
              <svg width="8" height="9" viewBox="0 0 8 9" aria-hidden="true">
                <polygon points="0,0 0,9 8,4.5" fill="currentColor" />
              </svg>
            </button>
          </div>
          <div className="tfp__editor">
            <TypeFighterEditor defaultValue={categories[0].examples[0].source} onMount={mountSrcEditor} />
          </div>
          <div className="tfp__prelude">
            <div className="tfp__prelude-label">Result type</div>
            <div className="tfp__result-typ">{resultType || "-"}</div>
            <div className="tfp__prelude-label">In scope</div>
            {prelude.map((p) => (
              <div key={p.name} className="tfp__prelude-entry">
                <span className="tfp__prelude-name">{p.name}</span>
                <span className="tfp__prelude-sep"> : </span>
                <span className="tfp__prelude-typ">{p.typ}</span>
              </div>
            ))}
          </div>
        </section>

        <div
          className={`tfp__splitter${dragging ? " tfp__splitter--dragging" : ""}`}
          onMouseDown={(e) => { e.preventDefault(); setDragging(true); }}
        />

        <section className="tfp__pane">
          <div className="tfp__pane-header">Emitted C#</div>
          <div className="tfp__editor" ref={csContainerRef} />
          {errorText && (
            <>
              <div className="tfp__pane-header tfp__pane-header--secondary tfp__pane-header--error">Error</div>
              <pre className="tfp__error">{errorText}</pre>
            </>
          )}
          <div className="tfp__pane-header tfp__pane-header--secondary">Result</div>
          <div className="tfp__result-pane">
            <div className="tfp__result-section">
              <div className="tfp__result-label">Value</div>
              <pre className="tfp__result-value">{result || <span className="tfp__muted">(no value)</span>}</pre>
            </div>
            <div className="tfp__result-section">
              <div className="tfp__result-label">stdout</div>
              <pre className="tfp__result-output">{output || <span className="tfp__muted">(empty)</span>}</pre>
            </div>
          </div>
        </section>
      </main>
    </div>
  );
}
