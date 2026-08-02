// Shared Monaco setup for the TypeFighter language: tokenizer, theme,
// language config and the global hover/completion/signature providers.
import * as monaco from "monaco-editor";
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { fetchCompletionAt, fetchPrelude, type CompletionEntry, type HoverInfo, type PreludeEntry } from "./api";

declare global {
  interface Window {
    __tfpMonacoInit?: boolean;
  }
}

// Everything language-related is served by the compiler (parser + solver):
// the prelude endpoint carries all builtins INCLUDING the stdlib bindings
// (typ marker "(stdlib)"), compile-info carries hovers + user-binding
// completions, completion-at resolves receiver members. The editor never
// scrapes source text and never duplicates language lists.

// (Re)register syntax highlighting with the given set of builtin identifiers.
export function setTypefighterTokens(builtins: string[]) {
  monaco.languages.setMonarchTokensProvider("typefighter", {
    keywords: ["let", "const", "true", "false", "if", "then", "else"],
    builtins,
    operators: ["=>", "=", "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "!", "&", "|", ";", ",", ".", ":"],
    tokenizer: {
      root: [
        [/\/\/.*$/, "comment"],
        [/__\w+/, "type"],
        [/[a-zA-Z_][\w]*/, {
          cases: {
            "@keywords": "keyword",
            "@builtins": "predefined",
            "@default": "identifier",
          },
        }],
        [/"([^"\\]|\\.)*"/, "string"],
        [/\d+(\.\d+)?/, "number"],
        [/[{}()\[\]]/, "@brackets"],
        [/[;,.:]/, "delimiter"],
        [/=>|<=|>=|==|!=|&&|\|\||[=+\-*\/<>!&|]/, "operator"],
        [/\s+/, "white"],
      ],
    },
  });
}

// Shared fixed-position layer for Monaco overflow widgets (hover, suggest).
// Editors inside modals/panels clip these via overflow:hidden; rendering them
// body-level above every overlay (modal backdrop is z-index 100) fixes that.
// Use with `fixedOverflowWidgets: true` + `overflowWidgetsDomNode`.
let overflowWidgetsNode: HTMLDivElement | undefined;
export function typefighterOverflowWidgetsNode(): HTMLDivElement {
  if (!overflowWidgetsNode) {
    const node = document.createElement("div");
    node.className = "monaco-editor"; // monaco widget styles key off this class
    node.style.position = "fixed";
    node.style.top = "0";
    node.style.left = "0";
    node.style.zIndex = "10000";
    document.body.appendChild(node);
    overflowWidgetsNode = node;
  }
  return overflowWidgetsNode;
}

// Builtins to highlight from a loaded prelude: every prelude name except the
// operators (op_*, never written as identifiers).
export function editorBuiltinsFromPrelude(prelude: PreludeEntry[]): string[] {
  return prelude.map((p) => p.name).filter((n) => !n.startsWith("op_"));
}

// Register the `typefighter` language, tokens, theme and bracket config once.
export function ensureTypefighterLanguage() {
  if (typeof window === "undefined" || window.__tfpMonacoInit) return;
  self.MonacoEnvironment = { getWorker: () => new editorWorker() };
  monaco.languages.register({ id: "typefighter" });
  setTypefighterTokens([]);
  monaco.editor.defineTheme("typefighter-dark", {
    base: "vs-dark",
    inherit: true,
    rules: [
      { token: "keyword",    foreground: "C586C0", fontStyle: "bold" },
      { token: "predefined", foreground: "4EC9B0" },
      { token: "type",       foreground: "DCDCAA" },
      { token: "string",     foreground: "CE9178" },
      { token: "number",     foreground: "B5CEA8" },
      { token: "comment",    foreground: "6A9955", fontStyle: "italic" },
      { token: "operator",   foreground: "D4D4D4" },
      { token: "delimiter",  foreground: "808080" },
    ],
    colors: {},
  });
  monaco.languages.setLanguageConfiguration("typefighter", {
    comments: { lineComment: "//" },
    brackets: [["{", "}"], ["[", "]"], ["(", ")"]],
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"' },
    ],
  });
  window.__tfpMonacoInit = true;
}

// Per-model IntelliSense data: each editor pushes what it got from
// compile-info (keyed by model URI), so the single global providers can
// serve any number of typefighter editors correctly. Both hovers and the
// user-binding completions are SOLVER results -- never scraped from text.
const modelHovers = new Map<string, HoverInfo[]>();
const modelCompletions = new Map<string, CompletionEntry[]>();

export function setModelHovers(model: monaco.editor.ITextModel, hovers: HoverInfo[]) {
  modelHovers.set(model.uri.toString(), hovers ?? []);
}

export function setModelCompletions(model: monaco.editor.ITextModel, completions: CompletionEntry[]) {
  modelCompletions.set(model.uri.toString(), completions ?? []);
}

// Register the language's providers ONCE (idempotent across HMR via a window
// flag). Hover: each editor pushes its compile-info hovers via setModelHovers.
// Completion: THE one global provider for every typefighter editor -- nothing
// else may register one (a second provider duplicates every entry).
export function ensureTypefighterProviders() {
  if (typeof window === "undefined") return;
  const w = window as unknown as { __tfSharedProviders?: boolean };
  if (w.__tfSharedProviders) return;
  w.__tfSharedProviders = true;

  monaco.languages.registerHoverProvider("typefighter", {
    provideHover(model, position) {
      const hovers = modelHovers.get(model.uri.toString());
      if (!hovers) return null;
      const offset = model.getOffsetAt(position);
      const hit = hovers.find((h) => offset >= h.startIdx && offset <= h.endIdx);
      if (!hit) return null;
      const start = model.getPositionAt(hit.startIdx);
      const end = model.getPositionAt(hit.endIdx);
      return {
        range: new monaco.Range(start.lineNumber, start.column, end.lineNumber, end.column),
        contents: [{ value: "```typefighter\n" + (hit.typ ? hit.name + " : " + hit.typ : hit.name) + "\n```" }],
      };
    },
  });

  // Completion: after a dot the backend solves the receiver type and returns
  // its members (values.<NAME>, math.<block>, record fields); everywhere else
  // the full symbol list (prelude + stdlib + user bindings from the source).
  monaco.languages.registerCompletionItemProvider("typefighter", {
    triggerCharacters: [".", "`"],
    async provideCompletionItems(model, position) {
      const word = model.getWordUntilPosition(position);
      const range = new monaco.Range(
        position.lineNumber, word.startColumn, position.lineNumber, word.endColumn);
      const offset = model.getOffsetAt(position);
      const before = model.getValue().slice(0, offset);

      if (/\.\s*[\w`]*$/.test(before)) {
        const fields = await fetchCompletionAt(model.getValue(), offset).catch(() => []);
        return {
          suggestions: fields.map((n) => ({
            label: n,
            kind: monaco.languages.CompletionItemKind.Field,
            // Field names may carry spaces/specials -> backtick-quote those.
            insertText: /^[A-Za-z_]\w*$/.test(n) ? n : "`" + n + "`",
            range,
          })),
        };
      }

      const prelude = await preludeForCompletion();
      const seen = new Set<string>();
      const suggestions: monaco.languages.CompletionItem[] = [];
      const push = (label: string, kind: monaco.languages.CompletionItemKind, detail: string, sort: string) => {
        if (seen.has(label)) return;
        seen.add(label);
        suggestions.push({ label, kind, detail, insertText: label, sortText: sort + label, range });
      };
      for (const p of prelude) {
        if (p.name.startsWith("op_") || p.name.startsWith("__")) continue;
        const isNamespace = p.fields.length > 0 && p.name !== "values";
        if (p.name === "values") push(p.name, monaco.languages.CompletionItemKind.Struct, "host values (values.`NAME`)", "0");
        else if (isNamespace) push(p.name, monaco.languages.CompletionItemKind.Module, p.fields.join(", "), "0");
        else if (p.typ === "(stdlib)") push(p.name, monaco.languages.CompletionItemKind.Function, "stdlib", "1");
        else if (p.typ.includes("->")) push(p.name, monaco.languages.CompletionItemKind.Function, p.typ, "2");
        else push(p.name, monaco.languages.CompletionItemKind.Constant, p.typ, "2");
      }
      // User bindings: solver results from the last compile-info round trip
      // (pushed per model via setModelCompletions) -- never text-scraped.
      for (const c of modelCompletions.get(model.uri.toString()) ?? []) {
        if (c.kind === "binding") {
          push(c.name, monaco.languages.CompletionItemKind.Variable, c.typ || "binding", "1");
        }
      }
      return { suggestions };
    },
  });

  // Parameter hints on `(` and `,`: resolve the callee's type from the
  // model's hovers (compile-info) and split it into parameters.
  monaco.languages.registerSignatureHelpProvider("typefighter", {
    signatureHelpTriggerCharacters: ["(", ","],
    signatureHelpRetriggerCharacters: [","],
    async provideSignatureHelp(model, position) {
      const ctx = findCallContext(model, position);
      if (!ctx) return null;
      // Hovers come from the last successful compile; while typing an
      // incomplete call they may be stale or empty, so fall back to the
      // STATIC prelude type (namespace members live as __ns_member).
      const hovers = modelHovers.get(model.uri.toString()) ?? [];
      let typ = hovers.find((h) => h.name === ctx.name && h.typ)?.typ;
      if (!typ) {
        const prelude = await preludeForCompletion();
        const flat = ctx.ns ? "__" + ctx.ns + "_" + ctx.name : ctx.name;
        typ = prelude.find((p) => p.name === flat)?.typ;
      }
      if (!typ) return null;
      const sig = parseFunctionType(typ);
      return {
        value: {
          signatures: [{
            label: `${ctx.name} : ${typ}`,
            parameters: sig.params.length > 0
              ? sig.params.map((p) => ({ label: p }))
              : [{ label: typ }],
          }],
          activeSignature: 0,
          activeParameter: Math.min(ctx.activeParam, Math.max(sig.params.length - 1, 0)),
        },
        dispose() {},
      };
    },
  });
}

// Innermost open call at the cursor: callee name + active parameter index.
function findCallContext(model: monaco.editor.ITextModel, position: monaco.IPosition) {
  const text = model.getValueInRange({
    startLineNumber: 1, startColumn: 1,
    endLineNumber: position.lineNumber, endColumn: position.column,
  });
  let depth = 0;
  let activeParam = 0;
  for (let i = text.length - 1; i >= 0; i--) {
    const c = text[i];
    if (c === ")") depth++;
    else if (c === "(") {
      if (depth === 0) {
        const before = text.slice(0, i).trimEnd();
        const m = /(?:([A-Za-z_]\w*)\s*\.\s*)?([A-Za-z_]\w*)$/.exec(before);
        if (!m || !m[2]) return null;
        return { ns: m[1] ?? null, name: m[2], activeParam };
      }
      depth--;
    } else if (c === "," && depth === 0) {
      activeParam++;
    }
  }
  return null;
}

// Parse a TypeFighter function type string into params + return. Handles
// right-associative `A -> B -> C`, outer parens, the `<tv_...>.` poly prefix,
// and angle brackets (Stream<...>).
export function parseFunctionType(typ: string): { params: string[]; ret: string } {
  let s = typ.replace(/^<[^>]*>\./, "").trim();
  if (s.startsWith("(") && s.endsWith(")")) {
    let depth = 0, ok = true;
    for (let i = 0; i < s.length - 1; i++) {
      if (s[i] === "(") depth++;
      else if (s[i] === ")") { depth--; if (depth === 0) { ok = false; break; } }
    }
    if (ok) s = s.slice(1, -1).trim();
  }
  const parts: string[] = [];
  let depth = 0, last = 0;
  for (let i = 0; i < s.length - 3; i++) {
    if (s[i] === "(" || s[i] === "<") depth++;
    else if (s[i] === ")" || s[i] === ">") depth--;
    else if (depth === 0 && s.slice(i, i + 4) === " -> ") {
      parts.push(s.slice(last, i).trim());
      last = i + 4;
      i += 3;
    }
  }
  parts.push(s.slice(last).trim());
  if (parts.length < 2) return { params: [], ret: s };
  return { params: parts.slice(0, -1), ret: parts[parts.length - 1] };
}

// Re-register syntax highlighting from the live prelude, once per page life:
// new backend builtins light up without a frontend edit.
let tokensRefreshed = false;
export function refreshTypefighterTokensFromPrelude() {
  if (tokensRefreshed) return;
  tokensRefreshed = true;
  preludeForCompletion().then((p) => {
    if (p.length > 0) setTypefighterTokens(editorBuiltinsFromPrelude(p));
    else tokensRefreshed = false;
  });
}

// Process-lifetime prelude cache for identifier completion (the prelude only
// changes when the host re-registers its values, i.e. on service restart).
let preludeCachePromise: Promise<PreludeEntry[]> | undefined;
function preludeForCompletion(): Promise<PreludeEntry[]> {
  if (!preludeCachePromise) {
    preludeCachePromise = fetchPrelude().then((p) => {
      // Cold start: values may arrive without fields -- don't memoize that.
      const values = p.find((e) => e.name === "values");
      if (values && values.fields.length === 0) preludeCachePromise = undefined;
      return p;
    }).catch(() => {
      preludeCachePromise = undefined;
      return [] as PreludeEntry[];
    });
  }
  return preludeCachePromise;
}

