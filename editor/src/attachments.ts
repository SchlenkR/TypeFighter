// Attachable editor behaviours. Each attach* function wires one concern onto
// an existing editor instance and returns a disposer -- the editor component
// itself stays thin, hosts compose only what they need.
import * as monaco from "monaco-editor";
import { fetchCompileInfo, type CompileInfoResponse } from "./api";
import { setModelCompletions, setModelHovers } from "./monaco";

export type CompileInfoAttachmentOptions = {
  // Debounce after edits; a first check runs after initialDelayMs.
  debounceMs?: number;
  initialDelayMs?: number;
  // Full response per round trip -- for status lines, result types, etc.
  onInfo?: (info: CompileInfoResponse) => void;
};

// Debounced compile-info loop: error squiggles + hover data on the model,
// full response to the host. Transport failures are swallowed (IntelliSense
// must never break the editor); the next edit simply retries.
export function attachCompileInfo(
  editor: monaco.editor.IStandaloneCodeEditor,
  opts: CompileInfoAttachmentOptions = {},
): () => void {
  const debounceMs = opts.debounceMs ?? 300;
  let timer: number | undefined;
  let disposed = false;

  const check = async () => {
    const model = editor.getModel();
    if (!model) return;
    try {
      const info = await fetchCompileInfo(editor.getValue());
      if (disposed || model.isDisposed()) return;
      setModelHovers(model, info.hovers ?? []);
      setModelCompletions(model, info.completions ?? []);
      const markers: monaco.editor.IMarkerData[] = [];
      if (info.error && info.errorRange) {
        const start = model.getPositionAt(info.errorRange.startIdx);
        const end = model.getPositionAt(info.errorRange.endIdx);
        markers.push({
          severity: monaco.MarkerSeverity.Error,
          message: info.error,
          startLineNumber: start.lineNumber,
          startColumn: start.column,
          endLineNumber: end.lineNumber,
          endColumn: end.column,
        });
      }
      monaco.editor.setModelMarkers(model, "typefighter", markers);
      opts.onInfo?.(info);
    } catch {
      /* retry on next edit */
    }
  };

  const schedule = (delay: number) => {
    if (timer !== undefined) window.clearTimeout(timer);
    timer = window.setTimeout(check, delay);
  };

  const sub = editor.onDidChangeModelContent(() => schedule(debounceMs));
  schedule(opts.initialDelayMs ?? 0);

  return () => {
    disposed = true;
    if (timer !== undefined) window.clearTimeout(timer);
    sub.dispose();
  };
}
