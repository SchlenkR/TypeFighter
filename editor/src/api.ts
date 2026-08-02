// Typed wrappers around the TypeFighter language endpoints. All HTTP goes
// through the host-injected transport (see transport.ts).
import { tfLanguageRequest } from "./transport";

export type HoverInfo = { startIdx: number; endIdx: number; name: string; typ: string };
export type CompletionEntry = { name: string; typ: string; kind: string };
export type ErrorRange = { startIdx: number; endIdx: number };
export type CompileInfoResponse = {
  error: string;
  errorRange: ErrorRange | null;
  hovers: HoverInfo[];
  completions: CompletionEntry[];
  resultType: string;
};
export type EvalResponse = { cSharp: string; result: string; output: string; error: string };
export type PreludeEntry = { name: string; typ: string; fields: string[] };

export function fetchCompileInfo(source: string): Promise<CompileInfoResponse> {
  return tfLanguageRequest<CompileInfoResponse>({ method: "POST", path: "/compile-info", data: { source } });
}

export function fetchEval(source: string): Promise<EvalResponse> {
  return tfLanguageRequest<EvalResponse>({ method: "POST", path: "/eval", data: { source } });
}

export function fetchPrelude(): Promise<PreludeEntry[]> {
  return tfLanguageRequest<PreludeEntry[]>({ method: "GET", path: "/prelude" });
}

export function fetchCompletionAt(source: string, offset: number): Promise<string[]> {
  return tfLanguageRequest<string[]>({ method: "POST", path: "/completion-at", data: { source, offset } });
}
