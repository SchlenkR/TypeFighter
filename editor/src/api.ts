// Typed wrappers around the TypeFighter language endpoints.
import {
  tfLanguageRequest,
  type TfRequest,
  type TfTransport,
} from "./transport";

export type HoverInfo = {
  startIdx: number;
  endIdx: number;
  name: string;
  typ: string;
};
export type CompletionEntry = { name: string; typ: string; kind: string };
export type ErrorRange = { startIdx: number; endIdx: number };
export type CompileInfoResponse = {
  error: string;
  errorRange: ErrorRange | null;
  hovers: HoverInfo[];
  completions: CompletionEntry[];
  resultType: string;
};
export type EvalResponse = {
  cSharp: string;
  result: string;
  output: string;
  error: string;
};
export type PreludeEntry = { name: string; typ: string; fields: string[] };
export type CompletionAtInfo = { isMemberAccess: boolean; fields: string[] };

// The dataflow picture of a source, read off the solved AST by the compiler.
// `startIdx`/`endIdx` are -1 for nodes with no place in the user's text.
export type GraphNode = {
  id: string;
  kind: string;
  label: string;
  detail: string;
  typ: string;
  binds: string;
  group: string;
  startIdx: number;
  endIdx: number;
};
export type GraphEdge = {
  fromId: string;
  toId: string;
  port: string;
  typ: string;
};
export type GraphGroup = { id: string; label: string; parent: string };
export type GraphResponse = {
  error: string;
  nodes: GraphNode[];
  edges: GraphEdge[];
  groups: GraphGroup[];
  rootId: string;
};

export type TypeFighterLanguageClient = {
  fetchCompileInfo(source: string): Promise<CompileInfoResponse>;
  fetchEval(source: string): Promise<EvalResponse>;
  fetchPrelude(): Promise<PreludeEntry[]>;
  fetchCompletionAt(source: string, offset: number): Promise<CompletionAtInfo>;
  fetchGraph(source: string): Promise<GraphResponse>;
};

type TfLanguageRequest = Omit<TfRequest, "url"> & { path: string };

export function createTypeFighterLanguageClient(
  request: TfTransport,
  languageBasePath = ""
): TypeFighterLanguageClient {
  const send = <T>(req: TfLanguageRequest): Promise<T> => {
    const { path, ...rest } = req;
    return request<T>({ ...rest, url: languageBasePath + path });
  };

  return {
    fetchCompileInfo: (source) =>
      send<CompileInfoResponse>({
        method: "POST",
        path: "/compile-info",
        data: { source },
      }),
    fetchEval: (source) =>
      send<EvalResponse>({ method: "POST", path: "/eval", data: { source } }),
    fetchPrelude: () =>
      send<PreludeEntry[]>({ method: "GET", path: "/prelude" }),
    fetchCompletionAt: (source, offset) =>
      send<CompletionAtInfo>({
        method: "POST",
        path: "/completion-at",
        data: { source, offset },
      }),
    fetchGraph: (source) =>
      send<GraphResponse>({ method: "POST", path: "/graph", data: { source } }),
  };
}

export const defaultTypeFighterLanguageClient: TypeFighterLanguageClient = {
  fetchCompileInfo: (source) =>
    tfLanguageRequest<CompileInfoResponse>({
      method: "POST",
      path: "/compile-info",
      data: { source },
    }),
  fetchEval: (source) =>
    tfLanguageRequest<EvalResponse>({
      method: "POST",
      path: "/eval",
      data: { source },
    }),
  fetchPrelude: () =>
    tfLanguageRequest<PreludeEntry[]>({ method: "GET", path: "/prelude" }),
  fetchCompletionAt: (source, offset) =>
    tfLanguageRequest<CompletionAtInfo>({
      method: "POST",
      path: "/completion-at",
      data: { source, offset },
    }),
  fetchGraph: (source) =>
    tfLanguageRequest<GraphResponse>({
      method: "POST",
      path: "/graph",
      data: { source },
    }),
};

export function fetchCompileInfo(source: string): Promise<CompileInfoResponse> {
  return defaultTypeFighterLanguageClient.fetchCompileInfo(source);
}

export function fetchEval(source: string): Promise<EvalResponse> {
  return defaultTypeFighterLanguageClient.fetchEval(source);
}

export function fetchPrelude(): Promise<PreludeEntry[]> {
  return defaultTypeFighterLanguageClient.fetchPrelude();
}

export function fetchCompletionAt(
  source: string,
  offset: number
): Promise<CompletionAtInfo> {
  return defaultTypeFighterLanguageClient.fetchCompletionAt(source, offset);
}

export function fetchGraph(source: string): Promise<GraphResponse> {
  return defaultTypeFighterLanguageClient.fetchGraph(source);
}
