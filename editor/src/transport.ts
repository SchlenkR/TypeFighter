// Host-injected HTTP transport. The package never talks to a concrete HTTP
// client or hardcodes route prefixes; the hosting app wires both once at
// startup via configureTypeFighterTransport.

export type TfRequest = {
  method: "GET" | "POST";
  url: string;
  data?: unknown;
  signal?: AbortSignal;
  timeout?: number;
};

export type TfTransport = <T>(req: TfRequest) => Promise<T>;

export type TypeFighterTransportConfig = {
  request: TfTransport;
  // Prefix of the language endpoints (compile-info, eval, prelude,
  // completion-at), e.g. "/app-demo/typefighter".
  languageBasePath: string;
  // Prefix of the formula-assistant endpoints (chat, models). Optional;
  // without it the assistant functions throw.
  assistantBasePath?: string;
};

let config: TypeFighterTransportConfig | undefined;

export function configureTypeFighterTransport(c: TypeFighterTransportConfig) {
  config = c;
}

type TfPathRequest = {
  method: "GET" | "POST";
  path: string;
  data?: unknown;
  signal?: AbortSignal;
  timeout?: number;
};

function requireConfig(): TypeFighterTransportConfig {
  if (!config) {
    throw new Error(
      "TypeFighter transport is not configured. Call configureTypeFighterTransport({ request, languageBasePath, ... }) once at app startup.");
  }
  return config;
}

export function tfLanguageRequest<T>(req: TfPathRequest): Promise<T> {
  const c = requireConfig();
  return c.request<T>({ ...req, url: c.languageBasePath + req.path });
}

export function tfAssistantRequest<T>(req: TfPathRequest): Promise<T> {
  const c = requireConfig();
  if (!c.assistantBasePath) {
    throw new Error(
      "TypeFighter assistant endpoints are not configured. Set assistantBasePath in configureTypeFighterTransport.");
  }
  return c.request<T>({ ...req, url: c.assistantBasePath + req.path });
}
