// Web Worker hosting the .NET WASM runtime with the TypeFighter compiler.
// Pure request/response - no streaming, no watchdog (the language is total).

// dotnet/runtime#114918: without this the loader treats the worker as a
// runtime-internal one and never boots.
(globalThis as { dotnetSidecar?: boolean }).dotnetSidecar = true;

// Native dynamic import, hidden from bundlers: the runtime is served as
// static files, not importable through their transforms.
const importNative = new Function("url", "return import(url)") as (url: string) => Promise<{ dotnet: any }>;

type TfWasmExports = {
  Init(): string;
  CompileInfo(source: string): string;
  Eval(source: string): string;
  Prelude(): string;
  CompletionAt(source: string, offset: number): string;
};

export type TfWorkerRequestBody =
  | { kind: "init"; dotnetUrl: string }
  | { kind: "compileInfo"; source: string }
  | { kind: "eval"; source: string }
  | { kind: "prelude" }
  | { kind: "completionAt"; source: string; offset: number };

export type TfWorkerRequest = TfWorkerRequestBody & { seq: number };

export type TfWorkerResponse =
  | { seq: number; ok: true; result: unknown }
  | { seq: number; ok: false; error: string };

const scope = self as unknown as Worker;
let interop: TfWasmExports | null = null;

async function boot(dotnetUrl: string): Promise<unknown> {
  const { dotnet } = await importNative(new URL(dotnetUrl, self.location.origin).href);
  const { getAssemblyExports, getConfig } = await dotnet
    .withDiagnosticTracing(false)
    .create();
  const exports = await getAssemblyExports(getConfig().mainAssemblyName);
  interop = exports.PureState.TypeFighter.Wasm.TypeFighterInterop as TfWasmExports;
  const init = JSON.parse(interop.Init()) as { error?: string };
  if (init.error) throw new Error("TypeFighter WASM init failed: " + init.error);
  return init;
}

const requireInterop = (): TfWasmExports => {
  if (!interop) throw new Error("TypeFighter WASM runtime not initialized");
  return interop;
};

scope.onmessage = async (e: MessageEvent<TfWorkerRequest>) => {
  const req = e.data;
  try {
    let result: unknown;
    switch (req.kind) {
      case "init":
        result = await boot(req.dotnetUrl);
        break;
      case "compileInfo":
        result = JSON.parse(requireInterop().CompileInfo(req.source));
        break;
      case "eval":
        result = JSON.parse(requireInterop().Eval(req.source));
        break;
      case "prelude":
        result = JSON.parse(requireInterop().Prelude());
        break;
      case "completionAt":
        result = JSON.parse(requireInterop().CompletionAt(req.source, req.offset));
        break;
    }
    scope.postMessage({ seq: req.seq, ok: true, result } satisfies TfWorkerResponse);
  } catch (err) {
    scope.postMessage({ seq: req.seq, ok: false, error: String(err) } satisfies TfWorkerResponse);
  }
};
