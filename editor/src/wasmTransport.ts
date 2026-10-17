// TfTransport implementation backed by the .NET WASM compiler in a Web
// Worker - the serverless alternative to an HTTP transport. Usage:
//
//   const request = await createTypeFighterWasmTransport({ dotnetUrl: "/wasm/_framework/dotnet.js" });
//   configureTypeFighterTransport({ request, languageBasePath: "" });
import type { TfRequest, TfTransport } from "./transport";
import type {
  TfWorkerRequestBody,
  TfWorkerResponse,
} from "./wasm/typefighter.worker";

export type TfWasmStatus = "booting" | "ready";

export type TypeFighterWasmNumberRecord = {
  name: string;
  fields: string[];
};

export type TypeFighterWasmOptions = {
  // URL of dotnet.js inside the published _framework folder,
  // e.g. "/wasm/_framework/dotnet.js".
  dotnetUrl: string;
  numberRecords?: TypeFighterWasmNumberRecord[];
  onStatus?: (status: TfWasmStatus) => void;
  onFatalError?: (error: Error) => void;
};

export async function createTypeFighterWasmTransport(
  opts: TypeFighterWasmOptions
): Promise<TfTransport> {
  opts.onStatus?.("booting");
  const worker = new Worker(
    new URL("./wasm/typefighter.worker.ts", import.meta.url),
    { type: "module" }
  );

  let nextSeq = 1;
  const pending = new Map<
    number,
    { resolve: (v: unknown) => void; reject: (e: Error) => void }
  >();
  let terminalError: Error | undefined;

  const failWorker = (error: Error): Error => {
    if (terminalError) return terminalError;

    terminalError = error;
    for (const entry of pending.values()) entry.reject(terminalError);
    pending.clear();
    worker.terminate();
    opts.onFatalError?.(terminalError);
    return terminalError;
  };

  worker.onmessage = (e: MessageEvent<TfWorkerResponse>) => {
    const res = e.data;
    const entry = pending.get(res.seq);
    if (!entry) return;
    pending.delete(res.seq);
    if (res.ok) entry.resolve(res.result);
    else entry.reject(failWorker(new Error(res.error)));
  };
  worker.onerror = (e) => {
    failWorker(
      new Error("TypeFighter WASM worker error: " + (e.message ?? "unknown"))
    );
  };
  worker.onmessageerror = () => {
    failWorker(
      new Error("TypeFighter WASM worker returned an unreadable response.")
    );
  };

  const call = (req: TfWorkerRequestBody): Promise<unknown> =>
    new Promise((resolve, reject) => {
      if (terminalError) {
        reject(terminalError);
        return;
      }
      const seq = nextSeq++;
      pending.set(seq, { resolve, reject });
      try {
        worker.postMessage({ ...req, seq });
      } catch (error) {
        failWorker(error instanceof Error ? error : new Error(String(error)));
      }
    });

  const dotnetUrl = new URL(opts.dotnetUrl, document.baseURI).href;
  try {
    await call({
      kind: "init",
      dotnetUrl,
      numberRecords: opts.numberRecords ?? [],
    });
  } catch (error) {
    const initError = error instanceof Error ? error : new Error(String(error));
    throw failWorker(initError);
  }
  opts.onStatus?.("ready");

  return <T>(req: TfRequest): Promise<T> => {
    const source = (req.data as { source?: string } | undefined)?.source ?? "";
    switch (`${req.method} ${req.url}`) {
      case "POST /compile-info":
        return call({ kind: "compileInfo", source }) as Promise<T>;
      case "POST /eval":
        return call({ kind: "eval", source }) as Promise<T>;
      case "GET /prelude":
        return call({ kind: "prelude" }) as Promise<T>;
      case "POST /graph":
        return call({ kind: "graph", source }) as Promise<T>;
      case "POST /completion-at":
        return call({
          kind: "completionAt",
          source,
          offset: (req.data as { offset?: number } | undefined)?.offset ?? 0,
        }) as Promise<T>;
      default:
        return Promise.reject(
          new Error(
            `TypeFighter WASM transport: unsupported request ${req.method} ${req.url}`
          )
        );
    }
  };
}
