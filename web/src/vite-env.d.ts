// Ambient types for Vite's asset and worker query-suffix imports.
// The `?worker` suffix tells Vite to emit the module as a dedicated
// Web Worker and import a constructor.
declare module "*?worker" {
    const workerCtor: new () => Worker;
    export default workerCtor;
}

declare module "*?raw" {
    const src: string;
    export default src;
}

// Minimal shape of Vite's injected `import.meta.env`. The full type
// lives in `vite/client`, but pulling that in would require a broader
// types dependency — we only use the two flags below.
interface ImportMetaEnv {
    readonly MODE: string;
    readonly PROD: boolean;
    readonly DEV: boolean;
    readonly BASE_URL: string;
}
interface ImportMeta {
    readonly env: ImportMetaEnv;
}
