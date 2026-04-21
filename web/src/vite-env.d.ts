// Ambient types for Vite's asset and worker query-suffix imports.
// The `?worker` suffix tells Vite to emit the module as a dedicated
// Web Worker and import a constructor.
declare module "*?worker" {
    const workerCtor: new () => Worker;
    export default workerCtor;
}
