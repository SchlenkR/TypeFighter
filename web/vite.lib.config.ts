import { defineConfig } from "vite";
import { resolve } from "path";

// Library build: emits a single IIFE-free ESM file that re-exports the
// Fable-compiled compiler API. Used by the smoke check and by anything
// that wants the compiler as a single-file dependency without Monaco.
export default defineConfig({
    build: {
        outDir: "dist-lib",
        emptyOutDir: true,
        target: "es2022",
        sourcemap: true,
        lib: {
            entry: resolve(__dirname, "fable-out/Api.js"),
            formats: ["es"],
            fileName: () => "typefighter.js"
        }
    }
});
