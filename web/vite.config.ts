import { defineConfig } from "vite";
import { resolve } from "path";

// Single-page app that consumes the Fable-compiled TypeFighter compiler
// as an ESM library. For the library artifact (no HTML, one JS file that
// exports `compile`) run `vite build --config vite.lib.config.ts` — this
// default config is for the dev server and the Monaco app bundle.
export default defineConfig({
    root: ".",
    server: {
        port: 5173,
        open: false
    },
    build: {
        outDir: "dist",
        emptyOutDir: true,
        target: "es2022",
        sourcemap: true,
        rollupOptions: {
            input: resolve(__dirname, "index.html")
        }
    }
});
