import { defineConfig } from "vite";
import { resolve } from "path";

// Single-page app that consumes the Fable-compiled TypeFighter compiler
// as an ESM library. For the library artifact (no HTML, one JS file that
// exports `compile`) run `vite build --config vite.lib.config.ts` — this
// default config is for the dev server and the Monaco app bundle.
//
// `base` is read from the env so the same config serves local dev at `/`
// and the GitHub Pages deploy at `/TypeFighter/playground/`. The GH
// Actions workflow sets PLAYGROUND_BASE before running `npm run build`.
export default defineConfig({
    root: ".",
    base: process.env.PLAYGROUND_BASE || "/",
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
