# TypeFighter

> **📖 [Browse the generated docs site →](https://schlenkr.github.io/TypeFighter/)**

TypeFighter is a small, experimental language built around a modern, inference-first type system. The headline feature is **structural records instead of nominal ones**: records are compared by the fields they actually have, not by a declared name — so a function that needs `{ name: String }` accepts *any* record with that field, no boilerplate declarations required.

On top of that: row polymorphism, set-theoretic literal and union types, and classical polymorphic functions — all figured out by the type checker with (almost) no annotations.

> Companion repository for the YouTube video: [Type Inference Explained](https://www.youtube.com/watch?v=fSRTVrjvo70)

## Playground

The playground is a fully static site: the complete compiler (including Roslyn) runs in the browser via WebAssembly. Examples double as a top-to-bottom language tutorial.

```bash
./build-wasm.sh          # publish the WASM compiler into site/public/wasm
pnpm install
cd site && pnpm dev      # http://localhost:3100
```

## Running the test suite

```bash
dotnet test
```

## Project layout

```
TypeFighter/
├── src/
│   ├── TypeFighter/         # Language core: parser, desugar, solver, C# emit, Roslyn
│   ├── TypeFighter.Web/     # Hosting facade: builtin catalog, eval service, API surface
│   ├── TypeFighter.Wasm/    # Browser host: the compiler as WebAssembly (JSExport facade)
│   └── TypeFighter.Tests/   # NUnit tests
├── libs/LocSta2/            # Vendored stream-block library (math, gen, logic, dsp)
├── editor/                  # @purestate/typefighter-editor: Monaco language support,
│                            # editor component, playground, tutorial overview
├── site/                    # Static playground site (Vite), served without any backend
├── docs/design/             # Design ADRs
└── build-wasm.sh            # dotnet publish + copy the WASM bundle into site/
```

## License

This project is **not available for use** in any projects, whether commercial or non-commercial. If you are interested in using it, please [contact me](https://github.com/SchlenkR) directly.
