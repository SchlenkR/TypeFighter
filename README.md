# TypeFighter

> **📖 [Playground + Docs](https://schlenkr.github.io/TypeFighter/)**

TypeFighter is a small, experimental language built around a modern, inference-first type system. The headline feature is **structural records instead of nominal ones**: records are compared by the fields they actually have, not by a declared name — so a function that needs `{ name: String }` accepts *any* record with that field, no boilerplate declarations required.

On top of that: row polymorphism, set-theoretic literal and union types, and classical polymorphic functions — all figured out by the type checker with (almost) no annotations.

> Companion repository for the YouTube video: [Type Inference Explained](https://www.youtube.com/watch?v=fSRTVrjvo70)

## Documentation

The [documentation site](https://schlenkr.github.io/TypeFighter/) is generated directly from the test suite under `src/TypeFighter.Tests/TestCases/`. Every test is a minimal example of what the type system can — and can't yet — do, grouped into categories:

Literals · Functions · Let bindings · Generalization · Arrays · Records · Rows · Polymorphism · Intersections · Type hierarchies

To build the site locally:

```bash
dotnet fsi docs/build.fsx
open docs/output/index.html
```

## Running the test suite

```bash
dotnet test
```

## Project layout

```
TypeFighter/
├── docs/
│   ├── build.fsx              # Site generator (reads tests → emits HTML)
│   └── output/                # Generated site (gitignored)
├── src/
│   ├── TypeFighter/           # Type inference engine
│   │   ├── Lang.fs            # Core types, constraints, solver
│   │   └── ExpressionDsl.fs   # AST-building DSL used in tests
│   ├── TypeFighter.Tests/     # NUnit tests; one file per feature area
│   └── visu/                  # Optional AST visualizer (TypeScript + Vite)
└── .github/workflows/docs.yml # CI: builds and deploys the docs site
```

## Visualizer (optional)

A legacy AST visualizer lives under `src/visu/`. To start it:

```bash
npm install
npm start
```

## License

This project is **not available for use** in any projects, whether commercial or non-commercial. If you are interested in using it, please [contact me](https://github.com/SchlenkR) directly.
