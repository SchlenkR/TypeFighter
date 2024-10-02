# Type Fighter

Type Fighter is an experimental solver for ML-style programs, providing building language elements like functions, let- and do-bindings, and types like records and polymorphic types.

2 approaches: graph based (currently not working) and non-graph based (which works like a "Lineares Gleichungssystem").

Es ordnet jedem Term im Programm einen Typ zu und bricht bei Fehlern sofort ab.

There is no textual language available, but an AST with some convenience functions to write programs.

There's currently no runtime and no editor.

## Examples

## Playground / F# Interactive

Have a look at `b_nonGraphBasedSolver/src/TypeFighter.Tests/TestCases`. All tests are easy to understand, and you can use them directly in F# Interactive.

### Visualization and Debugging

Running tests in F# Interactive will print the AST and the type assignments. You can also use the `visualize` function to print the AST as a tree.

in the `b_nonGraphBasedSolver` directory:

```bash
npm i
npm run dev
```

Then open `http://localhost:3000` in your browser.


