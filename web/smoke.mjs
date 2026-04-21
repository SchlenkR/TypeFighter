// Smoke test: import the Fable-compiled Api and exercise compile()
// against a handful of sources to confirm the F# → JS pipeline boots
// in plain Node without Vite.
import { compile } from "./fable-out/Api.js";

const cases = [
    {
        name: "hello",
        source: `let greeting = concat("Hello, ")("World");\nlog(greeting)`,
        expectError: false
    },
    {
        name: "type error",
        source: `log(1)`,
        expectError: true
    },
    {
        name: "parse error",
        source: `let x =`,
        expectError: true
    }
];

let failures = 0;
for (const c of cases) {
    const result = compile(c.source);
    const ok = c.expectError ? result.error !== "" : result.error === "";
    console.log(`${ok ? "PASS" : "FAIL"}  ${c.name}`);
    if (!ok) {
        failures++;
        console.log("  js:   ", JSON.stringify(result.js));
        console.log("  error:", JSON.stringify(result.error));
    }
}

process.exit(failures === 0 ? 0 : 1);
