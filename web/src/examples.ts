// Curated sample programs grouped by feature. The source of truth is
// `examples.json` — it's shared with the F# smoke-test harness
// (TypeFighter.Web.Tests), which compiles every example and asserts
// the "Type errors" category fails while the rest succeed.
//
// Adding an example: just add it to examples.json. No code changes.
import data from "./examples.json";

export type Example = {
    title: string;
    source: string;
};

export type Category = {
    name: string;
    examples: Example[];
    expectsError?: boolean;
};

export const categories: Category[] = data as Category[];
