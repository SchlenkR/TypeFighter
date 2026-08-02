// Curated sample programs grouped by feature; `examples.json` is the source
// of truth. Adding an example: just add it to examples.json. No code changes.
// Hosts contribute domain categories via insertCategoriesBefore and pass the
// merged list to TypeFighterPlayground.
import data from "./examples.json";

export type Example = {
    title: string;
    source: string;
};

// Tutorial content between a category's lead line and its example cards:
// prose paragraphs and NON-runnable illustration code (e.g. comparisons
// with other languages). Runnable snippets belong in `examples`.
export type IntroBlock =
    | { text: string }
    | { code: string; lang?: "typefighter" | "csharp" | "text"; caption?: string };

export type Category = {
    name: string;
    // Concept notes shown on the overview page: what the block is about.
    description?: string;
    // Optional tutorial blocks, rendered in order before the cards.
    intro?: IntroBlock[];
    examples: Example[];
    expectsError?: boolean;
};

export const defaultCategories: Category[] = data as Category[];

export function insertCategoriesBefore(
    base: Category[],
    beforeName: string,
    extra: Category[],
): Category[] {
    const idx = base.findIndex((c) => c.name === beforeName);
    if (idx < 0) return [...base, ...extra];
    return [...base.slice(0, idx), ...extra, ...base.slice(idx)];
}
