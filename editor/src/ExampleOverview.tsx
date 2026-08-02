import { useEffect, useState } from "react";
import * as monaco from "monaco-editor";
import { type Category, type Example, type IntroBlock } from "./examples";

// Card overview of all examples, grouped by category. The leading comment
// lines become a prose description; the remaining code is rendered with the
// real Monarch tokenizer via monaco.editor.colorize (no editor instances).
// Standalone building block; hosts decide what onPick does.
//
// CSS contract (dark fallbacks): --tfe-accent, --tfe-danger, --tfe-text,
// --tfe-text-muted, --tfe-border, --tfe-card-bg, --tfe-hover-bg.

function splitExample(source: string): { description: string; code: string } {
  const lines = source.split("\n");
  const commentText = lines
    .filter((l) => l.trim().startsWith("//"))
    .map((l) => l.trim().replace(/^\/\/\s?/, ""))
    .filter((l) => l.length > 0)
    .join(" ");
  const code = lines.filter((l) => !l.trim().startsWith("//")).join("\n").trim();
  return { description: commentText, code };
}

// Token colors mirroring the typefighter-dark theme (see monaco.ts).
const TOKEN_COLORS: Record<string, string> = {
  keyword: "#C586C0",
  predefined: "#4EC9B0",
  type: "#DCDCAA",
  string: "#CE9178",
  number: "#B5CEA8",
  comment: "#6A9955",
  operator: "#D4D4D4",
  delimiter: "#808080",
};

type TokenLine = { text: string; type: string }[];

// monaco.editor.tokenize runs the same Monarch tokenizer the editor uses --
// no editor instance per card needed. The language may still be initialising
// on the very first render, so an untyped-only result is retried once.
function ColorizedCode({ code, lang = "typefighter" }: { code: string; lang?: string }) {
  const [lines, setLines] = useState<TokenLine[] | null>(null);
  useEffect(() => {
    if (lang === "text") { setLines(null); return; }
    let alive = true;
    let retryTimer: number | undefined;
    const tokenizeNow = (isRetry: boolean) => {
      const raw = monaco.editor.tokenize(code, lang);
      const srcLines = code.split("\n");
      const result: TokenLine[] = srcLines.map((line, i) => {
        const tokens = raw[i] ?? [];
        if (tokens.length === 0) return [{ text: line, type: "" }];
        return tokens.map((t, j) => ({
          text: line.slice(t.offset, j + 1 < tokens.length ? tokens[j + 1].offset : line.length),
          type: t.type.split(".")[0],
        }));
      });
      const anyTyped = result.some((l) => l.some((t) => t.type !== "" && t.text.trim() !== ""));
      if (!anyTyped && !isRetry) {
        retryTimer = window.setTimeout(() => { if (alive) tokenizeNow(true); }, 300);
        return;
      }
      if (alive) setLines(result);
    };
    tokenizeNow(false);
    return () => {
      alive = false;
      if (retryTimer !== undefined) window.clearTimeout(retryTimer);
    };
  }, [code, lang]);

  const style: React.CSSProperties = {
    margin: 0, fontSize: 13, lineHeight: 1.6,
    fontFamily: "ui-monospace, Menlo, monospace",
    color: "var(--tfe-text, #d4d4d4)",
    background: "transparent",
    whiteSpace: "pre", overflowX: "auto",
  };
  if (lines === null) return <pre style={style}>{code}</pre>;
  return (
    <pre style={style}>
      {lines.map((line, i) => (
        <div key={i}>
          {line.map((t, j) => {
            const color = TOKEN_COLORS[t.type];
            return <span key={j} style={color ? { color } : undefined}>{t.text}</span>;
          })}
          {line.every((t) => t.text === "") ? " " : null}
        </div>
      ))}
    </pre>
  );
}

// A tutorial block between the category lead and its cards: a prose
// paragraph or a static (non-clickable) illustration code block.
function IntroBlockView({ block }: { block: IntroBlock }) {
  if ("text" in block) {
    return (
      <div style={{
        margin: "0 0 10px", maxWidth: 760,
        fontSize: 13, lineHeight: 1.6,
        fontFamily: "system-ui, sans-serif",
        color: "var(--tfe-text, #c8c8c8)",
      }}>
        {block.text}
      </div>
    );
  }
  return (
    <div style={{
      margin: "0 0 12px", maxWidth: 760,
      border: "1px solid var(--tfe-border, #3c3c3c)",
      borderRadius: 6, padding: "10px 12px",
      background: "var(--tfe-card-bg, #252526)",
    }}>
      {block.caption && (
        <div style={{
          fontSize: 11, marginBottom: 6,
          fontFamily: "system-ui, sans-serif",
          color: "var(--tfe-text-muted, #9d9d9d)",
        }}>
          {block.caption}
        </div>
      )}
      <ColorizedCode code={block.code} lang={block.lang ?? "typefighter"} />
    </div>
  );
}

export function ExampleOverview({
  categories,
  onPick,
}: {
  categories: Category[];
  onPick: (ex: Example, cat: Category) => void;
}) {
  return (
    <div style={{ padding: "16px 24px 32px", overflowY: "auto", height: "100%", boxSizing: "border-box" }}>
      {/* Centered reading column: tutorial pages should not span huge monitors. */}
      <div style={{ maxWidth: 1040, margin: "0 auto" }}>
      {categories.map((cat, catIdx) => (
        <section
          key={cat.name}
          style={{
            marginBottom: 56,
            // Dezente Trennlinie strukturiert die Sektionen beim Scrollen.
            borderTop: catIdx > 0 ? "1px solid var(--tfe-border, #3c3c3c)" : "none",
            paddingTop: catIdx > 0 ? 32 : 0,
          }}
        >
          <div style={{
            margin: "0 0 10px", fontSize: 19, fontWeight: 700,
            textTransform: "uppercase", letterSpacing: 1.2,
            fontFamily: "system-ui, sans-serif",
            color: cat.expectsError ? "var(--tfe-danger, #f48771)" : "var(--tfe-text, #e8e8e8)",
          }}>
            {cat.name}
          </div>
          {cat.description && (
            <div style={{
              margin: "0 0 10px", maxWidth: 760,
              fontSize: 13, lineHeight: 1.55, fontStyle: "italic",
              fontFamily: "system-ui, sans-serif",
              color: "var(--tfe-text, #c8c8c8)", opacity: 0.75,
            }}>
              {cat.description}
            </div>
          )}
          {cat.intro?.map((block, i) => <IntroBlockView key={i} block={block} />)}
          <div style={{
            display: "grid",
            gridTemplateColumns: "repeat(auto-fill, minmax(350px, 1fr))",
            gap: 16,
          }}>
            {cat.examples.map((ex) => {
              const { description, code } = splitExample(ex.source);
              return (
                <div
                  key={ex.title}
                  className="tfe-example-card"
                  onClick={() => onPick(ex, cat)}
                  title="Klick oeffnet das Beispiel im Editor"
                  style={{
                    border: "1px solid var(--tfe-border, #3c3c3c)",
                    borderRadius: 6,
                    padding: "12px 14px",
                    cursor: "pointer",
                    background: "var(--tfe-card-bg, #252526)",
                    minWidth: 0,
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = "var(--tfe-hover-bg, rgba(255, 255, 255, 0.06))")}
                  onMouseLeave={(e) => (e.currentTarget.style.background = "var(--tfe-card-bg, #252526)")}
                >
                  <div style={{
                    fontSize: 15, fontWeight: 600, marginBottom: 6,
                    fontFamily: "system-ui, sans-serif",
                    color: "var(--tfe-text, #e8e8e8)",
                    whiteSpace: "nowrap", overflow: "hidden", textOverflow: "ellipsis",
                  }}>
                    {ex.title}
                  </div>
                  {description && (
                    <div style={{
                      fontSize: 12, lineHeight: 1.5, marginBottom: 10,
                      color: "var(--tfe-text-muted, #9d9d9d)",
                    }}>
                      {description}
                    </div>
                  )}
                  <ColorizedCode code={code} />
                </div>
              );
            })}
          </div>
        </section>
      ))}
      </div>
    </div>
  );
}
