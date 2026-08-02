import { ReactNode } from "react";

// Klickbare Starter-Beispiele: je eins pro Idee. Der Host liefert die Inhalte
// (title/description/name/source) und entscheidet in onPick, was "Beispiel
// laden" bedeutet (z.B. Quelltext + Namensvorschlag in den Editor).
//
// CSS contract (dark fallbacks): --tfe-accent, --tfe-text, --tfe-text-muted,
// --tfe-border, --tfe-hover-bg.
export type CurveExample = { title: string; description: string; name: string; source: string };

export function ExampleGallery({
  examples,
  onPick,
  disabled,
  footerHint,
}: {
  examples: CurveExample[];
  onPick: (ex: CurveExample) => void;
  disabled?: boolean;
  footerHint?: ReactNode;
}) {
  return (
    <div style={{ flex: 1, minHeight: 0, overflowY: "auto" }}>
      {examples.map((ex) => (
        <div
          key={ex.title}
          onClick={() => { if (!disabled) onPick(ex); }}
          title="Klick lädt das Beispiel als neue Kurve in den Editor"
          style={{ padding: "8px 10px", cursor: disabled ? "default" : "pointer", borderBottom: "1px solid var(--tfe-border, #3c3c3c)" }}
          onMouseEnter={(e) => (e.currentTarget.style.background = "var(--tfe-hover-bg, rgba(255, 255, 255, 0.06))")}
          onMouseLeave={(e) => (e.currentTarget.style.background = "transparent")}
        >
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--tfe-accent, #4ec9b0)" }}>{ex.title}</div>
          <div style={{ fontSize: 11, color: "var(--tfe-text-muted, #9d9d9d)", margin: "2px 0 4px" }}>{ex.description}</div>
          <div style={{
            fontSize: 10, fontFamily: "monospace", color: "var(--tfe-text, #eeeeee)", opacity: 0.85,
            whiteSpace: "pre", overflow: "hidden", textOverflow: "ellipsis",
          }}>
            {ex.source.split("\n").filter((l) => l.trim() && !l.trim().startsWith("//")).slice(0, 3).join("\n")}
          </div>
        </div>
      ))}
      {footerHint && (
        <div style={{ padding: "8px 10px", fontSize: 10, color: "var(--tfe-text-muted, #9d9d9d)" }}>
          {footerHint}
        </div>
      )}
    </div>
  );
}
