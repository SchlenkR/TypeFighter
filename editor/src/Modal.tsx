import { useEffect, ReactNode } from "react";

// A themed modal overlay: dimmed backdrop + centered card. Closes on backdrop
// click and Escape.
//
// CSS contract: hosts theme the modal by defining these variables (all have
// dark fallbacks): --tfe-overlay-bg, --tfe-panel-bg, --tfe-text, --tfe-border,
// --tfe-shadow, --tfe-icon.
export default function Modal({
  title,
  onClose,
  children,
  width = 520,
  height,
}: {
  title: ReactNode;
  onClose: () => void;
  children: ReactNode;
  width?: number | string;
  height?: number | string;
}) {
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === "Escape") onClose(); };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose]);

  return (
    <div
      onMouseDown={(e) => { if (e.target === e.currentTarget) onClose(); }}
      style={{
        position: "fixed", inset: 0, zIndex: 100,
        background: "var(--tfe-overlay-bg, rgba(0, 0, 0, 0.55))",
        display: "flex", alignItems: "center", justifyContent: "center",
      }}
    >
      <div
        style={{
          width, height, maxWidth: "calc(100vw - 48px)", maxHeight: "calc(100vh - 48px)",
          display: "flex", flexDirection: "column",
          background: "var(--tfe-panel-bg, #252526)",
          color: "var(--tfe-text, #eeeeee)",
          border: "1px solid var(--tfe-border, #3c3c3c)",
          borderRadius: 8,
          boxShadow: "0 24px 80px var(--tfe-shadow, rgba(0, 0, 0, 0.45)), 0 6px 20px var(--tfe-shadow, rgba(0, 0, 0, 0.45))",
          overflow: "hidden",
        }}
      >
        <div style={{
          display: "flex", alignItems: "center", justifyContent: "space-between",
          padding: "12px 16px",
          borderBottom: "1px solid var(--tfe-border, #3c3c3c)",
          flexShrink: 0,
        }}>
          <div style={{ fontSize: 15, fontWeight: 600 }}>{title}</div>
          <button
            onClick={onClose}
            title="Schließen"
            style={{
              background: "transparent", color: "var(--tfe-icon, var(--tfe-text, #eeeeee))",
              border: "none", fontSize: 20, lineHeight: 1, cursor: "pointer", padding: "0 4px",
            }}
          >
            x
          </button>
        </div>
        <div style={{ flex: 1, minHeight: 0, display: "flex", flexDirection: "column" }}>
          {children}
        </div>
      </div>
    </div>
  );
}
