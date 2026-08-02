import { useEffect, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  TypeFighterPlayground,
  configureTypeFighterTransport,
  createTypeFighterWasmTransport,
} from "@purestate/typefighter-editor";

const screenStyle: React.CSSProperties = {
  height: "100%", display: "flex", flexDirection: "column", gap: 12,
  alignItems: "center", justifyContent: "center",
  background: "#1e1e1e", color: "#d4d4d4",
  fontFamily: "system-ui, sans-serif", fontSize: 14,
};

// Das TypeFighter-Logo (assets/logo.svg), in drei animierbare Gruppen
// zerlegt: linker Kaempfer, rechter Kaempfer, Forall-Kern.
function AnimatedLogo() {
  return (
    <svg className="tf-logo" viewBox="0 0 140 60" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
      <g stroke="currentColor" strokeWidth="2" fill="none" strokeLinejoin="round" strokeLinecap="round">
        <g className="tf-left">
          <path d="M 14 10 L 34 6 L 40 30 L 34 54 L 14 50 Z" />
          <line x1="24" y1="8" x2="24" y2="52" />
          <line x1="40" y1="30" x2="54" y2="30" />
        </g>
        <g className="tf-right">
          <path d="M 126 10 L 106 6 L 100 30 L 106 54 L 126 50 Z" />
          <line x1="116" y1="8" x2="116" y2="52" />
          <line x1="100" y1="30" x2="86" y2="30" />
        </g>
      </g>
      <g className="tf-core">
        <circle cx="70" cy="30" r="15" fill="#181825" stroke="currentColor" strokeWidth="2" />
        <text x="70" y="36" textAnchor="middle" fontSize="17" fontFamily="'Times New Roman', serif" fill="currentColor" fontWeight="700">{"∀"}</text>
      </g>
    </svg>
  );
}

function Tile({ title, text, onClick }: { title: string; text: string; onClick: () => void }) {
  return (
    <div
      className="tf-tile"
      onClick={onClick}
      style={{
        width: 300, padding: "20px 22px", cursor: "pointer",
        border: "1px solid #3c3c3c", borderRadius: 10, background: "#252526",
        transition: "border-color 0.15s",
      }}
      onMouseEnter={(e) => (e.currentTarget.style.borderColor = "#4ec9b0")}
      onMouseLeave={(e) => (e.currentTarget.style.borderColor = "#3c3c3c")}
    >
      <div style={{ fontSize: 18, fontWeight: 600, color: "#4ec9b0", marginBottom: 8 }}>{title}</div>
      <div style={{ fontSize: 13.5, lineHeight: 1.55, color: "#c8c8c8" }}>{text}</div>
    </div>
  );
}

function Landing({ onPick }: { onPick: (page: "docs" | "playground") => void }) {
  return (
    <div style={{ ...screenStyle, gap: 20 }}>
      <AnimatedLogo />
      <div style={{ fontSize: 26, fontWeight: 600, letterSpacing: 0.5, color: "#4ec9b0" }}>TypeFighter</div>
      <div style={{ maxWidth: 620, textAlign: "center", fontSize: 14.5, lineHeight: 1.6, color: "#c8c8c8" }}>
        An inference-first expression language: structural records, constrained
        polymorphism and a monadic step protocol, compiled to C# and executed
        by Roslyn.
      </div>
      <div style={{ display: "flex", gap: 18, marginTop: 10, flexWrap: "wrap", justifyContent: "center" }}>
        <Tile
          title="Documentation"
          text="A guided tour of the language: every concept explained, every example one click away from running."
          onClick={() => onPick("docs")}
        />
        <Tile
          title="Playground"
          text="Write TypeFighter with full IDE support: hover types, completion, error squiggles. Run instantly."
          onClick={() => onPick("playground")}
        />
      </div>
      <div style={{ marginTop: 14, fontSize: 12, color: "#8a8a8a" }}>
        No backend involved: the complete compiler runs in your browser (WebAssembly).
      </div>
    </div>
  );
}

function App() {
  const [status, setStatus] = useState<"booting" | "ready" | "failed">("booting");
  const [error, setError] = useState("");
  const [page, setPage] = useState<"landing" | "docs" | "playground">("landing");

  // Boot starts immediately, so the compiler warms up while the visitor is
  // still reading the landing page.
  useEffect(() => {
    createTypeFighterWasmTransport({ dotnetUrl: `${import.meta.env.BASE_URL}wasm/_framework/dotnet.js` })
      .then((request) => {
        configureTypeFighterTransport({ request, languageBasePath: "" });
        setStatus("ready");
      })
      .catch((e) => {
        setError(String(e));
        setStatus("failed");
      });
  }, []);

  if (page === "landing") {
    return <Landing onPick={setPage} />;
  }
  if (status === "failed") {
    return (
      <div style={screenStyle}>
        <div style={{ fontSize: 20, color: "#f48771" }}>The compiler could not be loaded</div>
        <pre style={{ maxWidth: 700, whiteSpace: "pre-wrap", color: "#f48771" }}>{error}</pre>
      </div>
    );
  }
  if (status === "booting") {
    return (
      <div style={screenStyle}>
        <AnimatedLogo />
        <div style={{ fontSize: 18, fontWeight: 600, letterSpacing: 0.5, color: "#4ec9b0" }}>TypeFighter</div>
      </div>
    );
  }
  return (
    <>
      <TypeFighterPlayground key={page} initialView={page === "docs" ? "overview" : "editor"} />
      <button
        onClick={() => setPage("landing")}
        title="Back to start"
        style={{
          position: "fixed", left: 12, bottom: 12, zIndex: 50,
          padding: "4px 12px", fontSize: 12, cursor: "pointer",
          background: "#252526", color: "#9d9d9d",
          border: "1px solid #3c3c3c", borderRadius: 6,
        }}
      >
        Home
      </button>
    </>
  );
}

createRoot(document.getElementById("root")!).render(<App />);
