import { useEffect, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  TypeFighterPlayground,
  configureTypeFighterTransport,
  createTypeFighterWasmTransport,
} from "@purestate/typefighter-editor";

const bootStyle: React.CSSProperties = {
  height: "100%", display: "flex", flexDirection: "column", gap: 12,
  alignItems: "center", justifyContent: "center",
  background: "#1e1e1e", color: "#d4d4d4",
  fontFamily: "system-ui, sans-serif", fontSize: 14,
};

// Das TypeFighter-Logo (assets/logo.svg aus dem GitHub-Repo), in drei
// animierbare Gruppen zerlegt: linker Kaempfer, rechter Kaempfer, Forall-Kern.
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

function App() {
  const [status, setStatus] = useState<"booting" | "ready" | "failed">("booting");
  const [error, setError] = useState("");

  useEffect(() => {
    createTypeFighterWasmTransport({ dotnetUrl: "/wasm/_framework/dotnet.js" })
      .then((request) => {
        configureTypeFighterTransport({ request, languageBasePath: "" });
        setStatus("ready");
      })
      .catch((e) => {
        setError(String(e));
        setStatus("failed");
      });
  }, []);

  if (status === "booting") {
    return (
      <div style={bootStyle}>
        <AnimatedLogo />
        <div style={{ fontSize: 18, fontWeight: 600, letterSpacing: 0.5, color: "#4ec9b0" }}>TypeFighter</div>
      </div>
    );
  }
  if (status === "failed") {
    return (
      <div style={bootStyle}>
        <div style={{ fontSize: 20, color: "#f48771" }}>Compiler konnte nicht geladen werden</div>
        <pre style={{ maxWidth: 700, whiteSpace: "pre-wrap", color: "#f48771" }}>{error}</pre>
      </div>
    );
  }
  return <TypeFighterPlayground />;
}

createRoot(document.getElementById("root")!).render(<App />);
