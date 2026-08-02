// KI-Formel-Assistent: baut den System-Prompt aus dem live geladenen Prelude
// (Namespace-Bausteine mit Signaturen) und führt einen Compile-Korrektur-Loop:
// jede vom Modell vorgeschlagene Formel wird gegen compile-info geprüft, Fehler
// gehen automatisch zurück ans Modell (max. 2 Korrekturrunden).
//
// Der LLM-Call läuft über einen Host-Durchreich-Endpunkt (das LLM selbst
// sendet keine CORS-Header, direkter Browser-Zugriff scheitert daran).
// Domänen-Anteile (Prompt-Zeilen, Beispiele, Settings-Defaults) injiziert der
// Host über configureTypeFighterAssistant.
import { tfAssistantRequest } from "./transport";
import { fetchCompileInfo, fetchPrelude, type PreludeEntry } from "./api";

export type ChatMessage = { role: "system" | "user" | "assistant"; content: string };

// Endpunkt/Key/Modell sind im KI-Tab einstellbar und überleben im
// localStorage. Leere Felder = Server-Defaults.
export type AssistantSettings = { baseUrl: string; apiKey: string; model: string };

export type AssistantPromptConfig = {
  // Satzteil hinter "eine kleine Sprache für ..."; Default "berechnete Kurven".
  purpose?: string;
  // Zusätzliche Zeilen im Block DIE SPRACHE (Domänen-Regeln des Hosts).
  domainRules?: string[];
  // Beispiel für Mehrfach-Bindungen (mit Einrückung, mehrere Zeilen).
  bindingExample?: string;
  // Beispiel für die Methoden-Schreibweise.
  methodCallExample?: string;
  // Kommentar-Beispiel im ANTWORTFORMAT (mit Einrückung, mehrere Zeilen).
  commentExample?: string;
  // Zusätzliche/überschreibende Parameter-Hinweise pro Baustein.
  extraBlockHints?: Record<string, string>;
};

export type AssistantHostConfig = {
  settingsStorageKey?: string;
  defaults?: Partial<AssistantSettings>;
  prompt?: AssistantPromptConfig;
};

let hostConfig: AssistantHostConfig = {};

export function configureTypeFighterAssistant(cfg: AssistantHostConfig) {
  hostConfig = cfg;
}

function settingsKey(): string {
  return hostConfig.settingsStorageKey ?? "typefighter:formula-assistant-settings";
}

function settingsDefaults(): AssistantSettings {
  return { baseUrl: "", apiKey: "", model: "", ...hostConfig.defaults };
}

// Modell-Liste des Endpunkts, über den Host-Proxy (CORS).
export async function fetchAssistantModels(baseUrl: string, apiKey: string): Promise<string[]> {
  const res = await tfAssistantRequest<{ models: string[]; error: string }>({
    method: "POST", path: "/models",
    data: { baseUrl: baseUrl.trim() || undefined, apiKey: apiKey.trim() || undefined },
    timeout: 30000,
  });
  if (res.error) throw new Error(res.error);
  return res.models ?? [];
}

export function loadAssistantSettings(): AssistantSettings {
  try {
    const s = localStorage.getItem(settingsKey());
    if (s) return { ...settingsDefaults(), ...JSON.parse(s) };
  } catch { /* defaults */ }
  return settingsDefaults();
}

export function saveAssistantSettings(s: AssistantSettings) {
  try { localStorage.setItem(settingsKey(), JSON.stringify(s)); } catch { /* ignore */ }
}

// Ein aufklappbarer Einblick-Schritt: Denkprozess, Formel-Versuch, Compile-Check.
export type TraceStep = { label: string; body: string };

export type AssistantTurn = {
  // Was im Chat angezeigt wird (Erklärtext des Modells, <think> entfernt).
  display: string;
  // Die extrahierte, compile-geprüfte Formel -- null, wenn keine (gültige) dabei war.
  formula: string | null;
  // Compile-Fehler der letzten Formel, falls auch nach Korrekturen rot.
  error: string | null;
  // Vollständiges Protokoll der Runde, im UI eingeklappt darstellbar.
  trace: TraceStep[];
};

// Sofort abbrechbar machen, auch wenn die Transport-Kette das `signal` nicht
// durchreicht: das Promise verliert dann nur das Rennen (der Server beendet
// die eine Anfrage noch ins Leere -- verschmerzbar).
function withAbort<T>(p: Promise<T>, signal?: AbortSignal): Promise<T> {
  if (!signal) return p;
  return new Promise<T>((resolve, reject) => {
    const onAbort = () => reject(new DOMException("abgebrochen", "AbortError"));
    if (signal.aborted) { onAbort(); return; }
    signal.addEventListener("abort", onAbort, { once: true });
    p.then(
      (v) => { signal.removeEventListener("abort", onAbort); resolve(v); },
      (e) => { signal.removeEventListener("abort", onAbort); reject(e); },
    );
  });
}

async function llmChat(messages: ChatMessage[], signal?: AbortSignal): Promise<string> {
  const cfg = loadAssistantSettings();
  const res = await withAbort(tfAssistantRequest<{ content: string; error: string }>({
    method: "POST", path: "/chat",
    signal,
    data: {
      baseUrl: cfg.baseUrl.trim() || undefined,
      apiKey: cfg.apiKey.trim() || undefined,
      model: cfg.model.trim() || undefined,
      messages,
    },
    timeout: 190000,
  }), signal);
  if (res.error) throw new Error(res.error);
  return res.content ?? "";
}

// Denk-Blöcke (Qwen & Co.) abtrennen statt wegwerfen: der Inhalt wird im
// Chat aufklappbar angeboten.
function splitThinking(s: string): { clean: string; thinking: string } {
  const parts: string[] = [];
  const clean = s.replace(/<think>([\s\S]*?)<\/think>/g, (_, t) => { parts.push(t.trim()); return ""; }).trim();
  return { clean, thinking: parts.join("\n\n") };
}

// Letzten ```-Codeblock extrahieren (das Antwortformat verlangt genau einen).
// WICHTIG: Ein Sprach-Tag nach den Backticks zählt nur, wenn direkt ein
// Zeilenumbruch folgt -- beginnt der Code auf derselben Zeile (```formel...),
// würde sonst das erste Wort der Formel als "Sprache" verschluckt.
export function extractFormula(s: string): string | null {
  const matches = [...s.matchAll(/```([a-zA-Z]*\n)?([\s\S]*?)```/g)];
  if (matches.length === 0) return null;
  const code = matches[matches.length - 1][2].trim();
  return code.length > 0 ? code : null;
}

// Kuratierte Parameter-Hinweise pro Baustein (Feedback des Modells: ohne
// Einheiten-Semantik sind die reinen Typen mehrdeutig). Fallback: Typ-String.
const BLOCK_HINTS: Record<string, string> = {
  "math.ema": "(quelle, alpha 0..1) -- exponentielle Glättung; startet auf dem ersten Quellwert",
  "math.movingAverage": "(quelle, n Samples) -- gleitender Mittelwert",
  "math.rollingStdDev": "(quelle, n Samples) -- Standardabweichung im Fenster",
  "math.rollingMin": "(quelle, n Samples) -- Minimum im Fenster",
  "math.rollingMax": "(quelle, n Samples) -- Maximum im Fenster",
  "math.runningMin": "(quelle) -- Allzeit-Minimum seit Kurvenstart",
  "math.runningMax": "(quelle) -- Allzeit-Maximum seit Kurvenstart",
  "math.diff": "(quelle, startwert) -- Differenz zum vorherigen Sample; erster Wert: quelle - startwert",
  "math.cumulativeSum": "(quelle, startwert) -- laufende Summe",
  "math.cumulativeProduct": "(quelle, startwert) -- laufendes Produkt",
  "math.clamp": "(quelle, min, max) -- begrenzt auf [min, max]",
  "math.rescale": "(quelle, vonMin, vonMax, nachMin, nachMax) -- lineare Umskalierung",
  "math.windowedReduce": "(quelle, n Samples, f wie a => b => a + b, startwert) -- Fold über das Fenster",
  "gen.counter": "(start, schritt) -- zählt pro Sample hoch; braucht keine Quelle",
  "gen.fibonacci": "-- Fibonacci-Folge pro Sample (Wert, kein Aufruf)",
  "gen.whiteNoise": "(seed ganzzahlig) -- deterministisches Rauschen -1..1",
  "gen.sineOsc": "(freq, sampleRate) -- Sinus -1..1; Phase += freq/sampleRate pro Sample; Periode = sampleRate/freq Samples",
  "gen.sawOsc": "(freq, sampleRate) -- Sägezahn -1..1, gleiche Frequenzlogik wie sineOsc",
  "gen.squareOsc": "(freq, pulsbreite 0..1, sampleRate) -- Rechteck -1..1",
  "gen.triangleOsc": "(freq, sampleRate) -- Dreieck -1..1",
  "logic.hold": "(quelle, prädikat wie v => v > 50, startwert) -- hält den letzten Treffer-Wert",
  "logic.latch": "(triggerBool, quelle, startwert) -- übernimmt quelle bei trigger=wahr, hält sonst",
  "logic.edge": "(triggerBool) -- +1 bei steigender, -1 bei fallender Flanke, sonst 0",
  "logic.changed": "(quelle) -- Boolean: Wert hat sich zum Vorsample geändert",
  "logic.debounce": "(triggerBool, n Samples) -- Boolean: erst nach n Treffern in Folge wahr",
  "logic.toggle": "(triggerBool) -- Boolean: kippt bei jedem Treffer",
  "logic.countWhere": "(quelle, prädikat) -- zählt Treffer-Samples seit Kurvenstart (monoton)",
  "logic.countSince": "(triggerBool) -- Samples seit letztem Treffer (0 beim Treffer)",
  "logic.countIn": "(triggerBool, n Samples) -- Treffer im rollierenden Fenster",
  "logic.rate": "(triggerBool, n Samples) -- Trefferanteil 0..1 im Fenster",
  "logic.threshold": "(quelle, schwelle) -- +1 bei Aufwärts-, -1 bei Abwärtskreuzung, sonst 0",
  "logic.crossover": "(a, b) -- +1 wenn a über b kreuzt, -1 darunter, sonst 0",
  "logic.delayBy1": "(quelle, startwert) -- um 1 Sample verzögert",
  "logic.delayByN": "(quelle, n Samples, startwert) -- um n Samples verzögert; erste n Werte = startwert",
  "dsp.lowPass1": "(quelle, grenzfrequenz, sampleRate) -- 1-Pol-Tiefpass",
  "dsp.highPass1": "(quelle, grenzfrequenz, sampleRate) -- 1-Pol-Hochpass",
  "dsp.biquadLowPass": "(quelle, freq, guete q ~0.7, sampleRate) -- resonanter Tiefpass",
  "dsp.biquadHighPass": "(quelle, freq, q, sampleRate) -- resonanter Hochpass",
  "dsp.biquadBandPass": "(quelle, mittenfreq, q, sampleRate) -- Bandpass",
  "dsp.dcBlock": "(quelle, r ~0.995) -- entfernt Gleichanteil",
  "dsp.envFollow": "(quelle, sampleRate, attackMs, releaseMs) -- Hüllkurvenfolger",
  "dsp.adsr": "(gateBool, attack, decay, sustain 0..1, release; Zeiten in Samples) -- Hüllkurve 0..1",
  "dsp.softClip": "(quelle, drive) -- weiche Sättigung (tanh)",
  "dsp.hardClip": "(quelle, limit) -- hartes Begrenzen auf +/-limit",
  "dsp.gate": "(quelle, hüllkurve, schwelle) -- 0 solange hüllkurve unter schwelle",
  "dsp.ringMod": "(a, b) -- multipliziert zwei Kurven",
  "dsp.bitCrush": "(quelle, bits) -- quantisiert auf 2^bits Stufen",
  "dsp.crossfade": "(a, b, mix 0..1) -- lineare Überblendung",
  "dsp.rms": "(quelle, n Samples) -- RMS-Pegel im Fenster",
  "dsp.zeroCrossRate": "(quelle, n Samples) -- Nulldurchgangsrate 0..1 im Fenster",
  "dsp.peakHold": "(quelle, abklingfaktor ~0.995 pro Sample) -- obere Hüllkurve",
};

// Library-Referenz aus dem Prelude: alle Namespace-Bausteine mit Signatur.
function buildReference(prelude: PreludeEntry[]): string {
  const hints = { ...BLOCK_HINTS, ...hostConfig.prompt?.extraBlockHints };
  const lines: string[] = [];
  for (const p of prelude) {
    const m = /^__([a-z]+)_(\w+)$/.exec(p.name);
    if (!m) continue;
    const key = `${m[1]}.${m[2]}`;
    lines.push(hints[key] ? `${key}${hints[key]}` : `${key} : ${p.typ}`);
  }
  lines.sort();
  return lines.join("\n");
}

const DEFAULT_BINDING_EXAMPLE = `  glatt = math.ema(quelle, 0.2)
  steigung = math.diff(glatt, 0)
  steigung`;

const DEFAULT_COMMENT_EXAMPLE = `  // 240 Samples Verschiebung nach hinten
  lag = logic.delayByN(quelle, 240, 0)`;

export async function buildSystemPrompt(): Promise<ChatMessage> {
  const prelude = await fetchPrelude().catch(() => [] as PreludeEntry[]);
  const reference = buildReference(prelude);
  const p = hostConfig.prompt ?? {};
  const purpose = p.purpose ?? "berechnete Kurven";
  const domainRules = (p.domainRules ?? []).map((r) => `- ${r}`).join("\n");
  const bindingExample = p.bindingExample ?? DEFAULT_BINDING_EXAMPLE;
  const methodCallExample = p.methodCallExample ?? "quelle.ema(0.2) == math.ema(quelle, 0.2)";
  const commentExample = p.commentExample ?? DEFAULT_COMMENT_EXAMPLE;
  const content = `Du bist ein Formel-Assistent für TypeFighter, eine kleine Sprache für ${purpose}.

Deine EINZIGE Aufgabe: TypeFighter-Formeln entwerfen, korrigieren und kurz erklären. Du hast keinerlei Werkzeuge -- kein Dateisystem, kein Netzwerk, keine Codeausführung. Du gibst ausschließlich Text aus. Fragen ohne Bezug zu Formeln lehnst du in einem Satz freundlich ab.

DIE SPRACHE:
- Ein Programm besteht aus Zeilen der Form \`name = ausdruck\`; die LETZTE Zeile ist der Ergebnis-Ausdruck. Keine Semikolons. Kommentare mit //.
- Das Ergebnis ist EIN Zahlenwert pro Sample-Zeitpunkt; daraus entsteht die Kurve.
- Operatoren: + - * / == != < <= > >= && || !, Klammern, if bedingung then a else b, Lambdas: x => x > 50
- Arithmetik funktioniert DIREKT zwischen Quellwerten, Konstanten und Baustein-Ergebnissen, pro Sample ausgewertet. Kein Zip oder Map nötig.
${domainRules ? domainRules + "\n" : ""}- Mehrere Bindungen sind erlaubt und üblich; die letzte Zeile referenziert das Ergebnis:
${bindingExample}
- Stream-Bausteine liegen in Namespaces (math.*, gen.*, logic.*, dsp.*) und nehmen die Quelle als ERSTES Argument. Als Quelle geht alles: Quellwert, Konstante, Ergebnis eines anderen Bausteins.
- Methoden-Schreibweise: quelle.baustein(restliche argumente) für JEDEN Baustein, z.B. ${methodCallExample}.
- Lambdas (x => ...) bekommen den aktuellen Zahlenwert pro Sample und dürfen äußere Bindungen nutzen: schwelle = 50 dann x => x > schwelle.
- Boolean-Ergebnisse (changed, debounce, toggle) fürs Chart in Zahlen wandeln: if b then 1 else 0.
- Fenster-Bausteine liefern am Anfang Werte über das noch unvolle Fenster -- das ist okay und muss nicht behandelt werden.

VERFÜGBARE BAUSTEINE:
${reference}

ANTWORTFORMAT (strikt):
- Antworte kurz auf Deutsch (2-4 Sätze Erklärung).
- Die Formel steht IMMER in genau EINEM \`\`\`-Codeblock. Die öffnenden \`\`\` stehen ALLEIN auf einer Zeile, die Formel beginnt auf der nächsten Zeile.
- Kommentiere die Formel mit kurzen //-Kommentaren: pro logischem Schritt eine Kommentarzeile, die das WAS und WARUM nennt (inkl. Einheiten- und Fenster-Mathematik). Keine Romane, keine Kommentar-Wiederholung des Offensichtlichen.
- Kommentare stehen IMMER auf einer EIGENEN Zeile DIREKT ÜBER der Code-Zeile, die sie erklären -- NIEMALS hinten an eine Code-Zeile angehängt. Beispiel:
${commentExample}
- Wenn dir ein Compilerfehler gemeldet wird, korrigiere die Formel und antworte erneut mit genau einem Codeblock.`;
  return { role: "system", content };
}

// Eine Assistenten-Runde: Modell fragen, Formel extrahieren, compile-prüfen,
// Fehler bis zu 2x automatisch zurückmelden.
export async function runAssistantTurn(
  history: ChatMessage[],
  onStatus: (s: string) => void,
  signal?: AbortSignal,
): Promise<{ turn: AssistantTurn; history: ChatMessage[] }> {
  const messages = [...history];
  const trace: TraceStep[] = [];
  let display = "";
  let formula: string | null = null;
  let error: string | null = null;

  for (let attempt = 0; attempt < 3; attempt++) {
    const v = attempt === 0 ? "" : ` (Versuch ${attempt + 1})`;
    onStatus(attempt === 0 ? "Modell denkt nach..." : `Korrigiere${v}...`);
    if (signal?.aborted) throw new DOMException("abgebrochen", "AbortError");
    const raw = await llmChat(messages, signal);
    if (signal?.aborted) throw new DOMException("abgebrochen", "AbortError");
    const { clean, thinking } = splitThinking(raw);
    if (thinking) trace.push({ label: `Denkprozess${v}`, body: thinking });
    messages.push({ role: "assistant", content: clean });
    // Nie stumm bleiben: reine <think>-Antworten (Token-Limit mitten im
    // Denken) wuerden sonst als unsichtbare leere Bubble enden.
    display = clean || "(Das Modell hat keinen Antworttext geliefert - siehe Denkprozess.)";
    formula = extractFormula(clean);
    if (!formula) { error = null; break; }
    trace.push({ label: `Formel-Vorschlag${v}`, body: formula });

    onStatus("Prüfe Formel...");
    const info = await fetchCompileInfo(formula).catch(() => null);
    if (!info || !info.error) {
      trace.push({ label: `Compile-Check${v}`, body: info ? `OK -- Ergebnistyp: ${info.resultType || "?"}` : "Compile-Dienst nicht erreichbar (übersprungen)" });
      error = null;
      break;
    }
    error = info.error;
    trace.push({ label: `Compile-Check${v}`, body: `FEHLER:\n${info.error}` });
    messages.push({
      role: "user",
      content: `Compilerfehler:\n${info.error}\n\nBitte korrigiere die Formel. Antworte nur kurz und mit genau einem Codeblock.`,
    });
  }

  return { turn: { display, formula, error, trace }, history: messages };
}
