// The source as a dataflow picture. Read-only on purpose: it shows what the
// compiler understood, it does not change it. Every block comes from a node of
// the solved AST, so the picture cannot say something the text does not.

import "./TypeFighterGraph.scss";

import { useEffect, useMemo, useRef, useState } from "react";
import type {
  GraphNode,
  GraphResponse,
  TypeFighterLanguageClient,
} from "./api";
import {
  boxOf,
  isCompact,
  isDecision,
  isLiteral,
  layoutGraph,
  type LayoutMode,
  type Placed,
} from "./graphLayout";

export type TypeFighterGraphProps = {
  source: string;
  languageClient: TypeFighterLanguageClient;
  className?: string;
  debounceMs?: number;
  mode?: LayoutMode;
  // Told which piece of the source a block stands for, so a host can highlight
  // it in the editor. -1/-1 for blocks the desugar left without a place.
  onSelect?: (range: { startIdx: number; endIdx: number } | null) => void;
};

const EMPTY: GraphResponse = {
  error: "",
  nodes: [],
  edges: [],
  groups: [],
  rootId: "",
};

// What is written under a block. The namespace of a call, the number of items of
// a record - whatever the block itself does not already say.
const footerOf = (node: GraphNode) => node.detail;

// A port with a name belongs on its wire - a record's field, a match's scrutinee.
// A bare number does not: the order the wires arrive in already says that.
const named = (port: string) => port !== "" && !/^\d+$/.test(port);

function NodeCard({
  entry,
  isRoot,
  dimmed,
  onEnter,
  onLeave,
  onPick,
}: {
  entry: Placed;
  isRoot: boolean;
  dimmed: boolean;
  onEnter: () => void;
  onLeave: () => void;
  onPick: () => void;
}) {
  const { node } = entry;
  const box = boxOf(entry);
  const decision = isDecision(node);
  const compact = isCompact(node) || isLiteral(node);
  const footer = footerOf(node);

  return (
    <div
      className={
        "tf-graph__slot" +
        (dimmed ? " tf-graph__slot--dimmed" : "") +
        (isRoot ? " tf-graph__slot--root" : "")
      }
      style={{
        left: box.x,
        top: box.y,
        width: box.width,
        height: box.height,
      }}
      onMouseEnter={onEnter}
      onMouseLeave={onLeave}
      onClick={onPick}
    >
      {node.binds && <span className="tf-graph__binds">{node.binds}</span>}

      {decision ? (
        <div
          className="tf-graph__node tf-graph__node--decision"
          title={node.typ ? `${node.label} : ${node.typ}` : node.label}
        >
          <svg
            className="tf-graph__diamond"
            viewBox="0 0 100 100"
            preserveAspectRatio="none"
          >
            <polygon points="50,3 97,50 50,97 3,50" vectorEffect="non-scaling-stroke" />
          </svg>
          <span className="tf-graph__label">{node.label}</span>
        </div>
      ) : (
        <div
          className={
            `tf-graph__node tf-graph__node--${node.kind}` +
            (compact ? " tf-graph__node--compact" : "") +
            (!compact && footer ? " tf-graph__node--footed" : "")
          }
          title={node.typ ? `${node.label} : ${node.typ}` : node.label}
        >
          <span className="tf-graph__label">{node.label}</span>
          {!compact && footer && (
            <span className="tf-graph__footer">{footer}</span>
          )}
        </div>
      )}
    </div>
  );
}

export function TypeFighterGraph({
  source,
  languageClient,
  className,
  debounceMs = 300,
  mode = "across",
  onSelect,
}: TypeFighterGraphProps) {
  const [graph, setGraph] = useState<GraphResponse>(EMPTY);
  const [pending, setPending] = useState(false);
  const [hovered, setHovered] = useState<string | null>(null);

  const surfaceRef = useRef<HTMLDivElement | null>(null);
  const worldRef = useRef<HTMLDivElement | null>(null);
  const viewport = useRef({ x: 0, y: 0, scale: 1 });
  // A fresh picture is fitted into the frame once; after that the reader's own
  // pan and zoom win, or every keystroke would yank the view back.
  const fitted = useRef("");

  useEffect(() => {
    if (!source.trim()) {
      setGraph(EMPTY);
      return;
    }

    let dropped = false;
    setPending(true);

    const timer = window.setTimeout(() => {
      languageClient
        .fetchGraph(source)
        .then((next) => {
          // Ein halb getippter Ausdruck parst nicht und liefert kein Bild. Das
          // letzte gueltige bleibt darum stehen - die Meldung legt sich darueber,
          // statt die Flaeche leerzuraeumen.
          if (!dropped)
            setGraph((before) =>
              next.error ? { ...before, error: next.error } : next
            );
        })
        .catch(() => {
          if (!dropped)
            setGraph((before) => ({
              ...before,
              error: "The graph is unavailable.",
            }));
        })
        .finally(() => {
          if (!dropped) setPending(false);
        });
    }, debounceMs);

    return () => {
      dropped = true;
      window.clearTimeout(timer);
    };
  }, [source, languageClient, debounceMs]);

  const layout = useMemo(
    () => layoutGraph(graph.nodes, graph.edges, graph.groups, mode),
    [graph, mode]
  );

  // What is lit when the pointer rests on a block: the block, and everything one
  // step away from it. That is the reading a dataflow picture is for - where a
  // value came from and where it goes.
  const lit = useMemo(() => {
    if (!hovered) return null;
    const near = new Set<string>([hovered]);
    for (const edge of graph.edges) {
      if (edge.fromId === hovered) near.add(edge.toId);
      if (edge.toId === hovered) near.add(edge.fromId);
    }
    return near;
  }, [hovered, graph.edges]);

  const apply = () => {
    const world = worldRef.current;
    if (!world) return;
    const { x, y, scale } = viewport.current;
    world.style.transform = `translate(${x}px, ${y}px) scale(${scale})`;
  };

  useEffect(() => {
    const surface = surfaceRef.current;
    if (!surface || !layout.width) return;

    const key = `${graph.nodes.map((n) => n.id).join(",")}|${mode}`;

    // A frame that has not been measured yet would fit the picture against
    // nothing and leave it at the smallest zoom, so the fit waits for a real
    // size and the observer brings it back when the frame finally has one.
    const fit = () => {
      const room = { width: surface.clientWidth, height: surface.clientHeight };
      if (room.width < 80 || room.height < 80) return;
      if (fitted.current === key) return;
      fitted.current = key;

      const padding = 56;
      const scale = Math.max(
        0.25,
        Math.min(
          1,
          (room.width - padding) / layout.width,
          (room.height - padding) / layout.height
        )
      );
      viewport.current = {
        scale,
        x: (room.width - layout.width * scale) / 2,
        y: (room.height - layout.height * scale) / 2,
      };
      apply();
    };

    fit();

    const observer = new ResizeObserver(fit);
    observer.observe(surface);
    return () => observer.disconnect();
  }, [layout, graph.nodes, mode]);

  useEffect(() => {
    const surface = surfaceRef.current;
    if (!surface) return;

    let dragging = false;
    let originX = 0;
    let originY = 0;

    const onMove = (event: PointerEvent) => {
      if (!dragging) return;
      viewport.current.x = event.clientX - originX;
      viewport.current.y = event.clientY - originY;
      apply();
    };

    const stop = () => {
      dragging = false;
      surface.style.cursor = "grab";
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", stop);
    };

    const onDown = (event: PointerEvent) => {
      if (event.button !== 0 && event.button !== 1) return;
      dragging = true;
      originX = event.clientX - viewport.current.x;
      originY = event.clientY - viewport.current.y;
      surface.style.cursor = "grabbing";
      window.addEventListener("pointermove", onMove);
      window.addEventListener("pointerup", stop);
      event.preventDefault();
    };

    const onWheel = (event: WheelEvent) => {
      event.preventDefault();
      const rect = surface.getBoundingClientRect();
      const pointerX = event.clientX - rect.left;
      const pointerY = event.clientY - rect.top;
      const { x, y, scale } = viewport.current;
      const next = Math.min(
        2,
        Math.max(0.2, scale * (event.deltaY < 0 ? 1.12 : 1 / 1.12))
      );
      viewport.current = {
        scale: next,
        x: pointerX - ((pointerX - x) / scale) * next,
        y: pointerY - ((pointerY - y) / scale) * next,
      };
      apply();
    };

    surface.addEventListener("pointerdown", onDown);
    surface.addEventListener("wheel", onWheel, { passive: false });

    return () => {
      surface.removeEventListener("pointerdown", onDown);
      surface.removeEventListener("wheel", onWheel);
      stop();
    };
  }, []);

  // Ein Fehler verdeckt das Bild absichtlich: was da noch steht, gehoert zu einer
  // Fassung des Textes, die es nicht mehr gibt.
  const notice = !source.trim()
    ? { kind: "empty" as const, text: "Nothing to draw yet." }
    : graph.error
      ? { kind: "error" as const, text: graph.error }
      : !graph.nodes.length && !pending
        ? { kind: "empty" as const, text: "Nothing to draw yet." }
        : null;

  return (
    <div className={"tf-graph" + (className ? " " + className : "")}>
      <div className="tf-graph__surface" ref={surfaceRef}>
        <div className="tf-graph__world" ref={worldRef}>
          <div
            className={
              "tf-graph__canvas" + (graph.error ? " tf-graph__canvas--stale" : "")
            }
            style={{ width: layout.width, height: layout.height }}
          >
            {layout.groups.map(({ group, box }) => (
              <div
                key={group.id}
                className="tf-graph__group"
                style={{
                  left: box.x,
                  top: box.y,
                  width: box.width,
                  height: box.height,
                }}
              >
                <span className="tf-graph__group-name">{group.label}</span>
              </div>
            ))}

            <svg
              className="tf-graph__wires"
              width={layout.width}
              height={layout.height}
            >
              <defs>
                <marker
                  id="tf-graph-head"
                  viewBox="0 0 10 10"
                  refX="5"
                  refY="5"
                  markerWidth="10"
                  markerHeight="10"
                  markerUnits="userSpaceOnUse"
                  orient="auto"
                >
                  <path d="M0 0 L10 5 L0 10 Z" className="tf-graph__head" />
                </marker>
              </defs>
              {layout.edges.map(({ edge, path }, index) => {
                const on =
                  lit !== null &&
                  (edge.fromId === hovered || edge.toId === hovered);
                return (
                  <path
                    key={`${edge.fromId}>${edge.toId}#${index}`}
                    d={path}
                    className={
                      "tf-graph__wire" + (on ? " tf-graph__wire--lit" : "")
                    }
                    markerEnd="url(#tf-graph-head)"
                  >
                    <title>{edge.typ}</title>
                  </path>
                );
              })}
            </svg>

            {layout.edges
              .filter(({ edge }) => named(edge.port))
              .map(({ edge, mid }, index) => (
                <span
                  key={`${edge.fromId}>${edge.toId}#p${index}`}
                  className="tf-graph__port"
                  style={{ left: mid.x, top: mid.y }}
                >
                  {edge.port}
                </span>
              ))}

            {layout.placed.map((entry) => (
              <NodeCard
                key={entry.node.id}
                entry={entry}
                isRoot={entry.node.id === graph.rootId}
                dimmed={lit !== null && !lit.has(entry.node.id)}
                onEnter={() => setHovered(entry.node.id)}
                onLeave={() => setHovered(null)}
                onPick={() =>
                  onSelect?.(
                    entry.node.startIdx < 0
                      ? null
                      : {
                          startIdx: entry.node.startIdx,
                          endIdx: entry.node.endIdx,
                        }
                  )
                }
              />
            ))}
          </div>
        </div>
      </div>

      {pending && (
        <div className="tf-graph__working" role="status">
          <span className="tf-graph__working-chip">
            <span className="tf-graph__spinner" aria-hidden="true" />
            Reading
          </span>
        </div>
      )}

      {notice && (
        <div
          className={`tf-graph__notice tf-graph__notice--${notice.kind}`}
          role={notice.kind === "error" ? "alert" : "status"}
        >
          {notice.text}
        </div>
      )}
    </div>
  );
}

export default TypeFighterGraph;
