// Where the blocks of a dataflow graph go and how the wires between them run.
// Pure geometry: no DOM, no React. The layering and the obstacle-avoiding edge
// routing follow the workflow canvas of the Repo Cockpit; what is new here is
// that arguments keep their order - a wire meets its target at the port that
// belongs to it, so `a - b` cannot read as `b - a`.

import type { GraphEdge, GraphGroup, GraphNode } from "./api";

export type Box = { x: number; y: number; width: number; height: number };
export type Point = { x: number; y: number };

export type LayoutMode = "across" | "down";

// A block that says a word is drawn wide enough to read it; one that says a
// symbol is drawn as a token. Both sit in the same band, so a row stays a row.
const BAND_W = 168;
const BAND_H = 92;
const GAP_X = 88;
const GAP_Y = 40;
const TOKEN = 58;
// A choice needs more room than a token: the wires meet the TIPS of a diamond,
// so its corners have to reach past the words that hang off them.
const DIAMOND = 82;
const HEAD = 8;

const COMPACT = new Set(["operator", "param"]);

// How wide a value is drawn: as wide as it reads, between a minimum that keeps
// short ones from looking pinched and a maximum that keeps a long string from
// pushing the whole picture apart. Past that the block cuts it and the pointer
// gets the rest.
const LITERAL_H = 46;
const LITERAL_MIN = 60;
const LITERAL_MAX = BAND_W;
// Schriftbreite bei 0.85rem Monospace, plus Polsterung und Rand der Pille.
const PER_CHAR = 8.4;
const LITERAL_PAD = 34;

export const isCompact = (node: GraphNode) => COMPACT.has(node.kind);
export const isDecision = (node: GraphNode) => node.kind === "decision";
export const isLiteral = (node: GraphNode) => node.kind === "literal";

const literalWidth = (node: GraphNode) =>
  Math.max(
    LITERAL_MIN,
    Math.min(LITERAL_MAX, Math.round(node.label.length * PER_CHAR) + LITERAL_PAD)
  );

export type Placed = {
  node: GraphNode;
  x: number;
  y: number;
};

export const boxOf = (entry: Placed): Box => {
  if (isLiteral(entry.node)) {
    const width = literalWidth(entry.node);
    return {
      x: entry.x + (BAND_W - width) / 2,
      y: entry.y + (BAND_H - LITERAL_H) / 2,
      width,
      height: LITERAL_H,
    };
  }

  // A token and a diamond are both square and both centred in the band, so a row
  // reads as a row whatever shape stands in it.
  const side = isDecision(entry.node) ? DIAMOND : isCompact(entry.node) ? TOKEN : 0;

  return side
    ? {
        x: entry.x + (BAND_W - side) / 2,
        y: entry.y + (BAND_H - side) / 2,
        width: side,
        height: side,
      }
    : { x: entry.x, y: entry.y, width: BAND_W, height: BAND_H };
};

// An expression graph is acyclic by construction - there is no recursion in the
// language - so a node's layer is simply the longest path from any source.
function layersOf(nodes: GraphNode[], edges: GraphEdge[]): GraphNode[][] {
  const depth = new Map<string, number>();
  for (const node of nodes) depth.set(node.id, 0);

  for (let round = 0; round < nodes.length; round++) {
    let moved = false;
    for (const edge of edges) {
      const candidate = (depth.get(edge.fromId) ?? 0) + 1;
      if (candidate > (depth.get(edge.toId) ?? 0) && candidate < nodes.length) {
        depth.set(edge.toId, candidate);
        moved = true;
      }
    }
    if (!moved) break;
  }

  const rows = new Map<number, GraphNode[]>();
  for (const node of nodes) {
    const level = depth.get(node.id) ?? 0;
    rows.set(level, [...(rows.get(level) ?? []), node]);
  }

  // Blocks of one lambda stand together in their row, or the area drawn around
  // them would reach across everything that happens to share the row.
  return [...rows.entries()]
    .sort((a, b) => a[0] - b[0])
    .map(([, row]) => [...row].sort((a, b) => a.group.localeCompare(b.group)));
}

// Where in its layer a node sits. Free blocks fill the first band, the blocks of
// a lambda the one after it - so the area drawn around a lambda can never reach
// over a block that does not belong to it.
function slots(layers: GraphNode[][]) {
  const free = layers.map((layer) => layer.filter((node) => !node.group));
  const held = layers.map((layer) => layer.filter((node) => node.group));

  const freeRows = Math.max(...free.map((row) => row.length), 0);
  const heldRows = Math.max(...held.map((row) => row.length), 0);
  const rows = Math.max(freeRows + heldRows, 1);

  return layers.map((_, level) => {
    const ownFree = free[level];
    const ownHeld = held[level];

    // Each band is centred in itself, so a short layer still reads as a column.
    const freeAt = (freeRows - ownFree.length) / 2;
    const heldAt = freeRows + (heldRows - ownHeld.length) / 2;

    return {
      rows,
      placed: [
        ...ownFree.map((node, index) => ({ node, row: freeAt + index })),
        ...ownHeld.map((node, index) => ({ node, row: heldAt + index })),
      ],
    };
  });
}

function place(layers: GraphNode[][], mode: LayoutMode): Placed[] {
  const bands = slots(layers);

  if (mode === "across") {
    return bands.flatMap((band, level) =>
      band.placed.map(({ node, row }) => ({
        node,
        x: level * (BAND_W + GAP_X),
        y: row * (BAND_H + GAP_Y),
      }))
    );
  }

  return bands.flatMap((band, level) =>
    band.placed.map(({ node, row }) => ({
      node,
      x: row * (BAND_W + GAP_X),
      y: level * (BAND_H + GAP_Y),
    }))
  );
}

type Line = {
  path: string;
  mid: Point;
  points: [Point, Point, Point, Point];
};

const curve = (
  x1: number,
  y1: number,
  ax: number,
  ay: number,
  bx: number,
  by: number,
  x2: number,
  y2: number
): Line => ({
  path: `M ${x1} ${y1} C ${ax} ${ay}, ${bx} ${by}, ${x2} ${y2}`,
  mid: {
    x: (x1 + 3 * ax + 3 * bx + x2) / 8,
    y: (y1 + 3 * ay + 3 * by + y2) / 8,
  },
  points: [
    { x: x1, y: y1 },
    { x: ax, y: ay },
    { x: bx, y: by },
    { x: x2, y: y2 },
  ],
});

const along = (points: Line["points"], t: number): Point => {
  const u = 1 - t;
  const a = u * u * u;
  const b = 3 * u * u * t;
  const c = 3 * u * t * t;
  const d = t * t * t;
  return {
    x: a * points[0].x + b * points[1].x + c * points[2].x + d * points[3].x,
    y: a * points[0].y + b * points[1].y + c * points[2].y + d * points[3].y,
  };
};

// A little air around every block a wire is not allowed to touch.
const AIR = 10;
const SAMPLES = 24;

const grown = (box: Box): Box => ({
  x: box.x - AIR,
  y: box.y - AIR,
  width: box.width + AIR * 2,
  height: box.height + AIR * 2,
});

// How badly a wire runs through what it must not: walked along the curve, the
// depth inside each block, summed. Zero is the only good answer - the rest only
// ranks how bad it got when nothing was free.
function intrusion(line: Line, obstacles: Box[]): number {
  const walls = obstacles.map(grown);
  let cost = 0;

  for (let step = 1; step < SAMPLES; step++) {
    const point = along(line.points, step / SAMPLES);
    for (const wall of walls) {
      const dx = Math.min(point.x - wall.x, wall.x + wall.width - point.x);
      const dy = Math.min(point.y - wall.y, wall.y + wall.height - point.y);
      if (dx > 0 && dy > 0) cost += Math.min(dx, dy);
    }
  }

  return cost;
}

// The first candidate that misses everything wins; when none does, the least bad
// one. Deterministic, so the same source always draws the same picture.
function routed(candidates: Line[], obstacles: Box[]): Line {
  let best = candidates[0];
  let least = Infinity;

  for (const line of candidates) {
    const cost = intrusion(line, obstacles);
    if (cost === 0) return line;
    if (cost < least) {
      least = cost;
      best = line;
    }
  }

  return best;
}

// Where a wire meets its target. Arguments keep their order: the first one
// arrives at the top of the incoming side, the last at the bottom. Position
// carries the order, so no wire has to be labelled to say which argument it is.
function portPoint(box: Box, index: number, count: number, mode: LayoutMode): Point {
  const share = (index + 1) / (count + 1);
  return mode === "across"
    ? { x: box.x, y: box.y + box.height * share }
    : { x: box.x + box.width * share, y: box.y };
}

const outPoint = (box: Box, mode: LayoutMode): Point =>
  mode === "across"
    ? { x: box.x + box.width, y: box.y + box.height / 2 }
    : { x: box.x + box.width / 2, y: box.y + box.height };

const RING = 84;

function wireBetween(
  from: Box,
  to: Box,
  index: number,
  count: number,
  mode: LayoutMode,
  obstacles: Box[]
): Line {
  const start = outPoint(from, mode);
  const end = portPoint(to, index, count, mode);

  const bow = (off: number) => {
    if (mode === "across") {
      const bend = Math.max(34, Math.abs(end.x - start.x) * 0.5);
      return curve(
        start.x,
        start.y,
        start.x + bend,
        start.y + off,
        end.x - bend - HEAD / 2,
        end.y + off,
        end.x - HEAD / 2,
        end.y
      );
    }
    const bend = Math.max(34, Math.abs(end.y - start.y) * 0.5);
    return curve(
      start.x,
      start.y,
      start.x + off,
      start.y + bend,
      end.x + off,
      end.y - bend - HEAD / 2,
      end.x,
      end.y - HEAD / 2
    );
  };

  return routed([0, RING, -RING, RING * 2, -RING * 2].map(bow), obstacles);
}

export type LaidOutEdge = {
  edge: GraphEdge;
  path: string;
  mid: Point;
};

export type LaidOutGroup = {
  group: GraphGroup;
  box: Box;
};

export type Layout = {
  placed: Placed[];
  edges: LaidOutEdge[];
  groups: LaidOutGroup[];
  width: number;
  height: number;
};

// How much air a lambda's area keeps around the blocks inside it.
const GROUP_AIR = 20;

export function layoutGraph(
  nodes: GraphNode[],
  edges: GraphEdge[],
  groups: GraphGroup[],
  mode: LayoutMode
): Layout {
  if (!nodes.length) {
    return { placed: [], edges: [], groups: [], width: 0, height: 0 };
  }

  const placed = place(layersOf(nodes, edges), mode);
  const at = new Map(placed.map((entry) => [entry.node.id, entry]));

  // Arguments of one target, in the order the source writes them.
  const incoming = new Map<string, GraphEdge[]>();
  for (const edge of edges) {
    incoming.set(edge.toId, [...(incoming.get(edge.toId) ?? []), edge]);
  }

  const laidOut: LaidOutEdge[] = [];

  for (const [toId, group] of incoming) {
    const target = at.get(toId);
    if (!target) continue;

    group.forEach((edge, index) => {
      const source = at.get(edge.fromId);
      if (!source) return;

      const obstacles = placed
        .filter(
          (entry) => entry.node.id !== edge.fromId && entry.node.id !== toId
        )
        .map(boxOf);

      const line = wireBetween(
        boxOf(source),
        boxOf(target),
        index,
        group.length,
        mode,
        obstacles
      );

      laidOut.push({ edge, path: line.path, mid: line.mid });
    });
  }

  const boxes = placed.map(boxOf);
  const width = Math.max(...boxes.map((box) => box.x + box.width));
  const height = Math.max(...boxes.map((box) => box.y + box.height));

  // A lambda is drawn as the area its blocks take up. Innermost first, so a
  // nested lambda's area is painted over the one that holds it.
  const laidOutGroups = groups
    .map((group) => {
      const own = placed
        .filter((entry) => entry.node.group === group.id)
        .map(boxOf);
      if (!own.length) return null;

      const x = Math.min(...own.map((box) => box.x)) - GROUP_AIR;
      const y = Math.min(...own.map((box) => box.y)) - GROUP_AIR;
      const right = Math.max(...own.map((box) => box.x + box.width)) + GROUP_AIR;
      const bottom =
        Math.max(...own.map((box) => box.y + box.height)) + GROUP_AIR;

      return {
        group,
        box: { x, y, width: right - x, height: bottom - y },
      };
    })
    .filter((entry): entry is LaidOutGroup => entry !== null);

  return {
    placed,
    edges: laidOut,
    groups: laidOutGroups,
    width: width + GROUP_AIR,
    height: height + GROUP_AIR,
  };
}
