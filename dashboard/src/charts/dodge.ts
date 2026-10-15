// Beeswarm layout (spec_country.md C2; user edit 1b): the same greedy placement as Plot's dodge, but
// computed here so the chart's height always matches its stack, and with a column-stack fallback when
// too many dots share a value. Pure and deterministic.
//
//   dodgeLayout(px, r, padding, late): points in x order (ties by input index), each at the smallest
//   |y| (+ before - on a tie) where it overlaps no placed dot; returns each dot's y offset from the
//   centre line and the extent (max |y|). `late` dots (the selected and labelled ones) are placed after
//   all others, so they land on the edge of their cluster where a label has room.
//
//   stackLayout(px, r, padding, k, priority): snaps dots to pixel columns (one dot diameter + padding
//   wide), stacks at most k per column around the centre line, and reports the dots it hid (+m). The
//   priority dots are kept first and take the bottom of their column (the "+m" count sits on top).
//
//   swarmFit(px, opts): the whole policy: dodge at r 4, 3, 2.5, 2 until the swarm fits the height
//   limit; else stack mode at the smallest radius.

export interface DodgeResult {
  /** y offset of each dot from the centre line (px; positive = up), by input index */
  y: number[];
  /** max |y| over the dots (0 for none) */
  extent: number;
}

const EPS = 1e-7;

/** The smallest-|y| free offset for a dot at x among `near` placed dots (+ before - on a tie). */
function freeY(xi: number, near: readonly number[], px: readonly number[], y: readonly number[], d: number): number {
  if (!near.length) return 0;
  // forbidden open intervals (yj - h, yj + h) and the candidates at their ends
  const iv: [number, number][] = [];
  const cand: number[] = [0];
  for (const j of near) {
    const dx = xi - px[j];
    const h = Math.sqrt(Math.max(0, d * d - dx * dx));
    iv.push([y[j] - h, y[j] + h]);
    cand.push(y[j] + h, y[j] - h);
  }
  cand.sort((a, b) => Math.abs(a) - Math.abs(b) || b - a);
  for (const c of cand) if (iv.every(([a, b]) => c <= a + EPS || c >= b - EPS)) return c;
  return cand[cand.length - 1];
}

/**
 * Greedy beeswarm placement of dots of radius r at pixel positions px (input order kept in the output).
 * Dots flagged `late` are placed after all the others (in x order), so they sit on the edge of a cluster.
 */
export function dodgeLayout(px: readonly number[], r: number, padding = 1, late?: readonly boolean[]): DodgeResult {
  const n = px.length;
  const y = new Array<number>(n).fill(0);
  const d = 2 * r + padding;
  const byX = (a: number, b: number) => px[a] - px[b] || a - b;
  const all = px.map((_, i) => i);
  const first = (late ? all.filter((i) => !late[i]) : all).sort(byX);
  const last = late ? all.filter((i) => late[i]).sort(byX) : [];
  const placed: number[] = []; // the first pass, in x order
  let extent = 0;
  for (const i of first) {
    const xi = px[i];
    // neighbours: placed dots closer than one diameter in x (placement is in x order, so walk back)
    const near: number[] = [];
    for (let k = placed.length - 1; k >= 0; k--) {
      const j = placed[k];
      if (xi - px[j] >= d) break;
      near.push(j);
    }
    y[i] = freeY(xi, near, px, y, d);
    if (Math.abs(y[i]) > extent) extent = Math.abs(y[i]);
    placed.push(i);
  }
  // the late dots: every placed dot within one diameter in x is a neighbour
  const done = [...placed];
  for (const i of last) {
    const xi = px[i];
    const near = done.filter((j) => Math.abs(xi - px[j]) < d);
    y[i] = freeY(xi, near, px, y, d);
    if (Math.abs(y[i]) > extent) extent = Math.abs(y[i]);
    done.push(i);
  }
  return { y, extent };
}

export interface StackResult {
  /** x of each dot after snapping to its column (px), by input index; NaN when hidden */
  x: number[];
  /** y offset from the centre line, by input index; NaN when hidden */
  y: number[];
  /** max |y| over the shown dots */
  extent: number;
  /** columns that hide dots: the column's x, its top (max y of the shown dots) and the count hidden */
  overflow: { x: number; top: number; hidden: number }[];
  /** count of hidden dots */
  hidden: number;
}

/**
 * Column stacks: dots snap to columns one diameter + padding wide; each column stacks at most k dots
 * around the centre line (0, +d, -d, +2d, -2d, ...), the highest-priority dots first (lower `priority`
 * value first; then by x, then input order). The rest are hidden and counted.
 */
export function stackLayout(px: readonly number[], r: number, padding: number, k: number, priority?: readonly number[]): StackResult {
  const n = px.length;
  const d = 2 * r + padding;
  const x = new Array<number>(n).fill(NaN);
  const y = new Array<number>(n).fill(NaN);
  const cols = new Map<number, number[]>();
  for (let i = 0; i < n; i++) {
    const c = Math.round(px[i] / d);
    const list = cols.get(c);
    if (list) list.push(i);
    else cols.set(c, [i]);
  }
  const cap = Math.max(1, Math.floor(k));
  const overflow: StackResult['overflow'] = [];
  let extent = 0;
  let hidden = 0;
  for (const [c, list] of [...cols.entries()].sort((a, b) => a[0] - b[0])) {
    // kept: the first `cap` by priority; the slots alternate around the centre (0, +d, -d, +2d, ...)
    list.sort((a, b) => (priority ? priority[a] - priority[b] : 0) || px[a] - px[b] || a - b);
    const kept = list.slice(0, cap);
    const slots = kept.map((_, m) => (m === 0 ? 0 : (m % 2 === 1 ? 1 : -1) * Math.ceil(m / 2) * d));
    // the priority dots take the lowest slots (their labels go below; the "+m" count sits on top)
    const bottomUp = [...slots].sort((a, b) => a - b);
    const cx = c * d;
    let top = 0;
    kept.forEach((i, m) => {
      const off = priority ? bottomUp[m] : slots[m];
      x[i] = cx;
      y[i] = off;
      if (off > top) top = off;
      if (Math.abs(off) > extent) extent = Math.abs(off);
    });
    if (list.length > cap) {
      overflow.push({ x: cx, top, hidden: list.length - cap });
      hidden += list.length - cap;
    }
  }
  return { x, y, extent, overflow, hidden };
}

export interface SwarmFitOptions {
  /** preferred radii, largest first (default 4, 3, 2.5, 2) */
  radii?: readonly number[];
  padding?: number;
  /** the most vertical room the swarm may take, in px (centre line +- this / 2, dots included) */
  maxSwarm: number;
  /** lower = placed last in dodge mode (on a cluster's edge) and kept first in stack mode: the selected and labelled dots */
  priority?: readonly number[];
}

export interface SwarmFit {
  mode: 'dodge' | 'stack';
  r: number;
  /** pixel x of each dot (snapped to columns in stack mode); NaN when hidden */
  x: number[];
  /** y offset from the centre line (positive = up); NaN when hidden */
  y: number[];
  /** the vertical room the dots take: 2 x (extent + r) */
  span: number;
  overflow: StackResult['overflow'];
  hidden: number;
}

/** Dodge at decreasing radii until the swarm fits `maxSwarm`; else stack columns at the smallest radius. */
export function swarmFit(px: readonly number[], opts: SwarmFitOptions): SwarmFit {
  const radii = opts.radii?.length ? opts.radii : [4, 3, 2.5, 2];
  const padding = opts.padding ?? 1;
  // the priority dots (lower than the most common value) are placed last, on the edge of their cluster
  const pr = opts.priority;
  const worst = pr && pr.length ? Math.max(...pr) : 0;
  const late = pr && pr.some((p) => p < worst) ? pr.map((p) => p < worst) : undefined;
  for (const r of radii) {
    const { y, extent } = dodgeLayout(px, r, padding, late);
    const span = 2 * (extent + r);
    if (span <= opts.maxSwarm) return { mode: 'dodge', r, x: [...px], y, span, overflow: [], hidden: 0 };
  }
  const r = radii[radii.length - 1];
  const d = 2 * r + padding;
  // k dots alternate around the centre: the column spans 2 x ceil((k - 1) / 2) x d + 2r
  const half = Math.floor((opts.maxSwarm - 2 * r) / (2 * d));
  const k = Math.max(1, 2 * half + 1);
  const s = stackLayout(px, r, padding, k, opts.priority);
  return { mode: 'stack', r, x: s.x, y: s.y, span: 2 * (s.extent + r), overflow: s.overflow, hidden: s.hidden };
}
