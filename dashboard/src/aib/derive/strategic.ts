// The strategic layer (2026-10-20): Strategic Sectoral Compacts, their workstreams, the value-chain spine
// with each node's status per scenario, the country x node division of labour, the Strategic AIB Roles of
// countries, the compact-specific roles, the narratives and the bilateral theses, as the views read them
// from doc.strategic (contract.ts AibStrategic; R/dashboard/aib/aib_export.R aib_dash_strategic();
// docs/aib_methodology.md 16).
//
// A strategic compact is a persistent architecture: its id, steps and nodes are the same in every scenario;
// the scenario (Managed Interdependence by default, Decoupling as the stress test, Baseline and the partial
// disruption beside them) only changes each node's status, the portfolio, the missions and the partners.
// The compact candidates of the main document are its Workstreams (AIB-Cxx ids unchanged).
//
// Every reading here is the export's: no score is computed, no role or mission is re-derived. The division
// of labour's dots bin the export's own shares (DIVISION_BINS) and say so in their legend. Pure (no React,
// no IO); lookups go through an index built once per section (a WeakMap).

import { NEUTRAL_SERIES } from '../../theme/palette.ts';
import { isNum } from '../../lib/format.ts';
import {
  AIB_SPINE_STATUSES, AIB_STRATEGIC_ROLE_CODES, AIB_STRATEGIC_SCENARIO_IDS, type AibCompact, type AibCompactRole, type AibDoc, type AibNode, type AibRoleStatus,
  type AibStrategic, type AibStrategicCompact, type AibStrategicCompactScenario, type AibStrategicCountry, type AibStrategicRoleDef, type AibStrategicScenarioDef,
  type AibStrategicItems, type AibStrategicScenarioId, type AibStrategicSlots, type AibThesisTuple, type AibWorkstream,
} from '../contract.ts';
import { classInfo, type ClassMark } from './classes.ts';
import { fmtShare } from './format.ts';
import { STRATEGIC_FRAMING } from './scenarios.ts';

// ---- Index ---------------------------------------------------------------------------------------------

interface NodeHome {
  compact: AibStrategicCompact;
  /** the step's order and label */
  order: number;
  step: string;
  fn: string | null;
}

interface Index {
  compacts: Map<string, AibStrategicCompact>;
  workstreams: Map<string, AibWorkstream>;
  countries: Map<string, AibStrategicCountry>;
  roleDefs: Map<string, AibStrategicRoleDef>;
  scenarios: Map<string, AibStrategicScenarioDef>;
  rolesByCountry: Map<string, AibCompactRole[]>;
  rolesByCompact: Map<string, AibCompactRole[]>;
  roleByPair: Map<string, AibCompactRole>;
  nodeHome: Map<string, NodeHome>;
  theses: Map<string, AibThesisTuple>;
  thesesByCountry: Map<string, AibThesisTuple[]>;
}

const cache = new WeakMap<AibStrategic, Index>();
const pairKey = (a: string, b: string): string => (a < b ? `${a}|${b}` : `${b}|${a}`);
const byRelevance = (a: AibCompactRole, b: AibCompactRole): number =>
  (b.relevance ?? -1) - (a.relevance ?? -1) || Number(b.allied) - Number(a.allied) || a.iso3.localeCompare(b.iso3) || a.compact.localeCompare(b.compact);

function indexOf(s: AibStrategic): Index {
  let ix = cache.get(s);
  if (ix) return ix;
  const push = <K, V>(m: Map<K, V[]>, k: K, v: V) => {
    const l = m.get(k);
    if (l) l.push(v);
    else m.set(k, [v]);
  };
  const rolesByCountry = new Map<string, AibCompactRole[]>();
  const rolesByCompact = new Map<string, AibCompactRole[]>();
  const roleByPair = new Map<string, AibCompactRole>();
  for (const r of s.compactRoles) {
    push(rolesByCountry, r.iso3, r);
    push(rolesByCompact, r.compact, r);
    roleByPair.set(`${r.compact}|${r.iso3}`, r);
  }
  for (const l of rolesByCountry.values()) l.sort(byRelevance);
  for (const l of rolesByCompact.values()) l.sort(byRelevance);
  const nodeHome = new Map<string, NodeHome>();
  for (const c of s.compacts) {
    for (const st of c.steps) st.nodes.forEach((n, i) => nodeHome.set(n, { compact: c, order: st.order, step: st.label, fn: st.functions[i] ?? null }));
  }
  const theses = new Map<string, AibThesisTuple>();
  const thesesByCountry = new Map<string, AibThesisTuple[]>();
  for (const t of s.theses) {
    theses.set(pairKey(t[0], t[1]), t);
    push(thesesByCountry, t[0], t);
    if (t[1] !== t[0]) push(thesesByCountry, t[1], t);
  }
  ix = {
    compacts: new Map(s.compacts.map((c) => [c.id.toUpperCase(), c])),
    workstreams: new Map(s.workstreams.map((w) => [w.id.toUpperCase(), w])),
    countries: new Map(s.countries.map((c) => [c.iso3, c])),
    roleDefs: new Map(s.roleDefs.map((r) => [r.code, r])),
    scenarios: new Map(s.scenarios.map((x) => [x.id, x])),
    rolesByCountry,
    rolesByCompact,
    roleByPair,
    nodeHome,
    theses,
    thesesByCountry,
  };
  cache.set(s, ix);
  return ix;
}

// ---- Scenarios -------------------------------------------------------------------------------------------

/** The section's scenarios in display order (Baseline, Managed Interdependence, Partial supplier disruption, Decoupling). */
export function strategicScenarios(s: AibStrategic): AibStrategicScenarioDef[] {
  const ix = indexOf(s);
  return AIB_STRATEGIC_SCENARIO_IDS.flatMap((id) => {
    const d = ix.scenarios.get(id);
    return d ? [d] : [];
  });
}

/** One scenario's definition (null when the section does not list it). */
export const strategicScenarioDef = (s: AibStrategic, id: string | null | undefined): AibStrategicScenarioDef | null => (id ? (indexOf(s).scenarios.get(id) ?? null) : null);

/**
 * The scenario a strategic view reads: `choice` (the toggle, 'managed' or 'decoupling', or any listed id)
 * when the section lists it; else the section's default (defaultScenario: Managed Interdependence); else
 * the first listed.
 */
export function strategicScenarioFor(s: AibStrategic, choice?: string | null): AibStrategicScenarioId {
  const ix = indexOf(s);
  if (choice && ix.scenarios.has(choice)) return choice as AibStrategicScenarioId;
  if (ix.scenarios.has(s.defaultScenario)) return s.defaultScenario as AibStrategicScenarioId;
  return strategicScenarios(s)[0]?.id ?? 'managed';
}

/** The scenario has coalitions (critical partners and coalition members are arrays, not null). */
export const scenarioHasCoalitions = (s: AibStrategic, id: string): boolean => strategicScenarioDef(s, id)?.coalitions === true;
/** The scenario has an investment simulation (post-investment coverage, BUILD hosts). */
export const scenarioHasInvestment = (s: AibStrategic, id: string): boolean => strategicScenarioDef(s, id)?.investment === true;

/** "Strategic compact = persistent architecture; scenario = the stress applied to it." (the export's words, else the fallback). */
export const strategicFraming = (s: AibStrategic | null | undefined): string => s?.framing || STRATEGIC_FRAMING;

// ---- Compacts ------------------------------------------------------------------------------------------------

/** The strategic compacts in the export's order. */
export const strategicCompacts = (s: AibStrategic): AibStrategicCompact[] => s.compacts;

/** A strategic compact by id (case-insensitive), or null. */
export const strategicCompactOf = (s: AibStrategic, id: string | null | undefined): AibStrategicCompact | null => (id ? (indexOf(s).compacts.get(id.toUpperCase()) ?? null) : null);

/** A compact under one scenario (null when the export has no entry for it: never for a listed scenario). */
export const compactScenarioOf = (c: AibStrategicCompact, id: string): AibStrategicCompactScenario | null =>
  (c.scenarios as Record<string, AibStrategicCompactScenario | undefined>)[id] ?? null;

/** Every node of a compact, in spine order (steps by order, nodes as listed in each step). */
export const compactNodeIds = (c: AibStrategicCompact): string[] => c.steps.flatMap((st) => st.nodes);

/** Where a node sits in the strategic layer: its compact, step and function (null: a node of no strategic compact). */
export function nodeHomeOf(s: AibStrategic, nodeId: string | null | undefined): { compact: AibStrategicCompact; order: number; step: string; fn: string | null } | null {
  return nodeId ? (indexOf(s).nodeHome.get(nodeId) ?? null) : null;
}

/** "Rare-earth ore → Separated oxides → ... → Motors & generators": the compact's supply-chain span in words. */
export const spanWords = (c: AibStrategicCompact, sep = ' → '): string => c.steps.map((st) => st.label).join(sep);

/** The policy mix in words ("BUILD + COORDINATE"; "none" when the scenario needs no intervention, "no shock" under Baseline). */
export function policyMixWords(sc: AibStrategicCompactScenario | null): string {
  if (!sc) return '–';
  if (sc.policyMix.length) return sc.policyMix.join(' + ');
  return sc.counts.noShock > 0 ? 'no shock' : 'none';
}

/** The missions of a compact under a scenario, in slot order ([] when none). */
export const missionsOf = (sc: AibStrategicCompactScenario | null): AibStrategicCompactScenario['missions'] => sc?.missions ?? [];

// ---- The spine ------------------------------------------------------------------------------------------------

/** How a spine status is drawn and read: the five classes as their chips, NOT STRESSED and NO SHOCK as a dashed outline, NOT TESTED hatched like INSUFFICIENT DATA. */
export interface SpineStatusInfo {
  status: string;
  label: string;
  color: string;
  mark: ClassMark | 'dashed';
  meaning: string;
  /** BUILD, COORDINATE or MANAGE SPECIALIZATION: an intervention class */
  intervention: boolean;
}

const NON_CLASS_STATUS: Readonly<Record<string, Omit<SpineStatusInfo, 'status'>>> = {
  'NOT STRESSED': {
    label: 'NOT STRESSED',
    color: NEUTRAL_SERIES,
    mark: 'dashed',
    meaning: 'The scenario removes or shifts no supply in this node: it keeps its suppliers as they are today.',
    intervention: false,
  },
  'NOT TESTED': {
    label: 'NOT TESTED',
    color: NEUTRAL_SERIES,
    mark: 'hatch',
    meaning:
      'Concentrated, but the scenario does not test it: Managed Interdependence brings a node to the concentration of its benchmark heading, and this node’s benchmark is as concentrated, so no supply shifts. See its Decoupling status.',
    intervention: false,
  },
  'NO SHOCK': {
    label: 'NO SHOCK',
    color: NEUTRAL_SERIES,
    mark: 'dashed',
    meaning: 'Baseline: trade as it is today, before any shock, so the node has no class.',
    intervention: false,
  },
};

const INTERVENTION = new Set(['BUILD', 'COORDINATE', 'MANAGE SPECIALIZATION']);

/** The colour, mark and words of a spine status (an unknown status reads as INSUFFICIENT DATA, never a new colour). */
export function spineStatusInfo(status: string | null | undefined): SpineStatusInfo {
  const st = status ?? 'NO SHOCK';
  const other = NON_CLASS_STATUS[st];
  if (other) return { status: st, ...other };
  const c = classInfo(st);
  return { status: st, label: c.label, color: c.color, mark: c.mark, meaning: c.meaning, intervention: INTERVENTION.has(st) };
}

/** The statuses in legend order (the five classes, then NOT STRESSED, NOT TESTED and NO SHOCK). */
export const SPINE_STATUS_ORDER: readonly string[] = AIB_SPINE_STATUSES;

export interface SpineNode {
  id: string;
  /** the node's label (its id when the document has no such node) */
  label: string;
  /** resource, processing, components or systems (null when the export has none) */
  fn: string | null;
  /** its status under the scenario (null when the compact has no entry for the scenario) */
  status: string | null;
  /** coordinated (optimized-trade) coverage of its allied demand under the scenario */
  coverage: number | null;
  /** the investment host under the scenario, named only when no other host ties with the model's rank 1 */
  host: string | null;
  /** how many hosts tie with the model's rank 1 (null: not simulated or an older bundle) */
  ties: number | null;
  /** the document's node (the existing node evidence), null when it has none */
  node: AibNode | null;
}

export interface SpineStep {
  order: number;
  label: string;
  description: string | null;
  nodes: SpineNode[];
}

/**
 * The value chain of a compact under one scenario: its steps in order, each node with its status, coverage
 * and rank-1 host under the scenario (`nodeOf`: the document's nodes, for labels and the node evidence).
 * The same steps and nodes in every scenario; only the statuses change.
 */
export function spineOf(c: AibStrategicCompact, scenario: string, nodeOf?: ((id: string) => AibNode | null) | null): SpineStep[] {
  const sc = compactScenarioOf(c, scenario);
  const byId = new Map((sc?.nodes ?? []).map((t) => [t[0], t]));
  return c.steps.map((st) => ({
    order: st.order,
    label: st.label,
    description: st.description,
    nodes: st.nodes.map((id, i) => {
      const t = byId.get(id);
      const node = nodeOf ? nodeOf(id) : null;
      return {
        id,
        label: node?.label ?? id,
        fn: st.functions[i] ?? null,
        status: t ? t[1] : null,
        coverage: t && isNum(t[2]) ? t[2] : null,
        host: t ? t[3] : null,
        ties: t && isNum(t[4]) ? t[4] : null,
        node,
      };
    }),
  }));
}

/** A node's status in every listed scenario (display order): what changes when the scenario does. */
export function nodeStatusAcross(
  s: AibStrategic,
  c: AibStrategicCompact,
  nodeId: string,
): { scenario: AibStrategicScenarioDef; status: string | null; coverage: number | null; host: string | null; ties: number | null }[] {
  return strategicScenarios(s).map((d) => {
    const t = compactScenarioOf(c, d.id)?.nodes.find((x) => x[0] === nodeId);
    return { scenario: d, status: t ? t[1] : null, coverage: t && isNum(t[2]) ? t[2] : null, host: t ? t[3] : null, ties: t && isNum(t[4]) ? t[4] : null };
  });
}

/** A node's new-capacity host in words: the named host, or the tie ("Carpania", "4 hosts tie"; null: none). */
export function hostTieWords(host: string | null, ties: number | null, name: (iso3: string) => string): string | null {
  if (host) return name(host);
  if (ties !== null && ties > 1) return `${ties} hosts tie`;
  return null;
}

/** The spine's counts by status in legend order, the empty ones left out. */
export function statusCounts(sc: AibStrategicCompactScenario | null): { status: string; n: number }[] {
  if (!sc) return [];
  const k = sc.counts;
  const n: Record<string, number> = {
    COORDINATE: k.coordinate,
    BUILD: k.build,
    'MANAGE SPECIALIZATION': k.manage,
    'LOW INTERVENTION': k.low,
    'INSUFFICIENT DATA': k.insufficient,
    'NOT STRESSED': k.notStressed,
    'NOT TESTED': k.notTested ?? 0,
    'NO SHOCK': k.noShock,
  };
  return SPINE_STATUS_ORDER.map((status) => ({ status, n: n[status] ?? 0 })).filter((x) => x.n > 0);
}

// ---- Workstreams ------------------------------------------------------------------------------------------------

export interface WorkstreamRow {
  id: string;
  ws: AibWorkstream;
  /** the compact candidate of the main document (its coalition, nodes and evidence), null when absent */
  candidate: AibCompact | null;
  /** the strategic compact it is assigned to (null: none) */
  home: string | null;
  /** active under the scenario asked for (`activeIn` of workstreamsOf) */
  active: boolean;
}

/** A workstream by id (case-insensitive), or null. */
export const workstreamOf = (s: AibStrategic, id: string | null | undefined): AibWorkstream | null => (id ? (indexOf(s).workstreams.get(id.toUpperCase()) ?? null) : null);

/** The strategic compact a workstream (a compact candidate, AIB-Cxx) is assigned to, or null. */
export function workstreamHomeOf(s: AibStrategic, id: string | null | undefined): AibStrategicCompact | null {
  const w = workstreamOf(s, id);
  return w?.compact ? strategicCompactOf(s, w.compact) : null;
}

function rowsOf(s: AibStrategic, ids: readonly string[], doc: Pick<AibDoc, 'compacts'> | null | undefined, activeIn: string | null | undefined): WorkstreamRow[] {
  const cand = new Map((doc?.compacts ?? []).map((c) => [c.id.toUpperCase(), c]));
  return ids.flatMap((id) => {
    const ws = workstreamOf(s, id);
    if (!ws) return [];
    return [{ id: ws.id, ws, candidate: cand.get(ws.id.toUpperCase()) ?? null, home: ws.compact, active: activeIn ? ws.active.includes(activeIn) : false }];
  });
}

/** The workstreams assigned to a compact, in the export's order (`activeIn`: a scenario whose activity to mark). */
export function workstreamsOf(s: AibStrategic, compactId: string, doc?: Pick<AibDoc, 'compacts'> | null, activeIn?: string | null): WorkstreamRow[] {
  const c = strategicCompactOf(s, compactId);
  return c ? rowsOf(s, c.workstreams, doc, activeIn) : [];
}

/** The workstreams assigned elsewhere that also span the compact. */
export function alsoTouchingOf(s: AibStrategic, compactId: string, doc?: Pick<AibDoc, 'compacts'> | null, activeIn?: string | null): WorkstreamRow[] {
  const c = strategicCompactOf(s, compactId);
  return c ? rowsOf(s, c.alsoTouching, doc, activeIn) : [];
}

/** The ids of a compact's workstreams active under a scenario (the export's activeWorkstreams). */
export const activeWorkstreamsOf = (c: AibStrategicCompact, scenario: string): string[] => compactScenarioOf(c, scenario)?.activeWorkstreams ?? [];

// ---- The division of labour -------------------------------------------------------------------------------------

/** What the dots of the division of labour show: supply capability, allied demand, or investment-host rank. */
export type DivisionMeasure = 'supply' | 'demand' | 'host';
export const DIVISION_MEASURES: readonly DivisionMeasure[] = ['supply', 'demand', 'host'];

export interface DivisionCell {
  iso3: string;
  node: string;
  /** share of world supply of the node (the larger of world export and production shares) */
  supply: number | null;
  /** share of the allied demand scope's imports of the node */
  demand: number | null;
  /**
   * the model's best rank as a host of new capacity (Managed Interdependence or Decoupling, the better), only
   * where the economy is a capable host tied with the model's best on a node that needs new capacity; within
   * such a tie the rank is the model's capability tie-break (null: not such a host)
   */
  hostRank: number | null;
  /** a anchor position, s specialist position, h capable host tied with the best on a node that needs capacity, m momentum */
  flags: { anchor: boolean; specialist: boolean; host: boolean; momentum: boolean };
}

/** The export's tuples as cells. */
export function divisionCells(c: AibStrategicCompact): DivisionCell[] {
  return c.division.map(([iso3, node, supply, demand, hostRank, flags]) => ({
    iso3,
    node,
    supply: isNum(supply) ? supply : null,
    demand: isNum(demand) ? demand : null,
    hostRank: isNum(hostRank) ? hostRank : null,
    flags: { anchor: flags.includes('a'), specialist: flags.includes('s'), host: flags.includes('h'), momentum: flags.includes('m') },
  }));
}

/**
 * The bins of the dots. Shares (supply, demand): 1 dot from 2%, 2 from 10% (the role rules' anchor-position
 * and demand-node bar), 3 from 25%. Host rank: 3 dots for rank 1, 2 for ranks 2-3, 1 for any lower rank of a
 * credited host (every ranked host in the export ties with the model's best on gain).
 */
export const DIVISION_BINS = { share: [0.02, 0.1, 0.25] as const, hostRank: [5, 3, 1] as const };

export type DivisionLevel = 0 | 1 | 2 | 3;

/** A cell's intensity (0-3 dots) on one measure. */
export function divisionIntensity(cell: DivisionCell | null | undefined, measure: DivisionMeasure): DivisionLevel {
  if (!cell) return 0;
  if (measure === 'host') {
    const r = cell.hostRank;
    if (!isNum(r)) return 0;
    const [, two, three] = DIVISION_BINS.hostRank;
    return r <= three ? 3 : r <= two ? 2 : r >= 1 ? 1 : 0;
  }
  const v = measure === 'supply' ? cell.supply : cell.demand;
  if (!isNum(v)) return 0;
  const [one, two, three] = DIVISION_BINS.share;
  return v >= three ? 3 : v >= two ? 2 : v >= one ? 1 : 0;
}

/** The measure's value as printed ("18%", "rank 1", "–"). */
export function divisionValueText(cell: DivisionCell | null | undefined, measure: DivisionMeasure): string {
  if (!cell) return '–';
  if (measure === 'host') return isNum(cell.hostRank) ? `rank ${cell.hostRank}` : '–';
  return fmtShare(measure === 'supply' ? cell.supply : cell.demand, 0);
}

export interface DivisionLegend {
  measure: DivisionMeasure;
  /** what the dots are ("Share of world supply of the node") */
  title: string;
  levels: { n: 1 | 2 | 3; words: string }[];
  /** one sentence: what the matrix does and does not say */
  note: string;
}

const MEASURE_WORDS: Readonly<Record<DivisionMeasure, { title: string; short: string }>> = {
  supply: { title: 'Share of world supply of the node (exports or production, the larger)', short: 'Physical capability' },
  demand: { title: 'Share of the allied demand scope’s imports of the node', short: 'Allied demand' },
  host: {
    title:
      'Host of simulated new capacity on a node that needs it: a capable host tied with the model’s best on simulated gain (Managed Interdependence or Decoupling, the better rank; inside a tie the rank is the model’s capability tie-break)',
    short: 'Investment host',
  },
};

/** The short name of a measure ("Physical capability", "Allied demand", "Investment host"). */
export const divisionMeasureLabel = (m: DivisionMeasure): string => MEASURE_WORDS[m].short;

/** The legend of a measure: what one, two and three dots mean. */
export function divisionLegend(measure: DivisionMeasure): DivisionLegend {
  const pct = (x: number) => `${Math.round(x * 100)}%`;
  const [a, b, c] = DIVISION_BINS.share;
  const levels: DivisionLegend['levels'] =
    measure === 'host'
      ? [
          { n: 1, words: 'ranked 4th or lower' },
          { n: 2, words: 'ranked 2nd or 3rd' },
          { n: 3, words: 'the best-ranked host' },
        ]
      : [
          { n: 1, words: `${pct(a)} to ${pct(b)}` },
          { n: 2, words: `${pct(b)} to ${pct(c)}` },
          { n: 3, words: `${pct(c)} or more` },
        ];
  return {
    measure,
    title: MEASURE_WORDS[measure].title,
    levels,
    note: 'Who does what in the compact today, from the model’s evidence: no economy is expected to build every node.',
  };
}

export interface DivisionNode {
  id: string;
  label: string;
  step: string;
  order: number;
}

export interface DivisionRow {
  iso3: string;
  /** allied (demand scope), non-allied, or null when the section does not assess the economy */
  allied: boolean | null;
  /** its compact-specific role, when the export gives one */
  role: string | null;
  /** aligned with the matrix's nodes */
  cells: (DivisionCell | null)[];
  levels: DivisionLevel[];
  /** the sum of its levels (the row order's first key) */
  total: number;
}

export interface DivisionMatrix {
  compact: AibStrategicCompact;
  measure: DivisionMeasure;
  /** the compact's nodes in spine order */
  nodes: DivisionNode[];
  rows: DivisionRow[];
  /** economies with at least one dot left out by maxRows */
  hidden: number;
}

export interface DivisionOpts {
  measure?: DivisionMeasure;
  /** the document's nodes (labels) */
  nodeOf?: ((id: string) => AibNode | null) | null;
  /** at most this many rows (default 12); the rest are counted in `hidden` */
  maxRows?: number;
  /** exactly these economies, in this order (rows without a dot included) */
  countries?: readonly string[];
  /** allied economies only */
  alliedOnly?: boolean;
}

/**
 * The country x node matrix of a compact on one measure. Rows: the economies with at least one dot (or
 * `countries`), the `maxRows` with the most dots kept, then ordered along the chain (by the step of their
 * strongest cell, the most dots first within it) so the division of labour reads as a diagonal.
 */
export function divisionMatrix(s: AibStrategic, c: AibStrategicCompact, opts: DivisionOpts = {}): DivisionMatrix {
  const measure = opts.measure ?? 'supply';
  const ix = indexOf(s);
  const nodes: DivisionNode[] = c.steps.flatMap((st) => st.nodes.map((id) => ({ id, label: opts.nodeOf?.(id)?.label ?? id, step: st.label, order: st.order })));
  const col = new Map(nodes.map((n, i) => [n.id, i]));
  const byIso = new Map<string, (DivisionCell | null)[]>();
  for (const cell of divisionCells(c)) {
    const i = col.get(cell.node);
    if (i === undefined) continue;
    let r = byIso.get(cell.iso3);
    if (!r) byIso.set(cell.iso3, (r = nodes.map(() => null)));
    r[i] = cell;
  }
  const alliedOf = (iso: string): boolean | null => ix.countries.get(iso)?.allied ?? ix.roleByPair.get(`${c.id}|${iso}`)?.allied ?? null;
  const rowOf = (iso: string): DivisionRow => {
    const cells = byIso.get(iso) ?? nodes.map(() => null);
    const levels = cells.map((x) => divisionIntensity(x, measure));
    return { iso3: iso, allied: alliedOf(iso), role: ix.roleByPair.get(`${c.id}|${iso}`)?.role ?? null, cells, levels, total: levels.reduce<number>((a, b) => a + b, 0) };
  };
  if (opts.countries) return { compact: c, measure, nodes, rows: opts.countries.map(rowOf), hidden: 0 };
  let rows = [...byIso.keys()].map(rowOf).filter((r) => r.total > 0 && (!opts.alliedOnly || r.allied === true));
  const sumOf = (r: DivisionRow) => r.cells.reduce((a, x) => a + ((measure === 'supply' ? x?.supply : measure === 'demand' ? x?.demand : null) ?? 0), 0);
  rows.sort((a, b) => b.total - a.total || sumOf(b) - sumOf(a) || a.iso3.localeCompare(b.iso3));
  const max = opts.maxRows ?? 12;
  const hidden = Math.max(0, rows.length - max);
  rows = rows.slice(0, max);
  const lead = (r: DivisionRow) => {
    let best = 0;
    r.levels.forEach((l, i) => {
      if (l > r.levels[best]) best = i;
    });
    return best;
  };
  rows.sort((a, b) => lead(a) - lead(b) || b.total - a.total || sumOf(b) - sumOf(a) || a.iso3.localeCompare(b.iso3));
  return { compact: c, measure, nodes, rows, hidden };
}

// ---- Strategic AIB Roles ------------------------------------------------------------------------------------------

/** A role's definition by code, or null. */
export const roleDefOf = (s: AibStrategic, code: string): AibStrategicRoleDef | null => indexOf(s).roleDefs.get(code) ?? null;

/** A role's name ("Resource & Processing Anchor"; the code when the section has no definition). */
export const roleName = (s: AibStrategic, code: string): string => roleDefOf(s, code)?.name ?? code;

export interface RoleGate {
  /** the metric key (countries[].metrics) */
  metric: string;
  label: string;
  value: number | null;
  /** its percentile among the role's eligible economies (0-1) */
  pct: number | null;
}

/** One of a country's Strategic AIB Roles with its evidence: both gate values and percentiles. */
export interface StrategicRole {
  code: string;
  name: string;
  noun: string;
  /** primary, secondary, qualifies beyond the cap, or null (does not qualify) */
  status: AibRoleStatus | null;
  rank: number | null;
  /** the lower gate percentile: orders a country's own roles, never a score */
  strength: number | null;
  supplyRole: boolean;
  gates: RoleGate[];
}

export interface CountryStrategic {
  iso3: string;
  allied: boolean;
  majorSupplier: boolean;
  /** every assessed role, as the export orders them (qualifying first, by rank) */
  roles: StrategicRole[];
  primary: StrategicRole[];
  secondary: StrategicRole[];
  beyondCap: StrategicRole[];
  metrics: Record<string, number | null>;
  slots: AibStrategicSlots | null;
  /** the list slots' items as codes (null in a bundle written before 2026-10-21) */
  items: AibStrategicItems | null;
  narrative: string | null;
}

const num = (x: unknown): number | null => (isNum(x) ? x : null);

/** A country's Strategic AIB Roles, decoded (null when the section does not assess it). */
export function countryStrategicOf(s: AibStrategic, iso3: string | null | undefined): CountryStrategic | null {
  const c = iso3 ? indexOf(s).countries.get(iso3.toUpperCase()) : undefined;
  if (!c) return null;
  const roles: StrategicRole[] = c.roles.map(([code, status, rank, strength, g1, p1, g2, p2]) => {
    const d = roleDefOf(s, code);
    const gate = (i: 0 | 1, value: number | null, pct: number | null): RoleGate => ({ metric: d?.gates[i] ?? '', label: d?.gateLabels[i] ?? d?.gates[i] ?? '', value: num(value), pct: num(pct) });
    return {
      code,
      name: d?.name ?? code,
      noun: d?.noun ?? code,
      status: (status as AibRoleStatus | null) ?? null,
      rank: num(rank),
      strength: num(strength),
      supplyRole: d?.supplyRole ?? false,
      gates: [gate(0, g1, p1), gate(1, g2, p2)],
    };
  });
  const pick = (codes: readonly string[]) => codes.flatMap((k) => roles.filter((r) => r.code === k));
  const metrics = Array.isArray(c.metrics) || !c.metrics ? {} : c.metrics;
  return { iso3: c.iso3, allied: c.allied, majorSupplier: c.majorSupplier, roles, primary: pick(c.primary), secondary: pick(c.secondary), beyondCap: pick(c.beyondCap), metrics, slots: c.slots ?? null, items: c.items ?? null, narrative: c.narrative ?? null };
}

/** The assessed economies (allied ones first, then by code). */
export function strategicCountries(s: AibStrategic, opts: { alliedOnly?: boolean } = {}): AibStrategicCountry[] {
  return s.countries.filter((c) => !opts.alliedOnly || c.allied).sort((a, b) => Number(b.allied) - Number(a.allied) || a.iso3.localeCompare(b.iso3));
}

/** The economies that hold a role (with `status`: only at that status), the strongest first. */
export function roleHoldersOf(s: AibStrategic, code: string, status?: AibRoleStatus | AibRoleStatus[]): { iso3: string; role: StrategicRole }[] {
  const want = status === undefined ? null : new Set(Array.isArray(status) ? status : [status]);
  return s.countries
    .flatMap((c) => {
      const role = countryStrategicOf(s, c.iso3)?.roles.find((r) => r.code === code);
      return role && role.status && (!want || want.has(role.status)) ? [{ iso3: c.iso3, role }] : [];
    })
    .sort((a, b) => (b.role.strength ?? -1) - (a.role.strength ?? -1) || a.iso3.localeCompare(b.iso3));
}

/** The role codes in the export's order (RPA, STM, SMI, DMA, CIA, DSP, CBH). */
export const ROLE_CODES: readonly string[] = AIB_STRATEGIC_ROLE_CODES;

/** A country's narrative and its slots (null when the section has none for it). */
export function narrativeOf(s: AibStrategic, iso3: string | null | undefined): { narrative: string | null; slots: AibStrategicSlots | null } | null {
  const c = iso3 ? indexOf(s).countries.get(iso3.toUpperCase()) : undefined;
  if (!c || (!c.narrative && !c.slots)) return null;
  return { narrative: c.narrative ?? null, slots: c.slots ?? null };
}

// ---- Compact-specific roles ----------------------------------------------------------------------------------------

/** A country's roles in the strategic compacts, the most relevant first. */
export const compactRolesOfCountry = (s: AibStrategic, iso3: string | null | undefined): AibCompactRole[] => (iso3 ? (indexOf(s).rolesByCountry.get(iso3.toUpperCase()) ?? []) : []);

/** The countries' roles within one strategic compact, the most relevant first (allied before non-allied at equal relevance). */
export const compactRolesOfCompact = (s: AibStrategic, compactId: string | null | undefined): AibCompactRole[] => {
  const c = strategicCompactOf(s, compactId);
  return c ? (indexOf(s).rolesByCompact.get(c.id) ?? []) : [];
};

/** One country's role within one strategic compact, or null. */
export function compactRoleOf(s: AibStrategic, compactId: string | null | undefined, iso3: string | null | undefined): AibCompactRole | null {
  const c = strategicCompactOf(s, compactId);
  return c && iso3 ? (indexOf(s).roleByPair.get(`${c.id}|${iso3.toUpperCase()}`) ?? null) : null;
}

/** The strategic compacts where a country is a core (allied) or leading non-allied country, with its core entry. */
export interface CoreCompact {
  compact: AibStrategicCompact;
  role: string;
  step: string | null;
  /** core (allied) rather than a leading non-allied country */
  allied: boolean;
}

export function coreCompactsOf(s: AibStrategic, iso3: string): CoreCompact[] {
  return s.compacts.flatMap((c): CoreCompact[] => {
    const core = c.core.find((x) => x.iso3 === iso3);
    if (core) return [{ compact: c, role: core.role, step: core.step, allied: true }];
    const non = c.nonAllied.find((x) => x.iso3 === iso3);
    return non ? [{ compact: c, role: non.role, step: non.step, allied: false }] : [];
  });
}

// ---- Bilateral theses ------------------------------------------------------------------------------------------------

export interface BilateralThesis {
  /** the two economies as asked (a first) */
  a: string;
  b: string;
  /** the priority strategic compacts (where cooperation matters most) */
  compacts: AibStrategicCompact[];
  /** what the relationship is primarily about (AIB_THESIS_LOGIC labels) */
  logic: string[];
  /** in each priority compact, what each side contributes (its compact role, null when it has none) */
  complementarity: { compact: AibStrategicCompact; a: AibCompactRole | null; b: AibCompactRole | null }[];
  /** each side's Strategic AIB Roles (primary first; [] when not assessed) */
  rolesA: StrategicRole[];
  rolesB: StrategicRole[];
}

function thesisFrom(s: AibStrategic, t: AibThesisTuple, a: string): BilateralThesis {
  const b = t[0] === a ? t[1] : t[0];
  const compacts = t[2].flatMap((id) => {
    const c = strategicCompactOf(s, id);
    return c ? [c] : [];
  });
  const rolesOf = (iso: string) => {
    const c = countryStrategicOf(s, iso);
    return c ? [...c.primary, ...c.secondary] : [];
  };
  return {
    a,
    b,
    compacts,
    logic: t[3],
    complementarity: compacts.map((c) => ({ compact: c, a: compactRoleOf(s, c.id, a), b: compactRoleOf(s, c.id, b) })),
    rolesA: rolesOf(a),
    rolesB: rolesOf(b),
  };
}

/** The relationship thesis of a pair (either order; oriented a -> b as asked), or null when the section has none. */
export function thesisOf(s: AibStrategic, a: string | null | undefined, b: string | null | undefined): BilateralThesis | null {
  if (!a || !b) return null;
  const A = a.toUpperCase();
  const B = b.toUpperCase();
  const t = indexOf(s).theses.get(pairKey(A, B));
  return t ? thesisFrom(s, t, A) : null;
}

/** Every thesis a country is part of, oriented from it, the most priority compacts first. */
export function thesesOf(s: AibStrategic, iso3: string | null | undefined): BilateralThesis[] {
  if (!iso3) return [];
  const A = iso3.toUpperCase();
  return (indexOf(s).thesesByCountry.get(A) ?? [])
    .map((t) => thesisFrom(s, t, A))
    .sort((x, y) => y.compacts.length - x.compacts.length || y.logic.length - x.logic.length || x.b.localeCompare(y.b));
}
