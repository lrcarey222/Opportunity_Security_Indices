// The three scenarios (2026-10-19): Baseline (trade as it is today, 2024, before any shock), Managed
// Interdependence (the diversify_benchmark shock: no supplier is removed; where a step is more concentrated
// than comparable goods, no supplier may hold more than the share that brings it to that benchmark) and
// Decoupling (the headline shock under hold: China stops supplying the allied market and keeps buying).
// The toggle chooses Managed Interdependence (the default institutional lens since 2026-10-20) | Decoupling
// (the stress test) (scenarioUrl.ts); Baseline is the reference every comparison shows beside them, always in
// the order Baseline, Managed Interdependence, Decoupling. A strategic compact is the persistent
// architecture; the scenario is the stress applied to it (STRATEGIC_FRAMING).
//
// Where each scenario's figures come from:
//   Baseline     node.baseline / compact.baseline (main document) and the lazily loaded scenario-baseline
//                file (today's network, each node's and compact's top ten suppliers). Coverage is 1 and
//                nothing is lost by definition; there is no class.
//   Managed      node.diversify (a node the scenario did not run on is unchanged: the Baseline figures with
//                ran = false), compact.managed, and the scenario-managed file (the compacts solved under the
//                scenario: coalition, tiers, roles, path, trade, investment; the network; investment flows).
//   Decoupling   the main document: node and compact fields, compacts[], doc.network (its headline* fields:
//                three-way views drop edges whose headline contribution is 0; cum* still sum the five supply
//                shocks) and doc.investmentFlows.
//
// Labels, words and colours; uniform figures per node, compact and node set; per-scenario accessors for
// the detail sections. Pure (no React, no IO).

import { SCENARIO_DEFINITION, SCENARIO_METHOD_NAME, SCENARIO_NAME, STATE } from '../../content/vocabulary.ts';
import { NEUTRAL_SERIES } from '../../theme/palette.ts';
import { isNum } from '../../lib/format.ts';
import {
  AIB_CLASSES, AIB_SCENARIO_IDS, diversifyShockIdOf, scenarioDefsOf, scenarioFilesOf, scenarioNamesOf,
  type AibBaselineScenarioDoc, type AibCoalition, type AibCompact, type AibDoc, type AibInvestmentEdge, type AibInvestmentFlows, type AibManagedScenarioDoc,
  type AibManifest, type AibMeta, type AibMetrics, type AibNetwork, type AibNode, type AibPathStep, type AibRole, type AibScenarioDoc, type AibScenarioId,
  type AibSupplierShare, type AibTradeEdge,
} from '../contract.ts';
import { classInfo } from './classes.ts';
import { DIVERSIFIED, demandSplit, type DemandSplit } from './diversify.ts';
import { fmtShare } from './format.ts';
import type { AibToggle } from '../scenarioUrl.ts';

export type { AibScenarioId } from '../contract.ts';

// ---- Labels, words and colours -----------------------------------------------------------------------

/** The display order of every comparison: Baseline, Managed Interdependence, Decoupling. */
export const SCENARIO_ORDER: readonly AibScenarioId[] = AIB_SCENARIO_IDS;

/**
 * The words the dashboard falls back to when meta.scenarios does not carry them (an older bundle). The
 * labels are the method's names (Baseline, Decoupling); the pages print the plain display names
 * (content/vocabulary.ts SCENARIO_NAME: Today's network, Major-supplier disruption).
 */
export const SCENARIO_DEFAULTS: Readonly<Record<AibScenarioId, { label: string; words: string }>> = {
  baseline: { label: 'Baseline', words: 'Trade as it is today (2024), before any shock.' },
  managed: {
    label: 'Managed Interdependence',
    words:
      'No supplier is removed. Where a supply-chain step is more concentrated than comparable goods, no supplier may hold more than the share that would bring it to that benchmark; trade and new capacity replace the excess.',
  },
  decoupling: {
    label: 'Decoupling',
    words: 'China stops supplying the allied market (it keeps buying); trade and new capacity replace what it supplied.',
  },
};

/**
 * Managed Interdependence in plain words for the pages. The vocabulary's SCENARIO_DEFINITION.managed says
 * "trusted suppliers" take the excess; in this analysis any economy may supply (the candidate universe is
 * global), so the pages say "other suppliers" (proposed for the shared vocabulary).
 */
export const MANAGED_PLAIN_WORDS =
  'No supplier is banned. Where a supply-chain step is unusually concentrated, the analysis caps how much of allied demand any one supplier can serve and asks which other suppliers would need to take the excess.';

/** What the disruption scenario asks, after naming who stops supplying. */
const DISRUPTION_ASKS = 'The analysis asks what existing partners can replace, what coordinated trade adds, and what still requires new capacity.';

/** The disruption scenario in plain words, naming the economy the data remove (SCENARIO_DEFINITION.decoupling for China). */
export const disruptionWords = (who: string): string =>
  who === 'China' ? SCENARIO_DEFINITION.decoupling : `A major supplier (${who}, in this analysis) stops supplying allied markets. ${DISRUPTION_ASKS}`;

/**
 * One fixed colour per scenario, the same in every chart, apart from the class and coverage colours drawn in
 * the same rows (blue COORDINATE / restored by trade #1889b0, olive BUILD / new capacity #8b9d17, rose MANAGE
 * SPECIALIZATION #c96f7a, the grays, and the orange chrome #f58229). Managed Interdependence purple #9c72fe
 * and Decoupling yellow #d7d209, validated 2026-10-20 with the dataviz validator on the dark surface #1f2937,
 * every pair that involves a scenario colour: CVD dE >= 11.6 (purple against blue, protan) and normal-vision
 * dE >= 17.4 (yellow against olive), contrast >= 4.3:1; the two scenarios 38.3 apart. The yellow sits above
 * the categorical lightness band, as the Baseline gray does: no in-band pair clears the class colours.
 * Baseline is the neutral reference (NEUTRAL_SERIES, lighter than the LOW INTERVENTION gray), drawn hollow
 * (SCENARIO_MARK) so it never rests on colour alone; every chart also prints the scenario's name.
 */
export const SCENARIO_COLOR: Readonly<Record<AibScenarioId, string>> = {
  baseline: NEUTRAL_SERIES,
  managed: '#9c72fe',
  decoupling: '#d7d209',
};

/** How a scenario's own marks are drawn: Baseline hollow (the reference), the two scenarios filled. */
export const SCENARIO_MARK: Readonly<Record<AibScenarioId, 'fill' | 'hollow'>> = { baseline: 'hollow', managed: 'fill', decoupling: 'fill' };

export interface ScenarioInfo {
  id: AibScenarioId;
  /** the display name: "Today's network", "Managed Interdependence", "Major-supplier disruption" (SCENARIO_NAME) */
  label: string;
  /** the method's name, as the data write it ("Baseline", "Managed Interdependence", "Decoupling"): CSV columns, file names, methods text */
  methodLabel: string;
  /** the short words in plain language (the toggle's line, chart legends) */
  words: string;
  /** the data's own words for the scenario (meta.scenarios; else the defaults) */
  methodWords: string;
  /** the shock id (null for Baseline) */
  shockId: string | null;
  color: string;
  mark: 'fill' | 'hollow';
}

/** "Zembla supply removed" -> "Zembla" (the shocked economy of a "<economy> supply removed" label), else null. */
const shockedEconomy = (label: string | undefined): string | null => {
  const m = label ? /^(.+?) supply removed$/i.exec(label.trim()) : null;
  return m ? m[1] : null;
};

/**
 * A scenario's display name and plain words (content/vocabulary.ts), its method name and words
 * (meta.scenarios where the bundle carries it, else the defaults: Decoupling's words then name the headline
 * shock's economy when its label says "<X> supply removed"), shock and colour (the shock ids from
 * meta.diversify and meta.headlineShock). The disruption's plain words name the economy the data remove
 * (its shock's "<X> supply removed"); without one they are the data's words.
 */
export function scenarioInfo(meta: AibMeta | null | undefined, id: AibScenarioId): ScenarioInfo {
  const def = meta ? scenarioDefsOf(meta).find((d) => d.id === id) : undefined;
  const fallbackShock = !meta ? null : id === 'managed' ? diversifyShockIdOf(meta) : id === 'decoupling' ? meta.headlineShock : null;
  const who = id === 'decoupling' && meta ? shockedEconomy(meta.shocks.find((s) => s.id === (def?.shockId ?? meta.headlineShock))?.label) : null;
  let methodWords = def?.words || SCENARIO_DEFAULTS[id].words;
  if (!def?.words && who && who !== 'China') methodWords = `${who} stops supplying the allied market (it keeps buying); trade and new capacity replace what it supplied.`;
  const words = id === 'baseline' ? SCENARIO_DEFINITION.baseline : id === 'managed' ? MANAGED_PLAIN_WORDS : who ? disruptionWords(who) : methodWords;
  return {
    id,
    label: SCENARIO_NAME[id],
    methodLabel: def?.label || SCENARIO_METHOD_NAME[id],
    words,
    methodWords,
    shockId: def ? def.shockId : fallbackShock,
    color: SCENARIO_COLOR[id],
    mark: SCENARIO_MARK[id],
  };
}

/** The three scenarios in display order. */
export const scenarioList = (meta: AibMeta | null | undefined): ScenarioInfo[] => SCENARIO_ORDER.map((id) => scenarioInfo(meta, id));

/** A scenario's label. */
export const scenarioLabel = (id: AibScenarioId, meta?: AibMeta | null): string => scenarioInfo(meta, id).label;

/** A scenario's short words. */
export const scenarioWords = (id: AibScenarioId, meta?: AibMeta | null): string => scenarioInfo(meta, id).words;

/** The About section the toggle links to ("How the scenarios work"). */
export const SCENARIOS_ABOUT_HREF = '/compacts/about#scenarios';

/**
 * What the scenarios are to a strategic compact, in plain words (strategic.framing carries the export's
 * shorthand, "Strategic compact = persistent architecture; scenario = the stress applied to it").
 */
export const STRATEGIC_FRAMING = 'A strategic compact is the lasting division of labor; the scenario is the stress applied to it.';

/** What each toggle choice is for: Managed Interdependence the default view, the disruption the severe case (both are stress tests). */
export const SCENARIO_LENS: Readonly<Record<AibToggle, string>> = {
  managed: 'the default view',
  decoupling: 'the severe case',
};

/**
 * The scenario part of an export's file name (" Managed Interdependence", " Decoupling"): every toggle
 * choice is named, the default included, when the bundle has scenarios; '' without them (Decoupling alone).
 */
export const scenarioFileTag = (id: AibScenarioId, meta: AibMeta | null | undefined, available = true): string => (available ? ` ${scenarioInfo(meta, id).methodLabel}` : '');

/** The plain-language caveat every scenario view may carry. */
export const SCENARIO_CAVEAT = 'Scenarios are stress tests of how the market could be organized, not forecasts; dollar values are 2024 trade values.';

/** SCENARIO_CAVEAT with the bundle's trade year. */
export const scenarioCaveat = (meta?: Pick<AibMeta, 'tradeYear'> | null): string =>
  `Scenarios are stress tests of how the market could be organized, not forecasts; dollar values are ${meta?.tradeYear ?? 2024} trade values.`;

/**
 * The bundle carries Managed Interdependence as a scenario (the toggle is shown): meta.scenarios lists it,
 * or a compact has compact.managed (the main document decides: the exporter writes the scenario files with
 * these fields). An older bundle shows Decoupling alone, without the toggle.
 */
export function hasScenarios(doc: Pick<AibDoc, 'meta' | 'compacts'>): boolean {
  const defs = scenarioDefsOf(doc.meta);
  return defs.some((d) => d.id === 'managed') || doc.compacts.some((c) => c.managed !== undefined);
}

/** The bundle carries today's figures (meta.scenarios lists Baseline, or a node has node.baseline); Baseline's coverage is 1 by definition either way. */
export function hasBaseline(doc: Pick<AibDoc, 'meta' | 'nodes'>): boolean {
  return scenarioDefsOf(doc.meta).some((d) => d.id === 'baseline') || doc.nodes.some((n) => n.baseline !== undefined);
}

/** The scenario files the manifest names ({managed?, baseline?}). */
export const scenarioFiles = (manifest: Pick<AibManifest, 'files'>) => scenarioFilesOf(manifest);

/** The scenario the pages show: the toggle when the bundle has scenarios, else Decoupling. */
export const effectiveScenario = (toggle: AibToggle, available: boolean): AibToggle => (available ? toggle : 'decoupling');

/** What Managed Interdependence does to supply, in the words every run line uses (it removes no supplier). */
export const MANAGED_SHOCK_WORDS = 'no supplier removed; supply above the concentration cap shifts';

/**
 * The label of what a scenario does to supply ("No shock: trade as it is today", MANAGED_SHOCK_WORDS, the
 * headline shock's label). Managed Interdependence is never named by its shock's label (the diversification
 * scenario's), so the pages call it by one name.
 */
export function scenarioShockLabel(meta: AibMeta, id: AibScenarioId): string {
  if (id === 'baseline') return `No disruption: trade as it is today (${meta.tradeYear})`;
  if (id === 'managed') return MANAGED_SHOCK_WORDS;
  const sid = scenarioInfo(meta, id).shockId;
  return meta.shocks.find((s) => s.id === sid)?.label ?? sid ?? scenarioLabel(id, meta);
}

/**
 * The CSV columns that name the scenario of an export: aib_scenario (its label) and aib_scenario_shock (its
 * shock id; "none (observed trade)" for Baseline). Never "scenario" or "shock": csvWithRun() writes the
 * run's own supply scenario and shock there, and would overwrite them. Every AIB page uses these names.
 */
export function scenarioCsvFields(meta: AibMeta | null | undefined, id: AibScenarioId): { aib_scenario: string; aib_scenario_shock: string } {
  const s = scenarioInfo(meta, id);
  return { aib_scenario: s.methodLabel, aib_scenario_shock: s.shockId ?? 'none (observed trade)' };
}

/** The run of a CSV of one scenario's figures for csvWithRun(): the meta with that scenario's shock as the run's shock (Decoupling: as it is). */
export function runMetaOf(meta: AibMeta, id: AibScenarioId): AibMeta {
  if (id === 'decoupling') return meta;
  const s = scenarioInfo(meta, id);
  return { ...meta, headlineShock: s.shockId ?? 'none (observed trade)' };
}

/**
 * "BUILD 67% · LOW INTERVENTION 33%": the classes' shares of some lost (or shifted) supply ("" when nothing is
 * lost). A share above 0 that rounds to 0% reads "<1%", never "0%".
 */
export function classMixText(groups: readonly { cls: string; usd: number }[]): string {
  const total = groups.reduce((s, g) => s + Math.max(0, g.usd), 0);
  if (total <= 0) return '';
  return groups
    .filter((g) => g.usd > 0)
    .map((g) => `${classInfo(g.cls).label} ${g.usd / total < 0.005 ? '<1%' : fmtShare(g.usd / total, 0)}`)
    .join(' · ');
}

// ---- Uniform figures ---------------------------------------------------------------------------------

/**
 * One scenario's figures for a node, a compact or a set of nodes, on one basis: shares of the allied
 * (demand-scope) demand; USD a year at 2024 trade values. static / existing / optimized: coverage before
 * trade, after existing relationships, after optimized trade (every candidate partner coordinating);
 * residual = 1 - optimized (only new capacity fills it); dividend = optimized - existing; split: the bar
 * kept / restored by trade / new capacity (demandSplit), null without coverage figures.
 */
export interface ScenarioFigures {
  id: AibScenarioId;
  /**
   * Baseline: true (the observed state); Managed Interdependence: the scenario ran (a node above its
   * benchmark; a compact or set with at least one such node), false = unchanged from Baseline;
   * Decoupling: the node was optimized (false for an INSUFFICIENT DATA node).
   */
  ran: boolean;
  demandUsd: number | null;
  lostUsd: number | null;
  /** the share of demand lost or shifted (lostUsd / demandUsd) */
  exposure: number | null;
  static: number | null;
  existing: number | null;
  optimized: number | null;
  residual: number | null;
  residualUsd: number | null;
  dividend: number | null;
  split: DemandSplit | null;
}

export interface NodeScenario extends ScenarioFigures {
  node: AibNode;
  /** the class under the scenario (null for Baseline, and where Managed Interdependence did not run) */
  cls: string | null;
  /** concentration on the scope-supply basis: Baseline observed, the others optimized (null where not exported) */
  hhi: number | null;
  largest: string | null;
  largestShare: number | null;
  top3Share: number | null;
  /** Managed Interdependence: node.diversify.status (null when the node was not assessed); else null */
  status: string | null;
  /** Managed Interdependence: new and expanded relationships (null where not exported) */
  newLinks: number | null;
  expandedLinks: number | null;
}

const mul = (a: number | null | undefined, b: number | null | undefined): number | null => (isNum(a) && isNum(b) ? a * b : null);
const sub = (a: number | null | undefined, b: number | null | undefined): number | null => (isNum(a) && isNum(b) ? a - b : null);
const numOr = (x: number | null | undefined): number | null => (isNum(x) ? x : null);
const splitOf = (f: { static: number | null; optimized: number | null }): DemandSplit | null => demandSplit({ static: f.static, optimized: f.optimized });

function baselineNode(node: AibNode, id: AibScenarioId, ran: boolean): NodeScenario {
  const b = node.baseline;
  const demand = numOr(node.demandUsd);
  return {
    id,
    node,
    ran,
    demandUsd: demand,
    lostUsd: 0,
    exposure: 0,
    static: 1,
    existing: 1,
    optimized: 1,
    residual: 0,
    residualUsd: 0,
    dividend: 0,
    split: { kept: 1, trade: 0, build: 0 },
    cls: null,
    hhi: numOr(b?.hhi),
    largest: b ? b.largest : node.baseLargest,
    largestShare: b ? numOr(b.largestShare) : numOr(node.baseLargestShare),
    top3Share: numOr(b?.top3Share),
    status: null,
    newLinks: null,
    expandedLinks: null,
  };
}

/**
 * A node under one scenario. Baseline: coverage 1, nothing lost, no class, today's concentration
 * (node.baseline; else baseLargest). Managed Interdependence: node.diversify where the scenario ran (lost =
 * diversify.lostUsd, else excess x demand; dividend = diversify.dividend, else optimized - existing), else
 * unchanged: the Baseline figures with ran = false and the status kept. Decoupling: the node's own fields
 * (residualUsd = residual x demand).
 */
export function nodeScenario(node: AibNode, id: AibScenarioId): NodeScenario {
  if (id === 'baseline') return baselineNode(node, 'baseline', true);
  if (id === 'managed') {
    const d = node.diversify;
    if (!d || d.status !== DIVERSIFIED || !isNum(d.static) || !isNum(d.optimized)) return { ...baselineNode(node, 'managed', false), status: d?.status ?? null };
    const demand = numOr(node.demandUsd);
    const f = { static: d.static, optimized: d.optimized };
    return {
      id,
      node,
      ran: true,
      demandUsd: demand,
      lostUsd: isNum(d.lostUsd) ? d.lostUsd : mul(d.excess, demand),
      exposure: numOr(d.excess),
      static: d.static,
      existing: numOr(d.existing),
      optimized: d.optimized,
      residual: isNum(d.residual) ? d.residual : 1 - d.optimized,
      residualUsd: isNum(d.residualUsd) ? d.residualUsd : mul(isNum(d.residual) ? d.residual : 1 - d.optimized, demand),
      dividend: isNum(d.dividend) ? d.dividend : sub(d.optimized, d.existing),
      split: splitOf(f),
      cls: d.cls,
      hhi: numOr(d.hhi),
      largest: d.largest ?? null,
      largestShare: numOr(d.largestShare),
      top3Share: numOr(d.top3Share),
      status: d.status,
      newLinks: numOr(d.newLinks),
      expandedLinks: numOr(d.expandedLinks),
    };
  }
  const demand = numOr(node.demandUsd);
  return {
    id,
    node,
    ran: isNum(node.optimized),
    demandUsd: demand,
    lostUsd: numOr(node.lostUsd),
    exposure: numOr(node.exposure),
    static: numOr(node.static),
    existing: numOr(node.existing),
    optimized: numOr(node.optimized),
    residual: numOr(node.residual),
    residualUsd: mul(node.residual, demand),
    dividend: numOr(node.dividend),
    split: splitOf(node),
    cls: node.cls,
    hhi: numOr(node.hhi),
    largest: node.largest,
    largestShare: numOr(node.largestShare),
    top3Share: null,
    status: null,
    newLinks: null,
    expandedLinks: null,
  };
}

/** A node under the three scenarios, in display order. */
export const nodeScenarios = (node: AibNode): NodeScenario[] => SCENARIO_ORDER.map((id) => nodeScenario(node, id));

/** The nodes every scenario's totals weigh: those optimized under Decoupling with a demand (INSUFFICIENT DATA nodes are left out of all three). */
export const comparableNodes = (nodes: readonly AibNode[]): AibNode[] => nodes.filter((n) => isNum(n.demandUsd) && n.demandUsd > 0 && isNum(n.static) && isNum(n.optimized));

/** Lost supply by class (AIB_CLASSES order; classes with nothing lost left out). */
export interface ClassLost {
  cls: string;
  lostUsd: number;
  /** nodes of the class that lose supply */
  n: number;
}

export interface ScenarioTotals extends ScenarioFigures {
  /** the nodes weighed (comparableNodes) */
  nodes: number;
  /** of them, the nodes the scenario ran on (Baseline: every one) */
  nodesRun: number;
  /** the allied demand of the nodes that lose or shift supply (USD a year) */
  affectedDemandUsd: number;
  byClass: ClassLost[];
}

function weigh(rows: readonly NodeScenario[], id: AibScenarioId): Omit<ScenarioFigures, 'split'> & { split: DemandSplit | null } {
  let D = 0;
  let st = 0;
  let ex = 0;
  let op = 0;
  let lost = 0;
  let res = 0;
  let exOk = true;
  for (const r of rows) {
    const d = r.demandUsd ?? 0;
    if (d <= 0 || !isNum(r.static) || !isNum(r.optimized)) continue;
    D += d;
    st += r.static * d;
    op += r.optimized * d;
    if (isNum(r.existing)) ex += r.existing * d;
    else exOk = false;
    lost += r.lostUsd ?? 0;
    res += isNum(r.residualUsd) ? r.residualUsd : (1 - r.optimized) * d;
  }
  if (D <= 0) return { id, ran: false, demandUsd: null, lostUsd: null, exposure: null, static: null, existing: null, optimized: null, residual: null, residualUsd: null, dividend: null, split: null };
  const f = { static: st / D, optimized: op / D };
  const existing = exOk ? ex / D : null;
  return {
    id,
    ran: rows.some((r) => r.ran),
    demandUsd: D,
    lostUsd: lost,
    exposure: lost / D,
    static: f.static,
    existing,
    optimized: f.optimized,
    residual: 1 - f.optimized,
    residualUsd: res,
    dividend: sub(f.optimized, existing),
    split: splitOf(f),
  };
}

/** Lost supply by the class under the scenario (Baseline: none). */
export function lostByClassOf(rows: readonly NodeScenario[]): ClassLost[] {
  const by = new Map<string, ClassLost>();
  for (const r of rows) {
    if (!r.cls || !isNum(r.lostUsd) || r.lostUsd <= 0) continue;
    const g = by.get(r.cls) ?? { cls: r.cls, lostUsd: 0, n: 0 };
    g.lostUsd += r.lostUsd;
    g.n += 1;
    by.set(r.cls, g);
  }
  const order = (c: string) => {
    const i = (AIB_CLASSES as readonly string[]).indexOf(c);
    return i < 0 ? AIB_CLASSES.length : i;
  };
  return [...by.values()].sort((a, b) => order(a.cls) - order(b.cls) || a.cls.localeCompare(b.cls));
}

/**
 * A set of nodes under one scenario (the Overview, a sector): coverage demand-weighted over the comparable
 * nodes (a node the scenario did not run on counts as unchanged), lost supply and the gap summed, the
 * demand of the nodes that lose supply, and the lost supply by class.
 */
export function scenarioTotals(nodes: readonly AibNode[], id: AibScenarioId): ScenarioTotals {
  const rows = comparableNodes(nodes).map((n) => nodeScenario(n, id));
  return {
    ...weigh(rows, id),
    nodes: rows.length,
    nodesRun: rows.filter((r) => r.ran).length,
    affectedDemandUsd: rows.reduce((s, r) => s + (isNum(r.lostUsd) && r.lostUsd > 0 ? (r.demandUsd ?? 0) : 0), 0),
    byClass: lostByClassOf(rows),
  };
}

/** A set of nodes under the three scenarios, in display order. */
export const scenarioTotalsAll = (nodes: readonly AibNode[]): ScenarioTotals[] => SCENARIO_ORDER.map((id) => scenarioTotals(nodes, id));

// ---- Compacts ------------------------------------------------------------------------------------------

/** "BUILD 78%; LOW INTERVENTION 22%" -> [{cls: "BUILD", share: 0.78}, ...] (unparseable parts left out). */
export function parseClassMix(s: string | null | undefined): { cls: string; share: number }[] {
  if (!s) return [];
  return s
    .split(';')
    .map((part) => /^\s*(.+?)\s+(\d+(?:\.\d+)?)%\s*$/.exec(part))
    .filter((m): m is RegExpExecArray => m !== null)
    .map((m) => ({ cls: m[1], share: Number(m[2]) / 100 }));
}

export interface CompactScenario extends ScenarioFigures {
  compact: AibCompact;
  /** its nodes */
  nodes: number;
  /** its nodes the scenario ran on (Baseline: every node; Decoupling: the optimized ones) */
  nodesRun: number;
  /** the classes' shares of the lost supply ("BUILD 78%; ..."; null for Baseline, or where nothing ran) */
  classMix: string | null;
  /** concentration: Baseline the compact's observed HHI (compact.baseline); null otherwise (not exported) */
  hhi: number | null;
  /** Baseline: today's top suppliers [ISO3, share of the compact's allied demand]; [] otherwise */
  suppliers: AibSupplierShare[];
  /** where the figures come from: the export's own (compact.metrics / compact.managed / compact.baseline) or its nodes, aggregated here */
  source: 'export' | 'nodes';
}

/**
 * A compact under one scenario, on the basis of the node figures: Decoupling from compact.metrics (optimized
 * = metrics.globalOptimum: every candidate partner coordinating, as Managed Interdependence's optimized is;
 * else its nodes aggregated; else metrics.trade), Managed Interdependence from compact.managed (else its nodes
 * aggregated, a node the scenario did not run on unchanged), Baseline coverage 1 with compact.baseline.
 * `nodeOf` (the context's) lets the fallbacks aggregate the compact's nodes.
 */
export function compactScenario(c: AibCompact, id: AibScenarioId, nodeOf?: ((id: string) => AibNode | null) | null): CompactScenario {
  const ns = nodeOf ? c.nodes.map((x) => nodeOf(x)).filter((n): n is AibNode => !!n) : [];
  const agg = () => weigh(ns.map((n) => nodeScenario(n, id)), id);
  const base = { compact: c, nodes: c.nodes.length, hhi: null, suppliers: [] as AibSupplierShare[] };
  if (id === 'baseline') {
    const demand = numOr(c.metrics.demandUsd);
    return {
      ...base,
      id,
      ran: true,
      demandUsd: demand,
      lostUsd: 0,
      exposure: 0,
      static: 1,
      existing: 1,
      optimized: 1,
      residual: 0,
      residualUsd: 0,
      dividend: 0,
      split: { kept: 1, trade: 0, build: 0 },
      nodesRun: c.nodes.length,
      classMix: null,
      hhi: numOr(c.baseline?.hhi),
      suppliers: c.baseline?.suppliers ?? [],
      source: c.baseline ? 'export' : 'nodes',
    };
  }
  if (id === 'managed') {
    const m = c.managed;
    if (m) {
      const f = { static: numOr(m.static), optimized: numOr(m.optimized) };
      const demand = numOr(m.demandUsd) ?? numOr(c.metrics.demandUsd);
      return {
        ...base,
        id,
        ran: m.nodesRun > 0,
        demandUsd: demand,
        lostUsd: numOr(m.lostUsd),
        exposure: isNum(m.lostUsd) && isNum(demand) && demand > 0 ? m.lostUsd / demand : null,
        static: f.static,
        existing: numOr(m.existing),
        optimized: f.optimized,
        residual: isNum(m.residual) ? m.residual : isNum(f.optimized) ? 1 - f.optimized : null,
        residualUsd: numOr(m.residualUsd),
        dividend: sub(m.optimized, m.existing),
        split: splitOf(f),
        nodesRun: m.nodesRun,
        classMix: m.classMix,
        source: 'export',
      };
    }
    const rows = ns.map((n) => nodeScenario(n, id));
    const run = rows.filter((r) => r.ran);
    const byClass = lostByClassOf(rows);
    const lost = byClass.reduce((s, g) => s + g.lostUsd, 0);
    return {
      ...base,
      ...agg(),
      id,
      ran: run.length > 0,
      nodesRun: run.length,
      classMix: lost > 0 ? byClass.map((g) => `${g.cls} ${Math.round((g.lostUsd / lost) * 100)}%`).join('; ') : null,
      source: 'nodes',
    };
  }
  const mt: AibMetrics = c.metrics;
  const a = ns.length ? agg() : null;
  const optimized = isNum(mt.globalOptimum) ? mt.globalOptimum : (a?.optimized ?? numOr(mt.trade));
  const st = isNum(mt.static) ? mt.static : (a?.static ?? null);
  const demand = numOr(mt.demandUsd) ?? a?.demandUsd ?? null;
  const residual = isNum(optimized) ? 1 - optimized : null;
  return {
    ...base,
    id,
    ran: true,
    demandUsd: demand,
    lostUsd: numOr(mt.lostUsd) ?? a?.lostUsd ?? null,
    exposure: numOr(mt.exposure),
    static: st,
    existing: numOr(mt.existing),
    optimized,
    residual,
    residualUsd: a && isNum(a.residualUsd) ? a.residualUsd : mul(residual, demand),
    dividend: sub(optimized, mt.existing),
    split: splitOf({ static: st, optimized }),
    nodesRun: ns.length ? ns.filter((n) => isNum(n.optimized)).length : c.nodes.length,
    classMix: c.classMix ?? null,
    source: isNum(mt.globalOptimum) ? 'export' : 'nodes',
  };
}

/** A compact under the three scenarios, in display order. */
export const compactScenarios = (c: AibCompact, nodeOf?: ((id: string) => AibNode | null) | null): CompactScenario[] =>
  SCENARIO_ORDER.map((id) => compactScenario(c, id, nodeOf));

/**
 * A compact's coalition, tiers, roles, path and relationships under one scenario.
 *   status 'ready'    the detail is here (Managed Interdependence: coalition may still be null, with coalitionReason)
 *   status 'pending'  Managed Interdependence without its file (loading, failed, or not exported: the hook says which)
 *   status 'none'     no detail: Baseline (no shock, no coalition), or a compact the scenario file does not list
 */
export interface CompactDetail {
  id: AibScenarioId;
  status: 'ready' | 'pending' | 'none';
  /** why there is no coalition or no detail, in plain words (null when there is one) */
  reason: string | null;
  coalition: AibCoalition | null;
  nodesRun: number | null;
  anchors: string[];
  suppliers: string[];
  hosts: string[];
  technologyPartners: string[];
  bridges: string[];
  associates: string[];
  roles: AibRole[];
  metrics: AibMetrics | null;
  path: AibPathStep[];
  /** trade relationships: new or expanded (top 40 by USD; domestic retention excluded) */
  trade: AibTradeEdge[];
  investment: AibInvestmentEdge[];
}

const emptyDetail = (): Omit<CompactDetail, 'id' | 'status' | 'reason'> => ({
  coalition: null,
  nodesRun: null,
  anchors: [],
  suppliers: [],
  hosts: [],
  technologyPartners: [],
  bridges: [],
  associates: [],
  roles: [],
  metrics: null,
  path: [],
  trade: [],
  investment: [],
});

/** Why Baseline has no coalition. */
export const BASELINE_NO_COALITION = "Today's network imposes no disruption: no supply is removed, so there is no coalition to form.";

/** The detail of a compact under one scenario (`managed`: the scenario-managed file, null or undefined while it is not loaded). */
export function compactDetail(c: AibCompact, id: AibScenarioId, managed?: AibManagedScenarioDoc | null): CompactDetail {
  if (id === 'decoupling') {
    const { coalition, anchors, suppliers, hosts, technologyPartners, bridges, associates, roles, metrics, path, trade, investment } = c;
    return { id, status: 'ready', reason: null, coalition, nodesRun: c.nodes.length, anchors, suppliers, hosts, technologyPartners, bridges, associates, roles, metrics, path, trade, investment };
  }
  if (id === 'baseline') return { ...emptyDetail(), id, status: 'none', reason: BASELINE_NO_COALITION, nodesRun: c.nodes.length };
  if (!managed) return { ...emptyDetail(), id, status: 'pending', reason: null };
  const m = managed.compacts.find((x) => x.id === c.id);
  if (!m) return { ...emptyDetail(), id, status: 'none', reason: 'The Managed Interdependence results have no entry for this compact.' };
  return {
    id,
    status: 'ready',
    reason: m.coalition ? null : (m.coalitionReason ?? STATE.noCoalition),
    coalition: m.coalition,
    nodesRun: m.nodesRun,
    anchors: m.anchors,
    suppliers: m.suppliers,
    hosts: m.hosts,
    technologyPartners: m.technologyPartners,
    bridges: m.bridges,
    associates: m.associates,
    roles: m.roles,
    metrics: m.metrics,
    path: m.path,
    trade: m.trade,
    investment: m.investment,
  };
}

/** A compact's trade relationships under a scenario (Baseline: [], see compactSuppliersToday()). */
export const compactTrade = (c: AibCompact, id: AibScenarioId, managed?: AibManagedScenarioDoc | null): AibTradeEdge[] => compactDetail(c, id, managed).trade;

/** A compact's investment relationships under a scenario (Baseline: []). */
export const compactInvestment = (c: AibCompact, id: AibScenarioId, managed?: AibManagedScenarioDoc | null): AibInvestmentEdge[] => compactDetail(c, id, managed).investment;

// ---- Network and investment flows ------------------------------------------------------------------

const sideHeadline = (c: AibNetwork['countries'][number]) => c.supplier.headline + c.buyer.headline + c.host.headline + c.investor.headline;

/**
 * The Decoupling network as three-way views read it: only the edges with a headline (Decoupling)
 * contribution (trade or investment above 0), and the economies with a headline role, a kept edge or a
 * loss. The cum* fields are left as they are (they still sum the five supply shocks), and so are each
 * side's `partners` and `selected` counts: read Decoupling's partner counts with headlinePartnersOf()
 * (derive/network.ts), which counts them among the kept edges.
 */
export function headlineNetwork(n: AibNetwork): AibNetwork {
  const edges = n.edges.filter((e) => e.trade > 0 || e.investment > 0);
  const inEdge = new Set(edges.flatMap((e) => [e.from, e.to]));
  const countries = n.countries.filter((c) => inEdge.has(c.iso3) || sideHeadline(c) > 0 || (isNum(c.lostUsd) && c.lostUsd > 0));
  return { ...n, countries, edges };
}

const fileFor = <K extends AibScenarioDoc['scenario']>(d: AibScenarioDoc | null | undefined, id: K): Extract<AibScenarioDoc, { scenario: K }> | null =>
  d && d.scenario === id ? (d as Extract<AibScenarioDoc, { scenario: K }>) : null;

/**
 * The network of a scenario: Decoupling, doc.network through headlineNetwork() (`headlineOnly` false: as
 * exported); Managed Interdependence and Baseline, their file's network (null until it is loaded, or when
 * the file has none).
 */
export function scenarioNetwork(doc: Pick<AibDoc, 'network'>, id: AibScenarioId, scenarioDoc?: AibScenarioDoc | null, opts: { headlineOnly?: boolean } = {}): AibNetwork | null {
  if (id === 'decoupling') return doc.network ? (opts.headlineOnly === false ? doc.network : headlineNetwork(doc.network)) : null;
  return fileFor(scenarioDoc, id)?.network ?? null;
}

/** The investor -> host flows of a scenario: Decoupling doc.investmentFlows, Managed Interdependence its file's, Baseline none. */
export function scenarioInvestmentFlows(doc: Pick<AibDoc, 'investmentFlows'>, id: AibScenarioId, scenarioDoc?: AibScenarioDoc | null): AibInvestmentFlows | null {
  if (id === 'decoupling') return doc.investmentFlows ?? null;
  if (id === 'managed') return fileFor(scenarioDoc, 'managed')?.investmentFlows ?? null;
  return null;
}

// ---- Today's suppliers ---------------------------------------------------------------------------------

export interface SupplierToday {
  iso3: string;
  /** USD a year (from the share and the demand where the export gives only the share) */
  usd: number | null;
  /** share of the node's or compact's allied demand */
  share: number | null;
}

const fromShares = (s: readonly AibSupplierShare[] | undefined, demand: number | null | undefined): SupplierToday[] =>
  (s ?? []).map(([iso3, share]) => ({ iso3, share: numOr(share), usd: mul(share, demand) }));

/** A node's suppliers today: the top ten of the scenario-baseline file when loaded, else the top five of node.baseline. */
export function nodeSuppliersToday(node: AibNode, baseline?: AibBaselineScenarioDoc | null): SupplierToday[] {
  const row = baseline?.nodes.find((n) => n.id === node.id);
  if (row) return row.suppliers.map(([iso3, usd, share]) => ({ iso3, usd: numOr(usd), share: numOr(share) }));
  return fromShares(node.baseline?.suppliers, node.demandUsd);
}

/** A compact's suppliers today: the top ten of the scenario-baseline file when loaded, else the top five of compact.baseline. */
export function compactSuppliersToday(c: AibCompact, baseline?: AibBaselineScenarioDoc | null): SupplierToday[] {
  const row = baseline?.compacts.find((x) => x.id === c.id);
  if (row) return row.suppliers.map(([iso3, usd, share]) => ({ iso3, usd: numOr(usd), share: numOr(share) }));
  return fromShares(c.baseline?.suppliers, c.metrics.demandUsd);
}

// ---- Names ---------------------------------------------------------------------------------------------

/** An economy's name: the scenario file's names first (codes only it uses), then the main document's (`name`, the context's). */
export function scenarioNameOf(name: (iso3: string | null | undefined) => string, scenarioDoc?: AibScenarioDoc | null): (iso3: string | null | undefined) => string {
  if (!scenarioDoc) return name;
  const names = scenarioNamesOf(scenarioDoc);
  return (iso3) => (iso3 && names[iso3]) || name(iso3);
}
