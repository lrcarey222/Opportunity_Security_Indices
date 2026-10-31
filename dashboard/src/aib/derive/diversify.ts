// The diversification scenario (2026-10-19): "Diversified to its sector's concentration" (meta.diversify,
// node.diversify; scripts/32, a shock of type "diversify"). It removes no supplier. Each supply-chain node's
// world exporter concentration (HHI; Hong Kong and Macao counted with China) is set against a benchmark:
// the trade-weighted mean concentration of the OTHER HS6 lines of the node's HS4 customs heading or, where
// the heading has no other line, of the other nodes of its electro-industrial dictionary sector (the model
// keeps that sector; the views present nodes by OSI technology x stage). A node more concentrated than its
// benchmark (by more than a small tolerance the export does not state; config/aib.yml min_excess) gets the
// supplier-share cap that would bring its concentration down to it; suppliers above the cap in the allied
// market (the demand scope) lose the excess; existing relationships, then optimized trade, replace what they
// can with the cap holding; the rest is the new capacity diversification needs. A node above its benchmark
// by less than the tolerance carries the status "already as diversified as its benchmark" (nearBenchmark).
//
// Every share of node.diversify is a share of the node's allied demand (node.demandUsd): the supply to shift
// is excess x demandUsd, and residualUsd ~ residual x demandUsd. The split of the bar (kept / restored by
// trade / new capacity) sums to 1: kept is the coverage under the cap before trade (static), restored by
// trade is what optimized trade adds to it, new capacity the rest (1 - optimized, the residual). An export
// where coverage after trade sits below coverage before it (an earlier run) shows the lower figure as kept.
// The words never claim that what trade restores and the gap add up to the supply to shift.
//
// The class under the scenario carries its own PHYSICAL CAPABILITY NOT VALIDATED flag (node.variantWarnings
// under the scenario's shock id), in the words, the table and the CSV, as every variant's class does.
//
// Rows, their order and filters, the page totals, the axis of the concentration chart, the node block's
// sentences per status, and the CSV row. Pure.

import { isNum } from '../../lib/format.ts';
import { AIB_DIVERSIFY_SHOCK, AIB_DIVERSIFY_STATUSES, diversifyShockIdOf, type AibMeta, type AibNode, type AibNodeDiversify } from '../contract.ts';
import { classInfo, PHYSICAL_WARNING_INLINE } from './classes.ts';
import { fmtScore, fmtShare, fmtUsd, listWords, nameOf, plural } from './format.ts';
import { variantWarning } from './sensitivity.ts';
import { diversifySetting, DIVERSIFY_LABEL } from './settings.ts';
import { cellStagesOfNodes, cellTechsOfNodes, nodeInCell, osiFields, stageOptions, techOptions, type Taxonomy } from './taxonomy.ts';

export const [DIVERSIFIED, WITHIN_CAP, ALREADY_DIVERSIFIED, NO_BENCHMARK] = AIB_DIVERSIFY_STATUSES;

/** The page of the scenario. */
export const DIVERSIFY_HREF = '/aib/diversification';

/** The statuses in words: a label (the table, the CSV) and what it means for the node. */
export const DIVERSIFY_STATUS_WORDS: Readonly<Record<string, { label: string; meaning: string }>> = {
  [DIVERSIFIED]: { label: 'Diversified to the benchmark', meaning: 'more concentrated than its benchmark, with suppliers above the cap in the allied market: the scenario ran' },
  [WITHIN_CAP]: { label: 'Within the cap in the allied market', meaning: 'more concentrated than its benchmark worldwide, but no supplier to the allied market is above the cap: nothing to shift' },
  [ALREADY_DIVERSIFIED]: {
    label: 'Already as diversified as its benchmark',
    meaning: 'as diversified as its benchmark, or above it by less than a small tolerance: the scenario leaves it as it is',
  },
  [NO_BENCHMARK]: { label: 'No benchmark', meaning: 'no other line of its heading and no other supply-chain step of its dictionary sector to compare it with' },
};

/** A status in words (an unknown status as written). */
export const statusWords = (s: string): { label: string; meaning: string } => DIVERSIFY_STATUS_WORDS[s] ?? { label: s, meaning: s };

/** The words for a node above its benchmark by less than the tolerance the scenario leaves alone. */
export const NEAR_BENCHMARK_WORDS = 'within a small tolerance of its benchmark';

/** A row's status label: the status in words, with the tolerance named for a node just above its benchmark. */
export const rowStatusLabel = (r: Pick<DiversifyRow, 'd' | 'nearBenchmark'>): string => `${statusWords(r.d.status).label}${r.nearBenchmark ? ' (above it by less than a small tolerance)' : ''}`;

/** A class under the scenario as plain text, with the physical-capability warning named after it ("not run" without a class). */
export const diversifyClassText = (cls: string | null | undefined, warning: boolean | null = null): string =>
  cls ? `${classInfo(cls).label}${warning ? ` (${PHYSICAL_WARNING_INLINE})` : ''}` : 'not run';

/** The status order of the lists: diversified first, then within the cap, already diversified, no benchmark, anything else. */
const statusRank = (s: string): number => {
  const i = (AIB_DIVERSIFY_STATUSES as readonly string[]).indexOf(s);
  return i < 0 ? AIB_DIVERSIFY_STATUSES.length : i;
};

/** The bundle carries the scenario: meta.diversify, or a node with node.diversify. */
export const hasDiversify = (doc: { meta: AibMeta; nodes: readonly AibNode[] }): boolean => !!doc.meta.diversify || doc.nodes.some((n) => n.diversify !== undefined);

/** The scenario's label: meta.diversify.label, else its shock's label, else DIVERSIFY_LABEL. */
export function diversifyLabel(m: AibMeta): string {
  const s = diversifySetting(m);
  if (s) return s.label;
  const id = diversifyShockIdOf(m);
  return m.shocks.find((x) => x.id === id)?.label ?? DIVERSIFY_LABEL;
}

/** A node's allied demand split to scale: kept under the cap, restored by trade under it, new capacity (shares summing to 1). */
export interface DemandSplit {
  kept: number;
  trade: number;
  build: number;
}

const clamp01 = (x: number) => Math.max(0, Math.min(1, x));

/**
 * The split of the bar: kept = coverage under the cap before trade (static; the coverage after trade when
 * that is lower), restored by trade = optimized minus kept, new capacity = 1 - optimized. Null when the
 * scenario did not run for the node (no static or optimized coverage).
 */
export function demandSplit(d: Pick<AibNodeDiversify, 'static' | 'optimized'>): DemandSplit | null {
  if (!isNum(d.static) || !isNum(d.optimized)) return null;
  const opt = clamp01(d.optimized);
  const kept = Math.min(clamp01(d.static), opt);
  return { kept, trade: opt - kept, build: 1 - opt };
}

/** One node in the scenario, with its derived figures. */
export interface DiversifyRow {
  node: AibNode;
  d: AibNodeDiversify;
  /** more concentrated than its benchmark (diversified, or within the cap in the allied market) */
  above: boolean;
  /** the scenario ran for the node (status "diversified to the benchmark") */
  ran: boolean;
  /** current minus benchmark concentration (null without a benchmark) */
  hhiGap: number | null;
  /** the node's allied (demand-scope) demand, USD a year */
  demandUsd: number | null;
  /** the supply to shift, USD a year: what the capped suppliers lose (excess x allied demand) */
  shiftUsd: number | null;
  /** what trade under the cap restores, USD a year (the trade part of the split x allied demand) */
  tradeUsd: number | null;
  /** the new capacity diversification needs, USD a year (residualUsd; else residual x allied demand) */
  gapUsd: number | null;
  /** the bar (null when the scenario did not run) */
  split: DemandSplit | null;
  /**
   * the class under the scenario carries the PHYSICAL CAPABILITY NOT VALIDATED flag (node.variantWarnings
   * under the scenario's shock id); null without a class or in an export that predates the flags
   */
  clsWarning: boolean | null;
  /**
   * already as diversified as its benchmark, yet above it: by less than the tolerance the scenario leaves
   * alone (config/aib.yml min_excess; not in the export)
   */
  nearBenchmark: boolean;
}

/**
 * A node's row in the scenario; null when the node has no node.diversify (an older bundle, a node not
 * assessed). `shockId`: the scenario's id in node.variantWarnings (diversifyShockIdOf(meta)).
 */
export function diversifyRow(n: AibNode, shockId: string = AIB_DIVERSIFY_SHOCK): DiversifyRow | null {
  const d = n.diversify;
  if (!d || typeof d !== 'object') return null;
  const ran = d.status === DIVERSIFIED;
  const demandUsd = isNum(n.demandUsd) ? n.demandUsd : null;
  const split = ran ? demandSplit(d) : null;
  const times = (x: number | null | undefined) => (isNum(x) && demandUsd !== null ? x * demandUsd : null);
  const hhiGap = isNum(d.currentHhi) && isNum(d.benchmarkHhi) ? d.currentHhi - d.benchmarkHhi : null;
  return {
    node: n,
    d,
    above: ran || d.status === WITHIN_CAP,
    ran,
    hhiGap,
    demandUsd,
    // the exported USD where the export has it (the scenario toggle's figure: excess x demand before rounding)
    shiftUsd: ran && isNum(d.lostUsd) ? d.lostUsd : times(d.excess),
    tradeUsd: split ? times(split.trade) : null,
    gapUsd: isNum(d.residualUsd) ? d.residualUsd : times(d.residual),
    split,
    clsWarning: d.cls ? variantWarning(n, shockId) : null,
    nearBenchmark: d.status === ALREADY_DIVERSIFIED && hhiGap !== null && hhiGap > 0,
  };
}

/** Every node the scenario assessed (those with node.diversify), in the document's order. */
export function diversifyRows(nodes: readonly AibNode[], shockId: string = AIB_DIVERSIFY_SHOCK): DiversifyRow[] {
  return nodes.map((n) => diversifyRow(n, shockId)).filter((r): r is DiversifyRow => r !== null);
}

const desc = (a: number | null, b: number | null) => (isNum(b) ? b : -Infinity) - (isNum(a) ? a : -Infinity);

/**
 * The rows in reading order: by status (diversified first, then within the cap, already diversified, no
 * benchmark); then the capacity gap in USD, largest first (missing last); then the supply to shift, the
 * distance above the benchmark and the label.
 */
export function orderDiversify(rows: readonly DiversifyRow[]): DiversifyRow[] {
  return [...rows].sort(
    (a, b) =>
      statusRank(a.d.status) - statusRank(b.d.status) ||
      desc(a.gapUsd, b.gapUsd) ||
      desc(a.shiftUsd, b.shiftUsd) ||
      desc(a.hhiGap, b.hhiGap) ||
      a.node.label.localeCompare(b.node.label, 'en'),
  );
}

export interface DiversifyFilter {
  /** an OSI technology, or 'Not in the OSI basket' (taxonomy.ts NOT_IN_BASKET) */
  tech: string | null;
  /** one of the six stages: a node passes with a cell there (of the technology, when one is chosen; never node.stage) */
  stage: string | null;
  /** include the nodes the scenario did not diversify (within the cap, already as diversified, no benchmark) */
  all: boolean;
}

export const NO_DIVERSIFY_FILTER: DiversifyFilter = { tech: null, stage: null, all: false };

/**
 * The rows a filter keeps: by default only the nodes the scenario diversified; the technology and stage by
 * the node's cells (taxonomy.ts nodeInCell(): a node in several cells passes each).
 */
export function filterDiversify(rows: readonly DiversifyRow[], f: DiversifyFilter): DiversifyRow[] {
  return rows.filter((r) => (f.all || r.ran) && nodeInCell(r.node, f.tech, f.stage));
}

/** The Technology choices of some rows: the OSI technologies of their cells, 'Not in the OSI basket' last; only those with a cell at `stage` when one is chosen. */
export function diversifyTechOptions(rows: readonly DiversifyRow[], stage: string | null = null): { value: string; label: string }[] {
  return techOptions(cellTechsOfNodes(rows.map((r) => r.node), stage));
}

/** The Stage choices of some rows: the stages that hold their nodes' cells (of `tech` when one is chosen), in chain order (no AIB node has a Deployment cell). */
export function diversifyStageOptions(rows: readonly DiversifyRow[], tech: string | null = null): { value: string; label: string }[] {
  return stageOptions(cellStagesOfNodes(rows.map((r) => r.node), tech));
}

export interface DiversifyTotals {
  /** nodes in the document */
  nodes: number;
  /** nodes the scenario assessed (with node.diversify) */
  assessed: number;
  /** of those, nodes with a benchmark */
  withBenchmark: number;
  /** nodes more concentrated than their benchmark by more than the tolerance (diversified + within the cap) */
  above: number;
  diversified: number;
  withinCap: number;
  already: number;
  /** of the nodes already as diversified, those above their benchmark by less than the tolerance */
  nearBenchmark: number;
  noBenchmark: number;
  /** allied demand of the nodes above their benchmark, USD */
  demandUsd: number;
  /** supply to shift in those nodes, USD, and as a share of their allied demand */
  shiftUsd: number;
  shiftShare: number | null;
  /** what trade under the cap restores, USD */
  tradeUsd: number;
  /** the new capacity diversification needs, USD */
  gapUsd: number;
  /** diversified nodes where trade under the cap replaces everything (no new capacity needed) */
  closedByTrade: number;
  /** each capped supplier with the number of nodes it is capped in, most first (then by code) */
  capped: { iso3: string; nodes: number }[];
}

const sum = (xs: readonly (number | null)[]) => xs.reduce<number>((s, x) => s + (isNum(x) ? x : 0), 0);

/** The scenario's totals over every node of a document (the page's lede). */
export function diversifyTotals(nodes: readonly AibNode[]): DiversifyTotals {
  const rows = diversifyRows(nodes);
  const up = rows.filter((r) => r.above);
  const ran = rows.filter((r) => r.ran);
  const count = (s: string) => rows.filter((r) => r.d.status === s).length;
  const demandUsd = sum(up.map((r) => r.demandUsd));
  const shiftUsd = sum(up.map((r) => r.shiftUsd));
  const counts = new Map<string, number>();
  for (const r of ran) for (const iso of new Set(r.d.capped)) counts.set(iso, (counts.get(iso) ?? 0) + 1);
  return {
    nodes: nodes.length,
    assessed: rows.length,
    withBenchmark: rows.length - count(NO_BENCHMARK),
    above: up.length,
    diversified: ran.length,
    withinCap: count(WITHIN_CAP),
    already: count(ALREADY_DIVERSIFIED),
    nearBenchmark: rows.filter((r) => r.nearBenchmark).length,
    noBenchmark: count(NO_BENCHMARK),
    demandUsd,
    shiftUsd,
    shiftShare: demandUsd > 0 ? shiftUsd / demandUsd : null,
    tradeUsd: sum(ran.map((r) => r.tradeUsd)),
    gapUsd: sum(ran.map((r) => r.gapUsd)),
    closedByTrade: ran.filter((r) => r.split !== null && r.split.build <= 1e-9).length,
    capped: [...counts.entries()].map(([iso3, n]) => ({ iso3, nodes: n })).sort((a, b) => b.nodes - a.nodes || a.iso3.localeCompare(b.iso3)),
  };
}

/** "China (26 nodes), Germany (6) and the United States (5)": the most-capped suppliers, at most `max`. */
export function cappedWords(capped: DiversifyTotals['capped'], names: Readonly<Record<string, string>>, max = 3): string {
  return listWords(capped.slice(0, max).map((c, i) => `${nameOf(names, c.iso3)} (${i === 0 ? plural(c.nodes, 'step') : c.nodes})`));
}

/**
 * "39 of the 74 nodes with a benchmark are more concentrated than their benchmark by more than a small
 * tolerance" (the lede and About).
 */
export function aboveWords(t: Pick<DiversifyTotals, 'above' | 'withBenchmark'>): string {
  const one = t.above === 1;
  return `${t.above} of the ${plural(t.withBenchmark, 'supply-chain step')} with a benchmark ${one ? 'is' : 'are'} more concentrated than ${one ? 'its' : 'their'} benchmark by more than a small tolerance`;
}

/**
 * The totals in plain sentences (the page's lede): how many nodes with a benchmark are above it by more than
 * the tolerance (and how many sit above it by less), the supply to shift (USD and share of the allied
 * demand in those nodes), what trade restores and what is left for new capacity (two figures, not a split
 * of the supply to shift). The most-capped suppliers follow the tiles, not here. "" when the scenario
 * assessed no node.
 */
export function totalsWords(t: DiversifyTotals): string {
  if (!t.assessed) return '';
  const within = t.withinCap ? ` (in ${plural(t.withinCap, 'of them', 'of them')} no supplier to the allied market is above the cap, so nothing shifts)` : '';
  const near = t.nearBenchmark
    ? ` ${plural(t.nearBenchmark, 'other step')} ${t.nearBenchmark === 1 ? 'sits above its benchmark by less than that and is' : 'sit above their benchmark by less than that and are'} left as ${t.nearBenchmark === 1 ? 'it is' : 'they are'}.`
    : '';
  const head = `${aboveWords(t)}${within}.${near}`;
  if (!t.diversified) return head;
  const shift = `Capping their suppliers in the allied market at the share that would bring each down to its benchmark shifts ${fmtUsd(t.shiftUsd)} of supply${t.shiftShare !== null ? ` (${fmtShare(t.shiftShare, 0)} of the allied demand at those steps)` : ''} away from the capped suppliers.`;
  const trade = `Existing relationships and coordinated trade recover ${fmtUsd(t.tradeUsd)} with the cap holding; ${fmtUsd(t.gapUsd)} still needs new capacity${t.closedByTrade ? `, and at ${plural(t.closedByTrade, 'step')} trade replaces everything` : ''}.`;
  return `${head} ${shift} ${trade}`;
}

/** The axis maximum of the concentration scale: the largest value rounded up to the next 0.1 (at least 0.1, at most 1). */
export function hhiAxisMax(rows: readonly DiversifyRow[]): number {
  const m = Math.max(0, ...rows.flatMap((r) => [r.d.currentHhi, r.d.benchmarkHhi]).filter(isNum));
  return Math.min(1, Math.max(0.1, Math.ceil(m * 10 - 1e-9) / 10));
}

/** The relationships of node.diversify.partners ("DEU->FRA; DEU->ESP"), in order. */
export function relationshipsOf(s: string | null | undefined): { from: string; to: string }[] {
  if (!s) return [];
  return s
    .split(';')
    .map((p) => p.split('->').map((x) => x.trim()))
    .filter((p) => p.length === 2 && !!p[0] && !!p[1])
    .map(([from, to]) => ({ from, to }));
}

/** The relationships by name: "Germany → France; Germany → Spain" ("" when none). */
export function relationshipWords(s: string | null | undefined, names: Readonly<Record<string, string>>): string {
  return relationshipsOf(s)
    .map((e) => `${nameOf(names, e.from)} → ${nameOf(names, e.to)}`)
    .join('; ');
}

/**
 * The basis as a phrase: "the other HS6 lines of heading 8507", "the other HS6 lines of headings 2826 and
 * 7410", "the other nodes of its dictionary sector" (the export's "other nodes of the sector": the sector is
 * the electro-industrial dictionary's, not an OSI technology; "its benchmark" when the export has none).
 */
export function basisWords(basis: string | null | undefined): string {
  if (!basis) return 'its benchmark';
  const b = basis.trim();
  const hs = /^other HS6 lines of headings? (.+)$/i.exec(b);
  if (hs) {
    const codes = hs[1].split(/\s*[;,]\s*/).filter(Boolean);
    return `the other HS6 lines of heading${codes.length > 1 ? 's' : ''} ${listWords(codes)}`;
  }
  if (/^other nodes of the sector$/i.test(b)) return 'the other supply-chain steps of its dictionary sector';
  return /^other\b/i.test(b) ? `the ${b}` : b;
}

/** "0.38 against 0.09 for the other HS6 lines of heading 8507". */
export function benchmarkWords(d: Pick<AibNodeDiversify, 'currentHhi' | 'benchmarkHhi' | 'basis'>): string {
  return `${fmtScore(d.currentHhi)} against ${fmtScore(d.benchmarkHhi)} for ${basisWords(d.basis)}`;
}

const namesOf = (xs: readonly string[], names: Readonly<Record<string, string>>) => listWords(xs.map((x) => nameOf(names, x)));

/**
 * The node block's sentences, worded per status: diversified (the cap that would bring it down to the
 * benchmark, who is above it in the allied market, the supply to shift, what trade restores and the gap
 * left, then the class under the scenario with its physical-capability flag, the hosts and the largest new
 * or expanded relationships), within the cap (nothing to shift), already as diversified (or within a small
 * tolerance of the benchmark), or without a benchmark. `coverage: false` leaves out the sentence on what
 * trade restores and the gap left (a node block that shows them in its three-way coverage rows).
 */
export function diversifyWords(r: DiversifyRow, names: Readonly<Record<string, string>>, opts: { coverage?: boolean } = {}): string[] {
  const { d } = r;
  switch (d.status) {
    case ALREADY_DIVERSIFIED:
      return [
        r.nearBenchmark
          ? `${NEAR_BENCHMARK_WORDS[0].toUpperCase()}${NEAR_BENCHMARK_WORDS.slice(1)}: concentration (HHI) ${benchmarkWords(d)}. It is above the benchmark by less than the margin the scenario acts on, so the scenario leaves it as it is.`
          : `Already as diversified as its benchmark: concentration (HHI) ${benchmarkWords(d)}. The scenario leaves it as it is.`,
      ];
    case NO_BENCHMARK:
      return [
        `No benchmark: its customs heading has no other line and its dictionary sector no other supply-chain step to compare it with, so the scenario leaves it as it is.${isNum(d.currentHhi) ? ` Its concentration (HHI) is ${fmtScore(d.currentHhi)}.` : ''}`,
      ];
    case WITHIN_CAP:
      return [
        `More concentrated than its benchmark worldwide: concentration (HHI) ${benchmarkWords(d)}.`,
        `But no supplier to the allied market is above the ${isNum(d.cap) ? `${fmtShare(d.cap, 0)} ` : ''}cap that would bring it down to the benchmark, so nothing has to shift and the scenario does not run.`,
      ];
    case DIVERSIFIED:
      break;
    default:
      return [`Status in the scenario: ${d.status}. Concentration (HHI) ${benchmarkWords(d)}.`];
  }
  const out = [`More concentrated than its benchmark: concentration (HHI) ${benchmarkWords(d)}.`];
  const cap = isNum(d.cap)
    ? `The supplier-share cap that would bring it down to the benchmark is ${fmtShare(d.cap, 0)}, applied to the allied market.`
    : 'A supplier-share cap that would bring it down to the benchmark applies to the allied market.';
  const shift = `${fmtShare(d.excess)} of allied demand${isNum(r.shiftUsd) ? ` (${fmtUsd(r.shiftUsd)})` : ''} has to shift`;
  const one = d.capped.length === 1;
  out.push(
    d.capped.length
      ? `${cap} ${namesOf(d.capped, names)} ${one ? 'is' : 'are'} above it and lose${one ? 's' : ''} the excess: ${shift}.`
      : `${cap} ${shift[0].toUpperCase()}${shift.slice(1)}.`,
  );
  const gap =
    isNum(d.residual) && d.residual > 1e-9
      ? `new capacity has to fill the remaining ${fmtShare(d.residual)}${isNum(r.gapUsd) ? ` (${fmtUsd(r.gapUsd)})` : ''}`
      : 'trade under the cap replaces all of it, so no new capacity is needed';
  if (opts.coverage !== false) out.push(`With the cap holding, existing relationships bring coverage to ${fmtShare(d.existing)} and coordinated trade to ${fmtShare(d.optimized)}; ${gap}.`);
  const extra = [
    d.cls ? `What it needs under the scenario: ${diversifyClassText(d.cls, r.clsWarning)}.` : null,
    d.hosts.length ? `Best-placed countries to add the new capacity: ${namesOf(d.hosts, names)}.` : null,
    relationshipsOf(d.partners).length ? `Largest new or expanded relationships: ${relationshipWords(d.partners, names)}.` : null,
  ].filter((x): x is string => x !== null);
  if (extra.length) out.push(extra.join(' '));
  return out;
}

/** A row of the table and the CSV: plain values, names resolved; the run columns empty where the scenario did not run. */
export function diversifyCsvRow(r: DiversifyRow, names: Readonly<Record<string, string>>, tx: Taxonomy): Record<string, unknown> {
  const { node: n, d } = r;
  const place = osiFields(tx, n);
  return {
    node_id: n.id,
    node: n.label,
    technologies: place.technologies,
    stage: place.stages,
    aib_model_stage: place.modelStage,
    status: d.status,
    within_tolerance_of_benchmark: r.nearBenchmark,
    current_hhi: d.currentHhi,
    benchmark_hhi: d.benchmarkHhi,
    benchmark_basis: d.basis,
    supplier_cap: d.cap,
    capped_suppliers: d.capped.map((x) => nameOf(names, x)).join('; '),
    capped_iso3: d.capped.join('; '),
    allied_demand_usd: r.demandUsd,
    supply_to_shift_share: d.excess,
    supply_to_shift_usd: r.shiftUsd,
    coverage_under_cap_before_trade: d.static,
    coverage_after_existing_relationships: d.existing,
    coverage_after_optimized_trade: d.optimized,
    restored_by_trade_usd: r.tradeUsd,
    capacity_gap_share: d.residual,
    capacity_gap_usd: r.ran ? r.gapUsd : null,
    class_under_scenario: d.cls,
    class_under_scenario_warning: r.clsWarning,
    headline_class: n.cls,
    investment_hosts: d.hosts.map((x) => nameOf(names, x)).join('; '),
    largest_new_or_expanded_relationships: relationshipWords(d.partners, names),
    compact: n.compact,
    osi_cells: place.osiCells,
    dictionary_sector: place.dictionarySector,
  };
}
