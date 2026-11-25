// The Sector pages' answers first (usability pass, 2026-11-25), in plain words, from what the pages already
// show and no new measure:
//   * chainAnswer / chainAnswerText: "What matters in this supply chain?" on a technology's chain page, read
//     from the observed supplier-share rows of its concentration chart (the chain ladder): the largest
//     holder at each stage, the economies that lead most rows, the single most concentrated row, and what
//     the evidence leaves out (stages without comparable country-level evidence; stages measured by export
//     value only). No row, no claim: the text says the evidence is insufficient instead.
//   * strongestSentence: "Who is strongest today?" on a stage profile, the three largest holders of the
//     measure the page already leads with, naming the measure and the year.
//   * affordabilityEvidence: which cost and deployment measures the Explorer holds for a technology
//     (technology cost such as levelised cost, the world prices of its input commodities, deployment
//     measures at its Deployment stage) and which are licensed and not shown. It lists evidence; it never
//     grades affordability.
// Pure: no React, no IO.

import { STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, type StageSlug, type TechSlug } from '../contract.ts';
import type { ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import { fmtShare, listText } from '../format.ts';

const cap = (s: string): string => (s ? s.charAt(0).toUpperCase() + s.slice(1) : s);
const num = (x: number | null | undefined): number => (typeof x === 'number' && Number.isFinite(x) ? x : -1);

// ---- the technology chain page ------------------------------------------------------------------

/** One observed supplier-share row of the chain (a row of the concentration chart). */
export interface ChainLine {
  stage: StageSlug;
  /** the row in running text: "refining exports", "lithium refining" */
  words: string;
  /** the largest holder in running text ("the United States"), any holder kind; null when unknown */
  top1Name: string | null;
  top1Share: number | null;
  /** is the largest holder an economy or the EU (a region or a company group is not counted as a country position) */
  top1Economy: boolean;
  year: number;
  /** the measure's family ("exports", "production", ...) */
  family: string;
  /** holders with a value above zero */
  holders: number;
}

/**
 * A row with fewer holders than this is too thin to compare (one or two reporters make any share look like
 * control): the answer leaves it out of its claims and names it under the evidence limits. A display rule
 * of these summary sentences only; the chart still shows the row.
 */
export const MIN_HOLDERS = 3;

export interface ChainAnswer {
  /** every row with a known largest holder and at least MIN_HOLDERS holders */
  nLines: number;
  /** each stage with rows, in chain order, with its most concentrated row */
  byStage: { stage: StageSlug; line: ChainLine }[];
  /** economies that are the largest holder of at least one row, most rows first */
  strongest: { name: string; n: number }[];
  /** the row with the largest single-holder share */
  tightest: ChainLine | null;
  /** stages without comparable country-level evidence (as the stage grid says) */
  without: StageSlug[];
  /** stages with rows whose rows are all export value */
  exportsOnly: StageSlug[];
  /** rows left out because fewer than MIN_HOLDERS economies have a value */
  thin: ChainLine[];
}

/** A holder without its source tag: "China (IEA)" -> "China" (for counting who leads; the rows keep their label). */
export const holderKey = (name: string): string => name.replace(/\s*\((?:IEA|[A-Z]{2,8})\)$/, '');

/** The chain answer of a technology from its observed rows and its stages without comparable evidence. */
export function chainAnswer(lines: readonly ChainLine[], without: readonly StageSlug[]): ChainAnswer {
  const named = lines.filter((l) => l.top1Name !== null && num(l.top1Share) >= 0);
  const thin = named.filter((l) => l.holders < MIN_HOLDERS);
  const known = named.filter((l) => l.holders >= MIN_HOLDERS);
  const byStage: ChainAnswer['byStage'] = [];
  const exportsOnly: StageSlug[] = [];
  for (const s of STAGE_SLUGS) {
    const here = known.filter((l) => l.stage === s);
    if (!here.length) continue;
    const best = [...here].sort((a, z) => num(z.top1Share) - num(a.top1Share) || a.words.localeCompare(z.words, 'en'))[0];
    byStage.push({ stage: s, line: best });
    if (here.every((l) => l.family === 'exports')) exportsOnly.push(s);
  }
  // one economy, one count: a source-tagged holder ("China (IEA)", the IEA's reported capacity) counts as that economy
  const counts = new Map<string, number>();
  for (const l of known) if (l.top1Economy && l.top1Name) { const k = holderKey(l.top1Name); counts.set(k, (counts.get(k) ?? 0) + 1); }
  const strongest = [...counts.entries()].map(([name, n]) => ({ name, n })).sort((a, z) => z.n - a.n || a.name.localeCompare(z.name, 'en'));
  const tightest = byStage.length ? [...byStage].map((b) => b.line).sort((a, z) => num(z.top1Share) - num(a.top1Share))[0] : null;
  return { nLines: known.length, byStage, strongest, tightest, without: [...without], exportsOnly, thin };
}

/** "China holds 78% of refining exports (2024)" */
export const lineText = (l: ChainLine): string => `${l.top1Name} holds ${fmtShare(l.top1Share, 0)} of ${l.words} (${l.year})`;

export interface ChainAnswerText {
  /** null when no stage has a comparable measure (the page says the evidence is insufficient instead) */
  concentration: { stage: StageSlug; text: string }[] | null;
  strongest: string | null;
  implication: string | null;
  limits: string;
  /** set when there is nothing to conclude from */
  insufficient: string | null;
}

/** The chain answer in words (one sentence per row of the answer block). */
export function chainAnswerText(a: ChainAnswer, tech: TechSlug): ChainAnswerText {
  const label = TECH_LABELS[tech];
  const limits: string[] = [];
  if (a.without.length) limits.push(`No comparable country-level evidence for ${listText(a.without.map((s) => STAGE_LABELS[s]))}.`);
  if (a.thin.length) {
    const ws = a.thin.map((l) => l.words);
    const one = ws.length === 1;
    limits.push(`${cap(listText(ws))} ${one ? 'has' : 'have'} values for fewer than ${MIN_HOLDERS} economies and ${one ? 'is' : 'are'} left out of this summary.`);
  }
  if (a.exportsOnly.length) limits.push(`${listText(a.exportsOnly.map((s) => STAGE_LABELS[s]))} ${a.exportsOnly.length === 1 ? 'is' : 'are'} measured by export value only, not production.`);
  if (!a.nLines) {
    return {
      concentration: null,
      strongest: null,
      implication: null,
      limits: limits.join(' ') || 'No stage has a country-level measure with a world total.',
      insufficient: a.thin.length
        ? `Insufficient evidence: the ${label} measures have values for too few economies to say where supply is concentrated.`
        : `Insufficient evidence: no stage of ${label} has a country-level measure with a world total, so where supply is concentrated cannot be said.`,
    };
  }
  if (!limits.length) limits.push('Every stage has country-level evidence; the rows differ in source and year.');
  const [first, ...rest] = a.strongest.slice(0, 3);
  const strongest =
    a.nLines === 1
      ? `${first ? cap(first.name) : 'No single economy'} is the largest holder of the one measure shown.`
      : first
        ? `${cap(first.name)} is the largest holder in ${first.n} of the ${a.nLines} measures shown${rest.length ? `; ${listText(rest.map((r) => `${r.name} in ${r.n}`))}` : ''}.`
        : `No single economy is the largest holder of any of the ${a.nLines} measures shown.`;
  const t = a.tightest;
  return {
    concentration: a.byStage.map((b) => ({ stage: b.stage, text: `${cap(lineText(b.line))}.` })),
    strongest,
    // one measure has nothing to stand out from
    implication: t && a.nLines > 1 ? `The most concentrated measure: ${lineText(t)}. Concentration shows who holds the most today, not whether it could be replaced.` : null,
    limits: limits.join(' '),
    insufficient: null,
  };
}

/** The measure families that describe who supplies a market (trade, production, capacity). */
export const SUPPLY_FAMILIES: ReadonlySet<string> = new Set(['exports', 'production', 'capacity']);

/**
 * Whether the plain concentration line ("Not concentrated: supply is spread across many suppliers") may be
 * said of a measure: a supply measure with at least MIN_HOLDERS holders. A resource potential, an investment
 * total or a measure one or two economies report says nothing about how spread out supply is, so a low index
 * there must not read as low risk (the index itself is still shown).
 */
export function bandApplies(v: Pick<VarInfo, 'family'>, c: { n: number }): boolean {
  return SUPPLY_FAMILIES.has(v.family) && c.n >= MIN_HOLDERS;
}

// ---- the stage profile --------------------------------------------------------------------------

/** Said when a stage has no comparable country-level measure. */
export const NO_STAGE_MEASURE = 'No comparable country-level measure for this stage.';

/**
 * "Who is strongest today?": the largest holders (up to three, in running text) of the page's primary
 * measure, naming the measure and the year. `shares` is false for a measure without a world total (or one
 * that is itself a share), where the holders have the highest values, not the largest shares. With fewer
 * than MIN_HOLDERS holders (`holders`) the sentence says it is not a comparison.
 */
export function strongestSentence(names: readonly string[], what: string, year: number | null, shares: boolean, holders?: number): string {
  const top = names.slice(0, 3);
  if (!top.length || year === null || !what) return NO_STAGE_MEASURE;
  const one = top.length === 1;
  const verb = shares ? (one ? 'holds the largest share of' : 'hold the largest shares of') : one ? 'has the highest' : 'have the highest';
  const few = holders !== undefined && holders < MIN_HOLDERS ? ` Only ${holders === 1 ? 'one economy has' : `${holders} economies have`} a value, so this is not a comparison.` : '';
  return `${cap(listText(top))} ${verb} ${what} (${year}).${few}`;
}


// ---- affordability and deployment ---------------------------------------------------------------

/** One measure the Explorer holds, with where it opens in the Advanced explorer. */
export interface EvidenceRef {
  v: VarInfo;
  /** the item the measure is read for (the cell, or the linked fuel or commodity) */
  item: ItemInfo;
  stage: StageSlug;
  /** the latest observed year */
  year: number | null;
  /** the last projection year, when the measure carries projections */
  projectionTo: number | null;
  /** economies with a value (0 for a world-only measure such as a world price) */
  nGeo: number;
}

export interface AffordabilityEvidence {
  /** technology cost measures (levelised cost), any stage */
  cost: EvidenceRef[];
  /** world prices of the technology's input commodities (the first, plus every commodity, by link weight) */
  inputs: { ref: EvidenceRef; items: ItemInfo[] } | null;
  /** deployment measures at the Deployment stage, in DEPLOYMENT_VARS order (at most three) */
  deployment: EvidenceRef[];
  /** licensed sources the coverage table names for the technology (not shown in the public Explorer) */
  licensed: string[];
}

/** The deployment measures read at a technology's Deployment stage, in priority order. */
export const DEPLOYMENT_VARS = ['investment.power_additions', 'consumption.ev_sales', 'consumption.ev_stock', 'consumption.total_ej', 'consumption.weo', 'investment.annual'] as const;
const PRICE_VAR = 'prices.latest';

function refOf(model: SectorModel, v: VarInfo, item: ItemInfo, stage: StageSlug): EvidenceRef | null {
  const av = model.availOf(v, item.idx);
  if (!av.length) return null;
  const max = Math.max(...av.map((a) => a.year_max ?? -Infinity));
  const min = Math.min(...av.map((a) => a.year_min ?? Infinity));
  const proj = v.projection_from;
  // the latest observed year: the measure's own years in the item's range, before any projection
  const ys = (v.years ?? []).filter((y) => y >= min && y <= max && (proj === null || y < proj));
  const obs = ys.length ? Math.max(...ys) : v.years?.length ? -Infinity : proj !== null ? Math.min(max, proj - 1) : max;
  return { v, item, stage, year: Number.isFinite(obs) && obs >= min ? obs : null, projectionTo: proj !== null && max >= proj ? max : null, nGeo: Math.max(0, ...av.map((a) => a.n_geo ?? 0)) };
}

/** The items a measure is read for at a cell: the cell itself, or its linked fuels or commodities with data (largest link weight first). */
function itemsFor(model: SectorModel, v: VarInfo, cell: ItemInfo): ItemInfo[] {
  if (v.item_axis === 'cell') return model.availOf(v, cell.idx).length ? [cell] : [];
  return model
    .itemsOfCell(cell.idx, v.item_axis)
    .filter((l) => model.availOf(v, l.item.idx).length > 0)
    .sort((a, z) => z.weight - a.weight || a.item.sort - z.item.sort)
    .map((l) => l.item);
}

/** The cost and deployment evidence the Explorer holds for a technology (lists measures; grades nothing). */
export function affordabilityEvidence(model: SectorModel, tech: TechSlug): AffordabilityEvidence {
  const cells = STAGE_SLUGS.map((s) => ({ s, cell: model.cell(tech, s) })).filter((x): x is { s: StageSlug; cell: ItemInfo } => !!x.cell);
  const cost: EvidenceRef[] = [];
  for (const v of model.vars) {
    if (v.unit.unit_kind !== 'lcoe' || v.licence === 'licensed') continue;
    for (const { s, cell } of cells) {
      const it = itemsFor(model, v, cell)[0];
      const r = it ? refOf(model, v, it, s) : null;
      if (r) cost.push(r);
    }
  }
  let inputs: AffordabilityEvidence['inputs'] = null;
  const pv = model.varById.get(PRICE_VAR);
  if (pv) {
    const seen = new Map<number, { item: ItemInfo; stage: StageSlug }>();
    for (const { s, cell } of cells) for (const it of itemsFor(model, pv, cell)) if (!seen.has(it.idx)) seen.set(it.idx, { item: it, stage: s });
    const first = [...seen.values()][0];
    const ref = first ? refOf(model, pv, first.item, first.stage) : null;
    if (ref) inputs = { ref, items: [...seen.values()].map((x) => x.item) };
  }
  const deployment: EvidenceRef[] = [];
  const dep = model.cell(tech, 'deployment');
  if (dep) {
    for (const id of DEPLOYMENT_VARS) {
      const v = model.varById.get(id);
      if (!v || v.licence === 'licensed') continue;
      const it = itemsFor(model, v, dep)[0];
      const r = it ? refOf(model, v, it, 'deployment') : null;
      if (r) deployment.push(r);
      if (deployment.length >= 3) break;
    }
  }
  const licensed = new Set<string>();
  for (const { cell } of cells) for (const c of model.coverageOf(cell.idx)) if (c.status === 'licensed') for (const src of c.sources.length ? c.sources : ['licensed data']) licensed.add(src);
  return { cost, inputs, deployment, licensed: [...licensed] };
}

/** "2024", "2024; projections to 2035", or "" for a measure without an observed year. */
export function yearWords(r: Pick<EvidenceRef, 'year' | 'projectionTo'>): string {
  const y = r.year !== null ? String(r.year) : '';
  return r.projectionTo !== null ? `${y}${y ? '; ' : ''}projections to ${r.projectionTo}` : y;
}

/** Said when a technology has no technology-cost measure in the Explorer. */
export const NO_COST = 'No technology cost evidence in the Explorer for this technology.';
/** Said when a technology has no deployment measure in the Explorer. */
export const NO_DEPLOYMENT = 'No deployment measure in the Explorer for this technology.';
/** A licensed source named, never shown. */
export const licensedWords = (sources: readonly string[]): string => `${listText([...sources])} ${sources.length === 1 ? 'is' : 'are'} licensed and not shown in the public Explorer.`;
