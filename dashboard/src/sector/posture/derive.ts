// Pure derivations of the Sector strategy card (design section 5, 2026-10-29): every economy in one
// technology x six-stage node, as the canonical framework decides it (Need -> Prize -> Position -> Choice).
// Within one node the prize (the Strategic Industrial Value) barely varies across economies: its industrial
// commons and future option describe the node, and its market prize is the node's world market, lifted only
// where a national home market is measured. So the card plots Competitive Position
// (across) against the relevant need (up: supply-chain need at the producing stages, deployment need at
// Deployment & Use), marks each economy by its verb, and states the node's prize in words. No React here.

import { DEPLOYMENT_STAGE, stageDisplay } from '../../lib/stages.ts';
import type { PostureCell, PostureMeta, Verb } from '../../posture/contract.ts';
import { listAnd, plural, score100 } from '../../posture/derive.ts';
import { FENCE_LABEL, FENCE_OF_VERB, FENCE_ORDER, PRIZE_LABEL, VERB_FAMILY, VERB_LABEL, verbOf, type Fence } from '../../posture/grammar.ts';
import { nodePhrase } from '../../posture/strategy.ts';
import { mixedYards } from '../../posture/yards.ts';
import { AIB_SIDES } from '../../content/vocabulary.ts';
import { CONTROL_LAYER_ECONOMY_NOTE } from '../copy.ts';

/** The verbs in the order of the counts line, the legend and the takeaway. */
export const SECTOR_VERB_ORDER: readonly Verb[] = ['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch'];

/** The verbs a node's stage can take: Deployment & Use reads deployment need (Accelerate or Watch); Secure can override anywhere. */
export function verbsOfStage(stage: string): Verb[] {
  return stage === DEPLOYMENT_STAGE ? ['secure', 'accelerate', 'watch'] : ['secure', 'preserve', 'catch_up', 'partner', 'watch'];
}

/** One economy on the card's scatter. */
export interface StrategyPoint {
  iso3: string;
  name: string;
  verb: Verb;
  /** the relevant need, 0 to 1 (always measured: an economy without it is not plotted) */
  need: number;
  /** Competitive Position percentile among the node's contenders; null: not measured (drawn in the left strip) */
  cp: number | null;
  prize: number | null;
  cell: PostureCell;
}

/** The economies the scatter places: assessed, with a verb and a measured need (Competitive Position may be missing). */
export function strategyPoints(cells: readonly PostureCell[], names: Record<string, string>): StrategyPoint[] {
  const out: StrategyPoint[] = [];
  for (const c of cells) {
    const verb = verbOf(c);
    if (c.posture === 'N' || verb === null || c.need === null) continue;
    out.push({ iso3: c.iso3, name: names[c.iso3] ?? c.iso3, verb, need: c.need, cp: c.cp, prize: c.prize, cell: c });
  }
  // Secure last, so its outline is drawn on top; then weaker positions first
  return out.sort((a, b) => (a.verb === 'secure' ? 1 : 0) - (b.verb === 'secure' ? 1 : 0) || (a.cp ?? -1) - (b.cp ?? -1) || a.name.localeCompare(b.name));
}

/** Why an economy of the node is not on the scatter (null: it is). */
export function notPlottedReason(c: PostureCell | null): string | null {
  if (!c) return 'no strategic choice in the data';
  if (c.posture === 'N') return 'not assessed (need or prize not measured)';
  if (c.need === null) return 'need not measured';
  if (verbOf(c) === null) return 'no choice recorded';
  return null;
}

// ---- the node's prize -------------------------------------------------------------------------------

/** The prize of a node across its economies (null at Deployment & Use, which has none). */
export interface NodePrize {
  n: number;
  min: number;
  median: number;
  max: number;
  /** economies at or above the prize threshold */
  high: number;
  /** all high, all low, or the bar crossed by a home-market lift */
  kind: 'high' | 'low' | 'mixed';
}

export function nodePrize(cells: readonly PostureCell[], threshold: number): NodePrize | null {
  const v = cells.map((c) => c.prize).filter((x): x is number => x !== null && Number.isFinite(x)).sort((a, b) => a - b);
  if (!v.length) return null;
  const high = v.filter((x) => x >= threshold).length;
  return { n: v.length, min: v[0], median: v[Math.floor(v.length / 2)], max: v[v.length - 1], high, kind: high === v.length ? 'high' : high === 0 ? 'low' : 'mixed' };
}

/**
 * "Prize (Strategic Industrial Value) in this node: 85 for every economy, above the bar of 60." and its variants
 * (0-100 scale, as the Need ranking). `label` names the prize as the core does (prizeLabel: Industrial
 * Opportunity for a core whose prize is the market measure).
 */
export function prizeWords(stage: string, p: NodePrize | null, threshold: number, label: string = PRIZE_LABEL): string {
  if (stage === DEPLOYMENT_STAGE) {
    return `Deployment & Use has no prize of its own: the prize (${label}) is read at the stages that make or supply the technology.`;
  }
  if (!p) return `The prize (${label}) is not measured at this step.`;
  const bar = score100(threshold);
  const side = (x: number) => (x >= threshold ? 'at or above' : 'below');
  if (score100(p.min) === score100(p.max)) return `Prize (${label}) at this step: ${score100(p.median)} for every economy, ${side(p.median)} the bar of ${bar}.`;
  const range = `${score100(p.min)}–${score100(p.max)}`;
  if (p.kind === 'mixed') {
    return `Prize (${label}) at this step: ${score100(p.median)} for most economies (${range} where a national home market lifts it); ${p.high} of ${p.n} at or above the bar of ${bar}.`;
  }
  return `Prize (${label}) at this step: ${score100(p.median)} for most economies (${range} where a national home market lifts it), ${p.kind === 'high' ? 'at or above' : 'below'} the bar of ${bar} for all.`;
}

/** How the framework decides this node, in words (spec VIII), at the core's thresholds. */
export function decisionWords(stage: string, p: NodePrize | null, t: PostureMeta['thresholds']): string {
  if (stage === DEPLOYMENT_STAGE) {
    return `At Deployment & Use the choice reads deployment need only: at or above ${score100(t.deployment)}, Accelerate; below it, Watch. Accelerate is a deployment priority, not a call to manufacture the equipment at home; Competitive Position is context here.`;
  }
  const high = `where the prize is high, Competitive Position decides: the top quarter of this step's main producers (at or above ${score100(t.cp)}) Preserve; below it, a credible pathway (a capability anchor and a corroborating signal) means Catch Up, otherwise high need (at or above ${score100(t.need)}) means Partner and low need Watch`;
  const low = `where the prize is low, need decides: high need means Partner (a strong position is kept as Preserve and flagged for review), low need Watch`;
  const lead = !p ? `${cap(high)}; ${low}.` : p.kind === 'high' ? `${cap(high)}.` : p.kind === 'low' ? `${cap(low)}.` : `${cap(high)}; ${low}.`;
  return `${lead} Secure overrides all of these: a Secure economy keeps its true place on the chart.`;
}

const cap = (s: string) => s.charAt(0).toUpperCase() + s.slice(1);

/** A light label of the chart's plane (spec XIII.1 style): a tendency, never a rule; x is Competitive Position, y the need, both 0 to 1. */
export interface RegionLabel {
  label: string;
  x: number;
  y: number;
}

/**
 * Where the choices tend to fall on the Competitive Position x need plane of one node. Deployment & Use:
 * Accelerate above the deployment threshold, Watch below. A producing node with a high prize: Preserve right
 * of the Preserve bar; left of it Catch Up where a pathway is credible, else Partner (high need) or Watch.
 * A low prize: Partner above the need line (a strong position: Preserve, for review), Watch below. A node
 * whose prize crosses the bar for some economies gets no region labels (the marks tell).
 */
export function regionLabels(stage: string, kind: NodePrize['kind'] | null, t: PostureMeta['thresholds']): RegionLabel[] {
  if (stage === DEPLOYMENT_STAGE) {
    return [
      { label: 'ACCELERATE', x: 0.5, y: (t.deployment + 1) / 2 },
      { label: 'WATCH', x: 0.5, y: t.deployment / 2 },
    ];
  }
  if (kind === 'high') {
    return [
      { label: 'PRESERVE', x: (t.cp + 1) / 2, y: 0.5 },
      { label: 'CATCH UP OR PARTNER', x: t.cp / 2, y: (t.need + 1) / 2 },
      { label: 'CATCH UP OR WATCH', x: t.cp / 2, y: t.need / 2 },
    ];
  }
  if (kind === 'low') {
    return [
      { label: 'PARTNER', x: t.cp / 2, y: (t.need + 1) / 2 },
      { label: 'PRESERVE · REVIEW', x: (t.cp + 1) / 2, y: (t.need + 1) / 2 },
      { label: 'WATCH', x: 0.5, y: t.need / 2 },
    ];
  }
  return [];
}

// ---- the takeaway ---------------------------------------------------------------------------------

const desc = (v: number | null) => (v === null ? -Infinity : v);
/** Who leads each group in the takeaway. */
const RANK: Record<Verb, (a: PostureCell, b: PostureCell) => number> = {
  secure: (a, b) => desc(b.need) - desc(a.need),
  preserve: (a, b) => desc(b.cp) - desc(a.cp) || desc(b.share) - desc(a.share),
  catch_up: (a, b) => desc(b.prize) - desc(a.prize) || desc(b.cp) - desc(a.cp),
  partner: (a, b) => desc(b.need) - desc(a.need),
  accelerate: (a, b) => desc(b.need) - desc(a.need),
  watch: (a, b) => desc(b.need) - desc(a.need),
};

/** What each group of economies should do, as the takeaway says it ("should" agrees with one or many). */
// plain actions (vocabulary CHOICE_PROSE: "rely on partners for", "deploy faster", "leave open"), never the bare name in prose
const CLAUSE: Record<Verb, (stage: string) => string> = {
  secure: () => 'should secure it',
  preserve: () => 'should preserve a strong position',
  catch_up: () => 'should catch up from a credible base',
  partner: () => 'should rely on trusted partners for it',
  accelerate: () => 'should deploy faster',
  // Watch at Deployment & Use: the need is below the bar for acceleration (vocabulary WATCH_DEPLOYMENT_DEFINITION), never "on track"
  watch: (stage) => (stage === DEPLOYMENT_STAGE ? 'should watch rather than accelerate deployment (need below the bar for acceleration)' : 'should leave it open (low domestic priority)'),
};

const MAX_NAMED = 3;

/** "Aland", "Aland and Bland", "led by Aland, Bland and Cland" (a group of more than three). */
export function ledBy(names: readonly string[], total: number): string {
  const shown = names.slice(0, MAX_NAMED);
  return total > shown.length ? `led by ${listAnd(shown)}` : listAnd(shown);
}

/**
 * The card's takeaway: what the economies of one node should do, the six verbs in order, each group with its
 * leading economies named ("led by" when the group has more than three; Watch is counted, not named), then the
 * economies not assessed. Node names come from the core's phrase table ("battery materials refining").
 */
export function sectorStrategyTakeaway(tech: string, stage: string, cells: readonly PostureCell[], names: Record<string, string>, meta: Pick<PostureMeta, 'phrases'>): string {
  const phrase = nodePhrase(meta, tech, stage);
  const where = stage === DEPLOYMENT_STAGE && !meta.phrases[`${tech}|${stage}`] ? `${phrase} deployment` : phrase;
  if (!cells.length) return `No economy has a strategic choice in ${where}.`;
  const clauses: string[] = [];
  for (const verb of SECTOR_VERB_ORDER) {
    const group = cells.filter((c) => verbOf(c) === verb).sort(RANK[verb]);
    if (!group.length) continue;
    const head = `${plural(group.length, 'economy', 'economies')} ${CLAUSE[verb](stage)}`;
    clauses.push(verb === 'watch' ? head : `${head} (${ledBy(group.map((c) => names[c.iso3] ?? c.iso3), group.length)})`);
  }
  const na = cells.filter((c) => c.posture === 'N').length;
  if (na) clauses.push(`${plural(na, 'economy', 'economies')} ${na === 1 ? 'is' : 'are'} not assessed`);
  if (!clauses.length) return `No economy has a recorded choice in ${where}.`;
  const body = clauses.length === 1 ? clauses[0] : `${clauses.slice(0, -1).join('; ')}; and ${clauses[clauses.length - 1]}`;
  return `In ${where}, ${body}.`;
}

/**
 * The two sides of the allied division of labor at one producing step (vocabulary AIB_SIDES), from the
 * node's own cells: "Who has the strongest capability?" names the three highest Competitive Positions;
 * "Which countries need a partner?" names the economies whose choice is Partner ("led by" past three).
 * null at Deployment & Use (Competitive Position is context there and Partner is not a choice) or when no
 * economy has a measured position.
 */
export function alliedSides(stage: string, cells: readonly PostureCell[], names: Record<string, string>): string | null {
  if (stage === DEPLOYMENT_STAGE) return null;
  const nm = (c: PostureCell) => names[c.iso3] ?? c.iso3;
  const strong = cells.filter((c) => c.posture !== 'N' && c.cp !== null).sort((a, b) => desc(b.cp) - desc(a.cp) || nm(a).localeCompare(nm(b)));
  if (!strong.length) return null;
  const partners = cells.filter((c) => verbOf(c) === 'partner').sort(RANK.partner);
  const demand = partners.length ? `${ledBy(partners.map(nm), partners.length)} (their choice is Partner)` : 'none here: no economy has Partner as its choice';
  return `${AIB_SIDES.supply.question} ${listAnd(strong.slice(0, MAX_NAMED).map(nm))} (the highest Competitive Positions). ${AIB_SIDES.demand.question} ${cap(demand)}.`;
}

/**
 * The counts line's grouping: the economies of one step by fence (FENCE_OF_VERB; the legend's grouping), then
 * the economies not assessed: "1 High fence · 3 Medium fence · 2 Low fence". It replaces the engine's
 * Secure / Compete / Deploy families on the Sector card (copy refresh 2026-10-30): the Sector pages explain
 * the choices by fence, and the Deploy family put a producing-stage Watch beside Accelerate.
 */
export function fenceCountsText(cells: readonly PostureCell[]): string {
  const n: Record<Fence, number> = { high: 0, medium: 0, low: 0 };
  for (const c of cells) {
    const v = verbOf(c);
    if (v) n[FENCE_OF_VERB[v]]++;
  }
  return FENCE_ORDER.map((f) => `${n[f]} ${FENCE_LABEL[f]}`).join(' · ');
}

/**
 * The note for the economies whose step is mixed (their layers sit in a yard other than the step's own,
 * posture/yards.ts mixedYards): the mark's colour is the step's choice, not every layer's (vocabulary
 * MIXED_YARD_NOTE, CONTROL_LAYER_NOTE). Null when no economy of the step is mixed.
 */
export function mixedLayersNote(cells: readonly PostureCell[], names: Record<string, string>): string | null {
  const mixed = cells.filter((c) => mixedYards(c) !== null);
  if (!mixed.length) return null;
  const all = mixed.map((c) => names[c.iso3] ?? c.iso3).sort((a, b) => a.localeCompare(b));
  const who = all.length > MAX_NAMED ? `${all.slice(0, MAX_NAMED).join(', ')} and ${all.length - MAX_NAMED} more` : listAnd(all);
  return `Layers in another yard: for ${plural(mixed.length, 'economy', 'economies')} (${who}), a layer of this step, such as control software or a critical input, sits in a different yard from the step as a whole. A mark's colour is the step's own choice; the economy's Country Strategy Map shows each layer. ${CONTROL_LAYER_ECONOMY_NOTE}`;
}

/** The card's title: "Every economy's choice in Batteries — Refining & Processing" (the stage's display name). */
export const sectorStrategyTitle = (tech: string, stage: string): string => `Every economy's choice in ${tech} — ${stageDisplay(stage)}`;

/** The economy's Country Strategy Map, opened on this node (design D8: ?node=Tech|Stage). */
export function strategyHref(iso3: string, tech: string, stage: string): string {
  const q = new URLSearchParams({ node: `${tech}|${stage}` });
  return `/countries/${iso3}?${q.toString()}#strategy`;
}

/** The verb of a cell in the card's words ("Catch Up"; "Not assessed"). */
export const verbWord = (c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode'>): string => {
  const v = verbOf(c);
  return v ? VERB_LABEL[v] : 'Not assessed';
};

/** The family colour key of a verb (S, C or D). */
export const familyOf = (v: Verb) => VERB_FAMILY[v];

/** The rank of a row in the table twin: the six verbs in order, then not assessed; within one, the strongest position first. */
export function verbRank(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode'> | null): number {
  const v = c ? verbOf(c) : null;
  return v ? SECTOR_VERB_ORDER.indexOf(v) : SECTOR_VERB_ORDER.length;
}
