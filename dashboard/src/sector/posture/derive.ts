// Pure derivations of the Sector strategy card (design section 5, 2026-10-29): every economy in one
// technology x six-stage node, as the canonical framework decides it (Need -> Prize -> Position -> Choice).
// Within one node the prize (Industrial Opportunity) barely varies across economies: it is the node's world
// market prize, lifted only where a national home market is measured. So the card plots Competitive Position
// (across) against the relevant need (up: supply-chain need at the producing stages, deployment need at
// Deployment & Use), marks each economy by its verb, and states the node's prize in words. No React here.

import { DEPLOYMENT_STAGE, stageDisplay } from '../../lib/stages.ts';
import type { PostureCell, PostureMeta, Verb } from '../../posture/contract.ts';
import { listAnd, plural, score100 } from '../../posture/derive.ts';
import { VERB_FAMILY, VERB_LABEL, verbOf } from '../../posture/grammar.ts';
import { nodePhrase } from '../../posture/strategy.ts';

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
  if (!c) return 'not classified';
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

/** "Prize (Industrial Opportunity) in this node: 85 for every economy, above the bar of 60." and its variants (0-100 scale, as the Need ranking). */
export function prizeWords(stage: string, p: NodePrize | null, threshold: number): string {
  if (stage === DEPLOYMENT_STAGE) {
    return 'Deployment & Use has no prize of its own: Industrial Opportunity is read at the stages that make or supply the technology.';
  }
  if (!p) return 'The prize (Industrial Opportunity) is not measured in this node.';
  const bar = score100(threshold);
  const side = (x: number) => (x >= threshold ? 'at or above' : 'below');
  if (score100(p.min) === score100(p.max)) return `Prize (Industrial Opportunity) in this node: ${score100(p.median)} for every economy, ${side(p.median)} the bar of ${bar}.`;
  const range = `${score100(p.min)}–${score100(p.max)}`;
  if (p.kind === 'mixed') {
    return `Prize (Industrial Opportunity) in this node: ${score100(p.median)} for most economies (${range} where a national home market lifts it); ${p.high} of ${p.n} at or above the bar of ${bar}.`;
  }
  return `Prize (Industrial Opportunity) in this node: ${score100(p.median)} for most economies (${range} where a national home market lifts it), ${p.kind === 'high' ? 'at or above' : 'below'} the bar of ${bar} for all.`;
}

/** How the framework decides this node, in words (spec VIII), at the core's thresholds. */
export function decisionWords(stage: string, p: NodePrize | null, t: PostureMeta['thresholds']): string {
  if (stage === DEPLOYMENT_STAGE) {
    return `At Deployment & Use the choice reads deployment need only: at or above ${score100(t.deployment)}, Accelerate; below it, Watch. Accelerate is a deployment priority, not a call to manufacture the equipment at home; Competitive Position is context here.`;
  }
  const high = `where the prize is high, Competitive Position decides: the top quarter of the node's contenders (at or above ${score100(t.cp)}) Preserve; below it, a credible pathway means Catch Up, otherwise high need (at or above ${score100(t.need)}) means Partner and low need Watch`;
  const low = `where the prize is low, need decides: high need means Partner (a strong position is kept as Preserve and flagged for review), low need Watch`;
  const lead = !p ? `${cap(high)}; ${low}.` : p.kind === 'high' ? `${cap(high)}.` : p.kind === 'low' ? `${cap(low)}.` : `${cap(high)}; ${low}.`;
  return `${lead} Secure overrides the triage: a Secure economy keeps its true place on the chart.`;
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
const CLAUSE: Record<Verb, (stage: string) => string> = {
  secure: () => 'must secure it',
  preserve: () => 'should preserve a strong position',
  catch_up: () => 'should catch up, with the prize and a credible pathway',
  partner: () => 'should partner, needing it without a domestic case',
  accelerate: () => 'should accelerate deployment',
  watch: (stage) => (stage === DEPLOYMENT_STAGE ? 'should watch (no acceleration needed)' : 'should watch (domestic industrial priority low)'),
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
  if (!cells.length) return `No economy is classified in ${where}.`;
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
