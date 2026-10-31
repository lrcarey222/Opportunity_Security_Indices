// Pure rows of the Country page's sections (spec XII-XVI; design 5b): the Need rankings (supply-chain need
// at the producing stages, deployment need by technology), the Prize & Position points, the Strategic
// Choices groups (with the Watch reasons and the manufacturing-versus-deployment split), the Policy Alignment
// rows and the Allied Strategy rows with their gap logic. Each reads the core cells of one economy and, where
// a section shows components, that economy's detail document (null while it loads: the rows then carry the
// core values only). Nothing here invents a value: a missing input stays null and is shown as not assessed.

import { isProducingStage, stageDisplay, stageRank } from '../lib/stages.ts';
import type { AibGap, Alignment, DivisionNode, PostureCell, PostureDetail, PostureThresholds, SecurityRationale, Verb } from './contract.ts';
import type { DeploymentNeed, DetailPartner, NodeDetail } from './detail.ts';
import {
  ALIGNMENT_ATTENTION, ALLIED_GAP_WORDS, DEPLOY_MODE_LABEL, GAP_ORDER, MODE_LABEL, PARTNER_GAP_REASON_ORDER, partnerGapReasonWords, RATIONALE_TAG, RATIONALE_WORDS, type NeedTag, VERB_LABEL,
  verbOf, watchReasonWords,
} from './grammar.ts';

/** A node's cell key "Tech|Stage". */
export const cellKeyOf = (c: { tech: string; stage: string }): string => `${c.tech}|${c.stage}`;
const GAPS_WORST_FIRST = GAP_ORDER.filter((g): g is Exclude<AibGap, 'none'> => g !== 'none');
const reasonRank = (r: string): number => {
  const i = (PARTNER_GAP_REASON_ORDER as readonly string[]).indexOf(r);
  return i < 0 ? PARTNER_GAP_REASON_ORDER.length : i;
};

/** The detail cells of one economy by cell key (empty while the document loads). */
export function detailByKey(detail: PostureDetail | null): Map<string, NodeDetail> {
  return new Map((detail?.cells ?? []).map((d) => [d.key, d]));
}

// ---- Need (spec XII) ---------------------------------------------------------------------------------------

export interface SupplyNeedRow {
  key: string;
  tech: string;
  stage: string;
  verb: Verb | null;
  need: number | null;
  rationale: SecurityRationale | null;
  tags: NeedTag[];
}

/**
 * Panel A, "Where is dependence a problem?": the producing-stage nodes ranked by Secure significance, then
 * supply-chain need (Secure nodes first, highest need first; nodes without a measured need and not Secure
 * are left out). Tags: the Secure rationale's (CHOKEPOINT, SYSTEMIC RESILIENCE, CONTROL LAYER) and HIGH
 * DEPENDENCE at or above the need threshold.
 */
export function supplyNeedRanking(cells: readonly PostureCell[], t: Pick<PostureThresholds, 'need'>): SupplyNeedRow[] {
  return cells
    .filter((c) => isProducingStage(c.stage) && (c.posture === 'S' || c.need !== null))
    .map((c) => {
      const tags: NeedTag[] = [];
      if (c.posture === 'S' && c.rationale) tags.push(RATIONALE_TAG[c.rationale]);
      if (c.need !== null && c.need >= t.need) tags.push('HIGH DEPENDENCE');
      return { key: cellKeyOf(c), tech: c.tech, stage: c.stage, verb: verbOf(c), need: c.need, rationale: c.rationale, tags };
    })
    .sort((a, b) => Number(b.verb === 'secure') - Number(a.verb === 'secure') || (b.need ?? -1) - (a.need ?? -1) || stageRank(a.stage) - stageRank(b.stage));
}

export interface DeploymentNeedRow {
  key: string;
  tech: string;
  verb: Verb | null;
  need: number | null;
  /** the three families with basis and scenario, from the detail document (null while it loads) */
  families: DeploymentNeed | null;
}

/** Panel B, "Where does the energy system need to move faster?": the Deployment & Use nodes by deployment need (not assessed last). */
export function deploymentNeedRanking(cells: readonly PostureCell[], detail: PostureDetail | null): DeploymentNeedRow[] {
  const d = detailByKey(detail);
  return cells
    .filter((c) => c.needType === 'deployment')
    .map((c) => ({ key: cellKeyOf(c), tech: c.tech, verb: verbOf(c), need: c.need, families: d.get(cellKeyOf(c))?.deployment ?? null }))
    .sort((a, b) => (b.need ?? -1) - (a.need ?? -1) || a.tech.localeCompare(b.tech));
}

// ---- Prize & Position (spec XIII) --------------------------------------------------------------------------

export interface PrizePoint {
  key: string;
  tech: string;
  stage: string;
  verb: Verb;
  prize: number;
  cp: number;
  need: number | null;
  cell: PostureCell;
}

/** The producing-stage nodes with both a prize and a position (Secure at its true coordinates), for one stage or all; the rest are listed, not plotted. */
export function prizePositionPoints(cells: readonly PostureCell[], stage: string | 'all'): { plotted: PrizePoint[]; notPlotted: PostureCell[] } {
  const scope = cells.filter((c) => isProducingStage(c.stage) && (stage === 'all' || c.stage === stage));
  const plotted: PrizePoint[] = [];
  const notPlotted: PostureCell[] = [];
  for (const c of scope) {
    const v = verbOf(c);
    if (v && c.prize !== null && c.cp !== null) plotted.push({ key: cellKeyOf(c), tech: c.tech, stage: c.stage, verb: v, prize: c.prize, cp: c.cp, need: c.need, cell: c });
    else notPlotted.push(c);
  }
  return { plotted, notPlotted };
}

/** The light interpretive quadrant of a point (spec XIII.1): tendencies, not rules. */
export type PrizeQuadrant = 'strong_strong' | 'weak_strong_prize' | 'weak_weak' | 'strong_weak_prize';
export function prizeQuadrant(p: { prize: number; cp: number }, t: Pick<PostureThresholds, 'prize' | 'cp'>): PrizeQuadrant {
  const hp = p.prize >= t.prize;
  const hc = p.cp >= t.cp;
  return hp ? (hc ? 'strong_strong' : 'weak_strong_prize') : hc ? 'strong_weak_prize' : 'weak_weak';
}
export const PRIZE_QUADRANT_WORDS: Record<PrizeQuadrant, [string, string]> = {
  strong_strong: ['Strong position + strong prize', 'typically Preserve'],
  weak_strong_prize: ['Weak position + strong prize', 'Catch Up, given a real base to build from'],
  weak_weak: ['Weak position + weak prize', 'Watch, unless high vulnerability makes it Partner'],
  strong_weak_prize: ['Strong position + weak prize', 'mature or specialized capability: review whether preserving is still justified'],
};

/**
 * A node whose Competitive Position score is in the top quarter but that is not Preserve: the engine's Preserve
 * test also asks the node to be a contender above the capability floor (R decision_engine.R cp_high), and this
 * one is not. With a high prize it then takes Catch Up, Partner or Watch.
 */
export const TOP_QUARTER_NOT_PRESERVE = 'a top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor)';

/**
 * The words of a point's place on the Prize & Position chart: its quadrant's tendency (spec XIII.1), except a
 * high-prize node with a top-quarter score that is Catch Up, Partner or Watch, which did not pass the Preserve
 * test: that is a fact about the node, not a tendency, and "typically Preserve" would contradict its choice.
 */
export function prizeTendency(p: { prize: number; cp: number; verb: Verb }, t: Pick<PostureThresholds, 'prize' | 'cp'>): { words: string; tendency: boolean } {
  const q = prizeQuadrant(p, t);
  if (q === 'strong_strong' && (p.verb === 'catch_up' || p.verb === 'partner' || p.verb === 'watch')) {
    return { words: TOP_QUARTER_NOT_PRESERVE[0].toUpperCase() + TOP_QUARTER_NOT_PRESERVE.slice(1), tendency: false };
  }
  return { words: PRIZE_QUADRANT_WORDS[q].join(': '), tendency: true };
}

// ---- Strategic Choices (spec XIV) ------------------------------------------------------------------------

export interface ChoiceGroups {
  secure: PostureCell[];
  preserve: PostureCell[];
  catch_up: PostureCell[];
  partner: PostureCell[];
  accelerate: PostureCell[];
  watch: PostureCell[];
  /** nodes with a consistency flag (design D2), whatever their choice */
  review: PostureCell[];
  notAssessed: PostureCell[];
}

const d1 = (v: number | null) => (v === null ? -Infinity : v);

/** The six groups, each in its reading order: Secure by need, Preserve by position, Catch Up by prize, Partner by need, Accelerate by deployment need, Watch in chain order. */
export function choiceGroups(cells: readonly PostureCell[]): ChoiceGroups {
  const of = (v: Verb) => cells.filter((c) => verbOf(c) === v);
  const chain = (a: PostureCell, b: PostureCell) => stageRank(a.stage) - stageRank(b.stage) || a.tech.localeCompare(b.tech);
  return {
    secure: of('secure').sort((a, b) => d1(b.need) - d1(a.need) || chain(a, b)),
    preserve: of('preserve').sort((a, b) => d1(b.cp) - d1(a.cp) || chain(a, b)),
    catch_up: of('catch_up').sort((a, b) => d1(b.prize) - d1(a.prize) || chain(a, b)),
    partner: of('partner').sort((a, b) => d1(b.need) - d1(a.need) || chain(a, b)),
    accelerate: of('accelerate').sort((a, b) => d1(b.need) - d1(a.need) || chain(a, b)),
    watch: of('watch').sort(chain),
    review: cells.filter((c) => c.flag !== null).slice().sort(chain),
    notAssessed: cells.filter((c) => c.posture === 'N').slice().sort(chain),
  };
}

/**
 * Why a Watch node is not a domestic industrial priority (spec XIV), in words. The engine decides the reasons
 * (R sp_watch_reasons: low need, low prize, ample diversified supply, weak domestic case, option value, low
 * deployment need) and the detail document writes them: those are shown, in the engine's order, never
 * re-derived here. Until the detail is loaded (or where it writes none), only what the core cell says for sure:
 * low need, low prize, or deployment need below the acceleration bar. Ample diversified supply and a weak
 * domestic case need the engine's inputs, so they are never guessed from the core.
 */
export function watchReasons(c: PostureCell, t: PostureThresholds, d: NodeDetail | null): string[] {
  if (d && d.watchReasons.length) return [...new Set(d.watchReasons)].map(watchReasonWords);
  const codes: string[] = [];
  if (c.needType === 'deployment') {
    if (c.need !== null && c.need < t.deployment) codes.push('low_deployment_need');
  } else {
    if (c.need !== null && c.need < t.need) codes.push('low_need');
    if (c.prize !== null && c.prize < t.prize) codes.push('low_prize');
  }
  return codes.map(watchReasonWords);
}

/**
 * The domestic manufacturing priority of a technology (spec XIV: deployment priority is separate from it):
 * the choices of its producing stages, e.g. "Watch at Extraction and Assembly; Catch Up at Components".
 * `low` when no producing stage is Secure or Compete; `assessed` false when no producing stage has a choice.
 */
export function manufacturingPriority(tech: string, cells: readonly PostureCell[]): { low: boolean; assessed: boolean; words: string } {
  const prod = cells.filter((c) => c.tech === tech && isProducingStage(c.stage)).sort((a, b) => stageRank(a.stage) - stageRank(b.stage));
  const by = new Map<string, string[]>();
  for (const c of prod) {
    const v = verbOf(c);
    const w = v ? VERB_LABEL[v] : 'Not assessed';
    by.set(w, [...(by.get(w) ?? []), stageDisplay(c.stage)]);
  }
  const low = !prod.some((c) => c.posture === 'S' || c.posture === 'C');
  const assessed = prod.some((c) => verbOf(c) !== null);
  const words = [...by.entries()].map(([w, s]) => `${w} at ${s.join(', ')}`).join('; ');
  return { low, assessed, words: words || 'no producing stage is classified' };
}

/** The domestic manufacturing priority in one word: not assessed (no producing stage has a choice), low, or set at the producing stages. */
export const manufacturingPriorityWord = (m: { low: boolean; assessed: boolean }): string => (!m.assessed ? 'not assessed' : m.low ? 'low' : 'set at the producing stages');

/** The main risk of a Secure node (spec XIV): the coercive supplier's hold, else the largest supplier's, else the rationale in words. */
export function secureRisk(c: PostureCell, d: NodeDetail | null, names: Record<string, string>): string {
  const sc = d?.supplyChain;
  const name = (i: string) => names[i] ?? i;
  const pc = (v: number) => `${Math.round(v * 100)}%`;
  if (sc?.coerciveTop) return `${name(sc.coerciveTop)} holds ${sc.coerciveTopShare !== null ? pc(sc.coerciveTopShare) : 'a large share'} of world supply${sc.coerciveImportShare !== null ? `; ${pc(sc.coerciveImportShare)} of imports come from potentially coercive suppliers` : ''}`;
  if (sc?.topSupplier && sc.topSupplierShare !== null) return `${name(sc.topSupplier)} supplies ${pc(sc.topSupplierShare)} of imports`;
  if (sc?.operationalControl) return sc.operationalControl;
  return c.rationale ? RATIONALE_WORDS[c.rationale] : 'not stated';
}

/** The recommended treatment of a Secure node: the mitigation the detail writes, else the policy design text (build, then fence), else null. */
export function secureTreatment(d: NodeDetail | null, design: Record<string, { fence?: string; build?: string; gate?: string }>, variant: string | null): string | null {
  if (d?.mitigation) return d.mitigation;
  const t = variant ? design[`${d?.variant ?? variant}|${d?.delivery ?? ''}`] : undefined;
  return t?.build ?? t?.fence ?? t?.gate ?? null;
}

/** How far a node's position is below the Preserve bar (percentile points, rounded), or null when it is at or above it or not measured. */
export function positionGap(c: Pick<PostureCell, 'cp'>, t: Pick<PostureThresholds, 'cp'>): number | null {
  return c.cp === null || c.cp >= t.cp ? null : Math.round((t.cp - c.cp) * 100);
}

/**
 * A Catch Up node's position in words, from the cell (never a fixed sentence): "position not measured"; at or
 * above the Preserve bar, a top-quarter score that did not pass the Preserve test (not a contender, or below the
 * capability floor); else below the bar: "a position below the top quarter" in a sentence (`sentence`), or the
 * gap in points with the position (`gap`, the Strategic Choices field).
 */
export function catchUpPositionWords(c: Pick<PostureCell, 'cp'>, t: Pick<PostureThresholds, 'cp'>, style: 'sentence' | 'gap'): string {
  if (c.cp === null) return 'position not measured';
  if (c.cp >= t.cp) return TOP_QUARTER_NOT_PRESERVE;
  return style === 'sentence' ? 'a position below the top quarter' : `${positionGap(c, t)} points below the Preserve bar (position ${Math.round(c.cp * 100)})`;
}

/** The weak or moderate domestic case of a Partner node (spec XIV), in words. */
export function domesticCase(c: Pick<PostureCell, 'cp' | 'prize' | 'pathway'>, t: Pick<PostureThresholds, 'cp' | 'prize'>): string {
  const bits: string[] = [];
  // a Partner node at or above the bar failed the Preserve test (not a contender, or below the capability floor)
  if (c.cp !== null) bits.push(c.cp >= t.cp ? `position ${Math.round(c.cp * 100)} (top-quarter score, but not a main producer or below the capability floor)` : `position ${Math.round(c.cp * 100)} (below the Preserve bar of ${Math.round(t.cp * 100)})`);
  else bits.push('position not measured');
  if (c.prize !== null) bits.push(c.prize >= t.prize ? `prize ${Math.round(c.prize * 100)} (high)` : `prize ${Math.round(c.prize * 100)} (below ${Math.round(t.prize * 100)})`);
  if (c.pathway === 'no' || c.pathway === 'unknown') bits.push('no credible pathway');
  return bits.join('; ');
}

// ---- Policy Alignment (spec XV) --------------------------------------------------------------------------

export interface PolicyRow {
  key: string;
  tech: string;
  stage: string;
  verb: Verb;
  /** the recommendation in words ("Partner", "Accelerate") */
  recommendation: string;
  alignment: Alignment | null;
  alignmentConfidence: PostureCell['alignmentConfidence'];
  /** observed policy intensity: the revealed policy priority (within-country percentile rank) */
  intensity: number | null;
  /** NIPO support, a within-country percentile rank (not a share) */
  support: number | null;
  /** NIPO protection or localisation, a within-country percentile rank (not a share) */
  protection: number | null;
  /** false: no protection measure recorded (shown as "none recorded", not as its tied rank); null: not stated */
  protectionRecorded: boolean | null;
  /** policy momentum where the time series supports it: a within-country percentile rank of recent policy activity, or words; null: not available */
  momentum: string | number | null;
  sources: string[];
}

/** Every assessed node of one economy, the ones needing attention first (misaligned, under-prioritized, deployment gap, over-prioritized, mixed, aligned, insufficient data). */
export function policyRows(cells: readonly PostureCell[], detail: PostureDetail | null): PolicyRow[] {
  const d = detailByKey(detail);
  const order = (a: Alignment | null) => (a === null ? ALIGNMENT_ATTENTION.length : ALIGNMENT_ATTENTION.indexOf(a));
  return cells
    .filter((c) => verbOf(c) !== null)
    .map((c) => {
      const v = verbOf(c) as Verb;
      const p = d.get(cellKeyOf(c))?.policy;
      return {
        key: cellKeyOf(c), tech: c.tech, stage: c.stage, verb: v,
        recommendation: v === 'secure' ? 'Secure' : v in MODE_LABEL ? MODE_LABEL[v as keyof typeof MODE_LABEL] : DEPLOY_MODE_LABEL[v as keyof typeof DEPLOY_MODE_LABEL],
        alignment: c.alignment, alignmentConfidence: c.alignmentConfidence,
        intensity: p?.priority ?? null, support: p?.support ?? null, protection: p?.protection ?? null, protectionRecorded: p?.protectionRecorded ?? null,
        momentum: p?.momentum ?? null, sources: p?.sources ?? [],
      };
    })
    .sort((a, b) => order(a.alignment) - order(b.alignment) || stageRank(a.stage) - stageRank(b.stage) || a.tech.localeCompare(b.tech));
}

/** Observed policy intensity in words, from the within-country percentile: high (top third), moderate, low. */
export function intensityWord(v: number | null): string {
  return v === null ? 'not available' : v >= 2 / 3 ? 'high' : v >= 1 / 3 ? 'moderate' : 'low';
}

/** An ordinal number: 1st, 2nd, 3rd, 4th, 11th, 12th, 13th, 21st, 92nd. */
export function ordinal(n: number): string {
  const tens = Math.abs(n) % 100;
  const ones = Math.abs(n) % 10;
  return `${n}${tens >= 11 && tens <= 13 ? 'th' : ones === 1 ? 'st' : ones === 2 ? 'nd' : ones === 3 ? 'rd' : 'th'}`;
}

/**
 * A within-country percentile rank in words. Observed policy intensity, support, protection and policy momentum
 * are ranks among the country's own nodes (R policy_priority.R within_pct), never shares of attention or
 * changes: "high, 92nd percentile in the country" (high: the top third, as intensityWord). `short` leaves out
 * "in the country" where the column or note already says so; a missing value is "not available".
 */
export function countryRankWords(v: number | null, short = false): string {
  if (v === null || !Number.isFinite(v)) return 'not available';
  return `${intensityWord(v)}, ${ordinal(Math.round(v * 100))} percentile${short ? '' : ' in the country'}`;
}

/** Protection or localisation in words: "none recorded" where the records hold no protection measure (its rank is then only a tie among zeros), else its rank. */
export function protectionWords(v: number | null, recorded: boolean | null, short = false): string {
  return recorded === false ? 'none recorded' : countryRankWords(v, short);
}

/** Policy momentum in words: the within-country rank of recent policy activity (never a signed change), words as written, else "not available". */
export function policyMomentumWords(m: string | number | null, short = false): string {
  if (m === null) return 'not available';
  return typeof m === 'number' ? countryRankWords(m, short) : m;
}

// ---- Allied Strategy (spec XVI) ----------------------------------------------------------------------------

export interface AlliedPartner {
  iso3: string;
  name: string;
  /** why this partner: strong position, trusted relationship, diversification (from the partner recommendation's numbers) */
  why: string[];
}

export type AlliedStatus = 'covered' | 'gap' | 'not_mapped';

export interface AlliedRow {
  key: string;
  tech: string;
  stage: string;
  /** Partner, or a Secure node whose delivery depends on trusted foreign capacity (allied or mixed) */
  verb: 'partner' | 'secure';
  need: number | null;
  rationale: SecurityRationale | null;
  delivery: string | null;
  partners: AlliedPartner[];
  /** covered: the allied division has a credible trusted holder and backups; gap: it has a gap; not_mapped: no division node */
  status: AlliedStatus;
  /** the gap logic in words (spec XVI.1), worst first */
  gaps: string[];
  /** no credible allied solution yet (the economy's partner gap): analytically valuable, never hidden */
  noSolution: boolean;
}

/** The reasons a recommended partner is named, from its numbers: they stay facts, with the thresholds in the words. */
export function partnerWhy(p: DetailPartner): string[] {
  const out: string[] = [];
  if (p.capability !== undefined && p.capability >= 0.05) out.push(`strong supply position (${Math.round(p.capability * 100)}% of world supply)`);
  if (p.cp !== undefined && p.cp >= 0.75) out.push('strong competitive position (top quarter)');
  if (p.relationship !== undefined && p.relationship >= 0.6) out.push('trusted relationship');
  if (p.diversification !== undefined && p.diversification !== null && p.diversification > 0) out.push('diversification contribution');
  return out;
}

/** The diversification value of the first partner with one: moving a tenth of imports to it cuts import concentration (HHI) by this much (R partner_recs). */
export function diversificationWords(partners: readonly DetailPartner[]): string | null {
  const p = partners.find((x) => typeof x.diversification === 'number' && x.diversification > 0);
  return p ? `moving 10% of imports to ${p.iso3} would cut import concentration (HHI) by ${(p.diversification as number).toFixed(3)}` : null;
}

/** The gap logic of one division node in the spec's words, worst first; concentration is read from the coercive powers' share where no gap names it. */
function gapWords(node: DivisionNode | undefined): string[] {
  if (!node) return [];
  const gaps = [...new Set([node.gap, ...node.gaps])].filter((g): g is Exclude<AibGap, 'none'> => g !== 'none');
  const ordered = GAPS_WORST_FIRST.filter((g) => gaps.includes(g));
  const words = [...new Set(ordered.map((g) => ALLIED_GAP_WORDS[g]))];
  if (!gaps.includes('excessive_concentration') && node.coerciveShare !== null && node.coerciveShare >= 0.5) {
    words.push(`concentration remains excessive (${Math.round(node.coerciveShare * 100)}% held by potentially coercive suppliers)`);
  }
  return words;
}

/**
 * The Allied Strategy rows (country need -> recommended partners -> why -> AIB status): every Partner node,
 * and every Secure node whose delivery leans on trusted foreign capacity (allied or mixed, from the detail
 * document). The AIB status is the allied division's: covered when its node has no gap, gap otherwise; a
 * Partner node with the economy's partner gap says "no credible allied solution yet".
 */
export function alliedRows(cells: readonly PostureCell[], detail: PostureDetail | null, division: readonly DivisionNode[], names: Record<string, string>): AlliedRow[] {
  const d = detailByKey(detail);
  const div = new Map(division.map((n) => [`${n.tech}|${n.stage}`, n]));
  const rows: AlliedRow[] = [];
  for (const c of cells) {
    const v = verbOf(c);
    const key = cellKeyOf(c);
    const det = d.get(key) ?? null;
    const delivery = det?.delivery ?? null;
    if (v !== 'partner' && !(v === 'secure' && (delivery === 'allied' || delivery === 'mixed'))) continue;
    const node = div.get(key);
    const noSolution = v === 'partner' && (c.partnerGap === true || det?.partnerGap === true);
    // the economy's own gap reasons (R partner_gap_reasons) where the detail writes them, else the allied division's
    const own = (det?.partnerGapReasons ?? []).slice().sort((a, b) => reasonRank(a) - reasonRank(b));
    const gaps = own.length ? [...new Set(own.map(partnerGapReasonWords))] : gapWords(node);
    const partners: AlliedPartner[] = (det?.partners ?? []).map((p) => ({ iso3: p.iso3, name: names[p.iso3] ?? p.iso3, why: partnerWhy(p) }));
    if (!partners.length && c.partner1 && !noSolution) partners.push({ iso3: c.partner1, name: names[c.partner1] ?? c.partner1, why: [] });
    const status: AlliedStatus = det?.aibStatus
      ? noSolution ? 'gap' : det.aibStatus
      : noSolution || (node && node.gap !== 'none') ? 'gap' : node ? 'covered' : 'not_mapped';
    rows.push({ key, tech: c.tech, stage: c.stage, verb: v, need: c.need, rationale: c.rationale, delivery, partners, status, gaps, noSolution });
  }
  return rows.sort((a, b) => Number(a.verb === 'secure') - Number(b.verb === 'secure') || (b.need ?? -1) - (a.need ?? -1) || stageRank(a.stage) - stageRank(b.stage));
}

/** The Allied Strategy's opening sentence, computed (spec XVI): "2 of Aland's priority nodes are better solved through allies; 1 Secure node also depends on trusted foreign capacity." */
export function alliedSentence(name: string, cells: readonly PostureCell[], rows: readonly AlliedRow[]): string {
  const priority = cells.filter((c) => c.posture === 'S' || c.posture === 'C').length;
  const partner = rows.filter((r) => r.verb === 'partner').length;
  const secure = rows.filter((r) => r.verb === 'secure').length;
  const gaps = rows.filter((r) => r.noSolution).length;
  const owner = name.endsWith('s') ? `${name}'` : `${name}'s`;
  const first = partner
    ? `${partner} of ${owner} ${priority} priority ${priority === 1 ? 'node' : 'nodes'} ${partner === 1 ? 'is' : 'are'} better solved through allies`
    : `None of ${owner} ${priority} priority ${priority === 1 ? 'node is' : 'nodes are'} a Partner choice`;
  const second = secure ? `${secure} Secure ${secure === 1 ? 'node also depends' : 'nodes also depend'} on trusted foreign capacity` : null;
  const third = gaps ? `${gaps} ${gaps === 1 ? 'has' : 'have'} no credible allied solution yet` : null;
  return `${[first, second, third].filter(Boolean).join('; ')}.`;
}
