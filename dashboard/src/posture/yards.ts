// Yards & Fences in the Explorer (2026-10-30; R yard_rollup.R, strategic_value.R, posture_export.R). The user's
// mapping: a Small yard sits behind a High fence (Secure); a Medium yard behind a medium, permeable fence
// (Preserve, Catch Up, Partner; Accelerate at Deployment & Use); a Large yard behind a Low fence (Watch).
// Capability layers (a grid's SCADA software, a battery's management firmware) are yard units of their own:
// each layer takes its own choice, and a node whose layers sit in different yards rolls up as mixed.
//
// What this module reads (all optional; an older bundle has none of it):
// - the core: the node's roll-up (cell.yards), the market prize beside the prize the choice read (cell.market,
//   cell.prize under meta.prize_measure) and the node-level evidence (meta.yf_nodes: the large-yard case and
//   its signals, the industrial-commons and future-option values, the market-only cap);
// - the economy's detail: its child layers (yardChildren, compact: the label and evidence are the core's),
//   the fence scope of its Secure layers and, where it differs from the node's, its own large-yard case.
// It composes the words the map, the fence sections and the drawer show: yard names, the mixed split, each
// layer's one-line reason and the large-yard case. Pure: no React, no IO.

import {
  ROLLUP_YARDS, yardOfVerb, type PostureCell, type PostureMeta, type Verb, type Yard, type YardRollup, type YfNode,
} from './contract.ts';
import type { LargeYardException, NodeDetail, YardChild, YardFenceScope } from './detail.ts';
import { YARD_NAME } from '../content/vocabulary.ts';
import { FENCE_LABEL, POSTURE_COLOR, VERB_COLOR, VERB_LABEL, VERB_OUTLINE, verbOf, type Fence } from './grammar.ts';

// ---- the yards ------------------------------------------------------------------------------------------

export const YARD_ORDER: readonly Yard[] = ['small', 'medium', 'large'];
/** The yard names (content/vocabulary.ts YARD_NAME; the definitions are vocabulary YARD_DEFINITION). */
export const YARD_LABEL: Record<Yard, string> = YARD_NAME;
/** The fence of each yard (the user's mapping). */
export const YARD_FENCE: Record<Yard, Fence> = { small: 'high', medium: 'medium', large: 'low' };
/** "Small yard · High fence", "Medium yard · Medium fence", "Large yard · Low fence". */
export const yardWords = (y: Yard): string => `${YARD_LABEL[y]} · ${FENCE_LABEL[YARD_FENCE[y]]}`;
/**
 * The fence colour of each yard, for the map's mixed cells: the one choice of the High fence (Secure orange) and
 * of the Low fence (Watch ink-600); the Medium fence holds four choices, so its colour is the Compete family's
 * brand blue (POSTURE_COLOR.C, NEIS --color-brand-blue).
 */
export const YARD_COLOR: Record<Yard, string> = { small: VERB_COLOR.secure, medium: POSTURE_COLOR.C, large: VERB_COLOR.watch };

/** The yard of a node from its own choice (null: not assessed). */
export const nodeYard = (c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode'>): Yard | null => yardOfVerb(verbOf(c));
/** The yards a roll-up spans, small to large. */
export const rollupYards = (r: YardRollup | null | undefined): readonly Yard[] => (r ? ROLLUP_YARDS[r] : []);
export const isMixedRollup = (r: YardRollup | null | undefined): boolean => rollupYards(r).length > 1;

/** The legend entry of a mixed node. */
export const MIXED_LEGEND = "Mixed: this step's layers sit in different yards";

/** A mixed node's yards: its own (from its choice) and the others its layers reach, small to large; null when it is not mixed. */
export function mixedYards(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode' | 'yards'>): { own: Yard; others: Yard[] } | null {
  const own = nodeYard(c);
  if (!own || !isMixedRollup(c.yards)) return null;
  const others = rollupYards(c.yards).filter((y) => y !== own);
  return others.length ? { own, others } : null;
}

/**
 * The fill of a mixed node: its own choice colour on the upper-left half, the fence colour of each other yard
 * on the lower-right (a quarter each when there are two), split on the diagonal by a hairline of the card
 * colour so a pale and a dark half stay apart. Null for a node that is not mixed (it keeps nodeFill).
 */
export function mixedFill(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode' | 'yards'>): string | null {
  const m = mixedYards(c);
  const v = verbOf(c);
  if (!m || !v) return null;
  const card = VERB_OUTLINE.secure;
  const stops: string[] = [`${VERB_COLOR[v]} 0 calc(50% - 1px)`, `${card} calc(50% - 1px) calc(50% + 1px)`];
  if (m.others.length === 1) stops.push(`${YARD_COLOR[m.others[0]]} calc(50% + 1px) 100%`);
  else {
    stops.push(`${YARD_COLOR[m.others[0]]} calc(50% + 1px) calc(75% - 1px)`, `${card} calc(75% - 1px) calc(75% + 1px)`, `${YARD_COLOR[m.others[1]]} calc(75% + 1px) 100%`);
  }
  return `linear-gradient(135deg, ${stops.join(', ')})`;
}

/** Whether a node's mark shows any Watch ink-600 (its own fill or a Large-yard part), which needs the paper-mute hairline on the card. */
export function showsLargeYard(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode' | 'yards'>): boolean {
  return verbOf(c) === 'watch' || (mixedYards(c)?.others.includes('large') ?? false);
}

/** "Small and Large yards", "Small, Medium and Large yards". */
const yardList = (ys: readonly Yard[]): string => {
  const w = ys.map((y) => YARD_LABEL[y].replace(' yard', ''));
  return `${w.length > 1 ? `${w.slice(0, -1).join(', ')} and ${w[w.length - 1]}` : w[0]} yard${w.length > 1 ? 's' : ''}`;
};

/**
 * A node's yards in words: "Medium yard" when its layers agree; mixed: "Mixed: this step in the Medium yard, its
 * layers also in the Large yard"; null without a roll-up.
 */
export function rollupWords(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode' | 'yards'>): string | null {
  if (!c.yards) return null;
  const m = mixedYards(c);
  if (!m) return isMixedRollup(c.yards) ? `Mixed: ${yardList(rollupYards(c.yards))}` : YARD_LABEL[rollupYards(c.yards)[0]];
  return `Mixed: this step in the ${YARD_LABEL[m.own]}, its layers also in the ${yardList(m.others)}`;
}

// ---- the node-level evidence (meta.yf_nodes) -------------------------------------------------------------

/** The key of a node or layer in meta.yf_nodes. */
export const yfKey = (tech: string, stage: string, layer = 'stage'): string => `${tech}|${stage}|${layer}`;

/** The node-level evidence of a node (layer "stage") or one of its layers; null when the core has none. */
export function yfNode(meta: Pick<PostureMeta, 'yf_nodes'>, tech: string, stage: string, layer = 'stage'): YfNode | null {
  return meta.yf_nodes[yfKey(tech, stage, layer)] ?? null;
}

/** A layer's name: the core's label, else its id in words ("ev-charging-mgmt" -> "ev charging mgmt"). */
export function layerLabel(meta: Pick<PostureMeta, 'yf_nodes'>, tech: string, stage: string, layerId: string): string {
  return yfNode(meta, tech, stage, layerId)?.label ?? layerId.replace(/[-_]+/g, ' ');
}

/** The ten large-yard signals in plain words (R strategic_value.R SP_LY_WORDS). */
export const LARGE_YARD_SIGNAL_WORDS: Record<string, string> = {
  mature: 'mature',
  passive: 'passive',
  standardized: 'standardized',
  substitutable: 'substitutable',
  easy_qualification: 'quick to qualify',
  diversified: 'diversified world supply',
  low_learning: 'little process learning',
  low_spillover: 'little cross-sector spillover',
  low_differentiation: 'little product differentiation',
  commoditized: 'commoditized',
};
/** Signals in words, joined: "mature, passive, quick to qualify" (an unknown name loses its underscores). */
export const signalWords = (names: readonly string[]): string => names.map((s) => LARGE_YARD_SIGNAL_WORDS[s] ?? s.replace(/_/g, ' ')).join(', ');

/** One node's large-yard case, resolved: the node's (meta.yf_nodes) or, where the economy's differs, its own. */
export interface LargeYardCase {
  /** true holds, false does not, null undecided */
  holds: boolean | null;
  /** how many commodity (non-required) signals hold */
  n: number | null;
  /** the commodity signals the case needs (the trace's bar), when the detail says */
  bar: number | null;
  signals: string[];
  unknown: string[];
  /** a large yard kept with a capability floor */
  floor: boolean;
  /** the economy's own case differs from the node's */
  own: boolean;
  /** the undecided reason the detail writes */
  reason: string | null;
}

/** The trace's large-yard step: its bar (the commodity signals needed). */
function largeYardBar(d: NodeDetail | null): number | null {
  const s = d?.trace.find((t) => t.test === 'large_yard_case' || t.step === 'large_yard');
  return typeof s?.threshold === 'number' ? s.threshold : null;
}

/**
 * The large-yard case of a node: the economy's own (detail largeYard, where it differs) else the node's
 * (meta.yf_nodes, layer "stage"); null when neither says. Producing stages only.
 */
export function largeYardOf(meta: Pick<PostureMeta, 'yf_nodes' | 'large_yard_signals'>, tech: string, stage: string, d: NodeDetail | null): LargeYardCase | null {
  const ex: LargeYardException | null = d?.largeYard ?? null;
  const bar = largeYardBar(d);
  if (ex) {
    const name = (i: number) => meta.large_yard_signals[i] ?? '?';
    return { holds: ex.case, n: ex.n, bar, signals: ex.s.map(name), unknown: ex.u.map(name), floor: ex.floor, own: true, reason: ex.reason };
  }
  const nd = yfNode(meta, tech, stage);
  if (!nd) return null;
  return { holds: nd.largeYard, n: nd.n, bar, signals: nd.signals, unknown: nd.unknown, floor: nd.floor === true, own: false, reason: null };
}

/** "3 commodity signals hold; 5 required" ("1 commodity signal holds" without a bar); null without a count. */
export const countWords = (n: number | null, bar: number | null): string | null =>
  n === null ? null : `${n} commodity ${n === 1 ? 'signal holds' : 'signals hold'}${bar !== null ? `; ${bar} required` : ''}`;

/**
 * The large-yard case in one line: "Holds: affordable, diversified access should dominate, whatever the size of
 * the market (6 commodity signals hold; 5 required)." / "Does not hold: 3 commodity signals hold; 5 required." /
 * the undecided reason.
 */
export function largeYardWords(ly: LargeYardCase): string {
  const count = countWords(ly.n, ly.bar);
  if (ly.holds === true) {
    return `Holds: affordable, diversified access should dominate, whatever the size of the market${count ? ` (${count})` : ''}${ly.floor ? '; kept with a capability floor (moderate industrial learning)' : ''}.`;
  }
  if (ly.holds === false) return `Does not hold${count ? `: ${count}` : ''}.`;
  return ly.reason ?? `Undecided: too little evidence observed${count ? ` (${count})` : ''}.`;
}

// ---- the prize: Strategic Industrial Value and its three families ---------------------------------------

/** The market-only cap in words (R strategic_value.R siv_cap). */
export const CAP_WORDS: Record<string, string> = {
  market_only: 'Market only: neither industrial commons nor future option reaches the gate, so the value is capped just below the prize bar (market size alone never makes a prize high)',
};
export const capWords = (cap: string | null | undefined): string | null => (cap ? (CAP_WORDS[cap] ?? cap.replace(/_/g, ' ')) : null);

/** The Strategic Industrial Value of a node or layer and its three families (the market prize from the core or the detail; commons, option and the cap from meta.yf_nodes). */
export interface SivFamilies {
  /** the prize the choice read (the node's; a layer ranks with its node's) */
  value: number | null;
  /** the market prize (Industrial Opportunity) */
  market: number | null;
  commons: number | null;
  option: number | null;
  commonsN: number | null;
  optionN: number | null;
  cap: string | null;
}
export function sivFamilies(
  meta: Pick<PostureMeta, 'yf_nodes'>, c: Pick<PostureCell, 'tech' | 'stage' | 'prize' | 'market'>, d: NodeDetail | null, layer = 'stage',
): SivFamilies {
  const nd = yfNode(meta, c.tech, c.stage, layer);
  return {
    value: c.prize, market: c.market ?? d?.prize.market ?? null,
    commons: nd?.commons ?? null, option: nd?.option ?? null, commonsN: nd?.commonsN ?? null, optionN: nd?.optionN ?? null, cap: nd?.cap ?? null,
  };
}

// ---- the child layers --------------------------------------------------------------------------------------

/** The engine variants in words (R posture_engine.R / decision_engine.R; the paper's yard and fence-scope variants). */
export const VARIANT_WORDS: Record<string, string> = {
  secure_control: 'a control layer separable from its hardware: the high fence covers the control path',
  secure_integrated: 'control built partly into the hardware: the high fence extends to the hardware',
  secure_whole: 'control inseparable from the hardware: the high fence covers the whole product',
  secure_unclear: 'a control layer whose separability from the hardware is not established',
  secure_both: 'a control path and a physical chokepoint',
  secure_input: 'a physical input a coercive supplier could withhold',
  secure_dual_use: 'a dual-use capability with national-security end uses',
  secure_resilience: 'systemic resilience: essential, import-dependent and slow to rebuild',
  compete_preserve: 'a strong position worth keeping',
  compete_catch_up: 'a high prize with a credible pathway',
  compete_partner: 'allies are the better answer than a domestic build',
  compete_costly: 'a strong case where protection is costly',
  deploy: 'domestic industrial priority low',
  deploy_watch: 'domestic industrial priority low',
  deploy_diversify: 'domestic industrial priority low; diversify supply',
  deploy_floor: 'a large yard kept with a capability floor',
  deploy_accelerate: 'deployment priority high',
};
export const variantWords = (v: string | null | undefined): string | null => (v ? (VARIANT_WORDS[v] ?? v.replace(/_/g, ' ')) : null);

/** The fence-scope note of a node with Secure layers. */
export const FENCE_SCOPE_WORDS: Record<YardFenceScope, string> = {
  extends_to_hardware: 'The high fence of a Secure layer here extends to the hardware it is built into, not the layer alone.',
  unclear: 'Whether the high fence of a Secure layer here must extend to its hardware is unclear: its separability is not established.',
};

/** A 0-1 value as a 0-100 score. */
const s100 = (v: number | null): string => (v === null ? 'not measured' : String(Math.round(v * 100)));

/**
 * The commons and option of a layer in words: "industrial commons 83, future option 50", "industrial commons 33
 * (1 of 3 observed), no future option" (a node that is not a leap candidate has no option tests); null without them.
 */
export function familyValueWords(nd: Pick<YfNode, 'commons' | 'option' | 'commonsN' | 'optionN'> | null): string | null {
  if (!nd || (nd.commons === null && nd.option === null)) return null;
  const obs = (n: number | null, of: number) => (n !== null && n < of ? ` (${n} of ${of} observed)` : '');
  const option = nd.option === 0 && nd.optionN === 0 ? 'no future option' : `future option ${s100(nd.option)}${obs(nd.optionN, 4)}`;
  return `industrial commons ${s100(nd.commons)}${obs(nd.commonsN, 3)}, ${option}`;
}

/**
 * One child layer's reason in one line, from its choice and variant and the layer's evidence in meta.yf_nodes:
 * Secure the fence scope of its variant; Watch the large-yard signals (or, without a case, the commons and
 * option values); the Medium-yard choices the commons and option values (and the market-only cap). Not
 * assessed says so.
 */
export function layerReason(meta: Pick<PostureMeta, 'yf_nodes'>, tech: string, stage: string, ch: Pick<YardChild, 'layerId' | 'choice' | 'variant'>): string {
  if (!ch.choice || ch.choice === 'not_assessed') return 'Not assessed: the layer has no choice of its own.';
  const nd = yfNode(meta, tech, stage, ch.layerId);
  const vw = variantWords(ch.variant);
  if (ch.choice === 'secure') return vw ? `${cap1(vw)}.` : 'Secure.';
  if (ch.choice === 'watch' && nd?.largeYard === true) {
    const sig = signalWords(nd.signals);
    return `Large-yard case: ${sig || 'the commodity signals hold'}${nd.floor || ch.variant === 'deploy_floor' ? '; kept with a capability floor' : ''}.`;
  }
  const fam = familyValueWords(nd);
  const capped = nd?.cap ? '; market-only value, capped below the prize bar' : '';
  const lead = vw ? cap1(vw) : VERB_LABEL[ch.choice as Verb];
  return `${lead}${fam ? `: ${fam}` : ''}${capped}.`;
}

const cap1 = (s: string): string => s.charAt(0).toUpperCase() + s.slice(1);

/** One child layer, resolved for display: its name, yard, choice and reason. */
export interface LayerUnit {
  layerId: string;
  label: string;
  yard: Yard | null;
  /** null when the layer is not assessed */
  verb: Verb | null;
  variant: string | null;
  reason: string;
}

/** A node's child layers in the detail's order, each with its name and reason. */
export function layerUnits(meta: Pick<PostureMeta, 'yf_nodes'>, d: Pick<NodeDetail, 'tech' | 'stage' | 'yardChildren'> | null): LayerUnit[] {
  if (!d) return [];
  return d.yardChildren.map((ch) => ({
    layerId: ch.layerId,
    label: layerLabel(meta, d.tech, d.stage, ch.layerId),
    yard: ch.yard ?? (ch.choice && ch.choice !== 'not_assessed' ? yardOfVerb(ch.choice) : null),
    verb: ch.choice && ch.choice !== 'not_assessed' ? ch.choice : null,
    variant: ch.variant,
    reason: layerReason(meta, d.tech, d.stage, ch),
  }));
}
