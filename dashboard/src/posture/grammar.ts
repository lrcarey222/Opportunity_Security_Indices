// The posture visual grammar, one for the whole site (spec XX; design 5b): the same colour, mark and words
// for a choice on the Country, Sector and AIB views.
//
// - Three background families only: Secure, Compete, Deploy. A node that is not assessed has no colour: it
//   is drawn empty and hatched (design D1).
// - Markers inside: Secure carries its rationale (O operational / control, K chokepoint, R systemic
//   resilience); Compete its mode (P Preserve, C Catch Up, linked arrows Partner); Deploy its mode (an up
//   arrow Accelerate, a dot Watch). Arrows mean only these: trade momentum left the cell (design D7) and is
//   given in words in the drawer and the table.
// - Secondary treatments: policy alignment as the border (aligned, under-prioritized, over-prioritized,
//   wrong treatment / misaligned, mixed; a deployment gap is drawn as under-prioritized and worded
//   "under-response (deployment)"), evidence quality as opacity (strong, moderate, weak).
// - Need, Prize and Position are evidence dimensions in neutral ink: no colour family of their own.
//
// Posture colours are a categorical triad validated with the dataviz validator on the dark surface #1f2937
// (2026-10-28): "#d1497e,#1889b0,#9a8f3d" passes lightness band, chroma floor, CVD (worst protan dE 10.3),
// the normal-vision floor (19.8) and 3:1 contrast. A choice is never carried by colour alone: every cell
// and point also carries its mark or shape, and the legend names all of them.

import type {
  AibGap, Alignment, AlignmentConfidence, AssessedPosture, CompeteMode, DeployMode, Evidence, Momentum, Pathway, PostureCell, PostureCode, PostureWord,
  SecurityRationale, Verb,
} from './contract.ts';

// The stage shorts (Ext, Ref, Com, Asm, Dep, Rec) and the display names live in src/lib/stages.ts, so every
// module uses one map; re-exported here for the posture views.
export { STAGE_DISPLAY, STAGE_SHORT, stageDisplay, stageShort } from '../lib/stages.ts';

// ---- the three families -----------------------------------------------------------------------------

export const POSTURE_COLOR: Record<AssessedPosture, string> = { S: '#d1497e', C: '#1889b0', D: '#9a8f3d' };
export const POSTURE_LABEL: Record<PostureCode, string> = { S: 'Secure', C: 'Compete', D: 'Deploy', N: 'Not assessed' };
export const POSTURE_ORDER: AssessedPosture[] = ['S', 'C', 'D'];
export const POSTURE_FROM_WORD: Record<PostureWord, PostureCode> = { secure: 'S', compete: 'C', deploy: 'D', not_assessed: 'N' };
export const isAssessed = (p: PostureCode): p is AssessedPosture => p !== 'N';

/** The fill of a node that is not assessed: empty, hatched (never a family colour). */
export const NOT_ASSESSED_HATCH = 'repeating-linear-gradient(135deg, rgb(255 255 255 / 0.16) 0 2px, transparent 2px 6px)';
export const NOT_ASSESSED_BORDER = '1px dashed rgb(255 255 255 / 0.3)';

// ---- the six verbs ------------------------------------------------------------------------------------

/** The six verbs in the order of the counts line and the Choices section. */
export const VERB_ORDER: Verb[] = ['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch'];
export const VERB_LABEL: Record<Verb, string> = { secure: 'Secure', preserve: 'Preserve', catch_up: 'Catch Up', partner: 'Partner', accelerate: 'Accelerate', watch: 'Watch' };
/** The family each verb belongs to (its background). */
export const VERB_FAMILY: Record<Verb, AssessedPosture> = { secure: 'S', preserve: 'C', catch_up: 'C', partner: 'C', accelerate: 'D', watch: 'D' };
/** "Compete · Preserve", "Deploy · Accelerate", "Secure". */
export const VERB_LONG: Record<Verb, string> = {
  secure: 'Secure', preserve: 'Compete · Preserve', catch_up: 'Compete · Catch Up', partner: 'Compete · Partner', accelerate: 'Deploy · Accelerate', watch: 'Deploy · Watch',
};
/** The Choices section's subtitles (spec XIV). */
export const VERB_SUBTITLE: Record<Verb, string> = {
  secure: 'Repair acute vulnerabilities',
  preserve: 'Protect and extend existing strengths',
  catch_up: 'Build capability where the prize and pathway justify it',
  partner: 'Important capabilities better solved through allies',
  accelerate: 'Technologies the energy system needs more of',
  watch: 'Do not prioritize domestic industrial intervention',
};

export const MODE_LABEL: Record<CompeteMode, string> = { preserve: 'Preserve', catch_up: 'Catch Up', partner: 'Partner' };
export const DEPLOY_MODE_LABEL: Record<DeployMode, string> = { accelerate: 'Accelerate', watch: 'Watch' };
/** Compete marks: P Preserve, C Catch Up, linked arrows Partner. */
export const MODE_MARK: Record<CompeteMode, string> = { preserve: 'P', catch_up: 'C', partner: '⇄' };
/** Deploy marks: an up arrow Accelerate, a dot Watch. */
export const DEPLOY_MARK: Record<DeployMode, string> = { accelerate: '↑', watch: '•' };

/** The verb of a node (null: not assessed). */
export function verbOf(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode'>): Verb | null {
  if (c.posture === 'S') return 'secure';
  if (c.posture === 'C') return c.mode;
  if (c.posture === 'D') return c.deployMode;
  return null;
}

/** "Compete · Preserve", "Deploy · Watch", "Secure", "Not assessed" (a Compete or Deploy node without its mode: the family alone). */
export function choiceWord(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode'>): string {
  const v = verbOf(c);
  return v ? VERB_LONG[v] : POSTURE_LABEL[c.posture];
}

// ---- Secure rationales ---------------------------------------------------------------------------------

export const RATIONALE_LABEL: Record<SecurityRationale, string> = {
  operational_control: 'Operational control',
  coercive_chokepoint: 'Coercive chokepoint',
  systemic_resilience: 'Systemic resilience',
};
/** The short words of the legend and the sentence: "operational / control", "chokepoint", "systemic resilience". */
export const RATIONALE_SHORT: Record<SecurityRationale, string> = {
  operational_control: 'operational / control',
  coercive_chokepoint: 'chokepoint',
  systemic_resilience: 'systemic resilience',
};
export const RATIONALE_MARK: Record<SecurityRationale, string> = { operational_control: 'O', coercive_chokepoint: 'K', systemic_resilience: 'R' };
export const RATIONALE_ORDER: SecurityRationale[] = ['operational_control', 'coercive_chokepoint', 'systemic_resilience'];
export const RATIONALE_WORDS: Record<SecurityRationale, string> = {
  operational_control: 'a foreign actor could command, update or disable it, or it serves national-security end uses',
  coercive_chokepoint: 'a potentially coercive supplier could withhold it, substitution is hard and trusted supply too slow to restore',
  systemic_resilience: 'it carries an essential function, the country depends on imports, supply takes years to rebuild and trusted partners cannot replace it',
};

/** The reason tags of the Need ranking (spec XII A). */
export type NeedTag = 'CHOKEPOINT' | 'SYSTEMIC RESILIENCE' | 'CONTROL LAYER' | 'HIGH DEPENDENCE';
export const RATIONALE_TAG: Record<SecurityRationale, NeedTag> = {
  operational_control: 'CONTROL LAYER',
  coercive_chokepoint: 'CHOKEPOINT',
  systemic_resilience: 'SYSTEMIC RESILIENCE',
};

/** The centre mark of a node: its rationale, Compete mode or Deploy mode; nothing when not assessed. */
export function cellMark(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode' | 'rationale'>): string {
  if (c.posture === 'S') return c.rationale ? RATIONALE_MARK[c.rationale] : '';
  if (c.posture === 'C') return c.mode ? MODE_MARK[c.mode] : '';
  if (c.posture === 'D') return c.deployMode ? DEPLOY_MARK[c.deployMode] : '';
  return '';
}

/** The marker shape of a verb on a scatter (a Secure point keeps its true coordinates, outlined). */
export type VerbShape = 'diamond' | 'circle' | 'triangle' | 'square' | 'ring';
export const VERB_SHAPE: Record<Exclude<Verb, 'accelerate'>, VerbShape> = { secure: 'diamond', preserve: 'circle', catch_up: 'triangle', partner: 'square', watch: 'ring' };

// ---- policy alignment ---------------------------------------------------------------------------------

/** The five classes the map draws (plus no data): a deployment gap is drawn as under-prioritized. */
export type AlignmentClass = 'aligned' | 'under_prioritized' | 'over_prioritized' | 'misaligned' | 'mixed' | 'insufficient_data';
export const ALIGNMENT_CLASS_ORDER: AlignmentClass[] = ['aligned', 'under_prioritized', 'over_prioritized', 'misaligned', 'mixed'];
export const alignmentClass = (a: Alignment): AlignmentClass => (a === 'deployment_gap' ? 'under_prioritized' : a);

export const ALIGNMENT_CLASS_LABEL: Record<AlignmentClass, string> = {
  aligned: 'Aligned',
  under_prioritized: 'Under-prioritized',
  over_prioritized: 'Over-prioritized',
  misaligned: 'Wrong treatment / misaligned',
  mixed: 'Mixed',
  insufficient_data: 'Insufficient data',
};
/** The words of one alignment value: a deployment gap is "Under-prioritized: under-response (deployment)". */
export const ALIGNMENT_LABEL: Record<Alignment, string> = {
  ...ALIGNMENT_CLASS_LABEL,
  deployment_gap: 'Under-prioritized: under-response (deployment)',
};
/** What each alignment means, for the drawer and the Policy Alignment section (spec X). */
export const ALIGNMENT_WORDS: Record<Alignment, string> = {
  aligned: 'observed policy matches the recommended choice',
  under_prioritized: 'the recommended choice calls for more policy effort than observed',
  over_prioritized: 'heavy protection or localisation where no domestic industrial priority is recommended',
  misaligned: 'the policy treatment is the wrong one for the choice (e.g. localisation where allies are the answer)',
  deployment_gap: 'deployment needs to accelerate but deployment policy is weak: an under-response',
  mixed: 'the policy signals disagree',
  insufficient_data: 'too few policy records to judge',
};
/** A border style per class (never colour alone; the tooltip and drawer say it in words). */
export const ALIGNMENT_BORDER: Record<AlignmentClass, string> = {
  aligned: '1px solid rgb(255 255 255 / 0.18)',
  under_prioritized: '2px dashed #ffffff',
  over_prioritized: '2px dotted #ffffff',
  misaligned: '2px solid #ffffff',
  mixed: '2px double #ffffff',
  insufficient_data: '1px solid transparent',
};
export const ALIGNMENT_ICON: Partial<Record<AlignmentClass, string>> = { under_prioritized: '▽', over_prioritized: '△', misaligned: '≠', mixed: '≈' };
export const ALIGNMENT_CONFIDENCE_LABEL: Record<AlignmentConfidence, string> = { high: 'High', medium: 'Medium', low: 'Low' };

/** Policy alignment classes in the order the Policy Alignment section lists them (the ones needing attention first). */
export const ALIGNMENT_ATTENTION: Alignment[] = ['misaligned', 'under_prioritized', 'deployment_gap', 'over_prioritized', 'mixed', 'aligned', 'insufficient_data'];

// ---- evidence -------------------------------------------------------------------------------------------

export const EVIDENCE_LEVELS: Evidence[] = ['strong', 'moderate', 'weak'];
export const EVIDENCE_LABEL: Record<Evidence, string> = { strong: 'Strong', moderate: 'Moderate', weak: 'Weak' };
/** Evidence quality as opacity: strong evidence is solid, weak fades (and says so in words); unknown sits between. */
export function evidenceOpacity(ev: Evidence | null | undefined): number {
  return ev === 'strong' ? 1 : ev === 'moderate' ? 0.85 : ev === 'weak' ? 0.6 : 0.75;
}

// ---- trade momentum (words only: the drawer and the table, never the cell) --------------------------------

export const MOMENTUM_LABEL: Record<Momentum, string> = { up: 'rising', flat: 'flat', down: 'falling' };
export const MOMENTUM_UNAVAILABLE = 'Current momentum unavailable — incomplete matched-period reporting';

// ---- other words --------------------------------------------------------------------------------------

export const PATHWAY_LABEL: Record<Pathway, string> = { yes: 'Credible', no: 'Not credible', unknown: 'Unknown (too little evidence observed, or the resource base unknown; counts as not credible)' };
export const NEED_TYPE_LABEL = { supply_chain: 'Supply-chain need', deployment: 'Deployment need' } as const;
/** The observed-policy sources of a node (detail policy.sources), in words. */
export const POLICY_SOURCE_LABEL: Record<string, string> = { nipo: 'NIPO industrial policy records', pams: 'IEA Policies and Measures' };
/** Where a value comes from (design D12). */
export const BASIS_LABEL: Record<string, string> = {
  national: 'national data',
  regional_proxy: 'regional proxy',
  observed_trend: 'observed trend',
  global_context: 'global context',
  not_assessed: 'not assessed',
  peers: 'among peers',
  contenders: 'among contenders',
};
export const basisWords = (b: string | null | undefined): string | null => (b ? (BASIS_LABEL[b] ?? b.replace(/_/g, ' ')) : null);
export const DELIVERY_LABEL: Record<string, string> = { domestic: 'domestic', allied: 'allied', diversified_global: 'diversified global', mixed: 'mixed' };
export const deliveryWords = (d: string | null | undefined): string | null => (d ? (DELIVERY_LABEL[d] ?? d.replace(/_/g, ' ')) : null);
/** A consistency flag in words (review_strong_position_low_prize -> "review: strong position, low prize"). */
export const FLAG_LABEL: Record<string, string> = {
  review_strong_position_low_prize: 'Review: strong position with high need but a low prize (kept as Preserve, not Partner)',
  // R decision_engine.R SP_CHOICE_FLAGS
  review_top_quarter_below_floor: 'Review: position in the top quarter of contenders but below the capability floor (so not Preserve)',
};
export const flagWords = (f: string | null | undefined): string | null => (f ? (FLAG_LABEL[f] ?? f.replace(/_/g, ' ')) : null);

/**
 * Why a Watch node is Watch, in words: the engine's codes (R decision_engine.R sp_watch_reasons, the detail's
 * watch_reasons). Ample diversified supply is the engine's own test (no concentration watch: no potentially
 * coercive supplier holds much of world supply and own imports are not concentrated); a weak domestic case is a
 * high prize without a top-quarter position or a credible pathway; option value marks an emerging technology.
 */
export const WATCH_REASON_LABEL: Record<string, string> = {
  low_need: 'Low need',
  low_prize: 'Low prize',
  ample_diversified_supply: 'Ample diversified supply',
  weak_domestic_case: 'Weak domestic case',
  option_value: 'Emerging technology, research and option value',
  low_deployment_need: 'Deployment need below the acceleration bar',
};
/** One Watch reason code in words (an unknown code loses its underscores). */
export const watchReasonWords = (r: string): string => WATCH_REASON_LABEL[r] ?? r.replace(/_/g, ' ');

/** The policy-alignment notes R writes (posture_engine.R sp_policy_alignment flags; docs/strategic_position.md). */
export const ALIGNMENT_FLAG_LABEL: Record<string, string> = {
  partner_autarkic: 'protection or localisation in the top third where the choice is Partner',
  fence_without_build: 'heavy protection with little support (a fence without a build)',
  watch_protection: 'heavy protection or localisation where the choice is Watch',
  watch_localisation_deployment: 'heavy local-content protection on the equipment of a Watch deployment',
  watch_attention: 'heavy total policy attention where the choice is Watch (protection not measured)',
  weak_deployment_policy: 'deployment policy below the middle where the choice is Accelerate',
  sources_disagree: 'the policy sources (industrial-policy measures and IEA records) disagree',
};
export const alignmentFlagWords = (f: string | null | undefined): string | null => (f ? (ALIGNMENT_FLAG_LABEL[f] ?? f.replace(/_/g, ' ')) : null);

/**
 * The credible-pathway evidence legs and states R writes in pathway_basis (pathway.R: SP_PATHWAY_EVIDENCE, and
 * the reasons a pathway is not credible or unknown).
 */
export const PATHWAY_EVIDENCE_LABEL: Record<string, string> = {
  feasibility: 'feasibility',
  pipeline: 'pipeline',
  adjacent_stage: 'adjacent stage',
  learning: 'learning',
  input_cost: 'input-cost advantage',
  no_resource_base: 'no resource base',
  resource_base_unknown: 'resource base unknown',
  none_observed_fires: 'evidence observed, none sufficient',
  no_evidence: 'no evidence observed',
  // fewer legs fired than the pathway requires (config pathway.min_legs)
  too_few_legs: 'too few pieces of evidence',
  too_few_observed: 'too little evidence observed to reach the bar',
};
/** A ";"-list or array of R codes in words through a label table (unknown codes lose their underscores). */
function codeList(v: string | readonly string[] | null | undefined, labels: Record<string, string>): string | null {
  const parts = (typeof v === 'string' ? v.split(';') : (v ?? [])).map((x) => x.trim()).filter(Boolean);
  return parts.length ? parts.map((x) => labels[x] ?? x.replace(/_/g, ' ')).join(', ') : null;
}
export const pathwayEvidenceWords = (v: string | readonly string[] | null | undefined): string | null => codeList(v, PATHWAY_EVIDENCE_LABEL);

/** The tests of the compact decision trace (R decision_engine.R sp_choice_trace). */
export const TRACE_TEST_LABEL: Record<string, string> = {
  secure_override: 'Secure tests',
  deployment_need: 'Deployment need',
  supply_chain_need: 'Supply-chain need',
  industrial_opportunity: 'Industrial Opportunity',
  cp_contender_pct: 'Competitive Position among contenders',
  pathway_credible: 'Credible pathway',
  choice: 'Choice',
};
/** The coded values and results of the trace: routes, pathway evidence, levels and verbs. */
const TRACE_CODE_LABEL: Record<string, string> = {
  ...RATIONALE_SHORT,
  ...PATHWAY_EVIDENCE_LABEL,
  ...VERB_LABEL,
  not_assessed: 'not assessed',
  not_classified: 'not classified',
  not_top_quarter: 'not in the top quarter',
  capability_floor_not_met: 'top quarter, capability floor not met',
  not_a_contender: 'top quarter, not a contender',
};
export const traceTestWords = (t: string): string => TRACE_TEST_LABEL[t] ?? t.replace(/_/g, ' ');
export const traceCodeWords = (v: string): string => codeList(v, TRACE_CODE_LABEL) ?? v;

/** The decision inputs a borderline node is near the threshold of (R names in the core's `bd`, ";"-separated). */
export const BORDERLINE_INPUT_LABEL: Record<string, string> = {
  supply_chain_need: 'supply-chain need',
  deployment_need: 'deployment need',
  industrial_opportunity: 'Industrial Opportunity',
  cp_contender_pct: 'Competitive Position',
  need: 'need',
  prize: 'Industrial Opportunity',
  cp: 'Competitive Position',
};
export const borderlineWords = (b: string | null | undefined): string | null =>
  b ? b.split(';').map((x) => x.trim()).filter(Boolean).map((x) => BORDERLINE_INPUT_LABEL[x] ?? x.replace(/_/g, ' ')).join(', ') : null;

// ---- the allied division's gaps -------------------------------------------------------------------------

export const GAP_LABEL: Record<AibGap, string> = {
  none: 'Covered',
  no_trusted_supplier: 'No trusted supplier',
  single_point_of_failure: 'Single point of failure',
  insufficient_redundancy: 'Insufficient redundancy',
  insufficient_allied_capacity: 'Insufficient allied capacity',
  excessive_concentration: 'Excessive concentration',
  weak_evidence: 'Weak evidence',
};
/** The Allied Strategy gap logic in the spec's words (XVI.1). */
export const ALLIED_GAP_WORDS: Record<Exclude<AibGap, 'none'>, string> = {
  no_trusted_supplier: 'no trusted supplier exists',
  single_point_of_failure: 'only one credible supplier exists',
  weak_evidence: 'capability evidence is weak',
  insufficient_redundancy: 'allied capacity is insufficient',
  insufficient_allied_capacity: 'allied capacity is insufficient',
  excessive_concentration: 'concentration remains excessive',
};
/** An economy's own partner-gap reasons for one node (R partner_gap_reasons), worst first, in the spec's words (XVI.1). */
export const PARTNER_GAP_REASON_ORDER = ['no_trusted_supplier', 'single_credible_supplier', 'excessive_concentration', 'insufficient_allied_capacity', 'weak_evidence'] as const;
export const PARTNER_GAP_REASON_WORDS: Record<string, string> = {
  no_trusted_supplier: 'no trusted supplier exists',
  single_credible_supplier: 'only one credible supplier exists',
  weak_evidence: 'capability evidence is weak',
  insufficient_allied_capacity: 'allied capacity is insufficient',
  excessive_concentration: 'concentration remains excessive',
};
export const partnerGapReasonWords = (r: string): string => PARTNER_GAP_REASON_WORDS[r] ?? r.replace(/_/g, ' ');

/** Gap severity order (worst first) and a ring for the AIB matrix: a gap is drawn as a ring and a word. */
export const GAP_ORDER: AibGap[] = [
  'no_trusted_supplier', 'single_point_of_failure', 'excessive_concentration', 'insufficient_allied_capacity', 'insufficient_redundancy', 'weak_evidence', 'none',
];
export const GAP_RING: Record<AibGap, string> = {
  no_trusted_supplier: '#e2574c',
  single_point_of_failure: '#f58229',
  excessive_concentration: '#f58229',
  insufficient_allied_capacity: '#d1d5db',
  insufficient_redundancy: '#d1d5db',
  weak_evidence: '#6b7280',
  none: 'transparent',
};
