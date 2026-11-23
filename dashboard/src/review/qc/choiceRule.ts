// A TypeScript port of the canonical choice rule, sp_choice_rule() in R/strategic_position/decision_engine.R
// (:160-182), with the input definitions it reads (:84-160: sp_choice_inputs, sp_choice_prize_high,
// sp_choice_high_cp) and the large-yard case of strategic_value.R sp_large_yard_case (:287-332), operating on
// what a shipped posture-detail cell carries (its decision trace, need, prize, Competitive Position, share and
// pathway). It exists so the QC layer can (1) re-run the rule from the shipped trace and check it reproduces
// the shipped choice (docs/QC_SYSTEM_DESIGN.md 6.1) and (2) re-run it under alternative thresholds
// (qc/sensitivity.ts, design 4.6). It never changes a threshold or a classification of the Explorer.
//
// What the bundle does not ship is never guessed. The capability floor (t3_capability_lo) is read only where
// the trace or the borderline list implies it; the large-yard case under another min_signals only where the
// signal count and the borderline list decide it; a combination that stays open is evaluated both ways and
// reported as unresolved when the choice depends on it. All values are the bundle's 3-decimal roundings, so
// a value printed exactly on a threshold can sit on either side of it: reproduceShippedChoices() reports
// those cells with the reason, never silently. Pure: no IO.

import { DEPLOYMENT_STAGE } from '../../lib/stages.ts';
import type { PostureCell, PostureCore, Verb } from '../../posture/contract.ts';
import type { NodeDetail } from '../../posture/detail.ts';
import { cellKey } from './bundles.ts';

export type Choice = Verb | 'not_assessed';
/** R SP_CHOICES, in order. */
export const CHOICES: readonly Choice[] = ['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed'];

/**
 * The rule's settings. need / prize / cp / deployment come from the core's meta.thresholds; largeMin is the
 * trace's large_yard_case threshold (decision_framework.large_yard.min_signals); positionFirst is config only
 * (decision_framework.large_yard.position_first, not in the bundle); cpStrict replaces the inclusive >= at the
 * Competitive Position bar with > (a sensitivity, not the rule).
 */
export interface ChoiceThresholds {
  need: number;
  prize: number;
  cp: number;
  deployment: number;
  largeMin: number;
  positionFirst: boolean;
  cpStrict: boolean;
}

/** The settings the R defaults give where the bundle does not carry them (decision_engine.R sp_choice_thresholds). */
export const CONFIG_ONLY_DEFAULTS = { largeMin: 5, positionFirst: true, cpStrict: false, contenderMinShare: 0.005 } as const;

/** The thresholds of a decoded core (meta.thresholds) with the config-only settings. */
export function thresholdsFromCore(core: Pick<PostureCore, 'meta'>, over: Partial<ChoiceThresholds> = {}): ChoiceThresholds {
  const t = core.meta.thresholds;
  return {
    need: t.need,
    prize: t.prize,
    cp: t.cp,
    deployment: t.deployment,
    largeMin: CONFIG_ONLY_DEFAULTS.largeMin,
    positionFirst: CONFIG_ONLY_DEFAULTS.positionFirst,
    cpStrict: CONFIG_ONLY_DEFAULTS.cpStrict,
    ...over,
  };
}

/** Round a threshold so 0.67 + 0.05 compares as 0.72 (the shipped values have 3 decimals). */
export const roundTh = (x: number): number => Math.round(x * 1e9) / 1e9;

// ---- the rule itself ---------------------------------------------------------------------------------------------

/** The arguments of R sp_choice_rule(stage, secure, need, prize, cp_high, pathway, dn, th, large, capped). */
export interface RuleArgs {
  stage: string;
  secure: boolean;
  need: number | null;
  prize: number | null;
  cpHigh: boolean;
  pathway: string | null;
  dn: number | null;
  large: boolean;
  capped: boolean;
}

/** R sp_choice_prize_high: at or above the threshold and not capped as market-only. */
export const prizeHigh = (prize: number | null, th: Pick<ChoiceThresholds, 'prize'>, capped: boolean): boolean => prize !== null && prize >= th.prize && !capped;

/**
 * R sp_choice_rule, line for line (dplyr::case_when, first match wins):
 * secure -> secure; Deployment: dn NA -> not_assessed, dn >= deployment -> accelerate, else watch; producing:
 * need or prize NA -> not_assessed; large-yard case (unless position_first and a high position) -> watch;
 * hp & ch -> preserve; hp & pathway yes -> catch_up; hp & hn -> partner; hp -> watch; hn & ch -> preserve;
 * hn -> partner; else watch.
 */
export function spChoiceRule(a: RuleArgs, th: Pick<ChoiceThresholds, 'need' | 'prize' | 'deployment' | 'positionFirst'>): Choice {
  const dep = a.stage === DEPLOYMENT_STAGE;
  const hn = a.need !== null && a.need >= th.need;
  const hp = prizeHigh(a.prize, th, a.capped);
  const ch = a.cpHigh === true;
  const pw = a.pathway === 'yes';
  const ly = !dep && a.large === true && !(th.positionFirst && ch);
  if (a.secure) return 'secure';
  if (dep && a.dn === null) return 'not_assessed';
  if (dep && a.dn !== null && a.dn >= th.deployment) return 'accelerate';
  if (dep) return 'watch';
  if (a.need === null || a.prize === null) return 'not_assessed';
  if (ly) return 'watch';
  if (hp && ch) return 'preserve';
  if (hp && pw) return 'catch_up';
  if (hp && hn) return 'partner';
  if (hp) return 'watch';
  if (hn && ch) return 'preserve';
  if (hn) return 'partner';
  return 'watch';
}

// ---- inputs from a shipped detail cell ------------------------------------------------------------------------------

/** The trace's result words (null where the trace has no such step). */
export interface TracedLevels {
  secure: string | null;
  large: string | null;
  need: string | null;
  prize: string | null;
  position: string | null;
  pathway: string | null;
  deployment: string | null;
  choice: string | null;
}

export interface ChoiceInputs {
  stage: string;
  /** the trace's secure_override result is "yes" (the posture engine's Secure; not reproducible from the bundle) */
  secure: boolean;
  /** supply_chain_need (producing stages; null at Deployment or where not assessed) */
  need: number | null;
  /** the prize the rule reads (prize.value: the Strategic Industrial Value) */
  prize: number | null;
  /** the node's prize is capped as market-only (core meta.yf_nodes cap, else the reason's wording); null: unknown */
  capped: boolean | null;
  /** deployment_need (Deployment only) */
  dn: number | null;
  /** Competitive Position among contenders */
  cp: number | null;
  /** a contender (measured world share >= contenders.min_share, or a node measuring no share); null: unknown */
  contender: boolean | null;
  /** the capability lower bound is at least moderate (t3_capability_lo); null: not implied by the bundle */
  floorMet: boolean | null;
  pathway: string | null;
  /** the large-yard case: true holds, false does not, null undecided (NA); undefined: no large-yard step (Deployment, Secure) */
  large: boolean | null | undefined;
  /** how many of the non-required (commodity) signals hold (large_yard_n) */
  largeN: number | null;
  /** min_signals as the trace states it */
  largeMin: number | null;
  /** large_yard_near: the case fails for want of one signal (read from the borderline list on triage cells); null unknown */
  largeNear: boolean | null;
  borderline: string[];
  home_share: number | null;
  traced: TracedLevels;
  /** the trace's test ids, in order */
  steps: string[];
}

export interface ExtractContext {
  /** core meta.yf_nodes["Tech|Stage|stage"].cap === "market_only" (null: no node record) */
  capped?: boolean | null;
  /** decision_framework.contenders.min_share (config only; default 0.005) */
  contenderMinShare?: number;
  /** the shipped Competitive Position bar (core meta.thresholds.cp; default 0.75) */
  cpThreshold?: number;
}

const stepOf = (cell: Pick<NodeDetail, 'trace'>, test: string) => cell.trace.find((t) => t.test === test) ?? null;
const numVal = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : typeof v === 'string' && v !== '' && Number.isFinite(Number(v)) ? Number(v) : null);

/** Read the rule's inputs from a normalised detail cell (posture/detail.ts NodeDetail). */
export function extractChoiceInputs(cell: NodeDetail, stage: string = cell.stage, ctx: ExtractContext = {}): ChoiceInputs {
  const dep = stage === DEPLOYMENT_STAGE;
  const sSec = stepOf(cell, 'secure_override');
  const sLy = stepOf(cell, 'large_yard_case');
  const sNeed = stepOf(cell, 'supply_chain_need');
  const sPrize = stepOf(cell, 'strategic_industrial_value') ?? stepOf(cell, 'industrial_opportunity');
  const sCp = stepOf(cell, 'cp_contender_pct');
  const sPw = stepOf(cell, 'pathway_credible');
  const sDn = stepOf(cell, 'deployment_need');
  const sCh = stepOf(cell, 'choice');
  const traced: TracedLevels = {
    secure: sSec?.result ?? null,
    large: sLy?.result ?? null,
    need: sNeed?.result ?? null,
    prize: sPrize?.result ?? null,
    position: sCp?.result ?? null,
    pathway: sPw?.result ?? null,
    deployment: sDn?.result ?? null,
    choice: sCh?.result ?? null,
  };
  const minShare = ctx.contenderMinShare ?? CONFIG_ONLY_DEFAULTS.contenderMinShare;
  const borderline = cell.borderline;
  const home = cell.homeShare;
  const need = dep ? null : (cell.need ?? (sNeed ? numVal(sNeed.value) : null));
  const prize = dep ? null : (cell.prize.value ?? (sPrize ? numVal(sPrize.value) : null));
  const dn = dep ? (cell.need ?? (sDn ? numVal(sDn.value) : null)) : null;
  const cp = cell.cp ?? (sCp ? numVal(sCp.value) : null);
  const cpThreshold = sCp ? numVal(sCp.threshold) : null;
  // the capped prize: the node's own record, else the reason's words ("the market prize alone")
  const capped = ctx.capped ?? (cell.reason && /market prize alone/.test(cell.reason) ? true : null);
  // contender and capability floor (R sp_choice_trace pos_res; nb_cp in sp_decision_choice)
  let contender: boolean | null = home === null ? null : home >= minShare;
  let floorMet: boolean | null = null;
  const pos = traced.position;
  if (pos === 'high') {
    contender = true;
    floorMet = true;
  } else if (pos === 'capability_floor_not_met') {
    contender = true;
    floorMet = false;
  } else if (pos === 'not_a_contender') {
    contender = false;
  } else if (pos === 'not_top_quarter' && cp !== null) {
    // a triage cell within the band of the bar is tagged borderline on cp exactly when it is a contender with the floor met
    if (borderline.includes('cp_contender_pct')) {
      contender = true;
      floorMet = true;
    } else {
      const bar = cpThreshold ?? ctx.cpThreshold ?? 0.75;
      // strictly inside the band even after 3-dp rounding
      if (Math.abs(cp - bar) < 0.05 - 0.0005 && contender === true) floorMet = false;
    }
  }
  // large yard
  const large: boolean | null | undefined = sLy ? (sLy.result === 'yes' ? true : sLy.result === 'no' ? false : null) : undefined;
  const largeN = sLy ? numVal(sLy.value) : null;
  const largeMin = sLy ? numVal(sLy.threshold) : null;
  // a large-yard Watch never reads the position: under position_first its cp_high was false
  const lyDecided = large === true && traced.choice === 'watch' && sCp === null && sNeed === null;
  if (lyDecided && cp !== null && contender === true && floorMet === null && cp >= (ctx.cpThreshold ?? 0.75)) floorMet = false;
  // near: only meaningful (and only tagged) on a triage-read, assessed cell
  const triageRead = sNeed !== null && sPrize !== null && traced.need !== 'not_assessed' && need !== null && prize !== null;
  const largeNear = large === false && triageRead ? borderline.includes('large_yard_case') : null;
  return {
    stage,
    secure: traced.secure === 'yes',
    need,
    prize,
    capped,
    dn,
    cp,
    contender,
    floorMet,
    pathway: cell.pathway.credible ?? traced.pathway,
    large,
    largeN,
    largeMin,
    largeNear,
    borderline,
    home_share: home,
    traced,
    steps: cell.trace.map((t) => t.test),
  };
}

// ---- the rule on extracted inputs ---------------------------------------------------------------------------------

export type RuleMode =
  /** every level from the shipped (3-decimal) values: the reproducibility test */
  | 'values'
  /** at a threshold equal to the shipped one, the trace's own result word decides (the rule's logic alone); moved thresholds use the values */
  | 'trace_at_base';

export interface RuleOptions {
  mode?: RuleMode;
  /** the shipped thresholds (for trace_at_base: which thresholds a spec moved) */
  base?: ChoiceThresholds;
}

export interface RuleOutcome {
  /** null when the result depends on an input the bundle does not carry */
  choice: Choice | null;
  /** the inputs left open (cp_high, large_yard_case, capped) when they mattered */
  unresolved: string[];
}

const same = (a: number, b: number | undefined): boolean => b !== undefined && Math.abs(a - b) < 1e-9;

/**
 * A shipped value exactly on a threshold a spec moved there: the bundle rounds to 3 significant digits, so the
 * unrounded value can sit on either side (e.g. a deployment need shipped as 0.670 against a 0.67 spec). At the
 * shipped thresholds the trace's own word settles it; at a moved one nothing in the bundle does.
 */
export const onMovedThreshold = (value: number | null, th: number, baseTh: number | undefined, mode: RuleMode): boolean =>
  mode === 'trace_at_base' && value !== null && baseTh !== undefined && !same(th, baseTh) && Math.abs(value - th) < 1e-9;

/** cp_high (R sp_choice_high_cp) under the thresholds; null when the contender / floor status is not implied. */
export function cpHighUnder(i: ChoiceInputs, th: ChoiceThresholds, opt: RuleOptions = {}): boolean | null {
  if (opt.mode === 'trace_at_base' && opt.base && same(th.cp, opt.base.cp) && th.cpStrict === opt.base.cpStrict && i.traced.position !== null) {
    return i.traced.position === 'high';
  }
  if (i.cp === null) return false;
  // a shipped cp exactly on a moved bar: either side (cp_strict is the tie itself and stays decided)
  const tie = !th.cpStrict && onMovedThreshold(i.cp, th.cp, opt.base?.cp, opt.mode ?? 'values');
  const above = th.cpStrict ? i.cp > th.cp : i.cp >= th.cp;
  if (!above) return false;
  if (i.contender === false || i.floorMet === false) return false;
  if (i.contender === true && i.floorMet === true) return tie ? null : true;
  return null;
}

/**
 * The large-yard case under min_signals k (strategic_value.R sp_large_yard_case). From the shipped case at the
 * trace's min_signals: a case that holds keeps holding at a lower bar and holds at a higher one only with
 * enough signals; a case that fails gains it at k = min - 1 only when it failed for want of one signal
 * (large_yard_near); an undecided case can never hold at a higher bar. null: the bundle cannot decide.
 * Undefined input (no large-yard step) is false: the case is not read there.
 */
export function largeUnder(i: ChoiceInputs, k: number, opt: RuleOptions = {}): boolean | null {
  if (i.large === undefined) return false;
  const base = i.largeMin ?? CONFIG_ONLY_DEFAULTS.largeMin;
  if (k === base || (opt.mode === 'trace_at_base' && opt.base && k === opt.base.largeMin)) return i.large === true;
  const n = i.largeN;
  if (i.large === true) {
    if (k <= base) return true;
    return n === null ? null : n >= k;
  }
  if (k > base) return false;
  if (n === null) return null;
  if (n < k) return false;
  if (i.large === false) {
    if (n >= base) return false; // a required signal fails or a veto holds
    if (n === base - 1 && k === base - 1) return i.largeNear;
    return null;
  }
  return null; // undecided at the shipped bar, enough signals for the lower one: not decidable
}

/** high / low of a value against a threshold; null when the value sits exactly on a moved threshold (onMovedThreshold). */
function levelAt(value: number | null, th: number, baseTh: number | undefined, traced: string | null, mode: RuleMode): boolean | null {
  if (mode === 'trace_at_base' && baseTh !== undefined && same(th, baseTh) && (traced === 'high' || traced === 'low')) return traced === 'high';
  if (onMovedThreshold(value, th, baseTh, mode)) return null;
  return value !== null && value >= th;
}

/** Apply the rule to extracted inputs, resolving cp_high, the large-yard case and the cap; unknowns are tried both ways. */
export function choiceRule(i: ChoiceInputs, th: ChoiceThresholds, opt: RuleOptions = {}): RuleOutcome {
  const mode = opt.mode ?? 'values';
  const base = opt.base;
  const dep = i.stage === DEPLOYMENT_STAGE;
  // the rule never reads the position or the large-yard case at Deployment: never leave them open there
  const ch0 = dep ? false : cpHighUnder(i, th, opt);
  const ly0 = dep ? false : largeUnder(i, th.largeMin, opt);
  // the trace's levels stand in for the values at an unmoved threshold: express them as numbers the port compares
  const hn0 = levelAt(i.need, th.need, base?.need, i.traced.need, mode);
  const hp0 = levelAt(i.prize, th.prize, base?.prize, i.traced.prize, mode);
  const dn0 = levelAt(i.dn, th.deployment, base?.deployment, i.traced.deployment, mode);
  const cap0: boolean | null = i.capped;
  const opts = (v: boolean | null): boolean[] => (v === null ? [true, false] : [v]);
  const outcomes = new Set<Choice>();
  const open = new Set<string>();
  for (const hn of opts(hn0))
  for (const hpRaw of opts(hp0))
  for (const dnHigh of opts(dn0))
  for (const ch of opts(ch0))
    for (const ly of opts(ly0))
      for (const cap of hpRaw ? opts(cap0) : [cap0 ?? false]) {
        const c = spChoiceRule(
          {
            stage: i.stage,
            secure: i.secure,
            // levels as values against a unit threshold: the port then reads exactly hn / hp / dn >= threshold
            need: i.need === null ? null : hn ? 1 : 0,
            prize: i.prize === null ? null : hpRaw ? 1 : 0,
            cpHigh: ch,
            pathway: i.pathway,
            dn: dep ? (i.dn === null ? null : dnHigh ? 1 : 0) : null,
            large: ly,
            capped: cap,
          },
          { need: 1, prize: 1, deployment: 1, positionFirst: th.positionFirst },
        );
        outcomes.add(c);
      }
  if (outcomes.size === 1) return { choice: [...outcomes][0], unresolved: [] };
  const tie = (name: string) => `${name} (the shipped value equals the moved threshold at the bundle's 3-digit precision)`;
  if (hn0 === null) open.add(tie('supply_chain_need'));
  if (hp0 === null) open.add(tie('strategic_industrial_value'));
  if (dn0 === null) open.add(tie('deployment_need'));
  if (ch0 === null) open.add(onMovedThreshold(i.cp, th.cp, base?.cp, mode) && i.contender === true && i.floorMet === true ? tie('cp_contender_pct') : 'cp_high (capability floor or contender status not in the bundle)');
  if (ly0 === null) open.add('large_yard_case (signal evidence not in the bundle)');
  if (hp0 !== false && cap0 === null) open.add('siv_cap (market-only cap not in the bundle)');
  return { choice: null, unresolved: [...open] };
}

// ---- reproduction over the shipped bundle ----------------------------------------------------------------------------

/** The shipped choice of a core cell: Secure, the compete or deploy mode, or not_assessed. */
export function shippedChoice(c: Pick<PostureCell, 'posture' | 'mode' | 'deployMode'>): Choice | null {
  if (c.posture === 'S') return 'secure';
  if (c.posture === 'N') return 'not_assessed';
  if (c.posture === 'C') return c.mode;
  if (c.posture === 'D') return c.deployMode;
  return null;
}

/** The shipped choice of a detail cell. */
export function detailChoice(d: Pick<NodeDetail, 'posture' | 'mode' | 'deployMode'>): Choice | null {
  if (d.posture === 'secure') return 'secure';
  if (d.posture === 'not_assessed') return 'not_assessed';
  if (d.posture === 'compete') return d.mode;
  if (d.posture === 'deploy') return d.deployMode;
  return null;
}

export interface ReproductionRow {
  iso3: string;
  tech: string;
  stage: string;
  reproduced: boolean;
  shipped: Choice | null;
  /** the rule re-run on the shipped values (null: unresolved) */
  recomputed: Choice | null;
  /** the rule re-run on the trace's own result words (the logic alone) */
  recomputed_from_trace: Choice | null;
  /** why it is not reproduced (null when it is) */
  reason: string | null;
  /** the reason's category */
  /** which step decided the shipped choice: secure_override (read from the trace), deployment, not_assessed, large_yard, triage */
  path: string;
  category: 'ok' | 'no_detail' | 'trace_mismatch' | 'detail_core_mismatch' | 'rounding_on_threshold' | 'unresolved' | 'rule_mismatch';
}

export interface ReproductionSummary {
  cells: number;
  reproduced: number;
  rate: number;
  /** the rule's logic on the trace's result words */
  reproduced_from_trace: number;
  rate_from_trace: number;
  by_category: Record<string, number>;
  /** reproduced cells by the path that decided them; secure_override and the large-yard case itself are read from the trace, not recomputed */
  by_path: Record<string, { cells: number; reproduced: number }>;
}

const r3 = (x: number | null): string => (x === null ? 'NA' : String(x));

/** Which inputs sit on a threshold and read differently from the trace (the rounding explanation). */
function roundingNotes(i: ChoiceInputs, th: ChoiceThresholds): string[] {
  const out: string[] = [];
  const chk = (name: string, v: number | null, t: number, traced: string | null) => {
    if (v === null || (traced !== 'high' && traced !== 'low')) return;
    const lv = v >= t ? 'high' : 'low';
    if (lv !== traced) out.push(`${name} ${r3(v)} (3 dp) reads ${lv} against ${t} but the trace says ${traced}: the unrounded value sits on the other side of the threshold`);
  };
  chk('supply_chain_need', i.need, th.need, i.traced.need);
  chk('strategic_industrial_value', i.prize, th.prize, i.traced.prize);
  chk('deployment_need', i.dn, th.deployment, i.traced.deployment);
  if (i.cp !== null && i.traced.position !== null) {
    const above = i.cp >= th.cp;
    const tracedAbove = i.traced.position === 'high' || i.traced.position === 'capability_floor_not_met' || i.traced.position === 'not_a_contender';
    if (above !== tracedAbove && i.traced.position !== 'unknown') {
      out.push(`cp_contender_pct ${r3(i.cp)} (3 dp) is ${above ? 'at or above' : 'below'} ${th.cp} but the trace says ${i.traced.position}: the unrounded value sits on the other side of the bar`);
    }
  }
  return out;
}

/** The step that decided a shipped choice (R decision_step). */
export function decisionPath(i: ChoiceInputs, shipped: Choice | null): string {
  if (i.secure) return 'secure_override';
  if (shipped === 'not_assessed') return 'not_assessed';
  if (i.stage === DEPLOYMENT_STAGE) return 'deployment';
  if (shipped === 'watch' && i.large === true && !i.steps.includes('cp_contender_pct')) return 'large_yard';
  return 'triage';
}

/** Re-run the rule for one cell and explain any difference. */
export function reproduceCell(cell: PostureCell, detail: NodeDetail | null, th: ChoiceThresholds, ctx: ExtractContext = {}): ReproductionRow {
  const shipped = shippedChoice(cell);
  const row = (o: Partial<ReproductionRow>): ReproductionRow => ({
    iso3: cell.iso3, tech: cell.tech, stage: cell.stage, reproduced: false, shipped, recomputed: null, recomputed_from_trace: null, reason: null, path: 'unknown', category: 'ok', ...o,
  });
  if (!detail || !detail.trace.length) return row({ reason: 'no detail cell or no decision trace in the detail document', category: 'no_detail' });
  const inp = extractChoiceInputs(detail, cell.stage, ctx);
  const v = choiceRule(inp, th, { mode: 'values' });
  const t = choiceRule(inp, th, { mode: 'trace_at_base', base: th });
  const path = decisionPath(inp, shipped);
  const base = { recomputed: v.choice, recomputed_from_trace: t.choice, path };
  const dChoice = detailChoice(detail);
  if (inp.traced.choice !== shipped) {
    return row({ ...base, reason: `the trace ends in ${inp.traced.choice ?? 'no choice step'} but the core ships ${shipped ?? 'no choice'}`, category: 'trace_mismatch' });
  }
  if (dChoice !== shipped) {
    return row({ ...base, reason: `the detail document says ${dChoice ?? 'no choice'} but the core ships ${shipped ?? 'no choice'}`, category: 'detail_core_mismatch' });
  }
  if (v.choice === shipped) return row({ ...base, reproduced: true, reason: null, category: 'ok' });
  if (v.choice === null) {
    return row({ ...base, reason: `the rule cannot be re-run from the bundle: ${v.unresolved.join('; ')} decides between choices`, category: 'unresolved' });
  }
  const notes = roundingNotes(inp, th);
  if (t.choice === shipped && notes.length) {
    return row({ ...base, reason: `recomputed ${v.choice} from the shipped values; ${notes.join('; ')}`, category: 'rounding_on_threshold' });
  }
  return row({
    ...base,
    reason: `the rule port gives ${v.choice} (trace words: ${t.choice ?? 'unresolved'}) where ${shipped} is shipped; inputs need ${r3(inp.need)}, prize ${r3(inp.prize)}${inp.capped ? ' (capped)' : ''}, cp ${r3(inp.cp)} (${inp.traced.position ?? 'no position step'}), pathway ${inp.pathway ?? 'NA'}, large ${String(inp.large)} n=${r3(inp.largeN)}, dn ${r3(inp.dn)}`,
    category: 'rule_mismatch',
  });
}

/** The market-only cap of each node from the core (meta.yf_nodes, the whole-node layer). */
export function cappedNodes(core: Pick<PostureCore, 'meta'>): Map<string, boolean> {
  const out = new Map<string, boolean>();
  for (const n of Object.values(core.meta.yf_nodes)) if (n.layer === 'stage') out.set(`${n.tech}|${n.stage}`, n.cap === 'market_only');
  return out;
}

/** Re-run the rule for every core cell from its detail cell. details: ISO3 -> {cells}. */
export function reproduceShippedChoices(
  core: Pick<PostureCore, 'meta' | 'cells'>,
  details: Record<string, { cells: NodeDetail[] }>,
  th: ChoiceThresholds = thresholdsFromCore(core),
): { rows: ReproductionRow[]; summary: ReproductionSummary } {
  const caps = cappedNodes(core);
  const index = new Map<string, NodeDetail>();
  for (const [iso, d] of Object.entries(details)) for (const c of d.cells) index.set(cellKey(iso, c.tech, c.stage), c);
  const rows = core.cells.map((c) => reproduceCell(c, index.get(cellKey(c.iso3, c.tech, c.stage)) ?? null, th, { capped: caps.get(`${c.tech}|${c.stage}`) ?? null }));
  const by: Record<string, number> = {};
  for (const r of rows) by[r.category] = (by[r.category] ?? 0) + 1;
  const n = rows.length;
  const ok = rows.filter((r) => r.reproduced).length;
  const okT = rows.filter((r) => r.recomputed_from_trace !== null && r.recomputed_from_trace === r.shipped).length;
  const byPath: Record<string, { cells: number; reproduced: number }> = {};
  for (const r of rows) {
    const p = (byPath[r.path] ??= { cells: 0, reproduced: 0 });
    p.cells++;
    if (r.reproduced) p.reproduced++;
  }
  return { rows, summary: { cells: n, reproduced: ok, rate: n ? ok / n : 0, reproduced_from_trace: okT, rate_from_trace: n ? okT / n : 0, by_category: by, by_path: byPath } };
}
