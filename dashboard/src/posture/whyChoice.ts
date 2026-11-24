// "Why this result?" for one supply-chain step (external review readiness, 2026-11-23): the shipped decision
// trace (detail cells' trace[] {step, test, value, threshold, result}, choice_possible, pivotal, reason) read
// back in the order of the choice rule (R decision_engine.R sp_choice_rule; the full TypeScript port lives in
// review/qc/choiceRule.ts, which this file does not import so the review code stays out of the profile chunk):
//   secure -> Deployment (deployment need) -> not assessed -> large yard (unless a top-quarter position) ->
//   high prize & top-quarter position: Preserve -> high prize & credible pathway: Catch Up -> high prize & high
//   need: Partner -> high prize: Watch -> high need & top-quarter position: Preserve -> high need: Partner -> Watch.
// Every level is the trace's own result word and every number the trace's value and bar: nothing is re-derived
// or guessed. When the branch the trace implies is not the shipped choice, the engine's own sentence is shown
// instead, with no "why not". "Why not" names the nearest alternative: the one the evidence also allows
// (choice_possible, with the pivotal test near its bar), else the alternative the rule's failed test rules out.
// Pure: no React, no IO.

import { CHOICE_NAME, RATIONALE_NAME } from '../content/vocabulary.ts';
import { DEPLOYMENT_STAGE } from '../lib/stages.ts';
import type { PostureMeta, SecurityRationale, Verb } from './contract.ts';
import type { NodeDetail, TraceStep } from './detail.ts';

export type ChoiceOrNa = Verb | 'not_assessed';

export interface WhyChoice {
  /** the shipped choice the explanation is for */
  choice: ChoiceOrNa | null;
  /** why the step has this choice, in one sentence with the trace's numbers */
  because: string;
  /** the nearest alternative and why it was not chosen; absent when the trace names none */
  whyNot?: { alt: string; text: string };
}

type Detail = Pick<NodeDetail, 'stage' | 'trace' | 'reason' | 'choicePossible' | 'pivotal' | 'notAssessedReason' | 'posture' | 'mode' | 'deployMode'>;

const CHOICES: readonly ChoiceOrNa[] = ['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed'];
const isChoice = (v: unknown): v is ChoiceOrNa => typeof v === 'string' && (CHOICES as readonly string[]).includes(v);

const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : typeof v === 'string' && v !== '' && Number.isFinite(Number(v)) ? Number(v) : null);
const f2 = (v: number | null): string => (v === null ? 'not measured' : v.toFixed(2));
const cap = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);
const altName = (c: ChoiceOrNa): string => (c === 'not_assessed' ? 'Not assessed' : CHOICE_NAME[c]);

/** The shipped choice: the trace's choice step, else the detail's posture and mode. */
export function shippedChoice(d: Detail): ChoiceOrNa | null {
  const t = d.trace.find((s) => s.test === 'choice')?.result;
  if (isChoice(t)) return t;
  if (d.posture === 'secure') return 'secure';
  if (d.posture === 'compete') return d.mode;
  if (d.posture === 'deploy') return d.deployMode;
  if (d.posture === 'not_assessed') return 'not_assessed';
  return null;
}

interface Read {
  step: TraceStep | null;
  result: string | null;
  value: number | null;
  bar: number | null;
}

function read(d: Detail, fallbackBar: number | null, ...tests: string[]): Read {
  const step = tests.map((t) => d.trace.find((s) => s.test === t)).find(Boolean) ?? null;
  return { step, result: step?.result ?? null, value: num(step?.value), bar: num(step?.threshold) ?? fallbackBar };
}

/** "(need 0.81, bar 0.67)" */
const nums = (label: string, r: Read): string => `(${label} ${f2(r.value)}${r.bar !== null ? `, bar ${f2(r.bar)}` : ''})`;

/** The position among producers in words, from the trace's result word. */
function positionWords(r: Read): string {
  switch (r.result) {
    case 'high':
      return `the position is in the top quarter of producers ${nums('position', r)}`;
    case 'not_top_quarter':
      return `the position is below the top quarter of producers ${nums('position', r)}`;
    case 'not_a_contender':
      return 'the country is too small a producer to be ranked';
    case 'capability_floor_not_met':
      return 'production capability is below the floor needed to be ranked';
    default:
      return 'the position among producers is not measured';
  }
}

/** The words of a test near its bar, for a close call ("supply risk (need 0.69, bar 0.67)"). */
function testNear(d: Detail, test: string): string | null {
  const s = d.trace.find((x) => x.test === test);
  const v = num(s?.value);
  const bar = num(s?.threshold);
  if (!s || v === null || bar === null) return null;
  const pair = (label: string) => `(${label} ${f2(v)}, bar ${f2(bar)})`;
  switch (test) {
    case 'supply_chain_need':
      return `supply risk ${pair('need')}`;
    case 'strategic_industrial_value':
    case 'industrial_opportunity':
      return `strategic value ${pair('value')}`;
    case 'cp_contender_pct':
      return `the position among producers ${pair('position')}`;
    case 'deployment_need':
      return `deployment need ${pair('need')}`;
    case 'large_yard_case':
      return `the open-market test (${v} signals, bar ${bar})`;
    default:
      return null;
  }
}

/** The alternative the evidence also allows (choice_possible), with the pivotal test near its bar. */
function closeCall(d: Detail, choice: ChoiceOrNa): WhyChoice['whyNot'] | undefined {
  const alts = (d.choicePossible ?? '').split('|').filter((x): x is ChoiceOrNa => isChoice(x) && x !== choice && x !== 'not_assessed');
  if (!alts.length) return undefined;
  const near = (d.pivotal ?? '').split(';').map((t) => testNear(d, t)).filter((x): x is string => x !== null);
  const alt = altName(alts[0]);
  return {
    alt,
    // the summary already reads "Why not <alt>?": the text says only why it was close
    text: near.length ? `Close call: ${near.join(' and ')} ${near.length > 1 ? 'are' : 'is'} near the bar.` : `Close call: ${alt} was also possible on the same evidence.`,
  };
}

/**
 * Why a step has its choice, and why not the nearest alternative, from its shipped decision trace. `meta`
 * gives the bars where a trace step does not state its threshold.
 */
export function whyChoice(d: Detail, meta: Pick<PostureMeta, 'thresholds'>): WhyChoice {
  const t = meta.thresholds;
  const choice = shippedChoice(d);
  const engine = d.reason ? cap(d.reason.trim()) : null;
  const fallback = (): WhyChoice => ({ choice, because: engine ?? 'The decision trace for this step is not available.' });
  if (!d.trace.length) return fallback();

  const sec = read(d, null, 'secure_override');
  const dn = read(d, t.deployment, 'deployment_need');
  const ly = read(d, null, 'large_yard_case');
  const need = read(d, t.need, 'supply_chain_need');
  const prize = read(d, t.prize, 'strategic_industrial_value', 'industrial_opportunity');
  const pos = read(d, t.cp, 'cp_contender_pct');
  const pw = read(d, null, 'pathway_credible');
  const dep = d.stage === DEPLOYMENT_STAGE || (!!dn.step && !need.step);

  const hn = need.result === 'high';
  const hp = prize.result === 'high';
  const ch = pos.result === 'high';
  const pathway = pw.result === 'yes';

  let implied: ChoiceOrNa;
  let because: string;
  let natural: WhyChoice['whyNot'] | undefined;
  const needW = `supply risk is ${hn ? 'high' : 'below the bar'} ${nums('need', need)}`;
  const prizeW = `strategic value is ${hp ? 'high' : 'below the bar'} ${nums('value', prize)}`;

  if (sec.result === 'yes') {
    implied = 'secure';
    const code = typeof sec.step?.value === 'string' ? sec.step.value : null;
    const why = code && code in RATIONALE_NAME ? ` (${RATIONALE_NAME[code as SecurityRationale].toLowerCase()})` : '';
    because = `An acute vulnerability was found${why}, so Secure applies whatever the economic prize.`;
  } else if (dep) {
    if (dn.result === 'high') {
      implied = 'accelerate';
      because = `Deployment need is at or above the bar ${nums('need', dn)}: the energy system needs more of it. This says nothing about where the equipment is made.`;
    } else if (dn.result === 'low') {
      implied = 'watch';
      because = `Deployment need is below the bar ${nums('need', dn)}, so no major acceleration is needed.`;
    } else {
      implied = 'not_assessed';
      because = d.notAssessedReason ?? 'Deployment need is not measured, so this step is not assessed.';
    }
  } else if (need.result === 'not_assessed' || prize.result === 'not_assessed' || (!need.step && !prize.step && ly.result !== 'yes')) {
    implied = 'not_assessed';
    because = d.notAssessedReason ?? 'A measure the rule needs is missing, so this step is not assessed.';
  } else if (ly.result === 'yes' && !ch) {
    implied = 'watch';
    because = `It reads as a large, open market (large yard: ${ly.value ?? '–'} open-market signals, bar ${ly.bar ?? '–'}), where buying competitively is the default.`;
  } else if (hp && ch) {
    implied = 'preserve';
    because = `${cap(prizeW)} and ${positionWords(pos)}, so the default is to keep and extend it.`;
  } else if (hp && pathway) {
    implied = 'catch_up';
    because = `${cap(prizeW)} and there is a credible domestic pathway (capability anchor), but ${positionWords(pos)}, so the default is to build from that base.`;
    natural = { alt: CHOICE_NAME.preserve, text: `${cap(positionWords(pos))}.` };
  } else if (hp && hn) {
    implied = 'partner';
    because = `${cap(needW)} but ${positionWords(pos)} and there is no credible domestic pathway, so building at home is not the default.`;
    natural = { alt: CHOICE_NAME.catch_up, text: 'No credible domestic pathway (capability anchor) was found.' };
  } else if (hp) {
    implied = 'watch';
    because = `${cap(prizeW)}, but ${needW}, ${positionWords(pos)} and there is no credible domestic pathway, so it is left open.`;
    natural = { alt: CHOICE_NAME.catch_up, text: 'No credible domestic pathway (capability anchor) was found.' };
  } else if (hn && ch) {
    implied = 'preserve';
    because = `${cap(needW)} and ${positionWords(pos)}, so the default is to keep and extend it, although ${prizeW}.`;
  } else if (hn) {
    implied = 'partner';
    because = `${cap(needW)} but ${prizeW}, so reliable partners come before building at home.`;
    natural = { alt: CHOICE_NAME.catch_up, text: `Building at home needs strategic value at or above the bar ${nums('value', prize)}.` };
  } else {
    implied = 'watch';
    because = `Both supply risk ${nums('need', need)} and strategic value ${nums('value', prize)} are below their bars, so it is left open.`;
    natural = { alt: CHOICE_NAME.partner, text: `Partner needs supply risk at or above the bar ${nums('need', need)}.` };
  }

  // the trace must reproduce the shipped choice; otherwise show the engine's own words, never a guess
  if (choice !== null && implied !== choice) return fallback();
  const whyNot = (choice ? closeCall(d, choice) : undefined) ?? natural;
  return whyNot ? { choice, because, whyNot } : { choice, because };
}
