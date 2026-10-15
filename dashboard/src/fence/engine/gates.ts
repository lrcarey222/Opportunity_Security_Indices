// Yards & Fences decision engine: the three gates.
//
// Each gate answers its own question with tri-state logic (yes / no / unknown). An unknown input
// leaves a test unknown unless the answer is the same at both ends of the input's range, so partial
// evidence can still settle a test. There is no weighted score anywhere in this file.

import { INPUT_DEFS, optionLabel } from './inputs.ts';
import { MONTHS, RULES } from './rules.ts';
import { UNKNOWN } from './types.ts';
import type {
  Assessment,
  GateSet,
  InputId,
  InputRef,
  LargeYardGate,
  MediumYardGate,
  Predicate,
  RationaleStrength,
  SmallScope,
  SmallYardGate,
  Tri,
  Yard,
  YardVariant,
} from './types.ts';

// ── Tri-state helpers ────────────────────────────────────────────────────────────────────────────

export function and3(...xs: Tri[]): Tri {
  if (xs.includes('no')) return 'no';
  return xs.every((x) => x === 'yes') ? 'yes' : 'unknown';
}

export function or3(...xs: Tri[]): Tri {
  if (xs.includes('yes')) return 'yes';
  return xs.every((x) => x === 'no') ? 'no' : 'unknown';
}

function val(a: Assessment, id: InputId): string | number {
  return a.inputs[id].value as string | number;
}

function known(a: Assessment, id: InputId): boolean {
  return a.inputs[id].value !== UNKNOWN;
}

export function ref(a: Assessment, id: InputId): InputRef {
  const iv = a.inputs[id];
  return { id, value: iv.value as string | number, provenance: iv.provenance };
}

function pred(
  id: string,
  label: string,
  state: Tri,
  detail: string,
  inputs: InputRef[],
  opts: { marginal?: boolean; applicable?: boolean } = {},
): Predicate {
  return { id, label, state, detail, inputs, marginal: opts.marginal ?? false, applicable: opts.applicable ?? true };
}

const lbl = (a: Assessment, id: InputId) => optionLabel(id, val(a, id)).toLowerCase();
const pct = (x: number) => `${Math.round(x * 100)}%`;

// ── Applicability ────────────────────────────────────────────────────────────────────────────────

/** The digital path applies to control layers and to anything not known to be passive. */
export function digitalApplies(a: Assessment): boolean {
  return a.context.layer === 'control' || val(a, 'digitallyActive') !== 'no';
}

/** The physical path applies to everything except a pure control layer (software is not withheld
 *  by a mining cartel; its risk is command and access). */
export function physicalApplies(a: Assessment): boolean {
  return a.context.layer !== 'control';
}

// ── Small yard ───────────────────────────────────────────────────────────────────────────────────

function acutePredicate(a: Assessment): Predicate {
  const c = val(a, 'consequence');
  const inputs = [ref(a, 'consequence')];
  if (c === UNKNOWN)
    return pred('acute', 'Acute consequence', 'unknown', 'The consequence of disruption has not been assessed.', inputs);
  const yes = (RULES.acuteConsequence as readonly string[]).includes(String(c));
  return pred(
    'acute',
    'Acute consequence',
    yes ? 'yes' : 'no',
    yes
      ? `Disruption would have a ${lbl(a, 'consequence')} consequence for a critical function.`
      : `A ${lbl(a, 'consequence')} consequence does not reach the high-fence threshold, however concentrated supply is.`,
    inputs,
  );
}

function digitalPredicate(a: Assessment): Predicate {
  const applicable = digitalApplies(a);
  const inputs = [ref(a, 'digitallyActive'), ref(a, 'remoteControl'), ref(a, 'timeHorizon')];
  if (!applicable)
    return pred('digital', 'Control-layer vulnerability', 'no', 'Passive: no software, firmware or remote access to exploit.', inputs, {
      applicable: false,
    });
  const da: Tri = a.context.layer === 'control' ? 'yes' : (val(a, 'digitallyActive') as Tri);
  const rc = val(a, 'remoteControl');
  const rcTri: Tri = rc === UNKNOWN ? 'unknown' : rc === 'no' ? 'no' : 'yes';
  const h = val(a, 'timeHorizon');
  const hTri: Tri = h === UNKNOWN ? 'unknown' : (RULES.digitalHorizon as readonly string[]).includes(String(h)) ? 'yes' : 'no';
  const state = and3(da, rcTri, hTri);
  let detail: string;
  if (state === 'yes')
    detail =
      rc === 'limited'
        ? 'An external actor could exert limited control (commands, updates or data access) over this layer.'
        : 'An external actor could issue commands, push updates, disable functions or reach operating data.';
  else if (rcTri === 'no') detail = 'No external command, update or data path has been identified.';
  else if (hTri === 'no') detail = 'Any harm through the control layer would take years to become intolerable.';
  else if (da === 'no') detail = 'The capability is passive.';
  else detail = 'Whether an external actor could control this layer is not yet established.';
  return pred('digital', 'Control-layer vulnerability', state, detail, inputs, { marginal: rc === 'limited' });
}

function coercionPredicate(a: Assessment): Predicate {
  const iv = a.inputs.nonAlliedTopShare;
  const inputs = [ref(a, 'nonAlliedTopShare')];
  if (iv.value === UNKNOWN)
    return pred('coercion', 'Coercive concentration', 'unknown', 'The largest non-coalition supplier share is unknown.', inputs);
  const s = iv.value;
  const assumed = iv.provenance === 'judgment' || iv.provenance === 'scenario';
  const who = iv.subject && !assumed ? `${iv.subject} holds` : assumed ? 'The largest non-coalition supplier is assumed to hold' : 'The largest non-coalition supplier holds';
  const of = (!assumed && iv.measure) || 'world supply';
  const yes = s >= RULES.coercion.yes;
  const marginal = yes ? s < RULES.coercion.yes + 0.05 : s >= RULES.coercion.marginal;
  return pred(
    'coercion',
    'Coercive concentration',
    yes ? 'yes' : 'no',
    yes
      ? `${who} ${pct(s)} of ${of}: above the ${pct(RULES.coercion.yes)} screening line for coercive leverage.`
      : `${who} ${pct(s)} of ${of}, below the ${pct(RULES.coercion.yes)} screening line; concentration alone does not create coercive leverage.`,
    inputs,
    { marginal },
  );
}

function substitutionPredicate(a: Assessment): Predicate {
  const s = val(a, 'substitutability');
  const inputs = [ref(a, 'substitutability')];
  if (s === UNKNOWN) return pred('substitution', 'Hard to substitute', 'unknown', 'Substitutability has not been assessed.', inputs);
  const yes = s === 'hard' || s === 'impossible';
  return pred(
    'substitution',
    'Hard to substitute',
    yes ? 'yes' : 'no',
    yes ? `Substitution is ${lbl(a, 'substitutability')}.` : `Substitution is ${lbl(a, 'substitutability')}, so users have a way around a cutoff.`,
    inputs,
    { marginal: s === 'moderate' || s === 'hard' },
  );
}

type Bound = 'low' | 'high';

/** Months to restore supply minus months of cover, at the open (low) or security (high) end of
 *  every unknown input. */
function timingGap(a: Assessment, bound: Bound): number {
  const pick = <K extends 'timeHorizon' | 'qualificationTime' | 'reconstitutionTime' | 'stockpile'>(k: K): number => {
    const table = MONTHS[k] as Record<string, number>;
    const v = val(a, k);
    if (v !== UNKNOWN) return table[String(v)];
    const xs = Object.values(table);
    // larger restore time and smaller cover push toward leverage
    const security = k === 'qualificationTime' || k === 'reconstitutionTime' ? Math.max(...xs) : Math.min(...xs);
    const open = k === 'qualificationTime' || k === 'reconstitutionTime' ? Math.min(...xs) : Math.max(...xs);
    return bound === 'high' ? security : open;
  };
  const q = pick('qualificationTime');
  const r = pick('reconstitutionTime');
  const surge = val(a, 'surgeCapacity');
  const covers = surge === UNKNOWN ? bound === 'low' : surge === 'covers';
  const restore = covers ? q : Math.max(q, r);
  return restore - (pick('timeHorizon') + pick('stockpile'));
}

function timingPredicate(a: Assessment): Predicate {
  const inputs = (['timeHorizon', 'qualificationTime', 'reconstitutionTime', 'stockpile', 'surgeCapacity'] as InputId[]).map((id) =>
    ref(a, id),
  );
  const lo = timingGap(a, 'low');
  const hi = timingGap(a, 'high');
  const state: Tri = lo > 0 ? 'yes' : hi <= 0 ? 'no' : 'unknown';
  if (state === 'unknown')
    return pred(
      'timing',
      'Cannot be remedied in time',
      'unknown',
      'Whether supply could be restored before harm becomes intolerable depends on inputs not yet assessed.',
      inputs,
    );
  const gap = lo; // known inputs: lo === hi
  const marginal = Math.abs(gap) <= RULES.timingMarginalMonths;
  const fmt = (m: number) => (m >= 12 ? `${Math.round(m / 12)} year${Math.round(m / 12) === 1 ? '' : 's'}` : `${Math.round(m)} months`);
  return pred(
    'timing',
    'Cannot be remedied in time',
    state,
    state === 'yes'
      ? `Restoring trusted supply would take about ${fmt(gap)} longer than inventories and the time to harm can bridge.`
      : `Trusted supply could be restored, or inventories could bridge the gap, before harm becomes intolerable.`,
    inputs,
    { marginal },
  );
}

export function evaluateSmallYard(a: Assessment): SmallYardGate {
  const acute = acutePredicate(a);
  const digital = digitalPredicate(a);
  const coercion = coercionPredicate(a);
  const substitution = substitutionPredicate(a);
  const timing = timingPredicate(a);
  const pApplicable = physicalApplies(a);
  const pState = pApplicable ? and3(coercion.state, substitution.state, timing.state) : 'no';
  const physical = pred(
    'physical',
    'Physical chokepoint',
    pState,
    !pApplicable
      ? 'A control layer is not withheld like a material; its risk is command and access.'
      : pState === 'yes'
        ? 'Concentration, poor substitutes and slow replacement reinforce one another: coercive leverage exists.'
        : pState === 'no'
          ? 'At least one leg of coercive leverage (concentration, substitution, timing) is missing.'
          : 'Coercive leverage cannot be confirmed or ruled out on the evidence entered.',
    [...coercion.inputs, ...substitution.inputs, ...timing.inputs],
    { applicable: pApplicable, marginal: [coercion, substitution, timing].some((p) => p.marginal && p.state !== 'unknown') },
  );
  const state = and3(acute.state, or3(digital.state, physical.state));

  let scope: SmallScope | null = null;
  if (digital.state === 'yes') {
    const sep = val(a, 'separability');
    scope =
      sep === 'separable'
        ? 'control-layer'
        : sep === 'partial'
          ? 'control-plus-integrated'
          : sep === 'inseparable'
            ? 'whole-product'
            : 'unclear';
  } else if (physical.state === 'yes') scope = 'input';

  return { state, acute, digital, physical, physicalParts: { coercion, substitution, timing }, scope };
}

// ── Medium yard ──────────────────────────────────────────────────────────────────────────────────

function positionPredicate(a: Assessment): Predicate {
  const iv = a.inputs.homeShare;
  const comp = val(a, 'complementaryCapabilities');
  const inputs = [ref(a, 'homeShare'), ref(a, 'complementaryCapabilities')];
  if (iv.value === UNKNOWN)
    return pred('position', 'Present competitive position', 'unknown', "The country's share of world supply is unknown.", inputs);
  const s = iv.value;
  const assumed = iv.provenance === 'judgment' || iv.provenance === 'scenario';
  const country = assumed ? `${a.context.country} (assumed)` : a.context.country;
  const of = (!assumed && iv.measure) || 'world supply';
  if (s >= RULES.position.strong)
    return pred('position', 'Present competitive position', 'yes', `${country} holds ${pct(s)} of ${of}: an established position.`, inputs, {
      marginal: s < RULES.position.strong + 0.02,
    });
  if (s >= RULES.position.moderate) {
    const st: Tri = comp === UNKNOWN ? 'unknown' : comp === 'no' ? 'no' : 'yes';
    return pred(
      'position',
      'Present competitive position',
      st,
      st === 'yes'
        ? `${country} holds ${pct(s)} of ${of}, a foothold backed by complementary capabilities.`
        : st === 'no'
          ? `${country} holds ${pct(s)} of ${of} without the complementary capabilities to build on it.`
          : `${country} holds ${pct(s)} of ${of}; whether complementary capabilities back it is not assessed.`,
      inputs,
      { marginal: comp === 'partly' },
    );
  }
  return pred(
    'position',
    'Present competitive position',
    'no',
    `${country} holds ${pct(s)} of ${of}: little present position to defend (a catch-up or leap case must rest on learning or a successor technology).`,
    inputs,
    { marginal: s >= RULES.position.moderate - 0.01 },
  );
}

type Level = 'strong' | 'moderate' | 'weak';
const LEARN_IDS = ['processKnowledge', 'workforceSuppliers', 'adjacentInnovation'] as const;

function learningScore(a: Assessment, bound: Bound): number {
  let s = 0;
  for (const id of LEARN_IDS) {
    const v = val(a, id);
    if (v === 'yes') s += 1;
    else if (v === 'partly') s += 0.5;
    else if (v === UNKNOWN && bound === 'high') s += 1;
  }
  return s;
}

const scoreLevel = (s: number): Level => (s >= 2 ? 'strong' : s >= 1 ? 'moderate' : 'weak');

function learningPredicate(a: Assessment): MediumYardGate['learning'] {
  const inputs = LEARN_IDS.map((id) => ref(a, id));
  const lo = scoreLevel(learningScore(a, 'low'));
  const hi = scoreLevel(learningScore(a, 'high'));
  const level = lo === hi ? lo : UNKNOWN;
  const state: Tri = level === UNKNOWN ? (lo !== 'weak' ? 'yes' : 'unknown') : level === 'weak' ? 'no' : 'yes';
  const yesCount = LEARN_IDS.filter((id) => val(a, id) === 'yes').length;
  const detail =
    level === 'strong'
      ? 'Making it builds process knowledge, skills and suppliers that carry into the next generation.'
      : level === 'moderate'
        ? 'Some industrial learning would be lost with production, but the case is partial.'
        : level === 'weak'
          ? 'Little process knowledge, supplier capability or successor innovation depends on making it.'
          : 'The industrial-learning questions are not fully answered.';
  return {
    ...pred('learning', 'Industrial learning', state, detail, inputs, {
      marginal: level === 'moderate' && yesCount === 0,
    }),
    level,
  };
}

const LEAP_IDS = ['costPerfAdvantage', 'incumbentVulnerable', 'complementaryCapabilities', 'scalePathway', 'beyondNovelty'] as const;

function leapCredible(a: Assessment, bound: Bound): boolean {
  const v = (id: InputId) => {
    const x = val(a, id);
    return x === UNKNOWN ? (bound === 'high' ? 'yes' : 'no') : String(x);
  };
  if (v('beyondNovelty') !== 'yes' || v('costPerfAdvantage') !== 'yes') return false;
  const support = (['incumbentVulnerable', 'complementaryCapabilities', 'scalePathway'] as InputId[]).filter((id) => v(id) !== 'no').length;
  return support >= 2;
}

function leapPredicate(a: Assessment): Predicate {
  const applicable = a.strategies.includes('leapAhead');
  const inputs = LEAP_IDS.map((id) => ref(a, id));
  if (!applicable)
    return pred('leap', 'Credible leap-ahead case', 'no', 'No leap-ahead strategy is being tested.', inputs, { applicable: false });
  const lo = leapCredible(a, 'low');
  const hi = leapCredible(a, 'high');
  const state: Tri = lo ? 'yes' : !hi ? 'no' : 'unknown';
  const partly = (['incumbentVulnerable', 'complementaryCapabilities', 'scalePathway'] as InputId[]).some((id) => val(a, id) === 'partly');
  return pred(
    'leap',
    'Credible leap-ahead case',
    state,
    state === 'yes'
      ? 'A successor technology offers a real advantage, with evidence beyond novelty and a path to scale.'
      : state === 'no'
        ? val(a, 'beyondNovelty') !== 'yes'
          ? 'Promising, but there is not yet evidence beyond novelty.'
          : 'The successor technology lacks a clear advantage or a credible path to scale.'
        : 'The leap-ahead tests are not fully answered.',
    inputs,
    { marginal: state === 'yes' && partly },
  );
}

function premiumPredicate(a: Assessment): LargeYardGate['premium'] {
  const iv = a.inputs.premium;
  const inputs = [ref(a, 'premium')];
  if (iv.value === UNKNOWN)
    return { ...pred('premium', 'Cost of protection', 'unknown', 'The domestic price premium has not been entered.', inputs), band: UNKNOWN };
  const p = iv.value;
  const band = p > RULES.premium.high ? 'high' : p >= RULES.premium.moderate ? 'moderate' : 'low';
  return {
    ...pred(
      'premium',
      'Cost of protection',
      band === 'high' ? 'yes' : 'no',
      band === 'high'
        ? `Insisting on domestic production would add about ${Math.round(p)}% to buyers' costs.`
        : `A ${Math.round(p)}% premium is a ${band} cost of protection.`,
      inputs,
      { marginal: Math.abs(p - RULES.premium.high) <= 5 },
    ),
    band,
  };
}

function strengthOf(position: boolean, learning: Level, leap: boolean): RationaleStrength {
  const n = [position, learning !== 'weak', leap].filter(Boolean).length;
  if (learning === 'strong' || n >= 2) return 'strong';
  return n === 1 ? 'moderate' : 'none';
}

export function evaluateMediumYard(a: Assessment): MediumYardGate & { premium: LargeYardGate['premium'] } {
  const position = positionPredicate(a);
  const learning = learningPredicate(a);
  const leap = leapPredicate(a);
  const premium = premiumPredicate(a);
  // bracket the rationale strength over unknown sub-answers
  const posLo = position.state === 'yes';
  const posHi = position.state !== 'no';
  const lvLo = scoreLevel(learningScore(a, 'low'));
  const lvHi = scoreLevel(learningScore(a, 'high'));
  const leapLo = leap.state === 'yes';
  const leapHi = leap.applicable && leap.state !== 'no';
  const sLo = strengthOf(posLo, lvLo, leapLo);
  const sHi = strengthOf(posHi, lvHi, leapHi);
  const strength: RationaleStrength = sLo === sHi ? sLo : UNKNOWN;

  const decide = (s: RationaleStrength): Tri => {
    if (s === 'strong') return 'yes';
    if (s === 'none') return 'no';
    if (s === 'moderate') return premium.band === UNKNOWN ? 'unknown' : premium.band === 'high' ? 'no' : 'yes';
    return 'unknown';
  };
  const a1 = decide(sLo);
  const a2 = decide(sHi);
  const state: Tri = a1 === a2 ? a1 : 'unknown';
  return { state, position, learning, leap, strength, premium };
}

// ── Large yard and the strict reading ────────────────────────────────────────────────────────────

export function evaluateLargeYard(a: Assessment, medium: ReturnType<typeof evaluateMediumYard>, small: SmallYardGate): LargeYardGate {
  const hhi = a.inputs.hhi.value;
  const share = a.inputs.nonAlliedTopShare.value;
  const concentrationWatch =
    (hhi !== UNKNOWN && hhi >= RULES.concentration.high) || (share !== UNKNOWN && share >= RULES.coercion.marginal);
  const state: Tri =
    small.state === 'yes' || medium.state === 'yes' ? 'no' : small.state === 'no' && medium.state === 'no' ? 'yes' : 'unknown';
  return { state, premium: medium.premium, concentrationWatch };
}

export function evaluateGates(a: Assessment): GateSet {
  const small = evaluateSmallYard(a);
  const mediumFull = evaluateMediumYard(a);
  const large = evaluateLargeYard(a, mediumFull, small);
  const { premium: _premium, ...medium } = mediumFull;
  void _premium;
  const yard: Yard = small.state === 'yes' ? 'small' : medium.state === 'yes' ? 'medium' : 'large';
  let variant: YardVariant;
  if (yard === 'small' && small.digital.state === 'yes' && small.physical.state === 'yes') variant = 'small-both';
  else if (yard === 'small')
    variant =
      small.scope === 'control-layer'
        ? 'small-control'
        : small.scope === 'control-plus-integrated'
          ? 'small-integrated'
          : small.scope === 'whole-product'
            ? 'small-whole'
            : small.scope === 'input'
              ? 'small-input'
              : 'small-unclear';
  else if (yard === 'medium') variant = medium.strength === 'strong' && large.premium.band === 'high' ? 'medium-costly' : 'medium';
  else if (medium.strength === 'moderate' && large.premium.band === 'high') variant = 'large-floor';
  else if (large.concentrationWatch) variant = 'large-diversify';
  else variant = 'large';
  return { small, medium, large, yard, variant };
}

/** Whether an input is relevant to the assessment as configured (used for completeness and gaps). */
/** Whether an input still matters, given the yards the evidence leaves possible. Security inputs
 *  stop mattering once the small yard is ruled out; capability, leap and cost inputs once it is
 *  settled. The consequence and its timing always matter. */
export function isRelevant(a: Assessment, id: InputId, possible: readonly Yard[]): boolean {
  const g = INPUT_DEFS[id].gate;
  const smallCertain = possible.length === 1 && possible[0] === 'small';
  const smallExcluded = !possible.includes('small');
  if (id === 'hhi' || id === 'topSupplierShare' || id === 'alliedShare' || id === 'trl' || id === 'maturity') return false;
  if (g === 'gate1') return true;
  if (g === 'digital') return !smallExcluded && digitalApplies(a) && (id !== 'digitallyActive' || a.context.layer !== 'control');
  if (g === 'physical') return !smallExcluded && physicalApplies(a);
  if (g === 'leap') return !smallCertain && a.strategies.includes('leapAhead');
  return !smallCertain;
}

export { known };
