// Yards & Fences decision engine: classification, confidence, gaps, "what would change the answer",
// and the decision path.
//
// Unknown inputs are never filled. classify() evaluates the gates at the four corners of the
// unknowns (security inputs at their open or security end x capability inputs at their weak or
// strong end). If every corner gives the same yard, the yard is determinate; otherwise the result is
// "indeterminate" and names the yards still possible and the gaps that would decide between them.

import { evaluateGates, isRelevant } from './gates.ts';
import { ALL_INPUT_IDS, INPUT_DEFS, isNumericId, optionLabel } from './inputs.ts';
import { RULES } from './rules.ts';
import { DOMAINS, UNKNOWN } from './types.ts';
import type {
  Assessment,
  ChangeCondition,
  Confidence,
  DecisionNode,
  DecisionReason,
  GateSet,
  InputId,
  InputRef,
  MissingEvidence,
  Predicate,
  Yard,
  YardOutcome,
  YardResult,
  YardVariant,
} from './types.ts';

export const YARD_NAME: Record<YardOutcome, string> = {
  small: 'Small Yard',
  medium: 'Medium Yard',
  large: 'Large Yard',
  mixed: 'Mixed',
  indeterminate: 'Indeterminate',
};

const SECURITY_IDS: InputId[] = [
  'consequence',
  'timeHorizon',
  'digitallyActive',
  'remoteControl',
  'substitutability',
  'qualificationTime',
  'reconstitutionTime',
  'stockpile',
  'surgeCapacity',
  'nonAlliedTopShare',
];
const CAPABILITY_IDS: InputId[] = [
  'homeShare',
  'processKnowledge',
  'workforceSuppliers',
  'adjacentInnovation',
  'complementaryCapabilities',
  'costPerfAdvantage',
  'incumbentVulnerable',
  'scalePathway',
  'beyondNovelty',
  'premium',
];

type End = 'low' | 'high';

/** The value at the open/weak (low) or security/strong (high) end of an input's range. */
export function extremeValue(id: InputId, end: End): string | number {
  if (id === 'nonAlliedTopShare' || id === 'homeShare') return end === 'high' ? 1 : 0;
  if (id === 'premium') return end === 'high' ? 0 : 75;
  if (isNumericId(id)) return end === 'high' ? 1 : 0;
  const d = DOMAINS[id] as readonly string[];
  return end === 'high' ? d[d.length - 1] : d[0];
}

function withValue(a: Assessment, id: InputId, value: string | number): Assessment {
  return {
    ...a,
    inputs: { ...a.inputs, [id]: { ...a.inputs[id], value, provenance: 'scenario' } } as Assessment['inputs'],
  };
}

function resolveUnknowns(a: Assessment, ids: InputId[], end: End): Assessment {
  let out = a;
  for (const id of ids) if (a.inputs[id].value === UNKNOWN) out = withValue(out, id, extremeValue(id, end));
  return out;
}

const YARD_ORDER: Yard[] = ['small', 'medium', 'large'];

/** Every yard reachable by resolving the unknown inputs. */
export function possibleYards(a: Assessment): Yard[] {
  const set = new Set<Yard>();
  for (const s of ['low', 'high'] as End[])
    for (const c of ['low', 'high'] as End[])
      set.add(evaluateGates(resolveUnknowns(resolveUnknowns(a, SECURITY_IDS, s), CAPABILITY_IDS, c)).yard);
  return YARD_ORDER.filter((y) => set.has(y));
}

const sameSet = (x: Yard[], y: Yard[]) => x.length === y.length && x.every((v) => y.includes(v));
const yardList = (ys: Yard[]) => ys.map((y) => YARD_NAME[y]).join(' or ');

// ── Relevance and completeness ───────────────────────────────────────────────────────────────────

function relevantInputs(a: Assessment, possible: Yard[]): InputId[] {
  return ALL_INPUT_IDS.filter((id) => isRelevant(a, id, possible));
}

export function calculateEvidenceCompleteness(a: Assessment, possible = possibleYards(a)): number {
  const rel = relevantInputs(a, possible);
  if (rel.length === 0) return 1;
  return rel.filter((id) => a.inputs[id].value !== UNKNOWN).length / rel.length;
}

// ── Missing evidence ─────────────────────────────────────────────────────────────────────────────

function missingEvidence(a: Assessment, gates: GateSet, possible: Yard[]): MissingEvidence[] {
  const out: MissingEvidence[] = [];
  for (const id of relevantInputs(a, possible)) {
    if (a.inputs[id].value !== UNKNOWN) continue;
    const ifLow = possibleYards(withValue(a, id, extremeValue(id, 'low')));
    const ifHigh = possibleYards(withValue(a, id, extremeValue(id, 'high')));
    const pivotal = !sameSet(ifLow, possible) || !sameSet(ifHigh, possible);
    const def = INPUT_DEFS[id];
    const hi = optionLabel(id, extremeValue(id, 'high'));
    const lo = optionLabel(id, extremeValue(id, 'low'));
    const effect = pivotal
      ? `Pivotal: at "${hi}" the result could be ${yardList(ifHigh)}; at "${lo}", ${yardList(ifLow)}.`
      : `Does not change the yard on its own; filling it raises completeness and confidence.`;
    out.push({ input: id, label: def.label, pivotal, effect, ifLow, ifHigh });
  }
  if (gates.small.digital.state === 'yes' && a.inputs.separability.value === UNKNOWN && !out.some((m) => m.input === 'separability'))
    out.push({
      input: 'separability',
      label: INPUT_DEFS.separability.label,
      pivotal: false,
      effect: 'Decides whether the fence covers the control layer alone or more of the product.',
      ifLow: possible,
      ifHigh: possible,
    });
  for (const m of out)
    if (m.input === 'separability' && gates.small.digital.state === 'yes')
      m.effect = 'Decides whether the fence covers the control layer alone or more of the product.';
  return out.sort((x, y) => Number(y.pivotal) - Number(x.pivotal));
}

// ── What would change the answer ─────────────────────────────────────────────────────────────────

const NUMERIC_PROBES: Partial<Record<InputId, number[]>> = {
  nonAlliedTopShare: [0.3, RULES.coercion.yes, 0.65, 0.85],
  homeShare: [0.01, 0.05, RULES.position.strong + 0.02],
  premium: [5, 20, RULES.premium.high + 10, 60],
};

function candidateValues(id: InputId): (string | number)[] {
  if (isNumericId(id)) return NUMERIC_PROBES[id] ?? [];
  return [...(DOMAINS[id] as readonly string[])];
}

function distance(id: InputId, from: string | number, to: string | number): number {
  if (from === UNKNOWN) return 1;
  if (isNumericId(id)) {
    const span = id === 'premium' ? 75 : 1;
    return (Math.abs(Number(to) - Number(from)) / span) * 4;
  }
  const d = DOMAINS[id] as readonly string[];
  return Math.abs(d.indexOf(String(to)) - d.indexOf(String(from)));
}

const GENERIC: Record<Yard, ChangeCondition[]> = {
  small: [
    {
      toward: 'medium',
      text: 'Toward a smaller fence (Medium or Large Yard) once trusted substitutes, stockpiles or separable control layers mean a severe disruption could actually be survived, not just once alternatives are announced.',
      generic: true,
    },
  ],
  medium: [
    {
      toward: 'large',
      text: 'Toward Large Yard if production becomes commoditised and manufacturing generates little future learning.',
      generic: true,
    },
    {
      toward: 'small',
      text: 'Toward Small Yard if loss of supply would become intolerable before substitutes could be qualified, or a specific control layer creates a separate security concern.',
      generic: true,
    },
  ],
  large: [
    {
      toward: 'medium',
      text: 'Toward Medium Yard if manufacturing experience proves to matter for successor technologies, or a credible leap-ahead opportunity appears.',
      generic: true,
    },
    {
      toward: 'small',
      text: 'Toward Small Yard if a new digital control layer or a genuine chokepoint emerges.',
      generic: true,
    },
  ],
};

function changeConditions(a: Assessment, current: YardOutcome, possible: Yard[]): ChangeCondition[] {
  const out: ChangeCondition[] = [];
  if (current === 'indeterminate' || current === 'mixed') return out;
  const rel = relevantInputs(a, possible);
  const now = (id: InputId) => {
    const from = a.inputs[id].value as string | number;
    return from === UNKNOWN ? 'currently unknown' : `now ${optionLabel(id, from)}`;
  };
  const phrase = (id: InputId, to: string | number) => `${INPUT_DEFS[id].label.toLowerCase()} were ${optionLabel(id, to)} (${now(id)})`;
  const yards = (ys: Yard[]) => ys.map((y) => YARD_NAME[y]).join(' or ');
  // Inputs that can matter for each move: security inputs decide the small yard; capability
  // inputs decide medium against large. Restricting the search keeps it fast and meaningful.
  const pool = (target: Yard): InputId[] =>
    rel.filter((id) => (target === 'small' || current === 'small' ? SECURITY_IDS.includes(id) : CAPABILITY_IDS.includes(id)));
  const moves = (ids: InputId[]) =>
    ids.flatMap((id) => {
      const from = a.inputs[id].value as string | number;
      return candidateValues(id)
        .filter((to) => to !== from)
        .map((to) => ({ id, to, d: distance(id, from, to) + (from === UNKNOWN ? 0.5 : 0) }));
    });

  for (const target of YARD_ORDER) {
    if (target === current) continue;
    const singles = moves(pool(target)).sort((p, q) => p.d - q.d);
    // one change
    const hits: ChangeCondition[] = [];
    for (const m of singles) {
      if (hits.some((h) => h.input === m.id)) continue;
      const p = possibleYards(withValue(a, m.id, m.to));
      if (p.includes(current) || !p.includes(target)) continue;
      hits.push({
        toward: target,
        input: m.id,
        from: optionLabel(m.id, a.inputs[m.id].value as string | number),
        to: optionLabel(m.id, m.to),
        text:
          p.length === 1
            ? `Toward ${YARD_NAME[target]} if ${phrase(m.id, m.to)}.`
            : `Out of the ${YARD_NAME[current]} if ${phrase(m.id, m.to)}: then ${yards(p)}, depending on questions not yet answered.`,
      });
      if (hits.length >= 2) break;
    }
    if (hits.length) {
      for (const h of hits) if (!out.some((o) => o.text === h.text)) out.push(h);
      continue;
    }
    // two changes, closest first
    let found: ChangeCondition | null = null;
    let bestD = Infinity;
    for (let i = 0; i < singles.length; i++)
      for (let j = i + 1; j < singles.length; j++) {
        const p = singles[i];
        const q = singles[j];
        if (p.id === q.id || p.d + q.d >= bestD) continue;
        const ys = possibleYards(withValue(withValue(a, p.id, p.to), q.id, q.to));
        if (ys.includes(current) || !ys.includes(target)) continue;
        bestD = p.d + q.d;
        found = { toward: target, input: p.id, text: `Toward ${YARD_NAME[target]} only if two things changed: ${phrase(p.id, p.to)}, and ${phrase(q.id, q.to)}.` };
      }
    if (found && !out.some((o) => o.text === found.text)) out.push(found);
  }
  if (!out.length)
    out.push({
      toward: current as Yard,
      text: `Robust: no change to any one or two of the answers moves this result out of the ${YARD_NAME[current]}.`,
    });
  out.push(...GENERIC[current]);
  return out;
}

// ── Reasons, posture, warnings ───────────────────────────────────────────────────────────────────

const pct = (x: number) => `${Math.round(x * 100)}%`;

function reasons(a: Assessment, g: GateSet, yard: YardOutcome): DecisionReason[] {
  const r: DecisionReason[] = [];
  const add = (kind: DecisionReason['kind'], p: Predicate | string, inputs: InputId[] = []) =>
    r.push({ kind, text: typeof p === 'string' ? p : p.detail, inputs: typeof p === 'string' ? inputs : p.inputs.map((i) => i.id) });
  const { small, medium, large } = g;
  const share = a.inputs.nonAlliedTopShare;
  const hhi = a.inputs.hhi.value;
  const concentrated = (share.value !== UNKNOWN && share.value >= RULES.coercion.marginal) || (hhi !== UNKNOWN && hhi >= RULES.concentration.high);
  const concText =
    share.value !== UNKNOWN
      ? `${share.subject ?? 'The largest non-coalition supplier'} holds ${pct(share.value)} of ${share.measure ?? 'world supply'}`
      : hhi !== UNKNOWN
        ? `supplier concentration is high (HHI ${hhi.toFixed(2)})`
        : 'supply is concentrated';

  if (yard === 'small') {
    add('driver', small.acute);
    if (small.digital.state === 'yes') add('driver', small.digital);
    if (small.physical.state === 'yes') {
      add('driver', small.physicalParts.coercion);
      add('driver', small.physicalParts.substitution);
      add('driver', small.physicalParts.timing);
    }
    if (small.digital.state === 'yes' && small.physical.state !== 'yes' && concentrated)
      add('not-by', `Not by hardware market share: ${concText}, but the fence follows the control path, not the supplier count.`, ['nonAlliedTopShare']);
    add('not-by', 'Not by a high import share alone.', []);
  } else {
    if (small.acute.state === 'no') add('driver', small.acute);
    else if (small.state === 'no') {
      if (small.digital.applicable) add('driver', small.digital);
      if (small.physical.applicable) {
        const p = [small.physicalParts.coercion, small.physicalParts.substitution, small.physicalParts.timing].find((x) => x.state === 'no');
        add('driver', p ?? small.physical);
      }
    }
    if (concentrated && small.state !== 'yes')
      add('not-by', `Not a high fence despite concentration: ${concText}. Concentration is necessary for coercive leverage, not sufficient.`, [
        'nonAlliedTopShare',
        'hhi',
      ]);
  }
  if (yard === 'medium') {
    for (const p of [medium.position, medium.learning, medium.leap]) if (p.state === 'yes' && p.applicable) add('driver', p);
    if (medium.strength === 'moderate' && large.premium.band !== UNKNOWN) add('context', large.premium);
    add('not-by', "Not by the sector's headline Economic Opportunity score or market size alone.");
  }
  if (yard === 'large') {
    const neg = [medium.position, medium.learning, medium.leap].filter((p) => p.applicable && p.state === 'no');
    for (const p of neg.slice(0, 2)) add('driver', p);
    if (medium.strength === 'moderate' && large.premium.band === 'high') add('driver', large.premium);
    if (large.concentrationWatch) add('context', `${concText.charAt(0).toUpperCase()}${concText.slice(1)}: manage it with diversification, not self-sufficiency.`, ['hhi', 'nonAlliedTopShare']);
    add('not-by', 'Not a judgment that the technology is unimportant: its strategic value lies in cheap, fast, diversified deployment.');
  }
  return r;
}

const POSTURE: Record<YardVariant | 'indeterminate', [string, string]> = {
  'small-control': [
    'Secure the control layer; keep the hardware open',
    'Draw a narrow, high fence around firmware, communications and remote access: trusted-source and cybersecurity requirements, certified replacements and monitoring of the installed base. The rest of the product can stay in an open, competitive market.',
  ],
  'small-integrated': [
    'High fence around the control layer and the hardware tied to it',
    'Until the control function can be separated, the fence must include the hardware it is integrated with. Back separable, certifiable architectures so the fence can shrink.',
  ],
  'small-whole': [
    'High fence around the whole product, for now',
    'The vulnerable function cannot currently be separated, so the product as a whole needs trusted sourcing. Make separation the goal, and narrow the fence when it is achieved.',
  ],
  'small-unclear': [
    'Fence the control function; establish how far it reaches',
    'The vulnerability is real but its boundary is not established: whether the control layer can be isolated decides whether the fence covers firmware alone or the product.',
  ],
  'small-both': [
    'Secure the control layer and remediate the chokepoint',
    'Two problems at once: a control layer an outsider could command, and a physical input that could be withheld. Fence the control layer with trusted-source and cybersecurity requirements, and diversify qualified supply, stockpile and build surge capacity until a cutoff could be survived.',
  ],
  'small-input': [
    'Remediate the chokepoint until a cutoff could be survived',
    'Diversify qualified supply toward trusted producers, hold stockpiles, build surge capacity, and keep going until a prolonged cutoff could be survived. Domestic production is one tool, not the goal.',
  ],
  medium: [
    'Compete while remaining selectively open',
    'Build industrial capability, learning and competitiveness, with gates in the fence for foreign technology, capital or expertise when they deepen domestic capability. Judge support by capability gained, not investment announced.',
  ],
  'medium-costly': [
    'Compete, and make the cost of support explicit',
    'The capability case is strong, but protection is expensive. Prefer support that builds capability (R&D, finance, workforce, supplier development) over measures that raise buyers’ prices, and set a review date.',
  ],
  large: [
    'Keep it open: affordable, diversified access',
    'Default to openness. Use the lightest effective tools for genuine concentration risk, and keep cross-cutting safeguards (forced labour, safety, trade law) separate from yard placement.',
  ],
  'large-diversify': [
    'Keep it open, and diversify supply',
    'No security or learning rationale justifies a high fence, but supply is concentrated. Diversify toward trusted producers, use transparency or procurement tools, and avoid building a full domestic replica.',
  ],
  'large-floor': [
    'Open market with a limited, specified capability floor',
    'The capability case is real but does not justify a high premium. If a domestic floor is kept, state what capability it preserves, how much is enough, what premium buyers should pay, and when it would be reduced.',
  ],
  indeterminate: [
    'Fill the evidence gaps before drawing a fence',
    'The evidence entered is consistent with more than one yard. The pivotal gaps below would settle it; until then, prefer reversible measures that buy information.',
  ],
};

function warnings(a: Assessment, g: GateSet, yard: YardOutcome): string[] {
  const w: string[] = [];
  const s = new Set(a.strategies);
  if (yard === 'medium' || yard === 'indeterminate') {
    if (s.size === 0 && yard === 'medium') w.push('Choose preserve, catch up or leap ahead (or a combination) so the policy package has a direction.');
    if (s.has('preserve') && g.medium.position.state === 'no')
      w.push('Preserve: there is little present position to preserve. Consider catch-up or leap-ahead instead.');
    if (s.has('leapAhead') && g.medium.leap.state !== 'yes')
      w.push('Leap ahead: the case is not yet credible. Support exploration (R&D, demonstration) and set milestones before scaling.');
    if (s.has('catchUp'))
      w.push('Catch up: closing a gap usually depends on learning from the frontier. Keep a gate in the fence for licensing and joint ventures with conditions on capability transfer.');
  }
  if (a.premiumJustification === 'jobs')
    w.push('Jobs and regional development can be legitimate goals, but they are not a yard rationale. Weigh them openly against the premium buyers pay.');
  if (a.premiumJustification === 'security' && g.small.state !== 'yes')
    w.push('You named security as the benefit that justifies a premium, but the security tests were not met. Revisit Gates 1 and 2.');
  if (a.premiumJustification === 'learning' && g.medium.learning.state === 'no')
    w.push('You named industrial learning as the benefit, but the learning questions were answered "no".');
  return w;
}

// ── Confidence ───────────────────────────────────────────────────────────────────────────────────

function decisivePredicates(g: GateSet, yard: YardOutcome): Predicate[] {
  const { small, medium, large } = g;
  const smallBlockers = (): Predicate[] => {
    if (small.acute.state === 'no') return [small.acute];
    const out: Predicate[] = [];
    if (small.digital.applicable) out.push(small.digital);
    if (small.physical.applicable) {
      const p = [small.physicalParts.coercion, small.physicalParts.substitution, small.physicalParts.timing].filter((x) => x.state === 'no');
      out.push(...(p.length === 1 ? p : []));
    }
    return out;
  };
  if (yard === 'small') {
    const out: Predicate[] = [small.acute];
    if (small.digital.state === 'yes') out.push(small.digital);
    if (small.physical.state === 'yes') out.push(small.physicalParts.coercion, small.physicalParts.substitution, small.physicalParts.timing);
    return out;
  }
  const out = smallBlockers();
  const rationales = [medium.position, medium.learning, medium.leap].filter((p) => p.applicable);
  if (yard === 'medium') out.push(...rationales.filter((p) => p.state === 'yes'));
  if (yard === 'large') out.push(...rationales);
  if (medium.strength === 'moderate') out.push(large.premium);
  return out;
}

function confidenceOf(a: Assessment, g: GateSet, yard: YardOutcome, completeness: number): { c: Confidence; why: string[] } {
  const why: string[] = [];
  if (yard === 'indeterminate') return { c: 'low', why: ['The evidence entered is consistent with more than one yard.'] };
  const decisive = decisivePredicates(g, yard);
  const marginal = decisive.filter((p) => p.marginal);
  const inputs = new Map<InputId, InputRef>();
  for (const p of decisive) for (const i of p.inputs) inputs.set(i.id, i);
  const weakAssess = [...inputs.keys()].filter((id) => a.inputs[id].provenance === 'assessment' && a.inputs[id].assessmentConfidence === 'low');
  if (marginal.length) why.push(`${marginal.length} deciding answer${marginal.length > 1 ? 's sit' : ' sits'} near a threshold: ${marginal.map((p) => p.label.toLowerCase()).join(', ')}.`);
  if (weakAssess.length) why.push(`Rests on low-confidence curated assessments: ${weakAssess.map((id) => INPUT_DEFS[id].label.toLowerCase()).join(', ')}.`);
  why.push(`${Math.round(completeness * 100)}% of the relevant inputs are known.`);
  let c: Confidence;
  if (marginal.length >= 2 || completeness < RULES.confidence.lowCompleteness) c = 'low';
  else if (marginal.length === 0 && weakAssess.length === 0 && completeness >= RULES.confidence.highCompleteness) c = 'high';
  else c = 'moderate';
  return { c, why };
}

// ── Decision path ────────────────────────────────────────────────────────────────────────────────

function node(id: string, question: string, p: Predicate, children?: DecisionNode[]): DecisionNode {
  return { id, question, answer: p.applicable ? p.state : 'n/a', marginal: p.marginal, detail: p.detail, inputs: p.inputs, children };
}

export function generateDecisionPath(a: Assessment, g: GateSet, outcome: YardOutcome, scopeLabels?: string[]): DecisionNode[] {
  const { small, medium, large } = g;
  const path: DecisionNode[] = [];
  path.push(node('acute', 'Could disruption or foreign control cause acute harm?', small.acute));
  if (small.acute.state !== 'no') {
    if (small.digital.applicable) {
      const sep = a.inputs.separability.value;
      const children: DecisionNode[] = [];
      if (small.digital.state === 'yes')
        children.push({
          id: 'separable',
          question: 'Can the vulnerable layer be isolated?',
          answer: sep === UNKNOWN ? 'unknown' : sep === 'inseparable' ? 'no' : 'yes',
          marginal: sep === 'partial',
          detail: sep === UNKNOWN ? 'Not established.' : optionLabel('separability', sep),
          inputs: [{ id: 'separability', value: sep as string, provenance: a.inputs.separability.provenance }],
        });
      path.push(node('digital', 'Is there a digitally active control-layer vulnerability?', small.digital, children));
    }
    if (small.physical.applicable)
      path.push(
        node('physical', 'Is there a physical chokepoint with coercive leverage?', small.physical, [
          node('coercion', 'Does a potentially coercive supplier control most of supply?', small.physicalParts.coercion),
          node('substitution', 'Is it hard to substitute?', small.physicalParts.substitution),
          node('timing', 'Would harm arrive before trusted supply could be restored?', small.physicalParts.timing),
        ]),
      );
  }
  if (small.state !== 'yes') {
    const kids = [node('position', 'Is there a present position worth defending?', medium.position), node('learning', 'Would losing production cost meaningful industrial learning?', medium.learning)];
    if (medium.leap.applicable) kids.push(node('leap', 'Is there a credible leap-ahead opportunity?', medium.leap));
    if (medium.strength === 'moderate') kids.push(node('premium', 'Is the cost of protection high?', large.premium));
    path.push({
      id: 'medium',
      question: 'Is this a capability worth building or preserving?',
      answer: medium.state,
      detail:
        medium.strength === 'strong'
          ? 'A strong capability case.'
          : medium.strength === 'moderate'
            ? 'One capability rationale holds; the cost of protection decides.'
            : medium.strength === 'none'
              ? 'No capability rationale holds.'
              : 'The capability case is not settled.',
      inputs: [],
      children: kids,
    });
  }
  path.push({
    id: 'outcome',
    question: 'Result',
    answer: outcome === 'indeterminate' ? 'unknown' : 'yes',
    detail: outcome === 'small' && scopeLabels?.length ? `${YARD_NAME.small}: ${scopeLabels.join('; ')}` : YARD_NAME[outcome],
    inputs: [],
    outcome,
  });
  return path;
}

const SCOPE_LABEL: Record<string, string> = {
  'control-layer': 'applies to the control layer only',
  'control-plus-integrated': 'covers the control layer and the hardware tied to it',
  'whole-product': 'must cover the whole product until the control layer can be separated',
  input: 'applies to the physical input',
  unclear: 'boundary of the control layer not yet established',
};

// ── Entry point ──────────────────────────────────────────────────────────────────────────────────

export function classify(a: Assessment): YardResult {
  const gates = evaluateGates(a);
  const possible = possibleYards(a);
  const yard: YardOutcome = possible.length === 1 ? possible[0] : 'indeterminate';
  const leaning =
    yard === 'indeterminate'
      ? evaluateGates(resolveUnknowns(resolveUnknowns(a, SECURITY_IDS, 'low'), CAPABILITY_IDS, 'low')).yard
      : undefined;
  const completeness = calculateEvidenceCompleteness(a, possible);
  const { c, why } = confidenceOf(a, gates, yard, completeness);
  const scope = yard === 'small' ? gates.small.scope ?? undefined : undefined;
  const scopeLabels = scope ? [SCOPE_LABEL[scope]] : undefined;
  const postureKey: YardVariant | 'indeterminate' = yard === 'indeterminate' ? 'indeterminate' : gates.variant;
  const [recommendedPosture, postureDetail] = POSTURE[postureKey];
  return {
    yard,
    leaning,
    possible,
    variant: gates.variant,
    scope,
    scopeLabels,
    confidence: c,
    confidenceWhy: why,
    evidenceCompleteness: completeness,
    reasons: reasons(a, gates, yard),
    missingEvidence: missingEvidence(a, gates, possible),
    changeConditions: changeConditions(a, yard, possible),
    recommendedPosture,
    postureDetail,
    strategies: a.strategies,
    warnings: warnings(a, gates, yard),
    gates,
    path: generateDecisionPath(a, gates, yard, scopeLabels),
  };
}

export { withValue };
