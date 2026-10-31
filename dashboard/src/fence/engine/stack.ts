// Yards & Fences decision engine: multi-layer systems and scenario transitions.

import { classify, YARD_NAME } from './classify.ts';
import { INPUT_DEFS, optionLabel } from './inputs.ts';
import type { Assessment, DecisionNode, InputId, Tri, Yard, YardOutcome, YardResult } from './types.ts';

/** A system's yard: one yard only if every layer can only be that yard; mixed if no yard is possible
 *  for every layer (whatever the unknowns resolve to, the system needs different fences); otherwise
 *  undecided. */
export function stackOutcome(results: Pick<YardResult, 'possible'>[]): YardOutcome {
  if (!results.length) return 'indeterminate';
  const all: Yard[] = ['small', 'medium', 'large'];
  const common = all.filter((y) => results.every((r) => r.possible.includes(y)));
  if (common.length === 0) return 'mixed';
  return common.length === 1 && results.every((r) => r.possible.length === 1) ? common[0] : 'indeterminate';
}

export interface StackLayerResult {
  componentId: string;
  label: string;
  result: YardResult;
}

export interface StackResult {
  outcome: YardOutcome;
  layers: StackLayerResult[];
  /** Determinate layers per yard. */
  byYard: Record<Yard, StackLayerResult[]>;
  indeterminate: StackLayerResult[];
}

/** Evaluate each layer of a system on its own evidence. A broad technology is never forced into one
 *  yard: if determinate layers land in different yards, the system is "mixed". */
export function evaluateStack(layers: { componentId: string; label: string; assessment: Assessment }[]): StackResult {
  const out = layers.map((l) => ({ componentId: l.componentId, label: l.label, result: classify(l.assessment) }));
  const byYard: Record<Yard, StackLayerResult[]> = { small: [], medium: [], large: [] };
  const indeterminate: StackLayerResult[] = [];
  for (const l of out) {
    const y = l.result.yard;
    if (y === 'small' || y === 'medium' || y === 'large') byYard[y].push(l);
    else indeterminate.push(l);
  }
  const outcome = stackOutcome(out.map((l) => l.result));
  return { outcome, layers: out, byYard, indeterminate };
}

export interface Transition {
  changed: boolean;
  from: YardOutcome;
  to: YardOutcome;
  headline: string;
  explanation: string;
  /** The decision node whose answer changed first. */
  pivot?: string;
  changedInputs: InputId[];
}

function flatten(nodes: DecisionNode[]): DecisionNode[] {
  return nodes.flatMap((n) => [n, ...flatten(n.children ?? [])]);
}

const TRI: Record<Tri | 'n/a', string> = { yes: 'YES', no: 'NO', unknown: 'UNKNOWN', 'n/a': 'N/A' };

/** Explain why a scenario moved the result: the first gate whose answer changed, and the changed
 *  inputs that feed it. Deterministic and inspectable. */
export function explainTransition(beforeA: Assessment, before: YardResult, afterA: Assessment, after: YardResult): Transition {
  const changedInputs = (Object.keys(afterA.inputs) as InputId[]).filter(
    (id) => beforeA.inputs[id].value !== afterA.inputs[id].value,
  );
  const determinate = (y: YardOutcome) => y === 'small' || y === 'medium' || y === 'large';
  const changed = before.yard !== after.yard || (determinate(before.yard) && determinate(after.yard) && before.variant !== after.variant);
  const headline = `${YARD_NAME[before.yard]} → ${YARD_NAME[after.yard]}`;
  if (!changed)
    return { changed, from: before.yard, to: after.yard, headline: YARD_NAME[after.yard], explanation: 'The yard is unchanged.', changedInputs };
  const b = new Map(flatten(before.path).map((n) => [n.id, n]));
  const pivotNode = flatten(after.path).find((n) => n.id !== 'outcome' && b.has(n.id) && b.get(n.id)!.answer !== n.answer);
  const describe = (ids: InputId[]) =>
    ids
      .map((id) => `${INPUT_DEFS[id].label.toLowerCase()} is now ${optionLabel(id, afterA.inputs[id].value as string | number)} (was ${optionLabel(id, beforeA.inputs[id].value as string | number)})`)
      .join(' and ');
  let explanation: string;
  if (pivotNode) {
    const feeding = changedInputs.filter((id) => pivotNode.inputs.some((i) => i.id === id) || (pivotNode.children ?? []).some((c) => c.inputs.some((i) => i.id === id)));
    const because = describe(feeding.length ? feeding : changedInputs);
    explanation = `"${pivotNode.question}" changed from ${TRI[b.get(pivotNode.id)!.answer]} to ${TRI[pivotNode.answer]}${because ? ` because ${because}` : ''}.`;
    if (before.yard === 'medium' && after.yard === 'large')
      explanation = `The industrial-capability rationale weakens: ${because || 'the capability case changed'}.`;
    if (before.yard === after.yard)
      explanation = `Same yard, different approach: ${after.recommendedPosture.toLowerCase()}. ${explanation}`;
  } else explanation = `The result changed because ${describe(changedInputs) || 'assumptions changed'}.`;
  return { changed, from: before.yard, to: after.yard, headline, explanation, pivot: pivotNode?.id, changedInputs };
}
