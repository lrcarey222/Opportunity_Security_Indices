// The canonical answer options of a review case and the canonical conclusion of a strategic compact
// (docs/QC_SYSTEM_DESIGN.md 4.2, 5 step 4). Labels are read from the Explorer's own constants, never re-typed:
// posture choices from content/vocabulary.ts CHOICE_NAME (+ not_assessed), AIB classes from AIB_CLASS_NAME,
// operational compact types (AIB-Cxx) from the data's type strings (aib/contract.ts AIB_COMPACT_TYPES) as
// aib/derive/types.ts compactTypeInfo labels them, partnership memo boxes from country/contract.ts
// MEMO_CHOICE_KINDS. Every set ends with the DONT_KNOW option ("I don't know / outside my expertise").
//
// Main has no compact/decide.ts (DECISION_INFO is on the unpushed redesign branch only). A Strategic Sectoral
// Compact (SC-*) is therefore read with the export's own materiality rule, as main's compact page renders it
// (aib/views/strategic/sections.tsx: a primary mission, or "Targeted action" when the export marks it
// `targeted`, or the scenario statement when there is no mission): sectoral / narrower / none. Two of the three
// labels are main's compactTypeInfo words; "Targeted action" is the mission tag of sections.tsx:295 (a TSX
// module, so the string is quoted here rather than imported). Pure: no IO.

import { AIB_CLASSES, AIB_COMPACT_TYPES, type AibStrategicCompactScenario } from '../../aib/contract.ts';
import { compactTypeInfo } from '../../aib/derive/types.ts';
import { AIB_CLASS_DEFINITION, AIB_CLASS_NAME, CHOICE_DEFINITION, CHOICE_NAME } from '../../content/vocabulary.ts';
import { MEMO_CHOICE_KINDS } from '../../country/contract.ts';
import type { Verb } from '../../posture/contract.ts';
import { VERB_ORDER } from '../../posture/grammar.ts';
import { DONT_KNOW, type AnswerOption, type CaseKey, type CaseKind } from '../model.ts';

/** The not-assessed option (a posture cell the data cannot call). */
export const NOT_ASSESSED_OPTION: AnswerOption = {
  value: 'not_assessed',
  label: 'Not assessed: the data cannot support a call',
  definition: 'The Explorer makes no strategic call when the need, the prize or the deployment need is not measured.',
};

/** "I don't know / outside my expertise": valid wherever a choice is asked. */
export const DONT_KNOW_OPTION: AnswerOption = { value: DONT_KNOW, label: "I don't know / outside my expertise" };

/** A strategic compact's conclusion under one scenario (the export's materiality rule; see the header). */
export type StrategicCompactDecision = 'sectoral' | 'narrower' | 'none';
export const STRATEGIC_COMPACT_DECISIONS: readonly StrategicCompactDecision[] = ['sectoral', 'narrower', 'none'];

const UNNECESSARY = AIB_COMPACT_TYPES[2];
/** Main's words for the three strategic-compact conclusions (compactTypeInfo; "Targeted action" from sections.tsx). */
export const STRATEGIC_COMPACT_DECISION_LABEL: Readonly<Record<StrategicCompactDecision, string>> = {
  sectoral: compactTypeInfo(AIB_COMPACT_TYPES[0]).short,
  narrower: 'Targeted action',
  none: compactTypeInfo(UNNECESSARY).short,
};
const STRATEGIC_COMPACT_DECISION_DEFINITION: Readonly<Record<StrategicCompactDecision, string>> = {
  sectoral: 'The lead (primary) mission clears the materiality floor: a coordinated arrangement across the chain is warranted.',
  narrower: 'Steps need action, but every mission falls below the materiality floor: act on those steps alone.',
  none: 'No step needs action under this scenario (the compact page shows the scenario statement instead of missions).',
};

/** The primary mission of a strategic compact scenario (slot "primary", else the first listed); null without one. */
export function primaryMission(sc: Pick<AibStrategicCompactScenario, 'missions'> | null | undefined): AibStrategicCompactScenario['missions'][number] | null {
  const ms = sc?.missions ?? [];
  return ms.find((m) => m.slot === 'primary') ?? ms[0] ?? null;
}

/** A strategic compact's conclusion under a scenario record; null when the export has no record. */
export function strategicCompactDecision(sc: Pick<AibStrategicCompactScenario, 'missions'> | null | undefined): StrategicCompactDecision | null {
  if (!sc) return null;
  const p = primaryMission(sc);
  if (!p) return 'none';
  return p.targeted ? 'narrower' : 'sectoral';
}

/** True for a strategic compact id (SC-*), false for an operational compact (AIB-Cxx). */
export const isStrategicCompactId = (sid: string | null | undefined): boolean => typeof sid === 'string' && /^SC-/i.test(sid);

function postureOptions(): AnswerOption[] {
  const verbs: Verb[] = [...VERB_ORDER];
  return [...verbs.map((v) => ({ value: v, label: CHOICE_NAME[v], definition: CHOICE_DEFINITION[v] })), NOT_ASSESSED_OPTION];
}

/**
 * The answer options of a case kind (key: the case key; for a compact it picks the strategic or the operational
 * set). Always ends with DONT_KNOW.
 */
export function answerOptions(kind: CaseKind, key: CaseKey = {}): AnswerOption[] {
  let opts: AnswerOption[];
  switch (kind) {
    case 'posture_cell':
    case 'sector':
      opts = postureOptions();
      break;
    case 'aib_node':
      opts = AIB_CLASSES.map((c) => ({ value: c, label: AIB_CLASS_NAME[c], definition: AIB_CLASS_DEFINITION[c] }));
      break;
    case 'compact':
      opts = isStrategicCompactId(key.sid)
        ? STRATEGIC_COMPACT_DECISIONS.map((d) => ({ value: d, label: STRATEGIC_COMPACT_DECISION_LABEL[d], definition: STRATEGIC_COMPACT_DECISION_DEFINITION[d] }))
        : AIB_COMPACT_TYPES.map((t) => ({ value: t, label: compactTypeInfo(t).short, definition: compactTypeInfo(t).explanation }));
      break;
    case 'partnership':
      opts = MEMO_CHOICE_KINDS.map((k) => ({ value: k, label: CHOICE_NAME[k], definition: CHOICE_DEFINITION[k] }));
      break;
  }
  return [...opts, DONT_KNOW_OPTION];
}

/** The label of a conclusion value for a case kind (the option's label; the value itself when it is not an option). */
export function conclusionLabel(kind: CaseKind, value: string, key: CaseKey = {}): string {
  return answerOptions(kind, key).find((o) => o.value === value)?.label ?? value;
}
