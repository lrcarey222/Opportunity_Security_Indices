// The canonical answer options of a review case and the canonical conclusion of a strategic compact
// (docs/QC_SYSTEM_DESIGN.md 4.2, 5 step 4; UX v2, docs/QC_UX_REVIEW.md 6). Labels are read from the Explorer's own
// constants, never re-typed: posture choices from content/vocabulary.ts CHOICE_NAME with CHOICE_ACTION and
// CHOICE_DEFINITION (Watch is shown as "Watch (keep open)", the first words of CHOICE_ACTION.watch: the name does
// not change), AIB classes from AIB_CLASS_NAME / AIB_CLASS_DEFINITION, operational compact types (AIB-Cxx) from
// the data's type strings (aib/contract.ts AIB_COMPACT_TYPES) as aib/derive/types.ts compactTypeInfo labels them,
// six-stage names from lib/stages.ts STAGE_DISPLAY (sector chains).
//
// Two sets per kind:
// - conclusionOptions(): every conclusion the Explorer can reach, including its model states (posture
//   `not_assessed`, the AIB class INSUFFICIENT DATA). The snapshot's conclusion is one of these.
// - answerOptions(): what a reviewer is offered before the reveal: the conclusions a reviewer can recommend (the
//   model states removed: they describe the model, not a recommendation), each with its one-line definition, then
//   the three reviewer states (model.ts REVIEWER_STATES: none of these / insufficient evidence / outside my
//   expertise). DONT_KNOW (v1) is no longer offered but stays a valid stored answer (review/manifest.ts).
//
// Main has no compact/decide.ts (DECISION_INFO is on the unpushed redesign branch only). A Strategic Sectoral
// Compact (SC-*) is therefore read with the export's own materiality rule, as main's compact page renders it
// (aib/views/strategic/sections.tsx: a primary mission, or "Targeted action" when the export marks it
// `targeted`, or the scenario statement when there is no mission): sectoral / narrower / none. Two of the three
// labels are main's compactTypeInfo words; "Targeted action" is the mission tag of sections.tsx:295 (a TSX
// module, so the string is quoted here rather than imported). The partnership memo's four boxes are labelled as
// R/partnerships/pm_document.R pm_doc_boxes writes them (quoted for the same reason: no TS constant exists).
// Pure: no IO.

import { AIB_CLASSES, AIB_COMPACT_TYPES, type AibStrategicCompactScenario } from '../../aib/contract.ts';
import { compactTypeInfo } from '../../aib/derive/types.ts';
import { AIB_CLASS_DEFINITION, AIB_CLASS_NAME, CHOICE_ACTION, CHOICE_DEFINITION, CHOICE_NAME } from '../../content/vocabulary.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import type { Verb } from '../../posture/contract.ts';
import { VERB_ORDER } from '../../posture/grammar.ts';
import { DONT_KNOW, REVIEWER_STATE_LABEL, REVIEWER_STATES, type AnswerOption, type CaseKey, type CaseKind } from '../model.ts';

/**
 * An answer option as the review data writes it (an AnswerOption with two optional additions, so v1 readers keep
 * working): `action`, the short action wording of a posture choice (CHOICE_ACTION), and `group`, which separates
 * the Explorer's conclusions from the reviewer states.
 */
export interface ReviewAnswerOption extends AnswerOption {
  action?: string;
  group?: 'conclusion' | 'reviewer_state';
}

/** The not-assessed conclusion (a posture cell the data cannot call). A model state: never a reviewer option. */
export const NOT_ASSESSED_OPTION: AnswerOption = {
  value: 'not_assessed',
  label: 'Not assessed: the data cannot support a call',
  definition: 'The Explorer makes no strategic call when the need, the prize or the deployment need is not measured.',
};

/** The AIB class that is a data state, not a recommendation (kept as a possible conclusion only). */
export const AIB_MODEL_STATE_CLASS = 'INSUFFICIENT DATA';

/** "I don't know / outside my expertise" (v1). No longer offered; still accepted in stored answers. */
export const DONT_KNOW_OPTION: AnswerOption = { value: DONT_KNOW, label: "I don't know / outside my expertise" };

/** The three reviewer states, offered after the conclusions on every blind question. */
export const REVIEWER_STATE_OPTIONS: readonly ReviewAnswerOption[] = REVIEWER_STATES.map((v) => ({ value: v, label: REVIEWER_STATE_LABEL[v], group: 'reviewer_state' as const }));

/** The sector-chain answer for "no stage needs allied intervention". */
export const NO_STAGE = 'no_stage';
export const NO_STAGE_LABEL = 'No stage needs allied intervention';

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
/** One line each, in plain words (the materiality floor is stated as numbers, never as jargon). */
export const STRATEGIC_COMPACT_DECISION_DEFINITION: Readonly<Record<StrategicCompactDecision, string>> = {
  sectoral:
    'A standing multi-country arrangement across the whole chain (coordinated investment, offtake, stockpiles, common trade rules), because joint action is needed on a large enough part of it (a quarter of the supply at risk, or US$1bn).',
  narrower: 'Some steps need joint action, but each is too small to justify a standing arrangement: act on those steps alone.',
  none: 'No step needs joint action under this scenario: existing trade and national policy are enough.',
};

/** The partnership memo's four boxes (R pm_doc_boxes), keyed by their kind. */
export type PartnershipBoxKind = 'preserve' | 'partner' | 'accelerate' | 'watch';
export const PARTNERSHIP_BOX_KINDS: readonly PartnershipBoxKind[] = ['preserve', 'partner', 'accelerate', 'watch'];
export const PARTNERSHIP_BOX_LABEL: Readonly<Record<PartnershipBoxKind, string>> = {
  preserve: 'Build together',
  partner: 'Trade and standards',
  accelerate: 'Institutions',
  watch: 'Stay out',
};
/** One line each, from the memo rules (docs/partnership_memos.md: states and boxes). */
export const PARTNERSHIP_BOX_DEFINITION: Readonly<Record<PartnershipBoxKind, string>> = {
  preserve: 'Lead with new production capacity built jointly on specific steps where trusted supply is short or both sides are exposed.',
  partner: 'Lead with trade: recognise each other as trusted suppliers in procurement and content rules, and agree common rules against circumvention.',
  accelerate: 'Lead with institutions: upgrade or create the agreements that carry the cooperation, without new joint capacity.',
  watch: 'Make no new joint commitments: the two sides compete, or the strategic case is weak.',
};

/** Watch as reviewers see it: "Watch (keep open)" (the name, then the first words of its action). */
const keepOpen = CHOICE_ACTION.watch.split(':')[0].trim().toLowerCase();
export const choiceOptionLabel = (v: Verb): string => (v === 'watch' ? `${CHOICE_NAME.watch} (${keepOpen})` : CHOICE_NAME[v]);

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

const conclusion = (o: ReviewAnswerOption): ReviewAnswerOption => ({ ...o, group: 'conclusion' });

function postureConclusions(): ReviewAnswerOption[] {
  const verbs: Verb[] = [...VERB_ORDER];
  return [...verbs.map((v) => conclusion({ value: v, label: choiceOptionLabel(v), action: CHOICE_ACTION[v], definition: CHOICE_DEFINITION[v] })), conclusion(NOT_ASSESSED_OPTION)];
}

/**
 * Every conclusion the Explorer can reach for a case kind (the key picks the compact family), model states
 * included. No reviewer state and no DONT_KNOW.
 */
export function conclusionOptions(kind: CaseKind, key: CaseKey = {}): ReviewAnswerOption[] {
  switch (kind) {
    case 'posture_cell':
    case 'sector':
      return postureConclusions();
    case 'aib_node':
      return AIB_CLASSES.map((c) => conclusion({ value: c, label: AIB_CLASS_NAME[c], definition: AIB_CLASS_DEFINITION[c] }));
    case 'compact':
      return isStrategicCompactId(key.sid)
        ? STRATEGIC_COMPACT_DECISIONS.map((d) => conclusion({ value: d, label: STRATEGIC_COMPACT_DECISION_LABEL[d], definition: STRATEGIC_COMPACT_DECISION_DEFINITION[d] }))
        : AIB_COMPACT_TYPES.map((t) => conclusion({ value: t, label: compactTypeInfo(t).short, definition: compactTypeInfo(t).explanation }));
    case 'partnership':
      return PARTNERSHIP_BOX_KINDS.map((k) => conclusion({ value: k, label: PARTNERSHIP_BOX_LABEL[k], definition: PARTNERSHIP_BOX_DEFINITION[k] }));
    case 'sector_chain':
      return [
        ...SUPPLY_STAGES.map((s) => conclusion({ value: s, label: STAGE_DISPLAY[s], definition: `Allied intervention is warranted at the ${STAGE_DISPLAY[s]} stage of this chain (choose it first if you would act there first).` })),
        conclusion({ value: NO_STAGE, label: NO_STAGE_LABEL, definition: 'Existing trade can absorb the stress at every stage; no stage needs joint action.' }),
      ];
  }
}

/** The model states among a kind's conclusions (never offered to reviewers). */
export const MODEL_STATE_VALUES: readonly string[] = [NOT_ASSESSED_OPTION.value, AIB_MODEL_STATE_CLASS];

/**
 * The options a reviewer is offered before the reveal: the recommendable conclusions with their one-line
 * definitions, then the three reviewer states.
 */
export function answerOptions(kind: CaseKind, key: CaseKey = {}): ReviewAnswerOption[] {
  const opts = conclusionOptions(kind, key).filter((o) => !MODEL_STATE_VALUES.includes(o.value));
  return [...opts, ...REVIEWER_STATE_OPTIONS.map((o) => ({ ...o }))];
}

/** The label of a conclusion value for a case kind (the conclusion's label; the value itself when it is not one). */
export function conclusionLabel(kind: CaseKind, value: string, key: CaseKey = {}): string {
  return conclusionOptions(kind, key).find((o) => o.value === value)?.label ?? value;
}

/** Every canonical conclusion label of every kind except the six stage names (the leak lint's word list). */
export function allConclusionLabels(): string[] {
  const out = new Set<string>();
  for (const v of VERB_ORDER) out.add(CHOICE_NAME[v]);
  for (const k of ['posture_cell', 'aib_node', 'partnership'] as const) for (const o of conclusionOptions(k)) out.add(o.label);
  for (const o of conclusionOptions('compact', { sid: 'SC-X' })) out.add(o.label);
  for (const o of conclusionOptions('compact', { sid: 'AIB-C01' })) out.add(o.label);
  out.add(NO_STAGE_LABEL);
  return [...out];
}
