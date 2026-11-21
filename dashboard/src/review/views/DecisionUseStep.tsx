// The decision-use questions of a case review (docs/QC_SYSTEM_DESIGN.md 4.4 decision_use), shown in the extras
// step only for cases flagged `decision_use`, after the reveal: would the reviewer do something different, is
// the conclusion actionable, does it identify the right problem, does it keep the diagnosis apart from the
// prescription, is the geography right, and what evidence or improvement would make it usable. Every question
// is optional; "I don't know / outside my expertise" skips them all and is recorded as DONT_KNOW in
// would_do_differently. Answers go into the case's flow state (review/flow.ts set_decision_use) and are
// submitted as a separate decision_use record.

import { DontKnowToggle, Likert4Input, TextArea } from '../components/form.tsx';
import { emptyDecisionUse, type DecisionUseDraft } from '../flow.ts';
import { DONT_KNOW, type Likert4 } from '../model.ts';

export const DECISION_USE_QUESTION = {
  would_do_differently: 'If you were advising a government on this, what would you do differently from what the Explorer suggests? (optional)',
  actionable: 'Is the conclusion specific enough to act on?',
  identifies_problem: 'Does it identify the right problem?',
  separates_diagnosis_from_prescription: 'Does it keep the diagnosis (what is wrong) apart from the prescription (what to do)?',
  right_geography: 'Does it point to the right countries or partners?',
  evidence_required: 'What evidence would a decision maker need before acting on it? (optional)',
  improvement: 'What one change would make it more useful? (optional)',
  decision_use: 'What decision could this inform, in your experience? (optional)',
} as const;

type LikertKey = 'actionable' | 'identifies_problem' | 'separates_diagnosis_from_prescription' | 'right_geography';
const LIKERT_KEYS: readonly LikertKey[] = ['actionable', 'identifies_problem', 'separates_diagnosis_from_prescription', 'right_geography'];

export function DecisionUseStep({ value, onChange }: { value: DecisionUseDraft | null; onChange: (patch: Partial<DecisionUseDraft>) => void }) {
  const d = value ?? emptyDecisionUse();
  const skipped = LIKERT_KEYS.every((k) => d[k] === null) && d.would_do_differently === DONT_KNOW;
  return (
    <section aria-labelledby="decision-use-h" className="space-y-2" data-testid="decision-use-step">
      <h3 id="decision-use-h" className="text-lg font-medium text-paper">
        Could a decision maker use this?
      </h3>
      <p className="max-w-3xl text-sm text-paper-mute">Every question here is optional. Leave a scale blank if you cannot judge it.</p>
      <DontKnowToggle
        checked={skipped}
        onChange={(v) =>
          onChange(v ? { actionable: null, identifies_problem: null, separates_diagnosis_from_prescription: null, right_geography: null, would_do_differently: DONT_KNOW } : { would_do_differently: '' })
        }
      />
      {!skipped && (
        <div className="space-y-1">
          <TextArea label={DECISION_USE_QUESTION.would_do_differently} rows={3} value={d.would_do_differently} onChange={(v) => onChange({ would_do_differently: v })} />
          {LIKERT_KEYS.map((k) => (
            <Likert4Input key={k} legend={DECISION_USE_QUESTION[k]} value={d[k]} onChange={(v: Likert4) => onChange({ [k]: v })} />
          ))}
          <TextArea label={DECISION_USE_QUESTION.evidence_required} rows={2} value={d.evidence_required ?? ''} onChange={(v) => onChange({ evidence_required: v })} />
          <TextArea label={DECISION_USE_QUESTION.improvement} rows={2} value={d.improvement ?? ''} onChange={(v) => onChange({ improvement: v })} />
          <TextArea label={DECISION_USE_QUESTION.decision_use} rows={2} value={d.decision_use ?? ''} onChange={(v) => onChange({ decision_use: v })} />
        </div>
      )}
    </section>
  );
}

export default DecisionUseStep;
