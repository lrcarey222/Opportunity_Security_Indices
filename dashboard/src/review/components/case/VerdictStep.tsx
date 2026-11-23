// The post-reveal verdict (docs/QC_UX_REVIEW.md sections 9 and 18). First the prior, as a computed fact (the
// blind answer against the Explorer's conclusion; never asked, so it cannot be rationalised after the reveal).
// Then the ten-way statement "Having seen the Explorer's reasoning, which best describes your view?" (model.ts
// REVIEW_VERDICT_LABEL, stored verbatim as verdict_code), validity ("Is the conclusion supported by the
// evidence?") and persuasion ("Did inspecting the evidence change your view?"), asked separately. A statement
// that disagrees, or "same conclusion, different reasons", asks where exactly it fails: the kind of problem
// (fixed by the statement except for "different reasons"), a sentence or two, and optionally the facts,
// assumptions or steps involved and the answer the reviewer would give instead.

import { Select } from '../../../components/ui.tsx';
import { STAGE_DISPLAY, isSupplyStage } from '../../../lib/stages.ts';
import { ChoiceGrid, MultiCheck, TextArea } from '../form.tsx';
import { verdictCodeConsistency, verdictCodeIssueType, verdictCodeNeedsIssue, type CaseFlowStateV2, type FlowActionV2 } from '../../flow.ts';
import {
  ISSUE_TYPE_LABEL,
  REVIEW_VERDICT_CODES,
  REVIEW_VERDICT_LABEL,
  VERDICT_LABEL,
  type IssueType,
  type Persuasion,
  type ReviewCase,
  type ReviewVerdictCode,
  type Validity,
} from '../../model.ts';
import { ChipChoice, conclusionChoices } from './BlindQuestion.tsx';

export const VERDICT_LEGEND = "Having seen the Explorer's reasoning, which best describes your view?";
export const VALIDITY_LEGEND = "Is the Explorer's conclusion supported by the evidence?";
export const PERSUASION_LEGEND = 'Did inspecting the evidence change your view?';
export const VALIDITY_LABEL: Readonly<Record<Validity, string>> = { supported: 'Supported', partly: 'Partly supported', not_supported: 'Not supported', unsure: 'Not sure' };
export const PERSUASION_LABEL: Readonly<Record<Persuasion, string>> = { changed: 'Yes, it changed my view', somewhat: 'Somewhat', unchanged: 'No' };

/** The prefixes of the "what is involved" ids beside a brief fact id. */
export const ASSUMPTION_REF = 'assumption:';
export const STEP_REF = 'step:';

const stageWords = (xs: readonly string[]) => xs.map((x) => (isSupplyStage(x) ? STAGE_DISPLAY[x] : x)).join(', ') || 'none';

export interface PriorFact {
  /** null when the blind answer was a reviewer state (nothing to compare). */
  differs: boolean | null;
  yours: string;
  explorer: string;
  /** Sector chain: the stage sets and their overlap. */
  sets?: { yours: string[]; explorer: string[]; overlap: number } | null;
}

export function PriorPanel({ p }: { p: PriorFact }) {
  return (
    <section className="panel mb-5 px-4 py-3" aria-labelledby="prior-h" data-testid="prior">
      <h4 id="prior-h" className="eyebrow">
        Your initial assessment and the Explorer&rsquo;s
      </h4>
      <p className="mt-1.5 text-sm text-paper-dim">
        You answered <span className="text-paper">{p.yours}</span>; the Explorer concluded <span className="text-paper">{p.explorer}</span>.{' '}
        <span className="text-paper" data-testid="prior-result">
          {p.differs === null ? 'There is no recommendation of yours to compare.' : p.differs ? 'They differ.' : 'They match.'}
        </span>
      </p>
      {p.sets && (
        <p className="mt-1 text-xs text-paper-mute">
          Stages you selected: {stageWords(p.sets.yours)}. The Explorer&rsquo;s: {stageWords(p.sets.explorer)}. Overlap {Math.round(100 * p.sets.overlap)}%.
        </p>
      )}
      <p className="mt-1 text-xs text-paper-mute">A different answer is not a mistake on either side: say below what the evidence supports.</p>
    </section>
  );
}

export interface VerdictStepProps {
  c: ReviewCase;
  s: CaseFlowStateV2;
  dispatch: (a: FlowActionV2) => void;
  prior: PriorFact;
  tried: boolean;
}

/** The ids the reviewer can point at: the brief's facts, its assumptions and the reasoning's steps. */
export function involvedOptions(c: ReviewCase): { value: string; label: string }[] {
  const b = c.snapshot.brief;
  const r = c.snapshot.reasoning;
  if (!b && !r) return c.reveal_fields.filter((id) => c.snapshot.facts[id]).map((id) => ({ value: id, label: c.snapshot.facts[id].label }));
  return [
    ...(b?.evidence ?? []).map((f) => ({ value: f.id, label: `Fact: ${f.label}` })),
    ...(b?.assumptions ?? []).map((a) => ({ value: `${ASSUMPTION_REF}${a.id}`, label: `Assumption: ${a.label}` })),
    ...(r?.steps ?? []).map((st) => ({ value: `${STEP_REF}${st.id}`, label: `Step: ${st.label}` })),
  ];
}

export function VerdictStep({ c, s, dispatch, prior, tried }: VerdictStepProps) {
  const r = s.reveal;
  const code = r.verdict_code;
  const needs = verdictCodeNeedsIssue(code);
  const fixed = verdictCodeIssueType(code);
  const outside = code === 'outside_expertise';
  const consistency = verdictCodeConsistency(code, prior.differs);
  const set = (patch: Extract<FlowActionV2, { type: 'set_reveal' }>['patch']) => dispatch({ type: 'set_reveal', patch });
  return (
    <div data-testid="verdict-form">
      <PriorPanel p={prior} />
      {r.legacy_verdict !== null && (
        <p className="mb-3 text-xs text-paper-mute" data-testid="legacy-verdict">
          Your earlier verdict on this case: {VERDICT_LABEL[r.legacy_verdict]} The review now asks the question below instead.
        </p>
      )}
      <ChoiceGrid
        legend={VERDICT_LEGEND}
        name="verdict-code"
        options={REVIEW_VERDICT_CODES.map((v, i) => ({ value: v, label: `${i + 1}. ${REVIEW_VERDICT_LABEL[v]}` }))}
        value={code}
        onChange={(v) => dispatch({ type: 'set_verdict_code', code: v as ReviewVerdictCode })}
        error={tried && !code ? 'Choose the statement that best describes your view.' : null}
      />
      {consistency === 'inconsistent' && (
        <p className="mb-4 text-xs text-paper-mute" role="status" data-testid="consistency-note">
          {prior.differs ? 'Your initial answer differed from the Explorer’s, so a “same conclusion” statement may not fit.' : 'Your initial answer matched the Explorer’s, so “persuaded me” may not fit.'} Keep it if it is what you mean.
        </p>
      )}
      {code && !outside && (
        <div className="grid gap-x-6 md:grid-cols-2">
          <ChipChoice<Validity>
            legend={VALIDITY_LEGEND}
            options={(Object.keys(VALIDITY_LABEL) as Validity[]).map((k) => ({ value: k, label: VALIDITY_LABEL[k] }))}
            value={r.validity}
            onChange={(v) => set({ validity: v })}
            error={tried && !r.validity ? 'Say whether the evidence supports the conclusion.' : null}
          />
          <ChipChoice<Persuasion>
            legend={PERSUASION_LEGEND}
            options={(Object.keys(PERSUASION_LABEL) as Persuasion[]).map((k) => ({ value: k, label: PERSUASION_LABEL[k] }))}
            value={r.persuasion}
            onChange={(v) => set({ persuasion: v })}
            error={tried && !r.persuasion ? 'Say whether the evidence changed your view.' : null}
          />
        </div>
      )}
      {needs && (
        <section className="panel px-4 py-3" aria-labelledby="issue-h" data-testid="verdict-issue">
          <h4 id="issue-h" className="mb-2 text-sm font-medium text-paper">
            {code === 'same_different_reasons' ? 'Which reasons differ?' : 'Where exactly does it fail?'}
          </h4>
          {fixed ? (
            <p className="mb-3 text-sm text-paper-dim" data-testid="issue-type">
              Kind of problem: <span className="text-paper">{ISSUE_TYPE_LABEL[fixed]}</span>
            </p>
          ) : (
            <ChoiceGrid
              legend="What kind of difference is it?"
              name="issue-type"
              options={(Object.keys(ISSUE_TYPE_LABEL) as IssueType[]).map((k) => ({ value: k, label: ISSUE_TYPE_LABEL[k] }))}
              value={r.issue_type}
              onChange={(v) => set({ issue_type: v as IssueType })}
              error={tried && !r.issue_type ? 'Say what kind of difference it is.' : null}
            />
          )}
          <TextArea
            label={code === 'same_different_reasons' ? 'Your reasons, and how they differ from the Explorer’s' : 'Describe it in a sentence or two'}
            required
            rows={3}
            value={r.issue_text}
            onChange={(v) => set({ issue_text: v })}
            error={tried && !r.issue_text.trim() ? 'Say in a sentence or two where it fails, or which reasons differ.' : null}
          />
          <MultiCheck legend="Which facts, assumptions or steps are involved? (optional)" options={involvedOptions(c)} value={r.metric_ids} onChange={(v) => set({ metric_ids: v })} />
          <Select
            label="What would you conclude instead? (optional)"
            className="mb-2 max-w-md"
            value={r.suggested_choice ?? ''}
            options={[{ value: '', label: 'No suggestion' }, ...conclusionChoices(c.answer_options).map((o) => ({ value: o.value, label: o.label }))]}
            onChange={(v) => set({ suggested_choice: v || null })}
          />
        </section>
      )}
    </div>
  );
}

export default VerdictStep;
