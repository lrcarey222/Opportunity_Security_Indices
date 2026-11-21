// The blind answer (docs/QC_UX_REVIEW.md section 6): "Based on the evidence above, what ... would you
// recommend?" with the case's canonical conclusions and their one-line definitions (qc/answerOptions.ts built
// them from content/vocabulary.ts; Watch reads "Watch (keep open)"), then the three reviewer states (none of
// these / insufficient evidence / outside my expertise). Insufficient evidence opens "What information is
// missing?" (EvidenceGap choices + text); none of these asks what the reviewer would recommend instead. A
// recommendation is followed by Confidence (Low / Medium / High) and "What is the most important factor driving
// your judgment?" (one DriverFactor + optional text). A sector chain asks for every stage that needs action, then
// the one to act on first. Pure presentation over review/flow.ts BlindDraftV2: the parent dispatches the patches.

import { useId, type ReactNode } from 'react';
import { VERB_COLOR } from '../../../posture/grammar.ts';
import type { Verb } from '../../../posture/contract.ts';
import { ChoiceGrid, MultiCheck, TextArea, type ChoiceOption } from '../form.tsx';
import type { BlindDraftV2 } from '../../flow.ts';
import {
  CONFIDENCE_LEVEL_LABEL,
  DONT_KNOW,
  DRIVER_FACTOR_LABEL,
  EVIDENCE_GAP_LABEL,
  INSUFFICIENT_EVIDENCE,
  NONE_OF_THESE,
  OUTSIDE_EXPERTISE,
  REVIEWER_STATE_LABEL,
  REVIEWER_STATES,
  type AnswerOption,
  type CaseKind,
  type ConfidenceLevel,
  type DriverFactor,
  type EvidenceGap,
  type ReviewProduct,
} from '../../model.ts';

/** The sector-chain answer "no stage needs allied intervention" (qc/answerOptions.ts NO_STAGE). */
export const NO_STAGE_VALUE = 'no_stage';

export const BLIND_LEGEND: Readonly<Record<ReviewProduct, string>> = {
  country: 'Based on the evidence above, what broad strategy would you recommend?',
  sector: 'Based on the evidence above, what kind of allied response would you recommend?',
  compact: 'Based on the evidence above, what would you recommend for this chain?',
  partnership: 'Based on the evidence above, what should lead this cooperation?',
};
export const SECTOR_CHAIN_STAGES_LEGEND = 'Based on the evidence above, at which stages is allied intervention warranted? Select every stage that needs it.';
export const SECTOR_CHAIN_FIRST_LEGEND = 'Which of them would you act on first?';
export const STATES_LEGEND = 'Or, if you cannot recommend one of these:';
export const CONFIDENCE_LEGEND = 'How confident are you?';
export const DRIVER_LEGEND = 'What is the most important factor driving your judgment?';
export const MISSING_LEGEND = 'What information is missing?';

const isVerb = (v: string): v is Verb => Object.prototype.hasOwnProperty.call(VERB_COLOR, v);

/** A single choice shown as chips (Low / Medium / High, the driver factors). */
export function ChipChoice<T extends string>({ legend, options, value, onChange, hint, error }: { legend: ReactNode; options: readonly { value: T; label: string }[]; value: T | null; onChange: (v: T) => void; hint?: ReactNode; error?: string | null }) {
  const group = useId();
  const hintId = hint ? `${group}-hint` : undefined;
  const errId = error ? `${group}-error` : undefined;
  return (
    <fieldset className="mb-4" aria-describedby={[hintId, errId].filter(Boolean).join(' ') || undefined} aria-invalid={error ? true : undefined}>
      <legend className="mb-1 text-sm font-medium text-paper">{legend}</legend>
      {hint && (
        <p id={hintId} className="mb-1.5 text-xs text-paper-mute">
          {hint}
        </p>
      )}
      <div className="flex flex-wrap gap-1.5">
        {options.map((o) => {
          const id = `${group}-${o.value}`;
          const on = value === o.value;
          return (
            <label
              key={o.value}
              htmlFor={id}
              className="tag cursor-pointer has-[:focus-visible]:outline-2 has-[:focus-visible]:outline-offset-2 has-[:focus-visible]:outline-signal"
              data-checked={on || undefined}
              style={on ? { borderColor: 'var(--color-signal)', color: 'var(--color-paper)' } : undefined}
            >
              <input id={id} type="radio" name={group} value={o.value} checked={on} onChange={() => onChange(o.value)} className="sr-only" />
              {o.label}
            </label>
          );
        })}
      </div>
      {error && (
        <p id={errId} role="alert" className="mt-1 text-xs text-alert">
          {error}
        </p>
      )}
    </fieldset>
  );
}

const isState = (v: string) => REVIEWER_STATES.includes(v) || v === DONT_KNOW;

/** The conclusions a reviewer may recommend (options other than the reviewer states). */
export function conclusionChoices(options: readonly AnswerOption[]): ChoiceOption[] {
  return options.filter((o) => !isState(o.value)).map((o) => ({ value: o.value, label: o.label, definition: o.definition, color: isVerb(o.value) ? VERB_COLOR[o.value] : undefined }));
}

/** The three reviewer states (their labels from the case when it carries them, else model.ts). */
export function stateChoices(options: readonly AnswerOption[]): ChoiceOption[] {
  return REVIEWER_STATES.map((v) => ({ value: v, label: options.find((o) => o.value === v)?.label ?? REVIEWER_STATE_LABEL[v] }));
}

export interface BlindQuestionProps {
  kind: CaseKind;
  product: ReviewProduct;
  options: readonly AnswerOption[];
  blind: BlindDraftV2;
  onPatch: (patch: Partial<Omit<BlindDraftV2, 'answered_at' | 'legacy'>>) => void;
  /** Show the errors (after a failed lock attempt). */
  tried: boolean;
}

export function BlindQuestion({ kind, product, options, blind: b, onPatch, tried }: BlindQuestionProps) {
  const chain = kind === 'sector_chain';
  const conclusions = conclusionChoices(options);
  const states = stateChoices(options);
  const name = useId();
  const stages = conclusions.filter((o) => o.value !== NO_STAGE_VALUE);
  const noStage = conclusions.find((o) => o.value === NO_STAGE_VALUE) ?? null;
  const choice = b.choice;
  const state = choice !== null && isState(choice);
  const recommends = choice !== null && (!state || choice === NONE_OF_THESE);
  const firstOptions = stages.filter((o) => b.choices.includes(o.value));

  const pickChoice = (v: string) => {
    if (chain && (v === NO_STAGE_VALUE || isState(v))) onPatch({ choice: v, choices: v === NO_STAGE_VALUE ? [] : b.choices });
    else onPatch({ choice: v });
  };

  return (
    <div data-testid="blind-question">
      {chain ? (
        <>
          <MultiCheck
            legend={SECTOR_CHAIN_STAGES_LEGEND}
            options={stages.map((o) => ({ value: o.value, label: o.label }))}
            value={b.choices}
            onChange={(v) => {
              const ordered = stages.map((o) => o.value).filter((x) => v.includes(x));
              // keep the first stage when it is still selected; one selected stage is the first by itself
              const keep = choice && ordered.includes(choice) ? choice : ordered.length === 1 ? ordered[0] : ordered.length === 0 && choice && (isState(choice) || choice === NO_STAGE_VALUE) ? choice : null;
              onPatch({ choices: ordered, choice: keep });
            }}
            hint="The stages are the six of the supply chain; the strip above marks the ones this chain covers."
            error={tried && !choice && !b.choices.length ? 'Select the stages that need allied intervention, or choose one of the options below.' : null}
          />
          {firstOptions.length > 1 && (
            <ChoiceGrid legend={SECTOR_CHAIN_FIRST_LEGEND} name={`${name}-first`} options={firstOptions} value={choice} onChange={pickChoice} error={tried && b.choices.length > 0 && (!choice || !b.choices.includes(choice)) ? 'Choose the stage you would act on first.' : null} />
          )}
          <ChoiceGrid legend={STATES_LEGEND} name={`${name}-states`} options={[...(noStage ? [noStage] : []), ...states]} value={choice} onChange={pickChoice} />
        </>
      ) : (
        <>
          <ChoiceGrid legend={BLIND_LEGEND[product]} name={name} options={conclusions} value={choice} onChange={pickChoice} error={tried && !choice ? 'Choose an answer, or one of the options below.' : null} />
          <ChoiceGrid legend={STATES_LEGEND} name={name} options={states} value={choice} onChange={pickChoice} />
        </>
      )}

      {choice === INSUFFICIENT_EVIDENCE && (
        <div className="panel mb-4 px-4 py-3" data-testid="missing-evidence">
          <MultiCheck
            legend={MISSING_LEGEND}
            options={(Object.keys(EVIDENCE_GAP_LABEL) as EvidenceGap[]).map((k) => ({ value: k, label: EVIDENCE_GAP_LABEL[k] }))}
            value={b.missing_evidence}
            onChange={(v) => onPatch({ missing_evidence: v as EvidenceGap[] })}
            hint="Pick every kind of information you would need to judge this case."
            error={tried && b.missing_evidence.length === 0 ? 'Say what information is missing (pick at least one).' : null}
          />
          <TextArea label="Anything more specific? (optional)" rows={2} value={b.missing_evidence_text ?? ''} onChange={(v) => onPatch({ missing_evidence_text: v })} />
        </div>
      )}

      {choice === NONE_OF_THESE && <TextArea label="What would you recommend instead? (optional)" rows={2} value={b.own_recommendation ?? ''} onChange={(v) => onPatch({ own_recommendation: v })} />}

      {recommends && (
        <>
          <ChipChoice<ConfidenceLevel>
            legend={CONFIDENCE_LEGEND}
            options={(Object.keys(CONFIDENCE_LEVEL_LABEL) as ConfidenceLevel[]).map((k) => ({ value: k, label: CONFIDENCE_LEVEL_LABEL[k] }))}
            value={b.confidence_level}
            onChange={(v) => onPatch({ confidence_level: v })}
            error={tried && !b.confidence_level ? 'Say how confident you are.' : null}
          />
          <ChipChoice<DriverFactor>
            legend={DRIVER_LEGEND}
            options={(Object.keys(DRIVER_FACTOR_LABEL) as DriverFactor[]).map((k) => ({ value: k, label: DRIVER_FACTOR_LABEL[k] }))}
            value={b.driver}
            onChange={(v) => onPatch({ driver: v })}
            error={tried && !b.driver ? 'Pick the most important factor driving your judgment.' : null}
          />
          {b.driver && <TextArea label="Say more about it (optional)" rows={2} value={b.driver_text ?? ''} onChange={(v) => onPatch({ driver_text: v })} />}
        </>
      )}

      {choice !== null && choice !== OUTSIDE_EXPERTISE && <TextArea label="Your reasoning (optional)" rows={3} value={b.rationale ?? ''} onChange={(v) => onPatch({ rationale: v })} />}
    </div>
  );
}

export default BlindQuestion;
