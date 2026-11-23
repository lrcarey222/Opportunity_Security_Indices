// The two red-team questions (docs/QC_UX_REVIEW.md section 10), shown only when the reviewer disagrees or gave a
// low-confidence blind answer (review/flow.ts needsRedTeam): "What would have to be true for the Explorer's
// conclusion to be correct?" and "What single piece of additional evidence would most help resolve this case?"
// (one EvidenceGap + optional text). Both optional; the answers feed the admin dashboard's evidence gaps.

import { ChoiceGrid, TextArea } from '../form.tsx';
import type { CaseFlowStateV2, FlowActionV2 } from '../../flow.ts';
import { EVIDENCE_GAP_LABEL, type EvidenceGap } from '../../model.ts';

export const MUST_BE_TRUE_LEGEND = "What would have to be true for the Explorer's conclusion to be correct?";
export const RESOLVING_LEGEND = 'What single piece of additional evidence would most help resolve this case?';

export function RedTeamStep({ s, dispatch, conclusion }: { s: CaseFlowStateV2; dispatch: (a: FlowActionV2) => void; conclusion: string }) {
  const r = s.reveal;
  const set = (patch: Extract<FlowActionV2, { type: 'set_reveal' }>['patch']) => dispatch({ type: 'set_reveal', patch });
  return (
    <div data-testid="red-team">
      <p className="mb-4 max-w-3xl text-sm text-paper-mute">
        You disagreed with the Explorer or were not confident. Two optional questions help us see exactly what would settle the case (the Explorer concluded {conclusion}).
      </p>
      <TextArea label={MUST_BE_TRUE_LEGEND} hint="Name the fact, threshold or assumption that would have to hold." rows={3} value={r.must_be_true ?? ''} onChange={(v) => set({ must_be_true: v })} />
      <ChoiceGrid
        legend={RESOLVING_LEGEND}
        name="resolving-evidence"
        options={(Object.keys(EVIDENCE_GAP_LABEL) as EvidenceGap[]).map((k) => ({ value: k, label: EVIDENCE_GAP_LABEL[k] }))}
        value={r.resolving_evidence}
        onChange={(v) => set({ resolving_evidence: v as EvidenceGap })}
      />
      <TextArea label="Say more about that evidence (optional)" rows={2} value={r.resolving_evidence_text ?? ''} onChange={(v) => set({ resolving_evidence_text: v })} />
    </div>
  );
}

export default RedTeamStep;
