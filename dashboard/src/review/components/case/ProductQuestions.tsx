// The tailored post-reveal questions of a product (model.ts PRODUCT_QUESTIONS): for a sector, "Has the Explorer
// located the actual strategic bottleneck?"; for a partnership, industrial complementarity or diplomatic
// compatibility, then incentives, asymmetry, durability and substitution; for a compact, "Does this compact
// materially ameliorate the stated threat?". Every question offers "Not sure"; the first is required
// (review/flow.ts productErrorsV2), the rest are optional.

import { ChoiceGrid } from '../form.tsx';
import { productQuestions, type CaseFlowStateV2, type FlowActionV2 } from '../../flow.ts';

export function ProductQuestions({ s, dispatch, tried }: { s: CaseFlowStateV2; dispatch: (a: FlowActionV2) => void; tried: boolean }) {
  const qs = productQuestions(s.product);
  if (!qs.length) return null;
  return (
    <div data-testid="product-questions" data-product={s.product}>
      {qs.map((q, i) => (
        <ChoiceGrid
          key={q.id}
          name={`product-${q.id}`}
          legend={i === 0 ? q.text : `${q.text} (optional)`}
          options={q.options}
          value={s.reveal.product_answers[q.id] ?? null}
          onChange={(v) => dispatch({ type: 'set_product_answer', id: q.id, value: v })}
          error={i === 0 && tried && !s.reveal.product_answers[q.id] && s.reveal.verdict_code !== 'outside_expertise' ? 'Answer this question, or choose “Not sure”.' : null}
        />
      ))}
    </div>
  );
}

export default ProductQuestions;
