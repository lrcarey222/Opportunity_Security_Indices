// "Why X rather than Y?" and "What would make Y more appropriate?" (docs/QC_UX_REVIEW.md section 8), first-class
// after the reveal: each neighbouring alternative the decision rule rejected (snapshot.reasoning.alternatives,
// computed by qc/reasoning.ts from the rule actually applied), why the Explorer did not choose it, what would have
// to change for it to become the better answer, and the decisive margin. This is where the judgment threshold a
// reviewer should scrutinise is exposed.

import type { AlternativeExplanation } from '../../model.ts';

export function AlternativesPanel({ conclusion, alternatives }: { conclusion: string; alternatives: readonly AlternativeExplanation[] | null | undefined }) {
  const alts = alternatives ?? [];
  return (
    <section aria-labelledby="alternatives-h" data-testid="alternatives">
      <h4 id="alternatives-h" className="mb-2 text-base font-medium text-paper">
        Why this, not the alternative?
      </h4>
      {!alts.length ? (
        <p className="text-sm italic text-paper-mute">No neighbouring alternative is computed for this case in this release.</p>
      ) : (
        <div className="grid gap-3 md:grid-cols-2">
          {alts.map((a) => (
            <article key={a.value} className="panel px-4 py-3" data-alternative={a.value} aria-label={`${conclusion} rather than ${a.label}`}>
              <h5 className="text-sm font-medium text-paper">
                Why {conclusion} rather than {a.label}?
              </h5>
              <p className="mt-1 text-sm text-paper-dim">{a.why_not}</p>
              <h5 className="mt-2 text-sm font-medium text-paper">What would make {a.label} more appropriate?</h5>
              <p className="mt-1 text-sm text-paper-dim">{a.would_need}</p>
              {a.margin && <p className="mt-1.5 text-xs text-paper-mute tabular">Decisive margin: {a.margin}</p>}
            </article>
          ))}
        </div>
      )}
    </section>
  );
}

export default AlternativesPanel;
