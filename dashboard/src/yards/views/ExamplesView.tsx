// /yards-fences/examples: one-click demonstrations that the framework gives different answers for
// different problems. Each is computed live by the engine from curated inputs and the data.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { useTitle } from '../../shell/title.ts';
import { EXAMPLES } from '../content/examples.ts';
import { stateSearch } from '../state/assessment.ts';
import { subjectsOf, useYards } from '../state/YardsContext.tsx';
import { evaluateStack } from '../engine/index.ts';
import { buildAssessment, evidenceFor } from '../data/adapter.ts';
import { SectionHead, YardBadge } from '../components/kit.tsx';
import { PAGE } from '../YardsLayout.tsx';

export default function ExamplesView() {
  useTitle('Examples', 'Draw the Fence');
  const { data } = useYards();
  const outcomes = useMemo(
    () =>
      EXAMPLES.map((ex) => {
        const st = ex.state();
        const subs = subjectsOf(st, data);
        const res = evaluateStack(
          subs.map((s) => ({
            componentId: s.key,
            label: s.label,
            assessment: buildAssessment(s, st.layers[s.key] ?? { answers: {}, scenario: {}, strategies: [] }, evidenceFor(s, data.bundle, data.reference), { country: data.bundle.country.name, withScenario: true }),
          })),
        );
        return { ex, st, res };
      }),
    [data],
  );
  return (
    <div className={PAGE}>
      <SectionHead eyebrow="Examples" title="Six assessments, six different problems">
        Each example uses the curated inputs and the data, plus a few assumptions of its own (marked YOUR JUDGMENT). The engine computes the result live; change any assumption and it changes.
      </SectionHead>
      <ul className="grid gap-5 md:grid-cols-2">
        {outcomes.map(({ ex, st, res }) => (
          <li key={ex.id}>
            <article className="card flex h-full flex-col p-6">
              <h2 className="text-xl font-medium text-paper">{ex.title}</h2>
              <p className="mt-1 text-sm text-paper-mute">{ex.question}</p>
              <div className="mt-4">
                <YardBadge yard={res.outcome} sub={res.layers.length > 1 ? `${res.layers.length} layers` : res.layers[0]?.result.recommendedPosture} />
              </div>
              {res.layers.length > 1 && (
                <ul className="mt-3 space-y-1">
                  {res.layers.map((l) => (
                    <li key={l.componentId} className="flex items-center gap-2 text-sm text-paper-dim">
                      <YardBadge yard={l.result.yard} size="sm" />
                      <span className="truncate">{l.label}</span>
                    </li>
                  ))}
                </ul>
              )}
              <p className="mt-4 text-sm leading-relaxed text-paper-dim">{ex.summary}</p>
              <p className="mt-2 text-sm text-paper-mute">Shows: {ex.shows}</p>
              <div className="mt-auto flex flex-wrap gap-3 pt-5">
                <Link className="btn btn-primary btn-sm" to={`/yards-fences/${ex.view}${stateSearch(st)}`}>
                  Open
                </Link>
                <Link className="btn btn-ghost btn-sm" to={`/yards-fences/assess${stateSearch({ ...st, step: 1 })}`}>
                  Walk through it
                </Link>
              </div>
            </article>
          </li>
        ))}
      </ul>
    </div>
  );
}
