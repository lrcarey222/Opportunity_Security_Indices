// /fence: "Build your fence". Choose instruments and intensities; the package is
// compared with the diagnosis by explicit rules (engine/policy.ts): under-fencing, over-fencing,
// mismatch, closed gates, missing exits. No model or AI decides fit.

import { useId, useMemo } from 'react';
import { Link } from 'react-router';
import { Segmented } from '../../components/ui.tsx';
import { useTitle } from '../title.ts';
import { evaluatePolicyFit, POLICIES, successMetrics, YARD_NAME } from '../engine/index.ts';
import type { Intensity, PolicyId, Purpose } from '../engine/index.ts';
import { layerOf, withLayer, type PolicyState } from '../state/assessment.ts';
import { useYards } from '../state/YardsContext.tsx';
import { FenceDiagram } from '../components/ComponentStack.tsx';
import { ProvBadge, SectionHead, YardBadge } from '../components/kit.tsx';
import { PAGE } from '../FenceLayout.tsx';
import { NoAssessment } from './ResultView.tsx';

const LEVELS: { v: Intensity; label: string }[] = [
  { v: 0, label: 'Off' },
  { v: 1, label: 'Low' },
  { v: 2, label: 'Medium' },
  { v: 3, label: 'High' },
];

const PURPOSE_LABEL: Record<Purpose, string> = {
  secure: 'Secure the vulnerable layer',
  buffer: 'Bridge a cutoff',
  build: 'Build capability',
  protect: 'Protect from imports',
  open: 'Keep gates open',
  discipline: 'Discipline and exit',
};

const NEED_LABEL = { required: 'needed', helpful: 'helpful', neutral: 'optional', avoid: 'not indicated' } as const;

function PolicyRow({ id, label, description, level, onChange }: { id: PolicyId; label: string; description: string; level: Intensity; onChange: (v: Intensity) => void }) {
  const name = useId();
  return (
    <tr className="border-b hairline align-top last:border-b-0">
      <th scope="row" className="py-2.5 pr-3 text-left font-normal">
        <span className="block text-sm text-paper">{label}</span>
        <span className="block text-xs text-paper-mute">{description}</span>
      </th>
      {LEVELS.map((l) => (
        <td key={l.v} className="px-1 py-2.5 text-center">
          <label className="inline-flex cursor-pointer flex-col items-center gap-1 text-xs text-paper-mute">
            <input type="radio" className="checkbox" name={name} checked={level === l.v} onChange={() => onChange(l.v)} aria-label={`${label}: ${l.label}`} data-policy={id} />
            <span className="sm:sr-only">{l.label}</span>
          </label>
        </td>
      ))}
    </tr>
  );
}

export default function FenceView() {
  useTitle('Build your fence');
  const { state, subjects, focus, evaluate, update, href } = useYards();
  const e = useMemo(() => (focus ? evaluate(focus) : null), [focus, evaluate]);
  if (!state.sel || !focus || !e) return <NoAssessment />;
  const r = e.scenarioResult;
  const pkg: PolicyState = layerOf(state, focus.key).policy ?? { levels: {}, scope: 'control-layer' };
  const fit = evaluatePolicyFit(r, { levels: pkg.levels, securityScope: pkg.scope });
  const setPkg = (f: (p: PolicyState) => PolicyState) => update((s) => withLayer(s, focus.key, (l) => ({ ...l, policy: f(l.policy ?? { levels: {}, scope: 'control-layer' }) })));
  const variant = r.yard === 'indeterminate' || r.yard === 'mixed' ? 'indeterminate' : r.variant;
  const sev = { warning: 'border-alert', caution: 'border-signal', info: 'border-ink-500', ok: 'border-brand-green' } as const;
  return (
    <div className={PAGE}>
      <SectionHead eyebrow="Policy sandbox" title="Build your fence">
        Choose instruments and their intensity, then compare the fence you have built with the one the diagnosis calls for. The comparison uses explicit rules; read them on the Methodology page.
      </SectionHead>

      {subjects.length > 1 && (
        <div className="mb-5 flex flex-wrap gap-2" role="group" aria-label="Layer">
          {subjects.map((s) => (
            <button key={s.key} type="button" className="tag yf-chip" aria-pressed={s.key === focus.key} onClick={() => update((st) => ({ ...st, focus: s.key }))}>
              {s.label}
            </button>
          ))}
        </div>
      )}

      <div className="panel mb-8 flex flex-wrap items-center gap-x-6 gap-y-3 p-4">
        <span className="text-sm text-paper-dim">Diagnosis for {focus.label}:</span>
        <YardBadge yard={r.yard} sub={r.recommendedPosture} />
        {e.hasScenario && <ProvBadge p="scenario" />}
        <Link className="link ml-auto text-sm" to={href('result')}>
          Why this diagnosis
        </Link>
      </div>

      <div className="grid gap-8 lg:grid-cols-[minmax(0,1fr)_24rem]">
        <section aria-labelledby="yf-pkg">
          <div className="mb-3 flex flex-wrap items-end justify-between gap-4">
            <h2 id="yf-pkg" className="text-lg font-medium text-paper">
              Your package
            </h2>
            <Segmented<'control-layer' | 'whole-product'>
              label="Security measures apply to"
              value={pkg.scope}
              options={[
                { value: 'control-layer', label: 'The control layer' },
                { value: 'whole-product', label: 'The whole product' },
              ]}
              onChange={(v) => setPkg((p) => ({ ...p, scope: v }))}
            />
          </div>
          <div className="panel overflow-x-auto px-4">
            <table className="w-full min-w-[34rem] text-sm">
              <caption className="sr-only">Policy instruments and intensities</caption>
              <thead>
                <tr className="border-b hairline">
                  <th scope="col" className="label py-2 text-left">
                    Policy
                  </th>
                  {LEVELS.map((l) => (
                    <th key={l.v} scope="col" className="label w-16 py-2 text-center">
                      {l.label}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {POLICIES.map((p) => (
                  <PolicyRow
                    key={p.id}
                    id={p.id}
                    label={p.label}
                    description={p.description}
                    level={(pkg.levels[p.id] ?? 0) as Intensity}
                    onChange={(v) => setPkg((q) => ({ ...q, levels: { ...q.levels, [p.id]: v } }))}
                  />
                ))}
              </tbody>
            </table>
          </div>
          <button type="button" className="btn btn-quiet btn-sm mt-3" onClick={() => setPkg(() => ({ levels: {}, scope: 'control-layer' }))}>
            Clear the package
          </button>
        </section>

        <div className="space-y-6 lg:sticky lg:top-4 lg:self-start">
          <section aria-labelledby="yf-fit" aria-live="polite">
            <h2 id="yf-fit" className="mb-3 text-lg font-medium text-paper">
              Does the fence fit the diagnosis?
            </h2>
            <ul className="space-y-3">
              {fit.findings.map((f, i) => (
                <li key={`${f.kind}-${i}`} className={`yf-flip border-l-4 bg-ink-800 p-3 ${sev[f.severity]}`}>
                  <div className="text-sm font-semibold text-paper">{f.title}</div>
                  <p className="mt-1 text-sm leading-relaxed text-paper-dim">{f.text}</p>
                </li>
              ))}
            </ul>
          </section>
          <section className="panel p-4" aria-label="Fence comparison">
            <FenceDiagram diagnosed={fit.diagnosed} chosen={fit.chosen} yard={r.yard} />
          </section>
          <section className="panel p-4" aria-labelledby="yf-cov">
            <h3 id="yf-cov" className="eyebrow mb-2">
              What the package does, against what is needed
            </h3>
            <ul className="space-y-2">
              {(Object.keys(PURPOSE_LABEL) as Purpose[]).map((pp) => (
                <li key={pp} className="text-sm">
                  <div className="flex justify-between gap-2">
                    <span className="text-paper-dim">{PURPOSE_LABEL[pp]}</span>
                    <span className={fit.needs[pp] === 'required' ? 'text-paper' : 'text-paper-mute'}>{NEED_LABEL[fit.needs[pp]]}</span>
                  </div>
                  <span className="sr-only">
                    {`Package coverage ${fit.coverage[pp]} of 6${fit.needs[pp] === 'avoid' && fit.coverage[pp] > 0 ? ', beyond what the diagnosis indicates' : ''}.`}
                  </span>
                  <div className="yf-meter mt-1" aria-hidden="true">
                    <span className="yf-grow" style={{ width: `${Math.min(100, (fit.coverage[pp] / 6) * 100)}%`, background: fit.needs[pp] === 'avoid' && fit.coverage[pp] > 0 ? 'var(--color-signal)' : undefined }} />
                  </div>
                </li>
              ))}
            </ul>
            <p className="mt-3 text-sm text-paper-dim">
              Cost pressure on buyers: <span className="text-paper">{fit.costPressure}</span>
            </p>
          </section>
        </div>
      </div>

      <section aria-labelledby="yf-metrics" className="mt-12">
        <SectionHead id="yf-metrics" title="How we will know it worked">
          The evidence to track for a {YARD_NAME[r.yard].toLowerCase()} diagnosis, and the trigger to reassess.
        </SectionHead>
        <ul className="grid gap-3 md:grid-cols-2">
          {successMetrics(variant).map((m) => (
            <li key={m} className="panel p-4 text-sm leading-relaxed text-paper-dim">
              {m}
            </li>
          ))}
        </ul>
      </section>
    </div>
  );
}
