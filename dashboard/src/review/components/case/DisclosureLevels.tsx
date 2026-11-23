// Progressive disclosure of the analysis after the reveal (docs/QC_UX_REVIEW.md section 12). Level 1 (always
// visible) is ReasoningReveal itself. Level 2, "Why this result?": each step's value against its bar and its
// drivers, the analytical assumptions, the source years of the evidence, data quality and robustness (kept
// apart), and why NEIS chose the case. Level 3, "Inspect the analysis": the decision trace test by test (value ->
// bar -> result), every Explorer figure of the case with its note and source, for a country case the provenance
// chain from the Explorer's posture data (components/WhyThisResult.tsx over qc/provenance.ts: raw value ->
// transformation -> component -> decision step -> conclusion, loaded only when opened), and the deep link to the
// Explorer view. Technical review mode opens levels 2 and 3 by default; decision review keeps them collapsed.
// Only ever rendered after the blind answer is stored (CaseReviewView gates it; nothing here checks the flow).

import { useEffect, useState } from 'react';
import { Link } from 'react-router';
import { Disclosure, Tag } from '../../../components/ui.tsx';
import { AIB_CLASS_NAME } from '../../../content/vocabulary.ts';
import { traceCodeWords, traceTestWords } from '../../../posture/grammar.ts';
import type { PostureCore } from '../../../posture/contract.ts';
import type { NodeDetail } from '../../../posture/detail.ts';
import type { ReviewCase, RobustnessClass } from '../../model.ts';
import { WhyThisResult } from '../WhyThisResult.tsx';
import { factValue, methodCardHref, NOT_IN_RELEASE, sourceLine } from './EvidencePanel.tsx';

export const ROBUSTNESS_LABEL: Readonly<Record<RobustnessClass, { name: string; text: string }>> = {
  stable: { name: 'Stable', text: 'No alternative specification tested changes the conclusion.' },
  sensitive: { name: 'Sensitive', text: 'Small threshold moves do not change it, but a defensible alternative method does.' },
  fragile: { name: 'Fragile', text: 'A small move of one threshold changes the conclusion.' },
  not_testable: { name: 'Not testable', text: 'The inputs needed to test it are not in this release.' },
};

const fmtNum = (x: number | null | undefined): string => {
  if (typeof x !== 'number' || !Number.isFinite(x)) return '';
  if (Math.abs(x) >= 1e9) return `${(x / 1e9).toFixed(1)}bn`;
  if (Math.abs(x) >= 1e6) return `${(x / 1e6).toFixed(1)}m`;
  return String(Number(x.toPrecision(3)));
};

export function Badges({ c }: { c: ReviewCase }) {
  const r = c.snapshot.robustness ?? null;
  const dq = c.snapshot.data_quality ?? null;
  const flips = r ? [...r.local_flips, ...r.structural_flips] : [];
  const label = (v: string) => c.answer_options.find((o) => o.value === v)?.label ?? (AIB_CLASS_NAME as Record<string, string>)[v] ?? v;
  return (
    <div className="grid gap-3 md:grid-cols-2">
      <div className="panel px-4 py-3" data-testid="robustness">
        <div className="eyebrow">Analytical robustness</div>
        {r ? (
          <>
            <p className="mt-1.5 text-sm text-paper">
              <Tag accent={r.class === 'fragile'}>{ROBUSTNESS_LABEL[r.class].name}</Tag> <span className="text-paper-dim">{ROBUSTNESS_LABEL[r.class].text}</span>
            </p>
            <p className="mt-1 text-xs text-paper-mute">
              {r.specs_run} alternative specifications tested on the published data{r.fence_change ? '; at least one changes the fence' : ''}.
            </p>
            {flips.length > 0 && (
              <ul className="mt-1 list-disc space-y-1 pl-5 text-xs text-paper-dim" aria-label="What changes the conclusion">
                {flips.map((f) => (
                  <li key={f.spec_id}>
                    {f.label}: {label(f.choice)}
                    {f.fence_changed ? ' (a different fence)' : ''}
                  </li>
                ))}
              </ul>
            )}
            {r.note && <p className="mt-1 text-xs text-paper-mute">{r.note}</p>}
          </>
        ) : (
          <p className="mt-1.5 text-sm text-paper-mute">Not tested for this case in this release.</p>
        )}
      </div>
      <div className="panel px-4 py-3" data-testid="data-quality">
        <div className="eyebrow">Data quality</div>
        <p className="mt-1.5 text-sm text-paper">{dq?.grade ? `Graded ${dq.grade}` : 'No grade in this release'}</p>
        {dq && dq.notes.length > 0 && (
          <ul className="mt-1 list-disc space-y-1 pl-5 text-xs text-paper-mute">
            {dq.notes.map((n) => (
              <li key={n}>{n}</li>
            ))}
          </ul>
        )}
      </div>
    </div>
  );
}

/** Level 2: "Why this result?" */
export function WhyThisResultLevel({ c, defaultOpen, campaign }: { c: ReviewCase; defaultOpen: boolean; campaign?: string | null }) {
  const r = c.snapshot.reasoning ?? null;
  const brief = c.snapshot.brief ?? null;
  const sources = (brief?.evidence ?? []).filter((f) => sourceLine(f));
  return (
    <div data-testid="level-2">
      <Disclosure summary="Why this result? Scores, bars, drivers, assumptions, sources and data quality" defaultOpen={defaultOpen}>
        <div className="space-y-4">
          {r && r.steps.length > 0 && (
            <section aria-labelledby="l2-steps-h">
              <h5 id="l2-steps-h" className="text-sm font-medium text-paper">
                Each step: value against its bar, and what drives it
              </h5>
              <ul className="mt-1 space-y-2">
                {r.steps.map((st) => (
                  <li key={st.id} data-step-detail={st.id} className="text-sm">
                    <span className="text-paper">{st.label}</span>
                    <span className="text-paper-dim">: {st.level}</span>
                    {(st.value !== null || st.threshold !== null) && (
                      <span className="text-xs text-paper-mute tabular">
                        {' '}
                        ({st.value !== null ? `value ${fmtNum(st.value)}` : 'no value'}
                        {st.threshold !== null ? `, bar ${fmtNum(st.threshold)}` : ''})
                      </span>
                    )}
                    {st.drivers.length > 0 && (
                      <ul className="mt-0.5 list-disc pl-5 text-xs text-paper-dim">
                        {st.drivers.map((d) => (
                          <li key={d}>{d}</li>
                        ))}
                      </ul>
                    )}
                  </li>
                ))}
              </ul>
            </section>
          )}
          {brief && brief.assumptions.length > 0 && (
            <section aria-labelledby="l2-ass-h">
              <h5 id="l2-ass-h" className="text-sm font-medium text-paper">
                The assumptions behind it
              </h5>
              <ul className="mt-1 list-disc space-y-0.5 pl-5 text-xs text-paper-dim">
                {brief.assumptions.map((a) => (
                  <li key={a.id}>
                    {a.label}
                    {a.method_card_id && (
                      <>
                        {' '}
                        (
                        <Link className="link" to={methodCardHref(a.method_card_id, campaign)} target="_blank" rel="noopener noreferrer">
                          method card
                        </Link>
                        )
                      </>
                    )}
                  </li>
                ))}
              </ul>
            </section>
          )}
          {sources.length > 0 && (
            <section aria-labelledby="l2-src-h">
              <h5 id="l2-src-h" className="text-sm font-medium text-paper">
                Sources and source years
              </h5>
              <ul className="mt-1 list-disc space-y-0.5 pl-5 text-xs text-paper-dim" data-testid="source-years">
                {sources.map((f) => (
                  <li key={f.id}>
                    {f.label}: {sourceLine(f)}
                  </li>
                ))}
              </ul>
            </section>
          )}
          <Badges c={c} />
          <section aria-labelledby="l2-why-h">
            <h5 id="l2-why-h" className="text-sm font-medium text-paper">
              Why NEIS chose this case
            </h5>
            <p className="mt-0.5 max-w-3xl text-sm text-paper-dim" data-testid="why-selected">
              {c.why_selected}
            </p>
          </section>
        </div>
      </Disclosure>
    </div>
  );
}

function Trace({ c }: { c: ReviewCase }) {
  const t = c.snapshot.trace ?? [];
  if (!t.length) return <p className="text-sm text-paper-mute">This case has no test-by-test trace in the release; the steps above give each value and bar.</p>;
  const fmt = (x: number | null | undefined) => (typeof x === 'number' && Number.isFinite(x) ? String(x) : '');
  const label = (v: string) => c.answer_options.find((o) => o.value === v)?.label ?? traceCodeWords(v);
  return (
    <div className="overflow-x-auto">
      <table className="data-table w-full" data-testid="trace">
        <caption className="sr-only">How the conclusion was reached, test by test</caption>
        <thead>
          <tr>
            <th scope="col">Test</th>
            <th scope="col">Value</th>
            <th scope="col">Bar</th>
            <th scope="col">Result</th>
          </tr>
        </thead>
        <tbody>
          {t.map((r, i) => (
            <tr key={`${r.step}-${i}`}>
              <td className="text-paper-dim">{traceTestWords(r.test)}</td>
              <td className="tabular">{fmt(r.value)}</td>
              <td className="tabular">{fmt(r.threshold)}</td>
              <td>{r.step === 'choice' ? label(r.result) : traceCodeWords(r.result)}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function Figures({ c }: { c: ReviewCase }) {
  const ids = [...new Set([...c.context_fields, ...c.reveal_fields])].filter((id) => c.snapshot.facts[id]);
  if (!ids.length) return null;
  return (
    <div className="overflow-x-auto">
      <table className="data-table w-full" data-testid="reveal-facts">
        <caption className="sr-only">Every figure of the case as the Explorer holds it</caption>
        <thead>
          <tr>
            <th scope="col">Figure</th>
            <th scope="col">Value</th>
            <th scope="col">Note and source</th>
          </tr>
        </thead>
        <tbody>
          {ids.map((id) => {
            const f = c.snapshot.facts[id];
            const v = factValue(f);
            const src = [f.source, f.source_year].filter((x) => x !== null && x !== undefined && x !== '').join(', ');
            return (
              <tr key={id} data-fact={id}>
                <td className="text-paper-dim">{f.label}</td>
                <td className={v === NOT_IN_RELEASE ? 'italic text-paper-mute' : 'tabular'}>{v}</td>
                <td className="text-xs text-paper-mute">{[f.note, src ? `Source: ${src}` : null].filter(Boolean).join(' · ')}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

type PostureState = { status: 'idle' | 'loading' | 'error' | 'missing' } | { status: 'ready'; cell: NodeDetail; core: PostureCore };

/** The posture provenance chain from the Explorer's live posture data (loaded only when this is opened). */
function PostureProvenance({ iso3, tech, stage }: { iso3: string; tech: string; stage: string }) {
  const [open, setOpen] = useState(false);
  const [st, setSt] = useState<PostureState>({ status: 'idle' });
  const loaded = st.status === 'ready' || st.status === 'missing';
  // one load per opening (a close before it lands drops it; the status itself is not a dependency)
  useEffect(() => {
    if (!open || loaded) return;
    let live = true;
    setSt({ status: 'loading' });
    import('../../../posture/store.ts')
      .then(async (m) => {
        const store = m.postureStore();
        const [core, detail] = await Promise.all([store.core(), store.detail(iso3)]);
        const cell = detail.cells.find((x) => x.key === `${tech}|${stage}`) ?? null;
        if (!live) return;
        setSt(cell ? { status: 'ready', cell, core } : { status: 'missing' });
      })
      .catch(() => live && setSt({ status: 'error' }));
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [open, iso3, tech, stage]);
  return (
    <details className="disclosure border-t hairline" data-testid="posture-provenance" onToggle={(e) => setOpen((e.currentTarget as HTMLDetailsElement).open)}>
      <summary className="py-2 text-xs font-medium text-paper-dim hover:text-paper">Provenance chain: raw value, transformation, component score, bar and conclusion</summary>
      <p className="text-[0.6875rem] text-paper-mute">Read from the Explorer&rsquo;s current posture data, which may be newer than this case&rsquo;s snapshot.</p>
      {st.status === 'loading' && (
        <p className="text-xs text-paper-mute" role="status">
          Loading the Explorer&rsquo;s data…
        </p>
      )}
      {st.status === 'error' && <p className="text-xs text-paper-mute">The Explorer&rsquo;s posture data could not be loaded here: use the Explorer link below.</p>}
      {st.status === 'missing' && <p className="text-xs text-paper-mute">This step is {NOT_IN_RELEASE} of the Explorer&rsquo;s current posture data.</p>}
      {st.status === 'ready' && <WhyThisResult cell={st.cell} core={st.core} iso3={iso3} defaultOpen />}
    </details>
  );
}

/** Level 3: "Inspect the analysis". */
export function InspectAnalysisLevel({ c, defaultOpen }: { c: ReviewCase; defaultOpen: boolean }) {
  const path = c.snapshot.reasoning?.technical_path || c.explorer_path;
  const k = c.key;
  return (
    <div data-testid="level-3">
      <Disclosure summary="Inspect the analysis: the trace, every figure, the provenance and the Explorer view" defaultOpen={defaultOpen}>
        <div className="space-y-4">
          <section aria-labelledby="l3-trace-h">
            <h5 id="l3-trace-h" className="mb-1 text-sm font-medium text-paper">
              The decision, test by test
            </h5>
            <Trace c={c} />
          </section>
          <section aria-labelledby="l3-figs-h">
            <h5 id="l3-figs-h" className="mb-1 text-sm font-medium text-paper">
              Every figure the Explorer holds for this case
            </h5>
            <Figures c={c} />
          </section>
          {c.kind === 'posture_cell' && k.iso3 && k.tech && k.stage && <PostureProvenance iso3={k.iso3} tech={k.tech} stage={k.stage} />}
          <p className="text-sm">
            <Link className="link" to={path} target="_blank" rel="noopener noreferrer">
              Open in the Explorer
            </Link>{' '}
            <span className="text-xs text-paper-mute">(opens in a new tab; the live page may use newer data than this case: release {c.data_release})</span>
          </p>
        </div>
      </Disclosure>
    </div>
  );
}
