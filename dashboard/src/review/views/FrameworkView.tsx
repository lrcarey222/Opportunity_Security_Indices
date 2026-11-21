// /review/framework: framework comprehension (docs/QC_SYSTEM_DESIGN.md 5 step 6). One scenario of
// public/data/review/framework_scenarios.json at a time: the reviewer picks what the framework implies (or
// "I don't know"), how confident they are and the terms that confused them. Only once the answer is recorded
// (a framework_answer submission through the build's sink) does the page show the intended interpretation,
// its explanation and whether the two matched: the intended answer is not in the page before that. A mismatch
// is recorded as framework ambiguity by the admin's aggregation (aggregate.ts), never as the reviewer's error.

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { Loading, Notice, ProgressBar, Tag } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { useOptionalReviewCtx } from '../ReviewLayout.tsx';
import { appCommit } from '../appInfo.ts';
import { newSubmission } from '../context.ts';
import { DONT_KNOW, type AnswerOption, type FrameworkAnswerPayload, type FrameworkScenario } from '../model.ts';
import { getSink } from '../sink.ts';
import { reviewStorage, type ReviewStorage } from '../storage.ts';
import { pickCampaign, reviewStore, type ReviewManifestDoc } from '../data/store.ts';
import { STRATEGIC_COMPACT_DECISIONS } from '../qc/answerOptions.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';
import { ChoiceGrid, ConfidenceInput, MultiCheck, TextArea } from '../components/form.tsx';
import { reviewerProfileOrAnonymous } from '../components/IssueReportDialog.tsx';

/**
 * The options a scenario offers. A compact-decision scenario about a strategic compact (its intended answer is
 * one of sectoral / narrower / none) offers only those three: the built option set also lists the operational
 * compact types, which answer a different question.
 */
export function scenarioOptions(s: FrameworkScenario): AnswerOption[] {
  const opts = s.options.filter((o) => o.value !== DONT_KNOW);
  if (s.option_set === 'compact_decision' && (STRATEGIC_COMPACT_DECISIONS as readonly string[]).includes(s.intended_answer)) return opts.filter((o) => (STRATEGIC_COMPACT_DECISIONS as readonly string[]).includes(o.value));
  return opts;
}

const labelOf = (s: FrameworkScenario, v: string): string => (v === DONT_KNOW ? DONT_KNOW_LABEL : (s.options.find((o) => o.value === v)?.label ?? v));

interface Loaded {
  manifest: ReviewManifestDoc;
  scenarios: FrameworkScenario[];
}

/** The page's own heading under the review layout's title (the layout holds the h1). */
function ViewHead({ eyebrow, title, lede, children }: { eyebrow: string; title: string; lede: string; children?: ReactNode }) {
  return (
    <header className="mb-5">
      <p className="eyebrow">{eyebrow}</p>
      <h2 className="mt-1 text-xl font-medium text-paper">{title}</h2>
      <p className="mt-2 max-w-3xl text-sm leading-relaxed text-paper-dim">{lede}</p>
      {children}
    </header>
  );
}

export default function FrameworkView({ storage }: { storage?: ReviewStorage } = {}) {
  useTitle('Framework scenarios', 'Expert review');
  const layout = useOptionalReviewCtx();
  const store = storage ?? layout?.storage ?? reviewStorage();
  const [sp, setSp] = useSearchParams();
  const [data, setData] = useState<Loaded | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [version, setVersion] = useState(0);

  useEffect(() => {
    let live = true;
    const rs = reviewStore();
    Promise.all([rs.manifest(), rs.frameworkScenarios()]).then(
      ([manifest, doc]) => {
        if (!live) return;
        const ids = pickCampaign(manifest, sp.get('campaign'))?.framework_scenario_ids ?? [];
        setData({ manifest, scenarios: ids.length ? doc.scenarios.filter((s) => ids.includes(s.scenario_id)) : doc.scenarios });
      },
      (e) => live && setError(String((e as Error)?.message ?? e)),
    );
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const campaignId = data ? (pickCampaign(data.manifest, sp.get('campaign'))?.campaign_id ?? null) : null;
  const answered = useMemo(() => {
    const m = new Map<string, FrameworkAnswerPayload>();
    for (const s of store.listSubmissions(campaignId ?? undefined)) if (s.kind === 'framework_answer') m.set((s.payload as FrameworkAnswerPayload).scenario_id, s.payload as FrameworkAnswerPayload);
    return m;
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [store, campaignId, version]);

  if (error) return <Notice kind="error">The framework scenarios could not be loaded: {error}</Notice>;
  if (!data) return <Loading label="Loading the scenarios…" />;
  const list = data.scenarios;
  if (!list.length) return <Notice kind="info">This campaign has no framework scenarios.</Notice>;
  const idx = Math.max(0, list.findIndex((s) => s.scenario_id === sp.get('scenario')));
  const s = list[idx];
  const go = (i: number) => {
    const q = new URLSearchParams(sp);
    q.set('scenario', list[(i + list.length) % list.length].scenario_id);
    setSp(q, { replace: true });
  };
  return (
    <div className="max-w-3xl">
      <ViewHead
        eyebrow="Framework"
        title="What would the framework say?"
        lede="Short scenarios. Pick what you think the Explorer's framework implies before you see its intended reading. A mismatch tells us where the framework is unclear, not that you were wrong."
      >
        <div className="mt-4">
          <ProgressBar value={answered.size} max={list.length} label={`${answered.size} of ${list.length} scenarios answered`} />
        </div>
      </ViewHead>
      <nav className="mt-4 flex flex-wrap items-center justify-between gap-2" aria-label="Scenarios">
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => go(idx - 1)}>
          &larr; Previous
        </button>
        <span className="text-sm text-paper-mute tabular">
          Scenario {idx + 1} of {list.length}
        </span>
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => go(idx + 1)}>
          Next &rarr;
        </button>
      </nav>
      <ScenarioForm
        key={s.scenario_id}
        s={s}
        campaignId={campaignId}
        dataRelease={data.manifest.data_release.id}
        done={answered.get(s.scenario_id) ?? null}
        store={store}
        onAnswered={() => {
          setVersion((v) => v + 1);
          layout?.refresh();
        }}
        onNext={() => go(idx + 1)}
      />
      <p className="mt-6 text-sm text-paper-mute">
        <Link className="link" to="/review">
          &larr; Back to the review
        </Link>
      </p>
    </div>
  );
}

interface ScenarioDraft {
  answer: string | null;
  confidence: 1 | 2 | 3 | 4 | 5 | null;
  confusing_terms: string[];
  comment: string;
}

function ScenarioForm({
  s, campaignId, dataRelease, done, store, onAnswered, onNext,
}: {
  s: FrameworkScenario;
  campaignId: string | null;
  dataRelease: string;
  done: FrameworkAnswerPayload | null;
  store: ReviewStorage;
  onAnswered: () => void;
  onNext: () => void;
}) {
  const slot = [campaignId ?? '_none', 'framework_answer', s.scenario_id] as const;
  const [d, setD] = useState<ScenarioDraft>(() => ({ answer: null, confidence: null, confusing_terms: [], comment: '', ...(store.loadDraft<Partial<ScenarioDraft>>(...slot) ?? {}) }));
  const [result, setResult] = useState<FrameworkAnswerPayload | null>(done);
  const [tried, setTried] = useState(false);
  const [busy, setBusy] = useState(false);
  const [msg, setMsg] = useState<string | null>(null);
  const set = (patch: Partial<ScenarioDraft>) =>
    setD((prev) => {
      const next = { ...prev, ...patch };
      store.saveDraft(...slot, next);
      return next;
    });

  const submit = async () => {
    setTried(true);
    if (!d.answer || busy) return;
    setBusy(true);
    const payload: FrameworkAnswerPayload = {
      scenario_id: s.scenario_id,
      answer: d.answer,
      intended_answer: s.intended_answer,
      correct: d.answer === s.intended_answer,
      confidence: d.confidence,
      confusing_terms: [...d.confusing_terms],
      comment: d.comment.trim() || null,
    };
    const sub = newSubmission('framework_answer', payload, {
      profile: reviewerProfileOrAnonymous(store),
      campaign_id: campaignId,
      page_context: null,
      release: { data_release: dataRelease, app_commit: appCommit() },
    });
    const r = await getSink(undefined, store).submit(sub);
    setBusy(false);
    setMsg(r.message);
    if (r.ok) {
      store.deleteDraft(...slot);
      setResult(payload);
      onAnswered();
    }
  };

  return (
    <article className="panel mt-4 p-5" data-testid="framework-scenario" data-scenario={s.scenario_id}>
      <div className="flex flex-wrap items-center gap-2">
        <span className="eyebrow">{s.concept}</span>
        {result && <Tag>Answered</Tag>}
      </div>
      <p className="mt-3 text-[0.9375rem] leading-relaxed text-paper">{s.prompt}</p>
      {result ? (
        <Reveal s={s} r={result} onNext={onNext} />
      ) : (
        <form
          className="mt-5"
          noValidate
          onSubmit={(e) => {
            e.preventDefault();
            void submit();
          }}
        >
          <ChoiceGrid
            legend="What does the framework imply?"
            options={scenarioOptions(s)}
            value={d.answer}
            onChange={(v) => set({ answer: v })}
            dontKnowValue={DONT_KNOW}
            error={tried && !d.answer ? 'Pick an answer, or say you do not know.' : null}
          />
          <ConfidenceInput value={d.confidence} onChange={(v) => set({ confidence: v })} />
          {s.terms.length > 0 && (
            <MultiCheck legend="Which terms, if any, were unclear?" options={s.terms.map((t) => ({ value: t, label: t }))} value={d.confusing_terms} onChange={(v) => set({ confusing_terms: v })} />
          )}
          <TextArea label="Comment (optional)" rows={2} value={d.comment} onChange={(v) => set({ comment: v })} />
          <button type="submit" className="btn btn-primary btn-sm" disabled={busy}>
            {busy ? 'Recording…' : 'Record my answer and show the intended reading'}
          </button>
          {msg && (
            <p role="status" className="mt-2 text-sm text-paper-dim">
              {msg}
            </p>
          )}
        </form>
      )}
    </article>
  );
}

function Reveal({ s, r, onNext }: { s: FrameworkScenario; r: FrameworkAnswerPayload; onNext: () => void }) {
  const dontKnow = r.answer === DONT_KNOW;
  return (
    <div className="mt-5 border-t hairline pt-4" data-testid="framework-reveal">
      <p className="text-sm text-paper-dim">
        Your answer: <strong className="text-paper">{labelOf(s, r.answer)}</strong>
      </p>
      <p className="mt-1 text-sm text-paper-dim">
        The intended reading: <strong className="text-paper">{labelOf(s, s.intended_answer)}</strong>
      </p>
      <p className="mt-2 text-sm" data-testid="framework-match" data-match={dontKnow ? 'dont_know' : r.answer === s.intended_answer ? 'yes' : 'no'}>
        {dontKnow ? 'You did not pick an answer: that counts as no reading, not a mismatch.' : r.answer === s.intended_answer ? 'They match.' : 'They differ: this is recorded as a place where the framework may be unclear.'}
      </p>
      <p className="mt-3 text-sm leading-relaxed text-paper-dim">{s.explanation}</p>
      <button type="button" className="btn btn-ghost btn-sm mt-4" onClick={onNext}>
        Next scenario &rarr;
      </button>
    </div>
  );
}
