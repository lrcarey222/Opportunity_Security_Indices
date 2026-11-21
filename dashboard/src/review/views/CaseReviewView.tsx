// /review/case/:caseId: the blind-first case review (docs/QC_SYSTEM_DESIGN.md 5 step 4), driven by the pure state
// machine of review/flow.ts:
//   context  the case's context facts only (context_fields; any revealing fact id is dropped even if listed),
//            the question and the case key. Never the conclusion, fence, reason, why the case was chosen or the
//            link to the Explorer page (which shows the conclusion).
//   blind    the canonical answer options of the case (VERB_COLOR swatches for strategic choices), "I don't know /
//            outside my expertise", confidence 1-5, 1-3 driving factors from the case's own measures (labels
//            only, no values) and an optional rationale. Submitting stores and locks the answer.
//   reveal   only once the blind answer is stored: the Explorer's conclusion with its canonical label and
//            colour, the reveal facts, the decision trace with thresholds, the robustness class as its own
//            badge (separate from data quality), why NEIS chose the case, and "Open in the Explorer" (new tab).
//   verdict  1-7 (VERDICT_LABEL); 7 is preselected after an "I don't know" answer.
//   issue    the single most important miss: type + text, required for verdicts 3-6; optional metric ids from
//            the case's facts, the answer the reviewer would give, the evidence that would change their mind.
//   extras   DecisionUseStep when the case has decision-use questions, ThreatStep when it has threat ones.
//   done     the submission went through the build's sink (review/sink.ts); the next case is offered.
// The flow state autosaves as a draft on every change and resumes from it. A submission records the page
// context, the data release of the case file and the case snapshot's sha256.

import { useEffect, useMemo, useReducer, useState } from 'react';
import { Link, useLocation, useParams } from 'react-router';
import { Disclosure, EmptyState, Notice, Select, Tag } from '../../components/ui.tsx';
import { AIB_CLASS_NAME, SCENARIO_NAME } from '../../content/vocabulary.ts';
import { downloadText } from '../../lib/download.ts';
import { NOT_ASSESSED_HATCH, VERB_COLOR } from '../../posture/grammar.ts';
import type { Verb } from '../../posture/contract.ts';
import { getBuildInfo } from '../../shell/buildInfo.ts';
import { useTitle } from '../../shell/title.ts';
import { appCommit } from '../appInfo.ts';
import { ChoiceGrid, ConfidenceInput, MultiCheck, TextArea } from '../components/form.tsx';
import { capturePageContext, newSubmission } from '../context.ts';
import {
  blindErrors,
  canEnter,
  canReveal,
  DONT_KNOW_LABEL,
  flowReducer,
  initialFlowState,
  issueErrors,
  MAX_FACTORS,
  restoreFlowState,
  stepsFor,
  toDecisionUsePayload,
  toSubmissionPayload,
  toThreatPayload,
  verdictNeedsIssue,
  type CaseFlowState,
  type FlowStep,
} from '../flow.ts';
import { REVEALING_FACT_IDS } from '../manifest.ts';
import {
  DONT_KNOW,
  ISSUE_TYPE_LABEL,
  VERDICT_ISSUE_TYPE,
  VERDICT_LABEL,
  type CaseFact,
  type IssueType,
  type PageContext,
  type ReviewCase,
  type RobustnessClass,
  type Submission,
  type Verdict,
} from '../model.ts';
import { canonicalJson, sha256HexSync } from '../privacy.ts';
import { CASE_STATUS_LABEL, DRAFT_KIND, MODULE, useReviewCtx, type ReviewCtx } from '../ReviewLayout.tsx';
import { buildPacket, getSink, packetFileName, sinkMode } from '../sink.ts';
import { normaliseSectorTag } from '../tags.ts';
import { CASE_KIND_LABEL, MATCH_LABEL, REASON_LABEL } from './AssignmentsView.tsx';
import { DecisionUseStep } from './DecisionUseStep.tsx';
import { ThreatStep } from './ThreatStep.tsx';

/** Shown for a fact whose value the release does not carry. */
export const NOT_IN_RELEASE = 'not available in this release';
/** The factor id for "something not listed". */
export const OTHER_FACTOR = 'other';

export const STEP_LABEL: Readonly<Record<FlowStep, string>> = {
  context: 'The facts',
  blind: 'Your answer',
  reveal: 'The Explorer’s conclusion',
  verdict: 'Your verdict',
  issue: 'What it misses',
  extras: 'Use and threat',
  done: 'Done',
};

export const ROBUSTNESS_LABEL: Readonly<Record<RobustnessClass, { name: string; text: string }>> = {
  stable: { name: 'Stable', text: 'No alternative specification tested changes the conclusion.' },
  sensitive: { name: 'Sensitive', text: 'Small threshold moves do not change it, but a defensible alternative method does.' },
  fragile: { name: 'Fragile', text: 'A small move of one threshold changes the conclusion.' },
  not_testable: { name: 'Not testable', text: 'The inputs needed to test it are not in this release.' },
};

const isVerb = (v: string): v is Verb => Object.prototype.hasOwnProperty.call(VERB_COLOR, v);
const swatchOf = (v: string): string | undefined => (isVerb(v) ? VERB_COLOR[v] : v === 'not_assessed' ? NOT_ASSESSED_HATCH : undefined);
const now = () => new Date().toISOString();

/** The case key in words (never the conclusion). */
export function caseKeyText(c: Pick<ReviewCase, 'kind' | 'key'>): string {
  const k = c.key;
  const scn = k.scenario ? (SCENARIO_NAME as Record<string, string>)[k.scenario] ?? k.scenario : null;
  switch (c.kind) {
    case 'posture_cell':
    case 'sector':
      return [k.iso3, k.tech, k.stage].filter(Boolean).join(' · ');
    case 'aib_node':
      return [k.node, scn ? `scenario: ${scn}` : null].filter(Boolean).join(' · ');
    case 'compact':
      return [k.sid, scn ? `scenario: ${scn}` : null].filter(Boolean).join(' · ');
    case 'partnership':
      return k.pair ? k.pair.replace('_', ' and ') : '';
  }
}

/** The context facts that may show before the blind answer (a revealing id is never shown, even if listed). */
export const contextFactIds = (c: Pick<ReviewCase, 'context_fields'>): string[] => c.context_fields.filter((f) => !REVEALING_FACT_IDS.includes(f));

/** The driving-factor options: the case's measures by label (no values), context ones first, then 'other'. */
export function factorOptions(c: Pick<ReviewCase, 'context_fields' | 'reveal_fields' | 'snapshot'>): { primary: { value: string; label: string }[]; more: { value: string; label: string }[] } {
  const label = (id: string) => c.snapshot.facts[id]?.label ?? null;
  const ctx = contextFactIds(c).filter((id) => label(id));
  const more = c.reveal_fields.filter((id) => !REVEALING_FACT_IDS.includes(id) && !ctx.includes(id) && label(id));
  return {
    primary: [...ctx.map((id) => ({ value: id, label: label(id) as string })), { value: OTHER_FACTOR, label: 'Something not listed (say what in your reasoning)' }],
    more: more.map((id) => ({ value: id, label: label(id) as string })),
  };
}

/** A fact's shown value: its display, or NOT_IN_RELEASE when the release has no value. */
export function factValue(f: CaseFact | undefined): string {
  if (!f || f.value === null || f.value === undefined || (typeof f.value === 'number' && !Number.isFinite(f.value))) return NOT_IN_RELEASE;
  return f.display && f.display.trim() ? f.display : String(f.value);
}

function FactList({ c, ids, label, testid }: { c: ReviewCase; ids: string[]; label: string; testid: string }) {
  if (!ids.length) return null;
  return (
    <dl className="divide-y divide-white/10 border-y hairline" aria-label={label} data-testid={testid}>
      {ids.map((id) => {
        const f = c.snapshot.facts[id];
        const v = factValue(f);
        const missing = v === NOT_IN_RELEASE;
        const src = [f?.source, f?.source_year].filter((x) => x !== null && x !== undefined && x !== '').join(', ');
        return (
          <div key={id} className="grid gap-1 py-2 sm:grid-cols-[minmax(0,1fr)_minmax(0,1fr)] sm:gap-4" data-fact={id}>
            <dt className="text-sm text-paper-dim">{f?.label ?? id}</dt>
            <dd className="min-w-0 text-sm">
              <span className={missing ? 'italic text-paper-mute' : 'text-paper tabular'}>{v}</span>
              {(f?.note || src) && <span className="mt-0.5 block text-xs text-paper-mute">{[f?.note, src ? `Source: ${src}` : null].filter(Boolean).join(' · ')}</span>}
            </dd>
          </div>
        );
      })}
    </dl>
  );
}

function StepNav({ s, dispatch }: { s: CaseFlowState; dispatch: (a: { type: 'goto'; step: FlowStep }) => void }) {
  const steps = stepsFor(s);
  return (
    <ol className="mb-6 flex flex-wrap gap-1.5 text-xs" aria-label="Steps of this case">
      {steps.map((st, i) => {
        const current = st === s.step;
        const open = canEnter(s, st);
        return (
          <li key={st}>
            <button
              type="button"
              className="tag"
              disabled={!open || current}
              aria-current={current ? 'step' : undefined}
              style={current ? { borderColor: 'var(--color-signal)', color: 'var(--color-paper)' } : open ? undefined : { opacity: 0.5 }}
              onClick={() => dispatch({ type: 'goto', step: st })}
            >
              {i + 1}. {st === 'extras' ? extrasLabel(s) : STEP_LABEL[st]}
            </button>
          </li>
        );
      })}
    </ol>
  );
}

const extrasLabel = (s: Pick<CaseFlowState, 'has_decision_use' | 'has_threat'>) =>
  s.has_decision_use && s.has_threat ? STEP_LABEL.extras : s.has_threat ? 'The threat' : 'Using it';

function Conclusion({ c }: { c: ReviewCase }) {
  const k = c.snapshot.conclusion;
  const sw = swatchOf(k.value);
  return (
    <section className="panel border-l-2 border-l-signal px-4 py-4" aria-labelledby="conclusion-h" data-testid="conclusion">
      <h3 id="conclusion-h" className="eyebrow">
        The Explorer&rsquo;s conclusion
      </h3>
      <p className="mt-2 flex items-center gap-2 text-2xl font-medium text-paper">
        {sw && <span aria-hidden="true" className="inline-block h-4 w-4 flex-none" style={{ background: sw }} />}
        {k.label}
      </p>
      {k.detail && <p className="mt-1 text-sm text-paper-dim">{k.detail}</p>}
      {/* the reason is the Explorer's own text (its trace words or the AIB export), shown verbatim and marked as a quote */}
      {k.reason && (
        <p className="mt-2 text-sm text-paper-dim" data-testid="conclusion-reason">
          Why, in the Explorer&rsquo;s words: <q>{k.reason}</q>
        </p>
      )}
      {k.confidence && <p className="mt-1 text-xs text-paper-mute">Confidence in the call: {k.confidence}</p>}
    </section>
  );
}

function Badges({ c }: { c: ReviewCase }) {
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
              <Disclosure summary={`What changes it (${flips.length})`}>
                <ul className="list-disc space-y-1 pl-5 text-xs text-paper-dim">
                  {flips.map((f) => (
                    <li key={f.spec_id}>
                      {f.label}: {label(f.choice)}
                      {f.fence_changed ? ' (a different fence)' : ''}
                    </li>
                  ))}
                </ul>
              </Disclosure>
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

function Trace({ c }: { c: ReviewCase }) {
  const t = c.snapshot.trace ?? [];
  if (!t.length) return <p className="text-sm text-paper-mute">This case has no step-by-step decision trace in the release.</p>;
  const fmt = (x: number | null | undefined) => (typeof x === 'number' && Number.isFinite(x) ? String(x) : '');
  const label = (v: string) => c.answer_options.find((o) => o.value === v)?.label ?? v;
  return (
    <div className="overflow-x-auto">
      <table className="data-table w-full" data-testid="trace">
        <caption className="sr-only">How the conclusion was reached, test by test</caption>
        <thead>
          <tr>
            <th scope="col">Step</th>
            <th scope="col">Test</th>
            <th scope="col">Value</th>
            <th scope="col">Threshold</th>
            <th scope="col">Result</th>
          </tr>
        </thead>
        <tbody>
          {t.map((r, i) => (
            <tr key={`${r.step}-${i}`}>
              <td>{r.step.replace(/_/g, ' ')}</td>
              <td className="text-paper-mute">{r.test.replace(/_/g, ' ')}</td>
              <td className="tabular">{fmt(r.value)}</td>
              <td className="tabular">{fmt(r.threshold)}</td>
              <td>{r.step === 'choice' ? label(r.result) : r.result}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

/** The page context of a case submission: the review page's location, with the case's key and the release stamps. */
function casePageContext(ctx: ReviewCtx, c: ReviewCase, loc: { pathname: string; search: string }, params: Record<string, string | undefined>, at: string, metricIds: string[]): PageContext {
  const pc = capturePageContext(loc, params, getBuildInfo(), {
    captured_at: at,
    app_commit: appCommit(),
    data_release: c.data_release,
    displayed_result: c.snapshot.conclusion.label,
    metric_ids: metricIds,
    view: 'review-case',
  });
  const st = ctx.manifest.data_release.stamps;
  const k = c.key;
  return {
    ...pc,
    country: k.iso3 ?? pc.country,
    tech: k.tech ? (normaliseSectorTag(k.tech) ?? k.tech) : pc.tech,
    stage: k.stage ?? pc.stage,
    node: k.node ?? pc.node,
    pair: k.pair ?? pc.pair,
    sid: k.sid ?? pc.sid,
    scenario: k.scenario ?? pc.scenario,
    // the review page shows snapshots of these bundles (the review manifest's stamps), not bundles it loaded itself
    bundle_stamps: {
      posture: pc.bundle_stamps.posture ?? (st.posture ? { created: st.posture.created, git_commit: st.posture.git_commit } : null),
      country: pc.bundle_stamps.country ?? (st.country ? { created: st.country.created, git_commit: st.country.git_commit } : null),
      sector: pc.bundle_stamps.sector ?? (st.sector ? { created: null, git_commit: null, bundle_id: st.sector.bundle_id } : null),
      aib: pc.bundle_stamps.aib ?? (st.aib ? { created: st.aib.created, git_commit: null } : null),
      strategic: pc.bundle_stamps.strategic ?? (st.strategic ? { created: null, git_commit: null, bundle_id: st.strategic.bundle_id } : null),
      memos: pc.bundle_stamps.memos ?? (st.memos ? { created: null, git_commit: null, bundle_id: st.memos.manifest_sha256 } : null),
    },
  };
}

/** Send submissions through the build's sink. Packet mode keeps every one locally and downloads one file. */
async function deliver(ctx: ReviewCtx, subs: Submission[]): Promise<{ ok: boolean; message: string }> {
  const mode = sinkMode(import.meta.env);
  if (mode === 'packet') {
    for (const s of subs) ctx.storage.saveSubmission(s);
    try {
      const p = buildPacket(subs);
      downloadText(JSON.stringify(p, null, 2), packetFileName(p), 'application/json');
      return { ok: true, message: 'A file with your answers was downloaded: send it to NEIS. A copy stays in this browser.' };
    } catch {
      return { ok: false, message: 'The file could not be made; your answers are kept in this browser.' };
    }
  }
  const sink = getSink(import.meta.env, ctx.storage);
  let ok = true;
  let message = '';
  for (const s of subs) {
    const r = await sink.submit(s);
    ok &&= r.ok;
    if (!r.ok || !message) message = r.message;
  }
  return { ok, message };
}

function Buttons({ children }: { children: React.ReactNode }) {
  return <div className="mt-6 flex flex-wrap items-center gap-3">{children}</div>;
}

function CaseReview({ c }: { c: ReviewCase }) {
  const ctx = useReviewCtx();
  const loc = useLocation();
  const params = useParams();
  const cid = ctx.campaign.campaign_id;
  const a = ctx.assignmentOf(c.case_id) ?? { case_id: c.case_id, reason: 'self_selected' as const, match: 'outside' as const, optional: true };
  const [s, dispatch] = useReducer(flowReducer, null, () => restoreFlowState(ctx.storage.loadDraft(cid, DRAFT_KIND.case, c.case_id), c.case_id) ?? initialFlowState(c, { reason: a.reason, match: a.match }));
  const [tried, setTried] = useState(false);
  const [sending, setSending] = useState(false);
  const [result, setResult] = useState<{ ok: boolean; message: string } | null>(null);
  const [sendError, setSendError] = useState<string | null>(null);

  useEffect(() => dispatch({ type: 'start', at: now() }), []);
  // autosave on every change
  useEffect(() => {
    ctx.storage.saveDraft(cid, DRAFT_KIND.case, c.case_id, s);
  }, [s, cid, c.case_id, ctx.storage]);
  // the head's progress follows the stored answer and the submission
  const revealed = canReveal(s);
  const finished = s.step === 'done';
  const { refresh } = ctx;
  useEffect(() => refresh(), [revealed, finished, refresh]);
  useEffect(() => setTried(false), [s.step]);
  // a "don't know" answer preselects verdict 7 (the reviewer can change it)
  useEffect(() => {
    if (s.step === 'verdict' && s.reveal.verdict === null && s.suggested_verdict !== null) dispatch({ type: 'set_verdict', verdict: s.suggested_verdict });
  }, [s.step, s.reveal.verdict, s.suggested_verdict]);

  const options = useMemo(() => c.answer_options.filter((o) => o.value !== DONT_KNOW).map((o) => ({ value: o.value, label: o.label, definition: o.definition, color: swatchOf(o.value) })), [c.answer_options]);
  const factors = useMemo(() => factorOptions(c), [c]);
  const optionLabel = (v: string | null) => (v === DONT_KNOW ? DONT_KNOW_LABEL : (c.answer_options.find((o) => o.value === v)?.label ?? v ?? ''));
  const allFactIds = useMemo(() => [...new Set([...contextFactIds(c), ...c.reveal_fields])].filter((id) => c.snapshot.facts[id]), [c]);
  const lastBeforeDone: FlowStep = stepsFor(s).includes('extras') ? 'extras' : 'issue';

  const submit = async () => {
    const at = now();
    const fin = flowReducer(s, { type: 'next', at });
    if (fin.step !== 'done' || !ctx.profile) {
      setTried(true);
      return;
    }
    let subs: Submission[];
    try {
      const payload = toSubmissionPayload(fin, c);
      const opts = {
        profile: ctx.profile,
        campaign_id: cid,
        page_context: casePageContext(ctx, c, loc, params, at, fin.reveal.metric_ids),
        release: { data_release: c.data_release, app_commit: appCommit() },
        now: at,
      };
      subs = [newSubmission('case_review', payload, opts)];
      const du = toDecisionUsePayload(fin);
      if (du) subs.push(newSubmission('decision_use', du, opts));
      const th = toThreatPayload(fin, sha256HexSync(canonicalJson(c.snapshot.threat ?? null)));
      if (th) subs.push(newSubmission('threat_review', th, opts));
    } catch (e) {
      setSendError(e instanceof Error ? e.message : 'The review is incomplete.');
      return;
    }
    setSending(true);
    setSendError(null);
    const r = await deliver(ctx, subs);
    setSending(false);
    setResult(r);
    dispatch({ type: 'next', at });
  };

  const errors = s.step === 'blind' ? blindErrors(s) : s.step === 'issue' ? issueErrors(s) : [];
  const nextCase = ctx.assignments.find((x) => x.case_id !== c.case_id && ctx.statusOf(x.case_id) !== 'submitted') ?? null;
  const dk = s.blind.choice === DONT_KNOW;

  return (
    <article aria-labelledby="case-h">
      <div className="mb-2 flex flex-wrap items-center gap-2 text-xs">
        <span className="eyebrow">{CASE_KIND_LABEL[c.kind]}</span>
        <Tag>{REASON_LABEL[a.reason]}</Tag>
        <Tag>{MATCH_LABEL[a.match]}</Tag>
        {c.anchor && a.reason !== 'anchor' && <Tag>Anchor case</Tag>}
      </div>
      <h2 id="case-h" className="text-2xl font-medium text-paper">
        {c.title}
      </h2>
      <p className="mb-4 mt-1 text-sm text-paper-mute">{caseKeyText(c)}</p>
      <StepNav s={s} dispatch={dispatch} />

      {a.match !== 'in' && s.step !== 'done' && (
        <div className="mb-5">
          <Notice kind="info" title={a.match === 'outside' ? 'Outside your stated expertise' : 'Close to your stated expertise'}>
            Answer as far as you can judge; &ldquo;{DONT_KNOW_LABEL}&rdquo; is a useful answer here, and your answers are weighted by the expertise you stated.
          </Notice>
        </div>
      )}

      {s.step === 'context' && (
        <section aria-labelledby="context-h" data-testid="step-context">
          <h3 id="context-h" className="mb-1 text-lg font-medium text-paper">
            The facts
          </h3>
          <p className="mb-3 max-w-3xl text-sm text-paper-dim">{c.question}</p>
          <p className="mb-3 max-w-3xl text-xs text-paper-mute">These are the figures the Explorer works from. You will see its conclusion after you give your own answer.</p>
          {contextFactIds(c).length ? <FactList c={c} ids={contextFactIds(c)} label="Context facts" testid="context-facts" /> : <EmptyState>No context figures for this case: answer from your own knowledge.</EmptyState>}
          <Buttons>
            <button type="button" className="btn btn-primary" onClick={() => dispatch({ type: 'next' })}>
              Give your answer
            </button>
          </Buttons>
        </section>
      )}

      {s.step === 'blind' && (
        <section aria-labelledby="blind-h" data-testid="step-blind">
          <h3 id="blind-h" className="mb-3 text-lg font-medium text-paper">
            Your answer
          </h3>
          <ChoiceGrid
            legend={c.question}
            options={options}
            value={s.blind.choice}
            onChange={(v) => dispatch({ type: 'set_blind', patch: { choice: v } })}
            dontKnowValue={DONT_KNOW}
            error={tried && !s.blind.choice ? 'Choose an answer, or say you do not know.' : null}
          />
          {s.blind.choice && !dk && (
            <>
              <ConfidenceInput value={s.blind.confidence} onChange={(v) => dispatch({ type: 'set_blind', patch: { confidence: v } })} />
              <MultiCheck
                legend={`What drives your answer most? Pick 1 to ${MAX_FACTORS}.`}
                options={factors.primary}
                value={s.blind.factors.filter((f) => factors.primary.some((o) => o.value === f))}
                max={MAX_FACTORS - s.blind.factors.filter((f) => !factors.primary.some((o) => o.value === f)).length}
                onChange={(v) => dispatch({ type: 'set_blind', patch: { factors: [...v, ...s.blind.factors.filter((f) => !factors.primary.some((o) => o.value === f))] } })}
                error={tried && s.blind.factors.length === 0 ? 'Pick at least one factor that drives your answer.' : null}
              />
              {factors.more.length > 0 && (
                <Disclosure summary="More of the Explorer’s measures">
                  <MultiCheck
                    legend="Other measures that drive your answer"
                    options={factors.more}
                    value={s.blind.factors.filter((f) => factors.more.some((o) => o.value === f))}
                    max={MAX_FACTORS - s.blind.factors.filter((f) => !factors.more.some((o) => o.value === f)).length}
                    onChange={(v) => dispatch({ type: 'set_blind', patch: { factors: [...s.blind.factors.filter((f) => !factors.more.some((o) => o.value === f)), ...v] } })}
                  />
                </Disclosure>
              )}
            </>
          )}
          <TextArea label="Your reasoning (optional)" rows={3} value={s.blind.rationale ?? ''} onChange={(v) => dispatch({ type: 'set_blind', patch: { rationale: v } })} />
          {tried && errors.length > 0 && (
            <Notice kind="warning" title="Before you continue">
              {errors.join(' ')}
            </Notice>
          )}
          <Buttons>
            <button type="button" className="btn btn-ghost" onClick={() => dispatch({ type: 'back' })}>
              Back to the facts
            </button>
            <button
              type="button"
              className="btn btn-primary"
              onClick={() => {
                if (blindErrors(s).length) setTried(true);
                else dispatch({ type: 'submit_blind', at: now() });
              }}
            >
              Lock my answer and show the Explorer&rsquo;s conclusion
            </button>
            <span className="text-xs text-paper-mute">Your answer cannot be changed after this.</span>
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'reveal' && (
        <section aria-labelledby="reveal-h" className="space-y-5" data-testid="step-reveal">
          <h3 id="reveal-h" className="sr-only">
            The Explorer&rsquo;s conclusion and evidence
          </h3>
          <p className="text-sm text-paper-dim" data-testid="your-answer">
            Your answer: <span className="text-paper">{optionLabel(s.blind.choice)}</span>
            {!dk && s.blind.confidence ? ` (confidence ${s.blind.confidence} of 5)` : ''}
            {!dk && (s.blind.choice === c.snapshot.conclusion.value ? '. It matches the Explorer.' : '. It differs from the Explorer.')}
          </p>
          <Conclusion c={c} />
          <Badges c={c} />
          <div>
            <h4 className="mb-2 text-sm font-medium text-paper">The evidence behind it</h4>
            <FactList c={c} ids={c.reveal_fields.filter((id) => c.snapshot.facts[id])} label="Evidence" testid="reveal-facts" />
          </div>
          <div>
            <h4 className="mb-2 text-sm font-medium text-paper">How the decision was reached</h4>
            <Trace c={c} />
          </div>
          <div>
            <h4 className="mb-1 text-sm font-medium text-paper">Why NEIS chose this case</h4>
            <p className="max-w-3xl text-sm text-paper-dim">{c.why_selected}</p>
          </div>
          <p className="text-sm">
            <Link className="link" to={c.explorer_path} target="_blank" rel="noopener noreferrer">
              Open in the Explorer
            </Link>{' '}
            <span className="text-xs text-paper-mute">(opens in a new tab; the live page may use newer data than this case: release {c.data_release})</span>
          </p>
          <Buttons>
            <button type="button" className="btn btn-primary" onClick={() => dispatch({ type: 'next' })}>
              Give your verdict
            </button>
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'verdict' && (
        <section aria-labelledby="verdict-h" data-testid="step-verdict">
          <h3 id="verdict-h" className="mb-3 text-lg font-medium text-paper">
            Your verdict
          </h3>
          {dk && <p className="mb-3 text-sm text-paper-mute">You answered &ldquo;{DONT_KNOW_LABEL}&rdquo;, so the last verdict is preselected. Change it if you can judge the conclusion now.</p>}
          <ChoiceGrid
            legend="Which comes closest to your view of the Explorer's conclusion?"
            options={([1, 2, 3, 4, 5, 6, 7] as Verdict[]).map((v) => ({ value: String(v), label: `${v}. ${VERDICT_LABEL[v]}` }))}
            value={s.reveal.verdict === null ? null : String(s.reveal.verdict)}
            onChange={(v) => {
              const verdict = Number(v) as Verdict;
              dispatch({ type: 'set_verdict', verdict });
              const implied = VERDICT_ISSUE_TYPE[verdict];
              if (implied && verdictNeedsIssue(verdict)) dispatch({ type: 'set_issue', patch: { issue_type: implied } });
            }}
            error={tried && s.reveal.verdict === null ? 'Choose a verdict.' : null}
          />
          <Buttons>
            <button type="button" className="btn btn-ghost" onClick={() => dispatch({ type: 'back' })}>
              Back
            </button>
            <button type="button" className="btn btn-primary" onClick={() => (s.reveal.verdict === null ? setTried(true) : dispatch({ type: 'next' }))}>
              Continue
            </button>
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'issue' && (
        <section aria-labelledby="issue-h" data-testid="step-issue">
          <h3 id="issue-h" className="mb-1 text-lg font-medium text-paper">
            The single most important thing the Explorer misses
          </h3>
          <p className="mb-3 max-w-3xl text-sm text-paper-mute">
            {verdictNeedsIssue(s.reveal.verdict) ? 'Required for your verdict: say what kind of problem it is and describe it in a sentence or two.' : 'Optional: anything the Explorer misses, even though you agree.'}
          </p>
          <ChoiceGrid
            legend="What kind of problem is it?"
            options={(Object.keys(ISSUE_TYPE_LABEL) as IssueType[]).map((k) => ({ value: k, label: ISSUE_TYPE_LABEL[k] }))}
            value={s.reveal.issue_type}
            onChange={(v) => dispatch({ type: 'set_issue', patch: { issue_type: v as IssueType } })}
            error={tried && verdictNeedsIssue(s.reveal.verdict) && !s.reveal.issue_type ? 'Say what kind of problem it is.' : null}
          />
          <TextArea
            label="What does it miss?"
            required={verdictNeedsIssue(s.reveal.verdict)}
            rows={4}
            value={s.reveal.issue_text}
            onChange={(v) => dispatch({ type: 'set_issue', patch: { issue_text: v } })}
            error={tried && verdictNeedsIssue(s.reveal.verdict) && !s.reveal.issue_text.trim() ? 'Describe the single most important thing the Explorer misses.' : null}
          />
          <Disclosure summary="Add detail (optional)">
            <MultiCheck legend="Which of the figures shown are involved?" options={allFactIds.map((id) => ({ value: id, label: c.snapshot.facts[id].label }))} value={s.reveal.metric_ids} onChange={(v) => dispatch({ type: 'set_issue', patch: { metric_ids: v } })} />
            <Select
              label="What would you call it instead?"
              className="mb-4 max-w-md"
              value={s.reveal.suggested_choice ?? ''}
              options={[{ value: '', label: 'No suggestion' }, ...options.map((o) => ({ value: o.value, label: o.label }))]}
              onChange={(v) => dispatch({ type: 'set_issue', patch: { suggested_choice: v || null } })}
            />
            <TextArea label="What evidence would change your mind?" rows={2} value={s.reveal.evidence_wanted ?? ''} onChange={(v) => dispatch({ type: 'set_issue', patch: { evidence_wanted: v } })} />
          </Disclosure>
          {tried && errors.length > 0 && (
            <Notice kind="warning" title="Before you continue">
              {errors.join(' ')}
            </Notice>
          )}
          <Buttons>
            <button type="button" className="btn btn-ghost" onClick={() => dispatch({ type: 'back' })}>
              Back
            </button>
            {lastBeforeDone === 'issue' ? (
              <button type="button" className="btn btn-primary" disabled={sending} onClick={() => (issueErrors(s).length ? setTried(true) : void submit())}>
                Submit your review
              </button>
            ) : (
              <button type="button" className="btn btn-primary" onClick={() => (issueErrors(s).length ? setTried(true) : dispatch({ type: 'next' }))}>
                Continue
              </button>
            )}
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'extras' && (
        <section className="space-y-8" data-testid="step-extras">
          {s.has_decision_use && <DecisionUseStep value={s.decision_use} onChange={(patch) => dispatch({ type: 'set_decision_use', patch })} />}
          {s.has_threat && <ThreatStep threat={c.snapshot.threat} value={s.threat} onChange={(patch) => dispatch({ type: 'set_threat', patch })} />}
          <Buttons>
            <button type="button" className="btn btn-ghost" onClick={() => dispatch({ type: 'back' })}>
              Back
            </button>
            <button type="button" className="btn btn-primary" disabled={sending} onClick={() => void submit()}>
              Submit your review
            </button>
          </Buttons>
        </section>
      )}

      {sendError && (
        <div className="mt-4">
          <Notice kind="error" title="Not submitted">
            {sendError}
          </Notice>
        </div>
      )}

      {s.step === 'done' && (
        <section aria-labelledby="done-h" data-testid="step-done">
          <h3 id="done-h" className="mb-2 text-lg font-medium text-paper">
            Thank you: your review of this case is {CASE_STATUS_LABEL.submitted.toLowerCase()}
          </h3>
          {result && (
            <Notice kind={result.ok ? 'info' : 'warning'} title={result.ok ? 'Recorded' : 'Kept in this browser'}>
              {result.message}
            </Notice>
          )}
          <Buttons>
            {nextCase && (
              <Link className="btn btn-primary" to={`/review/case/${nextCase.case_id}${ctx.carry}`}>
                Next case: {ctx.caseById(nextCase.case_id)?.title ?? nextCase.case_id}
              </Link>
            )}
            <Link className="btn btn-ghost" to={`/review/cases${ctx.carry}`}>
              All your cases
            </Link>
          </Buttons>
        </section>
      )}
    </article>
  );
}

export default function CaseReviewView() {
  const { caseId } = useParams();
  const ctx = useReviewCtx();
  const c = ctx.caseById(caseId);
  useTitle(c ? c.title : 'Case', MODULE);
  if (!c)
    return (
      <Notice kind="warning" title="No such case">
        This campaign has no case {caseId}.{' '}
        <Link className="link" to={`/review/cases${ctx.carry}`}>
          See your cases
        </Link>
        .
      </Notice>
    );
  if (!ctx.profile)
    return (
      <EmptyState>
        <span>
          Before you review a case, tell us what you know:{' '}
          <Link className="link" to={`/review/profile${ctx.carry}`}>
            fill in your profile
          </Link>
          .
        </span>
      </EmptyState>
    );
  return <CaseReview key={c.case_id} c={c} />;
}
