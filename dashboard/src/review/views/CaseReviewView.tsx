// /review/case/:caseId: the blind-first case review, UX v2 (docs/QC_UX_REVIEW.md; NEIS brief: reviewers are blind
// to the Explorer's conclusion, not to the information needed to form one), driven by the pure state machine of
// review/flow.ts (flowReducerV2):
//   brief     the neutral case brief (components/case/CaseBrief.tsx, from snapshot.brief): identity, decision
//             question and context, objective, time horizon, scenario, the six-stage strip, data warnings, the
//             product's tailored section, 5-8 facts with reading, source and vintage, the analytical assumptions
//             apart from the evidence, what the data cannot show, definitions. Never the conclusion, a model
//             score, the reasoning, an alternative, a reveal-only fact, robustness or why the case was chosen.
//   blind     "Based on the evidence above, what ... would you recommend?" (components/case/BlindQuestion.tsx):
//             the canonical conclusions with one-line definitions, then none of these / insufficient evidence
//             (-> what information is missing?) / outside my expertise; Low / Medium / High confidence; the one
//             most important driver. A sector chain selects its stages and the one to act on first. Locking
//             stores the answer; it cannot change after.
//   reveal    only once the blind answer is stored: "Explorer conclusion: X", its one-sentence summary, the chain
//             of steps, "Therefore: X", the product's remainder, "Why this, not the alternative?", and levels 2
//             ("Why this result?") and 3 ("Inspect the analysis", with the Explorer deep link). Technical review
//             mode opens levels 2 and 3 by default.
//   verdict   the prior as a computed fact, the ten-way statement, validity, persuasion, and where it fails.
//   red_team  only after a disagreeing statement or a low-confidence answer.
//   product   the product's questions (bottleneck / complementarity / threat), when it has any.
//   extras    DecisionUseStep and ThreatStep for cases flagged so.
//   wrap      only when the reviewer wrote notes: convert each note into a typed report.
//   done      the submission went through the build's sink; links to the completion summary and the next case.
// The reviewer's notes (components/case/NotesPanel.tsx) are available from the brief to the wrap-up. Before the
// first case the reviewer is sent to the calibration screen (/review/start) with a return path. The flow state
// autosaves (storage.ts saveCaseDraftV2; a v1 draft is migrated by restoreFlowStateV2) and resumes. A submission
// records the page context, the data release and the case snapshot's sha256.

import { lazy, Suspense, useEffect, useMemo, useReducer, useState, type ReactNode } from 'react';
import { Link, Navigate, useLocation, useParams } from 'react-router';
import { EmptyState, Loading, Notice, Tag } from '../../components/ui.tsx';
import { SCENARIO_NAME } from '../../content/vocabulary.ts';
import { downloadText } from '../../lib/download.ts';
import { STAGE_DISPLAY, isSupplyStage, stageOfDisplay } from '../../lib/stages.ts';
import { getBuildInfo } from '../../shell/buildInfo.ts';
import { useTitle } from '../../shell/title.ts';
import { appCommit } from '../appInfo.ts';
import { capturePageContext, newSubmission } from '../context.ts';
import {
  canEnterV2,
  canRevealV2,
  DONT_KNOW_LABEL,
  flowReducerV2,
  initialFlowStateV2,
  isReviewerStateChoice,
  priorDiffers,
  restoreFlowStateV2,
  stageJaccard,
  stepErrorsV2,
  stepsForV2,
  toDecisionUsePayload,
  toSubmissionPayloadV2,
  toThreatPayload,
  type CaseFlowStateV2,
  type FlowActionV2,
  type FlowStepV2,
} from '../flow.ts';
import { REVEALING_FACT_IDS } from '../manifest.ts';
import {
  CONFIDENCE_LEVEL_LABEL,
  DONT_KNOW,
  REVIEWER_STATE_LABEL,
  type CaseFact,
  type CaseKind,
  type PageContext,
  type ReviewCase,
  type ReviewProduct,
  type Submission,
} from '../model.ts';
import { canonicalJson, sha256HexSync } from '../privacy.ts';
import { CASE_STATUS_LABEL, MODULE, REVIEW_PATH, useReviewCtx, withQuery, type ReviewCtx } from '../ReviewLayout.tsx';
import { buildPacket, getSink, packetFileName, sinkMode } from '../sink.ts';
import { effectiveReviewMode } from '../storage.ts';
import { normaliseSectorTag } from '../tags.ts';
import { MATCH_LABEL, REASON_LABEL } from './AssignmentsView.tsx';
import { BlindQuestion } from '../components/case/BlindQuestion.tsx';
import { CaseBrief } from '../components/case/CaseBrief.tsx';
import { factValue, NOT_IN_RELEASE } from '../components/case/EvidencePanel.tsx';
import { NotesPanel, NotesWrap } from '../components/case/NotesPanel.tsx';
import type { PriorFact } from '../components/case/VerdictStep.tsx';

export { factValue, NOT_IN_RELEASE } from '../components/case/EvidencePanel.tsx';

// the post-reveal steps load only once the blind answer is stored (one chunk: components/case/PostReveal.tsx)
const postReveal = () => import('../components/case/PostReveal.tsx');
const ReasoningReveal = lazy(() => postReveal().then((m) => ({ default: m.ReasoningReveal })));
const VerdictStep = lazy(() => postReveal().then((m) => ({ default: m.VerdictStep })));
const RedTeamStep = lazy(() => postReveal().then((m) => ({ default: m.RedTeamStep })));
const ProductQuestions = lazy(() => postReveal().then((m) => ({ default: m.ProductQuestions })));
const DecisionUseStep = lazy(() => postReveal().then((m) => ({ default: m.DecisionUseStep })));
const ThreatStep = lazy(() => postReveal().then((m) => ({ default: m.ThreatStep })));

function Later({ children }: { children: ReactNode }) {
  return <Suspense fallback={<Loading label="Loading the next step" />}>{children}</Suspense>;
}

/** The case kinds in words (the head of the case page). */
export const CASE_KIND_WORDS: Readonly<Record<CaseKind, string>> = {
  posture_cell: 'Country review',
  aib_node: 'Sector review: one allied supply-chain step',
  sector: 'Sector review',
  sector_chain: 'Sector review: a whole supply chain',
  compact: 'Compact review',
  partnership: 'Partnership review',
};

/** The product step's name in the step list. */
const PRODUCT_STEP_LABEL: Readonly<Record<ReviewProduct, string>> = {
  country: 'Country questions',
  sector: 'The bottleneck',
  partnership: 'The relationship',
  compact: 'Does it help?',
};

export const STEP_LABEL: Readonly<Record<FlowStepV2, string>> = {
  brief: 'The case',
  blind: 'Your assessment',
  reveal: 'The Explorer’s reasoning',
  verdict: 'Your verdict',
  red_team: 'Test the conclusion',
  product: 'Product questions',
  extras: 'Use and threat',
  wrap: 'Your notes',
  done: 'Done',
};

const now = () => new Date().toISOString();
const newNoteId = () => {
  const c = (globalThis as { crypto?: { randomUUID?: () => string } }).crypto;
  return `note-${c?.randomUUID ? c.randomUUID() : `${Date.now().toString(36)}-${Math.random().toString(16).slice(2)}`}`;
};

/** The case key in words (never the conclusion). */
export function caseKeyText(c: Pick<ReviewCase, 'kind' | 'key'>): string {
  const k = c.key;
  const scn = k.scenario ? ((SCENARIO_NAME as Record<string, string>)[k.scenario] ?? k.scenario) : null;
  switch (c.kind) {
    case 'posture_cell':
    case 'sector':
      return [k.iso3, k.tech, k.stage].filter(Boolean).join(' · ');
    case 'aib_node':
      return [k.node, scn ? `scenario: ${scn}` : null].filter(Boolean).join(' · ');
    case 'compact':
    case 'sector_chain':
      return [k.sid, scn ? `scenario: ${scn}` : null].filter(Boolean).join(' · ');
    case 'partnership':
      return k.pair ? k.pair.replace('_', ' and ') : '';
  }
}

/** The context facts that may show before the blind answer (a revealing id is never shown, even if listed). */
export const contextFactIds = (c: Pick<ReviewCase, 'context_fields'>): string[] => c.context_fields.filter((f) => !REVEALING_FACT_IDS.includes(f));

/** The extras step's name: decision use, the threat, or both. */
const extrasLabel = (s: Pick<CaseFlowStateV2, 'has_decision_use' | 'has_threat'>) => (s.has_decision_use && s.has_threat ? STEP_LABEL.extras : s.has_threat ? 'The threat' : 'Using it');

export function stepLabel(s: CaseFlowStateV2, st: FlowStepV2): string {
  if (st === 'extras') return extrasLabel(s);
  if (st === 'product') return PRODUCT_STEP_LABEL[s.product];
  return STEP_LABEL[st];
}

function StepNav({ s, dispatch }: { s: CaseFlowStateV2; dispatch: (a: FlowActionV2) => void }) {
  const steps = stepsForV2(s);
  const at = Math.max(0, steps.indexOf(s.step));
  return (
    <nav aria-label="Steps of this case" className="mb-5">
      <p className="mb-1.5 text-xs text-paper-mute tabular" data-testid="step-progress">
        Step {at + 1} of {steps.length}
      </p>
      <ol className="flex flex-wrap gap-1.5 text-xs">
        {steps.map((st, i) => {
          const current = st === s.step;
          const open = canEnterV2(s, st);
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
                {i + 1}. {stepLabel(s, st)}
              </button>
            </li>
          );
        })}
      </ol>
    </nav>
  );
}

/** The case's facts before the blind answer, for a case built without a brief (v1 data). */
function LegacyContext({ c }: { c: ReviewCase }) {
  const ids = contextFactIds(c).filter((id) => c.snapshot.facts[id]);
  return (
    <div className="space-y-3" data-testid="legacy-context">
      <h3 className="max-w-3xl text-lg font-medium text-paper">{c.question}</h3>
      {ids.length ? (
        <dl className="divide-y divide-white/10 border-y hairline" aria-label="Context facts" data-testid="context-facts">
          {ids.map((id) => {
            const f: CaseFact = c.snapshot.facts[id];
            const v = factValue(f);
            const src = [f.source, f.source_year].filter((x) => x !== null && x !== undefined && x !== '').join(', ');
            return (
              <div key={id} className="grid gap-1 py-2 sm:grid-cols-[minmax(0,1fr)_minmax(0,1fr)] sm:gap-4" data-fact={id}>
                <dt className="text-sm text-paper-dim">{f.label}</dt>
                <dd className="min-w-0 text-sm">
                  <span className={v === NOT_IN_RELEASE ? 'italic text-paper-mute' : 'text-paper tabular'}>{v}</span>
                  {(f.note || src) && <span className="mt-0.5 block text-xs text-paper-mute">{[f.note, src ? `Source: ${src}` : null].filter(Boolean).join(' · ')}</span>}
                </dd>
              </div>
            );
          })}
        </dl>
      ) : (
        <EmptyState>No context figures for this case: answer from your own knowledge.</EmptyState>
      )}
    </div>
  );
}

const stageWord = (x: string) => (isSupplyStage(x) ? STAGE_DISPLAY[x] : x);

/** The label of a stored blind choice. */
export function choiceLabel(c: Pick<ReviewCase, 'answer_options'>, v: string | null): string {
  if (!v) return '';
  if (v === DONT_KNOW) return DONT_KNOW_LABEL;
  return c.answer_options.find((o) => o.value === v)?.label ?? REVIEWER_STATE_LABEL[v] ?? stageWord(v);
}

/** The prior as a computed fact: the blind answer against the Explorer's conclusion (and, for a chain, its stages). */
export function priorOfCase(c: ReviewCase, s: CaseFlowStateV2): PriorFact {
  const explorerValue = c.snapshot.conclusion.value;
  const differs = priorDiffers(s.blind.choice, explorerValue);
  let sets: PriorFact['sets'] = null;
  if (c.kind === 'sector_chain' && s.blind.choice && !isReviewerStateChoice(s.blind.choice)) {
    const exp = (c.snapshot.reasoning?.sector?.explorer_stages ?? []).map((x) => stageOfDisplay(x) ?? x);
    const yours = s.blind.choices.length ? s.blind.choices : [s.blind.choice];
    sets = { yours, explorer: exp, overlap: stageJaccard(yours, exp) };
  }
  return { differs, yours: choiceLabel(c, s.blind.choice), explorer: c.snapshot.conclusion.label, sets };
}

/** "Your answer: ..." of the reveal. */
function yourAnswerText(c: ReviewCase, s: CaseFlowStateV2, prior: PriorFact): string {
  const b = s.blind;
  const parts = [`Your answer: ${choiceLabel(c, b.choice)}`];
  if (c.kind === 'sector_chain' && b.choices.length > 1) parts.push(` (stages: ${b.choices.map(stageWord).join(', ')}; first: ${stageWord(b.choice ?? '')})`);
  if (b.legacy?.confidence) parts.push(` (confidence ${b.legacy.confidence} of 5)`);
  else if (b.confidence_level) parts.push(` (confidence: ${CONFIDENCE_LEVEL_LABEL[b.confidence_level].toLowerCase()})`);
  if (prior.differs !== null) parts.push(prior.differs ? '. It differs from the Explorer.' : '. It matches the Explorer.');
  return parts.join('');
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

function Errors({ errors }: { errors: string[] }) {
  if (!errors.length) return null;
  return (
    <div className="mt-4">
      <Notice kind="warning" title="Before you continue">
        {errors.join(' ')}
      </Notice>
    </div>
  );
}

function CaseReview({ c }: { c: ReviewCase }) {
  const ctx = useReviewCtx();
  const loc = useLocation();
  const params = useParams();
  const cid = ctx.campaign.campaign_id;
  const a = ctx.assignmentOf(c.case_id) ?? { case_id: c.case_id, reason: 'self_selected' as const, match: 'outside' as const, optional: true };
  const [s, dispatch] = useReducer(flowReducerV2, null, () => {
    const opts = { reason: a.reason, match: a.match, review_mode: effectiveReviewMode(ctx.storage, ctx.profile), notes: ctx.storage.loadNotes(cid, c.case_id) };
    return restoreFlowStateV2(ctx.storage.loadCaseDraft(cid, c.case_id), c, opts) ?? initialFlowStateV2(c, opts);
  });
  const [tried, setTried] = useState(false);
  const [sending, setSending] = useState(false);
  const [result, setResult] = useState<{ ok: boolean; message: string } | null>(null);
  const [sendError, setSendError] = useState<string | null>(null);

  useEffect(() => dispatch({ type: 'start', at: now() }), []);
  // autosave on every change: the flow state, and the notes on their own key (they survive a discarded draft)
  useEffect(() => {
    ctx.storage.saveCaseDraftV2(cid, c.case_id, s);
  }, [s, cid, c.case_id, ctx.storage]);
  useEffect(() => {
    ctx.storage.saveNotes(cid, c.case_id, s.notes);
  }, [s.notes, cid, c.case_id, ctx.storage]);
  // the head's progress follows the stored answer and the submission
  const revealed = canRevealV2(s);
  const finished = s.step === 'done';
  const { refresh } = ctx;
  useEffect(() => refresh(), [revealed, finished, refresh]);
  useEffect(() => setTried(false), [s.step]);

  const brief = c.snapshot.brief ?? null;
  const prior = useMemo(() => priorOfCase(c, s), [c, s]);
  const steps = stepsForV2(s);
  const lastBeforeDone = steps[steps.length - 2];
  const conclusionLabel = c.snapshot.conclusion.label;
  const campaignParam = new URLSearchParams(ctx.carry).get('campaign');

  const submit = async () => {
    const at = now();
    if (stepErrorsV2(s).length) {
      setTried(true);
      return;
    }
    const fin = flowReducerV2(s, { type: 'next', at });
    if (fin.step !== 'done' || !ctx.profile) {
      setTried(true);
      return;
    }
    let subs: Submission[];
    try {
      // the prior is compared with the conclusion's value (for a sector chain, the lead stage's key)
      const payload = toSubmissionPayloadV2(fin, { case_id: c.case_id, snapshot_sha256: c.snapshot_sha256, kind: c.kind, snapshot: { conclusion: c.snapshot.conclusion } });
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

  const advance = () => (stepErrorsV2(s).length ? setTried(true) : dispatch({ type: 'next', at: now() }));
  const nextButton = (label = 'Continue') =>
    s.step === lastBeforeDone ? (
      <button type="button" className="btn btn-primary" disabled={sending} onClick={() => void submit()}>
        Submit your review
      </button>
    ) : (
      <button type="button" className="btn btn-primary" onClick={advance}>
        {label}
      </button>
    );
  const back = (label = 'Back') => (
    <button type="button" className="btn btn-ghost" onClick={() => dispatch({ type: 'back' })}>
      {label}
    </button>
  );
  const errors = tried ? stepErrorsV2(s) : [];
  const nextCase = ctx.assignments.find((x) => x.case_id !== c.case_id && ctx.statusOf(x.case_id) !== 'submitted') ?? null;

  return (
    <article aria-labelledby="case-h" data-review-mode={s.review_mode}>
      <div className="mb-2 flex flex-wrap items-center gap-2 text-xs">
        <span className="eyebrow">{CASE_KIND_WORDS[c.kind] ?? c.kind}</span>
        <Tag>{REASON_LABEL[a.reason]}</Tag>
        <Tag>{MATCH_LABEL[a.match]}</Tag>
        {c.anchor && a.reason !== 'anchor' && <Tag>Anchor case</Tag>}
        <Tag title={s.review_mode === 'technical' ? 'The detailed analysis opens by default after the reveal' : 'The detailed analysis stays folded after the reveal; open it when you want it'}>
          {s.review_mode === 'technical' ? 'Technical review' : 'Decision review'}
        </Tag>
      </div>
      <h2 id="case-h" className="text-2xl font-medium text-paper">
        {c.title}
      </h2>
      <p className="mb-4 mt-1 text-sm text-paper-mute" data-testid={brief ? 'brief-identity' : undefined}>
        {brief ? brief.identity.title : caseKeyText(c)}
      </p>
      <StepNav s={s} dispatch={dispatch} />

      {a.match !== 'in' && s.step !== 'done' && (
        <div className="mb-5">
          <Notice kind="info" title={a.match === 'outside' ? 'Outside your stated expertise' : 'Close to your stated expertise'}>
            Answer as far as you can judge; &ldquo;{REVIEWER_STATE_LABEL.insufficient_evidence}&rdquo; and &ldquo;{REVIEWER_STATE_LABEL.outside_expertise}&rdquo; are useful answers here, and your answers are weighted by the expertise you stated.
          </Notice>
        </div>
      )}

      {s.step !== 'done' && <NotesPanel s={s} dispatch={dispatch} newId={newNoteId} now={now} />}

      {s.step === 'brief' && (
        <section aria-label="The case" data-testid="step-context">
          {brief ? <CaseBrief brief={brief} campaign={campaignParam} showIdentity={false} /> : <LegacyContext c={c} />}
          <p className="mt-4 max-w-3xl text-xs text-paper-mute">You will see the Explorer&rsquo;s conclusion and its reasoning after you give your own assessment.</p>
          <Buttons>
            <button type="button" className="btn btn-primary" onClick={() => dispatch({ type: 'next' })}>
              Give your assessment
            </button>
          </Buttons>
        </section>
      )}

      {s.step === 'blind' && (
        <section aria-labelledby="blind-h" data-testid="step-blind">
          <h3 id="blind-h" className="mb-1 text-lg font-medium text-paper">
            Your assessment
          </h3>
          <p className="mb-4 max-w-3xl text-sm text-paper-dim">{brief?.decision_question ?? c.question}</p>
          <BlindQuestion kind={c.kind} product={s.product} options={c.answer_options} blind={s.blind} onPatch={(patch) => dispatch({ type: 'set_blind', patch })} tried={tried} />
          <Errors errors={errors} />
          <Buttons>
            {back('Back to the case')}
            <button
              type="button"
              className="btn btn-primary"
              onClick={() => {
                if (stepErrorsV2(s).length) setTried(true);
                else dispatch({ type: 'submit_blind', at: now() });
              }}
            >
              Lock my answer and see the Explorer&rsquo;s reasoning
            </button>
            <span className="text-xs text-paper-mute">Your answer cannot be changed after this.</span>
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'reveal' && (
        <section aria-label="The Explorer’s reasoning" data-testid="step-reveal">
          <Later>
            <ReasoningReveal c={c} yourAnswer={yourAnswerText(c, s, prior)} mode={s.review_mode} campaign={campaignParam} />
          </Later>
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
          <Later>
            <VerdictStep c={c} s={s} dispatch={dispatch} prior={prior} tried={tried} />
          </Later>
          <Errors errors={errors} />
          <Buttons>
            {back()}
            {nextButton()}
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'red_team' && (
        <section aria-labelledby="red-team-h" data-testid="step-red-team">
          <h3 id="red-team-h" className="mb-3 text-lg font-medium text-paper">
            Test the conclusion
          </h3>
          <Later>
            <RedTeamStep s={s} dispatch={dispatch} conclusion={conclusionLabel} />
          </Later>
          <Buttons>
            {back()}
            {nextButton()}
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'product' && (
        <section aria-labelledby="product-h" data-testid="step-product">
          <h3 id="product-h" className="mb-3 text-lg font-medium text-paper">
            {PRODUCT_STEP_LABEL[s.product]}
          </h3>
          <Later>
            <ProductQuestions s={s} dispatch={dispatch} tried={tried} />
          </Later>
          <Errors errors={errors} />
          <Buttons>
            {back()}
            {nextButton()}
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'extras' && (
        <section className="space-y-8" data-testid="step-extras">
          <Later>
            {s.has_decision_use && <DecisionUseStep value={s.decision_use} onChange={(patch) => dispatch({ type: 'set_decision_use', patch })} />}
            {s.has_threat && <ThreatStep threat={c.snapshot.threat} value={s.threat} onChange={(patch) => dispatch({ type: 'set_threat', patch })} />}
          </Later>
          <Buttons>
            {back()}
            {nextButton()}
          </Buttons>
        </section>
      )}

      {revealed && s.step === 'wrap' && (
        <section aria-labelledby="wrap-h" data-testid="step-wrap">
          <h3 id="wrap-h" className="mb-3 text-lg font-medium text-paper">
            Your notes
          </h3>
          <NotesWrap s={s} dispatch={dispatch} target={c.explorer_path} displayedResult={conclusionLabel} campaignId={cid} />
          <Buttons>
            {back()}
            {nextButton()}
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
            <Link className={nextCase ? 'btn btn-ghost' : 'btn btn-primary'} to={withQuery(REVIEW_PATH.summary, ctx.carry)}>
              See your review summary
            </Link>
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
  const loc = useLocation();
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
  // the short calibration screen comes before the first case (a submitted case opens directly), with the way back
  const seen = ctx.calibrationSeen || ctx.storage.loadCalibrationSeen() !== null;
  if (!seen && ctx.statusOf(c.case_id) !== 'submitted') return <Navigate replace to={withQuery(REVIEW_PATH.calibration, ctx.carry, { next: loc.pathname + loc.search })} />;
  return <CaseReview key={c.case_id} c={c} />;
}

