// The expert review layout (docs/QC_SYSTEM_DESIGN.md 5; UX v2, docs/QC_UX_REVIEW.md): loads
// data/review/manifest.json and the campaign's cases file (review/data/store.ts), reads the reviewer's profile,
// drafts and submissions from this browser (review/storage.ts), and gives every review view one context
// (useReviewCtx): the campaign, its cases, the profile, the assignments (review/assign.ts), the progress of each
// case (v2 drafts, v1 drafts still read), the review mode and whether the calibration screen was seen. The page
// head "Expert review" carries the reviewer's journey (profile, the one-minute calibration, the assigned cases)
// and the page links: the reviewer's own pages first; the optional deeper work (methodology, a case we got
// wrong, the framework) demoted to a quieter second row (the routes stay reachable; the admin dashboard is
// deliberately not linked). Drafts autosave from the views on every change; the head offers to resume an
// unfinished case. The campaign is ?campaign=<id> when given, else the first active one, else the first listed.
// A build without review data says so with the command that builds it. Nothing here sends anything anywhere.

import { createContext, useCallback, useContext, useEffect, useMemo, useReducer, useState, type ReactNode } from 'react';
import { Link, NavLink, Outlet, useLocation, useSearchParams } from 'react-router';
import { Loading, Notice, ProgressBar } from '../components/ui.tsx';
import { useTitle } from '../shell/title.ts';
import { assignCases } from './assign.ts';
import { campaignOf, pickCampaign, reviewStore, type ReviewCampaignEntry, type ReviewLoadError, type ReviewManifestDoc } from './data/store.ts';
import { FLOW_STEPS, FLOW_STEPS_V2, productOf, type CaseFlowState, type CaseFlowStateV2 } from './flow.ts';
import type { Assignment, Campaign, CasesFile, ReviewCase, ReviewerProfile, ReviewMode, Submission } from './model.ts';
import { ANY_CAMPAIGN, effectiveReviewMode, reviewStorage, type ReviewStorage } from './storage.ts';
import { profileMatchesCase } from './tags.ts';

export const PAGE = 'mx-auto w-full max-w-5xl px-4 py-6 sm:px-6 lg:px-8 lg:py-8';
export const MODULE = 'review';

/** Draft keys of the reviewer app (storage.ts namespaces them under the campaign). */
export const DRAFT_KIND = { case: 'case', meta: 'meta', profile: 'profile' } as const;
/** The profile form's draft lives outside any campaign. */
export const PROFILE_DRAFT_REF = ['profile', DRAFT_KIND.profile, 'form'] as const;
export const SELF_SELECTED_ID = 'self_selected';

/** The journey's own pages. */
export const REVIEW_PATH = {
  home: '/review',
  profile: '/review/profile',
  cases: '/review/cases',
  calibration: '/review/start',
  summary: '/review/summary',
  methods: '/review/methods',
  framework: '/review/framework',
  break: '/review/break',
} as const;
export const casePath = (caseId: string): string => `/review/case/${encodeURIComponent(caseId)}`;

export type CaseStatus = 'not_started' | 'in_progress' | 'submitted';
export const CASE_STATUS_LABEL: Readonly<Record<CaseStatus, string>> = { not_started: 'Not started', in_progress: 'In progress', submitted: 'Submitted' };

/**
 * Where a stored case draft stands: in progress once the reviewer has done anything beyond opening it. Reads a v2
 * draft (storage.ts loadCaseDraft, kind 'case_v2') and a v1 draft (kind 'case') alike; anything else is not started.
 */
export function draftStatus(d: Partial<CaseFlowState> | Partial<CaseFlowStateV2> | unknown): CaseStatus {
  if (!d || typeof d !== 'object') return 'not_started';
  if ((d as { version?: unknown }).version === 2) {
    const v2 = d as Partial<CaseFlowStateV2>;
    const step = FLOW_STEPS_V2.includes(v2.step as never) ? v2.step : 'brief';
    const reached = FLOW_STEPS_V2.includes(v2.reached as never) ? v2.reached : step;
    const b = v2.blind;
    const touched = !!b?.choice || (Array.isArray(b?.choices) && b.choices.length > 0) || !!b?.confidence_level || !!b?.driver || (Array.isArray(v2.notes) && v2.notes.length > 0);
    return step === 'brief' && reached === 'brief' && !touched ? 'not_started' : 'in_progress';
  }
  const v1 = d as Partial<CaseFlowState>;
  const step = FLOW_STEPS.includes(v1.step as never) ? v1.step : 'context';
  if (step === 'context' && !v1.blind?.choice && !(v1.blind?.factors ?? []).length) return 'not_started';
  return 'in_progress';
}

/** The case ids a list of submissions has a case review for. */
export function submittedCaseIds(subs: readonly Submission[]): Set<string> {
  const out = new Set<string>();
  for (const s of subs) if (s.kind === 'case_review') out.add((s.payload as { case_id: string }).case_id);
  return out;
}

/** A path with the campaign carried (`carry` is '' or '?campaign=...') and extra query parameters. */
export function withQuery(path: string, carry: string, extra: Record<string, string> = {}): string {
  const q = new URLSearchParams(carry.startsWith('?') ? carry.slice(1) : carry);
  for (const [k, v] of Object.entries(extra)) q.set(k, v);
  const s = q.toString();
  return s ? `${path}?${s}` : path;
}

/**
 * A return path from a query parameter, only when it is a review page of this site (no other origin, no
 * protocol-relative URL, not the calibration screen itself); else null.
 */
export function safeReturnPath(raw: string | null | undefined): string | null {
  if (typeof raw !== 'string' || !raw.startsWith('/review') || raw.startsWith('//')) return null;
  try {
    const u = new URL(raw, 'https://review.invalid');
    if (u.origin !== 'https://review.invalid') return null;
    if (u.pathname !== '/review' && !u.pathname.startsWith('/review/')) return null;
    if (u.pathname === REVIEW_PATH.calibration) return null;
    return `${u.pathname}${u.search}`;
  } catch {
    return null;
  }
}

/** Minutes a case takes, roughly: the brief, the blind answer and the reveal; more for the extra question sets. */
export function caseMinutes(c: Pick<ReviewCase, 'kind' | 'decision_use' | 'threat'>): number {
  let m = 6;
  if (productOf(c.kind) === 'partnership') m += 2;
  if (c.decision_use) m += 2;
  if (c.threat) m += 2;
  return m;
}

/** The time a set of cases takes, rounded to five minutes (at least five; zero for no case). */
export function estimateMinutes(cases: readonly Pick<ReviewCase, 'kind' | 'decision_use' | 'threat'>[]): number {
  if (!cases.length) return 0;
  const total = cases.reduce((n, c) => n + caseMinutes(c), 0);
  return Math.max(5, Math.round(total / 5) * 5);
}

export interface ReviewCtx {
  manifest: ReviewManifestDoc;
  entry: ReviewCampaignEntry;
  campaign: Campaign;
  /** Campaign rules the build did not carry (store.ts FALLBACK_*); shown on the home page. */
  fallbackRules: string[];
  casesFile: CasesFile;
  cases: ReviewCase[];
  caseById: (id: string | null | undefined) => ReviewCase | null;
  storage: ReviewStorage;
  profile: ReviewerProfile | null;
  saveProfile: (p: ReviewerProfile) => boolean;
  /** This campaign's submissions in this browser (re-read after refresh()). */
  submissions: Submission[];
  /** The current reviewer's own submissions in this campaign (empty without a profile). */
  mySubmissions: Submission[];
  /** Re-read submissions and drafts (after a submit or a draft change that the head shows). */
  refresh: () => void;
  /** The reviewer's assignments (empty without a profile). */
  assignments: Assignment[];
  /** The assignment of a case, or a self-selected one with the honest expertise match. */
  assignmentOf: (caseId: string) => Assignment | null;
  selfSelected: string | null;
  setSelfSelected: (caseId: string | null) => void;
  statusOf: (caseId: string) => CaseStatus;
  /** The next assigned (required) case to work on: an unfinished one first, else the first not started; null when none is open. */
  nextCase: ReviewCase | null;
  /** Every required assigned case is submitted (false when there are none). */
  allAssignedDone: boolean;
  /** Decision review (default) or technical review (storage.ts effectiveReviewMode). */
  reviewMode: ReviewMode;
  setReviewMode: (m: ReviewMode) => boolean;
  /** Whether the reviewer has read the calibration screen for this campaign. */
  calibrationSeen: boolean;
  markCalibrationSeen: () => void;
  /** The link to open a case: through the calibration screen first when it has not been seen (a submitted case opens directly). */
  caseHref: (caseId: string) => string;
  /** The link suffix that keeps the campaign (?campaign=...) when the URL named one. */
  carry: string;
}

const Ctx = createContext<ReviewCtx | null>(null);

export function useReviewCtx(): ReviewCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useReviewCtx outside ReviewLayout');
  return v;
}

/** The layout's context when the view is rendered inside ReviewLayout, else null (a view on its own, a test). */
export function useOptionalReviewCtx(): ReviewCtx | null {
  return useContext(Ctx);
}

/** What the home page's one primary button does. */
export type JourneyState = 'no_profile' | 'no_cases' | 'calibration' | 'resume' | 'next' | 'all_done';
export interface PrimaryAction {
  state: JourneyState;
  to: string;
  label: string;
  caseId: string | null;
}
export const PRIMARY_LABEL = 'Review your assigned cases';
export const SUMMARY_LABEL = 'See your summary';

/** The primary action of the review home: the profile, the calibration, the next open case, or the summary when every assigned case is in. */
export function primaryAction(ctx: Pick<ReviewCtx, 'profile' | 'statusOf' | 'nextCase' | 'allAssignedDone' | 'calibrationSeen' | 'carry'>): PrimaryAction {
  if (!ctx.profile) return { state: 'no_profile', to: withQuery(REVIEW_PATH.profile, ctx.carry), label: PRIMARY_LABEL, caseId: null };
  if (ctx.allAssignedDone) return { state: 'all_done', to: withQuery(REVIEW_PATH.summary, ctx.carry), label: SUMMARY_LABEL, caseId: null };
  const next = ctx.nextCase;
  if (!next) return { state: 'no_cases', to: withQuery(REVIEW_PATH.cases, ctx.carry), label: PRIMARY_LABEL, caseId: null };
  const target = withQuery(casePath(next.case_id), ctx.carry);
  if (!ctx.calibrationSeen) return { state: 'calibration', to: withQuery(REVIEW_PATH.calibration, ctx.carry, { next: target }), label: PRIMARY_LABEL, caseId: next.case_id };
  return { state: ctx.statusOf(next.case_id) === 'in_progress' ? 'resume' : 'next', to: target, label: PRIMARY_LABEL, caseId: next.case_id };
}

interface Loaded {
  manifest: ReviewManifestDoc;
  entry: ReviewCampaignEntry;
  casesFile: CasesFile;
}

type LoadState = { status: 'loading' } | { status: 'ready'; data: Loaded } | { status: 'error'; error: ReviewLoadError } | { status: 'empty'; manifest: ReviewManifestDoc };

function useReviewData(wanted: string | null): { state: LoadState; retry: () => void } {
  const [state, setState] = useState<LoadState>({ status: 'loading' });
  const [attempt, setAttempt] = useState(0);
  useEffect(() => {
    let live = true;
    const store = reviewStore();
    setState({ status: 'loading' });
    store
      .manifest()
      .then(async (manifest) => {
        const entry = pickCampaign(manifest, wanted);
        if (!entry) {
          if (live) setState({ status: 'empty', manifest });
          return;
        }
        const casesFile = await store.cases(entry);
        if (live) setState({ status: 'ready', data: { manifest, entry, casesFile } });
      })
      .catch((e: ReviewLoadError) => {
        if (live) setState({ status: 'error', error: e });
      });
    return () => {
      live = false;
    };
  }, [wanted, attempt]);
  return { state, retry: () => setAttempt((n) => n + 1) };
}

function ModuleTitle() {
  useTitle('', MODULE);
  return null;
}

/** The reviewer's own pages (the admin dashboard is not one of them). */
export const REVIEW_LINKS: readonly { to: string; label: string; end?: boolean }[] = [
  { to: REVIEW_PATH.home, label: 'Overview', end: true },
  { to: REVIEW_PATH.profile, label: 'Your profile' },
  { to: REVIEW_PATH.cases, label: 'Your cases' },
];

/** The optional deeper work, demoted to a second row (same names as the home page's "Want to go deeper?"). */
export const REVIEW_DEEPER_LINKS: readonly { to: string; label: string; blurb: string }[] = [
  { to: REVIEW_PATH.methods, label: 'Challenge the methodology', blurb: 'Read how each measure is built and say where a choice is weak.' },
  { to: REVIEW_PATH.break, label: 'Find a case we got wrong', blurb: 'Point to a country, sector or partnership where the Explorer misleads.' },
  { to: REVIEW_PATH.framework, label: 'Review the framework', blurb: 'Test whether the strategic choices are clear and distinct.' },
];

function Head({ ctx, children }: { ctx: Pick<ReviewCtx, 'carry'> | null; children?: ReactNode }) {
  const carry = ctx?.carry ?? '';
  return (
    <header className="mb-6">
      <div className="eyebrow mb-3">Allied Industrial Base Explorer</div>
      <h1 className="rule-neis text-3xl font-medium text-paper lg:text-4xl">Expert review</h1>
      <div className="mt-4 flex flex-wrap items-end justify-between gap-x-6 gap-y-1 border-b hairline pb-2">
        <nav aria-label="Expert review pages" className="flex flex-wrap gap-x-5 gap-y-1">
          {REVIEW_LINKS.map((l) => (
            <NavLink
              key={l.to}
              to={l.to + carry}
              end={l.end}
              className={({ isActive }) =>
                `border-b-2 py-1 text-sm font-medium uppercase tracking-wider transition-colors ${isActive ? 'border-signal text-paper' : 'border-transparent text-white/80 hover:text-paper'}`
              }
            >
              {l.label}
            </NavLink>
          ))}
        </nav>
        <nav aria-label="Optional review pages" className="flex flex-wrap items-baseline gap-x-4 gap-y-1 text-xs">
          <span className="text-paper-mute">Optional:</span>
          {REVIEW_DEEPER_LINKS.map((l) => (
            <NavLink key={l.to} to={l.to + carry} className={({ isActive }) => (isActive ? 'text-paper underline' : 'text-paper-mute hover:text-paper')}>
              {l.label}
            </NavLink>
          ))}
        </nav>
      </div>
      {children}
    </header>
  );
}

/** The journey indicator: profile, the calibration screen, the assigned cases. The optional work is not counted. */
function Progress({ ctx }: { ctx: ReviewCtx }) {
  const required = ctx.assignments.filter((a) => !a.optional);
  const done = required.filter((a) => ctx.statusOf(a.case_id) === 'submitted').length;
  const steps: { label: string; state: string; done: boolean }[] = [
    { label: '1. Your profile', state: ctx.profile ? 'saved' : 'to do', done: !!ctx.profile },
    { label: '2. Before you start', state: ctx.calibrationSeen ? 'read' : 'one minute', done: ctx.calibrationSeen },
    { label: '3. Your cases', state: ctx.profile ? `${done} of ${required.length} submitted` : 'after the profile', done: required.length > 0 && done === required.length },
  ];
  return (
    <div className="mt-4 space-y-3" data-testid="review-progress">
      <ol className="flex flex-wrap gap-2 text-xs" aria-label="Your progress">
        {steps.map((s) => (
          <li key={s.label} className="panel px-3 py-1.5" data-done={s.done || undefined}>
            <span className={s.done ? 'text-paper' : 'text-paper-dim'}>{s.label}</span>
            <span className="text-paper-mute">: {s.state}</span>
          </li>
        ))}
      </ol>
      {ctx.profile && required.length > 0 && <ProgressBar value={done} max={required.length} label="Assigned cases submitted" />}
    </div>
  );
}

/** The unfinished case to resume (the first in-progress assigned case, else any in-progress draft). The home page shows its own. */
function ResumeLink({ ctx }: { ctx: ReviewCtx }) {
  const loc = useLocation();
  const inProgress = ctx.cases.filter((c) => ctx.statusOf(c.case_id) === 'in_progress');
  const first = ctx.assignments.map((a) => inProgress.find((c) => c.case_id === a.case_id)).find(Boolean) ?? inProgress[0] ?? null;
  const path = loc.pathname.replace(/\/+$/, '') || '/';
  if (!first || path === REVIEW_PATH.home || path === REVIEW_PATH.calibration || loc.pathname.startsWith(casePath(first.case_id))) return null;
  return (
    <p className="mt-3 text-sm text-paper-dim">
      Unfinished:{' '}
      <Link className="link" to={ctx.caseHref(first.case_id)}>
        resume {first.title}
      </Link>{' '}
      (your answers so far are saved in this browser).
    </p>
  );
}

function ErrorScreen({ error, retry }: { error: ReviewLoadError; retry: () => void }) {
  if (error.code === 'NO_DATA')
    return (
      <Notice kind="info" title="No review data in this build">
        <p>{error.message}</p>
        <p className="mt-2 text-paper-mute">
          Developers: run <code>node --experimental-strip-types scripts/review/build-review-data.mjs</code> from <code>dashboard/</code>. The rest of the Explorer is unaffected.
        </p>
      </Notice>
    );
  return (
    <Notice
      kind="error"
      title="The review data could not be loaded"
      action={
        <button type="button" className="btn btn-ghost btn-sm" onClick={retry}>
          Retry
        </button>
      }
    >
      {error.message}
    </Notice>
  );
}

export default function ReviewLayout() {
  const [params] = useSearchParams();
  const wanted = params.get('campaign');
  const { state, retry } = useReviewData(wanted);
  const storage = reviewStorage();
  const [version, bump] = useReducer((n: number) => n + 1, 0);
  const [profile, setProfile] = useState<ReviewerProfile | null>(() => storage.loadProfile());
  const carry = wanted ? `?campaign=${encodeURIComponent(wanted)}` : '';
  const refresh = useCallback(() => bump(), []);

  const data = state.status === 'ready' ? state.data : null;
  const ctx = useMemo<ReviewCtx | null>(() => {
    if (!data) return null;
    void version;
    const { campaign, fallback } = campaignOf(data.entry, data.casesFile);
    const cases = data.casesFile.cases;
    const byId = new Map(cases.map((c) => [c.case_id, c]));
    const cid = campaign.campaign_id;
    const submissions = storage.listSubmissions(cid);
    const mySubmissions = profile ? submissions.filter((s) => s.reviewer.reviewer_id === profile.reviewer_id) : [];
    const submitted = submittedCaseIds(submissions);
    const selfRaw = storage.loadDraft<string>(cid, DRAFT_KIND.meta, SELF_SELECTED_ID);
    const selfSelected = typeof selfRaw === 'string' && byId.has(selfRaw) ? selfRaw : null;
    const assignments = profile ? assignCases(profile, campaign, cases, { selfSelected }) : [];
    const statusCache = new Map<string, CaseStatus>();
    const statusOf = (id: string): CaseStatus => {
      const hit = statusCache.get(id);
      if (hit) return hit;
      let st: CaseStatus = submitted.has(id) ? 'submitted' : draftStatus(storage.loadCaseDraft(cid, id));
      if (st === 'not_started' && storage.loadNotes(cid, id).length) st = 'in_progress';
      statusCache.set(id, st);
      return st;
    };
    const required = assignments.filter((a) => !a.optional && byId.has(a.case_id));
    const open = required.filter((a) => statusOf(a.case_id) !== 'submitted');
    const nextId = (open.find((a) => statusOf(a.case_id) === 'in_progress') ?? open[0])?.case_id ?? null;
    // read for this campaign (what this layout writes) or for any campaign (storage.ts's default)
    const calibrationSeen = storage.loadCalibrationSeen(cid) !== null || storage.loadCalibrationSeen(ANY_CAMPAIGN) !== null;
    return {
      manifest: data.manifest,
      entry: data.entry,
      campaign,
      fallbackRules: fallback,
      casesFile: data.casesFile,
      cases,
      caseById: (id) => (id ? (byId.get(id) ?? null) : null),
      storage,
      profile,
      saveProfile: (p) => {
        const ok = storage.saveProfile(p);
        setProfile(p);
        return ok;
      },
      submissions,
      mySubmissions,
      refresh,
      assignments,
      assignmentOf: (id) => {
        const a = assignments.find((x) => x.case_id === id);
        if (a) return a;
        const c = byId.get(id);
        if (!c) return null;
        return { case_id: id, reason: 'self_selected', match: profile ? profileMatchesCase(profile, c, campaign.assignment.min_domain_confidence) : 'outside', optional: true };
      },
      selfSelected,
      setSelfSelected: (id) => {
        if (id) storage.saveDraft(cid, DRAFT_KIND.meta, SELF_SELECTED_ID, id);
        else storage.deleteDraft(cid, DRAFT_KIND.meta, SELF_SELECTED_ID);
        bump();
      },
      statusOf,
      nextCase: nextId ? (byId.get(nextId) ?? null) : null,
      allAssignedDone: required.length > 0 && open.length === 0,
      reviewMode: effectiveReviewMode(storage, profile),
      setReviewMode: (m) => {
        const ok = storage.saveReviewMode(m);
        bump();
        return ok;
      },
      calibrationSeen,
      markCalibrationSeen: () => {
        storage.markCalibrationSeen(new Date().toISOString(), cid);
        bump();
      },
      caseHref: (id) => {
        const target = withQuery(casePath(id), carry);
        return calibrationSeen || submitted.has(id) ? target : withQuery(REVIEW_PATH.calibration, carry, { next: target });
      },
      carry,
    };
  }, [data, version, profile, storage, refresh, carry]);

  if (state.status === 'error')
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Head ctx={null} />
        <ErrorScreen error={state.error} retry={retry} />
      </div>
    );
  if (state.status === 'empty')
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Head ctx={null} />
        <Notice kind="info" title="No review campaign">
          This build's review data lists no campaign.
        </Notice>
      </div>
    );
  if (!ctx)
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Head ctx={null} />
        <Loading label="Loading the review campaign" />
      </div>
    );
  return (
    <Ctx.Provider value={ctx}>
      <div className={PAGE}>
        <ModuleTitle />
        <Head ctx={ctx}>
          <Progress ctx={ctx} />
          <ResumeLink ctx={ctx} />
          {!storage.persistent() && (
            <div className="mt-3">
              <Notice kind="warning" title="This browser is not keeping your answers">
                Browser storage is unavailable here, so your answers last only for this visit. Download them from the review overview before you leave.
              </Notice>
            </div>
          )}
        </Head>
        <Outlet />
        <p className="mt-10 border-t hairline pt-3 text-xs text-paper-mute tabular">
          Review data release {ctx.manifest.data_release.id}
          {ctx.manifest.data_release.complete ? '' : ` (stamps missing: ${ctx.manifest.data_release.missing.join(', ')})`}, built {ctx.manifest.built_at.slice(0, 10)}. Campaign {ctx.campaign.campaign_id}.
        </p>
      </div>
    </Ctx.Provider>
  );
}
