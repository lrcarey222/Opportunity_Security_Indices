// The expert review layout (docs/QC_SYSTEM_DESIGN.md 5): loads data/review/manifest.json and the campaign's
// cases file (review/data/store.ts), reads the reviewer's profile, drafts and submissions from this browser
// (review/storage.ts), and gives every review view one context (useReviewCtx): the campaign, its cases, the
// profile, the assignments (review/assign.ts) and the progress of each case. The page head "Expert review"
// carries a step and progress indicator and the links between the review modules (Cases, Methodology,
// Framework, Break the model; the admin dashboard is deliberately not linked). Drafts autosave from the views
// on every change; the head offers to resume an unfinished case. The campaign is ?campaign=<id> when given,
// else the first active one, else the first listed. A build without review data says so with the command
// that builds it. Nothing here sends anything anywhere.

import { createContext, useCallback, useContext, useEffect, useMemo, useReducer, useState, type ReactNode } from 'react';
import { Link, NavLink, Outlet, useLocation, useSearchParams } from 'react-router';
import { Loading, Notice, ProgressBar } from '../components/ui.tsx';
import { useTitle } from '../shell/title.ts';
import { assignCases } from './assign.ts';
import { campaignOf, pickCampaign, reviewStore, type ReviewCampaignEntry, type ReviewLoadError, type ReviewManifestDoc } from './data/store.ts';
import { FLOW_STEPS, type CaseFlowState } from './flow.ts';
import type { Assignment, Campaign, CasesFile, ReviewCase, ReviewerProfile, Submission } from './model.ts';
import { reviewStorage, type ReviewStorage } from './storage.ts';
import { profileMatchesCase } from './tags.ts';

export const PAGE = 'mx-auto w-full max-w-5xl px-4 py-6 sm:px-6 lg:px-8 lg:py-8';
export const MODULE = 'review';

/** Draft keys of the reviewer app (storage.ts namespaces them under the campaign). */
export const DRAFT_KIND = { case: 'case', meta: 'meta', profile: 'profile' } as const;
/** The profile form's draft lives outside any campaign. */
export const PROFILE_DRAFT_REF = ['profile', DRAFT_KIND.profile, 'form'] as const;
export const SELF_SELECTED_ID = 'self_selected';

export type CaseStatus = 'not_started' | 'in_progress' | 'submitted';
export const CASE_STATUS_LABEL: Readonly<Record<CaseStatus, string>> = { not_started: 'Not started', in_progress: 'In progress', submitted: 'Submitted' };

/** Where a stored case draft stands: in progress once the reviewer has done anything beyond opening it. */
export function draftStatus(d: Partial<CaseFlowState> | null): CaseStatus {
  if (!d || typeof d !== 'object') return 'not_started';
  const step = FLOW_STEPS.includes(d.step as never) ? d.step : 'context';
  if (step === 'context' && !d.blind?.choice && !(d.blind?.factors ?? []).length) return 'not_started';
  return 'in_progress';
}

/** The case ids a list of submissions has a case review for. */
export function submittedCaseIds(subs: readonly Submission[]): Set<string> {
  const out = new Set<string>();
  for (const s of subs) if (s.kind === 'case_review') out.add((s.payload as { case_id: string }).case_id);
  return out;
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
  /** Re-read submissions and drafts (after a submit or a draft change that the head shows). */
  refresh: () => void;
  /** The reviewer's assignments (empty without a profile). */
  assignments: Assignment[];
  /** The assignment of a case, or a self-selected one with the honest expertise match. */
  assignmentOf: (caseId: string) => Assignment | null;
  selfSelected: string | null;
  setSelfSelected: (caseId: string | null) => void;
  statusOf: (caseId: string) => CaseStatus;
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

/** The links between the review modules (the admin dashboard is not one of them). */
export const REVIEW_LINKS: readonly { to: string; label: string; end?: boolean }[] = [
  { to: '/review', label: 'Overview', end: true },
  { to: '/review/profile', label: 'Your profile' },
  { to: '/review/cases', label: 'Cases' },
  { to: '/review/methods', label: 'Methodology' },
  { to: '/review/framework', label: 'Framework' },
  { to: '/review/break', label: 'Break the model' },
];

function Head({ ctx, children }: { ctx: Pick<ReviewCtx, 'carry'> | null; children?: ReactNode }) {
  const carry = ctx?.carry ?? '';
  return (
    <header className="mb-6">
      <div className="eyebrow mb-3">Allied Industrial Base Explorer</div>
      <h1 className="rule-neis text-3xl font-medium text-paper lg:text-4xl">Expert review</h1>
      <nav aria-label="Expert review pages" className="mt-4 flex flex-wrap gap-x-5 gap-y-1 border-b hairline pb-2">
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
      {children}
    </header>
  );
}

/** The step and progress indicator: profile, assigned cases, methodology and framework answers. */
function Progress({ ctx }: { ctx: ReviewCtx }) {
  const required = ctx.assignments.filter((a) => !a.optional);
  const done = required.filter((a) => ctx.statusOf(a.case_id) === 'submitted').length;
  const methods = new Set(ctx.submissions.filter((s) => s.kind === 'method_feedback').map((s) => (s.payload as { card_id: string }).card_id)).size;
  const framework = new Set(ctx.submissions.filter((s) => s.kind === 'framework_answer').map((s) => (s.payload as { scenario_id: string }).scenario_id)).size;
  const steps: { label: string; state: string; done: boolean }[] = [
    { label: '1. Your profile', state: ctx.profile ? 'saved' : 'to do', done: !!ctx.profile },
    { label: '2. Your cases', state: ctx.profile ? `${done} of ${required.length} submitted` : 'after the profile', done: required.length > 0 && done === required.length },
    { label: '3. Methodology and framework', state: `${methods} cards, ${framework} scenarios answered`, done: false },
  ];
  return (
    <div className="mt-4 space-y-3" data-testid="review-progress">
      <ol className="flex flex-wrap gap-2 text-xs" aria-label="Your progress">
        {steps.map((s) => (
          <li key={s.label} className="panel px-3 py-1.5">
            <span className={s.done ? 'text-paper' : 'text-paper-dim'}>{s.label}</span>
            <span className="text-paper-mute">: {s.state}</span>
          </li>
        ))}
      </ol>
      {ctx.profile && required.length > 0 && <ProgressBar value={done} max={required.length} label="Assigned cases submitted" />}
    </div>
  );
}

/** The unfinished case to resume (the first in-progress assigned case, else any in-progress draft). */
function ResumeLink({ ctx }: { ctx: ReviewCtx }) {
  const loc = useLocation();
  const inProgress = ctx.cases.filter((c) => ctx.statusOf(c.case_id) === 'in_progress');
  const first = ctx.assignments.map((a) => inProgress.find((c) => c.case_id === a.case_id)).find(Boolean) ?? inProgress[0] ?? null;
  if (!first || loc.pathname.startsWith(`/review/case/${first.case_id}`)) return null;
  return (
    <p className="mt-3 text-sm text-paper-dim">
      Unfinished:{' '}
      <Link className="link" to={`/review/case/${first.case_id}${ctx.carry}`}>
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
    const submitted = submittedCaseIds(submissions);
    const selfRaw = storage.loadDraft<string>(cid, DRAFT_KIND.meta, SELF_SELECTED_ID);
    const selfSelected = typeof selfRaw === 'string' && byId.has(selfRaw) ? selfRaw : null;
    const assignments = profile ? assignCases(profile, campaign, cases, { selfSelected }) : [];
    const statusOf = (id: string): CaseStatus => (submitted.has(id) ? 'submitted' : draftStatus(storage.loadDraft<CaseFlowState>(cid, DRAFT_KIND.case, id)));
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
