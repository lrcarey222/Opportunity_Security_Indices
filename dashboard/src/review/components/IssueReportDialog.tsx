// The data-issue and break-the-model dialog (docs/QC_SYSTEM_DESIGN.md 4.4, 5 "Report a data issue", step 7).
// It captures the page context by itself: the router location (useLocation, never window.location, so it is
// right in the artifact's memory router) or a pasted Explorer address, the route params, the bundle stamps the
// page has published (shell/buildInfo), the data release and the app commit; and the metric the reviewer
// clicked, when a ReportIssueButton passes one. All of that is shown read-only. The reviewer writes only what
// is wrong (required), and optionally the corrected value and a source; severity is required; in break-the-
// model mode the break type too. Submits through the build's sink (review/sink.ts getSink). A visitor without a
// reviewer profile still can: an anonymous reviewer id is generated once and kept in the review store.
// Release: the page's own ID covers only the bundles it loaded, so the dialog also reads the small review manifest
// (data/review/manifest.json, review/data/store.ts) and records its release ID when every stamp the page loaded
// agrees with it (review/release.ts resolveReportRelease); otherwise the partial ID stays and page_context.
// release_note says which stamps differ. page_context.bundle_stamps always records the page's own stamps.
// role=dialog, aria-modal, Escape closes (and does not reach the cell drawer underneath), Tab stays inside.

import { useEffect, useId, useMemo, useRef, useState, type KeyboardEvent as ReactKeyboardEvent, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { useLocation, useParams } from 'react-router';
import { BREAK_TYPE_LABEL, type BreakType, type DataIssuePayload, type MetricRef, type PageContext, type ReviewerProfile, type Severity, type Submission } from '../model.ts';
import { capturePageContext, newSubmission, type LocationLike } from '../context.ts';
import { appCommit, pageRelease, releaseStampsFromBuildInfo } from '../appInfo.ts';
import { reviewStore, type ReviewStore } from '../data/store.ts';
import { resolveReportRelease, type ReleaseStamps } from '../release.ts';
import { getBuildInfo } from '../../shell/buildInfo.ts';
import { getSink, type SinkResult, type SubmissionSink } from '../sink.ts';
import { reviewStorage, type ReviewStorage } from '../storage.ts';
import { pseudonymReviewerCode } from '../privacy.ts';
import { Field, SeveritySelect, TextArea } from './form.tsx';

export type IssueMode = 'data_issue' | 'break_model';

export const BREAK_TYPES = Object.keys(BREAK_TYPE_LABEL) as BreakType[];

/** Where the anonymous reviewer id lives in the review store (a draft slot outside every campaign). */
const ANON_SLOT = ['_session', 'reviewer', 'anonymous'] as const;

/** The reviewer profile of this browser, or an anonymous one (its id generated once and stored). */
export function reviewerProfileOrAnonymous(store: ReviewStorage = reviewStorage(), now: () => string = () => new Date().toISOString()): ReviewerProfile {
  const p = store.loadProfile();
  if (p && typeof p.reviewer_id === 'string' && p.reviewer_id) return p;
  const saved = store.loadDraft<{ reviewer_id?: unknown; created_at?: unknown }>(...ANON_SLOT);
  let id = typeof saved?.reviewer_id === 'string' && saved.reviewer_id ? saved.reviewer_id : null;
  const created = typeof saved?.created_at === 'string' ? saved.created_at : now();
  if (!id) {
    const c = (globalThis as { crypto?: { randomUUID?: () => string } }).crypto;
    id = `anon-${c?.randomUUID ? c.randomUUID() : Math.random().toString(16).slice(2) + Date.now().toString(16)}`;
    store.saveDraft(...ANON_SLOT, { reviewer_id: id, created_at: created });
  }
  return { reviewer_id: id, name: null, anonymous: true, countries: [], sectors: [], stages: [], methods: [], confidence: {}, familiarity_neis: 0, familiarity_explorer: 0, advised_on: [], created_at: created };
}

/** A pasted Explorer address as a router location: a full URL (its path, or its hash route), or a path. */
export function locationOfAddress(address: string): LocationLike | null {
  const s = address.trim();
  if (!s) return null;
  try {
    const u = new URL(s, 'https://explorer.invalid');
    // a hash-routed copy (…/#/countries/KOR?node=…) keeps its route in the hash
    if (u.hash.startsWith('#/')) {
      const inner = new URL(u.hash.slice(1), 'https://explorer.invalid');
      return { pathname: inner.pathname, search: inner.search, hash: inner.hash };
    }
    return { pathname: u.pathname, search: u.search, hash: u.hash };
  } catch {
    return null;
  }
}

const FOCUSABLE = 'a[href], button:not([disabled]), textarea:not([disabled]), input:not([disabled]), select:not([disabled]), [tabindex]:not([tabindex="-1"])';

export interface IssueReportDialogProps {
  open: boolean;
  onClose: () => void;
  mode?: IssueMode;
  /** The metric the reviewer clicked (prefilled, read-only). */
  metric?: MetricRef | null;
  /** What the page shows for the thing reported ("Secure · High fence", "BUILD"). */
  displayedResult?: string | null;
  /** Report on this Explorer address instead of the current page (break-the-model from the review pages). */
  target?: string | null;
  campaignId?: string | null;
  /** Tests: the store, sink and clock (and the review data store whose manifest names the full release). */
  store?: ReviewStorage;
  reviewData?: ReviewStore;
  sink?: SubmissionSink;
  now?: () => string;
  onSubmitted?: (sub: Submission, result: SinkResult) => void;
}

/** The dialog (rendered into document.body while open). */
export function IssueReportDialog(props: IssueReportDialogProps) {
  if (!props.open) return null;
  return createPortal(<DialogBody {...props} />, document.body);
}

function DialogBody({ onClose, mode = 'data_issue', metric = null, displayedResult = null, target = null, campaignId = null, store, reviewData, sink, now, onSubmitted }: IssueReportDialogProps) {
  const routerLoc = useLocation();
  const routerParams = useParams();
  const titleId = useId();
  const ref = useRef<HTMLDivElement>(null);
  // undefined while computing, null when it could not be computed (no Web Crypto)
  const [release, setRelease] = useState<{ id: string; complete: boolean; missing: string[] } | null | undefined>(undefined);
  // the review manifest's release: undefined while loading, null when it could not be read
  const [reviewRelease, setReviewRelease] = useState<{ id: string; stamps: ReleaseStamps } | null | undefined>(undefined);
  const [whatIsWrong, setWhatIsWrong] = useState('');
  const [corrected, setCorrected] = useState('');
  const [citation, setCitation] = useState('');
  const [severity, setSeverity] = useState<Severity | null>(null);
  const [breakType, setBreakType] = useState<BreakType | ''>('');
  const [tried, setTried] = useState(false);
  const [busy, setBusy] = useState(false);
  const [result, setResult] = useState<SinkResult | null>(null);
  const [capturedAt] = useState(() => (now ?? (() => new Date().toISOString()))());

  const targetLoc = target ? locationOfAddress(target) : null;
  const loc: LocationLike = targetLoc ?? { pathname: routerLoc.pathname, search: routerLoc.search, hash: routerLoc.hash };
  const params = targetLoc ? {} : routerParams;
  const recorded = useMemo(
    () => (release === undefined || reviewRelease === undefined ? null : resolveReportRelease(release?.id ?? null, releaseStampsFromBuildInfo(getBuildInfo()), reviewRelease)),
    [release, reviewRelease],
  );
  const ctx: PageContext = useMemo(
    () => ({
      ...capturePageContext(loc, params, getBuildInfo(), {
        captured_at: capturedAt,
        app_commit: appCommit(),
        data_release: recorded?.data_release ?? release?.id ?? null,
        displayed_result: displayedResult,
        metric_ids: metric ? [metric.id] : [],
      }),
      // a report sent before the check finished keeps the page's partial ID and says so
      release_note: recorded ? recorded.release_note : 'the release check had not finished: the ID covers only the bundles this page loaded',
    }),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [loc.pathname, loc.search, loc.hash, JSON.stringify(params), capturedAt, release?.id, recorded?.data_release, recorded?.release_note, displayedResult, metric?.id],
  );

  useEffect(() => {
    let live = true;
    pageRelease().then(
      (r) => live && setRelease(r),
      () => live && setRelease(null),
    );
    (reviewData ?? reviewStore())
      .manifest()
      .then(
        (m) => live && setReviewRelease(m.data_release && typeof m.data_release.id === 'string' && m.data_release.stamps ? { id: m.data_release.id, stamps: m.data_release.stamps } : null),
        () => live && setReviewRelease(null),
      );
    return () => {
      live = false;
    };
  }, [reviewData]);

  // focus: the first field on open, back to the opener on close; Escape closes (before the drawer's own handler)
  useEffect(() => {
    const opener = document.activeElement as HTMLElement | null;
    const first = ref.current?.querySelector<HTMLElement>('textarea, input, select');
    (first ?? ref.current)?.focus();
    const onKey = (e: KeyboardEvent) => {
      if (e.key !== 'Escape') return;
      e.stopPropagation();
      e.preventDefault();
      onClose();
    };
    window.addEventListener('keydown', onKey, true);
    return () => {
      window.removeEventListener('keydown', onKey, true);
      if (opener && document.contains(opener)) opener.focus();
    };
  }, [onClose]);

  const trap = (e: ReactKeyboardEvent<HTMLDivElement>) => {
    if (e.key !== 'Tab' || !ref.current) return;
    const els = [...ref.current.querySelectorAll<HTMLElement>(FOCUSABLE)].filter((el) => !el.hasAttribute('disabled'));
    if (!els.length) return;
    const first = els[0];
    const last = els[els.length - 1];
    if (e.shiftKey && (document.activeElement === first || document.activeElement === ref.current)) {
      e.preventDefault();
      last.focus();
    } else if (!e.shiftKey && document.activeElement === last) {
      e.preventDefault();
      first.focus();
    }
  };

  const errors = {
    what: whatIsWrong.trim() ? null : 'Say what is wrong.',
    severity: severity ? null : 'Choose a severity.',
    breakType: mode === 'break_model' && !breakType ? 'Choose the kind of break.' : null,
    target: target && !targetLoc ? 'That address could not be read.' : null,
  };
  const invalid = Object.values(errors).some(Boolean);

  const submit = async () => {
    setTried(true);
    if (invalid || busy || !severity) return;
    setBusy(true);
    const s = store ?? reviewStorage();
    const profile = reviewerProfileOrAnonymous(s, now);
    const payload: DataIssuePayload = {
      metric,
      what_is_wrong: whatIsWrong.trim(),
      corrected_value: corrected.trim() || null,
      citation: citation.trim() || null,
      severity,
      ...(mode === 'break_model' && breakType ? { break_type: breakType } : {}),
    };
    const sub = newSubmission(mode, payload, {
      profile,
      campaign_id: campaignId,
      page_context: ctx,
      release: { data_release: ctx.data_release, app_commit: appCommit() },
      ...(now ? { now: now() } : {}),
    });
    const r = await (sink ?? getSink(undefined, s)).submit(sub);
    setResult(r);
    setBusy(false);
    onSubmitted?.(sub, r);
  };

  const metaRows: [string, ReactNode][] = [
    ['Metric', metric?.label ?? (displayedResult ? 'The result shown' : 'Not given (page-level report)')],
    ['Value shown', metric?.display ?? displayedResult ?? '–'],
    ['Country', metric?.country ?? ctx.country ?? '–'],
    ['Sector / technology', metric?.tech ?? ctx.tech ?? '–'],
    ['Stage', metric?.stage ?? ctx.stage ?? '–'],
    ['Source', metric?.source ?? 'not stated on this page'],
    ['Source year', metric?.source_year !== undefined && metric?.source_year !== null ? String(metric.source_year) : 'not stated on this page'],
    ['Transformation', metric?.transform ?? 'not stated on this page'],
    ['Page', <span className="break-all" key="u">{ctx.url}</span>],
    ['App version (git commit)', ctx.app_commit],
    [
      'Data release',
      release === undefined || reviewRelease === undefined
        ? 'computing…'
        : !ctx.release_note && ctx.data_release
          ? ctx.data_release
          : release
            ? `${release.id}${release.complete ? '' : ` (partial: ${release.missing.join(', ')} not loaded on this page)`}`
            : 'could not be computed in this browser',
    ],
    ['Release check', recorded === null ? 'computing…' : (ctx.release_note ?? 'the stamps this page loaded match the review release')],
  ];

  const isBreak = mode === 'break_model';
  return (
    <div className="fixed inset-0 z-[60] flex items-end justify-center bg-black/60 p-0 sm:items-center sm:p-4" data-testid="issue-dialog-backdrop">
      <div
        ref={ref}
        role="dialog"
        aria-modal="true"
        aria-labelledby={titleId}
        tabIndex={-1}
        onKeyDown={trap}
        className="panel max-h-[92vh] w-full max-w-xl overflow-y-auto bg-ink-850 p-5 outline-none sm:rounded-xl"
        data-testid="issue-dialog"
        data-mode={mode}
      >
        <div className="flex items-start justify-between gap-3">
          <div>
            <p className="eyebrow">{isBreak ? 'Break the model' : 'Report a data issue'}</p>
            <h2 id={titleId} className="rule-neis mt-1 text-lg font-medium text-paper">
              {isBreak ? 'What breaks here?' : 'What is wrong with this?'}
            </h2>
          </div>
          <button type="button" className="btn btn-ghost btn-sm" onClick={onClose} aria-label="Close the report">
            ✕
          </button>
        </div>

        {result ? (
          <div className="mt-4" role="status" data-testid="issue-result">
            <p className="text-sm text-paper">{result.ok ? 'Thank you: your report is recorded.' : 'Your report could not be sent.'}</p>
            <p className="mt-1 text-sm text-paper-dim">{result.message}</p>
            <p className="mt-1 text-xs text-paper-mute">Reference: {result.id}</p>
            <div className="mt-4">
              <button type="button" className="btn btn-primary btn-sm" onClick={onClose}>
                Close
              </button>
            </div>
          </div>
        ) : (
          <form
            className="mt-4"
            noValidate
            onSubmit={(e) => {
              e.preventDefault();
              void submit();
            }}
          >
            <details className="disclosure mb-4 border-t hairline" data-testid="issue-captured">
              <summary className="flex items-center gap-2 py-2 text-sm text-paper-dim">
                <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
                  ›
                </span>
                <span className="flex-1">
                  Captured automatically: {[ctx.country, metric?.tech ?? ctx.tech, ctx.stage].filter(Boolean).join(' · ') || ctx.route}
                </span>
              </summary>
              <dl className="pb-2 pl-4" data-testid="issue-metadata">
                {metaRows.map(([k, v]) => (
                  <div key={k} className="flex justify-between gap-3 py-0.5 text-xs">
                    <dt className="text-paper-mute">{k}</dt>
                    <dd className="text-right text-paper-dim" data-field={k}>
                      {v}
                    </dd>
                  </div>
                ))}
              </dl>
            </details>
            {errors.target && tried && (
              <p role="alert" className="mb-3 text-xs text-alert">
                {errors.target}
              </p>
            )}
            {isBreak && (
              <Field label="What kind of break is it?" required error={tried ? errors.breakType : null}>
                {({ id, describedBy, invalid: bad }) => (
                  <select id={id} className="field" value={breakType} aria-describedby={describedBy} aria-invalid={bad || undefined} onChange={(e) => setBreakType(e.target.value as BreakType)}>
                    <option value="" disabled>
                      Choose a kind of break
                    </option>
                    {BREAK_TYPES.map((b) => (
                      <option key={b} value={b}>
                        {BREAK_TYPE_LABEL[b]}
                      </option>
                    ))}
                  </select>
                )}
              </Field>
            )}
            <TextArea
              label="What is wrong?"
              required
              rows={4}
              value={whatIsWrong}
              onChange={setWhatIsWrong}
              hint="Be specific: the value, the source, the reasoning. One issue per report."
              error={tried ? errors.what : null}
            />
            <SeveritySelect value={severity} onChange={setSeverity} error={tried ? errors.severity : null} hint="Critical: it could reverse a strategic conclusion." />
            <Field label="Corrected value (optional)">
              {({ id }) => <input id={id} className="field" value={corrected} onChange={(e) => setCorrected(e.target.value)} />}
            </Field>
            <Field label="Source address or citation (optional)">
              {({ id }) => <input id={id} className="field" value={citation} onChange={(e) => setCitation(e.target.value)} placeholder="https://… or author, title, year" />}
            </Field>
            <p className="mb-3 text-xs text-paper-mute">
              Reported as {reviewerCodePreview(store)}. Nothing is published; your name is not attached unless your reviewer profile has one and is not anonymous.
            </p>
            <div className="flex flex-wrap gap-2">
              <button type="submit" className="btn btn-primary btn-sm" disabled={busy}>
                {busy ? 'Sending…' : 'Submit the report'}
              </button>
              <button type="button" className="btn btn-ghost btn-sm" onClick={onClose}>
                Cancel
              </button>
            </div>
          </form>
        )}
      </div>
    </div>
  );
}

/** The pseudonymous code a report will carry (the profile's id, else the stored anonymous one; nothing is created here). */
function reviewerCodePreview(store?: ReviewStorage): string {
  const s = store ?? reviewStorage();
  const p = s.loadProfile();
  if (p?.reviewer_id) return `reviewer ${pseudonymReviewerCode(p.reviewer_id)}`;
  const saved = s.loadDraft<{ reviewer_id?: unknown }>(...ANON_SLOT);
  return typeof saved?.reviewer_id === 'string' ? `anonymous reviewer ${pseudonymReviewerCode(saved.reviewer_id)}` : 'a new anonymous reviewer';
}
