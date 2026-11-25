// The review toolbar (docs/QC_SYSTEM_DESIGN.md 5 step 7, "Report a data issue"): a slim bar AppRoot mounts just
// before the footer. It renders NOTHING unless the review controls are on (review/appInfo.ts reviewUiEnabled:
// VITE_REVIEW_UI=1 or a reviewer profile in this browser), and nothing on the review pages themselves (they have
// their own layout). When on: "Report a data issue" and "Break the model" for the page on screen (the dialog
// captures its context) and a link back to /review. It also remembers the last Explorer page visited (outside
// /review), which the Break-the-model page offers as its default target: in memory, mirrored to sessionStorage
// when the browser allows it (try/catch).

import { lazy, Suspense, useCallback, useEffect, useState } from 'react';
import { Link, useLocation } from 'react-router';
import { reviewUiEnabled } from '../appInfo.ts';
import type { IssueMode } from './IssueReportDialog.tsx';

// the dialog (and the submission code behind it) loads on the first click, not with the page
const IssueReportDialog = lazy(() => import('./IssueReportDialog.tsx').then((m) => ({ default: m.IssueReportDialog })));

const LAST_KEY = 'aibx-review:v1:last-explorer-page';
let lastPage: string | null = null;

const isReviewPath = (pathname: string): boolean => /^\/review(\/|$)/.test(pathname);

/** Record an Explorer page (review pages are not recorded). */
export function rememberExplorerLocation(pathname: string, search = '', hash = ''): void {
  if (isReviewPath(pathname)) return;
  lastPage = `${pathname}${search}${hash}`;
  try {
    globalThis.sessionStorage?.setItem(LAST_KEY, lastPage);
  } catch {
    // storage blocked: memory only
  }
}

/** The last Explorer page this visitor saw (path + search), or null. */
export function lastExplorerLocation(): string | null {
  if (lastPage) return lastPage;
  try {
    return globalThis.sessionStorage?.getItem(LAST_KEY) ?? null;
  } catch {
    return null;
  }
}

/** Tests: forget the last page. */
export function resetLastExplorerLocation(): void {
  lastPage = null;
  try {
    globalThis.sessionStorage?.removeItem(LAST_KEY);
  } catch {
    // nothing to clear
  }
}

export function ReviewToolbar({ enabled }: { enabled?: boolean } = {}) {
  const loc = useLocation();
  const [mode, setMode] = useState<IssueMode | null>(null);
  const close = useCallback(() => setMode(null), []);
  useEffect(() => {
    rememberExplorerLocation(loc.pathname, loc.search, loc.hash);
  }, [loc.pathname, loc.search, loc.hash]);
  if (isReviewPath(loc.pathname) || !(enabled ?? reviewUiEnabled())) return null;
  return (
    <div className="border-t hairline bg-ink-850" data-testid="review-toolbar">
      <div className="mx-auto flex w-full max-w-7xl flex-wrap items-center gap-x-4 gap-y-2 px-4 py-2 sm:px-6 lg:px-8" role="region" aria-label="Expert review">
        <span className="eyebrow">Expert review</span>
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => setMode('data_issue')} aria-haspopup="dialog">
          Report a data issue
        </button>
        <button type="button" className="btn btn-ghost btn-sm" onClick={() => setMode('break_model')} aria-haspopup="dialog">
          Break the model
        </button>
        <Link className="link ml-auto text-sm" to="/review">
          Back to the review
        </Link>
      </div>
      {mode !== null && (
        <Suspense fallback={null}>
          <IssueReportDialog open onClose={close} mode={mode} />
        </Suspense>
      )}
    </div>
  );
}
