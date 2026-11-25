// "Report an issue" beside a substantive data point (docs/QC_SYSTEM_DESIGN.md 5): the posture cell drawer's
// decision card and measure rows, an AIB node's class, the compact and partnership headline sections. It
// renders NOTHING unless the review controls are on (review/appInfo.ts reviewUiEnabled: VITE_REVIEW_UI=1 or a
// reviewer profile in this browser), so the public Explorer is unchanged for ordinary visitors. A click opens
// the IssueReportDialog with the metric prefilled; the page context is captured by the dialog.

import { lazy, Suspense, useCallback, useState } from 'react';
import type { MetricRef } from '../model.ts';
import { reviewUiEnabled } from '../appInfo.ts';
import type { IssueMode } from './IssueReportDialog.tsx';

// the dialog (and the submission code behind it) loads on the first click, not with the page
const IssueReportDialog = lazy(() => import('./IssueReportDialog.tsx').then((m) => ({ default: m.IssueReportDialog })));

export interface ReportIssueButtonProps {
  metric?: MetricRef | null;
  displayedResult?: string | null;
  mode?: IssueMode;
  /** Visible text (default "Report an issue"); the accessible name adds the metric. */
  label?: string;
  className?: string;
  /** Tests: force the switch instead of reading the build and the review store. */
  enabled?: boolean;
}

export function ReportIssueButton({ metric = null, displayedResult = null, mode = 'data_issue', label = 'Report an issue', className, enabled }: ReportIssueButtonProps) {
  const [open, setOpen] = useState(false);
  const close = useCallback(() => setOpen(false), []);
  if (!(enabled ?? reviewUiEnabled())) return null;
  const about = metric?.label ?? displayedResult;
  return (
    <>
      <button
        type="button"
        className={`btn-quiet text-xs text-paper-mute hover:text-signal ${className ?? ''}`}
        aria-label={about ? `${label}: ${about}` : label}
        aria-haspopup="dialog"
        onClick={(e) => {
          // a row or card underneath may select on click: the report must not
          e.stopPropagation();
          setOpen(true);
        }}
        data-testid="report-issue"
        data-metric={metric?.id}
      >
        <span aria-hidden="true">⚑ </span>
        {label}
      </button>
      {open && (
        <Suspense fallback={null}>
          <IssueReportDialog open onClose={close} mode={mode} metric={metric} displayedResult={displayedResult} />
        </Suspense>
      )}
    </>
  );
}
