// /review/summary: the reviewer's completion summary (UX v2, docs/QC_UX_REVIEW.md 19). From the reviewer's own
// submissions in this browser (review/aggregate.ts reviewerSummary): "You reviewed N cases. N matched your initial
// assessment. N changed your view after reviewing the evidence. N raised a potential data issue." Then optional
// next steps, none forced: send the answers (the build's sink, said plainly: download one packet file in demo and
// packet mode, a copy in live mode where each answer was already sent), review another case, challenge the
// methodology, report another problematic case. Assigned cases still open are mentioned, not pushed.

import { Link } from 'react-router';
import { EmptyState } from '../../components/ui.tsx';
import { downloadText } from '../../lib/download.ts';
import { useTitle } from '../../shell/title.ts';
import { reviewerSummary, type ReviewerCompletionSummary } from '../aggregate.ts';
import type { ReviewCase } from '../model.ts';
import { MODULE, REVIEW_PATH, useReviewCtx, withQuery } from '../ReviewLayout.tsx';
import { buildPacket, packetFileName, sinkMode, type SinkMode } from '../sink.ts';

const plural = (n: number, one: string, many: string) => `${n} ${n === 1 ? one : many}`;

/** The summary sentences, in the brief's words. */
export function summaryLines(s: ReviewerCompletionSummary): { lead: string; lines: { id: string; text: string; caseIds: string[] }[] } {
  const lines = [
    { id: 'matched', text: `${s.n_matched_initial} matched your initial assessment.`, caseIds: s.case_ids.matched },
    { id: 'changed', text: `${s.n_changed_view} changed your view after reviewing the evidence.`, caseIds: s.case_ids.changed_view },
    { id: 'data', text: `${s.n_raised_data_issue} raised a potential data issue.`, caseIds: s.case_ids.data_issue },
  ];
  if (s.n_insufficient_evidence > 0)
    lines.push({ id: 'insufficient', text: `${s.n_insufficient_evidence} had too little evidence for you to judge: that is a finding in itself.`, caseIds: s.case_ids.insufficient_evidence });
  if (s.n_outside_expertise > 0) lines.push({ id: 'outside', text: `${s.n_outside_expertise} you marked as outside your expertise.`, caseIds: [] });
  return { lead: `You reviewed ${plural(s.n_cases_reviewed, 'case', 'cases')}.`, lines };
}

/** How the "send" step works in each storage mode. */
export const SEND_TEXT: Readonly<Record<SinkMode, { button: string; body: string }>> = {
  local: { button: 'Download your answers to send to NEIS', body: 'Your answers are in this browser only. Download them as one file and send it to NEIS.' },
  packet: { button: 'Download all your answers as one file', body: 'A file was downloaded each time you submitted. If you prefer, send this single file with every answer instead.' },
  http: { button: 'Download a copy of your answers', body: 'Each answer was sent to NEIS when you submitted it. You can keep a copy for your records.' },
};

export default function CompletionView() {
  useTitle('Your summary', MODULE);
  const ctx = useReviewCtx();
  const mode = sinkMode(import.meta.env);
  const mine = ctx.mySubmissions;
  const summary = reviewerSummary(mine, ctx.cases);
  const { lead, lines } = summaryLines(summary);
  const titleOf = (id: string) => ctx.caseById(id)?.title ?? id;
  const openRequired = ctx.assignments
    .filter((a) => !a.optional && ctx.statusOf(a.case_id) !== 'submitted')
    .map((a) => ctx.caseById(a.case_id))
    .filter((c): c is ReviewCase => c !== null);
  const download = () => {
    if (!mine.length) return;
    const packet = buildPacket(mine);
    downloadText(JSON.stringify(packet, null, 2), packetFileName(packet), 'application/json');
  };

  if (!ctx.profile || summary.n_cases_reviewed === 0)
    return (
      <section aria-labelledby="summary-h">
        <h2 id="summary-h" className="text-xl font-medium text-paper">
          Your summary
        </h2>
        <EmptyState>
          <span>
            You have not submitted a case review in this browser yet.{' '}
            <Link className="link" to={withQuery(ctx.profile ? REVIEW_PATH.cases : REVIEW_PATH.profile, ctx.carry)}>
              {ctx.profile ? 'See your cases' : 'Start with your profile'}
            </Link>
            .
          </span>
        </EmptyState>
      </section>
    );

  return (
    <div className="space-y-8">
      <section aria-labelledby="summary-h" className="max-w-3xl">
        <h2 id="summary-h" className="text-xl font-medium text-paper">
          Your summary
        </h2>
        <p className="mt-3 text-lg text-paper" data-testid="summary-lead">
          {lead}
        </p>
        <ul className="mt-2 space-y-2" data-testid="summary-lines">
          {lines.map((l) => (
            <li key={l.id} data-line={l.id} className="text-[0.9375rem] text-paper-dim">
              {l.text}
              {l.caseIds.length > 0 && <span className="block text-xs text-paper-mute">{l.caseIds.map(titleOf).join(' · ')}</span>}
            </li>
          ))}
        </ul>
        {openRequired.length > 0 && (
          <p className="mt-4 text-sm text-paper-mute" data-testid="summary-open">
            {plural(openRequired.length, 'assigned case is', 'assigned cases are')} still open
            {openRequired.length <= 3 ? `: ${openRequired.map((c) => c.title).join(' · ')}` : ''}. Come back to {openRequired.length === 1 ? 'it' : 'them'} whenever you like.
          </p>
        )}
      </section>

      <section aria-labelledby="next-h" className="max-w-3xl">
        <h2 id="next-h" className="text-lg font-medium text-paper">
          What next (all optional)
        </h2>
        <div className="mt-3 panel p-4">
          <p className="text-sm text-paper-dim">{SEND_TEXT[mode].body}</p>
          <button type="button" className="btn btn-primary btn-sm mt-3" onClick={download} disabled={!mine.length}>
            {SEND_TEXT[mode].button}
          </button>
        </div>
        <ul className="mt-4 space-y-2 text-sm" data-testid="summary-actions">
          <li>
            <Link className="link" to={withQuery(REVIEW_PATH.cases, ctx.carry)}>
              Review another case
            </Link>
            <span className="text-paper-mute">: one you were not given, or one of your optional cases.</span>
          </li>
          <li>
            <Link className="link" to={withQuery(REVIEW_PATH.methods, ctx.carry)}>
              Challenge the methodology
            </Link>
            <span className="text-paper-mute">: say where a measure or rule is weak.</span>
          </li>
          <li>
            <Link className="link" to={withQuery(REVIEW_PATH.break, ctx.carry)}>
              Report another problematic case
            </Link>
            <span className="text-paper-mute">: a country, sector or partnership where the Explorer misleads.</span>
          </li>
        </ul>
        <p className="mt-4 text-sm text-paper-mute">Nothing more is required. Thank you for your time.</p>
      </section>
    </div>
  );
}
