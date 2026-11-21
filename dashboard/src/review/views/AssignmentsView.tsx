// /review/cases: the reviewer's assignments (docs/QC_SYSTEM_DESIGN.md 5 step 3), from review/assign.ts on the
// profile, the campaign rules and a seed of the reviewer id. "Your cases" lists the required ones (the anchor
// case(s) several reviewers judge, then the cases in the reviewer's stated expertise, and, only when too few
// match, cases close to it, labelled so); "Optional" lists the one case beyond the stated expertise and the
// case the reviewer chose. Each shows why it was given, how it matches the profile and its completion. Below,
// "Choose one more case yourself" picks from the campaign's catalogue when the campaign allows it. Without a
// profile the page asks for one first. UX v2: a case not yet started opens through the one-minute calibration
// when it has not been read (ReviewLayout.tsx caseHref); each case shows its rough time; once every assigned case
// is in, the page links to the reviewer's summary.

import { Link } from 'react-router';
import { EmptyState, ProgressBar, Select, Tag } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import type { Assignment, AssignmentReason, CaseKind, ExpertiseMatch, ReviewCase } from '../model.ts';
import { CASE_STATUS_LABEL, caseMinutes, estimateMinutes, MODULE, REVIEW_PATH, useReviewCtx, withQuery, type CaseStatus } from '../ReviewLayout.tsx';

export const CASE_KIND_LABEL: Readonly<Record<CaseKind, string>> = {
  posture_cell: 'Country strategic choice',
  aib_node: 'Allied supply-chain step',
  compact: 'Strategic Sectoral Compact',
  partnership: 'Partnership',
  sector: 'Sector',
  sector_chain: 'Sector supply chain',
};

export const REASON_LABEL: Readonly<Record<AssignmentReason, string>> = {
  anchor: 'Anchor case: several reviewers judge it',
  assigned: 'Assigned to you',
  outside: 'Optional: beyond your stated expertise',
  self_selected: 'Your own choice',
};

export const MATCH_LABEL: Readonly<Record<ExpertiseMatch, string>> = {
  in: 'In your expertise',
  adjacent: 'Close to your expertise',
  outside: 'Outside your stated expertise',
};

function CaseRow({ c, a, status, href }: { c: ReviewCase; a: Assignment; status: CaseStatus; href: string }) {
  return (
    <li className="card p-4" data-testid={`assignment-${c.case_id}`} data-reason={a.reason} data-match={a.match}>
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div className="min-w-0 flex-1">
          <div className="eyebrow mb-1">{CASE_KIND_LABEL[c.kind]}</div>
          <Link className="text-base font-medium text-paper hover:underline" to={href}>
            {c.title}
          </Link>
          <div className="mt-2 flex flex-wrap gap-2 text-xs">
            <Tag accent={a.reason === 'anchor'}>{REASON_LABEL[a.reason]}</Tag>
            <Tag>{MATCH_LABEL[a.match]}</Tag>
            {status !== 'submitted' && <span className="self-center text-paper-mute tabular">~{caseMinutes(c)} minutes</span>}
          </div>
        </div>
        <div className="text-right text-sm">
          <div className={status === 'submitted' ? 'text-paper' : 'text-paper-mute'} data-testid="case-status">
            {CASE_STATUS_LABEL[status]}
          </div>
          <Link className="btn btn-ghost btn-sm mt-2" to={href}>
            {status === 'submitted' ? 'Look again' : status === 'in_progress' ? 'Resume' : 'Start'}
          </Link>
        </div>
      </div>
    </li>
  );
}

export default function AssignmentsView() {
  useTitle('Your cases', MODULE);
  const ctx = useReviewCtx();
  if (!ctx.profile)
    return (
      <EmptyState>
        <span>
          Cases are assigned from your profile.{' '}
          <Link className="link" to={`/review/profile${ctx.carry}`}>
            Fill in your profile first
          </Link>
          .
        </span>
      </EmptyState>
    );

  const rows = ctx.assignments.map((a) => ({ a, c: ctx.caseById(a.case_id) })).filter((x): x is { a: Assignment; c: ReviewCase } => x.c !== null);
  const required = rows.filter((r) => !r.a.optional);
  const optional = rows.filter((r) => r.a.optional);
  const done = required.filter((r) => ctx.statusOf(r.c.case_id) === 'submitted').length;
  const left = estimateMinutes(required.filter((r) => ctx.statusOf(r.c.case_id) !== 'submitted').map((r) => r.c));
  const assigned = new Set(rows.filter((r) => r.a.reason !== 'self_selected').map((r) => r.c.case_id));
  const choosable = ctx.cases.filter((c) => !assigned.has(c.case_id));

  return (
    <div className="space-y-8">
      <section aria-labelledby="yours-h">
        <h2 id="yours-h" className="text-xl font-medium text-paper">
          Your cases
        </h2>
        <p className="mt-2 max-w-3xl text-sm text-paper-dim">
          Chosen from your profile. Anchor cases are judged by several reviewers so NEIS can compare answers. Work through them in any order; your answers save as you go.
          {left > 0 && <span className="tabular"> About {left} minutes left.</span>}
        </p>
        {ctx.allAssignedDone && (
          <p className="mt-2 text-sm text-paper-dim" data-testid="assignments-done">
            Every assigned case is in. Thank you.{' '}
            <Link className="link" to={withQuery(REVIEW_PATH.summary, ctx.carry)}>
              See your summary
            </Link>
            .
          </p>
        )}
        {required.length ? (
          <>
            <div className="mt-4 max-w-md">
              <ProgressBar value={done} max={required.length} label={`${done} of ${required.length} submitted`} />
            </div>
            <ul className="mt-4 space-y-3" aria-label="Your assigned cases">
              {required.map(({ a, c }) => (
                <CaseRow key={c.case_id} a={a} c={c} status={ctx.statusOf(c.case_id)} href={ctx.caseHref(c.case_id)} />
              ))}
            </ul>
          </>
        ) : (
          <EmptyState>
            <span>
              No case in this campaign matches your profile yet.{' '}
              <Link className="link" to={`/review/profile${ctx.carry}`}>
                Add or rate more areas
              </Link>
              , or choose a case yourself below.
            </span>
          </EmptyState>
        )}
      </section>

      {optional.length > 0 && (
        <section aria-labelledby="optional-h">
          <h2 id="optional-h" className="text-lg font-medium text-paper">
            Optional
          </h2>
          <p className="mt-1 max-w-3xl text-sm text-paper-mute">
            A case beyond what you listed helps NEIS see how the Explorer reads to non-specialists. Answer &ldquo;Insufficient evidence to judge&rdquo; or &ldquo;Outside my expertise&rdquo; wherever you cannot judge.
          </p>
          <ul className="mt-3 space-y-3" aria-label="Optional cases">
            {optional.map(({ a, c }) => (
              <CaseRow key={c.case_id} a={a} c={c} status={ctx.statusOf(c.case_id)} href={ctx.caseHref(c.case_id)} />
            ))}
          </ul>
        </section>
      )}

      {ctx.campaign.assignment.allow_self_selected && choosable.length > 0 && (
        <section aria-labelledby="choose-h" className="max-w-xl">
          <h2 id="choose-h" className="text-lg font-medium text-paper">
            Choose one more case yourself
          </h2>
          <p className="mb-3 mt-1 text-sm text-paper-mute">Optional. Pick a case you know well that you were not given.</p>
          <Select
            label="Case"
            className="w-full"
            value={ctx.selfSelected ?? ''}
            options={[{ value: '', label: 'None' }, ...choosable.map((c) => ({ value: c.case_id, label: c.title, group: CASE_KIND_LABEL[c.kind] }))]}
            onChange={(v) => ctx.setSelfSelected(v || null)}
          />
        </section>
      )}
    </div>
  );
}
