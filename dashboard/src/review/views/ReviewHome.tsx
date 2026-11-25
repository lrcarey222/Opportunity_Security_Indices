// /review: the campaign's start page (UX v2, docs/QC_UX_REVIEW.md 14). It leads with "Help us break the AIB
// Explorer" and three short paragraphs (what is being tested, what NEIS wants to learn, how a case works), the
// time the reviewer's own assignment takes ("~N minutes · N cases", ReviewLayout.tsx estimateMinutes), and ONE
// primary action, "Review your assigned cases": to the profile when there is none, through the one-minute
// calibration when it has not been read, else to the next open case (an unfinished one first; the resume state is
// said under the button). Only once every assigned case is in does it offer "Want to go deeper?" (methodology, a
// case we got wrong, the framework). Lower down: the campaign, what happens to the answers (getSink().mode said
// plainly: Demo stays in this browser / Packet you download and send a file / Live sent to NEIS's endpoint), and a
// download of this browser's answers as one packet file for NEIS.

import { Link } from 'react-router';
import { Notice } from '../../components/ui.tsx';
import { downloadText } from '../../lib/download.ts';
import { useTitle } from '../../shell/title.ts';
import type { ReviewCase } from '../model.ts';
import { caseMinutes, estimateMinutes, MODULE, primaryAction, REVIEW_DEEPER_LINKS, REVIEW_PATH, useReviewCtx, withQuery, type PrimaryAction } from '../ReviewLayout.tsx';
import { buildPacket, packetFileName, sinkMode, type SinkMode } from '../sink.ts';

/** What each storage mode means for the reviewer (design 5 step 1). */
export const SINK_MODE_TEXT: Readonly<Record<SinkMode, { title: string; body: string }>> = {
  local: {
    title: 'Demo mode: your answers stay in this browser',
    body: 'Nothing is sent anywhere. When you have finished, download your answers as one file below and send it to NEIS. Clearing this browser’s site data deletes them.',
  },
  packet: {
    title: 'Packet mode: you download and send a file',
    body: 'Each time you submit, your browser downloads a small file with your answers. Send the files to NEIS. A copy also stays in this browser.',
  },
  http: {
    title: 'Live mode: answers are sent to NEIS',
    body: 'Each submission is sent to the review service NEIS configured for this build. A copy stays in this browser, so nothing is lost if sending fails.',
  },
};

export const CAMPAIGN_STATUS_TEXT = {
  draft: 'This campaign is a draft: answers given now are for testing the review pages.',
  active: 'This campaign is open.',
  closed: 'This campaign is closed: you can still look through the cases, but NEIS is no longer collecting answers.',
} as const;

/** The three lead paragraphs (the third depends on whether cases have been chosen yet). */
export const HOME_LEAD = {
  title: 'Help us break the AIB Explorer',
  testing: 'We’re testing an analytical tool for deciding where countries should build strategic capabilities, rely on trusted partners, or leave supply open.',
  wantToKnow: 'We want to know where the data, reasoning or recommendations fail.',
  selected: 'We’ve selected cases based on your expertise. You’ll first make your own assessment, then see how the Explorer reached its conclusion.',
  toSelect: 'We select cases based on your expertise, from a two-minute profile. You’ll first make your own assessment, then see how the Explorer reached its conclusion.',
} as const;

/** "~N minutes · N cases" for the reviewer's assigned cases (what is left once some are in). */
export function estimateLine(required: readonly ReviewCase[], isSubmitted: (id: string) => boolean): string {
  const open = required.filter((c) => !isSubmitted(c.case_id));
  const n = required.length;
  const cases = (k: number) => `${k} ${k === 1 ? 'case' : 'cases'}`;
  if (!open.length) return `${n} of ${cases(n)} submitted`;
  if (open.length === n) return `~${estimateMinutes(required)} minutes · ${cases(n)}`;
  return `~${estimateMinutes(open)} minutes left · ${n - open.length} of ${cases(n)} submitted`;
}

function ActionNote({ action, next }: { action: PrimaryAction; next: ReviewCase | null }) {
  switch (action.state) {
    case 'no_profile':
      return <>First, a two-minute profile of what you know. It decides which cases you are given and stays in this browser.</>;
    case 'no_cases':
      return <>No case in this campaign matches your profile yet. Your cases page lets you add areas or choose a case yourself.</>;
    case 'calibration':
      return <>First, one minute on how to read the cases. Then {next ? <>&ldquo;{next.title}&rdquo;</> : 'your first case'}.</>;
    case 'resume':
      return (
        <>
          You have an unfinished case: <strong className="text-paper">{next?.title}</strong>. Your answers so far are saved in this browser.
        </>
      );
    case 'next':
      return <>Next: {next?.title}.</>;
    case 'all_done':
      return <>You have submitted every assigned case. Thank you: nothing more is required.</>;
  }
}

export default function ReviewHome() {
  useTitle('Overview', MODULE);
  const ctx = useReviewCtx();
  const mode = sinkMode(import.meta.env);
  const text = SINK_MODE_TEXT[mode];
  const required = ctx.assignments
    .filter((a) => !a.optional)
    .map((a) => ctx.caseById(a.case_id))
    .filter((c): c is ReviewCase => c !== null);
  const action = primaryAction(ctx);
  const next = action.caseId ? ctx.caseById(action.caseId) : null;
  const mine = ctx.mySubmissions;
  const count = (k: string) => mine.filter((s) => s.kind === k).length;
  const perCase = ctx.cases.map(caseMinutes);
  const range = perCase.length ? [Math.min(...perCase), Math.max(...perCase)] : [6, 6];

  const download = () => {
    if (!mine.length) return;
    const packet = buildPacket(mine);
    downloadText(JSON.stringify(packet, null, 2), packetFileName(packet), 'application/json');
  };

  return (
    <div className="space-y-10">
      <section aria-labelledby="lead-h" className="max-w-3xl">
        <h2 id="lead-h" className="text-2xl font-medium text-paper lg:text-3xl">
          Help us break the <abbr title="Allied Industrial Base">AIB</abbr> Explorer
        </h2>
        <div className="mt-3 space-y-2 text-[0.9375rem] leading-relaxed text-paper-dim" data-testid="home-lead">
          <p>{HOME_LEAD.testing}</p>
          <p>{HOME_LEAD.wantToKnow}</p>
          <p>{ctx.profile ? HOME_LEAD.selected : HOME_LEAD.toSelect}</p>
        </div>
        <p className="mt-4 text-sm font-medium text-paper tabular" data-testid="home-estimate">
          {ctx.profile && required.length
            ? estimateLine(required, (id) => ctx.statusOf(id) === 'submitted')
            : range[0] === range[1]
              ? `~2 minutes for your profile, then about ${range[0]} minutes per case`
              : `~2 minutes for your profile, then about ${range[0]} to ${range[1]} minutes per case`}
        </p>
        <div className="mt-5">
          <Link className="btn btn-primary" to={action.to} data-testid="home-primary" data-state={action.state}>
            {action.label}
          </Link>
          <p className="mt-2 text-sm text-paper-dim" data-testid="home-resume">
            <ActionNote action={action} next={next} />
          </p>
          {ctx.profile && (
            <p className="mt-3 flex flex-wrap gap-x-4 gap-y-1 text-xs text-paper-mute">
              <Link className="link" to={withQuery(REVIEW_PATH.cases, ctx.carry)}>
                See all your cases
              </Link>
              <Link className="link" to={withQuery(REVIEW_PATH.profile, ctx.carry)}>
                Edit your profile
              </Link>
              {action.state !== 'all_done' && mine.some((s) => s.kind === 'case_review') && (
                <Link className="link" to={withQuery(REVIEW_PATH.summary, ctx.carry)}>
                  Your summary so far
                </Link>
              )}
            </p>
          )}
        </div>
      </section>

      {action.state === 'all_done' && (
        <section aria-labelledby="deeper-h" data-testid="home-deeper">
          <h2 id="deeper-h" className="text-lg font-medium text-paper">
            Want to go deeper?
          </h2>
          <p className="mt-1 max-w-3xl text-sm text-paper-mute">Optional. Each is useful on its own; do as much or as little as you like.</p>
          <ul className="mt-3 grid gap-3 sm:grid-cols-3">
            {REVIEW_DEEPER_LINKS.map((l) => (
              <li key={l.to} className="card p-4">
                <Link className="text-base font-medium text-paper hover:underline" to={withQuery(l.to, ctx.carry)}>
                  {l.label}
                </Link>
                <p className="mt-1 text-sm text-paper-dim">{l.blurb}</p>
              </li>
            ))}
          </ul>
          <p className="mt-2 text-xs text-paper-mute tabular">
            So far: {count('method_feedback')} methodology answers, {count('framework_answer')} framework answers, {count('break_model') + count('data_issue')} problems reported.
          </p>
        </section>
      )}

      <section aria-labelledby="about-h" className="space-y-4 border-t hairline pt-6">
        <h2 id="about-h" className="text-lg font-medium text-paper">
          About this review
        </h2>
        <div className="max-w-3xl">
          <h3 className="text-base font-medium text-paper">{ctx.campaign.name}</h3>
          {ctx.campaign.description && <p className="mt-1 text-sm leading-relaxed text-paper-dim">{ctx.campaign.description}</p>}
          <ul className="mt-3 list-disc space-y-1 pl-5 text-sm text-paper-dim">
            <li>Disagreement with a reason is the most useful answer you can give. A surprising result is not necessarily an error, and agreeing is not the goal.</li>
            <li>Every judgement question lets you answer &ldquo;Insufficient evidence to judge&rdquo; or &ldquo;Outside my expertise&rdquo;. Written comments are optional.</li>
            <li>Nothing you write is published. Your name is optional and is never shared beyond NEIS.</li>
          </ul>
          <p className="mt-3 text-sm text-paper-mute">{CAMPAIGN_STATUS_TEXT[ctx.campaign.status]}</p>
        </div>

        <Notice kind="info" title={text.title}>
          {text.body}
        </Notice>

        {ctx.profile && (
          <div>
            <button type="button" className="btn btn-ghost btn-sm" onClick={download} disabled={!mine.length}>
              Download your answers ({mine.length})
            </button>
            <p className="mt-1 text-xs text-paper-mute">One file with every answer you submitted in this browser, to send to NEIS. It carries your random reviewer code, and your name only if you gave one and did not choose to stay anonymous.</p>
          </div>
        )}

        {ctx.fallbackRules.length > 0 && (
          <p className="text-xs text-paper-mute">
            Note for NEIS: this build&rsquo;s review data does not carry the campaign&rsquo;s {ctx.fallbackRules.join(' and ').replace(/_/g, ' ')}; the standard rules of the review design are used.
          </p>
        )}
      </section>
    </div>
  );
}
