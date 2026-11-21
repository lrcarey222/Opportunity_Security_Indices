// /review: the campaign's start page (docs/QC_SYSTEM_DESIGN.md 5 step 1). What the exercise is, how long a case
// takes (about 10-15 minutes), that nothing is published, and how answers travel in this build, said plainly
// from getSink().mode (Demo: stays in this browser / Packet: you download and send a file / Live: sent to NEIS's
// endpoint). Then start (the profile) or continue (the cases, or the unfinished case), a progress overview of
// every review module, and a download of this browser's answers as one packet file for NEIS.

import { Link } from 'react-router';
import { Notice, StatTile } from '../../components/ui.tsx';
import { downloadText } from '../../lib/download.ts';
import { useTitle } from '../../shell/title.ts';
import { MODULE, useReviewCtx } from '../ReviewLayout.tsx';
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

export default function ReviewHome() {
  useTitle('Overview', MODULE);
  const ctx = useReviewCtx();
  const mode = sinkMode(import.meta.env);
  const text = SINK_MODE_TEXT[mode];
  const required = ctx.assignments.filter((a) => !a.optional);
  const submitted = required.filter((a) => ctx.statusOf(a.case_id) === 'submitted').length;
  const firstOpen = required.find((a) => ctx.statusOf(a.case_id) !== 'submitted') ?? null;
  const mine = ctx.profile ? ctx.submissions.filter((s) => s.reviewer.reviewer_id === ctx.profile?.reviewer_id) : [];
  const count = (k: string) => mine.filter((s) => s.kind === k).length;

  const download = () => {
    if (!mine.length) return;
    const packet = buildPacket(mine);
    downloadText(JSON.stringify(packet, null, 2), packetFileName(packet), 'application/json');
  };

  return (
    <div className="space-y-8">
      <section aria-labelledby="campaign-h">
        <h2 id="campaign-h" className="text-xl font-medium text-paper">
          {ctx.campaign.name}
        </h2>
        <p className="mt-2 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">
          {ctx.campaign.description ??
            'NEIS is asking specialists to test the Allied Industrial Base Explorer. You judge a few cases in your field before you see what the Explorer concluded, then say whether and why you agree.'}
        </p>
        <ul className="mt-3 max-w-3xl list-disc space-y-1 pl-5 text-sm text-paper-dim">
          <li>The purpose is to find where the Explorer is wrong: disagreement with a reason is the most useful answer you can give.</li>
          <li>Each case takes about 10 to 15 minutes. You answer first, then see the Explorer&rsquo;s conclusion and the evidence behind it.</li>
          <li>Every question that asks for a judgement has an &ldquo;I don&rsquo;t know / outside my expertise&rdquo; answer. Written comments are optional unless you disagree.</li>
          <li>Nothing you write is published. Your name is optional and is never shared beyond NEIS.</li>
        </ul>
        <p className="mt-3 text-sm text-paper-mute">{CAMPAIGN_STATUS_TEXT[ctx.campaign.status]}</p>
      </section>

      <Notice kind="info" title={text.title}>
        {text.body}
      </Notice>

      <section aria-labelledby="start-h">
        <h2 id="start-h" className="text-lg font-medium text-paper">
          {ctx.profile ? 'Continue' : 'Start'}
        </h2>
        {ctx.profile ? (
          <div className="mt-3 flex flex-wrap gap-3">
            {firstOpen ? (
              <Link className="btn btn-primary" to={`/review/case/${firstOpen.case_id}${ctx.carry}`}>
                {ctx.statusOf(firstOpen.case_id) === 'in_progress' ? 'Resume your case' : 'Start your next case'}
              </Link>
            ) : null}
            <Link className="btn btn-ghost" to={`/review/cases${ctx.carry}`}>
              See your cases
            </Link>
            <Link className="btn btn-quiet" to={`/review/profile${ctx.carry}`}>
              Edit your profile
            </Link>
          </div>
        ) : (
          <div className="mt-3">
            <p className="mb-3 max-w-3xl text-sm text-paper-dim">First, a short profile (two minutes): the countries, sectors, supply-chain stages and methods you know, each rated 1 to 5. It decides which cases you are given.</p>
            <Link className="btn btn-primary" to={`/review/profile${ctx.carry}`}>
              Start with your profile
            </Link>
          </div>
        )}
      </section>

      {ctx.profile && (
        <section aria-labelledby="progress-h">
          <h2 id="progress-h" className="text-lg font-medium text-paper">
            Your progress
          </h2>
          <div className="mt-3 grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
            <StatTile label="Assigned cases" value={`${submitted} of ${required.length}`} sub="submitted" />
            <StatTile label="Methodology cards" value={count('method_feedback')} sub={<Link className="link" to={`/review/methods${ctx.carry}`}>answered</Link>} />
            <StatTile label="Framework questions" value={count('framework_answer')} sub={<Link className="link" to={`/review/framework${ctx.carry}`}>answered</Link>} />
            <StatTile label="Problems reported" value={count('break_model') + count('data_issue')} sub={<Link className="link" to={`/review/break${ctx.carry}`}>break the model</Link>} />
          </div>
          <div className="mt-4">
            <button type="button" className="btn btn-ghost btn-sm" onClick={download} disabled={!mine.length}>
              Download your answers ({mine.length})
            </button>
            <p className="mt-1 text-xs text-paper-mute">One file with every answer you submitted in this browser, to send to NEIS. It carries your random reviewer code, and your name only if you gave one and did not choose to stay anonymous.</p>
          </div>
        </section>
      )}

      {ctx.fallbackRules.length > 0 && (
        <p className="text-xs text-paper-mute">
          Note for NEIS: this build&rsquo;s review data does not carry the campaign&rsquo;s {ctx.fallbackRules.join(' and ').replace(/_/g, ' ')}; the standard rules of the review design are used.
        </p>
      )}
    </div>
  );
}
