// /review/start: the one-minute calibration before the first case (UX v2, docs/QC_UX_REVIEW.md 15). Seven short
// points on how to read a case, not a methods tutorial: managed interdependence rather than self-sufficiency, that
// dependence is not automatically a problem and domestic production not automatically preferred, that Partner and
// Watch (keep open) can be successful conclusions, that a surprising result is not necessarily an error, that
// both false alarms and missed risks matter, and that the job is to judge the evidence and reasoning, not to agree
// with NEIS. "Continue" records that the screen was read (storage.ts markCalibrationSeen, per campaign) and goes
// to the return path (?next=, a review page only; else the next open case, else the profile or the cases).

import { useEffect, useState } from 'react';
import { Link, useNavigate, useSearchParams } from 'react-router';
import { CHOICE_DEFINITION, CHOICE_NAME } from '../../content/vocabulary.ts';
import { useTitle } from '../../shell/title.ts';
import { choiceOptionLabel } from '../qc/answerOptions.ts';
import { casePath, MODULE, REVIEW_PATH, safeReturnPath, useReviewCtx, withQuery } from '../ReviewLayout.tsx';

/** The calibration points: a claim and one sentence each. */
export const CALIBRATION_POINTS: readonly { id: string; claim: string; body: string }[] = [
  {
    id: 'managed',
    claim: 'The goal is managed interdependence, not self-sufficiency.',
    body: 'The Explorer asks which dependencies are worth keeping among trusted partners and where some backup is needed, not how a country could make everything itself.',
  },
  {
    id: 'dependence',
    claim: 'Dependence is not automatically a problem.',
    body: 'Buying from a diverse set of reliable suppliers can be the resilient, affordable answer. What matters is concentration, reliability of the suppliers and what a disruption would cost.',
  },
  {
    id: 'domestic',
    claim: 'Domestic production is not automatically preferred.',
    body: 'Building at home has costs and takes years. A conclusion that does not call for it is not a weaker conclusion.',
  },
  {
    id: 'success',
    claim: `${CHOICE_NAME.partner} and ${choiceOptionLabel('watch')} can be successful conclusions.`,
    body: `${CHOICE_NAME.partner}: ${CHOICE_DEFINITION.partner} ${choiceOptionLabel('watch')}: ${CHOICE_DEFINITION.watch}`,
  },
  {
    id: 'surprise',
    claim: 'A surprising result is not necessarily an error.',
    body: 'If a conclusion surprises you, look at the evidence before deciding. If the evidence still does not support it, say exactly where it fails.',
  },
  {
    id: 'both_ways',
    claim: 'Challenge false alarms and missed risks alike.',
    body: 'Tell us where the Explorer sees a risk or an opportunity that is not there, and where it misses one that is.',
  },
  {
    id: 'job',
    claim: 'Your job is to judge the evidence and reasoning, not to agree with NEIS.',
    body: 'Disagreement with a clear reason is the most useful answer. "Insufficient evidence to judge" and "Outside my expertise" are honest answers too.',
  },
];

export default function CalibrationView() {
  useTitle('Before you start', MODULE);
  const ctx = useReviewCtx();
  const navigate = useNavigate();
  const [params] = useSearchParams();
  const next = safeReturnPath(params.get('next'));
  const fallback = !ctx.profile
    ? withQuery(REVIEW_PATH.profile, ctx.carry)
    : ctx.nextCase
      ? withQuery(casePath(ctx.nextCase.case_id), ctx.carry)
      : withQuery(REVIEW_PATH.cases, ctx.carry);
  const target = next ?? fallback;

  // navigate once the layout's context says the screen was read, so the case view never sees the old flag
  const [going, setGoing] = useState<string | null>(null);
  useEffect(() => {
    if (going && ctx.calibrationSeen) navigate(going);
  }, [going, ctx.calibrationSeen, navigate]);
  const proceed = () => {
    ctx.markCalibrationSeen();
    setGoing(target);
  };

  return (
    <section aria-labelledby="calibration-h" className="max-w-3xl">
      <p className="eyebrow">About a minute</p>
      <h2 id="calibration-h" className="mt-1 text-xl font-medium text-paper">
        Before you start
      </h2>
      <p className="mt-2 text-sm text-paper-dim">Seven things to keep in mind as you read each case. You do not need to know how the Explorer works: each case gives you the evidence first.</p>
      <ol className="mt-5 space-y-4" data-testid="calibration-points">
        {CALIBRATION_POINTS.map((p, i) => (
          <li key={p.id} className="flex gap-3" data-point={p.id}>
            <span className="tabular text-sm text-paper-mute" aria-hidden="true">
              {i + 1}.
            </span>
            <div>
              <p className="text-[0.9375rem] font-medium text-paper">{p.claim}</p>
              <p className="mt-0.5 text-sm leading-relaxed text-paper-dim">{p.body}</p>
            </div>
          </li>
        ))}
      </ol>
      <div className="mt-6 flex flex-wrap items-center gap-4">
        <button type="button" className="btn btn-primary" onClick={proceed}>
          Continue
        </button>
        <Link className="text-sm text-paper-mute hover:text-paper" to={withQuery(REVIEW_PATH.home, ctx.carry)}>
          Back to the overview
        </Link>
      </div>
      {ctx.calibrationSeen && !going && <p className="mt-3 text-xs text-paper-mute">You have read this before; it is here whenever you want it.</p>}
    </section>
  );
}
