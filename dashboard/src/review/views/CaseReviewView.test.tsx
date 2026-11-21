// The blind-first case review (/review/case/:caseId, review/views/CaseReviewView.tsx) on a synthetic campaign:
// nothing that gives the conclusion away (its label, fence, reason, why the case was chosen, reveal-only values,
// the decision trace, the Explorer link) is in the DOM before the blind answer is stored, and all of it is after;
// the "I don't know" path; verdicts 3-6 need the explanation; decision-use and threat questions only for cases
// flagged so; the submission recorded in this browser with its page context, data release and snapshot sha256;
// outside-expertise handling; a null fact shows "not available in this release"; drafts resume; no bare acronyms
// or banned words.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';
import type { CaseReviewPayload, DecisionUsePayload, Submission, ThreatReviewPayload } from '../model.ts';
import { validateSubmission } from '../manifest.ts';
import { NOT_IN_RELEASE } from './CaseReviewView.tsx';
import { CAMPAIGN_ID, CASES, installDomStubs, koreaBatteriesProfile, RELEASE, resetReviewFixture, SECRET, SHA, useReviewFixture } from './__fixtures__/reviewFixture.ts';

let restore: () => void = () => undefined;
beforeAll(() => {
  restore = installDomStubs();
});
afterAll(() => restore());
afterEach(() => {
  cleanup();
  resetReviewFixture();
  resetLastLocations();
});

const T = { timeout: 20_000 };
const renderAt = (path: string) => {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
};
const main = () => document.querySelector('main') as HTMLElement;
const text = () => main().textContent ?? '';
const click = (name: RegExp | string) => fireEvent.click(screen.getByRole('button', { name }));
const radio = (name: RegExp | string) => fireEvent.click(screen.getByRole('radio', { name }));
const clean = () => {
  expect(bareAcronyms(main())).toEqual([]);
  expect(document.body.textContent).not.toMatch(AVOID_RE);
};

/** Everything that gives CASE_001's conclusion away. */
const REVEALING = [SECRET.reason, SECRET.detail, SECRET.why, SECRET.needDisplay, SECRET.cpDisplay, SECRET.choiceDisplay, SECRET.fenceDisplay];

function setup(profile = koreaBatteriesProfile()) {
  const f = useReviewFixture();
  f.storage.saveProfile(profile);
  return f;
}

async function toBlind() {
  await screen.findByTestId('step-context', {}, T);
  click('Give your answer');
  await screen.findByTestId('step-blind', {}, T);
}

describe('the blind-first guarantee', () => {
  it('shows only the context facts before the blind answer, and the conclusion, evidence, trace and link after it', async () => {
    setup();
    renderAt('/review/case/CASE_001');
    const ctxStep = await screen.findByTestId('step-context', {}, T);
    // context facts, with a missing value said plainly
    const facts = within(ctxStep).getByTestId('context-facts');
    expect(within(facts).getByText('6.2%')).toBeTruthy();
    expect((facts.querySelector('[data-fact="imports_to_gdp"] dd') as HTMLElement).textContent).toContain(NOT_IN_RELEASE);
    // nothing that reveals the conclusion: no conclusion, choice label, fence, reason, reveal-only value, trace or link
    for (const s of REVEALING) expect(text(), s).not.toContain(s);
    expect(text()).not.toContain('Preserve');
    expect(screen.queryByTestId('conclusion')).toBeNull();
    expect(screen.queryByTestId('trace')).toBeNull();
    expect(screen.queryByRole('link', { name: 'Open in the Explorer' })).toBeNull();
    expect(text()).not.toMatch(/Fragile|Medium fence/);
    clean();

    click('Give your answer');
    await screen.findByTestId('step-blind', {}, T);
    // the options name every choice, but nothing says which one the Explorer reached
    expect(screen.getByRole('radio', { name: /Preserve/ })).toBeTruthy();
    expect(screen.getByRole('radio', { name: new RegExp(DONT_KNOW_LABEL.replace(/[/]/g, '.')) })).toBeTruthy();
    for (const s of REVEALING) expect(text(), s).not.toContain(s);
    expect(screen.queryByTestId('conclusion')).toBeNull();
    expect(screen.queryByTestId('robustness')).toBeNull();
    // the later steps cannot be entered yet
    expect((screen.getByRole('button', { name: /The Explorer’s conclusion/ }) as HTMLButtonElement).disabled).toBe(true);
    clean();

    // the answer cannot be stored without a factor
    radio(/Catch Up/);
    click(/Lock my answer/);
    expect(screen.getAllByText(/Pick at least one factor/).length).toBeGreaterThan(0);
    expect(screen.queryByTestId('conclusion')).toBeNull();
    fireEvent.click(screen.getByLabelText(/4 · High/));
    fireEvent.click(screen.getByRole('checkbox', { name: /share of world supply/ }));
    click(/Lock my answer/);

    const reveal = await screen.findByTestId('step-reveal', {}, T);
    const concl = within(reveal).getByTestId('conclusion');
    expect(concl.textContent).toContain('Preserve');
    expect(concl.textContent).toContain(SECRET.detail);
    expect(concl.textContent).toContain(SECRET.reason);
    expect(concl.querySelector('[style*="background"]')).toBeTruthy();
    expect(within(reveal).getByTestId('your-answer').textContent).toMatch(/Catch Up.*differs from the Explorer/);
    expect(within(reveal).getByTestId('reveal-facts').textContent).toContain(SECRET.needDisplay);
    expect(within(reveal).getByTestId('trace').textContent).toContain('0.75');
    expect(text()).toContain(SECRET.why);
    // robustness and data quality: two separate badges
    expect(within(reveal).getByTestId('robustness').textContent).toContain('Fragile');
    expect(within(reveal).getByTestId('robustness').textContent).not.toContain('medium');
    expect(within(reveal).getByTestId('data-quality').textContent).toContain('Graded medium');
    const link = within(reveal).getByRole('link', { name: 'Open in the Explorer' });
    expect(link.getAttribute('href')).toBe('/countries/KOR?node=Batteries|Components');
    expect(link.getAttribute('target')).toBe('_blank');
    expect(link.getAttribute('rel')).toContain('noopener');
    clean();
  });

  it('a draft resumes at the stored step, and a hand-edited draft cannot skip the blind step', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_001');
    await toBlind();
    radio(/Partner/);
    cleanup();
    // reload: the draft resumes on the blind step with the choice kept
    renderAt('/review/case/CASE_001');
    await screen.findByTestId('step-blind', {}, T);
    expect((screen.getByRole('radio', { name: /Partner/ }) as HTMLInputElement).checked).toBe(true);
    cleanup();
    // a draft edited to jump to the reveal without a stored answer goes back to the blind step
    const d = storage.loadDraft<Record<string, unknown>>(CAMPAIGN_ID, 'case', 'CASE_001') as Record<string, unknown>;
    storage.saveDraft(CAMPAIGN_ID, 'case', 'CASE_001', { ...d, step: 'reveal', reached: 'reveal' });
    renderAt('/review/case/CASE_001');
    await screen.findByTestId('step-blind', {}, T);
    expect(screen.queryByTestId('conclusion')).toBeNull();
    for (const s of REVEALING) expect(text(), s).not.toContain(s);
  });
});

describe('the rest of the flow', () => {
  it('verdicts 3-6 need the explanation; decision-use questions show for a flagged case; the submission is recorded with its context', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_001');
    await toBlind();
    radio(/Catch Up/);
    fireEvent.click(screen.getByRole('checkbox', { name: /share of world supply/ }));
    click(/Lock my answer/);
    await screen.findByTestId('step-reveal', {}, T);
    click('Give your verdict');
    await screen.findByTestId('step-verdict', {}, T);
    radio(/^5\. Probably wrong because the methodology/);
    click('Continue');
    const issue = await screen.findByTestId('step-issue', {}, T);
    expect(issue.textContent).toContain('Required for your verdict');
    // the type is preselected from the verdict; the text is required
    expect((screen.getByRole('radio', { name: 'Method problem' }) as HTMLInputElement).checked).toBe(true);
    click('Continue');
    expect(await screen.findByText('Describe the single most important thing the Explorer misses.', { selector: '[role=alert]' }, T)).toBeTruthy();
    expect(screen.queryByTestId('step-extras')).toBeNull();
    fireEvent.change(screen.getByLabelText(/What does it miss\?/), { target: { value: 'The position bar sits on the 0.75 threshold.' } });
    click('Continue');
    const extras = await screen.findByTestId('step-extras', {}, T);
    expect(within(extras).getByTestId('decision-use-step')).toBeTruthy();
    expect(within(extras).queryByTestId('threat-step')).toBeNull();
    fireEvent.click(within(extras).getAllByLabelText(/3 · Mostly/)[0]);
    clean();
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    expect(screen.getByText(/Saved in this browser/)).toBeTruthy();

    const subs = storage.listSubmissions(CAMPAIGN_ID);
    expect(subs.map((s) => s.kind).sort()).toEqual(['case_review', 'decision_use']);
    for (const s of subs) expect(validateSubmission(s).errors).toEqual([]);
    const cr = subs.find((s) => s.kind === 'case_review') as Submission<'case_review'>;
    const p: CaseReviewPayload = cr.payload;
    expect(p.case_snapshot_sha256).toBe(SHA(1));
    expect(p.blind).toMatchObject({ choice: 'catch_up', factors: ['home_share'] });
    expect(p.reveal).toMatchObject({ verdict: 5, main_issue: { type: 'method', text: 'The position bar sits on the 0.75 threshold.' } });
    expect(p.reveal.revealed_at >= p.blind.answered_at).toBe(true);
    expect(p).toMatchObject({ assignment_reason: 'anchor', expertise_match: 'in', outside_expertise: false });
    expect(cr.data_release).toBe(RELEASE);
    expect(cr.campaign_id).toBe(CAMPAIGN_ID);
    expect(cr.reviewer).toMatchObject({ reviewer_id: 'rv-test-0001', anonymous: false });
    expect(cr.page_context).toMatchObject({ url: '/review/case/CASE_001', route: '/review/case', country: 'KOR', tech: 'batteries', stage: 'Components', data_release: RELEASE, displayed_result: 'Preserve' });
    expect(cr.page_context?.bundle_stamps.posture).toEqual({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' });
    const du = subs.find((s) => s.kind === 'decision_use')?.payload as DecisionUsePayload;
    expect(du).toMatchObject({ case_id: 'CASE_001', actionable: 3 });
    // the cases page now says submitted
    cleanup();
    renderAt('/review/cases');
    const row = await screen.findByTestId('assignment-CASE_001', {}, T);
    expect(within(row).getByTestId('case-status').textContent).toBe('Submitted');
  });

  it("the I don't know path: no confidence or factors, the reveal still follows, verdict 7 preselected, recorded as outside expertise", async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_002');
    await toBlind();
    radio(new RegExp(DONT_KNOW_LABEL.replace(/[/]/g, '.')));
    expect(screen.queryByRole('group', { name: 'How confident are you?' })).toBeNull();
    expect(screen.queryByRole('group', { name: /What drives your answer/ })).toBeNull();
    click(/Lock my answer/);
    await screen.findByTestId('step-reveal', {}, T);
    expect(screen.getByTestId('your-answer').textContent).toContain(DONT_KNOW_LABEL);
    click('Give your verdict');
    await screen.findByTestId('step-verdict', {}, T);
    await waitFor(() => expect((screen.getByRole('radio', { name: /^7\. / }) as HTMLInputElement).checked).toBe(true));
    click('Continue');
    const issue = await screen.findByTestId('step-issue', {}, T);
    expect(issue.textContent).toContain('Optional');
    // CASE_002 has no extras: the issue step submits
    expect(screen.queryByRole('button', { name: 'Continue' })).toBeNull();
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const p = storage.listSubmissions(CAMPAIGN_ID)[0].payload as CaseReviewPayload;
    expect(p.blind).toMatchObject({ choice: 'dont_know', confidence: null, factors: [] });
    expect(p.reveal.verdict).toBe(7);
    expect(p.outside_expertise).toBe(true);
  });

  it('a case outside the stated expertise says so and records the honest match', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_004');
    await screen.findByTestId('step-context', {}, T);
    const notice = screen.getAllByRole('status').find((n) => n.textContent?.includes('Outside your stated expertise')) as HTMLElement;
    expect(notice.textContent).toContain(DONT_KNOW_LABEL);
    click('Give your answer');
    await screen.findByTestId('step-blind', {}, T);
    radio(new RegExp(DONT_KNOW_LABEL.replace(/[/]/g, '.')));
    click(/Lock my answer/);
    await screen.findByTestId('step-reveal', {}, T);
    click('Give your verdict');
    await screen.findByTestId('step-verdict', {}, T);
    click('Continue');
    await screen.findByTestId('step-issue', {}, T);
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const p = storage.listSubmissions(CAMPAIGN_ID)[0].payload as CaseReviewPayload;
    expect(p.expertise_match).toBe('outside');
    expect(['outside', 'self_selected']).toContain(p.assignment_reason);
  });

  it('threat questions show only for a flagged case, with the before/after table and nulls as "not modelled"', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_005');
    await toBlind();
    expect(text()).not.toContain('Build capacity zz');
    expect(screen.queryByTestId('threat-table')).toBeNull();
    radio(/Build capacity/);
    fireEvent.click(screen.getByRole('checkbox', { name: /lost to the shock/ }));
    click(/Lock my answer/);
    await screen.findByTestId('step-reveal', {}, T);
    click('Give your verdict');
    await screen.findByTestId('step-verdict', {}, T);
    radio(/^1\. /);
    click('Continue');
    await screen.findByTestId('step-issue', {}, T);
    click('Continue');
    const extras = await screen.findByTestId('step-extras', {}, T);
    expect(within(extras).queryByTestId('decision-use-step')).toBeNull();
    const table = within(extras).getByTestId('threat-table');
    expect(table.textContent).toContain('Major-supplier disruption');
    expect(table.textContent).toMatch(/Time to build the new capacity\s*not modelled/);
    // after.hhi is null
    const hhiRow = within(table).getByText(/Supplier concentration/).closest('tr') as HTMLElement;
    expect(hhiRow.textContent).toContain('0.410');
    expect(hhiRow.textContent).toContain('not modelled');
    fireEvent.click(within(extras).getAllByLabelText(/2 · Somewhat/)[0]);
    fireEvent.click(within(extras).getByRole('checkbox', { name: 'Permitting' }));
    clean();
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const subs = storage.listSubmissions(CAMPAIGN_ID);
    expect(subs.map((s) => s.kind).sort()).toEqual(['case_review', 'threat_review']);
    const th = subs.find((s) => s.kind === 'threat_review')?.payload as ThreatReviewPayload;
    expect(th).toMatchObject({ case_id: 'CASE_005', reduces_threat: 2, missing_constraints: ['permitting'] });
    expect(th.threat_snapshot_sha256).toMatch(/^[0-9a-f]{64}$/);
  });

  it('asks for a profile first, and names a case the campaign does not have', async () => {
    useReviewFixture();
    renderAt('/review/case/CASE_001');
    expect(await screen.findByRole('link', { name: 'fill in your profile' }, T)).toBeTruthy();
    expect(text()).not.toContain(SECRET.reason);
    cleanup();
    setup();
    renderAt('/review/case/CASE_999');
    expect(await screen.findByText('No such case', {}, T)).toBeTruthy();
    expect(CASES.some((c) => c.case_id === 'CASE_999')).toBe(false);
  });
});
