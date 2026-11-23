// The blind-first case review, UX v2 (/review/case/:caseId, review/views/CaseReviewView.tsx) on a synthetic
// campaign with one case per Explorer product (views/__fixtures__/caseV2Fixture.ts):
// - blind first, per product: before the answer is stored the page holds no conclusion, reasoning, alternative,
//   reveal-only figure, robustness class or why-selected text; after it, all of them;
// - the neutral brief: identity, question, context, horizon, scenario, stage strip, facts with source and vintage,
//   warnings, assumptions apart from the evidence, a missing value as "not available in this release";
// - the insufficient-evidence branch, the verdict taxonomy with validity, persuasion and the computed prior, the
//   conditional red team, the product questions per product, the sector-chain multi-select;
// - notes autosave and resume, and convert at the wrap-up; review modes; source and provenance links;
// - drafts resume, a hand-edited draft cannot skip the blind step, a v1 draft migrates; the calibration screen
//   comes first; a case built without a brief still works; no bare acronyms or banned words.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import type { CaseReviewPayload, DecisionUsePayload, ReviewCase, Submission, ThreatReviewPayload } from '../model.ts';
import { validateSubmission } from '../manifest.ts';
import { CASE_DRAFT_KIND_V1, CASE_DRAFT_KIND_V2 } from '../storage.ts';
import { NOT_IN_RELEASE } from './CaseReviewView.tsx';
import { CAMPAIGN_ID, installDomStubs, koreaBatteriesProfile, RELEASE, resetReviewFixture, SHA, useReviewFixture } from './__fixtures__/reviewFixture.ts';
import { COMPACT, COUNTRY, LEGACY, PARTNERSHIP, revealTexts, SECTOR_CHAIN, SECTOR_NODE, V2_SECRET, v2Files } from './__fixtures__/caseV2Fixture.ts';

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
  expect(main().textContent).not.toMatch(AVOID_RE);
};

function setup(opts: { profile?: ReturnType<typeof koreaBatteriesProfile>; calibrated?: boolean } = {}) {
  const f = useReviewFixture(v2Files());
  f.storage.saveProfile(opts.profile ?? koreaBatteriesProfile());
  if (opts.calibrated !== false) f.storage.markCalibrationSeen('2026-11-21T00:00:00Z', CAMPAIGN_ID);
  return f;
}

async function toBlind() {
  await screen.findByTestId('step-context', {}, T);
  click('Give your assessment');
  await screen.findByTestId('step-blind', {}, T);
}

/** A post-reveal step, once its lazily loaded content is in (components/case/PostReveal.tsx). */
async function ready(id: string): Promise<HTMLElement> {
  const el = await screen.findByTestId(id, {}, T);
  await waitFor(() => expect(within(el).queryByText('Loading the next step')).toBeNull(), T);
  return el;
}

/** A recommendation with confidence and driver. */
function recommend(option: RegExp, confidence: 'Low' | 'Medium' | 'High' = 'High', driver = 'Domestic capability') {
  radio(option);
  radio(confidence);
  radio(driver);
}

async function lock() {
  click(/Lock my answer/);
  return ready('step-reveal');
}

/** Nothing of the reveal is in the page. */
function expectNothingRevealed(c: ReviewCase) {
  for (const s of revealTexts(c)) expect(text(), s).not.toContain(s);
  for (const id of ['conclusion', 'reasoning-reveal', 'alternatives', 'robustness', 'why-selected', 'level-2', 'level-3', 'trace', 'reveal-facts']) expect(screen.queryByTestId(id), id).toBeNull();
  expect(screen.queryByRole('link', { name: 'Open in the Explorer' })).toBeNull();
}

function expectEverythingRevealed(c: ReviewCase) {
  for (const s of revealTexts(c)) expect(text(), s).toContain(s);
  expect(screen.getByTestId('conclusion').textContent).toContain(c.snapshot.conclusion.label);
  expect(screen.getByTestId('alternatives')).toBeTruthy();
  expect(screen.getByTestId('robustness')).toBeTruthy();
}

describe('blind first, for every product', () => {
  for (const [c, answer] of [
    [COUNTRY, /^Catch Up/],
    [SECTOR_NODE, /^Coordinate trade/],
    [COMPACT, /^Targeted action/],
    [PARTNERSHIP, /^Trade and standards/],
  ] as const)
    it(`${c.case_id} (${c.kind}): nothing revealing before the answer is stored, all of it after`, async () => {
      setup();
      renderAt(`/review/case/${c.case_id}`);
      await screen.findByTestId('case-brief', {}, T);
      expectNothingRevealed(c);
      clean();
      await toBlind();
      // every option shows, nothing says which one the Explorer reached; later steps cannot be entered
      expect(screen.getByRole('radio', { name: /^None of these reflects my recommendation/ })).toBeTruthy();
      expect(screen.getByRole('radio', { name: /^Insufficient evidence to judge/ })).toBeTruthy();
      expect(screen.getByRole('radio', { name: /^Outside my expertise/ })).toBeTruthy();
      expect((screen.getByRole('button', { name: /The Explorer’s reasoning/ }) as HTMLButtonElement).disabled).toBe(true);
      recommend(answer, 'Medium');
      expectNothingRevealed(c);
      clean();
      await lock();
      expectEverythingRevealed(c);
      expect(screen.getByTestId('your-answer').textContent).toMatch(/differs from the Explorer/);
      clean();
    });

  it('CASE_105 (sector chain): stages selected, then the first; the Explorer stages and lead only after', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_105');
    await screen.findByTestId('brief-sector-stages', {}, T);
    expect(within(screen.getByTestId('stage-strip')).getAllByText(/covered by this chain/).length).toBe(3);
    expectNothingRevealed(SECTOR_CHAIN);
    expect(text()).not.toContain('zqv-lead');
    await toBlind();
    const stages = screen.getByRole('group', { name: /at which stages is allied intervention warranted/ });
    fireEvent.click(within(stages).getByRole('checkbox', { name: 'Refining & Processing' }));
    fireEvent.click(within(stages).getByRole('checkbox', { name: 'Component Manufacturing' }));
    // two stages: the reviewer says which comes first
    const first = screen.getByRole('group', { name: 'Which of them would you act on first?' });
    fireEvent.click(within(first).getByRole('radio', { name: /^Component Manufacturing/ }));
    radio('High');
    radio('Allied alternatives');
    expectNothingRevealed(SECTOR_CHAIN);
    await lock();
    expectEverythingRevealed(SECTOR_CHAIN);
    const strip = screen.getByTestId('sector-reveal-strip');
    expect(strip.querySelector('[data-stage="Assembly"]')?.textContent).toMatch(/acts here first/);
    expect(strip.querySelectorAll('[data-marked]').length).toBe(3);
    click('Give your verdict');
    await ready('step-verdict');
    expect(screen.getByTestId('prior').textContent).toMatch(/They differ/);
    expect(screen.getByTestId('prior').textContent).toMatch(/Overlap 67%/);
    radio(/^3\. Different initial conclusion, but the Explorer persuaded me/);
    radio('Supported');
    radio('Yes, it changed my view');
    click('Continue');
    await ready('step-product');
    expect(screen.getByRole('group', { name: /located the actual strategic bottleneck/ })).toBeTruthy();
    radio('Partly');
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const p = storage.listSubmissions(CAMPAIGN_ID)[0].payload as CaseReviewPayload;
    expect(p.blind).toMatchObject({ choice: 'Components', choices: ['Refining', 'Components'], confidence_level: 'high', driver: 'allied_alternatives' });
    // the prior compares the lead stage's key with the reviewer's first stage
    expect(p.reveal).toMatchObject({ verdict_code: 'persuaded', prior_differs: true, validity: 'supported', persuasion: 'changed', product_answers: { bottleneck_located: 'partly' } });
  });
});

describe('the neutral case brief', () => {
  it('shows identity, question, context, objective, horizon, scenario, the stage strip, facts with source and vintage, warnings, assumptions and definitions', async () => {
    setup();
    renderAt('/review/case/CASE_101');
    const brief = await screen.findByTestId('case-brief', {}, T);
    const b = COUNTRY.snapshot.brief!;
    expect(screen.getByTestId('brief-identity').textContent?.trim()).toBe(b.identity.title);
    expect(within(brief).getByTestId('brief-question').textContent).toBe(b.decision_question);
    expect(within(brief).getByTestId('brief-context').textContent).toBe(b.decision_context);
    expect(within(brief).getByTestId('brief-frame').textContent).toContain(b.objective);
    expect(within(brief).getByTestId('brief-horizon').textContent).toContain('Through 2035');
    expect(within(brief).getByTestId('brief-scenario').textContent).toContain("Today's network");
    const strip = within(brief).getByTestId('stage-strip');
    expect(strip.querySelectorAll('li').length).toBe(6);
    expect(strip.querySelector('[data-highlight]')?.getAttribute('data-stage')).toBe('Components');
    // facts: value, reading, source and vintage; a missing value said plainly; a fact's own warning inline
    const ev = within(brief).getByTestId('brief-evidence');
    const supplier = ev.querySelector('[data-fact="largest_supplier"]') as HTMLElement;
    expect(supplier.textContent).toContain('China, 76.6% of imports');
    expect(supplier.textContent).toContain('Reading: a majority from one supplier');
    expect(within(supplier).getByTestId('fact-source').textContent).toBe('Source: UN Comtrade · 2024 blended with the 12 months to Dec 2025');
    expect(within(ev.querySelector('[data-fact="import_scale"]') as HTMLElement).getByTestId('fact-value').textContent).toBe(NOT_IN_RELEASE);
    expect(ev.querySelector('[data-fact="own_world_share"]')?.textContent).toContain('Measure not stated');
    // country evidence is grouped by topic, in neutral words
    expect(ev.textContent).toContain('Where supply comes from');
    expect(ev.textContent).toContain('Domestic capability');
    // case warnings at once; assumptions apart from the evidence, with the method card
    expect(within(brief).getByTestId('brief-warnings').textContent).toContain('Production evidence limited');
    const ass = within(brief).getByTestId('brief-assumptions');
    expect(ass.textContent).toContain('Vulnerability is a rank, not a level');
    expect(ev.contains(ass)).toBe(false);
    expect(within(ass).getByRole('link', { name: 'Read the method card' }).getAttribute('href')).toBe('/review/methods?card=MC_007');
    expect(within(brief).getByTestId('brief-missing').textContent).toContain('Firm-level capacity is not captured.');
    expect(within(brief).getByTestId('brief-definitions').textContent).toContain('Share of world supply');
    clean();
  });

  it('tailors the brief: partnership sides and shared problem, the first links of a compact chain', async () => {
    setup();
    renderAt('/review/case/CASE_104');
    await screen.findByTestId('brief-partnership', {}, T);
    expect(screen.getByRole('heading', { name: 'United States brings' })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Japan brings' })).toBeTruthy();
    expect(screen.getByTestId('brief-side-b').textContent).toContain('No single-supplier dependence in this release.');
    expect(screen.getByTestId('brief-shared-problem').textContent).toContain('Rare-earth permanent magnets');
    expect(screen.queryByTestId('stage-strip')).toBeNull();
    clean();
    cleanup();
    renderAt('/review/case/CASE_103');
    const chain = await screen.findByTestId('brief-compact-chain', {}, T);
    expect([...chain.querySelectorAll('[data-chain]')].map((x) => x.getAttribute('data-chain'))).toEqual(['problem_today', 'existing_trade', 'gap_after_existing']);
    expect(within(chain).getAllByText('Estimate').length).toBe(2);
    expect(text()).not.toContain(V2_SECRET.chainRest);
  });

  it('a case built without a brief still shows its context facts and works', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_106');
    await screen.findByTestId('legacy-context', {}, T);
    expect((screen.getByTestId('context-facts').querySelector('[data-fact="imports_to_gdp"] dd') as HTMLElement).textContent).toContain(NOT_IN_RELEASE);
    expect(text()).not.toContain(LEGACY.snapshot.conclusion.reason as string);
    await toBlind();
    radio(/^Outside my expertise/);
    // outside expertise: no confidence or driver asked
    expect(screen.queryByRole('group', { name: 'How confident are you?' })).toBeNull();
    await lock();
    expect(screen.getByTestId('conclusion').textContent).toContain('Watch');
    click('Give your verdict');
    await ready('step-verdict');
    // outside expertise preselects the matching statement
    expect((screen.getByRole('radio', { name: /^10\. Outside my expertise/ }) as HTMLInputElement).checked).toBe(true);
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const p = storage.listSubmissions(CAMPAIGN_ID)[0].payload as CaseReviewPayload;
    expect(p).toMatchObject({ outside_expertise: true, blind: { choice: 'outside_expertise' }, reveal: { verdict_code: 'outside_expertise', verdict: 7, prior_differs: null } });
  });
});

describe('the blind answer', () => {
  it('insufficient evidence asks what is missing before it can be stored; the gaps are recorded', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_102');
    await toBlind();
    radio(/^Insufficient evidence to judge/);
    const missing = screen.getByTestId('missing-evidence');
    expect(within(missing).getByRole('group', { name: 'What information is missing?' })).toBeTruthy();
    click(/Lock my answer/);
    expect(screen.getAllByText(/Say what information is missing/).length).toBeGreaterThan(0);
    expect(screen.queryByTestId('conclusion')).toBeNull();
    fireEvent.click(within(missing).getByRole('checkbox', { name: 'Actual production capacity' }));
    fireEvent.click(within(missing).getByRole('checkbox', { name: 'Allied spare capacity' }));
    fireEvent.change(within(missing).getByLabelText(/Anything more specific/), { target: { value: 'Japanese magnet plant utilisation' } });
    await lock();
    expect(screen.getByTestId('your-answer').textContent).toContain('Insufficient evidence to judge');
    expect(screen.getByTestId('your-answer').textContent).not.toMatch(/differs|matches/);
    click('Give your verdict');
    await ready('step-verdict');
    expect(screen.getByTestId('prior-result').textContent).toMatch(/no recommendation of yours/);
    radio(/^4\. The Explorer's conclusion is plausible, but the evidence is insufficient/);
    radio('Partly supported');
    radio('No');
    expect(screen.getByTestId('issue-type').textContent).toContain('Insufficient evidence');
    fireEvent.change(screen.getByLabelText(/Describe it in a sentence or two/), { target: { value: 'No plant-level capacity.' } });
    click('Continue');
    // a disagreeing statement brings the red team
    await ready('step-red-team');
    fireEvent.change(screen.getByLabelText(/What would have to be true/), { target: { value: 'Japan could not double magnet output within two years.' } });
    radio('Qualification constraints / lead times');
    click('Continue');
    await ready('step-product');
    radio('Yes');
    click('Continue');
    const extras = await ready('step-extras');
    expect(within(extras).getByTestId('threat-table')).toBeTruthy();
    fireEvent.click(within(extras).getAllByLabelText(/2 · Somewhat/)[0]);
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const subs = storage.listSubmissions(CAMPAIGN_ID);
    expect(subs.map((s) => s.kind).sort()).toEqual(['case_review', 'threat_review']);
    for (const s of subs) expect(validateSubmission(s).errors).toEqual([]);
    const p = subs.find((s) => s.kind === 'case_review')!.payload as CaseReviewPayload;
    expect(p.blind).toMatchObject({ choice: 'insufficient_evidence', missing_evidence: ['production_capacity', 'allied_spare_capacity'], missing_evidence_text: 'Japanese magnet plant utilisation' });
    expect(p.reveal).toMatchObject({
      verdict_code: 'plausible_insufficient',
      verdict: 3,
      main_issue: { type: 'insufficient_evidence', text: 'No plant-level capacity.' },
      red_team: { must_be_true: 'Japan could not double magnet output within two years.', resolving_evidence: 'qualification_constraints' },
      product_answers: { bottleneck_located: 'yes' },
      prior_differs: null,
    });
    expect((subs.find((s) => s.kind === 'threat_review')!.payload as ThreatReviewPayload).case_id).toBe('CASE_102');
  });

  it('a recommendation needs a confidence level and a driver', async () => {
    setup();
    renderAt('/review/case/CASE_101');
    await toBlind();
    radio(/^Partner/);
    click(/Lock my answer/);
    expect(screen.getAllByText('Say how confident you are.').length).toBeGreaterThan(0);
    expect(screen.getAllByText('Pick the most important factor driving your judgment.').length).toBeGreaterThan(0);
    expect(screen.queryByTestId('conclusion')).toBeNull();
    expect(screen.getByRole('group', { name: 'What is the most important factor driving your judgment?' })).toBeTruthy();
    // Watch keeps its name and shows its keep-open meaning
    expect(screen.getByRole('radio', { name: /^Watch \(keep open\)/ })).toBeTruthy();
  });
});

describe('after the reveal', () => {
  it('country: the chain, Therefore, the alternative, the verdict taxonomy and the submission with its context', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_101');
    await toBlind();
    recommend(/^Preserve/, 'High');
    const reveal = await lock();
    expect(within(reveal).getByTestId('your-answer').textContent).toMatch(/Preserve.*confidence: high.*matches the Explorer/);
    const concl = within(reveal).getByTestId('conclusion');
    expect(concl.textContent).toContain('Explorer conclusion:');
    expect(within(concl).getByTestId('conclusion-summary').textContent).toBe(V2_SECRET.summary);
    expect([...reveal.querySelectorAll('[data-step]')].map((x) => x.getAttribute('data-step'))).toEqual(['need', 'prize', 'position']);
    expect(within(reveal).getByTestId('therefore').textContent).toContain('Therefore: Preserve');
    const alt = within(reveal).getByTestId('alternatives');
    expect(within(alt).getByRole('heading', { name: 'Why Preserve rather than Catch Up?' })).toBeTruthy();
    expect(within(alt).getByRole('heading', { name: 'What would make Catch Up more appropriate?' })).toBeTruthy();
    expect(alt.textContent).toContain('Decisive margin: Position 0.763 vs bar 0.75 (+0.013)');
    // levels 2 and 3 are folded in decision review; the deep link and the trace are there
    const l2 = within(reveal).getByTestId('level-2').querySelector('details') as HTMLDetailsElement;
    const l3 = within(reveal).getByTestId('level-3').querySelector('details') as HTMLDetailsElement;
    expect(l2.open).toBe(false);
    expect(l3.open).toBe(false);
    expect(within(reveal).getByTestId('source-years').textContent).toContain('UN Comtrade · 2024 blended with the 12 months to Dec 2025');
    expect(within(reveal).getByTestId('robustness').textContent).toContain('Fragile');
    expect(within(reveal).getByTestId('data-quality').textContent).toContain('Graded medium');
    expect(within(reveal).getByTestId('trace').textContent).toContain('0.75');
    expect(within(reveal).getByTestId('posture-provenance')).toBeTruthy();
    const link = within(reveal).getByRole('link', { name: 'Open in the Explorer' });
    expect(link.getAttribute('href')).toBe('/countries/KOR?node=Batteries|Components&tech=1');
    expect(link.getAttribute('target')).toBe('_blank');
    expect(link.getAttribute('rel')).toContain('noopener');
    clean();

    click('Give your verdict');
    await ready('step-verdict');
    expect(screen.getByTestId('prior-result').textContent).toBe('They match.');
    click('Continue');
    expect(screen.getAllByText('Choose the statement that best describes your view.').length).toBeGreaterThan(0);
    radio(/^2\. Same conclusion, but for different reasons/);
    radio('Partly supported');
    radio('Somewhat');
    // "different reasons": the reviewer picks the kind of difference and says which
    radio('Missing context');
    fireEvent.change(screen.getByLabelText(/Your reasons, and how they differ/), { target: { value: 'Korean cathode makers matter more than the trade share.' } });
    fireEvent.click(screen.getByRole('checkbox', { name: 'Assumption: Vulnerability is a rank, not a level' }));
    click('Continue');
    // high confidence and an agreeing statement: no red team; a country case has no product questions: decision use next
    const extras = await ready('step-extras');
    expect(screen.queryByTestId('step-red-team')).toBeNull();
    expect(within(extras).getByTestId('decision-use-step')).toBeTruthy();
    fireEvent.click(within(extras).getAllByLabelText(/3 · Mostly/)[0]);
    clean();
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    expect(screen.getByRole('link', { name: 'See your review summary' }).getAttribute('href')).toBe('/review/summary');

    const subs = storage.listSubmissions(CAMPAIGN_ID);
    expect(subs.map((s) => s.kind).sort()).toEqual(['case_review', 'decision_use']);
    for (const s of subs) expect(validateSubmission(s).errors).toEqual([]);
    const cr = subs.find((s) => s.kind === 'case_review') as Submission<'case_review'>;
    const p: CaseReviewPayload = cr.payload;
    expect(p.case_snapshot_sha256).toBe(SHA(101));
    expect(p.blind).toMatchObject({ choice: 'preserve', confidence: 5, confidence_level: 'high', driver: 'domestic_capability', factors: ['domestic_capability'], ux_version: 2 });
    expect(p.reveal).toMatchObject({
      verdict_code: 'same_different_reasons',
      verdict: 1,
      prior_differs: false,
      validity: 'partly',
      persuasion: 'somewhat',
      main_issue: { type: 'context', text: 'Korean cathode makers matter more than the trade share.', metric_ids: ['assumption:need_rank'] },
      red_team: null,
    });
    expect(p.reveal.revealed_at >= p.blind.answered_at).toBe(true);
    expect(p).toMatchObject({ expertise_match: 'in', outside_expertise: false, review_mode: 'decision' });
    expect(cr.data_release).toBe(RELEASE);
    expect(cr.page_context).toMatchObject({ url: '/review/case/CASE_101', country: 'KOR', tech: 'batteries', stage: 'Components', data_release: RELEASE, displayed_result: 'Preserve' });
    expect((subs.find((s) => s.kind === 'decision_use')!.payload as DecisionUsePayload).actionable).toBe(3);
    // the cases page now says submitted
    cleanup();
    renderAt('/review/cases');
    const row = await screen.findByTestId('assignment-CASE_101', {}, T);
    expect(within(row).getByTestId('case-status').textContent).toBe('Submitted');
  });

  it('a low-confidence answer brings the red team even when the statement agrees', async () => {
    setup();
    renderAt('/review/case/CASE_101');
    await toBlind();
    recommend(/^Preserve/, 'Low');
    await lock();
    click('Give your verdict');
    await ready('step-verdict');
    expect(screen.getByRole('button', { name: /Test the conclusion/ })).toBeTruthy();
    radio(/^1\. Same conclusion, broadly same reasoning/);
    radio('Supported');
    radio('No');
    click('Continue');
    expect(await ready('step-red-team')).toBeTruthy();
    expect(screen.getByRole('group', { name: /single piece of additional evidence/ })).toBeTruthy();
  });

  it('partnership: what coordination changes and the memo choices after the reveal, then the complementarity questions', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_104');
    await toBlind();
    recommend(/^Build together/, 'Medium', 'Allied alternatives');
    const reveal = await lock();
    expect(within(reveal).getByTestId('partnership-reveal').textContent).toContain(V2_SECRET.coordination);
    expect(reveal.querySelector('[data-memo-box="preserve"]')?.textContent).toContain('(leads)');
    click('Give your verdict');
    await ready('step-verdict');
    radio(/^1\. Same conclusion, broadly same reasoning/);
    radio('Supported');
    radio('No');
    click('Continue');
    await ready('step-product');
    expect(screen.getByRole('group', { name: /genuine industrial complementarity or primarily diplomatic compatibility/ })).toBeTruthy();
    expect(screen.getByRole('group', { name: /Do both partners have the incentive to act\? \(optional\)/ })).toBeTruthy();
    click('Submit your review');
    expect(screen.getAllByText(/Answer this question, or choose/).length).toBeGreaterThan(0);
    radio('Mostly industrial, partly diplomatic');
    radio('Hard to substitute');
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const p = storage.listSubmissions(CAMPAIGN_ID)[0].payload as CaseReviewPayload;
    expect(p.reveal.product_answers).toEqual({ complementarity: 'mostly_industrial', substitution: 'hard' });
  });

  it('compact: the rest of the causal chain after the reveal, then "does it ameliorate the threat?"', async () => {
    setup();
    renderAt('/review/case/CASE_103');
    await toBlind();
    recommend(/^Potential Sectoral Compact/, 'High', 'Concentration / security exposure');
    const reveal = await lock();
    const rest = within(reveal).getByTestId('reveal-compact-chain');
    expect([...rest.querySelectorAll('[data-chain]')].map((x) => x.getAttribute('data-chain'))).toEqual(['new_capacity', 'coalition']);
    expect(rest.getAttribute('start')).toBe('4');
    click('Give your verdict');
    await ready('step-verdict');
    radio(/^1\. /);
    radio('Supported');
    radio('No');
    click('Continue');
    await ready('step-product');
    expect(screen.getByRole('group', { name: 'Does this compact materially ameliorate the stated threat?' })).toBeTruthy();
  });
});

describe('review modes', () => {
  it('technical review opens levels 2 and 3 by default', async () => {
    setup({ profile: koreaBatteriesProfile({ review_mode: 'technical' }) });
    renderAt('/review/case/CASE_101');
    await screen.findByTestId('case-brief', {}, T);
    expect(screen.getByText('Technical review')).toBeTruthy();
    await toBlind();
    recommend(/^Catch Up/);
    const reveal = await lock();
    expect((within(reveal).getByTestId('level-2').querySelector('details') as HTMLDetailsElement).open).toBe(true);
    expect((within(reveal).getByTestId('level-3').querySelector('details') as HTMLDetailsElement).open).toBe(true);
  });
});

describe('notes', () => {
  it('autosave from the brief, resume after a reload, and convert at the wrap-up', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_104');
    await screen.findByTestId('case-brief', {}, T);
    fireEvent.change(screen.getByTestId('note-input'), { target: { value: 'Memo status looks stale.' } });
    await waitFor(() => expect(storage.loadNotes(CAMPAIGN_ID, 'CASE_104').map((n) => n.text)).toEqual(['Memo status looks stale.']));
    cleanup();
    renderAt('/review/case/CASE_104');
    await screen.findByTestId('notes-panel', {}, T);
    expect(screen.getByTestId('notes-panel').textContent).toContain('Memo status looks stale.');
    expect(screen.getByTestId('notes-panel').textContent).toContain('Written before your answer');
    await toBlind();
    recommend(/^Stay out/, 'High', 'Market opportunity');
    await lock();
    click('Give your verdict');
    await ready('step-verdict');
    radio(/^7\. I disagree with how the evidence is interpreted or weighted/);
    radio('Not supported');
    radio('No');
    fireEvent.change(screen.getByLabelText(/Describe it in a sentence or two/), { target: { value: 'Competition, not complementarity.' } });
    click('Continue');
    await ready('step-red-team');
    click('Continue');
    await ready('step-product');
    radio('Primarily diplomatic compatibility');
    // notes exist, so the wrap step follows
    click('Continue');
    const wrap = await screen.findByTestId('step-wrap', {}, T);
    const note = wrap.querySelector('[data-wrap-note]') as HTMLElement;
    fireEvent.click(within(note).getByRole('radio', { name: 'Data issue' }));
    // the full report opens with the note's text already in "What is wrong?"
    fireEvent.click(within(note).getByTestId('report-note'));
    const what = (await screen.findByLabelText(/^What is wrong\?/, {}, T)) as HTMLTextAreaElement;
    expect(what.value).toBe('Memo status looks stale.');
    fireEvent.click(screen.getByRole('button', { name: 'Close the report' }));
    clean();
    click('Submit your review');
    await screen.findByTestId('step-done', {}, T);
    const p = storage.listSubmissions(CAMPAIGN_ID)[0].payload as CaseReviewPayload;
    expect(p.notes).toHaveLength(1);
    expect(p.notes?.[0]).toMatchObject({ text: 'Memo status looks stale.', converted_to: 'data_issue' });
    expect(p.reveal).toMatchObject({ verdict_code: 'wrong_weighting', main_issue: { type: 'method' } });
  });
});

describe('drafts, migration and the calibration screen', () => {
  it('a draft resumes; a hand-edited draft cannot skip the blind step', async () => {
    const { storage } = setup();
    renderAt('/review/case/CASE_101');
    await toBlind();
    radio(/^Partner/);
    cleanup();
    renderAt('/review/case/CASE_101');
    await screen.findByTestId('step-blind', {}, T);
    expect((screen.getByRole('radio', { name: /^Partner/ }) as HTMLInputElement).checked).toBe(true);
    cleanup();
    const d = storage.loadDraft<Record<string, unknown>>(CAMPAIGN_ID, CASE_DRAFT_KIND_V2, 'CASE_101') as Record<string, unknown>;
    expect(d.version).toBe(2);
    storage.saveDraft(CAMPAIGN_ID, CASE_DRAFT_KIND_V2, 'CASE_101', { ...d, step: 'reveal', reached: 'verdict', blind: { ...(d.blind as object), answered_at: '2026-11-21T00:00:00Z' } });
    renderAt('/review/case/CASE_101');
    await screen.findByTestId('step-blind', {}, T);
    expectNothingRevealed(COUNTRY);
  });

  it('a v1 draft with a stored answer migrates: the answer stays locked and the v2 verdict is asked', async () => {
    const { storage } = setup();
    storage.saveDraft(CAMPAIGN_ID, CASE_DRAFT_KIND_V1, 'CASE_101', {
      version: 1, case_id: 'CASE_101', step: 'verdict', reached: 'verdict', assignment_reason: 'assigned', expertise_match: 'in', has_decision_use: true, has_threat: false, suggested_verdict: null,
      blind: { choice: 'catch_up', confidence: 4, factors: ['home_share'], rationale: null, answered_at: '2026-11-20T10:00:00Z' },
      reveal: { revealed_at: '2026-11-20T10:00:00Z', verdict: 5, issue_text: '', issue_type: null, metric_ids: [], suggested_choice: null, evidence_wanted: null },
      decision_use: null, threat: null, started_at: '2026-11-20T09:55:00Z', finished_at: null,
    });
    renderAt('/review/case/CASE_101');
    await ready('step-verdict');
    expect(screen.getByTestId('legacy-verdict').textContent).toContain('Probably wrong');
    expect(screen.getByTestId('prior-result').textContent).toBe('They differ.');
    click(/The Explorer’s reasoning/);
    await ready('step-reveal');
    expect(screen.getByTestId('your-answer').textContent).toMatch(/Catch Up \(confidence 4 of 5\)/);
  });

  it('sends a reviewer who has not seen the calibration screen there first, with the way back', async () => {
    setup({ calibrated: false });
    const router = renderAt('/review/case/CASE_101');
    await waitFor(() => expect(router.state.location.pathname).toBe('/review/start'), T);
    expect(new URLSearchParams(router.state.location.search).get('next')).toBe('/review/case/CASE_101');
    expect(screen.queryByTestId('case-brief')).toBeNull();
  });

  it('asks for a profile first, and names a case the campaign does not have', async () => {
    useReviewFixture(v2Files());
    renderAt('/review/case/CASE_101');
    expect(await screen.findByRole('link', { name: 'fill in your profile' }, T)).toBeTruthy();
    expect(text()).not.toContain(V2_SECRET.summary);
    cleanup();
    setup();
    renderAt('/review/case/CASE_999');
    expect(await screen.findByText('No such case', {}, T)).toBeTruthy();
  });
});
