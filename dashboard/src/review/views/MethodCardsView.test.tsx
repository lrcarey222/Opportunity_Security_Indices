// The method cards page (review/views/MethodCardsView.tsx): one card at a time with its current choice, why, an
// example linking into the Explorer and the alternatives; a card outside the reviewer's stated method expertise
// is marked and still answerable; Modify / Reject need the alternative or a comment; the draft autosaves; a
// submitted card is a method_feedback submission with its stance, and the progress moves.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { createReviewStorage, setReviewStorage, type KeyValueBackend, type ReviewStorage } from '../storage.ts';
import { reviewRoute } from '../routes.tsx';
import { memoryReviewSource, ReviewStore, setReviewStoreForTests } from '../data/store.ts';
import type { MethodCard, MethodFeedbackPayload, ReviewerProfile } from '../model.ts';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import MethodCardsView, { cardDraftErrors, exampleHoldReason } from './MethodCardsView.tsx';

function memoryBackend(): KeyValueBackend {
  const m = new Map<string, string>();
  return {
    get length() {
      return m.size;
    },
    key: (i) => [...m.keys()][i] ?? null,
    getItem: (k) => m.get(k) ?? null,
    setItem: (k, v) => void m.set(k, v),
    removeItem: (k) => void m.delete(k),
  };
}

const MANIFEST = {
  schema: 'aib-review-manifest',
  schema_version: 1,
  built_at: '2027-01-01T00:00:00Z',
  app_commit: 'abc1234',
  data_release: { id: 'rel-000000000001', stamps: {}, complete: true, missing: [] },
  campaigns: [
    { campaign_id: 'c1', name: 'Round 1', status: 'active', opens: null, closes: null, review_types: ['methodology'], n_cases: 0, anchor_cases: [], cases_file: 'cases.c1.json', method_card_ids: [], framework_scenario_ids: [] },
  ],
  files: [],
};

const card = (id: string, tags: string[], title: string): MethodCard => ({
  card_id: id,
  title,
  area: 'choice_rule',
  current_choice: `Current choice of ${id}.`,
  why: `Why ${id}.`,
  example: { text: `Example of ${id}.`, explorer_path: '/countries/AUS?node=Batteries|Extraction' },
  alternatives: [`First alternative to ${id}.`, `Second alternative to ${id}.`],
  affected_outputs: ['Watch and Preserve counts'],
  code_refs: ['R/strategic_position/decision_engine.R:160'],
  method_tags: tags,
});
const CARDS = [card('MC_001', ['industrial_policy'], 'A strong position alone does not make a capability worth preserving'), card('MC_002', ['trade_data'], 'Import shares use the reconciled matrix')];

const PROFILE: ReviewerProfile = {
  reviewer_id: 'rev-1', name: null, anonymous: true, countries: [], sectors: [], stages: [], methods: ['trade_data'], confidence: { 'method:trade_data': 4 },
  familiarity_neis: 1, familiarity_explorer: 1, advised_on: [], created_at: '2027-01-01T00:00:00Z',
};

// react-router's navigation builds a Request with jsdom's AbortSignal, which Node's Request refuses (the
// existing view tests stub it the same way)
const NodeRequest = globalThis.Request;
beforeAll(() => {
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
});
afterAll(() => {
  globalThis.Request = NodeRequest;
});

let store: ReviewStorage;
beforeEach(() => {
  store = createReviewStorage(memoryBackend());
  store.saveProfile(PROFILE);
  setReviewStoreForTests(new ReviewStore(memoryReviewSource({ 'manifest.json': MANIFEST, 'method_cards.json': { schema: 'aib-review-method-cards', cards: CARDS } })));
});
afterEach(() => {
  cleanup();
  setReviewStoreForTests(null);
});

function show(path = '/review/methods') {
  const router = createMemoryRouter([{ path: '/review/methods', element: <MethodCardsView storage={store} /> }], { initialEntries: [path] });
  return render(<RouterProvider router={router} />);
}

describe('MethodCardsView', () => {
  it('one card at a time: current choice, why, example with an Explorer link, alternatives; outside expertise marked', async () => {
    const { container } = show();
    const c = await screen.findByTestId('method-card');
    expect(c.getAttribute('data-card')).toBe('MC_001');
    expect(c.textContent).toContain('Current choice of MC_001.');
    expect(c.textContent).toContain('Why MC_001.');
    expect(c.textContent).toContain('Second alternative to MC_001.');
    expect(screen.getByRole('link', { name: /See it in the Explorer/ }).getAttribute('href')).toBe('/countries/AUS?node=Batteries|Extraction');
    expect(screen.getByText('Outside your stated expertise')).toBeTruthy();
    expect(screen.getByText('Card 1 of 2')).toBeTruthy();
    expect(bareAcronyms(container)).toEqual([]);
    expect(AVOID_RE.test(container.textContent ?? '')).toBe(false);
    fireEvent.click(screen.getByRole('button', { name: /Next card/ }));
    await waitFor(() => expect(screen.getByTestId('method-card').getAttribute('data-card')).toBe('MC_002'));
    expect(screen.getByText('In your expertise')).toBeTruthy();
  });

  it('records the stance as a method_feedback submission; the draft autosaves; progress moves', async () => {
    show();
    await screen.findByTestId('method-card');
    fireEvent.click(screen.getByLabelText(/^Modify/));
    expect(store.loadDraft<{ stance: string }>('c1', 'method_feedback', 'MC_001')?.stance).toBe('modify');
    fireEvent.click(screen.getByRole('button', { name: 'Submit this card' }));
    expect(screen.getAllByText('Say what you would do instead (the alternative or a comment).').length).toBeGreaterThan(0);
    expect(store.listSubmissions()).toEqual([]);
    fireEvent.change(screen.getByLabelText(/The alternative you would use/), { target: { value: 'Preserve when the world share is 10% or more.' } });
    fireEvent.click(screen.getByLabelText(/3 · Material/));
    fireEvent.change(screen.getByLabelText('Does it bias the results systematically?'), { target: { value: 'favours_new_entrants' } });
    fireEvent.click(screen.getByRole('button', { name: 'Submit this card' }));
    await screen.findByTestId('method-result');
    const subs = store.listSubmissions('c1');
    expect(subs).toHaveLength(1);
    expect(subs[0].kind).toBe('method_feedback');
    expect(subs[0].data_release).toBe('rel-000000000001');
    expect(subs[0].payload as MethodFeedbackPayload).toMatchObject({ card_id: 'MC_001', stance: 'modify', materiality: 3, bias_direction: 'favours_new_entrants', alternative: 'Preserve when the world share is 10% or more.', outside_expertise: false });
    expect(store.loadDraft('c1', 'method_feedback', 'MC_001')).toBeNull();
    await screen.findByText('1 of 2 cards answered');
  });

  it('inside the review layout: the layout holds the page title, the card form uses its store', async () => {
    setReviewStoreForTests(
      new ReviewStore(
        memoryReviewSource({
          'manifest.json': MANIFEST,
          'cases.c1.json': { schema: 'aib-review-cases', schema_version: 1, campaign_id: 'c1', data_release: 'rel-000000000001', built_at: '2027-01-01T00:00:00Z', app_commit: 'abc1234', cases: [] },
          'method_cards.json': { schema: 'aib-review-method-cards', cards: CARDS },
        }),
      ),
    );
    setReviewStorage(store);
    const router = createMemoryRouter([reviewRoute], { initialEntries: ['/review/methods'] });
    render(<RouterProvider router={router} />);
    await screen.findByTestId('method-card', {}, { timeout: 15_000 });
    expect(screen.getAllByRole('heading', { level: 1 }).map((h) => h.textContent)).toEqual(['Expert review']);
    expect(screen.getByRole('heading', { level: 2, name: 'Method cards' })).toBeTruthy();
    fireEvent.click(screen.getByLabelText(/^Retain/));
    fireEvent.click(screen.getByRole('button', { name: 'Submit this card' }));
    await screen.findByTestId('method-result');
    expect(store.listSubmissions('c1').map((x) => (x.payload as MethodFeedbackPayload).stance)).toEqual(['retain']);
    setReviewStorage(null);
  });

  it('blind-first: inside the review layout the example (it names case conclusions) waits for the assigned cases', async () => {
    const fx = await import('./__fixtures__/reviewFixture.ts');
    const { storage } = fx.useReviewFixture({ 'manifest.json': fx.manifest(), [`cases.${fx.CAMPAIGN_ID}.json`]: fx.casesFile(), 'method_cards.json': { schema: 'aib-review-method-cards', cards: CARDS } });
    storage.saveProfile(fx.koreaBatteriesProfile());
    const router = createMemoryRouter([reviewRoute], { initialEntries: ['/review/methods'] });
    render(<RouterProvider router={router} />);
    const c = await screen.findByTestId('method-card', {}, { timeout: 15_000 });
    expect(screen.getByTestId('example-held')).toBeTruthy();
    expect(c.textContent).not.toContain('Example of MC_001.');
    expect(screen.queryByRole('link', { name: /See it in the Explorer/ })).toBeNull();
    // the card stays answerable
    expect(c.textContent).toContain('Current choice of MC_001.');
    expect(screen.getByRole('button', { name: 'Submit this card' })).toBeTruthy();
    fx.resetReviewFixture();
  });

  it('exampleHoldReason: none outside the layout; no profile; open required case; all required submitted', () => {
    const a = (id: string, optional = false) => ({ case_id: id, reason: 'assigned' as const, match: 'in' as const, optional });
    const status = (done: string[]) => (id: string) => (done.includes(id) ? ('submitted' as const) : ('not_started' as const));
    expect(exampleHoldReason(null)).toBeNull();
    expect(exampleHoldReason({ profile: null, assignments: [], statusOf: status([]) })).toBe('no_profile');
    expect(exampleHoldReason({ profile: PROFILE, assignments: [a('CASE_001'), a('CASE_009', true)], statusOf: status([]) })).toBe('open_cases');
    expect(exampleHoldReason({ profile: PROFILE, assignments: [a('CASE_001'), a('CASE_009', true)], statusOf: status(['CASE_001']) })).toBeNull();
  });

  it('cardDraftErrors: a stance is required; modify and reject need an alternative or a comment', () => {
    const base = { stance: null, materiality: null, alternative: '', affected_outputs: '', bias_direction: null, comment: '', outside_expertise: false };
    expect(cardDraftErrors(base)).toHaveLength(1);
    expect(cardDraftErrors({ ...base, stance: 'retain' })).toEqual([]);
    expect(cardDraftErrors({ ...base, stance: 'reject' })).toHaveLength(1);
    expect(cardDraftErrors({ ...base, stance: 'reject', comment: 'Use capacity data.' })).toEqual([]);
  });
});
