// The framework scenarios page (review/views/FrameworkView.tsx): the intended reading and its explanation are
// not in the page until the reviewer has answered; answering records a framework_answer (with "I don't know"
// available) and then shows whether the two matched; a strategic compact-decision scenario offers only the
// three strategic conclusions.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { createReviewStorage, type KeyValueBackend, type ReviewStorage } from '../storage.ts';
import { memoryReviewSource, ReviewStore, setReviewStoreForTests } from '../data/store.ts';
import { DONT_KNOW, type FrameworkAnswerPayload, type FrameworkScenario } from '../model.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';
import { answerOptions } from '../qc/answerOptions.ts';
import FrameworkView, { scenarioOptions } from './FrameworkView.tsx';

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

const MANIFEST = {
  schema: 'aib-review-manifest',
  schema_version: 1,
  built_at: '2027-01-01T00:00:00Z',
  app_commit: 'abc1234',
  data_release: { id: 'rel-000000000001', stamps: {}, complete: true, missing: [] },
  campaigns: [{ campaign_id: 'c1', name: 'Round 1', status: 'active', opens: null, closes: null, review_types: ['framework'], n_cases: 0, anchor_cases: [], cases_file: 'cases.c1.json', method_card_ids: [], framework_scenario_ids: ['FS_001'] }],
  files: [],
};
const posture = answerOptions('posture_cell').filter((o) => o.value !== DONT_KNOW);
const FS_001: FrameworkScenario = {
  scenario_id: 'FS_001',
  concept: 'Import dependence with trusted suppliers',
  prompt: 'A country imports almost all of a refined material, mostly from trusted partners. What does the framework recommend?',
  options: posture,
  option_set: 'posture_choice',
  intended_answer: 'partner',
  explanation: 'INTENDED-EXPLANATION: high need with a low prize and no strong position is Partner.',
  terms: ['supply-chain need', 'trusted partners'],
};
const FS_002: FrameworkScenario = { ...FS_001, scenario_id: 'FS_002', explanation: 'Not in this campaign.' };

let store: ReviewStorage;
beforeEach(() => {
  store = createReviewStorage(memoryBackend());
  setReviewStoreForTests(new ReviewStore(memoryReviewSource({ 'manifest.json': MANIFEST, 'framework_scenarios.json': { schema: 'aib-review-framework-scenarios', scenarios: [FS_001, FS_002] } })));
});
afterEach(() => {
  cleanup();
  setReviewStoreForTests(null);
});

function show() {
  const router = createMemoryRouter([{ path: '/review/framework', element: <FrameworkView storage={store} /> }], { initialEntries: ['/review/framework'] });
  return render(<RouterProvider router={router} />);
}

describe('FrameworkView', () => {
  it('hides the intended reading until the reviewer has answered, then records the answer and shows the match', async () => {
    const { container } = show();
    await screen.findByTestId('framework-scenario');
    // the campaign lists FS_001 only
    expect(screen.getByText('Scenario 1 of 1')).toBeTruthy();
    expect(container.textContent).not.toContain('INTENDED-EXPLANATION');
    expect(container.textContent).not.toContain('The intended reading');
    expect(screen.queryByTestId('framework-reveal')).toBeNull();
    expect(screen.getByLabelText(DONT_KNOW_LABEL)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: /Record my answer/ }));
    expect(screen.getByText('Pick an answer, or say you do not know.')).toBeTruthy();
    expect(container.textContent).not.toContain('INTENDED-EXPLANATION');

    fireEvent.click(screen.getByLabelText(/^Secure/));
    fireEvent.click(screen.getByLabelText('trusted partners'));
    fireEvent.click(screen.getByRole('button', { name: /Record my answer/ }));
    const reveal = await screen.findByTestId('framework-reveal');
    expect(reveal.textContent).toContain('INTENDED-EXPLANATION');
    expect(screen.getByTestId('framework-match').getAttribute('data-match')).toBe('no');
    const subs = store.listSubmissions('c1');
    expect(subs).toHaveLength(1);
    expect(subs[0].kind).toBe('framework_answer');
    expect(subs[0].payload as FrameworkAnswerPayload).toMatchObject({ scenario_id: 'FS_001', answer: 'secure', intended_answer: 'partner', correct: false, confusing_terms: ['trusted partners'] });
  });

  it('a strategic compact-decision scenario offers the three strategic conclusions only', () => {
    const opts = answerOptions('compact', { sid: 'AIB-C01' })
      .concat(answerOptions('compact', { sid: 'SC-BATTERIES' }))
      .filter((o) => o.value !== DONT_KNOW);
    const s: FrameworkScenario = { ...FS_001, option_set: 'compact_decision', options: opts, intended_answer: 'sectoral' };
    expect(scenarioOptions(s).map((o) => o.value)).toEqual(['sectoral', 'narrower', 'none']);
    expect(scenarioOptions(FS_001)).toHaveLength(posture.length);
  });
});
