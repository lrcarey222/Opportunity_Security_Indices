// One page title per review page: ReviewLayout holds the only h1 ("Expert review"); the home, calibration,
// summary, profile, cases, method cards, framework, break-the-model and admin views put their own title in an h2
// under it (the deeper views read the layout through useOptionalReviewCtx, so they also render on their own).

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, render, screen } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { reviewRoute } from '../routes.tsx';
import { CAMPAIGN_ID, casesFile, installDomStubs, koreaBatteriesProfile, manifest, resetReviewFixture, useReviewFixture } from './__fixtures__/reviewFixture.ts';

let restore: () => void = () => undefined;
beforeAll(() => {
  restore = installDomStubs();
});
afterAll(() => restore());
afterEach(() => {
  cleanup();
  resetReviewFixture();
});

const T = { timeout: 15_000 };
const CARD = {
  card_id: 'MC_001', title: 'A card', area: 'choice_rule', current_choice: 'c', why: 'w', example: { text: 'e' }, alternatives: ['a'], affected_outputs: ['o'], code_refs: ['R/x.R:1'], method_tags: ['indices_statistics'],
};
const SCENARIO = {
  scenario_id: 'FS_001', concept: 'c', prompt: 'p', options: [{ value: 'partner', label: 'Partner' }, { value: 'watch', label: 'Watch' }], option_set: 'posture_choice', intended_answer: 'partner', explanation: 'e', terms: [],
};

const PAGES: [string, string][] = [
  ['/review', 'Help us break the AIB Explorer'],
  ['/review/start', 'Before you start'],
  ['/review/summary', 'Your summary'],
  ['/review/profile', 'Your profile'],
  ['/review/cases', 'Your cases'],
  ['/review/methods', 'Method cards'],
  ['/review/framework', 'What would the framework say?'],
  ['/review/break', 'Find where the Explorer is wrong'],
  ['/review/admin', 'QC dashboard'],
];

describe('review pages have one h1', () => {
  for (const [path, title] of PAGES)
    it(`${path}: the layout's h1 and the view's h2 "${title}"`, async () => {
      const { storage } = useReviewFixture({
        'manifest.json': manifest(),
        [`cases.${CAMPAIGN_ID}.json`]: casesFile(),
        'method_cards.json': { schema: 'aib-review-method-cards', cards: [CARD] },
        'framework_scenarios.json': { schema: 'aib-review-framework-scenarios', scenarios: [SCENARIO] },
        'qc_summary.json': {},
        'robustness.json': {},
      });
      storage.saveProfile(koreaBatteriesProfile());
      render(<RouterProvider router={createMemoryRouter([reviewRoute], { initialEntries: [path] })} />);
      await screen.findByRole('heading', { level: 2, name: title }, T);
      expect(screen.getAllByRole('heading', { level: 1 }).map((h) => h.textContent)).toEqual(['Expert review']);
    });
});
