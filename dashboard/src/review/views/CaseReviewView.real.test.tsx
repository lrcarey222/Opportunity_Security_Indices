// The blind-first guarantee on the committed review data (public/data/review, built by
// scripts/review/build-review-data.mjs from config/review and the shipped bundles; skipped when absent): for every
// case of every campaign, the context and blind steps carry none of the conclusion's words (its label outside the
// answer options, detail, reason), the case's why-selected text, or any revealing fact's display, and pass the
// bare-acronym and banned-word checks on the context step; after an "I don't know" answer the conclusion shows.
// The wording of the built texts themselves is checked once, against a list of known upstream findings.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { existsSync, readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms, bareAcronymsInText } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { memoryReviewSource, ReviewStore, setReviewStoreForTests, type ReviewManifestDoc } from '../data/store.ts';
import { DONT_KNOW_LABEL } from '../flow.ts';
import { REVEALING_FACT_IDS } from '../manifest.ts';
import { QUOTED_FROM_EXPLORER } from '../qc/snapshot.ts';
import type { CasesFile } from '../model.ts';
import { createReviewStorage, setReviewStorage } from '../storage.ts';
import { installDomStubs, koreaBatteriesProfile, memoryBackend } from './__fixtures__/reviewFixture.ts';

const DATA = join(dirname(fileURLToPath(import.meta.url)), '..', '..', '..', 'public', 'data', 'review');
const have = existsSync(join(DATA, 'manifest.json'));
const read = (f: string) => JSON.parse(readFileSync(join(DATA, f), 'utf8')) as unknown;
const manifest = have ? (read('manifest.json') as ReviewManifestDoc) : null;
const files: Record<string, unknown> = {};
const campaigns: { id: string; file: CasesFile }[] = [];
if (manifest) {
  files['manifest.json'] = manifest;
  for (const c of manifest.campaigns) {
    const f = read(c.cases_file) as CasesFile;
    files[c.cases_file] = f;
    campaigns.push({ id: c.campaign_id, file: f });
  }
}

let restore: () => void = () => undefined;
beforeAll(() => {
  restore = installDomStubs();
});
afterAll(() => restore());
afterEach(() => {
  cleanup();
  setReviewStoreForTests(null);
  setReviewStorage(null);
  resetLastLocations();
});

const T = { timeout: 30_000 };
const main = () => document.querySelector('main') as HTMLElement;

/** Words that give a case's conclusion away (texts long enough not to occur by chance). */
function secrets(c: CasesFile['cases'][number]): string[] {
  const k = c.snapshot.conclusion;
  const out = [k.reason, k.detail, c.why_selected].filter((x): x is string => typeof x === 'string' && x.length > 8);
  for (const id of REVEALING_FACT_IDS) {
    const f = c.snapshot.facts[id];
    // a revealing fact that only repeats an option label (the AIB class, the compact decision) is not a secret: every option shows
    if (f && typeof f.display === 'string' && f.display.length > 8 && !c.answer_options.some((o) => o.label === f.display)) out.push(f.display);
  }
  return out;
}

describe.skipIf(!have)('the committed cases, blind first', () => {
  for (const { id, file } of campaigns)
    for (const c of file.cases)
      it(`${id} ${c.case_id}: nothing revealing before the blind answer`, async () => {
        setReviewStoreForTests(new ReviewStore(memoryReviewSource(files)));
        const storage = createReviewStorage(memoryBackend());
        setReviewStorage(storage);
        storage.saveProfile(koreaBatteriesProfile());
        render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [`/review/case/${c.case_id}?campaign=${id}`] })} />);
        await screen.findByTestId('step-context', {}, T);
        const labelOutsideOptions = !c.answer_options.some((o) => o.label === c.snapshot.conclusion.label);
        for (const s of secrets(c)) expect(main().textContent, `context: ${s}`).not.toContain(s);
        if (labelOutsideOptions) expect(main().textContent).not.toContain(c.snapshot.conclusion.label);
        expect(screen.queryByTestId('conclusion')).toBeNull();
        expect(bareAcronyms(main())).toEqual([]);
        expect(main().textContent).not.toMatch(AVOID_RE);

        fireEvent.click(screen.getByRole('button', { name: 'Give your answer' }));
        await screen.findByTestId('step-blind', {}, T);
        // pick the first option so the driving factors (the case's measure labels) render too
        fireEvent.click(screen.getAllByRole('radio')[0]);
        await screen.findByRole('group', { name: /What drives your answer/ }, T);
        for (const s of secrets(c)) expect(main().textContent, `blind: ${s}`).not.toContain(s);
        expect(screen.queryByTestId('conclusion')).toBeNull();

        fireEvent.click(screen.getByRole('radio', { name: new RegExp(DONT_KNOW_LABEL.replace(/[/]/g, '.')) }));
        fireEvent.click(screen.getByRole('button', { name: /Lock my answer/ }));
        const concl = await screen.findByTestId('conclusion', {}, T);
        expect(concl.textContent).toContain(c.snapshot.conclusion.label);
      });
});

/**
 * Reviewer-facing texts of the build that use a word of content/vocabulary.ts AVOID_RE because they are QUOTED
 * verbatim from the Explorer's own data (the AIB export's reason for a node's class): they are kept as the Explorer
 * says them and marked as quotes (the reason fact carries snapshot.ts QUOTED_FROM_EXPLORER as its note; the reveal
 * shows the conclusion's reason as "Why, in the Explorer's words"). Every label the review pages write themselves
 * (snapshot.ts fact labels, threat.ts texts) is clean, so a NEW offender anywhere fails, and so does a fixed one
 * (remove it from the list).
 */
const KNOWN_WORDING: Readonly<Record<string, readonly string[]>> = {
  acronym: [],
  avoid: ['fact.reason.display', 'conclusion.reason'],
};

/** The texts the review pages show as headings (title), legends (question), option or factor labels. */
const LABEL_TEXT = /^(title|question|option|conclusion\.label|fact\.[a-z0-9_]+\.label)$/;

describe.skipIf(!have)('the wording of the committed cases', () => {
  it('uses no bare acronym or banned word beyond the known upstream findings', () => {
    const found = { acronym: new Set<string>(), avoid: new Set<string>() };
    for (const { file } of campaigns)
      for (const c of file.cases) {
        const s = c.snapshot;
        const texts: [string, string | null | undefined][] = [
          ['title', c.title],
          ['question', c.question],
          ['why_selected', c.why_selected],
          ['conclusion.label', s.conclusion.label],
          ['conclusion.detail', s.conclusion.detail],
          ['conclusion.reason', s.conclusion.reason],
          ...c.answer_options.map((o): [string, string] => ['option', `${o.label} ${o.definition ?? ''}`]),
          ...[...c.context_fields, ...c.reveal_fields].flatMap((id): [string, string | null | undefined][] => {
            const f = s.facts[id];
            return f
              ? [
                  [`fact.${id}.label`, f.label],
                  [`fact.${id}.display`, f.display],
                  [`fact.${id}.note`, f.note],
                ]
              : [];
          }),
          ['threat.threat', s.threat?.threat],
          ['threat.intervention', s.threat?.intervention],
          ['threat.notes', (s.threat?.notes ?? []).join(' ')],
          ['data_quality', (s.data_quality?.notes ?? []).join(' ')],
        ];
        for (const [where, t] of texts) {
          if (!t) continue;
          // acronyms are checked where the pages put the text in a heading, legend or label (the scanned scopes)
          if (LABEL_TEXT.test(where) && bareAcronymsInText(t).length) found.acronym.add(where);
          if (AVOID_RE.test(t)) found.avoid.add(where);
        }
      }
    expect([...found.acronym].sort()).toEqual([...KNOWN_WORDING.acronym].sort());
    expect([...found.avoid].sort()).toEqual([...KNOWN_WORDING.avoid].sort());
    // the quoted texts are marked as quotes
    for (const { file } of campaigns)
      for (const c of file.cases) {
        const f = c.snapshot.facts.reason;
        if (f && typeof f.display === 'string' && AVOID_RE.test(f.display)) expect(f.note, c.case_id).toBe(QUOTED_FROM_EXPLORER);
      }
  });
});
