// The blind-first guarantee on the committed review data (public/data/review, built by
// scripts/review/build-review-data.mjs from config/review and the shipped bundles; skipped when absent), for every
// case of every campaign (all 23 of the first round): before the blind answer the page carries none of the
// conclusion's words (its label outside the answer options, detail, reason), no reasoning text (the summary, the
// rule, the step sentences and drivers), no alternative ("why not", "what would make it", the margin), no
// reveal-only fact, no post-reveal remainder (the rest of a compact chain, a partnership's coordination changes and
// memo boxes, a sector chain's chokepoint note), no robustness class and not why the case was chosen, and it passes
// the bare-acronym and banned-word checks; after an "Outside my expertise" answer is locked, all of them are there.
// A reveal text that the neutral brief itself states (the same figure shown as evidence) or that an answer option
// shows is not a secret. The wording of the built texts themselves is checked once, against a list of known
// upstream findings.

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
import { REVEALING_FACT_IDS } from '../manifest.ts';
import { QUOTED_FROM_EXPLORER } from '../qc/snapshot.ts';
import type { CasesFile, ReviewCase } from '../model.ts';
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

/** The reveal-only texts of a case: the conclusion's words, the reasoning, the alternatives and the remainders. */
function revealTexts(c: ReviewCase): (string | null | undefined)[] {
  const k = c.snapshot.conclusion;
  const r = c.snapshot.reasoning;
  const out: (string | null | undefined)[] = [k.reason, k.detail, c.why_selected];
  // the revealing facts the reveal lists (a value the release does not carry shows as "not available")
  for (const id of REVEALING_FACT_IDS) {
    const f = c.snapshot.facts[id];
    if (f && c.reveal_fields.includes(id) && f.value !== null && f.value !== undefined && typeof f.display === 'string') out.push(f.display);
  }
  if (r) {
    out.push(r.conclusion.summary, r.rule);
    for (const st of r.steps) out.push(st.sentence, ...st.drivers);
    for (const a of r.alternatives) out.push(a.why_not, a.would_need, a.margin);
    for (const s of r.compact_chain ?? []) out.push(s.display, s.text);
    for (const x of r.partnership?.coordination_changes ?? []) out.push(x);
    for (const m of r.partnership?.memo_choices ?? []) out.push(...m.items);
    out.push(r.sector?.chokepoint_note);
  }
  return out;
}

/** Words that give a case's conclusion or the Explorer's reasoning away (texts long enough not to occur by chance). */
function secrets(c: ReviewCase): string[] {
  // what every option shows, and what the neutral brief itself states, is not a secret
  const shown = [JSON.stringify(c.snapshot.brief ?? {}), ...c.answer_options.map((o) => `${o.label} ${o.definition ?? ''}`)].join(' ');
  return [...new Set(revealTexts(c))].filter((x): x is string => typeof x === 'string' && x.length > 12 && !shown.includes(x));
}

const AFTER_IDS = ['conclusion', 'reasoning-reveal', 'robustness', 'why-selected', 'level-2', 'level-3'];

describe.skipIf(!have)('the committed cases, blind first', () => {
  it('holds the 23 cases of the first round, every one with a brief and a reasoning reveal', () => {
    const all = campaigns.flatMap((c) => c.file.cases);
    expect(all.length).toBeGreaterThanOrEqual(23);
    for (const c of all) {
      expect(c.snapshot.brief, c.case_id).toBeTruthy();
      expect(c.snapshot.reasoning, c.case_id).toBeTruthy();
    }
  });
  for (const { id, file } of campaigns)
    for (const c of file.cases)
      it(`${id} ${c.case_id} (${c.kind}): nothing revealing before the blind answer, all of it after`, async () => {
        setReviewStoreForTests(new ReviewStore(memoryReviewSource(files)));
        const storage = createReviewStorage(memoryBackend());
        setReviewStorage(storage);
        storage.saveProfile(koreaBatteriesProfile());
        storage.markCalibrationSeen('2026-11-21T00:00:00Z', id);
        render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [`/review/case/${c.case_id}?campaign=${id}`] })} />);
        await screen.findByTestId('step-context', {}, T);
        const hidden = secrets(c);
        const labelOutsideOptions = !c.answer_options.some((o) => o.label === c.snapshot.conclusion.label);
        const check = (where: string) => {
          const t = main().textContent ?? '';
          for (const s of hidden) expect(t, `${where}: ${s}`).not.toContain(s);
          if (labelOutsideOptions) expect(t, `${where}: the conclusion label`).not.toContain(c.snapshot.conclusion.label);
          for (const tid of [...AFTER_IDS, 'alternatives', 'trace', 'reveal-facts']) expect(screen.queryByTestId(tid), `${where}: ${tid}`).toBeNull();
          expect(screen.queryByRole('link', { name: 'Open in the Explorer' })).toBeNull();
          expect(bareAcronyms(main()), where).toEqual([]);
          expect(t, where).not.toMatch(AVOID_RE);
        };
        // the brief: its question, horizon, scenario and evidence
        const b = c.snapshot.brief;
        if (b) {
          expect(screen.getByTestId('brief-question').textContent).toBe(b.decision_question);
          expect(screen.getByTestId('brief-horizon').textContent).toContain(b.time_horizon.label);
          expect(screen.getByTestId('brief-scenario')).toBeTruthy();
          expect(screen.getByTestId('brief-evidence').querySelectorAll('[data-fact]').length).toBe(b.evidence.length);
        }
        check('brief');

        fireEvent.click(screen.getByRole('button', { name: 'Give your assessment' }));
        await screen.findByTestId('step-blind', {}, T);
        // pick a first answer so the confidence and driver questions render too
        if (c.kind === 'sector_chain') fireEvent.click(screen.getAllByRole('checkbox')[0]);
        else fireEvent.click(screen.getAllByRole('radio')[0]);
        await screen.findByRole('group', { name: 'What is the most important factor driving your judgment?' }, T);
        check('blind');

        fireEvent.click(screen.getByRole('radio', { name: /^Outside my expertise/ }));
        fireEvent.click(screen.getByRole('button', { name: /Lock my answer/ }));
        const concl = await screen.findByTestId('conclusion', {}, T);
        expect(concl.textContent).toContain(c.snapshot.conclusion.label);
        const t = main().textContent ?? '';
        for (const s of hidden) expect(t, `after: ${s}`).toContain(s);
        for (const tid of AFTER_IDS) expect(screen.getByTestId(tid), `after: ${tid}`).toBeTruthy();
        if (c.snapshot.reasoning?.alternatives.length) expect(screen.getByTestId('alternatives')).toBeTruthy();
        expect(screen.getByRole('link', { name: 'Open in the Explorer' })).toBeTruthy();
        expect(bareAcronyms(main())).toEqual([]);
      });
});

/**
 * Reviewer-facing texts of the build that use a word of content/vocabulary.ts AVOID_RE because they are QUOTED
 * verbatim from the Explorer's own data (the AIB export's reason for a node's class): they are kept as the Explorer
 * says them and marked as quotes (the reason fact carries snapshot.ts QUOTED_FROM_EXPLORER as its note; the reveal
 * shows the conclusion's reason as "In the Explorer's words"). Every label the review pages write themselves
 * (snapshot.ts fact labels, threat.ts texts, the brief and the reasoning) is clean, so a NEW offender anywhere
 * fails, and so does a fixed one (remove it from the list).
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
          ['brief', s.brief ? JSON.stringify(s.brief) : null],
          ['reasoning', revealTexts(c).filter((x) => x !== s.conclusion.reason && x !== s.facts.reason?.display).join(' ')],
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
