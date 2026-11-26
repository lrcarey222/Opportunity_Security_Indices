// The strategic-case block of the Partnerships pair view and the memo action (2026-11-25), on the synthetic
// strategic section: the answer sentence, the principal purposes with their compacts as links, the rest
// under "Also"; a pair without a thesis says no strategic case is identified (never alignment as a case);
// the partnership memo appears only for a pair the memo manifest lists, as PDF and Word downloads.

import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { strategicOf, type AibStrategic } from '../../aib/contract.ts';
import { readDoc, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { setMemoManifestForTests } from '../memoStore.ts';
import { NO_CASE, NO_CASE_NOTE } from '../strategicCase.ts';
import { StrategicCase } from '../StrategicCase.tsx';
import { CompactMemoList } from '../MemoCta.tsx';
import { readMemoManifest } from '../memos.ts';

afterEach(() => {
  cleanup();
  setMemoManifestForTests(undefined);
});

const doc = readDoc(SYNTHETIC_DIR);
const s = strategicOf(doc) as AibStrategic;
const name = (x: string) => doc.countries[x] ?? x;

const MANIFEST = {
  schema: 'osi-partnership-memos',
  pairs: { AAA_BBB: { country_a: 'AAA', country_b: 'BBB', json: 'm.json', pdf: 'memo-AAA_BBB.pdf', docx: 'memo-AAA_BBB.docx', as_of: '2026-11-17', format: 'brief', verified: false } },
  compacts: {},
  groups: {},
};

function show(a: string, b: string) {
  render(
    <MemoryRouter>
      <StrategicCase strategic={s} a={a} b={b} name={name} compactHref={(id) => `/compacts/s/${id}`} dataCreated="2026-11-24T00:00:00Z" />
    </MemoryRouter>,
  );
  return screen.getByTestId('strategic-case');
}

describe('StrategicCase', () => {
  it('a pair with a thesis: the answer, the principal purpose with its compacts, the rest under Also; the memo when shipped', async () => {
    setMemoManifestForTests(MANIFEST);
    const el = show('BBB', 'AAA');
    expect(el.getAttribute('data-state')).toBe('case');
    expect(within(el).getByTestId('strategic-case-answer').textContent).toBe('For Borduria and Avalon, the strategic case is mainly expanding a market.');
    const lead = el.querySelector('[data-purpose="market"]')!;
    expect(within(lead as HTMLElement).getByRole('link', { name: 'Wind Power Equipment' }).getAttribute('href')).toBe('/compacts/s/SC-WIND');
    expect(el.querySelector('[data-purpose="also"]')?.textContent).toMatch(/^Also: securing supply\.$/);
    const pdf = await within(el).findByRole('link', { name: 'Download partnership memo (PDF)' });
    expect(pdf.getAttribute('href')).toMatch(/data\/partnerships\/memo-AAA_BBB\.pdf$/);
    expect(within(el).getByRole('link', { name: 'Word version' }).getAttribute('href')).toMatch(/memo-AAA_BBB\.docx$/);
    expect(within(el).getByTestId('memo-cta').textContent).toMatch(/not independently verified\. It predates the current data release \(24 November 2026\)/);
    expect(bareAcronyms(el)).toEqual([]);
  });

  it('a pair without a thesis: no strategic case, no alignment offered as one, no memo', async () => {
    setMemoManifestForTests(MANIFEST);
    const el = show('BBB', 'CCC');
    expect(el.getAttribute('data-state')).toBe('none');
    expect(within(el).getByTestId('strategic-case-answer').textContent).toBe(NO_CASE);
    expect(el.textContent).toContain(NO_CASE_NOTE);
    await Promise.resolve();
    expect(within(el).queryByTestId('memo-cta')).toBeNull();
  });
});

describe('CompactMemoList', () => {
  it('lists every compact memo with a PDF, by title; nothing without a manifest', () => {
    const m = readMemoManifest({
      schema: 'osi-partnership-memos',
      pairs: {},
      compacts: { 'SC-SOLAR': { title: 'Solar Manufacturing Compact', json: 'a.json', pdf: 'memo-SC-SOLAR.pdf', verified: false }, 'SC-X': { title: 'No file', json: 'b.json' } },
      groups: {},
    });
    render(<CompactMemoList manifest={m} />);
    const el = screen.getByTestId('compact-memos');
    expect(within(el).getAllByRole('link').map((l) => l.textContent)).toEqual(['Solar Manufacturing Compact']);
    expect(el.textContent).toMatch(/not independently verified\.$/);
    cleanup();
    render(<CompactMemoList manifest={null} />);
    expect(screen.queryByTestId('compact-memos')).toBeNull();
  });
});
