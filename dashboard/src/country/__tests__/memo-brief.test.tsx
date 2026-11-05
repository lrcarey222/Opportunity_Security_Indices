// The country strategy brief format (memo format 'brief', scripts/92_export_country_briefs.R, 2026-11-02):
// checkMemoDoc accepts a brief's blocks (sub-headings, the choice box, tables, figures) and its appendix and
// refuses broken ones; the manifest's memo entry may carry format 'brief' and its figure files; MemoDocument
// renders a brief without a summary box, with its tables, figures (through fileUrl) and appendix.

import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { BRIEF_LABEL_PREFIX, checkCountryManifest, checkMemoDoc, isBrief, MEMO_FIGURE_FILE_RE, MEMO_LABEL_PREFIX, type MemoDoc } from '../contract.ts';
import { MemoDocument } from '../memo/MemoDocument.tsx';
import { haveFixture, readManifest } from './fixture.ts';

type AnyObj = Record<string, any>; // eslint-disable-line @typescript-eslint/no-explicit-any

const item = (text: string, refs: number[] = []) => ({ inlines: [{ t: 'text', v: text }], refs });
const value = (v: string, path: string) => ({ inlines: [{ t: 'text', v: 'Need is ' }, { t: 'value', v, path, claim: 'c1', src: 1 }, { t: 'text', v: '.' }], refs: [1] });

function brief(): AnyObj {
  return {
    schema: 'osi-country-memo', schema_version: 3, format: 'brief', format_version: 1, iso3: 'AUS',
    title: 'Australia and the Allied Industrial Base', subtitle: 'What to secure, build, source from partners, and deploy',
    eyebrow: 'Country strategy brief', label: `${BRIEF_LABEL_PREFIX} 30 October 2026.`, as_of: '2026-10-30', as_of_label: '30 October 2026',
    generated_at: '2026-11-02T13:00:00Z', verified_at: '2026-11-02T14:00:00Z', writer: 'w', verifier: 'v', words: 2000, words_tables: 300,
    facts_sha256: 'f'.repeat(64), src_sha256: 's'.repeat(64), claims_sha256: 'c'.repeat(64), summary: [],
    sections: [
      { n: 1, id: 'bottom_line', title: "Bottom line — Australia's strategic role", blocks: [
        { type: 'p', ...item('Australia supplies minerals.', [1]) },
        { type: 'choices', items: [{ label: 'Secure', kind: 'secure', ...item('Rare earths.') }, { label: 'Partner', kind: 'partner', ...item('Cells.') }] },
        { type: 'figure', id: 'strategy_map', n: 1, title: 'Where Australia should secure, build, partner or stay open', svg: 'memo-AUS-f1.0123abcd.svg',
          png: 'memo-AUS-f1.89abcdef.png', alt: 'Strategy Map of Australia', caption: item('Each cell is one step.') },
      ] },
      { n: 2, id: 'fences', title: 'Where Australia should secure, compete and stay open', blocks: [
        { type: 'h3', text: 'High fence — what cannot fail' },
        { type: 'table', n: 1, head: ['Capability', 'Why it matters'], rows: [[item('Rare-earth separation'), value('86', 'cells.electric-vehicles-refining.need')]], refs: [1] },
      ] },
    ],
    appendix: { title: 'Technical appendix', blocks: [{ type: 'ul', items: [item('Secure where an acute vulnerability is measured;')] }] },
    sources: [{ n: 1, id: 'neis_strategy', text: 'Country strategy choices.' }],
    footer: 'Allied Industrial Base Explorer, Country strategy brief: Australia',
  };
}

afterEach(cleanup);

describe('the brief format in the contract', () => {
  it('a well-formed brief passes checkMemoDoc, and isBrief tells it from a memo', () => {
    expect(checkMemoDoc(brief(), 'AUS')).toEqual([]);
    expect(isBrief(brief())).toBe(true);
    expect(isBrief({})).toBe(false);
    expect(MEMO_FIGURE_FILE_RE.test('memo-AUS-f2.0123abcd.png')).toBe(true);
    expect(MEMO_FIGURE_FILE_RE.test('memo-AUS.0123abcd.png')).toBe(false);
  });

  it('refuses broken brief blocks', () => {
    const cases: [string, (d: AnyObj) => void, RegExp][] = [
      ['unknown block', (d) => d.sections[1].blocks.push({ type: 'quote' }), /type: 'quote'/],
      ['choice kind', (d) => { d.sections[0].blocks[1].items[0].kind = 'build'; }, /kind: 'build'/],
      ['figure file', (d) => { d.sections[0].blocks[2].svg = 'figure.svg'; }, /svg: not a figure file name/],
      ['table row width', (d) => d.sections[1].blocks[1].rows[0].pop(), /1 cells, 2 columns/],
      ['section numbers', (d) => { d.sections[1].n = 5; }, /sections\[1\]\.n: not 2/],
      ['unknown format', (d) => { d.format = 'essay'; }, /memo\.format: 'essay'/],
      ['label', (d) => { d.label = 'Written by hand.'; }, /not the verbatim label/],
      ['memo label on a brief', (d) => { d.label = `${MEMO_LABEL_PREFIX} 30 October 2026.`; }, /not the verbatim label/],
    ];
    for (const [what, op, re] of cases) {
      const d = brief();
      op(d);
      expect(checkMemoDoc(d, 'AUS').join(' | '), what).toMatch(re);
    }
  });

  it.runIf(haveFixture())('a manifest memo entry may be a brief with its figure files', () => {
    const m = readManifest() as AnyObj;
    const usa = m.countries.find((c: AnyObj) => c.iso3 === 'USA');
    usa.memo = { ...usa.memo, format: 'brief', figures: ['memo-USA-f1.0123abcd.svg', 'memo-USA-f1.89abcdef.png'] };
    expect(checkCountryManifest(m)).toEqual([]);
    usa.memo.figures = ['map.svg'];
    expect(checkCountryManifest(m).join(' | ')).toMatch(/memo\.figures/);
    usa.memo.figures = [];
    usa.memo.format = 'essay';
    expect(checkCountryManifest(m).join(' | ')).toMatch(/memo\.format/);
  });
});

describe('MemoDocument renders a brief', () => {
  it('without a summary box, with its choice box, table, figure and appendix', () => {
    render(<MemoDocument doc={brief() as MemoDoc} fileUrl={(f) => `/data/country/${f}`} />);
    expect(screen.queryByRole('heading', { name: 'Summary' })).toBeNull();
    expect(screen.getByRole('heading', { level: 2, name: "1. Bottom line — Australia's strategic role" })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'High fence — what cannot fail' })).toBeTruthy();
    const box = screen.getByTestId('memo-choices');
    expect(within(box).getByText('Secure')).toBeTruthy();
    expect(within(box).getByText('Partner')).toBeTruthy();
    expect(screen.getByRole('columnheader', { name: 'Why it matters' })).toBeTruthy();
    expect(screen.getByRole('cell', { name: /Need is 86/ })).toBeTruthy();
    const img = screen.getByRole('img', { name: 'Strategy Map of Australia' }) as HTMLImageElement;
    expect(img.getAttribute('src')).toBe('/data/country/memo-AUS-f1.0123abcd.svg');
    expect(screen.getByText(/Figure 1\. Where Australia should secure/)).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Technical appendix' })).toBeTruthy();
    expect(screen.getByText('Secure where an acute vulnerability is measured;')).toBeTruthy();
  });

  it('outlines traced values in a table cell', () => {
    const { container } = render(<MemoDocument doc={brief() as MemoDoc} trace />);
    expect(container.querySelector('[data-path="cells.electric-vehicles-refining.need"]')?.textContent).toBe('86');
  });
});
