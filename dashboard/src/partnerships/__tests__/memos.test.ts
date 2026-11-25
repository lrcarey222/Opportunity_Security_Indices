// The shipped partnership and compact memos (memos.ts): the manifest reader, a pair's memo in either order,
// a compact's memo, the date and provenance line (with the notice when a memo predates the data release),
// and, on the shipped manifest when present, that every file it names exists.

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { compactMemoOf, MEMO_CONTENTS, memoDate, memoProvenance, pairMemoOf, readMemoManifest } from '../memos.ts';

const RAW = {
  schema: 'osi-partnership-memos',
  schema_version: 1,
  pairs: {
    USA_JPN: { pair: 'USA_JPN', country_a: 'USA', country_b: 'JPN', json: 'a.json', pdf: 'a.pdf', docx: 'a.docx', as_of: '2026-11-17', generated_at: '2026-11-18T00:54:02Z', format: 'brief', verified: false },
    KOR_CAN: { pair: 'KOR_CAN', country_a: 'KOR', country_b: 'CAN', json: 'b.json' },
  },
  compacts: { 'SC-SOLAR': { compact: 'SC-SOLAR', json: 'c.json', pdf: 'c.pdf', docx: 'c.docx', as_of: '2026-11-17', format: 'brief' } },
  groups: {},
};

describe('memos', () => {
  const m = readMemoManifest(RAW);

  it('reads the manifest; anything else is no manifest', () => {
    expect(m?.schema).toBe('osi-partnership-memos');
    expect(readMemoManifest({ schema: 'other' })).toBeNull();
    expect(readMemoManifest(null)).toBeNull();
  });

  it('finds a pair memo in either order, only with a file to download', () => {
    expect(pairMemoOf(m, 'USA', 'JPN')?.pdf).toBe('a.pdf');
    expect(pairMemoOf(m, 'jpn', 'usa')?.pdf).toBe('a.pdf');
    expect(pairMemoOf(m, 'KOR', 'CAN')).toBeNull();
    expect(pairMemoOf(m, 'USA', 'KOR')).toBeNull();
    expect(pairMemoOf(null, 'USA', 'JPN')).toBeNull();
  });

  it('finds a compact memo', () => {
    expect(compactMemoOf(m, 'sc-solar')?.docx).toBe('c.docx');
    expect(compactMemoOf(m, 'SC-WIND')).toBeNull();
  });

  it('dates and provenance: says when it was written, that it is not verified, and when it predates the data', () => {
    expect(memoDate('2026-11-17')).toBe('17 November 2026');
    expect(memoDate('2026-01-05T10:00:00Z')).toBe('5 January 2026');
    expect(memoDate('soon')).toBeNull();
    const e = pairMemoOf(m, 'USA', 'JPN')!;
    expect(memoProvenance(e, '2026-11-17T00:00:00Z')).toBe('Written 17 November 2026 from the Explorer data and a dated evidence layer; not independently verified.');
    expect(memoProvenance(e, '2026-11-24T05:01:17Z')).toBe(
      'Written 17 November 2026 from the Explorer data and a dated evidence layer; not independently verified. It predates the current data release (24 November 2026), so some figures may differ from this page.',
    );
    expect(MEMO_CONTENTS.pair).toMatch(/^A decision brief on the partnership/);
  });

  const dir = resolve(process.cwd(), 'public/data/partnerships');
  it.skipIf(!existsSync(resolve(dir, 'manifest.json')))('the shipped manifest: every pdf and docx it names exists', () => {
    const real = readMemoManifest(JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')));
    expect(real).not.toBeNull();
    for (const e of [...Object.values(real!.pairs), ...Object.values(real!.compacts)]) {
      for (const f of [e.pdf, e.docx]) if (f) expect(existsSync(resolve(dir, f)), f).toBe(true);
    }
    expect(pairMemoOf(real, 'JPN', 'USA')).not.toBeNull();
    expect(compactMemoOf(real, 'SC-BATTERIES')).not.toBeNull();
  });
});
