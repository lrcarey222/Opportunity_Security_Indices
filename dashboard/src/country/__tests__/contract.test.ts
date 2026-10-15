// The Country contract on the R-written synthetic fixture (scripts/88 --synthetic): the manifest, every
// fact sheet, the companion drivers files and the placeholder memo pass the runtime checks; the broken
// sheets shared with the R mirror (tests/testthat/fixtures/country_broken_cases.json) each fail with the
// expected problem; every display string follows the formatter's rules (A8.3).

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import {
  checkCountryManifest, checkDriversDoc, checkFactSheet, checkMemoDoc, COUNTRY_CELL_KEYS, COUNTRY_FILE_RE, countryMedianOf, MEMO_LABEL_PREFIX, quadrantOf, type FactSheet, type Fv,
} from '../contract.ts';
import { haveFixture, readJson, readManifest, readSheet } from './fixture.ts';

const CASES = JSON.parse(readFileSync(resolve(process.cwd(), '../tests/testthat/fixtures/country_broken_cases.json'), 'utf8')) as {
  base: string;
  cases: { op: string; keyword: string; what: string }[];
};

type AnyObj = Record<string, any>; // eslint-disable-line @typescript-eslint/no-explicit-any

/** Apply one named mutation (the same ops as tests/testthat/test-country-check.R). */
export function breakSheet(sheet: FactSheet, op: string): AnyObj {
  const s = structuredClone(sheet) as AnyObj;
  const placed = s.cells.findIndex((c: AnyObj) => c.placed);
  const ins = s.cells.findIndex((c: AnyObj) => c.si?.in_sample);
  const ranked = s.partners.rows.map((r: AnyObj, i: number) => [r, i]).filter(([r]: [AnyObj]) => r.rank?.v !== null);
  switch (op) {
    case 'quadrant_flip': {
      const c = s.cells[placed];
      c.quadrant = c.quadrant === 'grow' ? 'watch' : 'grow';
      break;
    }
    case 'country_median_off':
      s.summary.country_median.es.v += 5;
      break;
    case 'pos_wrong_side': {
      const p = s.cells[placed].es;
      p.pos = p.v >= p.med.v ? 10 : 90;
      break;
    }
    case 'si_parts_off':
      s.cells[ins].si.parts.eo.v += 0.1;
      break;
    case 'partner_rank_dup':
      ranked[1][0].rank = { ...ranked[0][0].rank };
      break;
    case 'reason_unknown':
      s.partners.rows[0].reason = 'xx';
      break;
    case 'cell_key_unknown':
      s.cells[0].key = 'fusion-upstream';
      break;
    case 'summary_count':
      s.summary.n_placed.v += 1;
      break;
    case 'order_swap': {
      const o = s.summary.es_order as string[];
      const pct = (k: string) => s.cells.find((c: AnyObj) => c.key === k).es.pct.v;
      const i = o.findIndex((k, j) => j + 1 < o.length && pct(o[j + 1]) !== pct(k));
      [o[i], o[i + 1]] = [o[i + 1], o[i]];
      break;
    }
    case 'fv_empty_d':
      s.cells[placed].es.pct.d = '';
      break;
    case 'no_es_cell_with_es': {
      const g = s.cells.find((c: AnyObj) => c.key === 'geothermal-upstream');
      g.es = structuredClone(s.cells[placed].es);
      break;
    }
    case 'placed_not_rankable':
      s.cells[placed].es.rankable = false;
      break;
    case 'caveat_unknown':
      s.caveats[0].id = 'nope';
      break;
    case 'priority_rank_offset':
      // the 2026-10 bug: out-of-sample sectors took the top ranks, so in-sample ranks ran past n
      s.cells[ins].priority.rank.v = s.cells[ins].si.n.v + 2;
      break;
    default:
      throw new Error(`unknown op ${op}`);
  }
  return s;
}

/** Integer percent, half away from zero; 1-99 unless every or no other economy is beaten (A8.3). */
const pctText = (v: number) => Math.min(v < 100 ? 99 : 100, Math.max(v > 0 ? 1 : 0, Math.round(Number(v.toFixed(6)))));

const ordinal = (n: number) => {
  const r = n % 100;
  const suf = r >= 11 && r <= 13 ? 'th' : (['th', 'st', 'nd', 'rd'][n % 10] ?? 'th');
  return `${n}${suf}`;
};

/** Walk every { v, d } pair with its path. */
function* fvs(x: unknown, path = ''): Generator<[string, Fv]> {
  if (Array.isArray(x)) {
    for (let i = 0; i < x.length; i++) yield* fvs(x[i], `${path}[${i}]`);
    return;
  }
  if (x && typeof x === 'object') {
    const o = x as AnyObj;
    if ('v' in o && 'd' in o && (typeof o.v === 'number' || o.v === null) && typeof o.d === 'string') yield [path, o as Fv];
    for (const [k, v] of Object.entries(o)) yield* fvs(v, path ? `${path}.${k}` : k);
  }
}

describe.skipIf(!haveFixture())('Country contract on the synthetic fixture', () => {
  const m = readManifest();

  it('the manifest passes and names files of the right form', () => {
    expect(checkCountryManifest(m)).toEqual([]);
    expect(m.synthetic).toBe(true);
    expect(m.validated).toBe(false);
    for (const c of m.countries) {
      if (c.file) expect(c.file).toMatch(COUNTRY_FILE_RE);
      if (c.drivers) expect(c.drivers).toMatch(COUNTRY_FILE_RE);
    }
    expect(m.counts.sheets).toBe(m.countries.filter((c) => c.file).length);
  });

  it('every sheet passes checkFactSheet with its manifest entry', () => {
    for (const e of m.countries.filter((c) => c.file)) {
      const s = readJson<FactSheet>(e.file!);
      expect(checkFactSheet(s, e), e.iso3).toEqual([]);
      expect(s.cells.map((c) => c.key)).toEqual(COUNTRY_CELL_KEYS);
      if (e.drivers) expect(checkDriversDoc(readJson(e.drivers), s), e.iso3).toEqual([]);
    }
  });

  it("quadrants split at the country's own median sector: the median of the plotted positions (2026-10-15)", () => {
    for (const e of m.countries.filter((c) => c.file)) {
      const s = readJson<FactSheet>(e.file!);
      const cm = countryMedianOf(s.cells);
      expect(s.summary.country_median.es.v, e.iso3).toBe(cm.es);
      expect(s.summary.country_median.eo.v, e.iso3).toBe(cm.eo);
      for (const c of s.cells.filter((x) => x.placed)) expect(c.quadrant).toBe(quadrantOf(c.es!.pos!, cm.es!, c.eo!.pos!, cm.eo!));
    }
  });

  it('a memo kept under the earlier quadrant rule needs its words and cannot be stale', () => {
    const mm = structuredClone(m) as AnyObj;
    const usa = mm.countries.find((c: AnyObj) => c.iso3 === 'USA');
    usa.memo.rule_note = 'quadrant_rule_2026_10_15';
    usa.memo.rule_note_d = 'Written on 14 October 2026, when the corners split each sector at the median economy.';
    expect(checkCountryManifest(mm)).toEqual([]);
    usa.memo.rule_note_d = null;
    expect(checkCountryManifest(mm).join(' | ')).toMatch(/rule_note_d/);
    usa.memo.rule_note_d = 'x';
    usa.memo.stale = true;
    expect(checkCountryManifest(mm).join(' | ')).toMatch(/stale memo/);
    usa.memo.stale = false;
    usa.memo.rule_note = 'nope';
    expect(checkCountryManifest(mm).join(' | ')).toMatch(/not a known rule note/);
  });

  it('the placeholder memo passes checkMemoDoc and carries the verbatim label', () => {
    const usa = m.countries.find((c) => c.iso3 === 'USA')!;
    expect(usa.memo).not.toBeNull();
    const doc = readJson(usa.memo!.json);
    expect(checkMemoDoc(doc, 'USA')).toEqual([]);
    expect((doc as AnyObj).label.startsWith(MEMO_LABEL_PREFIX)).toBe(true);
    expect((doc as AnyObj).placeholder).toBe(true);
  });

  it('display strings follow the formatter (A8.3)', () => {
    for (const e of m.countries.filter((c) => c.file)) {
      const s = readJson<FactSheet>(e.file!);
      for (const [path, f] of fvs(s)) {
        if (f.v === null) continue;
        if (/\.pct$/.test(path)) expect(f.d, path).toBe(`${pctText(f.v)}%`);
        else if (/\.si\.rank$|\.rank$|tie_rank$/.test(path) && Number.isInteger(f.v)) expect(f.d, path).toBe(ordinal(f.v));
        else if (/^cells\[\d+\]\.(es|eo)(\.med)?$/.test(path)) expect(f.d, path).toBe(f.v.toFixed(2));
        else if (/\.es\.order$/.test(path)) expect(f.d, path).toBe(f.v === 1 ? 'lowest' : `${ordinal(f.v)} lowest`);
        else if (/\.eo\.order$/.test(path)) expect(f.d, path).toBe(f.v === 1 ? 'highest' : `${ordinal(f.v)} highest`);
        expect(f.d, path).not.toMatch(/−/); // ASCII hyphen-minus only
      }
    }
  });

  describe('broken sheets shared with the R mirror', () => {
    const base = readSheet(CASES.base);
    it('the base sheet is valid', () => expect(checkFactSheet(base)).toEqual([]));
    for (const c of CASES.cases) {
      it(`${c.op}: ${c.what}`, () => {
        const problems = checkFactSheet(breakSheet(base, c.op));
        expect(problems.length).toBeGreaterThan(0);
        expect(problems.join(' | ')).toContain(c.keyword);
      });
    }
  });
});
