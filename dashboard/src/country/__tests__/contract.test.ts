// The Country contract on the R-written synthetic fixture (scripts/88 --synthetic): the manifest, every
// fact sheet, the companion drivers files and the placeholder memo pass the runtime checks; the broken
// sheets shared with the R mirror (tests/testthat/fixtures/country_broken_cases.json) each fail with the
// expected problem; every display string follows the formatter's rules (A8.3).

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { SUPPLY_STAGE_SLUGS, SUPPLY_STAGES } from '../../lib/stages.ts';
import {
  cellOf, checkCountryManifest, checkDriversDoc, checkFactSheet, checkMemoDoc, COUNTRY_BUDGET_GZ, COUNTRY_CELL_KEYS, COUNTRY_FILE_RE, COUNTRY_SCHEMA_VERSION, COUNTRY_STAGE_LABELS,
  COUNTRY_STAGES, COUNTRY_TECHS, countryMedianOf, isCellKey, MEMO_LABEL_PREFIX, MEMO_LABEL_PREFIX_V1, NO_ES_CELLS, noEsCellsOf, parseCellKey, quadrantOf, stageOfCellKey, type FactSheet, type Fv,
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
      // six stages (2026-10-28): the same key as tests/testthat/test-country-check.R
      s.cells[0].key = 'fusion-extraction';
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
      const g = s.cells.find((c: AnyObj) => c.key === 'geothermal-extraction');
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
    // design E3: the label names the NEIS baseline; the hidden schema-1 memos keep the label of their day, and only they may
    expect(MEMO_LABEL_PREFIX).toBe('AI-generated from the data and checked against it; NEIS baseline as of');
    const old = { ...(doc as AnyObj), label: `${MEMO_LABEL_PREFIX_V1} 12 October 2026.` };
    expect(checkMemoDoc(old, 'USA')).toEqual(['memo.label: not the verbatim label']);
    expect(checkMemoDoc({ ...old, schema_version: 1 }, 'USA')).toEqual([]);
    expect(checkMemoDoc({ ...(doc as AnyObj), schema_version: 1 }, 'USA')).toEqual(['memo.label: not the verbatim label']);
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

  describe('schema 2: the six supply-chain stages (2026-10-28)', () => {
    const usa = readSheet('USA');
    const problems = (edit: (s: AnyObj) => void) => {
      const s = structuredClone(usa) as AnyObj;
      edit(s);
      return checkFactSheet(s).join(' | ');
    };

    it('the stages and the 66 keys come from lib/stages (technology, then the stages in chain order)', () => {
      expect(COUNTRY_SCHEMA_VERSION).toBe(2);
      expect(COUNTRY_STAGES.map((s) => s.label)).toEqual([...SUPPLY_STAGES]);
      expect(COUNTRY_STAGES.map((s) => s.slug)).toEqual([...SUPPLY_STAGE_SLUGS]);
      expect(COUNTRY_STAGE_LABELS).toEqual([...SUPPLY_STAGES]);
      expect(COUNTRY_CELL_KEYS).toHaveLength(66);
      expect(COUNTRY_CELL_KEYS.slice(0, 6)).toEqual(['batteries-extraction', 'batteries-refining', 'batteries-components', 'batteries-assembly', 'batteries-deployment', 'batteries-recovery']);
      expect(COUNTRY_CELL_KEYS[65]).toBe('wind-recovery');
      // no technology slug ends in a stage slug, so parseCellKey's suffix match is exact (R tests the same)
      for (const t of COUNTRY_TECHS) for (const s of SUPPLY_STAGE_SLUGS) expect(t.slug.endsWith(`-${s}`)).toBe(false);
      for (const k of COUNTRY_CELL_KEYS) {
        const p = parseCellKey(k)!;
        expect(`${p.tech}-${p.stage}`).toBe(k);
        expect(stageOfCellKey(k)).toBe(COUNTRY_STAGES.find((s) => s.slug === p.stage)!.label);
      }
      for (const k of ['batteries-midstream', 'oil-upstream', 'solar-downstream', 'fusion-extraction']) {
        expect(isCellKey(k)).toBe(false);
        expect(stageOfCellKey(k)).toBeNull();
      }
    });

    it('the sectors without an Energy Security score are shipped (manifest and every sheet) and equal NO_ES_CELLS: at most 58 placed', () => {
      expect(NO_ES_CELLS).toEqual([
        'coal-recovery', 'electric-grid-deployment', 'gas-recovery', 'geothermal-extraction', 'geothermal-refining', 'geothermal-components', 'geothermal-assembly', 'geothermal-recovery',
      ]);
      expect(m.no_es_cells).toEqual(NO_ES_CELLS);
      for (const e of m.countries.filter((c) => c.file)) {
        const s = readJson<FactSheet>(e.file!);
        expect(s.no_es_cells, e.iso3).toEqual(NO_ES_CELLS);
        expect(noEsCellsOf(s)).toEqual(NO_ES_CELLS);
        for (const k of s.no_es_cells) expect(cellOf(s, k)?.es, `${e.iso3} ${k}`).toBeNull();
        expect(s.cells.filter((c) => c.placed).length).toBeLessThanOrEqual(COUNTRY_CELL_KEYS.length - NO_ES_CELLS.length);
      }
      expect(COUNTRY_CELL_KEYS.length - NO_ES_CELLS.length).toBe(58);
      // a sheet without the field falls back to the constant
      expect(noEsCellsOf({})).toBe(NO_ES_CELLS);
    });

    it('CM-FDI: Components and Assembly carry the same projects (manifest and every sheet)', () => {
      expect(m.cmfdi_stages).toEqual(['Components', 'Assembly']);
      for (const e of m.countries.filter((c) => c.file)) expect(readJson<FactSheet>(e.file!).meta.cmfdi_stages, e.iso3).toEqual(['Components', 'Assembly']);
    });

    it('every stage, key list and the coverage grid are six-stage; NIPO is not staged in the fixture (lists hidden, counts kept)', () => {
      for (const e of m.countries.filter((c) => c.file)) {
        const s = readJson<FactSheet>(e.file!);
        for (const c of s.cells) expect(c.stage).toBe(stageOfCellKey(c.key));
        expect(s.policy.coverage.cols).toEqual([...SUPPLY_STAGES]);
        for (const row of s.policy.coverage.specific) expect(row).toHaveLength(6);
        for (const k of s.partners.supplier_matrix.cells) expect(isCellKey(k)).toBe(true);
        if (s.policy.nipo?.covered) {
          expect(s.policy.nipo.staged, e.iso3).toBe(false);
          expect(s.policy.nipo.staged_d).toMatch(/six supply-chain stages/);
          expect(s.policy.nipo.policies).toEqual([]);
          expect(Object.keys(s.policy.nipo.cells)).toEqual([]);
        }
      }
    });

    it('checkFactSheet refuses a three-stage or inconsistent sheet', () => {
      expect(problems((s) => (s.schema_version = 1))).toMatch(/schema_version 1 is an export from before the six supply-chain stages/);
      expect(problems((s) => (s.cells[1].stage = 'Midstream'))).toMatch(/cells\[1\]\.stage: 'Midstream' is not a supply-chain stage/);
      expect(problems((s) => (s.cells[1].stage = 'Components'))).toMatch(/cells\[1\]\.stage: 'Components' is not the stage of the key batteries-refining/);
      expect(problems((s) => (s.policy.coverage.cols = ['Upstream', 'Midstream', 'Downstream']))).toMatch(/policy\.coverage\.cols: not the six supply-chain stages/);
      expect(problems((s) => (s.no_es_cells = ['geothermal-upstream']))).toMatch(/no_es_cells: 'geothermal-upstream' is not a sector key/);
      expect(problems((s) => delete s.no_es_cells)).toMatch(/no_es_cells: not an array/);
      expect(problems((s) => (s.meta.cmfdi_stages = ['Midstream']))).toMatch(/meta\.cmfdi_stages: not supply-chain stages/);
      expect(problems((s) => (s.partners.supplier_matrix.cells[0] = 'solar-midstream'))).toMatch(/supplier_matrix\.cells: 'solar-midstream' is not a sector key/);
      expect(problems((s) => (s.pick = { ...(s.pick ?? { si: { v: 0.5, d: '0.50' }, margin: { v: 0, d: '0' }, near_tie: false }), cell: 'oil-upstream', runner_up: null }))).toMatch(/pick\.cell: 'oil-upstream' is not a sector key/);
      expect(problems((s) => (s.policy.nipo.policies = [{ id: 1 }]))).toMatch(/per-sector measures listed although the run is not at the six stages/);
      const mm = structuredClone(m) as AnyObj;
      mm.schema_version = 1;
      expect(checkCountryManifest(mm).join(' | ')).toMatch(/schema_version 1 is an export from before the six supply-chain stages/);
      mm.schema_version = 2;
      mm.no_es_cells = ['electric-grid-downstream'];
      mm.cmfdi_stages = ['Midstream'];
      expect(checkCountryManifest(mm).join(' | ')).toMatch(/manifest\.no_es_cells: 'electric-grid-downstream' is not a sector key.*manifest\.cmfdi_stages: not supply-chain stages/);
    });

    it('the gzip budgets equal R COUNTRY_BUDGET_GZ (six-stage export: measured + ~15%)', () => {
      const r = readFileSync(resolve(process.cwd(), '../R/dashboard/country/country_check.R'), 'utf8');
      const line = /COUNTRY_BUDGET_GZ <- c\(([^)]*)\)/.exec(r)![1];
      const rb = Object.fromEntries(line.split(',').map((x) => x.trim().split(/\s*=\s*/)).map(([k, v]) => [k, Number(v)]));
      expect(COUNTRY_BUDGET_GZ.sheet).toBe(rb.sheet);
      expect(COUNTRY_BUDGET_GZ.manifest).toBe(rb.manifest);
      expect(COUNTRY_BUDGET_GZ.tradeMonthly).toBe(rb.trade_monthly);
      expect(COUNTRY_BUDGET_GZ).toMatchObject({ sheet: 60_416, manifest: 21_504, tradeMonthly: 25_600 });
    });
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
