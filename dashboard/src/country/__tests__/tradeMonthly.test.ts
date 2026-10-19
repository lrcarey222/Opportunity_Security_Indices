// The companion monthly trade files (additive 2026-10-19): the R-written fixture docs pass the contract check,
// broken docs fail with the same keywords as the R mirror (tests/testthat/test-country-trade-monthly.R),
// and the card's pure derives (cell pick, lines, notes, rows).

import { describe, expect, it } from 'vitest';
import { checkCountryManifest, checkTradeMonthlyDoc, COUNTRY_FILE_RE, indexTo100, type TradeMonthlyDoc } from '../contract.ts';
import { fmtUsd, monthLabel, monthTick, pickTradeCell, SUB_LINES, TOTAL_ID, tradeLines, tradeRows, unreportedMonths } from '../derive/tradeMonthly.ts';
import { haveFixture, readJson, readManifest } from './fixture.ts';

type AnyObj = Record<string, any>; // eslint-disable-line @typescript-eslint/no-explicit-any

describe.skipIf(!haveFixture())('monthly trade files (fixture)', () => {
  const m = readManifest();
  const withTrade = m.countries.filter((c) => c.trade_monthly);
  const doc = () => readJson<TradeMonthlyDoc>(withTrade.find((c) => c.iso3 === 'USA')!.trade_monthly!);

  it('the fixture lists monthly files for core members only, with valid names, and every doc passes', () => {
    expect(withTrade.map((c) => c.iso3).sort()).toEqual(['DEU', 'USA']);
    expect(checkCountryManifest(m)).toEqual([]);
    for (const c of withTrade) {
      expect(c.tier).toBe('core');
      expect(COUNTRY_FILE_RE.test(c.trade_monthly!)).toBe(true);
      expect(checkTradeMonthlyDoc(readJson(c.trade_monthly!), c.iso3), c.iso3).toEqual([]);
    }
  });

  it('broken docs fail with the R keywords', () => {
    const cases: [string, (d: AnyObj) => void, string][] = [
      ['declared', (d) => delete d.window.declared, 'expected 12 true/false'],
      ['name', (d) => (d.cells[0].subs[0].name = ''), 'without a name'],
      ['schema_version', (d) => (d.schema_version = 2), 'needs a newer app'],
      ['string value', (d) => (d.cells[0].subs[0].imports[0] = '5'), 'length'],
      ['total', (d) => (d.cells[0].total.imports[2] = 1), 'do not sum'],
      ['cell key', (d) => (d.cells[0].key = 'batteries-sideways'), 'unknown cell key'],
      ['months', (d) => d.window.months.shift(), 'expected 12 months'],
      ['iso3', (d) => (d.iso3 = 'DEU'), 'is not USA'],
      ['negative', (d) => (d.cells[0].subs[0].exports[1] = -5), 'negative value'],
    ];
    for (const [what, op, keyword] of cases) {
      const d = structuredClone(doc()) as AnyObj;
      op(d);
      expect(checkTradeMonthlyDoc(d, 'USA').join(' | '), what).toContain(keyword);
    }
    const bad = structuredClone(m) as AnyObj;
    bad.countries.find((c: AnyObj) => c.iso3 === 'USA').trade_monthly = 'trade-USA.json';
    expect(checkCountryManifest(bad).join(' | ')).toContain('not a monthly trade file name');
  });

  it('the selected sector is shown when the file has it, else the sector with the most trade', () => {
    const d = doc();
    expect(pickTradeCell(d, 'solar-midstream')).toEqual({ cell: d.cells.find((c) => c.key === 'solar-midstream'), fallback: false });
    const fb = pickTradeCell(d, 'oil-upstream');
    expect(fb.fallback).toBe(true);
    expect(fb.cell.key).toBe('batteries-midstream');
    expect(pickTradeCell(d, null).fallback).toBe(false);
  });

  it('lines: sub-components largest first, at most six, each indexed to its first month, plus the total', () => {
    const c = doc().cells.find((x) => x.key === 'batteries-midstream')!;
    const l = tradeLines(c, 'imports');
    const subs = l.lines.filter((x) => x.id !== TOTAL_ID);
    expect(subs.length).toBeLessThanOrEqual(SUB_LINES);
    const totals = subs.map((x) => x.points.reduce((s, p) => s + (Number.isFinite(p.y) ? p.y : 0), 0));
    expect([...totals].sort((a, b) => b - a)).toEqual(totals);
    expect(l.lines[l.lines.length - 1].id).toBe(TOTAL_ID);
    expect(new Set(subs.map((x) => x.color)).size).toBe(subs.length);
    for (const s of c.subs) expect([null, 100]).toContain(indexTo100(s.imports)[0]);
    expect(tradeRows(doc(), c, 'imports').filter((r) => r.month === doc().window.first && r.index_first_month_100 !== null).every((r) => r.index_first_month_100 === 100)).toBe(true);
  });

  it('a month the economy did not report is a gap and a note (Germany, last month of exports)', () => {
    const de = readJson<TradeMonthlyDoc>(withTrade.find((c) => c.iso3 === 'DEU')!.trade_monthly!);
    expect(unreportedMonths(de, 'exports')).toEqual([monthLabel(de.window.last)]);
    expect(unreportedMonths(de, 'imports')).toEqual([]);
    const c = de.cells[0];
    expect(c.total.exports[11]).toBeNull();
    const rows = tradeRows(de, c, 'exports');
    expect(rows.length).toBe((c.subs.length + 1) * 12);
    expect(rows.filter((r) => r.month === de.window.last).every((r) => r.value_usd === null)).toBe(true);
  });
});

describe('monthly trade helpers', () => {
  it('index to the first value; no base, no index', () => {
    expect(indexTo100([50, 100, null])).toEqual([100, 200, null]);
    expect(indexTo100([0, 5])).toEqual([null, null]);
    expect(indexTo100([null, 5])).toEqual([null, null]);
  });
  it('month labels, ticks and USD', () => {
    expect(monthLabel('2025-08')).toBe('Aug 2025');
    const ms = ['2025-08', '2025-09', '2025-10', '2025-11', '2025-12', '2026-01'];
    expect(ms.map((_, i) => monthTick(ms, i))).toEqual(["Aug '25", 'Sep', 'Oct', 'Nov', 'Dec', "Jan '26"]);
    expect(monthTick(ms, 0.5)).toBe('');
    expect(monthTick(ms, 9)).toBe('');
    expect(fmtUsd(1.234e9)).toBe('USD 1.23 bn');
    expect(fmtUsd(4.56e8)).toBe('USD 456 m');
    expect(fmtUsd(1.2e5)).toBe('USD 0.12 m');
    expect(fmtUsd(null)).toBe('–');
  });
});
