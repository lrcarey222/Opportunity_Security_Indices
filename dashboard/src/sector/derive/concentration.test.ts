import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureGeo, fixtureItem, fixtureVar } from '../__fixtures__/build.ts';
import { refSizes, validateChunk, type SectorTradeChunk } from '../contract.ts';
import { haveSynthetic, readSyntheticChunk, readSyntheticFile, readSyntheticManifest } from '../__fixtures__/__tests__/synthetic.ts';
import { factsIndexOf, type FactRow } from '../data/facts.ts';
import { modelOf, type GeoInfo } from '../data/model.ts';
import { basisWords, concentration, growth, growthKindWords, growthWords, isWorldBasis } from './concentration.ts';
import { summaryFromFacts, summaryParity } from './summary.ts';

const geo = (idx: number, group: GeoInfo['group'], kind: GeoInfo['kind'] = 'country'): GeoInfo => ({
  idx, code: `G${idx}`, name: `G${idx}`, kind, iso_n3: null, tier: group === 'core' ? 'core' : group === 'partner' ? 'partner' : 'none', group, eu_member: false, re_export_hub: false, bloc: null, wb_region: null, income: null, reason: null,
});
const GEOS: GeoInfo[] = [geo(0, 'china'), geo(1, 'core'), geo(2, 'partner'), geo(3, 'rest'), geo(4, 'na', 'residual'), geo(5, 'na', 'world')];
const row = (g: number, v: number): FactRow => ({ geo: g, value: v, status: 'obs', obs_year: null });

describe('concentration (3.6) on hand-computed cases', () => {
  it('shares, HHI, top-1, top-3, effective n and the group split', () => {
    const c = concentration([row(0, 50), row(1, 30), row(2, 15), row(3, 5)], GEOS, null, 'sum_all');
    expect(c.W).toBe(100);
    expect(c.n).toBe(4);
    expect(c.top1).toEqual({ geo: 0, share: 0.5 });
    expect(c.top3_share).toBeCloseTo(0.95, 15);
    expect(c.hhi).toBeCloseTo(0.25 + 0.09 + 0.0225 + 0.0025, 15);
    expect(c.eff_n).toBeCloseTo(1 / 0.365, 12);
    expect(c.split).toEqual({ china: 0.5, core: 0.3, partner: 0.15, rest: 0.05, unattributed: 0 });
    expect(c.hhi_lower_bound).toBe(false);
    expect(c.rows.map((r) => r.rank)).toEqual([1, 2, 3, 4]);
  });

  it('a source world total leaves an unattributed residual; HHI is a lower bound above 5%', () => {
    const c = concentration([row(0, 60), row(1, 20)], GEOS, { value: 100 }, 'source_world');
    expect(c.split.unattributed).toBeCloseTo(0.2, 15);
    expect(c.hhi).toBeCloseTo(0.4, 15);
    expect(c.hhi_lower_bound).toBe(true);
    const d = concentration([row(0, 60), row(1, 36)], GEOS, { value: 100 }, 'source_world');
    expect(d.hhi_lower_bound).toBe(false);
  });

  it('residual rows count in W and the unattributed band, never as holders', () => {
    const c = concentration([row(0, 60), row(4, 40)], GEOS, null, 'sum_listed');
    expect(c.W).toBe(100);
    expect(c.n).toBe(1);
    expect(c.split.unattributed).toBeCloseTo(0.4, 15);
    expect(c.rows.map((r) => r.geo)).toEqual([0]);
  });

  it('a mixed region (group na) is a ranked holder whose share is not attributed to a group', () => {
    const regions: GeoInfo[] = [geo(0, 'china', 'region'), geo(1, 'core', 'region'), geo(2, 'na', 'region'), geo(3, 'na', 'world')];
    const c = concentration([row(0, 70), row(1, 20), row(2, 10)], regions, null, 'sum_listed');
    expect(c.W).toBe(100);
    expect(c.rows.map((r) => r.geo)).toEqual([0, 1, 2]);
    expect(c.n).toBe(3);
    expect(c.split.china).toBeCloseTo(0.7, 15);
    expect(c.split.core).toBeCloseTo(0.2, 15);
    expect(c.split.partner).toBe(0);
    expect(c.split.unattributed).toBeCloseTo(0.1, 15);
    expect(c.hhi_lower_bound).toBe(false);
  });

  it('rounding never makes the unattributed band negative', () => {
    const c = concentration([row(0, 0.1), row(1, 0.2), row(2, 0.7000000000000001)], GEOS, { value: 1 }, 'one');
    expect(c.split.unattributed).toBeGreaterThanOrEqual(0);
  });

  it('ties share a rank; zeros are listed but not counted in n', () => {
    const c = concentration([row(0, 10), row(1, 10), row(2, 0)], GEOS, null, 'sum_all');
    expect(c.rows.map((r) => r.rank)).toEqual([1, 1, 3]);
    expect(c.n).toBe(2);
  });

  it('basis none gives no shares', () => {
    const c = concentration([row(0, 10)], GEOS, null, 'none');
    expect(c.W).toBeNull();
    expect(c.rows[0].share).toBeNull();
    expect(Number.isNaN(c.hhi)).toBe(true);
  });

  it('basis words never call a listed share a world share', () => {
    expect(basisWords('sum_listed', 'IEA')).toBe('of the economies listed by IEA');
    expect(basisWords('source_world', 'EI')).toBe('of world');
    expect(isWorldBasis('sum_listed')).toBe(false);
    expect(isWorldBasis('sum_all')).toBe(true);
  });

  it('growth: percent and CAGR; zero or tiny bases read "new"', () => {
    expect(growth('pct_2020_2024', 99.3, 202.7).value).toBeCloseTo(202.7 / 99.3 - 1, 12);
    expect(growth('cagr_2018_2025', 10, 20).value).toBeCloseTo(Math.pow(2, 1 / 7) - 1, 12);
    expect(growth('pct_2020_2024', 0, 5)).toEqual({ value: null, isNew: true });
    expect(growth('pct_2020_2024', 0.001, 5)).toEqual({ value: null, isNew: true });
    expect(growthWords(growth('pct_2020_2024', 0, 5))).toBe('new');
    expect(growthWords(growth('pct_2020_2024', 100, 204))).toBe('+104.0%');
    expect(growthKindWords('cagr_2018_2025')).toBe('a year on average, 2018-2025');
    expect(growthKindWords('pct_2020_2024')).toBe('change 2020-2024');
    expect(growthKindWords(null)).toBe('');
  });
});

describe('client summary = meta.summary (fixture)', () => {
  const fx = buildSectorFixture();
  const meta = validateChunk('sector-meta', fx.chunks['sector-meta']);
  const model = modelOf(meta);
  const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
  const trades: Record<string, SectorTradeChunk> = {};
  for (const n of Object.keys(fx.chunks)) if (n.startsWith('sector-trade-')) trades[n] = validateChunk(n as 'sector-trade-batteries', fx.chunks[n]);
  const tradeOf = (item: number) => {
    const it = model.items[item];
    return it?.techSlug ? (trades[`sector-trade-${it.techSlug}`] ?? null) : null;
  };

  it('every summary row is reproduced within 1e-12', () => {
    expect(model.summary.length).toBeGreaterThan(10);
    expect(summaryParity(model, facts, tradeOf)).toEqual([]);
  });

  it('batteries midstream: matrix W, China share and bloc mode remove intra-EU trade', () => {
    const bm = fixtureItem('cell', 'batteries-midstream');
    const v = model.varById.get('trade.exports')!;
    const m = summaryFromFacts(model, facts, v, bm, 2024, 'members', tradeOf(bm));
    const b = summaryFromFacts(model, facts, v, bm, 2024, 'bloc', tradeOf(bm));
    expect(m.top1_geo).toBe(fixtureGeo('CHN'));
    // DEU->FRA 2.5, FRA->DEU 1.2 and CYP->DEU 0.02 bn are intra-EU
    expect(m.W - b.W).toBeCloseTo((2.5 + 1.2 + 0.02) * 1e9, 0);
    expect(b.china).toBeGreaterThan(m.china);
    expect(m.growth_kind).toBe('pct_2020_2024');
    expect(Number.isFinite(m.growth)).toBe(true);
    expect(fixtureVar('trade.exports')).toBe(v.idx);
  });
});

describe.skipIf(!haveSynthetic())('client summary = meta.summary (R synthetic bundle, scripts/87 --synthetic)', () => {
  it('every summary row of the R bundle is reproduced within 1e-12', () => {
    const manifest = readSyntheticManifest();
    const meta = validateChunk('sector-meta', readSyntheticChunk('sector-meta')!);
    const ref = refSizes(meta);
    const model = modelOf(meta);
    const facts = factsIndexOf(validateChunk('sector-facts', readSyntheticChunk('sector-facts')!, ref));
    const trades = new Map<string, SectorTradeChunk>();
    for (const c of manifest.chunks) if (c.name.startsWith('sector-trade-')) trades.set(c.name, validateChunk(c.name as 'sector-trade-batteries', readSyntheticFile(c.file), ref));
    const tradeOf = (item: number) => {
      const it = model.items[item];
      return it?.techSlug ? (trades.get(`sector-trade-${it.techSlug}`) ?? null) : null;
    };
    expect(model.summary.length).toBeGreaterThan(10);
    expect(summaryParity(model, facts, tradeOf)).toEqual([]);
  });
});
