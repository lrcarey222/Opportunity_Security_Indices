// Sector Insights on the REAL bundle (spec_sector.md 10.4; integrator). Vitest project `sector-real`
// (`npm run test:sector-real`); every test skips when public/data/sector/manifest.json is absent (CI
// has no real data). Run it after scripts/87 has written the bundle.
//
//   * Every Appendix B reference value (the six-stage export of 2026-10-28: scripts/87's Appendix B log,
//     scratchpad map6/r-sector-export/appendix_b_real.txt) is reproduced by the TS derivations from the
//     decoded real chunks, at the precision the app shows it.
//   * meta.summary = the client recomputation for every item x var x year x EU mode.
//   * Every chunk passes validateChunk(); the headlines carry exactly the HEADLINE_RULES keys and
//     agree with the derivations.
//   * manifest.validation has no FAIL; its WARNs are listed in the test output.
//   * Sector data <= 9 MB gz; Strategic + Sector + Compacts on disk <= 32.5 MiB (R's own check, Sector chunks +
//     Strategic all_gz, is 26.5 MiB).

import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { beforeAll, describe, expect, it } from 'vitest';
import { decodeOsb, isGzip, type OsbDecoded } from '../../engine/codec.ts';
import {
  checkChunk,
  HEADLINE_RULES,
  refSizes,
  SECTOR_BUDGET_GZ,
  sectorManifestProblems,
  validateChunk,
  type HeadlineId,
  type SectorChunkName,
  type SectorClimateChunk,
  type SectorDevChunk,
  type SectorFactsChunk,
  type SectorManifest,
  type SectorMetaChunk,
  type SectorPolicyChunk,
  type SectorRefSizes,
  type SectorSeriesChunk,
  type SectorTradeChunk,
  type SectorClimateSrcChunk,
  type SectorFdiChunk,
  TECH_LABELS,
} from '../contract.ts';
import { factsIndexOf, seriesIndexOf, type FactsIndex } from '../data/facts.ts';
import { modelOf, type ItemInfo, type SectorModel, type VarInfo } from '../data/model.ts';
import { countryFamilies, countryVars, primaryMeasure } from '../derive/cells.ts';
import { fdiData, fdiItemOf, filterProjects } from '../derive/fdi.ts';
import { mineralStageItems } from '../derive/minerals.ts';
import { isPairCopy, SAME_FIGURE_NOTE, sameFigureText, splitText } from '../derive/pairs.ts';
import { subComponents } from '../derive/subs.ts';
import { mineralPaths, nzeCell, nzeCompare, nzeHeadline, nzeWorldLine } from '../derive/nze.ts';
import { cellMatch, climateData, emitters, fmtLifecycle, intensityKeys, intensityOf } from '../derive/climate.ts';
import { climateSrcData, eiRows, fuelRows, gcpCountry, gcpHeadline, nze21For, srcCell, weiSummary, weoRegionOf } from '../derive/sources.ts';
import { growth } from '../derive/concentration.ts';
import { ASSUMED_ZERO_WORDS, isAssumedZero, noteWords } from '../derive/notes.ts';
import { countryTotals, excludedCodes, flowsOf, league, matrixTotal } from '../derive/trade.ts';
import { summaryFromFacts, summaryParity } from '../derive/summary.ts';

const DATA_DIR = resolve(process.cwd(), 'public/data');
/** The parity rows of the climate sources (sector_src_parity()). */
const SRC_PARITY = /^climate \(decoded\): (GCP|EI |WEO regional|WEO regions|economy -> WEO|NZE 2021|WEI|climate sources)/;
const SECTOR_DIR = join(DATA_DIR, 'sector');
const HAVE = existsSync(join(SECTOR_DIR, 'manifest.json'));

function readChunkFile(file: string): OsbDecoded {
  let b: Uint8Array = readFileSync(join(SECTOR_DIR, file));
  if (isGzip(b)) b = gunzipSync(b);
  const out = new Uint8Array(b.length);
  out.set(b);
  return decodeOsb(out);
}

/** Bytes on disk under a directory (the .osb files are stored gzipped, as served). */
/** Bytes on disk under `dir`; `skip` names top-level folders left out (the Country Insights bundle has its own budget, A10). */
function dirBytes(dir: string, recurse: boolean, skip: readonly string[] = []): number {
  let n = 0;
  for (const f of readdirSync(dir)) {
    const p = join(dir, f);
    const s = statSync(p);
    if (s.isDirectory()) {
      if (recurse && !skip.includes(f)) n += dirBytes(p, true);
    } else n += s.size;
  }
  return n;
}

const r1 = (x: number) => Math.round(x * 10) / 10;
const r2 = (x: number) => Math.round(x * 100) / 100;
const pct1 = (x: number) => Math.round(x * 1000) / 10;
const r3 = (x: number) => Math.round(x * 1000) / 1000;

describe.skipIf(!HAVE)('Sector real bundle (public/data/sector)', () => {
  let manifest: SectorManifest;
  let meta: SectorMetaChunk;
  let ref: SectorRefSizes;
  let model: SectorModel;
  let facts: FactsIndex;
  let series: SectorSeriesChunk;
  let policy: SectorPolicyChunk;
  const trades = new Map<string, SectorTradeChunk>();
  const tradeOf = (item: number): SectorTradeChunk | null => {
    const it = model.items[item];
    return it?.techSlug ? (trades.get(`sector-trade-${it.techSlug}`) ?? null) : null;
  };
  const V = (id: string): VarInfo => {
    const v = model.varById.get(id);
    if (!v) throw new Error(`no var ${id}`);
    return v;
  };
  const I = (axis: string, slug: string): ItemInfo => {
    const it = model.item(axis as ItemInfo['axis'], slug);
    if (!it) throw new Error(`no item ${axis}:${slug}`);
    return it;
  };
  const G = (code: string): number => {
    const g = model.geoByCode.get(code);
    if (!g) throw new Error(`no geo ${code}`);
    return g.idx;
  };
  const S = (varId: string, axis: string, slug: string, year: number, eu: 'members' | 'bloc' = 'members') => {
    const it = I(axis, slug);
    return summaryFromFacts(model, facts, V(varId), it.idx, year, eu, tradeOf(it.idx));
  };

  beforeAll(() => {
    manifest = JSON.parse(readFileSync(join(SECTOR_DIR, 'manifest.json'), 'utf8')) as SectorManifest;
    const file = (n: SectorChunkName) => {
      const c = manifest.chunks.find((x) => x.name === n);
      if (!c) throw new Error(`manifest has no ${n}`);
      return c.file;
    };
    meta = validateChunk('sector-meta', readChunkFile(file('sector-meta')));
    ref = refSizes(meta);
    model = modelOf(meta);
    facts = factsIndexOf(validateChunk('sector-facts', readChunkFile(file('sector-facts')), ref) as SectorFactsChunk);
    series = validateChunk('sector-series', readChunkFile(file('sector-series')), ref) as SectorSeriesChunk;
    policy = validateChunk('sector-policy', readChunkFile(file('sector-policy')), ref) as SectorPolicyChunk;
    for (const c of manifest.chunks) if (c.name.startsWith('sector-trade-')) trades.set(c.name, validateChunk(c.name as 'sector-trade-batteries', readChunkFile(c.file), ref) as SectorTradeChunk);
  });

  describe('manifest, contract and budgets', () => {
    it('the manifest conforms; restricted data never ship; phase 1 + series', () => {
      expect(sectorManifestProblems(manifest)).toEqual([]);
      expect(manifest.flags.include_restricted).toBe(false);
      expect(manifest.phase).toBe(1);
      expect(manifest.flags.series).toBe(true);
      expect(manifest.flags.trade).toBe(true);
      expect(manifest.flags.policy_text).toBe(true);
      expect(manifest.encoding.lossy).toBe(false);
      // CM-FDI (2026-10-11): the lazy sector-fdi chunk ships with flags.fdi (an export with --no-fdi has neither)
      expect(manifest.flags.fdi).toBe(true);
      // Development & finance (2026-10-15): the lazy sector-dev chunk ships with flags.dev (--no-dev has neither)
      expect(manifest.flags.dev).toBe(true);
      // Climate TRACE emissions (2026-10-17): the lazy sector-climate chunk ships with flags.climate (--no-climate has neither)
      expect(manifest.flags.climate).toBe(true);
      // 18 chunks: meta, facts, 10 trade, policy, policy-text, series, fdi, dev, climate; 19 with the climate
      // sources (sector-climate-src, flags.climate_src, 2026-10-17; --no-climate-sources has neither)
      expect(manifest.chunks.length).toBe(manifest.flags.climate_src ? 19 : 18);
      expect(manifest.chunks.filter((c) => c.name === 'sector-climate-src').length).toBe(manifest.flags.climate_src ? 1 : 0);
      expect(manifest.chunks.filter((c) => c.name === 'sector-climate').length).toBe(1);
      expect(manifest.chunks.filter((c) => c.name === 'sector-dev').length).toBe(1);
      expect(manifest.chunks.filter((c) => c.name.startsWith('sector-trade-')).length).toBe(10);
      expect(manifest.chunks.filter((c) => c.name === 'sector-fdi').length).toBe(1);
      for (const s of manifest.sources) expect(s.path).not.toMatch(/^([a-zA-Z]:|[\\/]|~)/);
    });

    it('every chunk passes checkChunk() with 0 errors and 0 warnings', () => {
      for (const c of manifest.chunks) {
        const r = checkChunk(c.name, readChunkFile(c.file), c.name === 'sector-meta' ? undefined : ref);
        expect({ chunk: c.name, errors: r.errors, warnings: r.warnings }).toEqual({ chunk: c.name, errors: [], warnings: [] });
      }
    });

    it('validation has no FAIL; the WARNs are listed', () => {
      const v = manifest.validation as { check: string; status: string; n: number; detail: string | null }[];
      expect(v.filter((x) => x.status === 'FAIL')).toEqual([]);
      const warns = v.filter((x) => x.status === 'WARN');
      console.info(`manifest.validation: ${v.filter((x) => x.status === 'PASS').length} PASS, ${warns.length} WARN, 0 FAIL`);
      for (const w of warns) console.info(`  WARN ${w.check}${w.detail ? ` -- ${w.detail}` : ''}`);
      // the two expected WARNs of the six-stage run (EI hydro 1972 residual; the trade bases of 14 cells whose matrix and
      // exporter-reported China shares differ by more than 3 points), plus Climate TRACE's carried-forward latest
      // year (2026-10-17; shown on the pages, caveat climate_trace_repeat)
      expect(warns.filter((w) => !w.check.startsWith('climate: the latest year repeats') && !w.check.startsWith('climate: IEA pathways vintage check')).length).toBeLessThanOrEqual(2);
      // and the IEA pathways' vintage check against the previous WEO edition (2026-10-17; editions differ by design)
      expect(warns.filter((w) => w.check.startsWith('climate:') && !w.check.startsWith('climate: IEA pathways vintage check')).length).toBeLessThanOrEqual(1);
      expect(warns.filter((w) => w.check.startsWith('climate: IEA pathways vintage check')).length).toBeLessThanOrEqual(1);
    });

    // combined_total: 32.5 MiB since 2026-10-28 (the six-stage Strategic and Sector exports: 28.16 MiB measured on disk, + ~15%)
    it('budgets: each chunk, sector <= 9 MB gz, Strategic + Sector + Compacts <= 32.5 MiB on disk', () => {
      for (const c of manifest.chunks) {
        const key = (c.name.startsWith('sector-trade-') ? 'sector-trade' : c.name) as keyof typeof SECTOR_BUDGET_GZ;
        expect(c.bytes_gz, c.name).toBeLessThanOrEqual(SECTOR_BUDGET_GZ[key]);
        expect(statSync(join(SECTOR_DIR, c.file)).size, c.name).toBe(c.bytes_gz);
      }
      const sector = dirBytes(SECTOR_DIR, false);
      // Strategic + Sector + Compacts only: public/data/country (Country Insights fact sheets and memos) and
      // public/data/posture (the core plus one drawer per economy, loaded on demand) are budgeted separately
      const all = dirBytes(DATA_DIR, true, ['country', 'posture']);
      console.info(`data on disk (gz): sector ${(sector / 2 ** 20).toFixed(2)} MB, Strategic + Sector + Compacts ${(all / 2 ** 20).toFixed(2)} MB`);
      expect(sector).toBeLessThanOrEqual(SECTOR_BUDGET_GZ.sector_total);
      expect(all).toBeLessThanOrEqual(SECTOR_BUDGET_GZ.combined_total);
      expect(manifest.budget.sector_gz).toBeLessThanOrEqual(manifest.budget.limit_sector_gz);
    });

    it('headlines: one per rule, exactly the HEADLINE_RULES keys', () => {
      expect(manifest.headlines.map((h) => h.id).sort()).toEqual(Object.keys(HEADLINE_RULES).sort());
      for (const h of manifest.headlines) {
        const rule = HEADLINE_RULES[h.id as HeadlineId];
        expect(h.template).toBe(rule.rule);
        expect(Object.keys(h.values).sort()).toEqual(Object.keys(rule.values).sort());
        expect(Object.keys(h.labels).sort()).toEqual(Object.keys(rule.labels).sort());
        expect(h.href.startsWith('/sector')).toBe(true);
      }
    });

    it('the coverage glyph: 66 cells (11 technologies x 6 stages), 53 with country data', () => {
      expect(manifest.coverage_glyph.length).toBe(66);
      expect(manifest.coverage_glyph[0]).toMatchObject({ tech: 'batteries', stage: 'extraction' });
      expect(manifest.coverage_glyph[65]).toMatchObject({ tech: 'wind', stage: 'recovery' });
      const empty = manifest.coverage_glyph.filter((g) => !g.country_data).map((g) => `${g.tech}-${g.stage}`).sort();
      expect(empty).toEqual([
        'coal-recovery', 'electric-grid-deployment', 'gas-recovery', 'geothermal-assembly', 'geothermal-components', 'geothermal-extraction', 'geothermal-recovery',
        'geothermal-refining', 'green-hydrogen-assembly', 'green-hydrogen-components', 'green-hydrogen-deployment', 'green-hydrogen-recovery', 'green-hydrogen-refining',
      ]);
      // the dictionary's cells: the 66 headline cells and the three Semiconductors stages with trade
      expect(model.cells).toHaveLength(69);
      expect(model.cells.filter((c) => !c.headline).map((c) => c.slug)).toEqual(['semiconductors-refining', 'semiconductors-components', 'semiconductors-assembly']);
      // no cell, label or stage is three-stage
      for (const c of model.cells) expect(`${c.slug} ${c.label} ${c.stage}`).not.toMatch(/upstream|midstream|downstream/i);
    });
  });

  describe('meta.summary = the client recomputation (3.6)', () => {
    it('every summary row (item x var x year x EU mode) within 1e-12', () => {
      expect(model.summary.length).toBeGreaterThan(500);
      const modes = new Set(model.summary.map((s) => s.eu_mode));
      expect([...modes].sort()).toEqual(['bloc', 'members']);
      expect(summaryParity(model, facts, tradeOf)).toEqual([]);
    });
  });

  describe('Appendix B reference values (the six-stage export of 2026-10-28, scripts/87; map6/r-sector-export/appendix_b_real.txt)', () => {
    it('Batteries — Refining, matrix 2024', () => {
      const s = S('trade.exports', 'cell', 'batteries-refining', 2024);
      expect(r1(s.W / 1e9)).toBe(74.5);
      expect(s.n).toBe(184);
      expect(s.top1_geo).toBe(G('CHN'));
      expect(pct1(s.china)).toBe(18.7);
      expect(pct1(s.top3_share)).toBe(36.8);
      expect(r3(s.hhi)).toBe(0.072);
      expect(r1(s.eff_n)).toBe(13.8);
      expect(pct1(s.core)).toBe(42.1);
      expect(pct1(s.partner)).toBe(30.5);
      expect(pct1(s.rest)).toBe(8.6);
      expect(s.unattributed).toBe(0);
      expect(s.growth_kind).toBe('pct_2020_2024');
      expect(Math.round(s.growth * 100)).toBe(41);
      const s0 = S('trade.exports', 'cell', 'batteries-refining', 2020);
      expect(r1(s0.W / 1e9)).toBe(52.7);
      const usa = s.conc.rows.find((r) => r.geo === G('USA'))!;
      expect(pct1(usa.share!)).toBe(3.6);
      expect(usa.rank).toBe(9);
      expect(pct1(s.conc.rows.find((r) => r.geo === G('HKG'))!.share!)).toBe(0.2);
      // every coalition member (31 core + 29 partners) exports
      const coalition = model.geos.filter((g) => g.kind === 'country' && (g.tier === 'core' || g.tier === 'partner'));
      expect(coalition.length).toBe(60);
      expect(coalition.filter((g) => g.tier === 'core').length).toBe(31);
      expect(coalition.filter((g) => (s.conc.rows.find((r) => r.geo === g.idx)?.value ?? 0) > 0).length).toBe(60);
    });

    it('Batteries — Assembly, matrix 2024', () => {
      const s = S('trade.exports', 'cell', 'batteries-assembly', 2024);
      expect([r1(s.W / 1e9), s.n, model.geos[s.top1_geo!].code, pct1(s.china), pct1(s.top3_share), r3(s.hhi), r1(s.eff_n)]).toEqual([117.7, 183, 'CHN', 60.1, 72.0, 0.376, 2.7]);
      expect([pct1(s.core), pct1(s.partner), pct1(s.rest), s.unattributed, Math.round(s.growth * 100)]).toEqual([34.0, 4.4, 1.4, 0, 189]);
      expect(r1(S('trade.exports', 'cell', 'batteries-assembly', 2020).W / 1e9)).toBe(40.8);
      const usa = s.conc.rows.find((r) => r.geo === G('USA'))!;
      expect([pct1(usa.share!), usa.rank]).toEqual([5.8, 3]);
      expect(pct1(s.conc.rows.find((r) => r.geo === G('HKG'))!.share!)).toBe(0.5);
      const coalition = model.geos.filter((g) => g.kind === 'country' && (g.tier === 'core' || g.tier === 'partner'));
      expect(coalition.filter((g) => (s.conc.rows.find((r) => r.geo === g.idx)?.value ?? 0) > 0).length).toBe(60);
    });

    it('top exporters 2024 from the trade chunk = facts (Batteries — Refining and — Assembly)', () => {
      for (const [slug, W, want] of [
        ['batteries-refining', 74.5, ['CHN 13.97', 'IDN 9.32', 'KOR 4.14', 'JPN 4.13', 'CAN 3.61', 'COD 3.34', 'CHL 3.30', 'DEU 2.99', 'USA 2.69', 'AUS 2.35']],
        ['batteries-assembly', 117.7, ['CHN 70.77', 'POL 7.16', 'USA 6.77', 'HUN 6.51', 'KOR 4.70', 'JPN 4.04', 'DEU 3.74', 'CZE 2.09', 'MYS 1.67', 'VNM 1.53']],
      ] as const) {
        const it = I('cell', slug);
        const flows = flowsOf(tradeOf(it.idx)!, it.idx, 2024);
        const w = matrixTotal(flows);
        expect(r1(w / 1e9), slug).toBe(W);
        const top = league(countryTotals(flows), 'exports', w).slice(0, 10);
        expect(top.map((r) => `${model.geos[r.geo].code} ${r2(r.value / 1e9).toFixed(2)}`), slug).toEqual([...want]);
        // facts carry R's sum of the same flows (summation order: equal within 1e-12)
        for (const r of top) expect(Math.abs(facts.value(V('trade.exports').idx, it.idx, 2024, r.geo)!.value / r.value - 1)).toBeLessThan(1e-12);
      }
    });

    it('every trade cell, matrix 2024 (W USD bn, top-1 and its share, top-3, HHI, equal holders, exporters, core, partners, growth since 2020)', () => {
      // the 41 six-stage trade cells: Extraction 8, Refining 10, Components 10, Assembly 10, Recovery 3, Deployment none
      const rows: [string, number, string, number, number, number, number, number, number, number, number][] = [
        ['batteries-extraction', 18.3, 'AUS', 22.9, 54.7, 0.124, 8.1, 170, 30.8, 50.4, 37],
        ['batteries-refining', 74.5, 'CHN', 18.7, 36.8, 0.072, 13.8, 184, 42.1, 30.5, 41],
        ['batteries-components', 10.5, 'CHN', 37.4, 61.2, 0.178, 5.6, 131, 51.0, 8.8, 82],
        ['batteries-assembly', 117.7, 'CHN', 60.1, 72.0, 0.376, 2.7, 183, 34.0, 4.4, 189],
        ['batteries-recovery', 3.0, 'FIN', 14.9, 38.8, 0.069, 14.4, 131, 75.0, 12.4, 49],
        ['coal-extraction', 192.4, 'AUS', 32.8, 68.9, 0.197, 5.1, 119, 48.2, 32.5, 93],
        ['coal-refining', 10.0, 'CHN', 19.9, 53.5, 0.125, 8.0, 102, 42.4, 30.1, 58],
        ['coal-components', 5.4, 'CHN', 29.3, 48.2, 0.123, 8.1, 142, 58.5, 9.4, 11],
        ['coal-assembly', 5.2, 'CHN', 25.7, 47.7, 0.107, 9.4, 141, 56.8, 11.1, -10],
        ['electric-grid-extraction', 118.6, 'CHL', 26.2, 51.8, 0.125, 8.0, 118, 14.2, 71.9, 65],
        ['electric-grid-refining', 239.9, 'COD', 9.3, 24.1, 0.038, 26.1, 173, 38.3, 34.9, 59],
        ['electric-grid-components', 58.6, 'CHN', 29.9, 45.4, 0.115, 8.7, 192, 36.4, 27.6, 47],
        ['electric-grid-assembly', 353.6, 'CHN', 25.4, 40.8, 0.09, 11.1, 208, 48.3, 20.1, 48],
        ['electric-vehicles-extraction', 18.5, 'AUS', 22.7, 54.2, 0.122, 8.2, 170, 30.5, 50.2, 37],
        ['electric-vehicles-refining', 2.9, 'MMR', 28.5, 58.1, 0.148, 6.8, 70, 20.1, 23.9, 92],
        ['electric-vehicles-components', 295.5, 'CHN', 37.4, 49.7, 0.158, 6.3, 205, 38.1, 17.6, 76],
        ['electric-vehicles-assembly', 707.6, 'CHN', 20.3, 43.4, 0.085, 11.8, 208, 63.2, 13.0, 98],
        ['gas-extraction', 386.3, 'USA', 13.1, 37.2, 0.079, 12.6, 96, 48.2, 21.2, 111],
        ['gas-refining', 94.9, 'USA', 41.2, 59.9, 0.199, 5.0, 142, 54.6, 16.9, 89],
        ['gas-components', 46.2, 'USA', 23.3, 46.0, 0.098, 10.2, 194, 68.9, 9.9, 40],
        ['gas-assembly', 105.3, 'CHN', 20.0, 39.3, 0.078, 12.9, 205, 56.3, 17.4, 25],
        ['nuclear-extraction', 0.6, 'AUS', 75.9, 98.6, 0.604, 1.7, 25, 77.3, 14.5, 118],
        ['nuclear-refining', 20.4, 'KAZ', 22.8, 52.6, 0.13, 7.7, 43, 55.1, 5.7, 133],
        ['nuclear-components', 9.3, 'CHN', 16.7, 39.2, 0.084, 12.0, 139, 60.2, 5.4, 31],
        ['nuclear-assembly', 4.8, 'CHN', 24.6, 47.8, 0.105, 9.5, 131, 55.1, 10.6, -6],
        ['nuclear-recovery', 0.0, 'ZAF', 29.2, 63.9, 0.175, 5.7, 23, 53.2, 29.2, -40],
        ['oil-extraction', 1345.4, 'SAU', 13.9, 32.3, 0.066, 15.2, 132, 24.9, 29.4, 94],
        ['oil-refining', 888.7, 'USA', 12.5, 24.8, 0.046, 21.9, 204, 45.0, 27.5, 100],
        ['oil-components', 13.2, 'CHN', 28.7, 52.9, 0.126, 8.0, 178, 44.4, 17.0, 63],
        ['oil-assembly', 113.9, 'CHN', 23.0, 41.4, 0.087, 11.5, 207, 53.1, 18.0, 29],
        ['oil-recovery', 0.5, 'IND', 17.0, 40.3, 0.084, 11.9, 178, 57.8, 35.2, 114],
        ['solar-refining', 37.9, 'DEU', 12.2, 29.2, 0.054, 18.5, 127, 53.4, 16.4, 77],
        ['solar-components', 170.3, 'CHN', 23.0, 44.8, 0.1, 10.0, 208, 52.0, 18.5, 31],
        ['solar-assembly', 507.1, 'CHN', 26.4, 39.1, 0.094, 10.7, 208, 46.6, 21.0, 44],
        ['wind-extraction', 0.2, 'NGA', 75.5, 93.5, 0.588, 1.7, 21, 0.0, 24.5, 261],
        ['wind-refining', 33.4, 'DEU', 10.1, 26.0, 0.048, 20.7, 125, 50.2, 17.8, 53],
        ['wind-components', 92.8, 'CHN', 26.6, 45.3, 0.104, 9.6, 204, 52.2, 15.8, 28],
        ['wind-assembly', 286.1, 'CHN', 23.7, 39.7, 0.082, 12.2, 208, 48.9, 20.3, 48],
        ['semiconductors-refining', 5.5, 'USA', 23.7, 65.7, 0.159, 6.3, 90, 65.4, 11.9, 45],
        ['semiconductors-components', 124.4, 'CHN', 16.2, 41.7, 0.096, 10.4, 178, 52.5, 19.2, 25],
        ['semiconductors-assembly', 1219.2, 'TWN', 27.4, 58.5, 0.143, 7.0, 205, 34.3, 22.0, 31],
      ];
      expect(rows).toHaveLength(41);
      expect(model.cells.filter((c) => c.has_trade).map((c) => c.slug).sort()).toEqual(rows.map((r) => r[0]).sort());
      for (const [cell, W, top, share, top3, hhi, effn, n, core, partner, g] of rows) {
        const s = S('trade.exports', 'cell', cell, 2024);
        expect([cell, r1(s.W / 1e9), model.geos[s.top1_geo!].code, pct1(s.top1_share), pct1(s.top3_share), r3(s.hhi), r1(s.eff_n), s.n, pct1(s.core), pct1(s.partner), Math.round(s.growth * 100)]).toEqual([
          cell, W, top, share, top3, hhi, effn, n, core, partner, g,
        ]);
      }
    });

    it('a sub-component spanning stages appears under each, with that stage\'s codes (design S5)', () => {
      const subs = (slug: string) => {
        const it = I('cell', slug);
        return subComponents(tradeOf(it.idx)!, model, it.idx).map((x) => `${x.name} ${r2(x.world / 1e9).toFixed(2)}`);
      };
      expect(subs('batteries-refining')).toEqual(['Cobalt & Nickel Refining 26.91', 'Current Collectors & Foils 19.22', 'Cathode Materials & Battery Precursors 13.02', 'Lithium Chemicals 7.12', 'Separator Materials 3.02', 'Anode Materials 3.00', 'Electrolyte Materials 2.21']);
      expect(subs('electric-vehicles-components')).toEqual(['Vehicle Batteries & Packs 126.34', 'Electric Drive Units & Motors 65.51', 'Vehicle Wiring Harnesses 54.70', 'Motor & Generator Parts 24.14', 'Charging Infrastructure 15.13', 'Permanent Magnets & Magnet Parts 9.73']);
      expect(subs('electric-vehicles-assembly')).toEqual(['Electric Vehicle Assembly 361.60', 'Charging Infrastructure 346.04']);
      expect(subs('batteries-assembly')).toEqual(['Battery Cells 115.83', 'Battery Packs, Parts & BMS 1.89']);
    });

    it('Batteries — Refining and — Assembly, exporter-reported basis (the reconciliation rows)', () => {
      for (const [slug, total, china, hhi, n, matrix] of [
        ['batteries-refining', 72.8, 26.2, 0.103, 126, 74.5],
        ['batteries-assembly', 109.3, 56.0, 0.331, 127, 117.7],
      ] as const) {
        const it = I('cell', slug);
        const rc = model.reconcile.find((r) => r.item === it.idx && r.year === 2024)!;
        expect([r1(rc.reported_total / 1e9), pct1(rc.reported_china), r3(rc.reported_hhi), rc.n_reporters, r1(rc.matrix_total / 1e9)], slug).toEqual([total, china, hhi, n, matrix]);
      }
      // 2024 rows only (the exporter-reported basis has no 2020)
      expect(new Set(model.reconcile.map((r) => r.year))).toEqual(new Set([2024]));
    });

    it('CIM Batteries — Components 2025 (= Assembly, the same figure, noted), the Refining split, every CIM cell counted once', () => {
      const v = V('investment.annual');
      const bc = I('cell', 'batteries-components');
      const ba = I('cell', 'batteries-assembly');
      const usd = (x: number) => x * (model.unitOf(v, bc.idx).id.includes('bn') ? 1 : 1e-9);
      const s = S('investment.annual', 'cell', 'batteries-components', 2025);
      expect(r1(usd(s.W))).toBe(80.1);
      expect(r2(usd(facts.value(v.idx, bc.idx, 2025, G('CHN'))!.value))).toBe(32.99);
      expect(r2(usd(facts.value(v.idx, bc.idx, 2025, G('USA'))!.value))).toBe(28.51);
      expect([pct1(s.china), pct1(s.conc.rows.find((r) => r.geo === G('USA'))!.share!), pct1(s.core), pct1(s.partner), r3(s.hhi)]).toEqual([41.2, 35.6, 52.7, 6.1, 0.303]);
      expect(s.growth_kind).toBe('cagr_2018_2025');
      expect(pct1(s.growth)).toBe(27.6);
      expect(pct1(growth('cagr_2022_2025', S('investment.annual', 'cell', 'batteries-components', 2022).W, s.W).value!)).toBe(-3.1);
      // Assembly carries the same figure (design C2): every fact equal, both noted same_figure_pair
      const sa = S('investment.annual', 'cell', 'batteries-assembly', 2025);
      expect(sa.W).toBe(s.W);
      for (const r of facts.rows(v.idx, bc.idx, 2025)) {
        expect(facts.value(v.idx, ba.idx, 2025, r.geo)?.value).toBe(r.value);
        expect(r.notes).toContain(SAME_FIGURE_NOTE);
        expect(facts.value(v.idx, ba.idx, 2025, r.geo)?.notes).toContain(SAME_FIGURE_NOTE);
      }
      expect(sameFigureText(model, facts, v, ba)).toMatch(/^Same figure as Batteries — Components:/);
      // the critical-minerals capex split between the two Refining cells
      const br = I('cell', 'batteries-refining');
      const er = I('cell', 'electric-vehicles-refining');
      expect(r2(usd(S('investment.annual', 'cell', 'batteries-refining', 2025).W))).toBe(0.73);
      expect(r2(usd(S('investment.annual', 'cell', 'electric-vehicles-refining', 2025).W))).toBe(5.57);
      expect(pct1(S('investment.annual', 'cell', 'batteries-refining', 2025).china)).toBe(43.6);
      expect(splitText(model, facts, v, br)).toMatch(/^Batteries — Refining and Electric Vehicles — Refining split one critical-minerals investment series/);
      expect(splitText(model, facts, v, er)).not.toBeNull();
      // all CIM cells, 2025: the Assembly copies counted once (1875 bn, as the investment headline)
      let total = 0;
      let china = 0;
      let all = 0;
      for (const it of facts.itemsOf(v.idx)) {
        const w = facts.world(v.idx, it, 2025);
        if (!w) continue;
        all += w.value;
        if (facts.rows(v.idx, it, 2025).some((r) => isPairCopy(model.items[it], r.notes))) continue;
        total += w.value;
        china += facts.value(v.idx, it, 2025, G('CHN'))?.value ?? 0;
      }
      expect(Math.round(usd(total))).toBe(1875);
      expect(Math.round(usd(all))).toBe(2010);
      expect(pct1(china / total)).toBe(44.2);
      // the copied pairs: the four CIM technologies' Components and Assembly
      const paired = facts.itemsOf(v.idx).filter((it) => facts.rows(v.idx, it, 2025).some((r) => r.notes?.includes(SAME_FIGURE_NOTE))).map((it) => model.items[it].slug).sort();
      expect(paired).toEqual(['batteries-assembly', 'batteries-components', 'electric-vehicles-assembly', 'electric-vehicles-components', 'solar-assembly', 'solar-components', 'wind-assembly', 'wind-components']);
    });

    it('IEA Critical Minerals 2025 refining (lithium, battery-grade graphite, rare earths, cobalt)', () => {
      const v = V('production.refining');
      const ref2025 = (slug: string) => {
        const it = I('mineral', slug);
        const s = summaryFromFacts(model, facts, v, it.idx, 2025, 'members', null);
        return { s, chn: facts.value(v.idx, it.idx, 2025, G('CHN'))!.value };
      };
      const li = ref2025('lithium');
      expect([Math.round(li.chn), Math.round(li.s.W)]).toEqual([235, 319]);
      expect(pct1(li.s.china)).toBe(73.7);
      expect(r3(li.s.top3_share)).toBe(0.949);
      const gr = ref2025('graphite');
      expect([Math.round(gr.chn), Math.round(gr.s.W)]).toEqual([2139, 2266]);
      expect(pct1(gr.s.china)).toBe(94.4);
      expect(r3(gr.s.top3_share)).toBe(0.988);
      expect(pct1(ref2025('rare-earths').s.china)).toBe(83.9);
      expect(pct1(ref2025('cobalt').s.china)).toBe(75.6);
    });

    it('critical-minerals trade 2024 by stage (mineral_stage items, design C3)', () => {
      const v = V('trade.critmin_exports');
      expect(v.item_axis).toBe('mineral_stage');
      for (const [slug, W, top, share, hhi] of [
        ['lithium-refining', 6.24, 'CHL', 46.1, 0.353],
        ['graphite-refining', 2.79, 'CHN', 48.8, 0.264],
        ['cobalt-recovery', 0.08, 'GBR', 25.2, 0.123],
      ] as const) {
        const s = S('trade.critmin_exports', 'mineral_stage', slug, 2024);
        expect([slug, r2(s.W / 1e9), model.geos[s.top1_geo!].code, pct1(s.top1_share), r3(s.hhi)]).toEqual([slug, W, top, share, hhi]);
      }
      // 26 items: Extraction 9, Refining 11, Recovery 6; lithium is traded refined only, graphite never as scrap
      const ms = model.items.filter((x) => x.axis === 'mineral_stage');
      expect(ms).toHaveLength(26);
      expect(['extraction', 'refining', 'recovery'].map((st) => ms.filter((x) => x.stageSlug === st).length)).toEqual([9, 11, 6]);
      expect(mineralStageItems(model, I('mineral', 'lithium')).map((x) => x.slug)).toEqual(['lithium-refining']);
      expect(mineralStageItems(model, I('mineral', 'graphite')).map((x) => x.slug)).toEqual(['graphite-extraction', 'graphite-refining']);
      expect(mineralStageItems(model, I('mineral', 'copper')).map((x) => x.slug)).toEqual(['copper-extraction', 'copper-refining', 'copper-recovery']);
    });

    it('Red Book: Kazakhstan 37.6% of 2024 uranium mine production (Nuclear — Extraction)', () => {
      const s = S('production.uranium', 'cell', 'nuclear-extraction', 2024);
      expect(s.top1_geo).toBe(G('KAZ'));
      expect(pct1(s.top1_share)).toBe(37.6);
    });

    it('Atlas 2023: China share of Batteries — Refining 0.257 and — Assembly 0.529 (their own rows)', () => {
      const v = V('trade.atlas_share');
      expect(r3(facts.value(v.idx, I('cell', 'batteries-refining').idx, 2023, G('CHN'))!.value)).toBe(0.257);
      expect(r3(facts.value(v.idx, I('cell', 'batteries-assembly').idx, 2023, G('CHN'))!.value)).toBe(0.529);
    });

    it('export feasibility 2023: Batteries — Refining and — Assembly, Solar — Refining, Semiconductors — Refining', () => {
      const v = V('trade.feasibility');
      for (const [slug, n, top, med] of [
        ['batteries-refining', 210, ['CHN 0.508', 'DEU 0.440', 'ITA 0.424'], 0.078],
        ['batteries-assembly', 210, ['CHN 0.614', 'DEU 0.497', 'ITA 0.452'], 0.061],
        ['solar-refining', 205, ['DEU 0.505', 'CHN 0.479', 'ITA 0.436'], 0.085],
        ['semiconductors-refining', 207, ['CHN 0.519', 'DEU 0.446', 'FRA 0.420'], 0.089],
      ] as const) {
        const it = I('cell', slug);
        const rows = facts.rows(v.idx, it.idx, 2023).filter((r) => Number.isFinite(r.value)).sort((a, z) => z.value - a.value);
        expect(rows.length, slug).toBe(n);
        expect(rows.slice(0, 3).map((r) => `${model.geos[r.geo].code} ${r.value.toFixed(3)}`), slug).toEqual([...top]);
        const xs = rows.map((r) => r.value).sort((a, z) => a - z);
        const median = xs.length % 2 ? xs[(xs.length - 1) / 2] : (xs[xs.length / 2 - 1] + xs[xs.length / 2]) / 2;
        expect(r3(median), slug).toBe(med);
      }
    });

    it('basket: 259 codes, 105 in more than one cell (max 4); Batteries — Refining 31 codes, 0 shared, 7 sub-sectors; — Assembly 2 codes, 1 shared; 92 excluded rows, per technology', () => {
      const cells = new Map<string, Set<number>>();
      let excluded = 0;
      for (const [name, t] of trades) {
        const c = t['trade.codes'].cols;
        for (let i = 0; i < t['trade.codes'].nrow; i++) {
          const k = c.hs6[i]!;
          if (!cells.has(k)) cells.set(k, new Set());
          cells.get(k)!.add(c.item[i]);
        }
        excluded += t['trade.excluded'].nrow;
        // the excluded codes are listed per technology (design C6): every row names the chunk's technology
        const tech = TECH_LABELS[name.slice('sector-trade-'.length) as keyof typeof TECH_LABELS];
        expect(new Set(t['trade.excluded'].cols.tech).size <= 1 && (t['trade.excluded'].nrow === 0 || t['trade.excluded'].cols.tech[0] === tech), name).toBe(true);
      }
      expect(cells.size).toBe(259);
      expect([...cells.values()].filter((s) => s.size > 1).length).toBe(105);
      expect(Math.max(...[...cells.values()].map((s) => s.size))).toBe(4);
      const codes = (slug: string) => {
        const it = I('cell', slug);
        const t = tradeOf(it.idx)!['trade.codes'];
        return [...Array(t.nrow).keys()].filter((i) => t.cols.item[i] === it.idx).map((i) => ({ shared: t.cols.shared_n[i], sub: t.cols.sub_sector[i] }));
      };
      const br = codes('batteries-refining');
      expect([br.length, br.filter((x) => x.shared > 0).length, new Set(br.map((x) => x.sub)).size]).toEqual([31, 0, 7]);
      const ba = codes('batteries-assembly');
      expect([ba.length, ba.filter((x) => x.shared > 0).length, new Set(ba.map((x) => x.sub)).size]).toEqual([2, 1, 2]);
      expect(excluded).toBe(92);
      expect(excludedCodes(trades.get('sector-trade-batteries')!, 'Batteries')).toHaveLength(31);
    });

    it('CM-FDI: 1,042 projects, each on its Components cell; Assembly reads the same projects (design E5)', () => {
      const info = manifest.chunks.find((c) => c.name === 'sector-fdi')!;
      const d = fdiData(validateChunk('sector-fdi', readChunkFile(info.file), ref) as SectorFdiChunk);
      expect(d.stages).toEqual(['Components', 'Assembly']);
      expect(d.projects).toHaveLength(1042);
      expect(new Set(d.projects.map((p) => model.items[p.item].stageSlug))).toEqual(new Set(['components']));
      for (const [tech, n] of [['batteries', 314], ['electric-vehicles', 255], ['solar', 271], ['wind', 202]] as const) {
        for (const st of ['components', 'assembly']) expect(filterProjects(d.projects, { items: [fdiItemOf(model, d, I('cell', `${tech}-${st}`))] }), `${tech}-${st}`).toHaveLength(n);
      }
    });

    it('the headlines agree with the derivations', () => {
      const h = (id: string) => manifest.headlines.find((x) => x.id === id)!;
      const sup = h('supply');
      expect([sup.labels.geo, sup.labels.item, pct1(sup.values.share), Math.round(sup.values.value_kt), Math.round(sup.values.total_kt)]).toEqual(['CHN', 'graphite', 94.4, 2139, 2266]);
      // dependence: the headline Components and Assembly trade cells; the largest 2024 world total is Electric Vehicles — Assembly
      const dep = h('dependence');
      const ev = S('trade.exports', 'cell', 'electric-vehicles-assembly', 2024);
      expect([dep.labels.cell, dep.labels.cell_label, dep.href]).toEqual(['electric-vehicles-assembly', 'Electric Vehicles — Assembly', '/sector/electric-vehicles/assembly#trade']);
      expect(dep.values.share).toBeCloseTo(ev.china, 12);
      expect(dep.values.total_usd).toBeCloseTo(ev.W, 0);
      const manufacturing = model.cells.filter((c) => c.headline && c.has_trade && (c.stageSlug === 'components' || c.stageSlug === 'assembly'));
      for (const c of manufacturing) expect(S('trade.exports', 'cell', c.slug, 2024).W, c.slug).toBeLessThanOrEqual(ev.W);
      // investment: the Components and Assembly copies counted once
      const inv = h('investment');
      expect([Math.round(inv.values.total_bn), pct1(inv.values.china), pct1(inv.values.coalition)]).toEqual([1875, 44.2, 52.3]);
      // 61 summary rows of the cells' primary measures with country data; 10 have one holder at 50% or more
      expect([h('concentration').values.n, h('concentration').values.n_total]).toEqual([8, 57]);
      const res = h('reserves');
      expect([res.labels.geo, res.labels.item, pct1(res.values.share)]).toEqual(['ZAF', 'pgms', 82.7]);
      const en = h('energy');
      expect([en.labels.exporter, r2(en.values.exporter_ej), en.labels.importer, r2(en.values.importer_ej)]).toEqual(['USA', 5.77, 'CHN', 6.4]);
      // distinct IEA PAMS records dated 2026-2030 (36 jurisdiction rows; 27 of them one EU directive)
      expect(h('policy').values.n).toBe(10);
      const pr = h('prices');
      expect([pr.labels.item, pct1(pr.values.vol)]).toEqual(['natural-gas-eu', 80.7]);
    });
  });

  describe('time series (sector-series)', () => {
    it('series rows equal facts for the same key; EI 1965-2025; IMF monthly 2003-2025', () => {
      const idx = seriesIndexOf(series);
      const vf = V('production.fuels');
      const items = idx.itemsOf(vf.idx);
      expect(items.length).toBeGreaterThan(0);
      let shared = 0;
      expect(Math.min(...items.map((it) => idx.yearsOf(vf.idx, it)[0]))).toBe(1965);
      for (const it of items) {
        const ys = idx.yearsOf(vf.idx, it);
        expect(ys[ys.length - 1]).toBe(2025);
        for (const y of facts.yearsOf(vf.idx, it)) {
          for (const r of facts.rows(vf.idx, it, y)) {
            const s = idx.value(vf.idx, it, y, r.geo);
            if (s) {
              shared++;
              expect(Object.is(s.value, r.value)).toBe(true);
            }
          }
        }
      }
      expect(shared).toBeGreaterThan(0);
      const m = series['series.monthly'];
      expect(m.nrow).toBeGreaterThan(0);
      expect(Math.min(...m.cols.year)).toBe(2003);
      expect(Math.max(...m.cols.year)).toBe(2025);
    });

    it('policy: IEA PAMS records and CAT ratings', () => {
      expect(policy['policy.pams'].nrow).toBeGreaterThan(1000);
      expect(policy['policy.cat'].nrow).toBeGreaterThan(10);
    });
  });

  describe('review fixes (repair integrator, 2026-10-11)', () => {
    it('IEA EV regions whose members span groups are not attributed (North America, Other Asia Pacific, Rest of World)', () => {
      const ev = V('production.ev_region');
      for (const code of ['R:iea_ev:north-america', 'R:iea_ev:other-asia-pacific', 'R:iea_ev:rest-of-world']) expect(model.geos[G(code)].group).toBe('na');
      expect(model.geos[G('R:iea_ev:china')].group).toBe('china');
      expect(model.geos[G('R:iea_ev:european-union')].group).toBe('core');
      for (const it of facts.itemsOf(ev.idx)) {
        const y = facts.latestYear(ev.idx, it) as number;
        const s = model.summaryOf(it, ev.idx, y, 'members');
        if (!s) continue;
        expect(s.partner).toBe(0);
        expect(s.rest).toBe(0);
        expect(s.unattributed).toBeGreaterThan(0.05);
        expect(s.china + s.core + s.unattributed).toBeCloseTo(1, 9);
      }
    });

    it('EI successor states: no "reported zero" before they exist, none for the USSR after 1984', () => {
      const idx = seriesIndexOf(series);
      const v = V('consumption.total_ej');
      const sun = G('SUN');
      expect(model.geos[sun].name).toMatch(/USSR/);
      for (const it of idx.itemsOf(v.idx)) {
        for (const y of idx.yearsOf(v.idx, it)) {
          const su = idx.value(v.idx, it, y, sun);
          if (y >= 1985 && su) expect(su.status).not.toBe('zero');
          if (y < 1985 && su && su.value > 0) {
            for (const code of ['UKR', 'RUS', 'KAZ', 'BLR']) {
              const r = idx.value(v.idx, it, y, G(code));
              if (r) expect(r.status).not.toBe('zero');
            }
          }
        }
      }
    });

    it('self-sufficiency and the balance where EI has no production row: EIA fill, else production assumed 0 and drawn with the note (C5.5)', () => {
      const ss = V('consumption.self_sufficiency');
      const bal = V('consumption.balance');
      const gas = I('fuel', 'gas');
      const y = facts.latestYear(ss.idx, gas.idx) as number;
      // New Zealand gas: no EI production row; the EIA production of the same year fills it (e373ece)
      const nzl = facts.value(ss.idx, gas.idx, y, G('NZL'));
      expect(nzl?.status).toBe('obs');
      expect(Number.isFinite(nzl?.value)).toBe(true);
      expect(facts.notesOf(ss.idx, gas.idx, y, G('NZL'))).toContain('eia_production');
      // what is left unreported is drawn as a value with the assumed-zero note: an nsr fact with a value
      const assumed: string[] = [];
      for (const v of [ss, bal]) {
        for (const it of model.items.filter((x) => x.axis === 'fuel')) {
          const ly = facts.latestYear(v.idx, it.idx);
          if (ly == null) continue;
          for (const r of facts.rows(v.idx, it.idx, ly)) {
            if (r.status !== 'nsr' || !Number.isFinite(r.value)) continue;
            expect(isAssumedZero(v.id, r)).toBe(true);
            expect(noteWords(model, v.id, r)[0]).toBe(ASSUMED_ZERO_WORDS);
            assumed.push(`${v.id} ${it.slug} ${ly} ${model.geos[r.geo].code}`);
          }
        }
      }
      // the 2026-10-12 export: only Estonia coal (EI counts oil shale as coal)
      expect([...new Set(assumed.map((a) => a.split(' ').slice(1).join(' ')))]).toEqual(['coal 2025 EST']);
    });

    it('Development & finance: raw values with their builders\' checks; aid by stage on the cells; conduit centres flagged', () => {
      for (const c of [
        'parity X: decoded development.wdi identical() to partnership_strength_build_wdi_long()$value on wb_wdi.csv',
        'parity X: CRS aid received 2013-2024 by stage, ranked as the 3-digit stage builder (partnership_strength_build_oecd_supplychain()) ranks it, reproduces its aid_index_supplychain identical()',
      ]) expect(manifest.validation.find((x) => x.check === c)?.status).toBe('PASS');
      expect(manifest.validation.some((x) => /^parity X: IMF DIP positions .* reproduce its outbound_index identical\(\)$/.test(x.check) && x.status === 'PASS')).toBe(true);
      expect(manifest.validation.filter((x) => /^parity X: the builder on this export's raw inputs reproduces the stored /.test(x.check)).every((x) => x.status === 'PASS')).toBe(true);
      // WDI: GDP per capita of the United States, in constant 2015 dollars, with its own unit
      const gdp = I('indicator', 'ny-gdp-pcap-kd');
      expect(model.unitOf(V('development.wdi'), gdp.idx).id).toBe('usd_pc_2015');
      const us = facts.rows(V('development.wdi').idx, gdp.idx, facts.latestYear(V('development.wdi').idx, gdp.idx) as number).find((r) => r.geo === G('USA'));
      expect(us && us.value > 50_000 && us.value < 80_000).toBe(true);
      // aid for energy is a measure of every Deployment cell (the economy page and the profile's leaders); industry
      // aid of the Refining, Components and Assembly cells (the pipeline's purpose crosswalk)
      for (const st of ['refining', 'components', 'assembly']) expect(model.itemsOfCell(I('cell', `batteries-${st}`).idx, 'aid_sector').map((x) => x.item.slug), st).toEqual(['industry']);
      const solarDown = I('cell', 'solar-deployment');
      expect(countryVars(model, solarDown).map((v) => v.id)).toContain('development.oda_received');
      expect(model.itemsOfCell(solarDown.idx, 'aid_sector').map((x) => x.item.slug)).toEqual(['energy']);
      const dev = validateChunk('sector-dev', readChunkFile(manifest.chunks.find((c) => c.name === 'sector-dev')!.file), ref) as SectorDevChunk;
      const conduit = new Set(Array.from(dev['dev.conduits'].cols.geo));
      expect(conduit.has(G('NLD')) && conduit.has(G('LUX')) && conduit.has(G('HKG'))).toBe(true);
      let flagged = 0;
      for (let i = 0; i < dev['dev.dip'].nrow; i++) if (dev['dev.dip'].cols.conduit[i] !== 'none') flagged++;
      expect(flagged).toBeGreaterThan(100);
      expect(dev['dev.oda'].nrow).toBeGreaterThan(10_000);
    });

    it('sector-climate: Climate TRACE emissions identical() to data/processed, a match quality for every cell, plausible magnitudes', () => {
      for (const c of manifest.validation.filter((x) => x.check.startsWith('climate (decoded):'))) expect({ check: c.check, status: c.status }).toEqual({ check: c.check, status: 'PASS' });
      // 6 Climate TRACE parity rows, plus 6 for the IEA pathway tables (climate.nze_*) and 18 for the climate
      // sources (sector-climate-src) when the build has them
      const nzeRows = manifest.validation.filter((x) => x.check.startsWith('climate (decoded):') && /IEA pathway|mineral demand by scenario/.test(x.check)).length;
      expect(nzeRows === 0 || nzeRows === 6).toBe(true);
      const srcRows = manifest.validation.filter((x) => SRC_PARITY.test(x.check)).length;
      expect(srcRows === 0 || srcRows === 18).toBe(true);
      expect(manifest.validation.filter((x) => x.check.startsWith('climate (decoded):')).length - nzeRows - srcRows).toBe(6);
      const raw = validateChunk('sector-climate', readChunkFile(manifest.chunks.find((c) => c.name === 'sector-climate')!.file), ref) as SectorClimateChunk;
      const d = climateData(raw);
      expect(d.cells.size).toBe(model.cells.length);
      expect(cellMatch(d, I('cell', 'coal-extraction').idx).match).toBe('direct');
      expect(cellMatch(d, I('cell', 'solar-components').idx).match).toBe('none');
      expect(d.latest).toBeGreaterThanOrEqual(2025);
      const coal = d.srcs.find((x) => x.id === 'coal-mining')!;
      const e = emitters(d, model, coal.idx, d.latest);
      expect(e.world! > 1e9 && e.world! < 5e9).toBe(true);
      expect(e.rows[0].g.code).toBe('CHN');
      const power = d.srcs.find((x) => x.id === 'electricity-generation:coal')!;
      const w = intensityOf(d, intensityKeys(d, power)[0].key, model.world!.idx, d.latest)!;
      expect(w.value > 0.7 && w.value < 1.3 && w.unit === 't CO2e per MWh').toBe(true);
      expect(fmtLifecycle(d.lifecycle.get('Coal'))).toBe('820 gCO2e/kWh');
      expect(d.lifecycle.get('Electric Grid')?.proxyOf).toEqual(['Solar', 'Wind']);
    });

    it('sector-climate IEA pathways (climate.nze_*): parity PASS, every cell mapped, World scenarios as published, no Net Zero mineral scenario', () => {
      const raw = validateChunk('sector-climate', readChunkFile(manifest.chunks.find((c) => c.name === 'sector-climate')!.file), ref) as SectorClimateChunk;
      const n = climateData(raw).nze;
      if (!n) return void console.info('this build has no IEA pathway tables (--no-nze)');
      expect(manifest.validation.filter((x) => /^climate \(decoded\): (IEA pathway|mineral demand by scenario)/.test(x.check)).every((x) => x.status === 'PASS')).toBe(true);
      expect(n.cells.size).toBe(model.cells.length);
      expect(nzeCell(n, I('cell', 'solar-deployment').idx).match).toBe('direct');
      expect(nzeCell(n, I('cell', 'electric-vehicles-deployment').idx).match).toBe('proxy');
      expect(nzeCell(n, I('cell', 'semiconductors-refining').idx).match).toBe('none');
      expect(n.weo).toMatch(/^World Energy Outlook 20\d\d$/);
      expect(n.baseYear).toBeGreaterThanOrEqual(2024);
      const solar = nzeCompare(n, n.byId.get('cap_solar')!);
      expect(solar.nze! > solar.stated! && solar.stated! > solar.base!).toBe(true);
      const coal = nzeCompare(n, n.byId.get('tes_coal')!);
      expect(coal.nze! < coal.stated!).toBe(true);
      expect(nzeHeadline(n, I('cell', 'solar-deployment').idx)!.text).toMatch(/^The Net Zero pathway needs [\d,]+ GW of solar PV capacity by 2035/);
      expect(new Set(n.minerals.map((r) => r.scenario))).toEqual(new Set(['current', 'stated', 'high_demand']));
      const li = mineralPaths(n, nzeCell(n, I('cell', 'batteries-extraction').idx).mineralTechs);
      expect(li.map((m) => m.mineral)).toContain('Lithium');
      expect(nzeWorldLine(n)).toMatch(/^World context, not this economy’s: energy-sector CO2 was [\d.]+ Gt in 20\d\d/);
    });

    it('sector-climate-src (climate sources): parity PASS, published spot values, regions mapped, every cell with a match quality', () => {
      const info = manifest.chunks.find((c) => c.name === 'sector-climate-src');
      if (!info) return void console.info('this build has no climate sources (--no-climate-sources)');
      expect(manifest.validation.filter((x) => SRC_PARITY.test(x.check)).every((x) => x.status === 'PASS')).toBe(true);
      const d = climateSrcData(validateChunk('sector-climate-src', readChunkFile(info.file), ref) as SectorClimateSrcChunk);
      const G = (code: string) => model.geoByCode.get(code)!.idx;
      // GCP 2025 v1.0: Japan territorial 2024 262.5 MtC x 3.664 (962 Mt); consumption-based above territorial (negative transfers)
      const jp = gcpCountry(d, G('JPN'), G('WLD'))!;
      expect(jp.last!.year).toBe(2024);
      expect(jp.last!.value > 900 && jp.last!.value < 1000).toBe(true);
      expect(jp.lastCons!.transfer! < 0).toBe(true);
      expect(gcpHeadline(jp, 'Japan')).toMatch(/more than it emits at home/);
      expect(eiRows(d, G('DEU')).map((r) => r.ind.id)).toEqual(expect.arrayContaining(['ghg', 'renewables_share', 'carbon_intensity', 'power_low_carbon']));
      expect(weoRegionOf(d, G('JPN'), 'Japan')).toMatchObject({ region: 'Japan', match: 'own' });
      expect(weoRegionOf(d, G('DEU'), 'Germany')).toMatchObject({ region: 'European Union', match: 'member' });
      expect(weoRegionOf(d, G('BRA'), 'Brazil')).toMatchObject({ region: 'Brazil', match: 'own' });
      expect(d.weo.get('Japan')!.get(d.weoSeries.find((x) => x.id === 'co2_energy')!.idx)!.has('nze')).toBe(false);
      // NZE 2021: 4,956 GW of solar PV in 2030; WEI 2026: World solar investment USD 407 bn in 2025, 2026 an estimate
      expect(nze21For(d, 'cap_solar')!.value).toBe(4956);
      const sol = weiSummary(d, 'solar')!;
      expect([sol.year, sol.value, sol.est?.year]).toEqual([2025, 407, 2026]);
      expect(d.cells.size).toBe(model.cells.length);
      expect(srcCell(d, I('cell', 'solar-deployment').idx).match).toBe('direct');
      expect(srcCell(d, I('cell', 'semiconductors-components').idx).match).toBe('none');
      const coal = fuelRows(d, I('cell', 'coal-extraction').idx)[0];
      expect(coal.fuel).toBe('coal');
      expect(coal.share! > 0.35 && coal.share! < 0.45).toBe(true);
      expect(fuelRows(d, I('cell', 'solar-deployment').idx)).toEqual([]);
    });

    it('every cell with a dictionary primary measure resolves it (the Snapshot is never empty); fuel Deployment: consumption per person', () => {
      // (cells with country data only per linked mineral have no single primary)
      const missing = model.cells.filter((c) => model.primaryVarsOf(c.slug).length > 0 && !primaryMeasure(model, c)).map((c) => c.slug);
      expect(missing).toEqual([]);
      for (const slug of ['oil-deployment', 'gas-deployment', 'coal-deployment']) {
        const pm = primaryMeasure(model, I('cell', slug));
        expect(pm?.v.id).toBe('consumption.per_capita');
        const y = facts.latestYear(pm!.v.idx, pm!.item.idx) as number;
        expect(facts.rows(pm!.v.idx, pm!.item.idx, y).filter((r) => Number.isFinite(r.value)).length).toBeGreaterThan(50);
        expect(countryFamilies(model, I('cell', slug))).toContain('consumption');
      }
    });
  });
});
