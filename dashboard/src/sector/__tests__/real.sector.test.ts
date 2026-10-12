// Sector Insights on the REAL bundle (spec_sector.md 10.4; integrator). Vitest project `sector-real`
// (`npm run test:sector-real`); every test skips when public/data/sector/manifest.json is absent (CI
// has no real data). Run it after scripts/87 has written the bundle.
//
//   * Every Appendix B reference value (recomputed on the final data of 2026-10-10, user decision 4)
//     is reproduced by the TS derivations from the decoded real chunks, at the precision the app
//     shows it.
//   * meta.summary = the client recomputation for every item x var x year x EU mode.
//   * Every chunk passes validateChunk(); the headlines carry exactly the HEADLINE_RULES keys and
//     agree with the derivations.
//   * manifest.validation has no FAIL; its WARNs are listed in the test output.
//   * Sector data <= 9 MB gz; Strategic + Sector <= 25 MB gz.

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
  type SectorFactsChunk,
  type SectorManifest,
  type SectorMetaChunk,
  type SectorPolicyChunk,
  type SectorRefSizes,
  type SectorSeriesChunk,
  type SectorTradeChunk,
} from '../contract.ts';
import { factsIndexOf, seriesIndexOf, type FactsIndex } from '../data/facts.ts';
import { modelOf, type ItemInfo, type SectorModel, type VarInfo } from '../data/model.ts';
import { countryFamilies, primaryMeasure } from '../derive/cells.ts';
import { growth } from '../derive/concentration.ts';
import { countryTotals, flowsOf, league, matrixTotal } from '../derive/trade.ts';
import { summaryFromFacts, summaryParity } from '../derive/summary.ts';

const DATA_DIR = resolve(process.cwd(), 'public/data');
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
function dirBytes(dir: string, recurse: boolean): number {
  let n = 0;
  for (const f of readdirSync(dir)) {
    const p = join(dir, f);
    const s = statSync(p);
    if (s.isDirectory()) {
      if (recurse) n += dirBytes(p, true);
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
      // 16 chunks: meta, facts, 10 trade, policy, policy-text, series, fdi
      expect(manifest.chunks.length).toBe(16);
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
      // the two expected WARNs of the final run (EI hydro 1972 residual; oil-midstream trade bases)
      expect(warns.length).toBeLessThanOrEqual(2);
    });

    it('budgets: each chunk, sector <= 9 MB gz, Strategic + Sector <= 25 MB gz', () => {
      for (const c of manifest.chunks) {
        const key = (c.name.startsWith('sector-trade-') ? 'sector-trade' : c.name) as keyof typeof SECTOR_BUDGET_GZ;
        expect(c.bytes_gz, c.name).toBeLessThanOrEqual(SECTOR_BUDGET_GZ[key]);
        expect(statSync(join(SECTOR_DIR, c.file)).size, c.name).toBe(c.bytes_gz);
      }
      const sector = dirBytes(SECTOR_DIR, false);
      const all = dirBytes(DATA_DIR, true);
      console.info(`data on disk (gz): sector ${(sector / 2 ** 20).toFixed(2)} MB, Strategic + Sector ${(all / 2 ** 20).toFixed(2)} MB`);
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

    it('the coverage glyph: 33 cells, 28 with country data', () => {
      expect(manifest.coverage_glyph.length).toBe(33);
      const empty = manifest.coverage_glyph.filter((g) => !g.country_data).map((g) => `${g.tech}-${g.stage}`).sort();
      expect(empty).toEqual(['electric-grid-downstream', 'geothermal-midstream', 'geothermal-upstream', 'green-hydrogen-downstream', 'green-hydrogen-midstream']);
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

  describe('Appendix B reference values (final data, 2026-10-10)', () => {
    it('Batteries Midstream, matrix 2024', () => {
      const s = S('trade.exports', 'cell', 'batteries-midstream', 2024);
      expect(r1(s.W / 1e9)).toBe(202.7);
      expect(s.n).toBe(198);
      expect(s.top1_geo).toBe(G('CHN'));
      expect(pct1(s.china)).toBe(43.7);
      expect(pct1(s.top3_share)).toBe(53.6);
      expect(r3(s.hhi)).toBe(0.207);
      expect(r1(s.eff_n)).toBe(4.8);
      expect(pct1(s.core)).toBe(37.9);
      expect(pct1(s.partner)).toBe(14.0);
      expect(pct1(s.core + s.partner)).toBe(51.9);
      expect(pct1(s.rest)).toBe(4.4);
      expect(s.unattributed).toBe(0);
      expect(s.growth_kind).toBe('pct_2020_2024');
      expect(Math.round(s.growth * 100)).toBe(104);
      const s0 = S('trade.exports', 'cell', 'batteries-midstream', 2020);
      expect(r1(s0.W / 1e9)).toBe(99.3);
      const usa = s.conc.rows.find((r) => r.geo === G('USA'))!;
      expect(pct1(usa.share!)).toBe(4.8);
      expect(usa.rank).toBe(4);
      expect(pct1(s.conc.rows.find((r) => r.geo === G('HKG'))!.share!)).toBe(0.3);
      // every coalition member (31 core + 26 partners) exports
      const coalition = model.geos.filter((g) => g.kind === 'country' && (g.tier === 'core' || g.tier === 'partner'));
      expect(coalition.length).toBe(57);
      expect(coalition.filter((g) => g.tier === 'core').length).toBe(31);
      expect(coalition.filter((g) => (s.conc.rows.find((r) => r.geo === g.idx)?.value ?? 0) > 0).length).toBe(57);
    });

    it('Batteries Midstream, top exporters 2024 from the trade chunk = facts', () => {
      const bm = I('cell', 'batteries-midstream');
      const flows = flowsOf(tradeOf(bm.idx)!, bm.idx, 2024);
      const W = matrixTotal(flows);
      expect(r1(W / 1e9)).toBe(202.7);
      const top = league(countryTotals(flows), 'exports', W).slice(0, 10);
      expect(top.map((r) => `${model.geos[r.geo].code} ${r2(r.value / 1e9).toFixed(2)}`)).toEqual([
        'CHN 88.67', 'KOR 10.18', 'IDN 9.90', 'USA 9.78', 'JPN 9.32', 'POL 7.89', 'DEU 7.14', 'HUN 7.04', 'CAN 4.41', 'COD 3.34',
      ]);
      // facts carry R's sum of the same flows (summation order: equal within 1e-12)
      for (const r of top) expect(Math.abs(facts.value(V('trade.exports').idx, bm.idx, 2024, r.geo)!.value / r.value - 1)).toBeLessThan(1e-12);
    });

    it('other cells, matrix 2024 (W USD bn, top-1, HHI)', () => {
      const rows: [string, number, string, number, number][] = [
        ['solar-midstream', 711.5, 'CHN', 24.6, 0.088],
        ['electric-vehicles-midstream', 1006.0, 'CHN', 25.3, 0.098],
        ['electric-grid-midstream', 449.6, 'CHN', 24.7, 0.085],
        ['wind-midstream', 412.4, 'CHN', 22.8, 0.078],
        ['oil-upstream', 2211.0, 'USA', 10.4, 0.048],
        ['gas-upstream', 445.3, 'USA', 19.1, 0.083],
        ['coal-upstream', 192.5, 'AUS', 32.8, 0.196],
        ['semiconductors-midstream', 1349.1, 'TWN', 25.8, 0.133],
      ];
      for (const [cell, W, top, share, hhi] of rows) {
        const s = S('trade.exports', 'cell', cell, 2024);
        expect([cell, r1(s.W / 1e9), model.geos[s.top1_geo!].code, pct1(s.top1_share), r3(s.hhi)]).toEqual([cell, W, top, share, hhi]);
      }
    });

    it('Batteries Midstream, exporter-reported basis (the reconciliation row)', () => {
      const bm = I('cell', 'batteries-midstream');
      const rc = model.reconcile.find((r) => r.item === bm.idx && r.year === 2024)!;
      expect(r1(rc.reported_total / 1e9)).toBe(189.6);
      expect(Math.round(rc.reported_total / 1e9)).toBe(190);
      expect(pct1(rc.reported_china)).toBe(43.4);
      expect(r3(rc.reported_hhi)).toBe(0.206);
      expect(rc.n_reporters).toBe(133);
      expect(r1(rc.matrix_total / 1e9)).toBe(202.7);
      // 2024 rows only (the exporter-reported basis has no 2020)
      expect(new Set(model.reconcile.map((r) => r.year))).toEqual(new Set([2024]));
    });

    it('CIM Batteries Midstream 2025 and every CIM cell', () => {
      const s = S('investment.annual', 'cell', 'batteries-midstream', 2025);
      const v = V('investment.annual');
      const bm = I('cell', 'batteries-midstream');
      const usd = (x: number) => x * (model.unitOf(v, bm.idx).id.includes('bn') ? 1 : 1e-9);
      expect(r1(usd(s.W))).toBe(80.1);
      expect(r2(usd(facts.value(v.idx, bm.idx, 2025, G('CHN'))!.value))).toBe(32.99);
      expect(r2(usd(facts.value(v.idx, bm.idx, 2025, G('USA'))!.value))).toBe(28.51);
      expect(pct1(s.china)).toBe(41.2);
      expect(pct1(s.conc.rows.find((r) => r.geo === G('USA'))!.share!)).toBe(35.6);
      expect(pct1(s.core)).toBe(52.7);
      expect(pct1(s.partner)).toBe(6.1);
      expect(r3(s.hhi)).toBe(0.303);
      expect(s.growth_kind).toBe('cagr_2018_2025');
      // recomputed on the final data: +27.6% (the pre-rerun probe said +27.5%; decision 4)
      expect(pct1(s.growth)).toBe(27.6);
      const s22 = S('investment.annual', 'cell', 'batteries-midstream', 2022);
      expect(pct1(growth('cagr_2022_2025', s22.W, s.W).value!)).toBe(-3.1);
      // all CIM cells, 2025
      let total = 0;
      let china = 0;
      for (const it of facts.itemsOf(v.idx)) {
        const w = facts.world(v.idx, it, 2025);
        if (!w) continue;
        total += w.value;
        china += facts.value(v.idx, it, 2025, G('CHN'))?.value ?? 0;
      }
      expect(Math.round(usd(total))).toBe(1875);
      expect(pct1(china / total)).toBe(44.2);
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

    it('Red Book: Kazakhstan 37.6% of 2024 uranium mine production', () => {
      const s = S('production.uranium', 'cell', 'nuclear-upstream', 2024);
      expect(s.top1_geo).toBe(G('KAZ'));
      expect(pct1(s.top1_share)).toBe(37.6);
    });

    it('Atlas 2023: Batteries Midstream China share 0.409 (its own row)', () => {
      const v = V('trade.atlas_share');
      const bm = I('cell', 'batteries-midstream');
      expect(r3(facts.value(v.idx, bm.idx, 2023, G('CHN'))!.value)).toBe(0.409);
    });

    it('basket: 255 codes, 99 in more than one cell (max 4); Batteries Midstream 34 codes, 2 shared, 9 sub-sectors', () => {
      const cells = new Map<string, Set<number>>();
      let excluded = 0;
      for (const t of trades.values()) {
        const c = t['trade.codes'].cols;
        for (let i = 0; i < t['trade.codes'].nrow; i++) {
          const k = c.hs6[i]!;
          if (!cells.has(k)) cells.set(k, new Set());
          cells.get(k)!.add(c.item[i]);
        }
        excluded += t['trade.excluded'].nrow;
      }
      expect(cells.size).toBe(255);
      expect([...cells.values()].filter((s) => s.size > 1).length).toBe(99);
      expect(Math.max(...[...cells.values()].map((s) => s.size))).toBe(4);
      const bm = I('cell', 'batteries-midstream');
      const t = tradeOf(bm.idx)!['trade.codes'];
      const rows = [...Array(t.nrow).keys()].filter((i) => t.cols.item[i] === bm.idx);
      expect(rows.length).toBe(34);
      // shared_n = the number of OTHER cells whose basket has the code
      expect(rows.filter((i) => t.cols.shared_n[i] > 0).length).toBe(2);
      expect(new Set(rows.map((i) => t.cols.sub_sector[i])).size).toBe(9);
      expect(excluded).toBe(92);
    });

    it('the headlines agree with the derivations', () => {
      const h = (id: string) => manifest.headlines.find((x) => x.id === id)!;
      const sup = h('supply');
      expect([sup.labels.geo, sup.labels.item, pct1(sup.values.share), Math.round(sup.values.value_kt), Math.round(sup.values.total_kt)]).toEqual(['CHN', 'graphite', 94.4, 2139, 2266]);
      const dep = h('dependence');
      const ev = S('trade.exports', 'cell', 'electric-vehicles-midstream', 2024);
      expect(dep.labels.cell).toBe('electric-vehicles-midstream');
      expect(dep.values.share).toBeCloseTo(ev.china, 12);
      expect(dep.values.total_usd).toBeCloseTo(ev.W, 0);
      const inv = h('investment');
      expect([Math.round(inv.values.total_bn), pct1(inv.values.china), pct1(inv.values.coalition)]).toEqual([1875, 44.2, 52.3]);
      expect([h('concentration').values.n, h('concentration').values.n_total]).toEqual([5, 31]);
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

    it('self-sufficiency and the balance are not reported where EI has no production row', () => {
      const ss = V('consumption.self_sufficiency');
      const bal = V('consumption.balance');
      const gas = I('fuel', 'gas');
      const y = facts.latestYear(ss.idx, gas.idx) as number;
      const nzl = facts.value(ss.idx, gas.idx, y, G('NZL'));
      expect(nzl?.status).toBe('nsr');
      expect(Number.isNaN(nzl?.value)).toBe(true);
      const nb = facts.value(bal.idx, gas.idx, facts.latestYear(bal.idx, gas.idx) as number, G('NZL'));
      if (nb) expect(nb.status).toBe('nsr');
    });

    it('every cell with a dictionary primary measure resolves it (the Snapshot is never empty); fuel Downstream: consumption per person', () => {
      // (Green Hydrogen, Solar and Wind Upstream have country data only per linked mineral: no single primary)
      const missing = model.cells.filter((c) => model.primaryVarsOf(c.slug).length > 0 && !primaryMeasure(model, c)).map((c) => c.slug);
      expect(missing).toEqual([]);
      for (const slug of ['oil-downstream', 'gas-downstream', 'coal-downstream']) {
        const pm = primaryMeasure(model, I('cell', slug));
        expect(pm?.v.id).toBe('consumption.per_capita');
        const y = facts.latestYear(pm!.v.idx, pm!.item.idx) as number;
        expect(facts.rows(pm!.v.idx, pm!.item.idx, y).filter((r) => Number.isFinite(r.value)).length).toBeGreaterThan(50);
        expect(countryFamilies(model, I('cell', slug))).toContain('consumption');
      }
    });
  });
});
