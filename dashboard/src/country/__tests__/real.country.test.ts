// Country Insights on the real export (spec_country.md A12 "Real", C6.4): every fact sheet and drivers file
// passes the contract; quadrants re-derive from the plotted positions and the country's own median sector
// (2026-10-15), and each sector's above / below the median economy agrees with the Space view's medians on
// the Strategic core chunk for spot-checked sectors; for five economies the Energy Security, Economic
// Opportunity and Strategic Index values equal the Strategic core chunk's; for the United States, Japan
// and Germany the Strategic engine's baseline Partners from the country's viewpoint gives the same top-10
// partners and Partnership Strength values as the sheet (so "Open in Strategic Insights" shows the same);
// every memo entry has its files and a facts_sha256 matching its sheet, or a rule note when it was kept
// under the earlier quadrant rule. Six stages (schema 2, 2026-10-28): 66 sectors per sheet, the shipped
// no-Energy-Security list, the CM-FDI stages, NIPO not staged, allied picks at a supply stage, the hidden
// memos and the gzip budgets.
//
// Runs with COUNTRY_REAL=1 (the integrator's vitest project `country-real`); skipped without the data.

import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'node:zlib';
import { beforeAll, describe, expect, it } from 'vitest';
import { loadWebBundle } from '../../engine/__tests__/parity/harness.ts';
import { runPartner } from '../../engine/engine.ts';
import type { Bundle, Table } from '../../engine/types.ts';
import { SUPPLY_STAGES } from '../../lib/stages.ts';
import { median } from '../../views/Space/derive.ts';
import {
  checkCountryManifest, checkDriversDoc, checkFactSheet, checkMemoDoc, COUNTRY_BUDGET_GZ, COUNTRY_CELL_KEYS, countryMedianOf, legacyCornerOf, NO_ES_CELLS, stageOfCellKey,
  type CountryManifest, type FactSheet,
} from '../contract.ts';

const dataDir = fileURLToPath(new URL('../../../public/data/', import.meta.url));
const countryDir = join(dataDir, 'country');
const wanted = process.env.COUNTRY_REAL === '1';
const hasData = existsSync(join(countryDir, 'manifest.json')) && existsSync(join(dataDir, 'manifest.json'));

const readJson = <T>(file: string): T => JSON.parse(readFileSync(join(countryDir, file), 'utf8')) as T;
const slug = (tech: string, stage: string) => `${tech.toLowerCase().replace(/[^a-z0-9]+/g, '-')}-${stage.toLowerCase()}`;

function col<T = unknown>(t: Table, name: string): ArrayLike<T> {
  const c = (t.cols as Record<string, unknown>)[name];
  if (!c) throw new Error(`no column ${name}`);
  return c as ArrayLike<T>;
}

describe.skipIf(!wanted || !hasData)('Country Insights on the real export', () => {
  let m: CountryManifest;
  const sheets = new Map<string, FactSheet>();
  let bundle: Bundle;

  beforeAll(() => {
    m = readJson<CountryManifest>('manifest.json');
    for (const e of m.countries) if (e.file) sheets.set(e.iso3, readJson<FactSheet>(e.file));
    bundle = loadWebBundle(dataDir, ['core', 'partner']);
  }, 600_000);

  it('the manifest passes, is validated and names existing files', () => {
    expect(checkCountryManifest(m)).toEqual([]);
    expect(m.validated).toBe(true);
    expect(m.counts.core).toBe(31);
    for (const e of m.countries) {
      if (e.file) expect(existsSync(join(countryDir, e.file)), e.file).toBe(true);
      if (e.drivers) expect(existsSync(join(countryDir, e.drivers)), e.drivers).toBe(true);
    }
  });

  it('every sheet and drivers file passes the contract', () => {
    for (const e of m.countries.filter((c) => c.file)) {
      const s = sheets.get(e.iso3)!;
      expect(checkFactSheet(s, e), e.iso3).toEqual([]);
      if (e.drivers) expect(checkDriversDoc(readJson(e.drivers), s), e.iso3).toEqual([]);
    }
  });

  it('six stages (schema 2, 2026-10-28; Legacy OSI corner names, schema 3, 2026-10-29): 66 sectors, the shipped no-Energy-Security list, CM-FDI stages, NIPO not staged, picks at a supply stage', () => {
    expect(m.schema_version).toBe(3);
    expect(m.no_es_cells).toEqual(NO_ES_CELLS);
    expect(m.cmfdi_stages).toEqual(['Components', 'Assembly']);
    expect(m.label).toMatch(/^AI-generated from the data and checked against it; NEIS baseline as of /);
    const picksByStage = new Map<string, number>();
    for (const s of sheets.values()) {
      expect(s.cells.map((c) => c.key), s.iso3).toEqual(COUNTRY_CELL_KEYS);
      expect(s.no_es_cells, s.iso3).toEqual(NO_ES_CELLS);
      expect(s.meta.cmfdi_stages, s.iso3).toEqual(['Components', 'Assembly']);
      expect(s.cells.filter((c) => c.placed).length, s.iso3).toBeLessThanOrEqual(58);
      for (const k of NO_ES_CELLS) expect(s.cells.find((c) => c.key === k)!.es, `${s.iso3} ${k}`).toBeNull();
      expect(s.policy.coverage.cols).toEqual([...SUPPLY_STAGES]);
      const n = s.policy.nipo;
      if (n) {
        // the NIPO run is still three-stage: counts kept, no sector lists (design K1)
        expect(n.staged, s.iso3).toBe(false);
        expect(n.policies, s.iso3).toEqual([]);
        expect(n.staged_d).toMatch(/not yet matched to the six supply-chain stages/);
      }
      if (s.pick) {
        const st = stageOfCellKey(s.pick.cell)!;
        picksByStage.set(st, (picksByStage.get(st) ?? 0) + 1);
        // any stage but Deployment; the resource screen applies to Extraction only
        expect(st, s.iso3).not.toBe('Deployment');
        const pc = s.cells.find((c) => c.key === s.pick!.cell)!;
        expect(pc.si.pick_exclusion ?? null, `${s.iso3} ${pc.key}`).toBeNull();
      }
      for (const c of s.cells) if (c.si.pick_exclusion === 'no resource base') expect(c.stage, `${s.iso3} ${c.key}`).toBe('Extraction');
    }
    // the published export (handoff-r-country-export): USA 57 placed (Oil - Recovery has a constant Energy Security, so it is not placed), its pick solar-components; no Deployment pick
    expect(sheets.get('USA')!.cells.filter((c) => c.placed).length).toBe(57);
    expect(sheets.get('USA')!.pick?.cell).toBe('solar-components');
    expect(picksByStage.get('Deployment') ?? 0).toBe(0);
    expect(picksByStage.get('Extraction') ?? 0).toBeGreaterThan(0);
    // the memos written on three-stage sheets stay hidden (being revised, design K3)
    for (const e of m.countries.filter((c) => c.memo)) expect(e.memo!.stale, e.iso3).toBe(true);
  });

  it('the gzip sizes are within the six-stage budgets (COUNTRY_BUDGET_GZ, as R measures: gzip of the JSON text)', () => {
    let max = 0;
    for (const e of m.countries.filter((c) => c.file)) max = Math.max(max, gzipSync(readFileSync(join(countryDir, e.file!))).length);
    expect(max).toBeLessThanOrEqual(COUNTRY_BUDGET_GZ.sheet);
    expect(gzipSync(readFileSync(join(countryDir, 'manifest.json'))).length).toBeLessThanOrEqual(COUNTRY_BUDGET_GZ.manifest);
    for (const e of m.countries.filter((c) => c.trade_monthly)) {
      expect(gzipSync(readFileSync(join(countryDir, e.trade_monthly!))).length, e.iso3).toBeLessThanOrEqual(COUNTRY_BUDGET_GZ.tradeMonthly);
    }
  });

  it("Legacy OSI corners (schema 3) re-derive from the plotted positions and the country's median sector", () => {
    let n = 0;
    for (const s of sheets.values()) {
      const cm = countryMedianOf(s.cells);
      for (const c of s.cells.filter((x) => x.placed)) {
        expect(c.legacy_corner, `${s.iso3} ${c.key}`).toBe(legacyCornerOf(c.es!.pos!, cm.es!, c.eo!.pos!, cm.eo!));
        n++;
      }
    }
    expect(n).toBeGreaterThan(1000);
  });

  it("each sector's side of the median economy agrees with the Space view's medians on the Strategic core chunk (three sectors, every economy)", () => {
    const core = (bundle.baseline as Record<string, Record<string, Table>>).core;
    const es = core.es;
    const eo = core.eo;
    const key = (t: Table, i: number) => `${col<string>(t, 'iso3c')[i]}|${slug(col<string>(t, 'tech')[i], col<string>(t, 'supply_chain')[i])}`;
    const eoAt = new Map<string, number>();
    const eoRank = new Map<string, boolean>();
    for (let i = 0; i < eo.nrow; i++) {
      eoAt.set(key(eo, i), col<number>(eo, 'value')[i]);
      eoRank.set(key(eo, i), !!col<number | boolean>(eo, 'rankable')[i]);
    }
    // six stages (schema 2): an extraction, a refining and a deployment sector (each placeable for 85 or more economies)
    for (const cell of ['oil-extraction', 'batteries-refining', 'solar-deployment']) {
      const pts: { iso: string; es: number; eo: number }[] = [];
      for (let i = 0; i < es.nrow; i++) {
        const k = key(es, i);
        if (!k.endsWith(`|${cell}`) || !col<number | boolean>(es, 'rankable')[i] || !eoRank.get(k)) continue;
        const x = col<number>(es, 'value')[i];
        const y = eoAt.get(k);
        if (Number.isFinite(x) && y !== undefined && Number.isFinite(y)) pts.push({ iso: k.split('|')[0], es: x, eo: y });
      }
      const esMed = median(pts.map((p) => p.es));
      const eoMed = median(pts.map((p) => p.eo));
      let n = 0;
      for (const p of pts) {
        const c = sheets.get(p.iso)?.cells.find((x) => x.key === cell);
        if (!c || !c.placed) continue;
        // the cross-country context the tooltips keep: above or below the sector's median economy
        expect(c.es!.side, `${p.iso} ${cell}`).toBe(p.es >= esMed ? 'above' : 'below');
        expect(c.eo!.side, `${p.iso} ${cell}`).toBe(p.eo >= eoMed ? 'above' : 'below');
        n++;
      }
      expect(n, cell).toBeGreaterThan(30);
    }
  });

  it('Energy Security, Economic Opportunity and Strategic Index equal the Strategic core chunk for five economies', () => {
    const core = (bundle.baseline as Record<string, Record<string, unknown>>).core;
    const tables: [string, Table, string][] = [
      ['es', core.es as Table, 'value'],
      ['eo', core.eo as Table, 'value'],
      ['si', (core.si as Record<string, Table>).cells, 'strategic_index'],
    ];
    for (const iso of ['USA', 'JPN', 'DEU', 'KOR', 'BRA']) {
      const s = sheets.get(iso)!;
      for (const [what, t, vcol] of tables) {
        const isoCol = col<string>(t, 'iso3c');
        let n = 0;
        for (let i = 0; i < t.nrow; i++) {
          if (isoCol[i] !== iso) continue;
          const v = col<number>(t, vcol)[i];
          if (!Number.isFinite(v)) continue;
          const c = s.cells.find((x) => x.key === slug(col<string>(t, 'tech')[i], col<string>(t, 'supply_chain')[i]))!;
          const mine = what === 'si' ? c.si.v : c[what as 'es' | 'eo']!.v;
          expect(mine, `${iso} ${c.key} ${what}`).toBe(v);
          n++;
        }
        expect(n, `${iso} ${what}`).toBeGreaterThan(0);
      }
    }
  });

  it('the engine at the baseline gives the same top-10 partners and Partnership Strength (USA, JPN, DEU)', () => {
    const d = runPartner(bundle, bundle.baselineParams).psi.dyad;
    const rep = col<string>(d, 'reporter_iso');
    const par = col<string>(d, 'partner_iso');
    const psi = col<number>(d, 'psi');
    const rank = col<number>(d, 'rank');
    for (const iso of ['USA', 'JPN', 'DEU']) {
      const s = sheets.get(iso)!;
      const mine = new Map<string, { rank: number; psi: number }>();
      for (let i = 0; i < d.nrow; i++) {
        if (rep[i] === iso && Number.isFinite(rank[i]) && rank[i] > 0 && rank[i] < 1e9) mine.set(par[i], { rank: rank[i], psi: psi[i] });
      }
      const top = [...mine.entries()].sort((a, b) => a[1].rank - b[1].rank).slice(0, 10);
      expect(top.map(([p]) => p), iso).toEqual(s.partners.psi_order.slice(0, 10));
      for (const [p, x] of top) {
        const row = s.partners.rows.find((r) => r.iso3 === p)!;
        expect(Math.abs((row.psi.v ?? NaN) - x.psi), `${iso} ${p}`).toBeLessThanOrEqual(1e-12);
        expect(row.rank.v, `${iso} ${p}`).toBe(x.rank);
      }
    }
  }, 600_000);

  it('every memo entry has its files and a facts_sha256 matching its sheet', () => {
    for (const e of m.countries.filter((c) => c.memo)) {
      const memo = e.memo!;
      expect(e.tier, e.iso3).toBe('core');
      for (const f of [memo.json, memo.pdf, memo.docx, memo.md]) if (f) expect(existsSync(join(countryDir, f)), f).toBe(true);
      const doc = readJson<{ facts_sha256: string }>(memo.json);
      expect(checkMemoDoc(doc, e.iso3)).toEqual([]);
      if (!memo.stale) {
        expect(doc.facts_sha256, e.iso3).toBe(memo.facts_sha256);
        // kept under the earlier quadrant rule (scripts/88): written on an earlier sheet, with its notice
        if (doc.facts_sha256 !== sheets.get(e.iso3)!.checks.facts_sha256) expect(memo.rule_note, e.iso3).toBe('quadrant_rule_2026_10_15');
      }
    }
  });
});
