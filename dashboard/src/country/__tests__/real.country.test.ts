// Country Insights on the real export (spec_country.md A12 "Real", C6.4): every fact sheet and drivers file
// passes the contract; quadrants re-derive from values and medians and agree with the Space view's rule
// on the Strategic core chunk for spot-checked sectors; for five economies the Energy Security, Economic
// Opportunity and Strategic Index values equal the Strategic core chunk's; for the United States, Japan
// and Germany the Strategic engine's baseline Partners from the country's viewpoint gives the same top-10
// partners and Partnership Strength values as the sheet (so "Open in Strategic Insights" shows the same);
// every memo entry has its files and a matching facts_sha256.
//
// Runs with COUNTRY_REAL=1 (the integrator's vitest project `country-real`); skipped without the data.

import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { beforeAll, describe, expect, it } from 'vitest';
import { loadWebBundle } from '../../engine/__tests__/parity/harness.ts';
import { runPartner } from '../../engine/engine.ts';
import type { Bundle, Table } from '../../engine/types.ts';
import { median, regionOf } from '../../views/Space/derive.ts';
import {
  checkCountryManifest, checkDriversDoc, checkFactSheet, checkMemoDoc, quadrantOf, type CountryManifest, type FactSheet,
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

  it('quadrants re-derive from values and medians', () => {
    let n = 0;
    for (const s of sheets.values()) {
      for (const c of s.cells.filter((x) => x.placed)) {
        expect(c.quadrant, `${s.iso3} ${c.key}`).toBe(quadrantOf(c.es!.v!, c.es!.med.v!, c.eo!.v!, c.eo!.med.v!));
        n++;
      }
    }
    expect(n).toBeGreaterThan(1000);
  });

  it('corners agree with the Space view rule on the Strategic core chunk (three sectors, every economy)', () => {
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
    for (const cell of ['oil-upstream', 'batteries-midstream', 'solar-downstream']) {
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
        // regionOf() returns the Space view's region label ("Grow: high opportunity, ...")
        expect(c.quadrant, `${p.iso} ${cell}`).toBe((regionOf(p.es, p.eo, esMed, eoMed) ?? '').split(':')[0].toLowerCase());
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
      if (!memo.stale) expect(doc.facts_sha256, e.iso3).toBe(sheets.get(e.iso3)!.checks.facts_sha256);
    }
  });
});
