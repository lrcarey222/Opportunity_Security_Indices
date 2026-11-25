// Partner eligibility against the real posture bundle (public/data/posture; external review readiness,
// 2026-11-23): for non-aligned reporters the engine ranks China and Russia among the "trusted" partners
// (R supply_structure.R sp_nonaligned_trust). The split (partnerEligibility.ts) must keep them out of every
// eligible list (Sri Lanka, Kenya, Bangladesh), keep them visible as suppliers outside the trusted network,
// and leave the United States' lists unchanged. Also checks the Allied Strategy rows and the fence reason built
// on the split. Skipped without an export, like real.posture.test.ts.

import { existsSync, readFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { checkPostureDetail, checkPostureManifest, decodeCore, type PostureCell } from '../contract.ts';
import { countryNames } from '../derive.ts';
import { fenceGroups, fenceReason } from '../fences.ts';
import { networkOf, splitPartners } from '../partnerEligibility.ts';
import { alliedRows } from '../sections.ts';

const DIR = resolve(process.cwd(), 'public/data/posture');
const read = (f: string): unknown => JSON.parse(readFileSync(join(DIR, f), 'utf8'));
const NOT_ELIGIBLE = ['CHN', 'RUS'];

describe.skipIf(!existsSync(join(DIR, 'manifest.json')))('partner eligibility on the real posture bundle', () => {
  const manifest = checkPostureManifest(read('manifest.json'));
  const core = decodeCore(read(manifest.core) as Record<string, unknown>);
  const net = networkOf(core.countries);
  const names = countryNames(core);
  const detailOf = (iso3: string) => checkPostureDetail(read(manifest.details[iso3]));
  const cellsOf = (iso3: string): PostureCell[] => core.cells.filter((c) => c.iso3 === iso3);

  it('China and Russia are outside the trusted network', () => {
    for (const x of NOT_ELIGIBLE) expect(net.has(x), x).toBe(false);
  });

  it.each(['LKA', 'KEN', 'BGD'])('%s: no China or Russia among the eligible partners of any step', (iso3) => {
    const d = detailOf(iso3);
    const withPartners = d.cells.filter((c) => c.partners.length);
    expect(withPartners.length).toBeGreaterThan(0);
    for (const c of withPartners) {
      const { eligible, outside } = splitPartners(c.partners, net);
      for (const p of eligible) expect(NOT_ELIGIBLE, `${iso3} ${c.tech}|${c.stage}`).not.toContain(p.iso3);
      expect(eligible.length + outside.length).toBe(c.partners.length);
    }
    // the Allied Strategy rows and the fence reasons built on the split
    for (const r of alliedRows(cellsOf(iso3), d, core.division, names, net)) for (const p of r.partners) expect(NOT_ELIGIBLE, `${iso3} ${r.key}`).not.toContain(p.iso3);
    const g = fenceGroups(cellsOf(iso3), d);
    for (const n of [...g.compete, ...g.secure]) {
      const why = fenceReason(n, core, names) ?? '';
      expect(why, `${iso3} ${n.key}`).not.toMatch(/^Best-placed partner: (China|Russia)/);
    }
  });

  it('Sri Lanka: China and Russia stay listed, as suppliers outside the trusted network', () => {
    const outside = new Set(detailOf('LKA').cells.flatMap((c) => splitPartners(c.partners, net).outside.map((p) => p.iso3)));
    for (const x of NOT_ELIGIBLE) expect(outside.has(x), x).toBe(true);
    const rows = alliedRows(cellsOf('LKA'), detailOf('LKA'), core.division, names, net);
    expect(rows.some((r) => r.outside.some((p) => p.iso3 === 'CHN'))).toBe(true);
    expect(rows.some((r) => r.noEligible)).toBe(true);
  });

  it('United States: the eligible partners are the engine partners, unchanged', () => {
    const d = detailOf('USA');
    const withPartners = d.cells.filter((c) => c.partners.length);
    expect(withPartners.length).toBeGreaterThan(0);
    for (const c of withPartners) {
      const { eligible, outside } = splitPartners(c.partners, net);
      expect(eligible, `${c.tech}|${c.stage}`).toEqual(c.partners);
      expect(outside).toEqual([]);
    }
  });
});
