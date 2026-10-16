// The real Sectoral Compacts bundle (public/data/aib/, written by scripts/84_export_aib_data.R) against
// the contract: `npm run test:aib-real` (skipped without public/data/aib/manifest.json). The manifest
// and the document pass the runtime checks the app applies; the file is the one the manifest names, its
// name carries its md5 and no stale document is left; the document agrees with the manifest; every code
// the exporter promises a name for has one; compacts are ordered by lost supply; members hold roles;
// nodes and compacts point at each other; the coalition paths run from step 0. Its size is reported.

import { createHash } from 'node:crypto';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { describe, expect, it } from 'vitest';
import { AIB_DOC_FILE_RE, checkAibManifest, checkCompactsDoc, countryNamesOf, type AibDoc, type AibManifest } from '../contract.ts';
import { REAL_DIR } from './fixture.ts';

const have = existsSync(resolve(REAL_DIR, 'manifest.json'));

describe.skipIf(!have)('the real Sectoral Compacts bundle (public/data/aib)', () => {
  const manifest = have ? (JSON.parse(readFileSync(resolve(REAL_DIR, 'manifest.json'), 'utf8')) as AibManifest) : (null as unknown as AibManifest);
  const file = have ? resolve(REAL_DIR, manifest.files.compacts) : '';
  const bytes = have && existsSync(file) ? readFileSync(file) : Buffer.alloc(0);
  const doc = bytes.length ? (JSON.parse(bytes.toString('utf8')) as AibDoc) : (null as unknown as AibDoc);

  it('the manifest passes the contract and names an existing, content-addressed document', () => {
    expect(checkAibManifest(manifest)).toEqual([]);
    expect(existsSync(file)).toBe(true);
    const hash = createHash('md5').update(bytes).digest('hex').slice(0, 8);
    expect(manifest.files.compacts).toBe(`compacts.${hash}.json`);
    expect(readdirSync(REAL_DIR).filter((f) => AIB_DOC_FILE_RE.test(f))).toEqual([manifest.files.compacts]);
  });

  it('the document passes the contract', () => {
    expect(checkCompactsDoc(doc)).toEqual([]);
  });

  it('the document agrees with the manifest', () => {
    expect([doc.meta.universe, doc.meta.headlineShock, doc.meta.headlineScenario]).toEqual([manifest.universe, manifest.headlineShock, manifest.headlineScenario]);
    expect(doc.meta.shocks.map((s) => s.id)).toContain(doc.meta.headlineShock);
    expect(doc.meta.scenarios).toContain(doc.meta.headlineScenario);
  });

  it('every code the exporter names has a name', () => {
    const names = countryNamesOf(doc);
    const codes = new Set<string>();
    for (const c of doc.compacts) {
      for (const x of [...c.coalition.members, ...c.coalition.t99, ...c.roles.map((r) => r.iso3)]) codes.add(x);
      for (const e of c.trade) codes.add(e.from).add(e.to);
      for (const e of c.investment) codes.add(e.investor).add(e.host);
    }
    for (const n of doc.nodes) for (const x of [n.baseLargest, n.largest, ...n.hosts]) if (x) codes.add(x);
    expect([...codes].filter((x) => !names[x]).sort()).toEqual([]);
  });

  it('compacts are ordered by lost supply; members hold roles; nodes and compacts point at each other', () => {
    const lost = doc.compacts.map((c) => c.metrics.lostUsd ?? 0);
    expect(lost).toEqual([...lost].sort((a, b) => b - a));
    for (const c of doc.compacts) {
      const withRole = new Set(c.roles.map((r) => r.iso3));
      expect(c.coalition.members.filter((m) => !withRole.has(m)), `${c.id} members without a role`).toEqual([]);
      for (const id of c.nodes) expect(doc.nodes.find((n) => n.id === id)?.compact, `${c.id} node ${id}`).toBe(c.id);
    }
    for (const n of doc.nodes.filter((x) => x.compact)) expect(doc.compacts.find((c) => c.id === n.compact)?.nodes, n.id).toContain(n.id);
  });

  it('the additive fields agree with each other where the export carries them', () => {
    // each value is rounded to 4 decimals by the exporter
    const near = (a: number, b: number) => Math.abs(a - b) <= 2e-4;
    for (const c of doc.compacts) {
      const m = c.metrics;
      if (typeof m.compactGain === 'number' && typeof m.anchorCompact === 'number' && typeof m.anchorFragmented === 'number') {
        expect(near(m.compactGain, m.anchorCompact - m.anchorFragmented), `${c.id} compactGain`).toBe(true);
      }
      // the trade compact keeps every economy's existing response and adds the members' relationships;
      // the investment response adds capacity on top of it
      if (typeof m.trade === 'number' && typeof m.existing === 'number') expect(m.trade, `${c.id} trade >= existing`).toBeGreaterThanOrEqual(m.existing - 2e-4);
      if (typeof m.investment === 'number' && typeof m.trade === 'number') expect(m.investment, `${c.id} investment >= trade`).toBeGreaterThanOrEqual(m.trade - 2e-4);
      if (c.type.startsWith('compact unnecessary')) expect(typeof m.compactGain, `${c.id} an unnecessary compact has a gain`).toBe('number');
      if (c.secondaryClass) {
        expect(doc.meta.classes, `${c.id} secondaryClass`).toContain(c.secondaryClass);
        expect(c.classMix ?? '', `${c.id} classMix names the secondary class`).toContain(c.secondaryClass);
      }
    }
    for (const n of doc.nodes) {
      const caps = n.frontier.map((f) => f.cap);
      for (const k of ['tightestCostless', 'minRequiredDiversification'] as const) {
        const v = n[k];
        if (typeof v === 'number') expect(caps.some((cap) => near(cap, v)), `${n.id} ${k} is a tested limit`).toBe(true);
      }
      for (const f of n.frontier) {
        if (typeof f.secondShock === 'number' && typeof f.coverage === 'number' && typeof f.largestShare === 'number') {
          expect(near(f.secondShock, f.coverage * (1 - f.largestShare)), `${n.id} cap ${f.cap} secondShock`).toBe(true);
        }
      }
    }
  });

  it('every coalition path starts at step 0 with the anchors and grows by one economy a step', () => {
    for (const c of doc.compacts) {
      if (!c.path.length) continue;
      const steps = [...c.path].sort((a, b) => a.step - b.step);
      expect(steps[0].step, c.id).toBe(0);
      expect(steps[0].added, c.id).toBeNull();
      steps.forEach((s, i) => {
        if (i === 0) return;
        expect(s.members.length, `${c.id} step ${s.step}`).toBe(steps[i - 1].members.length + 1);
        expect(s.members, `${c.id} step ${s.step}`).toContain(s.added);
      });
    }
  });

  it('reports its size', () => {
    const gz = gzipSync(bytes, { level: 9 }).length;
    console.log(
      `aib bundle: ${manifest.files.compacts} ${(bytes.length / 1024).toFixed(0)} kB (${(gz / 1024).toFixed(1)} kB gz); ` +
        `${doc.compacts.length} compacts, ${doc.nodes.length} nodes, ${Object.keys(countryNamesOf(doc)).length} economies named`,
    );
    expect(gz).toBeLessThan(2 * 1024 * 1024);
  });
});
