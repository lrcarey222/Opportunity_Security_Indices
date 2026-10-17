// The real Sectoral Compacts bundle (public/data/aib/, written by scripts/84_export_aib_data.R) against
// the contract: `npm run test:aib-real` (skipped without public/data/aib/manifest.json). The manifest
// and the document pass the runtime checks the app applies; the file is the one the manifest names, its
// name carries its md5 and no stale document is left; the document agrees with the manifest; every code
// the exporter promises a name for has one; compacts are ordered by lost supply; members hold roles;
// nodes and compacts point at each other; the coalition paths run from step 0; the methodology fields
// hang together (compacts only from in-scope nodes, the warning only on the classes it may mark, the
// tiers against the coalition lists, the N-1 readings, the growth flag); no critical supply partner has a
// single substitute and substitutesNeeded agrees with the single substitutes; an unreached target's
// members are the widest greedy set; every variant and growth bound carries its own warning flag. Its
// size is reported.

import { createHash } from 'node:crypto';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { describe, expect, it } from 'vitest';
import {
  AIB_DOC_FILE_RE, AIB_PHYSICAL_WARNING, AIB_TIERS, checkAibManifest, checkCompactsDoc, coalitionMembersOf, countryNamesOf, inStrategicScope, variantsOf, variantWarningsOf,
  type AibDoc, type AibManifest,
} from '../contract.ts';
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
    // the methodology settings, where the manifest states them
    for (const k of ['demandScope', 'candidateUniverse', 'shockSemantics', 'supplyConstraint'] as const) {
      if (manifest[k] !== undefined) expect(doc.meta[k] ?? null, k).toBe(manifest[k]);
    }
    if (doc.meta.shockSemantics && doc.meta.semantics) expect(doc.meta.semantics[0]).toBe(doc.meta.shockSemantics);
    if (doc.meta.supplyConstraint && doc.meta.constraints) expect(doc.meta.constraints[0]).toBe(doc.meta.supplyConstraint);
  });

  it('every code the exporter names has a name', () => {
    const names = countryNamesOf(doc);
    const codes = new Set<string>();
    for (const c of doc.compacts) {
      const co = c.coalition;
      for (const x of [
        ...co.members, ...co.t99, ...(co.greedy ?? []), ...(co.criticalPartners ?? []), ...(co.criticalExternal ?? []), ...(co.criticalExternalMarginal ?? []), ...(co.external ?? []),
        ...c.roles.map((r) => r.iso3),
      ]) {
        codes.add(x);
      }
      for (const r of c.roles) for (const x of r.substitutes ?? []) codes.add(x);
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
        // the instantaneous N-1 is mechanical: coverage x (1 - largest share) (secondShock in an older bundle)
        const inst = f.instantaneousN1 ?? f.secondShock;
        if (typeof inst === 'number' && typeof f.coverage === 'number' && typeof f.largestShare === 'number') {
          expect(near(inst, f.coverage * (1 - f.largestShare)), `${n.id} cap ${f.cap} instantaneousN1`).toBe(true);
        }
        // the true N-1 removes the allocation's largest supplier
        if (typeof f.n1Removed === 'string' && f.largest) expect(f.n1Removed, `${n.id} cap ${f.cap} n1Removed`).toBe(f.largest);
      }
    }
  });

  it('the methodology fields hang together where the export carries them', () => {
    const scoped = doc.meta.strategicScope && doc.meta.strategicScope !== 'all';
    const byId = new Map(doc.nodes.map((n) => [n.id, n]));
    for (const n of doc.nodes) {
      // the warning marks only a LOW INTERVENTION or COORDINATE class
      if (n.warning) {
        expect(n.warning, n.id).toBe(AIB_PHYSICAL_WARNING);
        expect(['LOW INTERVENTION', 'COORDINATE'], `${n.id} warning on ${n.cls}`).toContain(n.cls);
      }
      // the headline class under the headline supply constraint
      if (n.variants && doc.meta.supplyConstraint && doc.meta.supplyConstraint in n.variants) expect(n.variants[doc.meta.supplyConstraint], n.id).toBe(n.cls);
      // the growth flag: COORDINATE under some bounds and another class under others
      if (n.growth && typeof n.coordinateSensitiveToGrowth === 'boolean') {
        const cls = n.growth.map((g) => g.cls);
        expect(n.coordinateSensitiveToGrowth, n.id).toBe(cls.includes('COORDINATE') && new Set(cls.filter((c) => c !== null)).size > 1);
      }
    }
    for (const c of doc.compacts) {
      const co = c.coalition;
      // compacts are formed only from nodes of the strategic scope
      if (scoped) for (const id of c.nodes) expect(inStrategicScope(byId.get(id)!), `${c.id} node ${id} in scope`).toBe(true);
      // the physical warnings name exactly the compact's warned nodes
      if (c.physicalWarnings) {
        const warned = c.nodes.map((id) => byId.get(id)!).filter((n) => n.warning).map((n) => n.label).sort();
        expect([...c.physicalWarnings].sort(), `${c.id} physicalWarnings`).toEqual(warned);
      }
      // critical supply partners are members; external dependencies are not
      for (const x of co.criticalPartners ?? []) expect(co.members, `${c.id} critical partner ${x}`).toContain(x);
      for (const x of [...(co.criticalExternal ?? []), ...(co.external ?? [])]) expect(co.members, `${c.id} external ${x}`).not.toContain(x);
      // the exact minimum is never larger than the greedy coalition at the same target
      if (/^exact/i.test(co.method ?? '') && co.t95.length && co.greedy?.length) expect(co.t95.length, `${c.id} exact <= greedy`).toBeLessThanOrEqual(co.greedy.length);
      for (const r of c.roles) {
        if (r.tier === undefined || r.tier === null) continue;
        expect(AIB_TIERS as readonly string[], `${c.id} ${r.iso3} tier`).toContain(r.tier);
        // members are core coordinating members or critical supply partners
        expect(r.member, `${c.id} ${r.iso3} member vs tier ${r.tier}`).toBe(r.tier === 'core coordinating member' || r.tier === 'critical supply partner');
        if (r.tier === 'critical supply partner') expect(co.criticalPartners ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
        if (r.tier === 'critical external dependency') expect(co.criticalExternal ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
        if (r.tier === 'critical external dependency (marginal)') expect(co.criticalExternalMarginal ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
      }
    }
  });

  it('no critical supply partner has a single substitute; substitutesNeeded agrees with the single substitutes', () => {
    for (const c of doc.compacts) {
      const co = c.coalition;
      for (const r of c.roles) {
        const at = `${c.id} ${r.iso3}`;
        if (r.tier === 'critical supply partner') {
          // by definition no single other candidate replaces it: several economies together, or none
          expect(r.substitutes ?? [], `${at} single substitutes`).toEqual([]);
          if (r.substitutesNeeded !== undefined) expect(r.substitutesNeeded === null || r.substitutesNeeded >= 2, `${at} substitutesNeeded ${r.substitutesNeeded}`).toBe(true);
        }
        if (r.substitutesNeeded === undefined) continue;
        // one addition restores the target exactly when a single substitute exists
        if ((r.substitutes ?? []).length) expect(r.substitutesNeeded, `${at} has single substitutes`).toBe(1);
        if (r.substitutesNeeded === 1) expect((r.substitutes ?? []).length, `${at} needs one addition`).toBeGreaterThan(0);
        // only members of a reached coalition are tested
        if (!r.member) expect(r.substitutesNeeded, `${at} non-member`).toBeNull();
        if (r.member && r.substitutesNeeded !== null && r.substitutesNeeded >= 2) expect(co.criticalPartners ?? [], at).toContain(r.iso3);
      }
      // the critical partners carry the critical supply partner tier
      for (const x of co.criticalPartners ?? []) expect(c.roles.filter((r) => r.iso3 === x).map((r) => r.tier), `${c.id} ${x}`).toContain('critical supply partner');
      // a marginal critical external dependency is not also a material one
      for (const x of co.criticalExternalMarginal ?? []) expect(co.criticalExternal ?? [], `${c.id} ${x}`).not.toContain(x);
    }
  });

  it('the coalition: the members of an unreached target are the widest greedy set; the tie-break flag is a boolean', () => {
    for (const c of doc.compacts) {
      const co = c.coalition;
      expect(coalitionMembersOf(c).length, `${c.id} members`).toBeGreaterThan(0);
      if (/^greedy_sufficient_coalition/i.test(co.method ?? '')) {
        expect(co.t95, `${c.id} t95 of an unreached target`).toEqual([]);
        expect(co.criticalPartners ?? [], `${c.id} no critical partner when unreached`).toEqual([]);
      } else if (co.method) {
        expect(co.method, c.id).toMatch(/^exact/);
        expect([...co.members].sort(), `${c.id} members are the exact coalition`).toEqual([...co.t95].sort());
      }
      if (co.robustTieBreakSolved !== undefined) expect(typeof co.robustTieBreakSolved, c.id).toBe('boolean');
      if (co.associatesReach99 === true) expect(co.reached99 || co.t99.length > 0, `${c.id} associates reach the top target`).toBe(true);
    }
  });

  it('every class shown carries its own warning flag: one per variant, one per growth bound, only on the classes the warning may mark', () => {
    const marked = ['LOW INTERVENTION', 'COORDINATE'];
    for (const n of doc.nodes) {
      const vw = variantWarningsOf(n);
      if (vw !== null) {
        expect(Object.keys(vw).sort(), `${n.id} variantWarnings keys`).toEqual(Object.keys(variantsOf(n)).sort());
        for (const [k, w] of Object.entries(vw)) if (w) expect(marked, `${n.id} ${k} warned on ${variantsOf(n)[k]}`).toContain(variantsOf(n)[k]);
        // the headline constraint's flag is the headline warning
        const head = doc.meta.supplyConstraint;
        if (head && head in vw) expect(vw[head], `${n.id} headline flag`).toBe(!!n.warning);
      }
      for (const g of n.growth ?? []) {
        if (g.warning === undefined) continue;
        expect(typeof g.warning, `${n.id} ${g.label}`).toBe('boolean');
        if (g.warning) expect(marked, `${n.id} ${g.label} warned on ${g.cls}`).toContain(g.cls);
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
