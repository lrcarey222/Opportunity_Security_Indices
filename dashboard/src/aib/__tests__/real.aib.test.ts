// The real Sectoral Compacts bundle (public/data/aib/, written by scripts/84_export_aib_data.R) against
// the contract: `npm run test:aib-real` (skipped without public/data/aib/manifest.json). The manifest
// and the document pass the runtime checks the app applies; the file is the one the manifest names, its
// name carries its md5 and no stale document is left; the document agrees with the manifest; every code
// the exporter promises a name for has one; compacts are ordered by lost supply; members hold roles;
// nodes and compacts point at each other; the coalition paths run from step 0; the methodology fields
// hang together (compacts only from in-scope nodes, the warning only on the classes it may mark, the
// tiers against the coalition lists, the N-1 readings, the growth flag); no critical supply partner has a
// single substitute and substitutesNeeded agrees with the single substitutes; an unreached target's
// members are the widest greedy set; every variant and growth bound carries its own warning flag; the
// diversification scenario's status counts (38 diversified, 1 within the cap, 35 already as diversified, 3
// not assessed), its capped suppliers named, its run fields null exactly where it did not run. Its size is
// reported.

import { createHash } from 'node:crypto';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { describe, expect, it } from 'vitest';
import {
  AIB_DOC_FILE_RE, AIB_PHYSICAL_WARNING, AIB_TIERS, checkAibManifest, checkCompactsDoc, coalitionMembersOf, countryNamesOf, diversifyShockIdOf, indicesOf, inStrategicScope, supplyScenariosOf, variantsOf,
  variantWarningsOf, type AibDoc, type AibManagedScenarioDoc, type AibManifest,
} from '../contract.ts';
import { diversifyRow, diversifyTotals, relationshipsOf } from '../derive/diversify.ts';
import { everyShock } from '../derive/network.ts';
import { compactTechs, compactTitle, techTitle } from '../derive/format.ts';
import { NOT_IN_BASKET, nodeInTech, osiCellHref, osiCellOf, osiFields, profiledCellsOfNode, taxonomyOf, techKeysOfNode, technologiesOfNodes } from '../derive/taxonomy.ts';
import { DEPLOYMENT_STAGE, parseCellSlug } from '../../lib/stages.ts';
import { decodeCore, POSTURE_SCHEMA_VERSION } from '../../posture/contract.ts';
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
    expect(supplyScenariosOf(doc.meta)).toContain(doc.meta.headlineScenario);
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
        // members are core coordinating members, critical supply partners or prospective hosts
        expect(r.member, `${c.id} ${r.iso3} member vs tier ${r.tier}`).toBe(
          r.tier === 'core coordinating member' || r.tier === 'critical supply partner' || r.tier === 'prospective host',
        );
        if (r.tier === 'critical supply partner') expect(co.criticalPartners ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
        // a prospective host is in only for simulated capacity: listed as such, never critical on current supply
        if (r.tier === 'prospective host') {
          expect(co.prospectiveHosts ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
          expect(co.criticalPartners ?? [], `${c.id} ${r.iso3}`).not.toContain(r.iso3);
          expect(r.looCritical, `${c.id} ${r.iso3} looCritical`).not.toBe(true);
        }
        if (r.tier === 'critical external dependency') expect(co.criticalExternal ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
        if (r.tier === 'critical external dependency (marginal)') expect(co.criticalExternalMarginal ?? [], `${c.id} ${r.iso3}`).toContain(r.iso3);
      }
    }
  });

  it('the taxonomy hangs together: six-stage crosswalk ids, OSI technologies with Sector profiles, the basket-less bucket, the dictionary provenance; titles keep every node label whole', () => {
    const tx = taxonomyOf(doc.meta);
    const byId = new Map(doc.nodes.map((n) => [n.id, n]));
    for (const n of doc.nodes) {
      // every crosswalk id is a six-stage OSI cell of a technology the Sector pages profile; every sub-component sits in one of the node's cells
      for (const c of n.yfCells) {
        expect(osiCellOf(c), `${n.id} cell ${c}`).not.toBeNull();
        expect(osiCellHref({ slug: c }), `${n.id} cell ${c} Sector profile`).toMatch(/^\/sector\/[a-z-]+\/(extraction|refining|components|assembly|deployment|recovery)$/);
      }
      for (const sc of n.yfSubcomponents) expect(n.yfCells.some((c) => sc.startsWith(`${c}::`)), `${n.id} sub-component ${sc}`).toBe(true);
      // its Sector links are the distinct profiles of its six-stage cells, in their order
      expect(profiledCellsOfNode(n).map((c) => c.href), `${n.id} Sector links`).toEqual([...new Set(n.yfCells.map((c) => osiCellHref({ slug: c })!))]);
      // its technologies are those of its cells; a node with none is in the 'Not in the OSI basket' bucket, never in a technology
      expect(techKeysOfNode(n), n.id).toEqual(n.yfCells.length ? [...new Set(n.yfCells.map((c) => osiCellOf(c)!.tech))].sort((a, b) => a.localeCompare(b, 'en')) : [NOT_IN_BASKET]);
      // the three-stage cells an export may still write (no longer read) stay three-stage
      for (const c of n.yfCellsLegacy ?? []) expect(parseCellSlug(c)?.legacy, `${n.id} three-stage cell ${c}`).toBe(true);
      if (tx.dictionary && indicesOf(doc.meta).length) {
        // the provenance: the node's index lists its dictionary sector (shown only as dictionary_sector)
        const ix = indicesOf(doc.meta).find((x) => x.code === n.indexCode) ?? indicesOf(doc.meta).find((x) => n.techs.every((t) => x.sectors.includes(t)));
        expect(ix, `${n.id} index`).toBeTruthy();
        for (const t of n.techs) expect(ix!.sectors, `${n.id} sector ${t}`).toContain(t);
        expect(osiFields(tx, n).dictionarySector, n.id).toBe(n.techs.join('; '));
      }
    }
    // the bucket: every node without a cell, and only those
    const bare = doc.nodes.filter((n) => !n.yfCells.length);
    expect(doc.nodes.filter((n) => nodeInTech(n, NOT_IN_BASKET)).map((n) => n.id)).toEqual(bare.map((n) => n.id));
    expect(technologiesOfNodes(doc.nodes).includes(NOT_IN_BASKET)).toBe(bare.length > 0);
    console.log(`AIB nodes: ${doc.nodes.length}; not in the OSI basket: ${bare.length}; OSI technologies: ${technologiesOfNodes(doc.nodes).filter((t) => t !== NOT_IN_BASKET).join(', ')}`);
    for (const c of doc.compacts) {
      if (tx.dictionary && indicesOf(doc.meta).length) {
        const want = [...new Set(c.technologies.map((t) => indicesOf(doc.meta).find((x) => x.sectors.includes(t))?.code))].filter(Boolean).sort();
        expect([...(c.indices ?? [])].sort(), `${c.id} indices`).toEqual(want);
      } else expect(c.indices ?? [], c.id).toEqual([]);
      // the display title: the OSI technologies of its nodes (capped), then the labels whole (commas and all), never a piece of the exported name
      const own = c.nodes.map((id) => byId.get(id)!);
      const title = compactTitle(c, doc.nodes, { full: true });
      const techs = compactTechs(own);
      expect(title.startsWith(techs.length ? `${techTitle(techs)}: ` : own[0].label), `${c.id} title ${title}`).toBe(true);
      for (const id of c.nodes) expect(title, `${c.id} title`).toContain(byId.get(id)!.label);
    }
    if (tx.dictionary) {
      const sectors = new Set(indicesOf(doc.meta).flatMap((ix) => ix.sectors));
      for (const t of doc.meta.scopeTechs ?? []) expect(sectors.has(t), `scope sector ${t}`).toBe(true);
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

  it('the network and investment flows (2026-10-17): the headline variant, one best-ranked host per node, every code named', () => {
    if (!doc.network || !doc.investmentFlows) return;
    const net = doc.network;
    const inv = doc.investmentFlows;
    for (const v of [net.variant, inv.variant]) {
      expect(v.shock).toBe(doc.meta.headlineShock);
      expect(v.scenario).toBe(doc.meta.headlineScenario);
      if (doc.meta.supplyConstraint) expect(v.supplyConstraint).toBe(doc.meta.supplyConstraint);
    }
    // the flows, like the compacts and the country summary, cover only nodes of the strategic scope
    const nodeById = new Map(doc.nodes.map((n) => [n.id, n]));
    for (const x of [...inv.pairs, ...inv.hosts]) expect(nodeById.has(x.node) && inStrategicScope(nodeById.get(x.node)!), `flow node ${x.node} in scope`).toBe(true);
    const names = countryNamesOf(doc);
    for (const c of net.countries) expect(names[c.iso3]).toBeTruthy();
    for (const p of inv.pairs) {
      expect(names[p.investor]).toBeTruthy();
      expect(names[p.host]).toBeTruthy();
    }
    // the roles the summary names are the sides with a selected relationship
    for (const c of net.countries) {
      const sides = { supplier: c.supplier, buyer: c.buyer, investment_host: c.host, investor: c.investor } as const;
      for (const [k, sd] of Object.entries(sides)) expect(c.roles.includes(k)).toBe(sd.selected > 0);
    }
    // each node's best-ranked host is one economy, and it is a ranked host of that node
    const bestHost = new Map<string, Set<string>>();
    for (const p of inv.pairs.filter((x) => x.rank === 1)) bestHost.set(p.node, (bestHost.get(p.node) ?? new Set()).add(p.host));
    for (const [node, hosts] of bestHost) {
      expect(hosts.size).toBe(1);
      expect(inv.hosts.some((h) => h.node === node && h.rank === 1 && hosts.has(h.host))).toBe(true);
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

  it('the diversification scenario: its status counts, every capped supplier named, the run fields null exactly where it did not run', () => {
    // structure and counts only: a model rerun changes the values (coverages, gaps, USD, hosts, classes), not these
    const dv = doc.meta.diversify;
    expect(dv).toBeTruthy();
    expect(dv!.shockId).toBe(diversifyShockIdOf(doc.meta));
    expect(doc.meta.shocks.map((s) => s.id)).toContain(dv!.shockId);
    const counts: Record<string, number> = {};
    for (const n of doc.nodes) if (n.diversify) counts[n.diversify.status] = (counts[n.diversify.status] ?? 0) + 1;
    expect(counts).toEqual({ 'diversified to the benchmark': 38, 'within the cap in the allied market': 1, 'already as diversified as its benchmark': 35 });
    expect(doc.nodes.filter((n) => n.diversify === undefined)).toHaveLength(3);
    const t = diversifyTotals(doc.nodes);
    expect([t.above, t.withBenchmark, t.diversified]).toEqual([39, 74, 38]);
    const names = countryNamesOf(doc);
    const id = dv!.shockId;
    for (const n of doc.nodes) {
      const d = n.diversify;
      const v = variantsOf(n);
      if (!d) {
        // not assessed: no class under the scenario
        expect(v[id] ?? null, n.id).toBeNull();
        continue;
      }
      const ran = d.status === 'diversified to the benchmark';
      const noBenchmark = d.status === 'no benchmark';
      // every capped supplier (and host) is an ISO3 code the bundle names
      for (const x of [...d.capped, ...d.hosts]) {
        expect(x, n.id).toMatch(/^[A-Z]{3}$/);
        expect(names[x], `${n.id} ${x}`).toBeTruthy();
      }
      for (const e of relationshipsOf(d.partners)) for (const x of [e.from, e.to]) expect(names[x], `${n.id} ${x}`).toBeTruthy();
      // the cap: null exactly when the node is already as diversified as its benchmark, or has none (a node within
      // the cap in the allied market carries the cap it was tested against)
      expect(d.cap === null, `${n.id} cap`).toBe(d.status === 'already as diversified as its benchmark' || noBenchmark);
      // the class under the scenario, and the variant: null exactly where the scenario did not run
      expect(d.cls === null, `${n.id} cls`).toBe(!ran);
      expect(v[id] ?? null, `${n.id} variant`).toBe(d.cls);
      for (const k of ['static', 'existing', 'optimized', 'residual', 'residualUsd'] as const) expect(d[k] === null, `${n.id} ${k}`).toBe(!ran);
      // suppliers above the cap and supply to shift only where it ran; within the cap nothing shifts
      expect(d.capped.length > 0, `${n.id} capped`).toBe(ran);
      if (ran) expect(d.excess!, n.id).toBeGreaterThan(0);
      if (d.status === 'within the cap in the allied market') expect(d.excess ?? 0, n.id).toBe(0);
      // a benchmark and its basis wherever there is one; above it wherever the cap bites
      expect(d.benchmarkHhi === null, `${n.id} benchmark`).toBe(noBenchmark);
      expect(d.basis === null, `${n.id} basis`).toBe(noBenchmark);
      if (d.cap !== null) expect(d.currentHhi!, n.id).toBeGreaterThan(d.benchmarkHhi!);
      if (ran) expect(diversifyRow(n, id)!.split, n.id).not.toBeNull();
      // already as diversified: at or below the benchmark, or above it by no more than the small tolerance the words name
      if (d.status === 'already as diversified as its benchmark' && d.currentHhi !== null && d.benchmarkHhi !== null) expect(d.currentHhi - d.benchmarkHhi, n.id).toBeLessThanOrEqual(0.01);
      // the class under the scenario carries the exported physical-capability flag
      if (ran) expect(diversifyRow(n, id)!.clsWarning, n.id).toBe(variantWarningsOf(n)?.[id] === true);
    }
    // the network's "every shock" sums: the words name the scenario apart wherever the export lists it
    if (doc.network?.variant.shocks?.includes(id)) expect(everyShock(doc).note).toContain('and the diversification scenario, which removes no supplier');
  });

  it('Managed Interdependence: a compact never passes its own optimum under the cap; type and archetype follow its file', () => {
    const f = (manifest.files as { scenarios?: { managed?: string } }).scenarios?.managed;
    if (!f || !existsSync(resolve(REAL_DIR, f))) return;
    const md = JSON.parse(readFileSync(resolve(REAL_DIR, f), 'utf8')) as AibManagedScenarioDoc;
    for (const c of md.compacts) {
      const m = c.metrics;
      if (!c.coalition || !m) continue;
      // the trade compact restricts the scenario's optimized state at the same cap (lexicographic coverage tolerance aside)
      if (typeof m.trade === 'number' && typeof m.globalOptimum === 'number') expect(m.trade, c.id).toBeLessThanOrEqual(m.globalOptimum + 2e-3);
      if (typeof m.existing === 'number' && typeof m.trade === 'number') expect(m.existing, c.id).toBeLessThanOrEqual(m.trade + 1e-6);
      if (c.type !== undefined) expect(typeof c.type === 'string' && typeof c.archetype === 'string', c.id).toBe(true);
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

// The allied division the AIB Overview shows comes from the posture bundle (public/data/posture, scripts/91,
// schema 2; design section 5): its Partner demand is the economies whose canonical choice for the node is
// Compete · Partner and its Secure demand those that Secure it; Deploy · Accelerate is never counted, and no
// division node sits at Deployment & Use. Skipped without a schema-2 posture core.
const POSTURE_DIR = resolve(REAL_DIR, '..', 'posture');
const postureManifest = existsSync(resolve(POSTURE_DIR, 'manifest.json')) ? (JSON.parse(readFileSync(resolve(POSTURE_DIR, 'manifest.json'), 'utf8')) as { schema_version?: number; core?: string }) : null;
const havePosture = !!postureManifest && postureManifest.schema_version === POSTURE_SCHEMA_VERSION && !!postureManifest.core && existsSync(resolve(POSTURE_DIR, postureManifest.core));

describe.skipIf(!havePosture)('the real allied division (public/data/posture, schema 2)', () => {
  it('Partner demand counts Compete · Partner choices and Secure demand Secure ones; Deploy · Accelerate is not counted; no node at Deployment', () => {
    const core = decodeCore(JSON.parse(readFileSync(resolve(POSTURE_DIR, postureManifest?.core as string), 'utf8')));
    const key = (t: string, s: string) => `${t}|${s}`;
    const partner = new Map<string, number>();
    const secure = new Map<string, number>();
    for (const c of core.cells) {
      if (c.posture === 'C' && c.mode === 'partner') partner.set(key(c.tech, c.stage), (partner.get(key(c.tech, c.stage)) ?? 0) + 1);
      if (c.posture === 'S') secure.set(key(c.tech, c.stage), (secure.get(key(c.tech, c.stage)) ?? 0) + 1);
    }
    expect(core.division.length).toBeGreaterThan(0);
    expect(core.division.filter((d) => d.stage === DEPLOYMENT_STAGE)).toEqual([]);
    const off = core.division.filter((d) => d.partnerDemand !== (partner.get(key(d.tech, d.stage)) ?? 0) || d.secureDemand !== (secure.get(key(d.tech, d.stage)) ?? 0));
    expect(off.map((d) => `${d.tech}|${d.stage}: partner ${d.partnerDemand}, secure ${d.secureDemand}`)).toEqual([]);
    // Accelerate exists only at Deployment, which has no division node
    expect(core.cells.filter((c) => c.deployMode === 'accelerate' && c.stage !== DEPLOYMENT_STAGE)).toEqual([]);
  });
});
