// The real posture bundle (public/data/posture, scripts/91 from scripts/26) against the schema-2 contract:
// the manifest, decodeCore() on the core (every vocabulary, the spec VIII invariants, the thresholds) and
// checkPostureDetail() on every economy's drawer document, plus the cross-file agreements the contract
// cannot see alone (66 nodes per economy, the same posture in core and detail, the manifest's choice counts,
// Need only of the node's own type, missing evidence left null rather than zero). Skipped without an export,
// like budget.test.ts.

import { existsSync, readFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { DEPLOYMENT_STAGE, SUPPLY_STAGES } from '../../lib/stages.ts';
import { checkPostureDetail, checkPostureManifest, decodeCore, type PostureCell } from '../contract.ts';
import { WATCH_REASON_LABEL } from '../grammar.ts';

const DIR = resolve(process.cwd(), 'public/data/posture');
const read = (f: string): unknown => JSON.parse(readFileSync(join(DIR, f), 'utf8'));
const WORD = { S: 'secure', C: 'compete', D: 'deploy', N: 'not_assessed' } as const;
const verbOf = (c: PostureCell): string => (c.posture === 'S' ? 'secure' : c.posture === 'N' ? 'not_assessed' : (c.mode ?? c.deployMode ?? '?'));

describe.skipIf(!existsSync(join(DIR, 'manifest.json')))('real posture bundle (public/data/posture) against the schema-2 contract', () => {
  const manifest = checkPostureManifest(read('manifest.json'));
  const raw = read(manifest.core) as Record<string, unknown>;
  const core = decodeCore(raw);
  const byIso = new Map<string, PostureCell[]>();
  for (const c of core.cells) byIso.set(c.iso3, [...(byIso.get(c.iso3) ?? []), c]);

  it('the core decodes, with the six stages, the thresholds and a phrase for every node', () => {
    expect(core.meta.stages).toEqual([...SUPPLY_STAGES]);
    const t = core.meta.thresholds;
    for (const v of [t.need, t.prize, t.cp, t.deployment, t.borderline_band]) expect(v > 0 && v < 1).toBe(true);
    expect(Object.keys(core.meta.stage_display)).toEqual([...SUPPLY_STAGES]);
    for (const tech of core.meta.techs) for (const s of SUPPLY_STAGES) expect(core.meta.phrases[`${tech}|${s}`], `${tech}|${s}`).toBeTruthy();
    expect(core.cells.length).toBe((manifest.counts as { cells: number }).cells);
  });

  it('every economy has its 66 technology x stage nodes once', () => {
    const n = core.meta.techs.length * SUPPLY_STAGES.length;
    expect(byIso.size).toBe(core.countries.length);
    for (const [iso3, cells] of byIso) {
      expect(cells.length, iso3).toBe(n);
      expect(new Set(cells.map((c) => `${c.tech}|${c.stage}`)).size, iso3).toBe(n);
    }
  });

  it('the choice counts are the manifest counts, and every verb occurs', () => {
    const counts: Record<string, number> = {};
    for (const c of core.cells) counts[verbOf(c)] = (counts[verbOf(c)] ?? 0) + 1;
    const choices = (read('manifest.json') as { counts: { choices?: Record<string, number> } }).counts.choices ?? {};
    expect(Object.keys(choices).length).toBe(7);
    for (const [verb, n] of Object.entries(choices)) expect(counts[verb] ?? 0, verb).toBe(n);
    for (const verb of ['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed']) expect(counts[verb] ?? 0, verb).toBeGreaterThan(0);
  });

  it('Need is the node type\'s own: deployment need at Deployment & Use, supply-chain need elsewhere; Secure carries a rationale', () => {
    for (const c of core.cells) {
      expect(c.needType, `${c.iso3} ${c.tech} ${c.stage}`).toBe(c.stage === DEPLOYMENT_STAGE ? 'deployment' : 'supply_chain');
      if (c.stage === DEPLOYMENT_STAGE) expect(c.prize, `${c.iso3} ${c.tech} prize at Deployment`).toBeNull();
      if (c.posture === 'S') expect(c.rationale, `${c.iso3} ${c.tech} ${c.stage}`).not.toBeNull();
      else expect(c.rationale).toBeNull();
      if (c.need !== null) expect(c.need >= 0 && c.need <= 1).toBe(true);
      if (c.prize !== null) expect(c.prize >= 0 && c.prize <= 1).toBe(true);
      if (c.cp !== null) expect(c.cp >= 0 && c.cp <= 1).toBe(true);
    }
  });

  it('a node is assessed only with its evidence: Compete and Watch at producing stages have need and prize, Deploy at Deployment has deployment need', () => {
    for (const c of core.cells) {
      const at = `${c.iso3} ${c.tech} ${c.stage}`;
      if (c.posture === 'C' || (c.posture === 'D' && c.stage !== DEPLOYMENT_STAGE)) {
        expect(c.need, at).not.toBeNull();
        expect(c.prize, at).not.toBeNull();
      }
      if (c.posture === 'D' && c.stage === DEPLOYMENT_STAGE) expect(c.need, at).not.toBeNull();
      if (c.mode === 'catch_up') expect(c.pathway, at).toBe('yes');
    }
  });

  it('every drawer document decodes and agrees with the core', () => {
    const details = Object.entries(manifest.details);
    expect(details.length).toBe(core.countries.length);
    for (const [iso3, file] of details) {
      const d = checkPostureDetail(read(file));
      expect(d.iso3).toBe(iso3);
      const cells = byIso.get(iso3) ?? [];
      expect(d.cells.length, iso3).toBe(cells.length);
      const post = new Map(cells.map((c) => [`${c.tech}|${c.stage}`, c]));
      for (const n of d.cells) {
        const c = post.get(n.key);
        const at = `${iso3} ${n.key}`;
        expect(c, at).toBeDefined();
        if (!c) continue;
        expect(n.posture, at).toBe(WORD[c.posture]);
        expect(n.mode, at).toBe(c.mode);
        expect(n.deployMode, at).toBe(c.deployMode);
        if (n.stage === DEPLOYMENT_STAGE) {
          expect(n.supplyChain, at).toBeNull();
          expect(n.deployment, at).not.toBeNull();
          for (const f of [n.deployment!.demand, n.deployment!.policyGap, n.deployment!.system]) {
            if (f.value !== null) expect(f.value >= 0 && f.value <= 1, at).toBe(true);
            if (f.value !== null) expect(f.basis, at).not.toBeNull();
          }
        } else {
          expect(n.deployment, at).toBeNull();
          expect(n.supplyChain, at).not.toBeNull();
        }
        if (n.posture === 'not_assessed') expect(n.notAssessedReason, at).toBeTruthy();
        if (n.posture === 'secure') expect(n.rationale, at).toBe(c.rationale);
        for (const s of n.policy.sources) expect(s, at).not.toMatch(/[+;]|^none$/);
        // support, protection and momentum are within-country percentile ranks (0-1), never shares or signed changes
        for (const v of [n.policy.priority, n.policy.support, n.policy.protection, typeof n.policy.momentum === 'number' ? n.policy.momentum : null]) {
          if (v !== null) expect(v >= 0 && v <= 1, at).toBe(true);
        }
      }
    }
  });

  it("every Watch node carries the engine's reasons (R sp_watch_reasons), each a code the drawer has words for; no other node has any", () => {
    for (const [iso3, file] of Object.entries(manifest.details)) {
      for (const n of checkPostureDetail(read(file)).cells) {
        const at = `${iso3} ${n.key}`;
        if (n.deployMode !== 'watch') {
          expect(n.watchReasons, at).toEqual([]);
          continue;
        }
        expect(n.watchReasons.length, at).toBeGreaterThan(0);
        for (const r of n.watchReasons) expect(Object.keys(WATCH_REASON_LABEL), at).toContain(r);
        if (n.stage === DEPLOYMENT_STAGE) expect(n.watchReasons, at).toEqual(['low_deployment_need']);
        else expect(n.watchReasons, at).not.toContain('low_deployment_need');
      }
    }
  });
});
