// The SYNTHETIC Sectoral Compacts fixture (src/aib/__fixtures__/synthetic/data/aib/: invented economies
// and numbers, three compacts (one whose middle target is out of reach), eight nodes, one of them outside
// the strategic scope; every field of the final model: per-variant and per-growth-bound warnings, single
// substitutes and substitutesNeeded, the leave-one-out flag, the six tiers with a marginal critical
// external dependency, associatesReach99, robustTieBreakSolved), for tests only (node fs): the manifest,
// the document, an AibSource over the files, and stripMethodology() to turn a document into one written
// before the 2026-10-16 methodology fields (and the 2026-10-17 network and investment flows). AIB_DATA_DIR overrides the directory (e.g. public/data/aib for
// a local run of the view tests on the real export).

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { AibDoc, AibManifest } from '../contract.ts';
import { AibLoadError, type AibSource } from '../data/store.ts';

const TIER_ROLES = ['Critical supply partner', 'Critical external dependency', 'Critical external dependency (marginal)', 'External dependency'];

/**
 * The document as a bundle written before the 2026-10-16 methodology fields (in place): no demand scope,
 * semantics, constraint or strategic scope; no policy needs, warnings (headline or per variant),
 * variants, growth or N-1 readings; the instantaneous N-1 back under its earlier name, secondShock; no
 * coalition method, tie-break flag, greedy coalition, tiers or tier roles, leave-one-out flags or
 * substitutes; no node policy needs or physical warnings. The universe label stays.
 */
export function stripMethodology(doc: AibDoc): AibDoc {
  const m = doc.meta as unknown as Record<string, unknown>;
  for (const k of ['demandScope', 'candidateUniverse', 'nDemandScope', 'shockSemantics', 'supplyConstraint', 'semantics', 'constraints', 'strategicScope', 'scopeTechs']) delete m[k];
  for (const n of doc.nodes) {
    const x = n as unknown as Record<string, unknown>;
    for (const k of [
      'policyNeeds', 'warning', 'physicalValidation', 'physicalData', 'physicalDivergence', 'inStrategicScope', 'variants', 'variantWarnings', 'growth', 'coordinateSensitiveToGrowth',
      'n1Reoptimized', 'instantaneousN1', 'diversion',
    ]) {
      delete x[k];
    }
    for (const f of n.frontier) {
      f.secondShock = f.instantaneousN1 ?? null;
      delete f.instantaneousN1;
      delete f.n1Reoptimized;
      delete f.n1Removed;
    }
  }
  for (const c of doc.compacts) {
    for (const k of ['method', 'robustTieBreakSolved', 'greedy', 'criticalPartners', 'criticalExternal', 'criticalExternalMarginal', 'external', 'associatesReach99'] as const) delete c.coalition[k];
    delete c.nodePolicyNeeds;
    delete c.physicalWarnings;
    c.roles = c.roles.filter((r) => !TIER_ROLES.includes(r.role));
    for (const r of c.roles) {
      delete r.tier;
      delete r.critical;
      delete r.looCritical;
      delete r.substitutes;
      delete r.substitutesNeeded;
    }
  }
  // the emergent network and the investment flows (2026-10-17)
  delete doc.network;
  delete doc.investmentFlows;
  return doc;
}

const here = resolve(process.cwd());
export const SYNTHETIC_DIR = resolve(here, 'src/aib/__fixtures__/synthetic/data/aib');
export const FIXTURE_DIR = resolve(here, process.env.AIB_DATA_DIR || 'src/aib/__fixtures__/synthetic/data/aib');
/** The real export (scripts/84), when present; AIB_REAL_DIR checks another export directory. */
export const REAL_DIR = resolve(here, process.env.AIB_REAL_DIR || 'public/data/aib');

export function haveFixture(dir = FIXTURE_DIR): boolean {
  return existsSync(resolve(dir, 'manifest.json'));
}

export function readManifest(dir = FIXTURE_DIR): AibManifest {
  return JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')) as AibManifest;
}

export function readDoc(dir = FIXTURE_DIR): AibDoc {
  return JSON.parse(readFileSync(resolve(dir, readManifest(dir).files.compacts), 'utf8')) as AibDoc;
}

/** An AibSource over a directory's files (records the files requested); `edit` changes the document. */
export function fixtureSource(dir = FIXTURE_DIR, edit?: (doc: AibDoc) => void): AibSource & { calls: string[] } {
  const calls: string[] = [];
  return {
    calls,
    async manifest() {
      calls.push('manifest.json');
      if (!haveFixture(dir)) throw new AibLoadError('NO_DATA', 'data/aib/manifest.json does not exist (HTTP 404).');
      return readManifest(dir);
    },
    async json(file) {
      calls.push(file);
      if (!existsSync(resolve(dir, file))) throw new AibLoadError('NETWORK', `data/aib/${file} does not exist (HTTP 404).`);
      const doc = JSON.parse(readFileSync(resolve(dir, file), 'utf8')) as AibDoc;
      if (edit) edit(doc);
      return doc;
    },
  };
}
