// The SYNTHETIC Sectoral Compacts fixture (src/aib/__fixtures__/synthetic/data/aib/: invented economies
// and numbers, three compacts (one whose middle target is out of reach), eight nodes; every field of the
// final model: per-variant and per-growth-bound warnings, single substitutes and substitutesNeeded, the
// leave-one-out flag, the six tiers with a marginal critical external dependency, associatesReach99,
// robustTieBreakSolved). Since 2026-10-19 its nodes come from the electro-industrial dictionary (taxonomy
// "dictionary", core scope, strategic scope all): EIS sectors with "/" ("Electrolysers/H2 equipment"),
// node labels with commas ("Magnets, magnet parts & assemblies"), Downstream nodes, six indices (one, E,
// without a node), OSI cells through the crosswalk (the magnets node in two cells and three OSI
// sub-components; the Solar sector's Downstream node in OSI Solar Midstream; the electrolysers node in no
// cell), and a compact spanning two indices (AIB-C01: A Rare earths and B Magnets). Since 2026-10-19 too,
// the diversification scenario (meta.diversify; the diversify_benchmark shock and variant): the magnets
// node diversified with two capped suppliers, the anode node diversified with no gap left (residual 0),
// the towers node diversified with one (its class under the scenario flagged PHYSICAL CAPABILITY NOT
// VALIDATED), the rare-earth compounds within the cap in the allied market, the natural graphite and PV
// modules nodes already as diversified as their benchmark (graphite just above it, within the tolerance the
// scenario leaves alone: 0.263 against 0.26), the electrolysers node
// without a benchmark and the transformers node not assessed (no node.diversify); the network's shocks
// (network.variant.shocks) list the scenario too, as the export writes every configured shock there.
// Since 2026-10-19 (later) too, the scenario toggle: meta.scenarios lists Baseline, Managed Interdependence
// and Decoupling (with meta.supplyScenarios, the supply scenarios it replaces); node.baseline on the seven
// optimized nodes (not the INSUFFICIENT DATA transformers node); the node.diversify extras (filled on the
// three nodes the scenario ran on, null elsewhere); compact.managed (one node run in each compact) and
// compact.baseline; and the two scenario files the manifest names: scenario-managed (the three compacts
// solved under the scenario: AIB-C03's 99% target out of reach; ZZZ an external dependency of AIB-C01; its
// network of six edges; investment flows for the magnets node only, a rank-2 alternative among them) and
// scenario-baseline (today's network, every pair with at least 2% of a node's demand; each node's and
// compact's top ten suppliers).
//
// For tests only (node fs): the manifest, the document, the scenario files (readScenarioDoc), an AibSource
// over the files (fixtureSource: `edit` changes the main document, `opts.editScenario` a scenario file,
// `opts.noScenarioFiles` drops files.scenarios from the manifest), stripScenarios() to turn the document
// into one written before the scenario toggle (stripDiversify() and stripMethodology() strip it too), toMasterTaxonomy() to
// turn the document into one of the OSI basket (meta without the taxonomy fields, OSI technology names
// from the crosswalk, strategic scope electro_industrial with the electrolysers node, as Green Hydrogen,
// outside it), stripDiversify() to turn it into one written before the diversification scenario, and
// stripMethodology() to turn a document into one written before the 2026-10-16 methodology fields (and the
// 2026-10-17 network and investment flows, and the 2026-10-19 taxonomy and diversification scenario: an
// older bundle is a master one). AIB_DATA_DIR overrides the directory (e.g. public/data/aib for a local
// run of the view tests on the real export).

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import {
  AIB_DIVERSIFY_SHOCK, AIB_SCENARIO_FILE_RE, scenarioFilesOf, type AibBaselineScenarioDoc, type AibDoc, type AibManagedScenarioDoc, type AibManifest, type AibScenarioDoc,
  type AibScenarioFileId,
} from '../contract.ts';
import { AibLoadError, type AibSource } from '../data/store.ts';
import { osiCellOf } from '../derive/taxonomy.ts';
import { stageSlug } from '../../lib/format.ts';

const TIER_ROLES = ['Critical supply partner', 'Critical external dependency', 'Critical external dependency (marginal)', 'External dependency'];

/** The master strategic scope of the fixture (config/aib.yml electro_industrial). */
export const MASTER_SCOPE_TECHS = ['Batteries', 'Electric Vehicles', 'Solar', 'Wind', 'Electric Grid', 'Semiconductors', 'Nuclear'];

/**
 * The document as a bundle of the OSI basket (master taxonomy; in place; a no-op on a master bundle): no
 * taxonomy, dictionary scope, indices, node index or compact indices; each node's technologies and stage
 * from its first OSI cells (a node in no cell becomes Green Hydrogen, outside the strategic scope), its
 * cells its own technology x stage; compact technologies, stages and names in the exporter's master form
 * ("Electric Vehicles/Wind: ..."); the network's technologies from its nodes; strategic scope
 * electro_industrial.
 */
export function toMasterTaxonomy(doc: AibDoc): AibDoc {
  const m = doc.meta as unknown as Record<string, unknown>;
  if (m.taxonomy !== 'dictionary') return doc;
  for (const k of ['taxonomy', 'dictionaryScope', 'indices']) delete m[k];
  if (m.strategicScope !== undefined) {
    m.strategicScope = 'electro_industrial';
    m.scopeTechs = [...MASTER_SCOPE_TECHS];
  }
  for (const n of doc.nodes) {
    delete n.indexCode;
    const cells = n.yfCells.map(osiCellOf).filter((c): c is NonNullable<typeof c> => c !== null);
    const stage = cells[0]?.stage ?? 'Midstream';
    const techs = cells.length ? [...new Set(cells.filter((c) => c.stage === stage).map((c) => c.tech))] : ['Green Hydrogen'];
    n.techs = techs;
    n.stage = stage;
    n.yfCells = techs.map((t) => stageSlug(t, stage));
    n.yfSubcomponents = n.yfSubcomponents.filter((s) => n.yfCells.some((c) => s.startsWith(`${c}::`)));
    if (n.inStrategicScope !== undefined) n.inStrategicScope = techs.some((t) => MASTER_SCOPE_TECHS.includes(t));
  }
  const byId = new Map(doc.nodes.map((n) => [n.id, n]));
  for (const c of doc.compacts) {
    delete c.indices;
    const ns = c.nodes.map((id) => byId.get(id)).filter((n): n is NonNullable<typeof n> => !!n);
    c.technologies = [...new Set(ns.flatMap((n) => n.techs))].sort((a, b) => a.localeCompare(b, 'en'));
    c.stages = [...new Set(ns.map((n) => n.stage))].sort();
    c.name = `${c.technologies.join('/')}: ${ns.map((n) => n.label).join(', ')}`;
  }
  if (doc.network) {
    for (const e of doc.network.edges) e.techs = [...new Set(e.nodes.flatMap((id) => byId.get(id)?.techs ?? []))].sort((a, b) => a.localeCompare(b, 'en'));
    for (const c of doc.network.countries) {
      c.technologies = [...new Set(doc.network.edges.filter((e) => e.from === c.iso3 || e.to === c.iso3).flatMap((e) => e.techs))].sort((a, b) => a.localeCompare(b, 'en'));
    }
    doc.network.variant.strategicScope = 'electro_industrial';
  }
  if (doc.investmentFlows) doc.investmentFlows.variant.strategicScope = 'electro_industrial';
  return doc;
}

/** The node.diversify fields of the scenario toggle (2026-10-19). */
const DIVERSIFY_EXTRAS = ['lostUsd', 'dividend', 'hhi', 'largest', 'largestShare', 'top3Share', 'newLinks', 'expandedLinks'] as const;

/**
 * The document as a bundle written before the scenario toggle (in place): meta.scenarios back to the supply
 * scenarios (meta.supplyScenarios, then removed), no node.baseline, no node.diversify extras, no
 * compact.managed or compact.baseline. A no-op on a document without them.
 */
export function stripScenarios(doc: AibDoc): AibDoc {
  const m = doc.meta as unknown as Record<string, unknown>;
  if (Array.isArray(m.supplyScenarios)) m.scenarios = m.supplyScenarios;
  else if (Array.isArray(m.scenarios) && !m.scenarios.every((s) => typeof s === 'string')) m.scenarios = [doc.meta.headlineScenario];
  delete m.supplyScenarios;
  for (const n of doc.nodes) {
    delete n.baseline;
    if (n.diversify) for (const k of DIVERSIFY_EXTRAS) delete n.diversify[k];
  }
  for (const c of doc.compacts) {
    delete c.managed;
    delete c.baseline;
  }
  return doc;
}

/**
 * The document as a bundle written before the diversification scenario (in place): no meta.diversify, no
 * node.diversify, and no diversify_benchmark shock in meta.shocks or variant (with its warning flag); nor
 * the scenario toggle that came after it (stripScenarios).
 */
export function stripDiversify(doc: AibDoc): AibDoc {
  stripScenarios(doc);
  const id = doc.meta.diversify?.shockId ?? AIB_DIVERSIFY_SHOCK;
  delete doc.meta.diversify;
  doc.meta.shocks = doc.meta.shocks.filter((s) => s.id !== id);
  for (const n of doc.nodes) {
    delete n.diversify;
    if (n.variants && !Array.isArray(n.variants)) delete n.variants[id];
    if (n.variantWarnings && !Array.isArray(n.variantWarnings)) delete n.variantWarnings[id];
  }
  if (doc.network?.variant.shocks) doc.network.variant.shocks = doc.network.variant.shocks.filter((s) => s !== id);
  return doc;
}

/**
 * The document as a bundle written before the 2026-10-16 methodology fields (in place): no demand scope,
 * semantics, constraint or strategic scope; no policy needs, warnings (headline or per variant),
 * variants, growth or N-1 readings; the instantaneous N-1 back under its earlier name, secondShock; no
 * coalition method, tie-break flag, greedy coalition, tiers or tier roles, leave-one-out flags or
 * substitutes; no node policy needs or physical warnings. The universe label stays. Such a bundle
 * predates the diversification scenario (stripDiversify) and the dictionary (toMasterTaxonomy) too.
 */
export function stripMethodology(doc: AibDoc): AibDoc {
  stripDiversify(doc);
  toMasterTaxonomy(doc);
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

/** A scenario file the manifest names (null when it names none: an older bundle). */
export function readScenarioDoc(id: 'managed', dir?: string): AibManagedScenarioDoc | null;
export function readScenarioDoc(id: 'baseline', dir?: string): AibBaselineScenarioDoc | null;
export function readScenarioDoc(id: AibScenarioFileId, dir = FIXTURE_DIR): AibScenarioDoc | null {
  const f = scenarioFilesOf(readManifest(dir))[id];
  return f ? (JSON.parse(readFileSync(resolve(dir, f), 'utf8')) as AibScenarioDoc) : null;
}

export interface FixtureSourceOpts {
  /** change a scenario file as it is read */
  editScenario?: (doc: AibScenarioDoc, id: AibScenarioFileId) => void;
  /** a manifest without files.scenarios (an older bundle) */
  noScenarioFiles?: boolean;
}

/**
 * An AibSource over a directory's files (records the files requested); `edit` changes the main document
 * (a compacts.<h8>.json file), `opts` the scenario files and the manifest.
 */
export function fixtureSource(dir = FIXTURE_DIR, edit?: (doc: AibDoc) => void, opts: FixtureSourceOpts = {}): AibSource & { calls: string[] } {
  const calls: string[] = [];
  return {
    calls,
    async manifest() {
      calls.push('manifest.json');
      if (!haveFixture(dir)) throw new AibLoadError('NO_DATA', 'data/aib/manifest.json does not exist (HTTP 404).');
      const m = readManifest(dir);
      if (opts.noScenarioFiles) delete (m.files as { scenarios?: unknown }).scenarios;
      return m;
    },
    async json(file) {
      calls.push(file);
      if (!existsSync(resolve(dir, file))) throw new AibLoadError('NETWORK', `data/aib/${file} does not exist (HTTP 404).`);
      const x = JSON.parse(readFileSync(resolve(dir, file), 'utf8')) as unknown;
      const sc = AIB_SCENARIO_FILE_RE.exec(file);
      if (sc) {
        if (opts.editScenario) opts.editScenario(x as AibScenarioDoc, sc[1] as AibScenarioFileId);
      } else if (edit) edit(x as AibDoc);
      return x;
    },
  };
}
