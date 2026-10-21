// Sectoral Compacts data contract (schema osi-aib v1). The interface between
// scripts/84_export_aib_data.R (R/dashboard/aib/aib_export.R, which writes these files) and the views
// in src/aib/. Pure: no IO, no React.
//
// Files under <base>data/aib/:
//   manifest.json            AibManifest (no-cache; written last, names the current document)
//   compacts.<h8>.json       AibDoc: meta, country names, compacts and every classified node (immutable)
//
// Shares are 0-1 (the UI prints them as %); USD values are dollars (the UI prints $m / $bn). Arrays are
// always arrays (the exporter wraps every array field in I()). Numbers may be null where R had NA.
// Strings the exporter can write as NA (a node's data quality, its baseline largest supplier, ...) are
// typed `string | null` here and tolerated by the checks.
//
// Runtime checks: checkAibManifest() and checkCompactsDoc() return problem strings (empty = valid);
// the store throws AibContractError with them.
//
// Additive fields (2026-10-15, after the R review; still v1): Compact.metrics.globalOptimum,
// anchorFragmented, anchorCompact, compactGain; Compact.classMix, secondaryClass; Node.tightestCostless,
// minRequiredDiversification; frontier[].specialization; and the compact type "compact unnecessary: ...".
// They are optional here: an earlier bundle without them still validates. From the same review,
// metrics.trade is "the trade compact": every economy keeps its existing-relationship response and the
// coalition's members may also open or expand relationships, so it is at least metrics.existing;
// metrics.investment adds the standard increment at up to two member hosts per BUILD node.
//
// Methodology fields (2026-10-16; still v1, all optional so an older bundle validates):
//   manifest + meta   demandScope (the importers whose lost supply is restored; "coalition" = the allied
//                     core and partner tiers of the coalition table, NOT a compact's coalition) and
//                     candidateUniverse (the exporters that may supply; "global" = any economy) as
//                     separate settings, nDemandScope, the shock semantics (hold: the shocked economy keeps
//                     buying, so other exporters' sales to it stay with it; release: its purchases fall and
//                     that share of the sales to it is freed), the supply constraint
//                     (production_constrained: where credible production or capacity data exist they decide
//                     WHO can supply, a listed producer keeps its full trade response and an unlisted
//                     exporter keeps its current shipments to the demand scope but cannot expand or redirect;
//                     trade_only: observed exports alone), the modes run (semantics, constraints) and the
//                     strategic scope (any configured name, e.g. electro_industrial: compacts only from the
//                     scopeTechs nodes). meta.universe stays, now a label ("coalition demand / global
//                     partners").
//   node              policyNeeds (multi-label: any of COORDINATE, BUILD, MANAGE SPECIALIZATION beside the
//                     single headline class), warning ("PHYSICAL CAPABILITY NOT VALIDATED"),
//                     physicalValidation / physicalData / physicalDivergence, inStrategicScope, variants
//                     (the class under the other shocks, the conservative scenario, release semantics and
//                     each supply constraint), growth (existing coverage, dividend and class at each bound on
//                     how far an existing relationship may grow) and coordinateSensitiveToGrowth,
//                     n1Reoptimized (true N-1: the largest supplier removed and the trade re-optimized),
//                     instantaneousN1 (coverage x (1 - largest share), mechanical) and diversion.
//   node.frontier[]   instantaneousN1, n1Reoptimized, n1Removed. secondShock (the earlier name of the
//                     instantaneous N-1) is no longer written; it is accepted in an older bundle.
//   compact           coalition.method (the members are the EXACT minimum coalition when the middle target
//                     is reached: the fewest economies meeting it at every node, anchors always in; among
//                     coalitions of that size the widest margin over the target in the weakest node, then
//                     the most supply. When the target is out of reach the method starts with
//                     "greedy_sufficient_coalition" and members is the widest greedy set; an earlier export
//                     wrote [] and kept that set as the roles with member = true: read the members with
//                     coalitionMembersOf()), robustTieBreakSolved (false: the tie-break hit its time limit),
//                     coalition.greedy (the greedy sufficient coalition, a heuristic), criticalPartners,
//                     prospectiveHosts (2026-10-20), criticalExternal, criticalExternalMarginal, external,
//                     associatesReach99; nodePolicyNeeds; physicalWarnings; and per role its coalition tier
//                     (AIB_TIERS, seven since prospective host joined on 2026-10-20),
//                     critical and leave-one-out (looCritical) flags, its SINGLE substitutes and
//                     substitutesNeeded. Externals hold roles with member = false, like associates.
//   node (later)      variantWarnings (variant -> the class under it carries the physical-capability
//                     warning) and growth[].warning: the warning of every class a node shows.
//
// Taxonomy fields (2026-10-19; still v1, all optional so an older bundle validates):
//   meta              taxonomy: where the nodes come from. "dictionary": the electro-industrial HS6
//                     dictionary (RMI Electro-Industrial Stack: index > sector > stage > HS6; primary rows
//                     only, so each code sits in one node); "master" (absent in an older bundle): the OSI
//                     basket (technology x stage x sub-sector). dictionaryScope (the dictionary's `scope`
//                     column: "core", mostly specificity 1-2 plus a few shared codes kept as core with a
//                     written justification, the headline; "extended": core plus the other shared codes,
//                     a sensitivity; null for master) and indices ([{code, name, sectors}], the six EIS
//                     indices A-F with their sectors; [] for master).
//   node              indexCode: the EIS index ("A".."F") of the node's sector (null for master). With the
//                     dictionary, node.techs holds the node's EIS sector(s) (names may contain "/"), not an
//                     OSI technology; node.yfCells / yfSubcomponents are the OSI cells and sub-components its
//                     codes sit in (a trade-weighted crosswalk to the OSI basket; many-to-many; [] where a node
//                     has no basket code).
//   compact           indices: the EIS indices of its sectors ([] for master). compact.name joins the sectors
//                     with " / " and lists node labels that may contain ", ": display it, never parse it.
//
// Diversification fields (2026-10-19, later; still v1, all optional so an older bundle validates): the
// scenario "Diversified to its sector's concentration" (a shock of type "diversify" in config/aib.yml,
// id diversify_benchmark). It removes no supplier: each node's world exporter concentration (HHI, Hong
// Kong and Macao counted with China) is set against a benchmark, the trade-weighted mean HHI of the other
// HS6 lines of its HS4 heading (else of the other nodes of its sector); a node above it gets the supplier-
// share cap that would bring it down, every supplier above the cap in the allied market (the demand scope) loses
// the excess (no supplier may ship more than cap x the allied market's demand; a capped ally keeps its own
// purchases), existing relationships then optimized trade replace what they can with the cap holding, and
// the rest is the new capacity diversification needs (the investment simulation ranks its hosts).
//   meta              diversify: {shockId, label, benchmark ("hs4" | "sector")}.
//   node              diversify (absent for a node the scenario did not assess): status (AIB_DIVERSIFY_STATUSES),
//                     currentHhi, benchmarkHhi, basis ("other HS6 lines of heading 8507", "other nodes of
//                     the sector"), cap (null when already as diversified or without a benchmark), capped
//                     (the suppliers above the cap in the allied market; [] when none), excess (the share of
//                     the node's allied demand above the cap: supply to shift = excess x node.demandUsd), the
//                     coverages static / existing / optimized of the node's allied demand under the cap,
//                     residual (only new capacity fills it) and residualUsd, hosts, partners ("EXP->IMP;
//                     ...") and cls (the class under the scenario); the run fields are null where the
//                     scenario did not run. node.variants[shockId] is that class (null where it did not run).
//
// Scenario fields (2026-10-19, the scenario toggle; still v1, all optional so an older bundle validates and
// renders with Decoupling only). Three scenarios: Baseline (trade as it is today, before any shock),
// Managed Interdependence ("managed": the diversify_benchmark shock) and Decoupling (the headline shock
// under hold: this main document). The R mirror is R/dashboard/aib/aib_check.R (aib_check_scenarios,
// aib_check_scenario_doc).
//   manifest          files.scenarios: { managed?: "scenario-managed.<h8>.json", baseline?:
//                     "scenario-baseline.<h8>.json" } (AIB_SCENARIO_FILE_RE; the hash as for compacts),
//                     lazily loaded (AibScenarioDoc, checkScenarioDoc()).
//   meta              scenarios: the list {id, label, shockId, words} (baseline, managed, decoupling).
//                     COLLISION: meta.scenarios was, and in an older bundle still is, the supply scenarios
//                     (["conservative", "reallocation"]). Both shapes validate; read the supply scenarios
//                     with supplyScenariosOf() (meta.supplyScenarios when meta.scenarios holds the objects)
//                     and the scenario list with scenarioDefsOf(); never read meta.scenarios directly.
//   node              baseline: {hhi, top3Share, largest, largestShare, suppliers: [iso3, share][] (top 5,
//                     shares of the node's allied demand)}, observed, before any shock (scope-supply basis).
//   node.diversify    lostUsd (excess x demandUsd), dividend (optimized - existing), hhi, largest,
//                     largestShare, top3Share (the optimized state under the scenario, on node.hhi's basis),
//                     newLinks, expandedLinks: filled where the scenario ran, null otherwise.
//   compact           managed: {nodesRun, demandUsd, lostUsd, static, existing, optimized, residual,
//                     residualUsd, classMix} (demand-weighted over ALL its nodes; a node the scenario did not
//                     run on counts as unchanged); baseline: {hhi, suppliers: [iso3, share][] (top 5, shares
//                     of the compact's allied demand)}.

// ---- Schema ----------------------------------------------------------------------------------------

export const AIB_SCHEMA = 'osi-aib';
/** 2 since 2026-10-20: meta.scenarios became the scenario list (objects); an app reading 1 asks for a reload. Version 1 bundles still read. */
export const AIB_VERSION = 2;
/** Relative to the app's base URL. Written only by scripts/84. */
export const AIB_DATA_DIR = 'data/aib/';
export const AIB_MANIFEST_FILE = 'manifest.json';
/** The document file name scripts/84 writes (and the only kind it deletes). */
export const AIB_DOC_FILE_RE = /^compacts\.[0-9a-f]{8}\.json$/;
/** Path segments under /compacts that are views, not compact ids. */
export const AIB_RESERVED = ['about'] as const;

// ---- Enums -----------------------------------------------------------------------------------------

/** The node classes in the order the views list them (meta.classes carries the same list). */
export const AIB_CLASSES = ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION', 'INSUFFICIENT DATA'] as const;
export type AibClass = (typeof AIB_CLASSES)[number];

export const AIB_ARCHETYPES = ['Trade / Coordination Compact', 'Investment / Capacity Compact', 'Specialization Compact', 'Integrated Compact'] as const;
export type AibArchetype = (typeof AIB_ARCHETYPES)[number];

/** The compact types; "compact unnecessary": the coalition barely improves the demand anchors' coverage
 *  over what their existing relationships give them (fragmented action). */
export const AIB_COMPACT_TYPES = ['candidate compact', 'bilateral arrangement sufficient', 'compact unnecessary: existing relationships do nearly as well'] as const;
export type AibCompactType = (typeof AIB_COMPACT_TYPES)[number];

export const AIB_RELATIONSHIPS = ['new', 'expanded'] as const;
export type AibRelationship = (typeof AIB_RELATIONSHIPS)[number];

/** The policy needs a node can carry at once (the multi-label profile beside the headline class). */
export const AIB_POLICY_NEEDS = ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION'] as const;
export type AibPolicyNeed = (typeof AIB_POLICY_NEEDS)[number];

/** The warning the exporter writes on a LOW INTERVENTION or COORDINATE class it cannot validate physically. */
export const AIB_PHYSICAL_WARNING = 'PHYSICAL CAPABILITY NOT VALIDATED';

/** How much of world production a node's physical proxy covers. */
export const AIB_PHYSICAL_DATA = ['complete', 'incomplete', 'none'] as const;
export type AibPhysicalData = (typeof AIB_PHYSICAL_DATA)[number];

/**
 * The modes of the demand scope and of the candidate-partner universe. "coalition" is the allied core and
 * partner tiers of the coalition table (57 economies in the headline run), not a compact's coalition;
 * "global" is any economy in the trade data.
 */
export const AIB_SCOPE_MODES = ['coalition', 'configured', 'global'] as const;
export type AibScopeMode = (typeof AIB_SCOPE_MODES)[number];

/**
 * Shock semantics: hold (an export shock; the shocked economy keeps buying) or release (a two-way cut: the
 * shocked economy's purchases also fall by the same share).
 */
export const AIB_SEMANTICS = ['hold', 'release'] as const;
export type AibSemantics = (typeof AIB_SEMANTICS)[number];

/**
 * Supply constraint. production_constrained: where credible production or capacity data exist, they decide
 * who can supply (a listed producer keeps its full trade response; an exporter they do not list keeps its
 * current shipments to the demand scope but cannot expand or redirect supply; shares are never turned into
 * trade values). trade_only: observed exports alone.
 */
export const AIB_CONSTRAINTS = ['production_constrained', 'trade_only'] as const;
export type AibConstraint = (typeof AIB_CONSTRAINTS)[number];

/** The strategic scopes config/aib.yml ships with (words only: meta.strategicScope may be any configured name and is not validated against this list). */
export const AIB_STRATEGIC_SCOPES = ['electro_industrial', 'all'] as const;
export type AibStrategicScope = (typeof AIB_STRATEGIC_SCOPES)[number];

/**
 * Where the nodes come from (meta.taxonomy; absent = master): the electro-industrial HS6 dictionary (EIS
 * index > sector > stage) or the OSI basket (technology x stage x sub-sector).
 */
export const AIB_TAXONOMIES = ['dictionary', 'master'] as const;
export type AibTaxonomy = (typeof AIB_TAXONOMIES)[number];

/** The dictionary scopes (its `scope` column): core (mostly specificity 1-2, the headline) or extended (core plus the shared codes, a sensitivity). */
export const AIB_DICTIONARY_SCOPES = ['core', 'extended'] as const;

/** An index code of the Electro-Industrial Stack: one capital letter ("A".."F"). */
export const AIB_INDEX_CODE_RE = /^[A-Z]$/;

/**
 * The statuses of a node in the diversification scenario (node.diversify.status): the scenario ran (the
 * node is more concentrated than its benchmark and a supplier to the allied market is above the cap);
 * above the benchmark worldwide but no supplier to the allied market exceeds the cap (nothing to shift);
 * already as diversified as its benchmark; no benchmark (its heading has no other line, its sector no
 * other node).
 */
export const AIB_DIVERSIFY_STATUSES = [
  'diversified to the benchmark',
  'within the cap in the allied market',
  'already as diversified as its benchmark',
  'no benchmark',
] as const;
export type AibDiversifyStatus = (typeof AIB_DIVERSIFY_STATUSES)[number];

/**
 * The coalition tiers of a compact's economies, from the most to the least involved:
 *   core coordinating member                 a member of the exact coalition that is not a critical supply
 *                                            partner (the demand anchors, and suppliers one other candidate
 *                                            could replace)
 *   critical supply partner                  a needed member no single other candidate can replace
 *   prospective host                         a member in only for the simulated capacity it would host
 *                                            (its current supply is critical nowhere; added 2026-10-20)
 *   associate partner                        the fewest additions to the core (held in) that reach the
 *                                            highest target
 *   critical external dependency             a non-member whose loss pushes a node below the middle target
 *                                            with the coalition fixed, supplying at least
 *                                            critical_min_supply_share (1%) of the anchors' demand there
 *   critical external dependency (marginal)  the same, under that share: critical only because the
 *                                            coalition sits near the target
 *   external dependency                      another non-member supplying a material share (5%)
 *   development host                         a non-member that is a leading development host of a BUILD node:
 *                                            future capacity, never counted toward the target (2026-10-20)
 */
export const AIB_TIERS = [
  'core coordinating member',
  'critical supply partner',
  'prospective host',
  'associate partner',
  'critical external dependency',
  'critical external dependency (marginal)',
  'external dependency',
  'development host',
] as const;
export type AibTier = (typeof AIB_TIERS)[number];

// ---- Manifest ----------------------------------------------------------------------------------------

export interface AibManifest {
  schema: typeof AIB_SCHEMA;
  version: number;
  created: string;
  /** a label of the run ("coalition demand / global partners") */
  universe: string;
  headlineShock: string;
  headlineScenario: string;
  /** additive (2026-10-16): the settings meta carries too */
  demandScope?: string | null;
  candidateUniverse?: string | null;
  shockSemantics?: string | null;
  supplyConstraint?: string | null;
  /** scenarios (additive, 2026-10-19): the lazily loaded scenario files; read them with scenarioFilesOf() */
  files: { compacts: string; scenarios?: Partial<Record<AibScenarioFileId, string | null>> };
}

// ---- Document ----------------------------------------------------------------------------------------

export interface AibShock {
  id: string;
  label: string;
}

/** One index of the Electro-Industrial Stack (meta.indices): "A" Materials & Inputs, with its sectors. */
export interface AibIndex {
  code: string;
  name: string;
  sectors: string[];
}

/** The diversification scenario of a run (meta.diversify). */
export interface AibDiversifyMeta {
  /** its shock id in meta.shocks and node.variants ("diversify_benchmark") */
  shockId: string;
  label: string | null;
  /** "hs4": the other HS6 lines of the node's HS4 heading, falling back to the other nodes of its sector; "sector": the other nodes of its sector */
  benchmark: string | null;
  /** the HHI margin above its benchmark within which a node counts as already as diversified (config min_excess; additive) */
  minExcess?: number | null;
}

/**
 * One node in the diversification scenario (node.diversify). Shares are of the node's allied demand
 * (node.demandUsd): the supply to shift is excess x demandUsd and residualUsd ~ residual x demandUsd.
 */
export interface AibNodeDiversify {
  /** one of AIB_DIVERSIFY_STATUSES (shown as written when unknown) */
  status: string;
  /** the node's world exporter concentration (HHI, 0-1; 1 = a single supplier) */
  currentHhi: number | null;
  /** the benchmark: the trade-weighted mean HHI of the other lines of its heading (or of the other nodes of its sector) */
  benchmarkHhi: number | null;
  /** "other HS6 lines of heading 8507", "other nodes of the sector" */
  basis: string | null;
  /** the supplier-share cap that would bring the HHI down to the benchmark (null when already as diversified, or without a benchmark) */
  cap: number | null;
  /** the suppliers above the cap in the allied market ([] when none) */
  capped: string[];
  /** the share of the node's allied demand above the cap: the supply to shift */
  excess: number | null;
  /** coverage of the node's allied demand under the cap: before trade, after existing relationships, after optimized trade (null where the scenario did not run) */
  static: number | null;
  existing: number | null;
  optimized: number | null;
  /** the share of allied demand only new capacity fills */
  residual: number | null;
  residualUsd: number | null;
  /** the leading investment hosts for that new capacity (the scaling hosts; may be []) */
  hosts: string[];
  /** the leading specialty and development hosts there (additive, 2026-10-20) */
  specialtyHosts?: string[];
  developmentHosts?: string[];
  /** the largest new or expanded relationships, "EXP->IMP; ..." (null when none) */
  partners: string | null;
  /** the class under the scenario (null where it did not run) */
  cls: string | null;
  // The scenario toggle (additive, 2026-10-19): filled where the scenario ran, null otherwise.
  /** the supply that shifts away from the capped suppliers, USD a year (excess x the node's demandUsd) */
  lostUsd?: number | null;
  /** the coordination dividend under the scenario: optimized minus existing */
  dividend?: number | null;
  /** the optimized state under the scenario, on the basis of node.hhi / node.largestShare (share of served supply) */
  hhi?: number | null;
  largest?: string | null;
  largestShare?: number | null;
  top3Share?: number | null;
  /** the relationships the optimized trade opens or expands */
  newLinks?: number | null;
  expandedLinks?: number | null;
}

/** [ISO3, share]: a supplier and its share (of a node's or a compact's allied demand). */
export type AibSupplierShare = [string, number | null];
/** [ISO3, USD a year, share]: a supplier, what it ships, and its share of the demand. */
export type AibSupplierFlow = [string, number | null, number | null];

/**
 * A node as it is today (node.baseline; additive, 2026-10-19): observed trade, before any shock, on the
 * scope-supply basis (shares of the supply to the demand scope; the same in every shock). suppliers: the
 * top five observed suppliers to the allied market, as shares of the node's allied demand.
 */
export interface AibNodeBaseline {
  hhi: number | null;
  top3Share: number | null;
  largest: string | null;
  largestShare: number | null;
  suppliers: AibSupplierShare[];
}

/**
 * A compact under Managed Interdependence (compact.managed; additive, 2026-10-19): demand-weighted over ALL
 * its nodes, a node the scenario did not run on counted as unchanged (coverage 1, nothing lost).
 */
export interface AibCompactManaged {
  /** its nodes the scenario ran on */
  nodesRun: number;
  demandUsd: number | null;
  lostUsd: number | null;
  static: number | null;
  existing: number | null;
  optimized: number | null;
  residual: number | null;
  residualUsd: number | null;
  /** the classes' shares of the lost supply under the scenario ("BUILD 78%; LOW INTERVENTION 22%"); null when no node ran */
  classMix: string | null;
}

/** A compact as it is today (compact.baseline; additive, 2026-10-19). */
export interface AibCompactBaseline {
  /** the scope-supply HHI of the compact's summed observed supply */
  hhi: number | null;
  /** the top five suppliers over its nodes, as shares of the compact's allied demand */
  suppliers: AibSupplierShare[];
}

export interface AibMeta {
  /** a label of the run ("coalition demand / global partners"); an older bundle wrote the one universe */
  universe: string;
  headlineShock: string;
  headlineScenario: string;
  /** the importers whose lost supply is restored ("coalition": the allied core and partner tiers, not a compact's coalition; additive) */
  demandScope?: AibScopeMode | null;
  /** the exporters that may add or redirect supply ("global": any economy; additive) */
  candidateUniverse?: AibScopeMode | null;
  /** the number of economies in the demand scope (additive) */
  nDemandScope?: number | null;
  /** the headline shock semantics (additive) */
  shockSemantics?: AibSemantics | null;
  /** the headline supply constraint (additive) */
  supplyConstraint?: AibConstraint | null;
  /** every shock semantics run, the headline first (additive) */
  semantics?: string[];
  /** every supply constraint run, the headline first (additive) */
  constraints?: string[];
  /** the technologies compacts are formed from (additive): electro_industrial, all or another configured scope */
  strategicScope?: string | null;
  /** the technologies (dictionary: the sectors) of the strategic scope (additive) */
  scopeTechs?: string[];
  /** where the nodes come from (additive, 2026-10-19): "dictionary" or "master"; absent = master */
  taxonomy?: string | null;
  /** the dictionary scope: "core" or "extended" (null for master; additive) */
  dictionaryScope?: string | null;
  /** the EIS indices with their sectors ([] for master; additive) */
  indices?: AibIndex[];
  /** the diversification scenario (additive, 2026-10-19; absent in an older bundle) */
  diversify?: AibDiversifyMeta;
  tradeYear: number;
  built: string;
  shocks: AibShock[];
  /**
   * the supply scenarios (["conservative", "reallocation"]) in an older bundle, or the scenario list
   * (AibScenarioDef[], 2026-10-19): read them with supplyScenariosOf() and scenarioDefsOf()
   */
  scenarios: string[] | AibScenarioDef[];
  /** the supply scenarios, when meta.scenarios holds the scenario list (additive, 2026-10-19) */
  supplyScenarios?: string[];
  /** the five classes, in order (AIB_CLASSES) */
  classes: string[];
  /** coverage targets of the minimum sufficient coalition, e.g. [0.9, 0.95, 0.99] */
  targets: number[];
  /** the classification settings (config/aib.yml `classification`) */
  thresholds: Record<string, number>;
}

export interface AibCoalition {
  /**
   * the coalition at the middle target: the exact minimum (method "exact ...") when that target is
   * reached; the widest greedy set when it is not (method "greedy_sufficient_coalition ..."). An earlier
   * export wrote [] in that case and kept the set as the roles with member = true: the pages read the
   * members with coalitionMembersOf()
   */
  members: string[];
  /** the exact minimum coalition at each target ([] when not reached) */
  t90: string[];
  t95: string[];
  t99: string[];
  reached99: boolean;
  /** the best minimum coverage of the anchors' demand on the greedy path */
  maxCoverage: number | null;
  /**
   * how the members are found (additive): "exact (binary program: ...)" (with "; the solver hit its time
   * limit ..." when minimality is not proven), or "greedy_sufficient_coalition (...)" when the middle
   * target is out of reach
   */
  method?: string | null;
  /**
   * the exact program's tie-break (among minimum-size coalitions: the widest margin over the target in the
   * weakest node, then the most supply) was solved; false: it hit its time limit (additive)
   */
  robustTieBreakSolved?: boolean;
  /** the greedy sufficient coalition at the middle target, a heuristic ([] when it does not reach it; additive) */
  greedy?: string[];
  /** members no single other candidate can replace at the middle target (additive) */
  criticalPartners?: string[];
  /**
   * members in the coalition only for the simulated capacity they would host as leading investment hosts:
   * their current supply is critical in no node (tier "prospective host"; additive, 2026-10-20)
   */
  prospectiveHosts?: string[];
  /**
   * non-members whose loss pushes a node below the middle target with the coalition fixed (where a node is
   * below it already, lowers its coverage by a point or more), each supplying at least
   * critical_min_supply_share of the anchors' demand (additive)
   */
  criticalExternal?: string[];
  /** the same test, supplying less than that share: critical only because the coalition sits near the target (additive) */
  criticalExternalMarginal?: string[];
  /** other non-members supplying a material share of the anchors' demand in a node (additive) */
  external?: string[];
  /**
   * the associates (the fewest additions to the core coalition, held in) reach the highest target; the
   * highest target's own minimum coalition (t99) is solved apart and need not contain the core (additive)
   */
  associatesReach99?: boolean;
}

export interface AibRole {
  iso3: string;
  role: string;
  basis: string;
  /** in the minimum coalition (false: an associate, or an external dependency) */
  member: boolean;
  /** the coalition tier (AIB_TIERS; additive) */
  tier?: string | null;
  /** a critical supply partner or a critical external dependency, marginal ones included (additive) */
  critical?: boolean;
  /**
   * leave-one-out with the coalition fixed: losing its current supply pushes a node below the middle target
   * (true for nearly every non-anchor member by construction of a minimum coalition, but not for a
   * prospective host, whose simulated capacity is not current supply; additive)
   */
  looCritical?: boolean;
  /**
   * a member's SINGLE substitutes: the non-members any ONE of which, added to the rest of the coalition,
   * restores the middle target when this member's supply is lost (most supply first, at most five). []
   * for a member the rest of the coalition does without, for a critical supply partner (none exists, by
   * definition) and when the target is not reached. An export without substitutesNeeded wrote an earlier
   * reading: the economies the exact program added when the member's supply was lost (additive)
   */
  substitutes?: string[];
  /**
   * how many additions restore the middle target when this member's supply is lost: 0 when the rest of the
   * coalition still reaches it, 1 when a single substitute does, 2 or more for a critical supply partner
   * that only several economies together replace; null when nothing restores it, or the economy was not
   * tested (a non-member, or a target that is not reached) (additive)
   */
  substitutesNeeded?: number | null;
}

export interface AibMetrics {
  demandUsd: number | null;
  lostUsd: number | null;
  exposure: number | null;
  static: number | null;
  existing: number | null;
  trade: number | null;
  investment: number | null;
  dividend: number | null;
  residual: number | null;
  residualAfterInvestment: number | null;
  /** coverage if every economy coordinated (the global optimum; additive) */
  globalOptimum?: number | null;
  /** the demand anchors' coverage through existing relationships: fragmented action (additive) */
  anchorFragmented?: number | null;
  /** the demand anchors' coverage under the compact's minimum coalition (additive) */
  anchorCompact?: number | null;
  /** anchorCompact - anchorFragmented (additive) */
  compactGain?: number | null;
}

export interface AibPathStep {
  step: number;
  /** the economy added at this step (null at step 0: the demand anchors alone) */
  added: string | null;
  members: string[];
  minCoverage: number | null;
  meanCoverage: number | null;
  /** node id -> coverage of the anchors' demand (the exporter writes [] when empty) */
  nodeCoverage: Record<string, number>;
}

export interface AibTradeEdge {
  from: string;
  to: string;
  node: string;
  usd: number | null;
  share: number | null;
  relationship: AibRelationship;
  opportunity: number | null;
}

export interface AibInvestmentEdge {
  investor: string;
  host: string;
  node: string;
  usd: number | null;
  share: number | null;
  friendshore: number | null;
}

export interface AibCompact {
  /** "AIB-C01" */
  id: string;
  /** "Sector A / Sector B: Node 1, Node 2, and N more": display only (labels may contain ", "); build titles with compactTitle() */
  name: string;
  /** the OSI technologies (master) or the EIS sectors (dictionary) of its nodes */
  technologies: string[];
  stages: string[];
  /** the EIS indices of its sectors ([] for master; additive, 2026-10-19) */
  indices?: string[];
  archetype: AibArchetype;
  type: AibCompactType;
  /** node ids */
  nodes: string[];
  coalition: AibCoalition;
  anchors: string[];
  suppliers: string[];
  /** the scaling hosts the coalition takes in */
  hosts: string[];
  /** the specialty hosts the coalition takes in and the leading development hosts of its BUILD nodes (additive, 2026-10-20) */
  specialtyHosts?: string[];
  developmentHosts?: string[];
  technologyPartners: string[];
  bridges: string[];
  associates: string[];
  roles: AibRole[];
  metrics: AibMetrics;
  /** lost-supply shares of the intervention classes, "BUILD 78%; COORDINATE 22%" (additive) */
  classMix?: string | null;
  /** the second class when it holds enough of the lost supply to keep its instruments (additive) */
  secondaryClass?: string | null;
  /** each node's multi-label policy needs, "Sub-sector: BUILD + MANAGE SPECIALIZATION; ..." ("none" when empty; additive) */
  nodePolicyNeeds?: string | null;
  /** the sub-sectors of the compact whose class carries the physical-capability warning (additive) */
  physicalWarnings?: string[];
  /** the instruments of every archetype whose class holds enough of the lost supply */
  policyNeeds: string[];
  path: AibPathStep[];
  trade: AibTradeEdge[];
  investment: AibInvestmentEdge[];
  /** the compact under Managed Interdependence (additive, 2026-10-19) */
  managed?: AibCompactManaged;
  /** the compact as it is today (additive, 2026-10-19) */
  baseline?: AibCompactBaseline;
}

export interface AibFrontierPoint {
  /** the supplier-share limit: no candidate exporter may provide more than this share */
  cap: number;
  coverage: number | null;
  largestShare: number | null;
  largest: string | null;
  /** instantaneous N-1 retained coverage: coverage x (1 - largest share), nothing re-optimized (additive) */
  instantaneousN1?: number | null;
  /** N-1 re-optimized: the largest remaining supplier removed and the trade re-optimized (additive) */
  n1Reoptimized?: number | null;
  /** the supplier the N-1 test removes (additive) */
  n1Removed?: string | null;
  /** the earlier name of instantaneousN1; no longer written, accepted in an older bundle */
  secondShock?: number | null;
  /** supply-weighted specialization score of the optimized supply, 0-1 (additive) */
  specialization?: number | null;
}

/** The class of a node at one bound on how far an existing relationship may grow. */
export interface AibGrowthPoint {
  /** "1.25x", "2x" (the headline), "4x", "unlimited" */
  label: string;
  existing: number | null;
  dividend: number | null;
  cls: string | null;
  /** the class at this bound carries PHYSICAL CAPABILITY NOT VALIDATED (additive) */
  warning?: boolean;
}

export interface AibNode {
  id: string;
  label: string;
  /** the OSI technologies (master) or the node's EIS sector (dictionary) */
  techs: string[];
  stage: string;
  /** the EIS index of the node's sector ("A".."F"; null for master; additive, 2026-10-19) */
  indexCode?: string | null;
  cls: string;
  reason: string | null;
  /** the multi-label policy needs (any of AIB_POLICY_NEEDS; additive) */
  policyNeeds?: string[];
  /** "PHYSICAL CAPABILITY NOT VALIDATED" or null (additive) */
  warning?: string | null;
  /** how physical data bear on the class, in words (additive) */
  physicalValidation?: string | null;
  physicalData?: AibPhysicalData | null;
  /** how far physical and trade concentration differ (additive) */
  physicalDivergence?: number | null;
  /** in the strategic scope (compacts are formed only from these; additive, absent = in scope) */
  inStrategicScope?: boolean;
  /** the class under each other variant: shock id, scenario, "release", "production_constrained", "trade_only" (additive) */
  variants?: Record<string, string | null>;
  /** variant -> the class under it carries PHYSICAL CAPABILITY NOT VALIDATED (the keys of variants; [] when empty; additive) */
  variantWarnings?: Record<string, boolean>;
  /** existing coverage, dividend and class at each existing-relationship growth bound (additive) */
  growth?: AibGrowthPoint[];
  /** COORDINATE under some growth bounds and another class under others (additive) */
  coordinateSensitiveToGrowth?: boolean;
  /** at the headline supplier limit: N-1 re-optimized and instantaneous N-1 coverage (additive) */
  n1Reoptimized?: number | null;
  instantaneousN1?: number | null;
  /** share of demand the optimized solution serves with exports redirected from markets outside the demand scope (additive) */
  diversion?: number | null;
  /** the node in the diversification scenario (additive, 2026-10-19; absent in an older bundle and for a node it did not assess) */
  diversify?: AibNodeDiversify;
  /** the node as it is today, before any shock (additive, 2026-10-19; every optimized node) */
  baseline?: AibNodeBaseline;
  quality: string | null;
  flags: string[];
  exposure: number | null;
  static: number | null;
  existing: number | null;
  optimized: number | null;
  dividend: number | null;
  residual: number | null;
  demandUsd: number | null;
  lostUsd: number | null;
  baseLargest: string | null;
  baseLargestShare: number | null;
  largest: string | null;
  largestShare: number | null;
  hhi: number | null;
  /** the leading scaling hosts (established producers) */
  hosts: string[];
  /** the leading specialty hosts (high-income specialists below scale) and development hosts (middle-income, below scale) (additive, 2026-10-20) */
  specialtyHosts?: string[];
  developmentHosts?: string[];
  bestGain: number | null;
  /**
   * the best host the alignment screen keeps out (an economy outside the demand scope in an excluded UNGA
   * bloc), its resilience gain and the alignment cost: the gain the eligible hosts fall short of it by, 0
   * when one does as well; absent where no host was screened (additive, 2026-10-20)
   */
  screenedHost?: AibScreenedHost;
  partners: string | null;
  treatment: string | null;
  compact: string | null;
  yfCells: string[];
  yfSubcomponents: string[];
  /** the tightest supplier-share limit that costs at most 2 points of coverage (additive) */
  tightestCostless?: number | null;
  /** the loosest limit at which coverage stays >= 80% after the N-1 test (re-optimized where the bundle has it); null when none does (additive) */
  minRequiredDiversification?: number | null;
  frontier: AibFrontierPoint[];
}

export interface AibDoc {
  schema: typeof AIB_SCHEMA;
  version: number;
  meta: AibMeta;
  /** ISO3 -> name for every code used (the exporter writes [] when there is none) */
  countries: Record<string, string>;
  /** ordered by lost supply, largest first */
  compacts: AibCompact[];
  /** every classified sub-sector node, in a compact or not */
  nodes: AibNode[];
  /** the emergent AIB network of the headline variant (additive, 2026-10-17) */
  network?: AibNetwork;
  /** the investor -> host flows of the marginal investment simulation, headline variant (additive, 2026-10-17) */
  investmentFlows?: AibInvestmentFlows;
}

// ---- Emergent network and investment flows (additive, 2026-10-17) ------------------------------------

/** The run a network or investment section comes from (one variant: the headline). */
export interface AibVariant {
  shock: string;
  scenario: string;
  shockSemantics: string | null;
  supplyConstraint: string | null;
  strategicScope: string | null;
  demandScope: string | null;
  candidateUniverse: string | null;
  universe: string | null;
}

/** The four network roles of emergent_aib_country_summary.csv, as the model names them. */
export const AIB_NETWORK_ROLES = ['supplier', 'buyer', 'investment_host', 'investor'] as const;
export type AibNetworkRole = (typeof AIB_NETWORK_ROLES)[number];

export const AIB_STAGES = ['Upstream', 'Midstream', 'Downstream'] as const;

/**
 * One side of an economy in the network (scripts/32, emergent_aib_country_summary.csv): its distinct
 * partners, the (node, shock) relationships the solution selects, and its contributions, in shares of a
 * node's demand summed over the nodes and partners of the role (so above 1 is possible): `headline` under
 * the headline shock, `cumulative` summed over every configured shock. Partner counts by stage.
 */
export interface AibNetworkSide {
  partners: number;
  selected: number;
  headline: number;
  cumulative: number;
  upstream: number;
  midstream: number;
  downstream: number;
  /** trade sides only: the part of `cumulative` carried by new relationships */
  dividend?: number;
  /** trade sides only: the optimized trade of the headline shock (USD a year) */
  volumeUsd?: number;
}

export interface AibNetworkCountry {
  iso3: string;
  /** the model's own network roles (AIB_NETWORK_ROLES): every side with a selected relationship */
  roles: string[];
  technologies: string[];
  /** supply it loses to the headline shock (USD a year; null: not an importer in the demand scope) */
  lostUsd: number | null;
  /** nodes in which it loses supply */
  exposedNodes: number | null;
  /** exporting to buyers */
  supplier: AibNetworkSide;
  /** importing from suppliers */
  buyer: AibNetworkSide;
  /** hosting new capacity that serves investors */
  host: AibNetworkSide;
  /** the economy whose gap the new capacity at a host serves */
  investor: AibNetworkSide;
}

/**
 * One bilateral relationship, oriented as goods move (supplier -> buyer): trade runs exporter -> importer;
 * investment runs host -> investor (the host's new capacity serves the investor, so `to` invests in
 * `from`). Only each node's best-ranked host counts as an investment relationship.
 */
export interface AibNetworkEdge {
  from: string;
  to: string;
  /** (node, shock) trade relationships selected */
  nTrade: number;
  /** (node, shock) investment relationships selected */
  nInvestment: number;
  nShocks: number;
  nodes: string[];
  techs: string[];
  stages: string[];
  /** relationships in nodes where the shock removes at least the high-severity share */
  nHighSeverity: number;
  /** optimized trade of the headline shock (USD a year) */
  tradeUsd: number;
  /** headline shock: share of node demand the trade serves (summed over nodes) */
  trade: number;
  /** headline shock: share of node demand the new capacity serves */
  investment: number;
  /** every shock: trade, the part of it in new relationships, investment */
  cumTrade: number;
  cumDividend: number;
  cumInvestment: number;
  /** mean Export Opportunity (supplier -> buyer) and Friendshore (buyer -> supplier) scores, NEIS defaults */
  opportunity: number | null;
  friendshore: number | null;
}

export interface AibNetwork {
  /** with the shocks summed into the cumulative figures */
  variant: AibVariant & { shocks?: string[] };
  countries: AibNetworkCountry[];
  edges: AibNetworkEdge[];
}

/**
 * One investor -> host flow: in the simulation of `rank`-th host of a node, the supply of the new capacity
 * that serves the investor's lost demand. Each rank is a SEPARATE simulation of the increment (the same
 * nominal share of node demand, capped at the host's current scale in the node): only rank 1 is the
 * selected host; flows of different ranks are alternatives and must not be added up.
 */
export interface AibInvestmentPair {
  investor: string;
  host: string;
  node: string;
  rank: number;
  /** supply from the new capacity (USD a year) */
  usd: number;
  /** the same as a share of the node's demand */
  share: number | null;
  /** the investor's lost supply as a share of the node's demand */
  lostShare: number | null;
  /** the investor's Friendshore score for the host, NEIS defaults */
  friendshore: number | null;
  /** the host's Export Opportunity score for the investor, NEIS defaults */
  opportunity: number | null;
}

/** A node's best screened host (allied_investment_screened_hosts.csv, the headline increment; additive, 2026-10-20). */
export interface AibScreenedHost {
  host: string;
  /** its resilience gain with the diversified supplier limit (the gain the ranking reads) */
  gain: number | null;
  /** the alignment cost: how far the best eligible host's gain falls short of it (>= 0) */
  cost: number | null;
}

/** A ranked host of a node (allied_investment_partnerships.csv, the headline increment). */
export interface AibInvestmentHost {
  node: string;
  host: string;
  rank: number;
  /** marginal resilience gain: coverage after minus before */
  gain: number | null;
  before: number | null;
  after: number | null;
  /** share of the increment used */
  newCapacityUsed: number | null;
  /** additional demand served (USD a year) */
  servedUsd: number | null;
  /** 3 = the strongest capability evidence */
  capabilityTier: number | null;
  capabilityBasis: string | null;
  /**
   * the increment simulated at this host, a share of node demand: the nominal increment capped at a multiple
   * of the host's current scale in the node (additive, 2026-10-20)
   */
  hostIncrement?: number | null;
  /**
   * the ranking criterion that placed the host against the next one up (rank 1: against rank 2):
   * "resilience gain", "diversification (HHI change)", "capability tier", "capability at scale",
   * "Friendshore", "compatibility" or "tied" (additive, 2026-10-20)
   */
  rankBasis?: string | null;
  /** the host's type: scaling, specialty, development, or '' for none (additive, 2026-10-20) */
  type?: string | null;
}

export interface AibInvestmentFlows {
  /** with the simulated increment (share of node demand) and the deepest host rank kept */
  variant: AibVariant & { increment: number; maxRank: number };
  pairs: AibInvestmentPair[];
  hosts: AibInvestmentHost[];
}

// ---- Scenarios (additive, 2026-10-19) ----------------------------------------------------------------

/**
 * The scenarios, in the order every comparison shows them: Baseline (trade as it is today, 2024, before any
 * shock), Managed Interdependence (the diversify_benchmark shock) and Decoupling (the headline shock under
 * hold: China stops supplying the allied market and keeps buying; the main document).
 */
export const AIB_SCENARIO_IDS = ['baseline', 'managed', 'decoupling'] as const;
export type AibScenarioId = (typeof AIB_SCENARIO_IDS)[number];

/** The scenarios with a lazily loaded file of their own (Decoupling is the main document). */
export const AIB_SCENARIO_FILE_IDS = ['managed', 'baseline'] as const;
export type AibScenarioFileId = (typeof AIB_SCENARIO_FILE_IDS)[number];

export const AIB_SCENARIO_SCHEMA = 'osi-aib-scenario';
export const AIB_SCENARIO_VERSION = 1;
/** A scenario file name scripts/84 writes (and deletes when stale); the group is the scenario. */
export const AIB_SCENARIO_FILE_RE = /^scenario-(managed|baseline)\.[0-9a-f]{8}\.json$/;

/** One scenario of meta.scenarios: its label, its shock (null for baseline) and its words. */
export interface AibScenarioDef {
  id: AibScenarioId;
  label: string;
  shockId: string | null;
  words: string;
}

/**
 * A China compact under Managed Interdependence (scenario-managed, compacts[]): the same id and nodes as
 * in the main document, solved under the scenario. coalition is null (with coalitionReason) when the
 * scenario ran on none of its nodes. The arrays are [] when the export leaves them out (the store fills
 * them: normalizeScenarioDoc()).
 */
export interface AibScenarioCompact {
  id: string;
  /** its nodes the scenario ran on */
  nodesRun: number;
  coalition: AibCoalition | null;
  /** why coalition is null ("no node of this compact is above its benchmark by more than a small tolerance"); null otherwise */
  coalitionReason: string | null;
  /**
   * The compact's type, archetype, policy needs and secondary class under the scenario (from its classes,
   * compact gain and coalition size; 2026-10-20, optional in the file): null / [] without a coalition or in
   * an older file (normalizeScenarioDoc()).
   */
  type: AibCompactType | null;
  archetype: AibArchetype | null;
  policyNeeds: string[];
  secondaryClass: string | null;
  anchors: string[];
  suppliers: string[];
  /** the scaling hosts the coalition takes in */
  hosts: string[];
  /** the specialty hosts the coalition takes in and the leading development hosts of its BUILD nodes (additive, 2026-10-20) */
  specialtyHosts?: string[];
  developmentHosts?: string[];
  technologyPartners: string[];
  bridges: string[];
  associates: string[];
  roles: AibRole[];
  /** null when the export writes none (a compact without a coalition) */
  metrics: AibMetrics | null;
  path: AibPathStep[];
  /** the scenario's trade relationships: top 40 by USD, domestic retention excluded */
  trade: AibTradeEdge[];
  investment: AibInvestmentEdge[];
}

/** Today's suppliers of one node (scenario-baseline, nodes[]): the top ten, shares of the node's allied demand. */
export interface AibBaselineNodeSuppliers {
  id: string;
  suppliers: AibSupplierFlow[];
}

/** Today's suppliers of one compact (scenario-baseline, compacts[]): the top ten over its nodes, shares of its allied demand. */
export interface AibBaselineCompactSuppliers {
  id: string;
  suppliers: AibSupplierFlow[];
}

interface AibScenarioDocBase {
  schema: typeof AIB_SCENARIO_SCHEMA;
  version: number;
  created: string;
  /** ISO3 -> name for every code used in the file ([] in the export when empty; read with scenarioNamesOf()) */
  countries: Record<string, string>;
}

/**
 * scenario-managed.<h8>.json: the China compacts solved under Managed Interdependence; the network of the
 * scenario's shock alone (its headline fields are the scenario's; cum* = headline; variant.shock and
 * variant.shocks name diversify_benchmark); its investor -> host flows (same increments and ranks as the main
 * document).
 */
export interface AibManagedScenarioDoc extends AibScenarioDocBase {
  scenario: 'managed';
  shockId: string;
  compacts: AibScenarioCompact[];
  network?: AibNetwork;
  investmentFlows?: AibInvestmentFlows;
}

/**
 * scenario-baseline.<h8>.json: trade as it is today. network: observed 2024 supplier -> buyer pairs in the
 * strategic scope, demand-scope buyers, pairs whose summed share of node demand is at least 0.005 (trade =
 * observed share summed over nodes, tradeUsd = observed USD, investment 0, cum* = headline, nShocks 0,
 * variant.shock "baseline", variant.scenario "observed", variant.shocks []); nodes and compacts: their top
 * ten observed suppliers.
 */
export interface AibBaselineScenarioDoc extends AibScenarioDocBase {
  scenario: 'baseline';
  shockId: null;
  network?: AibNetwork;
  nodes: AibBaselineNodeSuppliers[];
  compacts: AibBaselineCompactSuppliers[];
}

export type AibScenarioDoc = AibManagedScenarioDoc | AibBaselineScenarioDoc;
/** The scenario file of each id. */
export type AibScenarioDocOf<K extends AibScenarioFileId> = K extends 'managed' ? AibManagedScenarioDoc : AibBaselineScenarioDoc;

// ---- Runtime checks ----------------------------------------------------------------------------------

export class AibContractError extends Error {
  readonly problems: string[];
  constructor(what: string, problems: string[]) {
    super(`${what}: ${problems.slice(0, 5).join('; ')}${problems.length > 5 ? ` (+${problems.length - 5} more)` : ''}`);
    this.name = 'AibContractError';
    this.problems = problems;
  }
}

type Obj = Record<string, unknown>;
const isObj = (x: unknown): x is Obj => typeof x === 'object' && x !== null && !Array.isArray(x);
const isStr = (x: unknown): x is string => typeof x === 'string';
const isNum = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);
const MAX_PROBLEMS = 200;

class Problems {
  readonly list: string[] = [];
  add(msg: string): void {
    if (this.list.length < MAX_PROBLEMS) this.list.push(msg);
  }
  str(x: unknown, path: string, nullable = false): void {
    if (!(isStr(x) || (nullable && x === null))) this.add(`${path}: expected a string${nullable ? ' or null' : ''}`);
  }
  /** a finite number, or null when nullable; optional bounds */
  num(x: unknown, path: string, opts: { nullable?: boolean; min?: number; max?: number; int?: boolean } = {}): void {
    if (x === null && opts.nullable) return;
    if (!isNum(x)) {
      this.add(`${path}: expected a number${opts.nullable ? ' or null' : ''}`);
      return;
    }
    if (opts.min !== undefined && x < opts.min - 1e-9) this.add(`${path}: ${x} below ${opts.min}`);
    if (opts.max !== undefined && x > opts.max + 1e-9) this.add(`${path}: ${x} above ${opts.max}`);
    if (opts.int && !Number.isInteger(x)) this.add(`${path}: ${x} is not an integer`);
  }
  /** an additive field: absent is fine, else a number or null with the bounds */
  optNum(x: unknown, path: string, opts: { min?: number; max?: number } = {}): void {
    if (x === undefined) return;
    this.num(x, path, { ...opts, nullable: true });
  }
  /** an additive field: absent is fine, else a string or null */
  optStr(x: unknown, path: string): void {
    if (x === undefined) return;
    this.str(x, path, true);
  }
  bool(x: unknown, path: string): void {
    if (typeof x !== 'boolean') this.add(`${path}: expected true or false`);
  }
  strArr(x: unknown, path: string): x is string[] {
    if (!Array.isArray(x)) {
      this.add(`${path}: expected an array${isStr(x) ? ' (a one-item array collapsed to a string?)' : ''}`);
      return false;
    }
    const bad = x.findIndex((v) => !isStr(v));
    if (bad >= 0) {
      this.add(`${path}[${bad}]: expected a string`);
      return false;
    }
    return true;
  }
  arr(x: unknown, path: string): x is unknown[] {
    if (!Array.isArray(x)) {
      this.add(`${path}: expected an array${isObj(x) ? ' (a one-item array collapsed to an object?)' : ''}`);
      return false;
    }
    return true;
  }
  oneOf<T>(x: unknown, vals: readonly T[], path: string): void {
    if (!vals.includes(x as T)) this.add(`${path}: '${String(x)}' is not one of ${vals.join(', ')}`);
  }
  /** an additive field: absent or null is fine, else one of the values */
  optOneOf<T>(x: unknown, vals: readonly T[], path: string): void {
    if (x === undefined || x === null) return;
    this.oneOf(x, vals, path);
  }
  /** an additive field: absent is fine, else true or false */
  optBool(x: unknown, path: string): void {
    if (x === undefined) return;
    this.bool(x, path);
  }
  /** an additive field: absent is fine, else an array of strings (each one of `vals` when given) */
  optStrArr(x: unknown, path: string, vals?: readonly string[]): void {
    if (x === undefined) return;
    if (this.strArr(x, path) && vals) x.forEach((v, i) => this.oneOf(v, vals, `${path}[${i}]`));
  }
}

/** A share in [0, 1] (or null); a small tolerance for rounding. */
const SHARE = { nullable: true, min: 0, max: 1 } as const;

/** Problems with data/aib/manifest.json (empty when it is valid). */
export function checkAibManifest(x: unknown): string[] {
  if (!isObj(x)) return ['manifest: not an object'];
  const p = new Problems();
  if (x.schema !== AIB_SCHEMA) p.add(`manifest.schema: expected '${AIB_SCHEMA}'`);
  if (!isNum(x.version)) p.add('manifest.version: not a number');
  else if (Math.floor(x.version) > AIB_VERSION) p.add(`manifest: version ${x.version} needs a newer app`);
  for (const k of ['created', 'universe', 'headlineShock', 'headlineScenario']) p.str(x[k], `manifest.${k}`);
  for (const k of ['demandScope', 'candidateUniverse', 'shockSemantics', 'supplyConstraint']) p.optStr(x[k], `manifest.${k}`);
  if (!isObj(x.files)) p.add('manifest.files: not an object');
  else {
    if (!isStr(x.files.compacts) || !AIB_DOC_FILE_RE.test(x.files.compacts)) p.add('manifest.files.compacts: not a compacts.<8 hex>.json file name');
    checkScenarioFiles(p, x.files.scenarios);
  }
  return p.list;
}

/**
 * manifest.files.scenarios (2026-10-19): absent or null is fine; else an object whose managed and baseline
 * entries (each absent, null, or a scenario-<id>.<8 hex>.json name of that same scenario) are checked; other
 * keys are ignored (a newer export's scenario this app does not show).
 */
function checkScenarioFiles(p: Problems, s: unknown): void {
  if (s === undefined || s === null) return;
  if (!isObj(s)) return p.add('manifest.files.scenarios: not an object');
  for (const id of AIB_SCENARIO_FILE_IDS) {
    const f = s[id];
    if (f === undefined || f === null) continue;
    const m = isStr(f) ? AIB_SCENARIO_FILE_RE.exec(f) : null;
    if (!m || m[1] !== id) p.add(`manifest.files.scenarios.${id}: not a scenario-${id}.<8 hex>.json file name`);
  }
}

/** True when a manifest's version is newer than this app reads (the module asks for a reload). */
export const needsNewerApp = (problems: readonly string[]): boolean => problems.some((q) => /needs a newer app/.test(q));

function checkMeta(p: Problems, m: unknown): void {
  if (!isObj(m)) return p.add('meta: not an object');
  for (const k of ['universe', 'headlineShock', 'headlineScenario', 'built']) p.str(m[k], `meta.${k}`);
  p.num(m.tradeYear, 'meta.tradeYear', { int: true });
  if (p.arr(m.shocks, 'meta.shocks')) {
    m.shocks.forEach((s, i) => {
      if (!isObj(s)) return p.add(`meta.shocks[${i}]: not an object`);
      p.str(s.id, `meta.shocks[${i}].id`);
      p.str(s.label, `meta.shocks[${i}].label`);
    });
  }
  checkMetaScenarios(p, m.scenarios);
  p.optStrArr(m.supplyScenarios, 'meta.supplyScenarios');
  if (p.strArr(m.classes, 'meta.classes')) {
    for (const c of m.classes) if (!(AIB_CLASSES as readonly string[]).includes(c)) p.add(`meta.classes: '${c}' is not one of ${AIB_CLASSES.join(', ')}`);
  }
  if (p.arr(m.targets, 'meta.targets')) m.targets.forEach((t, i) => p.num(t, `meta.targets[${i}]`, { min: 0, max: 1 }));
  if (!isObj(m.thresholds)) p.add('meta.thresholds: not an object');
  else for (const [k, v] of Object.entries(m.thresholds)) p.num(v, `meta.thresholds.${k}`);
  // the methodology settings (additive)
  p.optOneOf(m.demandScope, AIB_SCOPE_MODES, 'meta.demandScope');
  p.optOneOf(m.candidateUniverse, AIB_SCOPE_MODES, 'meta.candidateUniverse');
  if (m.nDemandScope !== undefined) p.num(m.nDemandScope, 'meta.nDemandScope', { nullable: true, min: 0, int: true });
  p.optOneOf(m.shockSemantics, AIB_SEMANTICS, 'meta.shockSemantics');
  p.optOneOf(m.supplyConstraint, AIB_CONSTRAINTS, 'meta.supplyConstraint');
  p.optStrArr(m.semantics, 'meta.semantics', AIB_SEMANTICS);
  p.optStrArr(m.constraints, 'meta.constraints', AIB_CONSTRAINTS);
  // any configured scope: scripts/32 accepts every key under strategic_scope.technologies
  p.optStr(m.strategicScope, 'meta.strategicScope');
  p.optStrArr(m.scopeTechs, 'meta.scopeTechs');
  // the taxonomy (2026-10-19): written as strings (a value this app does not know reads as master)
  p.optStr(m.taxonomy, 'meta.taxonomy');
  p.optStr(m.dictionaryScope, 'meta.dictionaryScope');
  if (m.indices !== undefined && p.arr(m.indices, 'meta.indices')) {
    const seen = new Set<string>();
    m.indices.forEach((ix, i) => {
      const at = `meta.indices[${i}]`;
      if (!isObj(ix)) return p.add(`${at}: not an object`);
      p.str(ix.code, `${at}.code`);
      if (isStr(ix.code)) {
        if (!AIB_INDEX_CODE_RE.test(ix.code)) p.add(`${at}.code: '${ix.code}' is not one capital letter`);
        if (seen.has(ix.code)) p.add(`${at}.code: duplicate '${ix.code}'`);
        seen.add(ix.code);
      }
      p.str(ix.name, `${at}.name`);
      p.strArr(ix.sectors, `${at}.sectors`);
    });
  }
  // the diversification scenario (2026-10-19): absent in an older bundle; the benchmark is any configured name
  const dv = m.diversify;
  if (dv !== undefined) {
    if (!isObj(dv)) p.add('meta.diversify: not an object');
    else {
      p.str(dv.shockId, 'meta.diversify.shockId');
      p.optStr(dv.label, 'meta.diversify.label');
      p.optStr(dv.benchmark, 'meta.diversify.benchmark');
      p.optNum(dv.minExcess, 'meta.diversify.minExcess', { min: 0, max: 1 });
    }
  }
}

/**
 * meta.scenarios: an array of strings (the supply scenarios: an older bundle) or of scenario objects
 * ({id one of AIB_SCENARIO_IDS, label, shockId a string or null, words}; 2026-10-19), not both.
 */
function checkMetaScenarios(p: Problems, s: unknown): void {
  if (!p.arr(s, 'meta.scenarios')) return;
  if (s.every(isStr)) return;
  const seen = new Set<string>();
  s.forEach((x, i) => {
    const at = `meta.scenarios[${i}]`;
    if (!isObj(x)) return p.add(`${at}: expected a scenario object (meta.scenarios mixes names and scenario objects)`);
    p.oneOf(x.id, AIB_SCENARIO_IDS, `${at}.id`);
    if (isStr(x.id)) {
      if (seen.has(x.id)) p.add(`${at}.id: duplicate '${x.id}'`);
      seen.add(x.id);
    }
    p.str(x.label, `${at}.label`);
    p.str(x.shockId, `${at}.shockId`, true);
    p.str(x.words, `${at}.words`);
  });
}

/** [ISO3, number...] tuples (node / compact suppliers): an array of arrays of `n` items, the first an ISO3 string, then USD >= 0 (n = 3) and a share. */
function checkSupplierTuples(p: Problems, x: unknown, at: string, n: 2 | 3): void {
  if (!p.arr(x, at)) return;
  x.forEach((t, i) => {
    const ta = `${at}[${i}]`;
    if (!Array.isArray(t) || t.length !== n) return p.add(`${ta}: expected [${n === 3 ? 'ISO3, USD, share' : 'ISO3, share'}]`);
    p.str(t[0], `${ta}[0]`);
    if (n === 3) p.num(t[1], `${ta}[1]`, { nullable: true, min: 0 });
    p.num(t[n - 1], `${ta}[${n - 1}]`, SHARE);
  });
}

/** node.baseline (2026-10-19): absent is fine; else shares in 0-1 or null, the largest supplier a string or null, the top suppliers [ISO3, share]. */
function checkNodeBaseline(p: Problems, b: unknown, at: string): void {
  if (b === undefined) return;
  if (!isObj(b)) return p.add(`${at}: not an object`);
  for (const k of ['hhi', 'top3Share', 'largestShare']) p.num(b[k], `${at}.${k}`, SHARE);
  p.str(b.largest, `${at}.largest`, true);
  checkSupplierTuples(p, b.suppliers, `${at}.suppliers`, 2);
}

/**
 * node.diversify (2026-10-19): absent is fine; else the status (one of AIB_DIVERSIFY_STATUSES), shares in
 * 0-1 or null, the supply only new capacity fills in USD (>= 0 or null), the capped suppliers and the hosts
 * as arrays of strings, the basis and the relationships as strings or null, and the class under the
 * scenario null or one of meta.classes.
 */
function checkNodeDiversify(p: Problems, d: unknown, at: string, classes: readonly string[]): void {
  if (d === undefined) return;
  if (!isObj(d)) return p.add(`${at}: not an object`);
  p.oneOf(d.status, AIB_DIVERSIFY_STATUSES, `${at}.status`);
  for (const k of ['currentHhi', 'benchmarkHhi', 'cap', 'excess', 'static', 'existing', 'optimized', 'residual']) p.num(d[k], `${at}.${k}`, SHARE);
  p.num(d.residualUsd, `${at}.residualUsd`, { nullable: true, min: 0 });
  for (const k of ['basis', 'partners']) p.str(d[k], `${at}.${k}`, true);
  for (const k of ['capped', 'hosts']) p.strArr(d[k], `${at}.${k}`);
  for (const k of ['specialtyHosts', 'developmentHosts']) p.optStrArr(d[k], `${at}.${k}`);
  checkCls(p, d.cls, `${at}.cls`, classes);
  // the scenario toggle (2026-10-19; each absent, or null where the scenario did not run)
  p.optNum(d.lostUsd, `${at}.lostUsd`, { min: 0 });
  p.optNum(d.dividend, `${at}.dividend`, { min: -1, max: 1 });
  for (const k of ['hhi', 'largestShare', 'top3Share']) p.optNum(d[k], `${at}.${k}`, { min: 0, max: 1 });
  p.optStr(d.largest, `${at}.largest`);
  for (const k of ['newLinks', 'expandedLinks']) if (d[k] !== undefined) p.num(d[k], `${at}.${k}`, { nullable: true, min: 0, int: true });
}

/** node.screenedHost (2026-10-20): absent is fine; else the host (a string), its gain (-1..1 or null) and the cost (0..1 or null). */
function checkNodeScreenedHost(p: Problems, s: unknown, at: string): void {
  if (s === undefined) return;
  if (!isObj(s)) return p.add(`${at}: not an object`);
  p.str(s.host, `${at}.host`);
  p.num(s.gain, `${at}.gain`, { nullable: true, min: -1, max: 1 });
  p.num(s.cost, `${at}.cost`, { nullable: true, min: 0, max: 1 });
}

/** The index codes of meta.indices (empty when the bundle has none: index fields are then not cross-checked). */
function indexCodesOf(m: unknown): Set<string> {
  const out = new Set<string>();
  if (isObj(m) && Array.isArray(m.indices)) for (const ix of m.indices) if (isObj(ix) && isStr(ix.code)) out.add(ix.code);
  return out;
}

/** An additive index code (node.indexCode): absent or null is fine; else a string, one of meta.indices when it lists any. */
function checkIndexCode(p: Problems, x: unknown, path: string, codes: ReadonlySet<string>): void {
  if (x === undefined || x === null) return;
  if (!isStr(x)) return p.add(`${path}: expected a string or null`);
  if (codes.size && !codes.has(x)) p.add(`${path}: '${x}' is not an index of meta.indices`);
}

/** A class written beside the headline one (a variant, a growth bound): null, or one of meta.classes. */
function checkCls(p: Problems, x: unknown, path: string, classes: readonly string[]): void {
  if (x === null) return;
  if (!isStr(x)) p.add(`${path}: expected a string or null`);
  else if (!classes.includes(x)) p.add(`${path}: '${x}' is not one of meta.classes`);
}

function checkNode(p: Problems, n: unknown, at: string, classes: readonly string[], codes: ReadonlySet<string> = new Set()): void {
  if (!isObj(n)) return p.add(`${at}: not an object`);
  for (const k of ['id', 'label', 'stage']) p.str(n[k], `${at}.${k}`);
  checkIndexCode(p, n.indexCode, `${at}.indexCode`, codes);
  if (!isStr(n.cls)) p.add(`${at}.cls: expected a string`);
  else if (!classes.includes(n.cls)) p.add(`${at}.cls: '${n.cls}' is not one of meta.classes`);
  for (const k of ['reason', 'quality', 'baseLargest', 'largest', 'partners', 'treatment', 'compact']) p.str(n[k], `${at}.${k}`, true);
  for (const k of ['techs', 'flags', 'hosts', 'yfCells', 'yfSubcomponents']) p.strArr(n[k], `${at}.${k}`);
  for (const k of ['specialtyHosts', 'developmentHosts']) p.optStrArr(n[k], `${at}.${k}`);
  for (const k of ['exposure', 'static', 'existing', 'optimized', 'residual', 'baseLargestShare', 'largestShare', 'hhi']) p.num(n[k], `${at}.${k}`, SHARE);
  // the dividend and the best marginal gain are differences of shares
  for (const k of ['dividend', 'bestGain']) p.num(n[k], `${at}.${k}`, { nullable: true, min: -1, max: 1 });
  for (const k of ['demandUsd', 'lostUsd']) p.num(n[k], `${at}.${k}`, { nullable: true, min: 0 });
  for (const k of ['tightestCostless', 'minRequiredDiversification']) p.optNum(n[k], `${at}.${k}`, { min: 0, max: 1 });
  // the methodology fields (additive)
  p.optStrArr(n.policyNeeds, `${at}.policyNeeds`, AIB_POLICY_NEEDS);
  for (const k of ['warning', 'physicalValidation']) p.optStr(n[k], `${at}.${k}`);
  p.optOneOf(n.physicalData, AIB_PHYSICAL_DATA, `${at}.physicalData`);
  p.optNum(n.physicalDivergence, `${at}.physicalDivergence`, { min: -1, max: 1 });
  p.optBool(n.inStrategicScope, `${at}.inStrategicScope`);
  p.optBool(n.coordinateSensitiveToGrowth, `${at}.coordinateSensitiveToGrowth`);
  for (const k of ['n1Reoptimized', 'instantaneousN1']) p.optNum(n[k], `${at}.${k}`, { min: 0, max: 1 });
  p.optNum(n.diversion, `${at}.diversion`, { min: 0 });
  checkNodeDiversify(p, n.diversify, `${at}.diversify`, classes);
  checkNodeBaseline(p, n.baseline, `${at}.baseline`);
  checkNodeScreenedHost(p, n.screenedHost, `${at}.screenedHost`);
  const v = n.variants;
  // a named list; [] when empty (jsonlite)
  if (v !== undefined) {
    if (Array.isArray(v) ? v.length > 0 : !isObj(v)) p.add(`${at}.variants: expected an object of variant -> class`);
    else if (isObj(v)) for (const [k, c] of Object.entries(v)) checkCls(p, c, `${at}.variants.${k}`, classes);
  }
  const vw = n.variantWarnings;
  if (vw !== undefined) {
    if (Array.isArray(vw) ? vw.length > 0 : !isObj(vw)) p.add(`${at}.variantWarnings: expected an object of variant -> true or false`);
    else if (isObj(vw)) for (const [k, w] of Object.entries(vw)) p.bool(w, `${at}.variantWarnings.${k}`);
  }
  if (n.growth !== undefined && p.arr(n.growth, `${at}.growth`)) {
    n.growth.forEach((g, i) => {
      const ga = `${at}.growth[${i}]`;
      if (!isObj(g)) return p.add(`${ga}: not an object`);
      p.str(g.label, `${ga}.label`);
      p.num(g.existing, `${ga}.existing`, SHARE);
      p.num(g.dividend, `${ga}.dividend`, { nullable: true, min: -1, max: 1 });
      checkCls(p, g.cls, `${ga}.cls`, classes);
      p.optBool(g.warning, `${ga}.warning`);
    });
  }
  if (p.arr(n.frontier, `${at}.frontier`)) {
    n.frontier.forEach((f, i) => {
      const fa = `${at}.frontier[${i}]`;
      if (!isObj(f)) return p.add(`${fa}: not an object`);
      p.num(f.cap, `${fa}.cap`, { min: 0, max: 1 });
      p.num(f.coverage, `${fa}.coverage`, SHARE);
      p.num(f.largestShare, `${fa}.largestShare`, SHARE);
      p.str(f.largest, `${fa}.largest`, true);
      for (const k of ['instantaneousN1', 'n1Reoptimized', 'secondShock', 'specialization']) p.optNum(f[k], `${fa}.${k}`, { min: 0, max: 1 });
      p.optStr(f.n1Removed, `${fa}.n1Removed`);
    });
  }
}

/** compact.coalition (an object; the R mirror and checkScenarioCompact() share it). */
function checkCoalition(p: Problems, co: unknown, at: string): void {
  if (!isObj(co)) return p.add(`${at}: not an object`);
  for (const k of ['members', 't90', 't95', 't99']) p.strArr(co[k], `${at}.${k}`);
  p.bool(co.reached99, `${at}.reached99`);
  p.num(co.maxCoverage, `${at}.maxCoverage`, SHARE);
  p.optStr(co.method, `${at}.method`);
  for (const k of ['greedy', 'criticalPartners', 'prospectiveHosts', 'criticalExternal', 'criticalExternalMarginal', 'external']) p.optStrArr(co[k], `${at}.${k}`);
  for (const k of ['associatesReach99', 'robustTieBreakSolved']) p.optBool(co[k], `${at}.${k}`);
}

function checkRoles(p: Problems, roles: unknown, at: string): void {
  if (!p.arr(roles, at)) return;
  roles.forEach((r, i) => {
    const ra = `${at}[${i}]`;
    if (!isObj(r)) return p.add(`${ra}: not an object`);
    for (const k of ['iso3', 'role', 'basis']) p.str(r[k], `${ra}.${k}`);
    p.bool(r.member, `${ra}.member`);
    // an unknown tier is shown as written; only its type is checked
    p.optStr(r.tier, `${ra}.tier`);
    p.optBool(r.critical, `${ra}.critical`);
    p.optBool(r.looCritical, `${ra}.looCritical`);
    p.optStrArr(r.substitutes, `${ra}.substitutes`);
    if (r.substitutesNeeded !== undefined) p.num(r.substitutesNeeded, `${ra}.substitutesNeeded`, { nullable: true, min: 0, int: true });
  });
}

function checkMetrics(p: Problems, m: unknown, at: string): void {
  if (!isObj(m)) return p.add(`${at}: not an object`);
  for (const k of ['demandUsd', 'lostUsd']) p.num(m[k], `${at}.${k}`, { nullable: true, min: 0 });
  for (const k of ['exposure', 'static', 'existing', 'trade', 'investment', 'residual', 'residualAfterInvestment']) p.num(m[k], `${at}.${k}`, SHARE);
  p.num(m.dividend, `${at}.dividend`, { nullable: true, min: -1, max: 1 });
  for (const k of ['globalOptimum', 'anchorFragmented', 'anchorCompact']) p.optNum(m[k], `${at}.${k}`, { min: 0, max: 1 });
  p.optNum(m.compactGain, `${at}.compactGain`, { min: -1, max: 1 });
}

function checkPath(p: Problems, path: unknown, at: string): void {
  if (!p.arr(path, at)) return;
  path.forEach((s, i) => {
    const sa = `${at}[${i}]`;
    if (!isObj(s)) return p.add(`${sa}: not an object`);
    p.num(s.step, `${sa}.step`, { min: 0, int: true });
    p.str(s.added, `${sa}.added`, true);
    p.strArr(s.members, `${sa}.members`);
    p.num(s.minCoverage, `${sa}.minCoverage`, SHARE);
    p.num(s.meanCoverage, `${sa}.meanCoverage`, SHARE);
    const nc = s.nodeCoverage;
    if (Array.isArray(nc) ? nc.length > 0 : !isObj(nc)) p.add(`${sa}.nodeCoverage: expected an object of node coverages`);
    else if (isObj(nc)) for (const [k, v] of Object.entries(nc)) p.num(v, `${sa}.nodeCoverage.${k}`, SHARE);
  });
}

function checkTradeEdges(p: Problems, trade: unknown, at: string): void {
  if (!p.arr(trade, at)) return;
  trade.forEach((e, i) => {
    const ea = `${at}[${i}]`;
    if (!isObj(e)) return p.add(`${ea}: not an object`);
    for (const k of ['from', 'to', 'node']) p.str(e[k], `${ea}.${k}`);
    p.num(e.usd, `${ea}.usd`, { nullable: true });
    p.num(e.share, `${ea}.share`, { nullable: true });
    p.oneOf(e.relationship, AIB_RELATIONSHIPS, `${ea}.relationship`);
    p.num(e.opportunity, `${ea}.opportunity`, { nullable: true });
  });
}

function checkInvestmentEdges(p: Problems, inv: unknown, at: string): void {
  if (!p.arr(inv, at)) return;
  inv.forEach((e, i) => {
    const ea = `${at}[${i}]`;
    if (!isObj(e)) return p.add(`${ea}: not an object`);
    for (const k of ['investor', 'host', 'node']) p.str(e[k], `${ea}.${k}`);
    p.num(e.usd, `${ea}.usd`, { nullable: true });
    p.num(e.share, `${ea}.share`, { nullable: true });
    p.num(e.friendshore, `${ea}.friendshore`, { nullable: true });
  });
}

/** compact.managed (2026-10-19): absent is fine; else nodesRun an integer from 0 to the compact's node count, USD >= 0 or null, shares in 0-1 or null, classMix a string or null. */
function checkCompactManaged(p: Problems, m: unknown, at: string, nNodes: number | null): void {
  if (m === undefined) return;
  if (!isObj(m)) return p.add(`${at}: not an object`);
  p.num(m.nodesRun, `${at}.nodesRun`, { min: 0, int: true, ...(nNodes === null ? {} : { max: nNodes }) });
  for (const k of ['demandUsd', 'lostUsd', 'residualUsd']) p.num(m[k], `${at}.${k}`, { nullable: true, min: 0 });
  for (const k of ['static', 'existing', 'optimized', 'residual']) p.num(m[k], `${at}.${k}`, SHARE);
  p.optStr(m.classMix, `${at}.classMix`);
}

/** compact.baseline (2026-10-19): absent is fine; else the HHI a share or null and the top suppliers [ISO3, share]. */
function checkCompactBaseline(p: Problems, b: unknown, at: string): void {
  if (b === undefined) return;
  if (!isObj(b)) return p.add(`${at}: not an object`);
  p.num(b.hhi, `${at}.hhi`, SHARE);
  checkSupplierTuples(p, b.suppliers, `${at}.suppliers`, 2);
}

function checkCompact(p: Problems, c: unknown, at: string, nodeIds: ReadonlySet<string>, codes: ReadonlySet<string> = new Set()): void {
  if (!isObj(c)) return p.add(`${at}: not an object`);
  for (const k of ['id', 'name']) p.str(c[k], `${at}.${k}`);
  if (c.indices !== undefined && p.strArr(c.indices, `${at}.indices`) && codes.size) {
    for (const x of c.indices) if (!codes.has(x)) p.add(`${at}.indices: '${x}' is not an index of meta.indices`);
  }
  p.oneOf(c.archetype, AIB_ARCHETYPES, `${at}.archetype`);
  p.oneOf(c.type, AIB_COMPACT_TYPES, `${at}.type`);
  for (const k of ['technologies', 'stages', 'anchors', 'suppliers', 'hosts', 'technologyPartners', 'bridges', 'associates', 'policyNeeds']) p.strArr(c[k], `${at}.${k}`);
  for (const k of ['specialtyHosts', 'developmentHosts']) p.optStrArr(c[k], `${at}.${k}`);
  if (p.strArr(c.nodes, `${at}.nodes`)) {
    if (!c.nodes.length) p.add(`${at}.nodes: a compact without nodes`);
    for (const id of c.nodes) if (!nodeIds.has(id)) p.add(`${at}.nodes: '${id}' is not a node of the document`);
  }
  checkCoalition(p, c.coalition, `${at}.coalition`);
  p.optStr(c.nodePolicyNeeds, `${at}.nodePolicyNeeds`);
  p.optStrArr(c.physicalWarnings, `${at}.physicalWarnings`);
  checkRoles(p, c.roles, `${at}.roles`);
  checkMetrics(p, c.metrics, `${at}.metrics`);
  p.optStr(c.classMix, `${at}.classMix`);
  p.optStr(c.secondaryClass, `${at}.secondaryClass`);
  checkPath(p, c.path, `${at}.path`);
  checkTradeEdges(p, c.trade, `${at}.trade`);
  checkInvestmentEdges(p, c.investment, `${at}.investment`);
  checkCompactManaged(p, c.managed, `${at}.managed`, Array.isArray(c.nodes) ? c.nodes.length : null);
  checkCompactBaseline(p, c.baseline, `${at}.baseline`);
}

/** Problems with a compacts.<h8>.json document (empty when it is valid). */
export function checkCompactsDoc(x: unknown): string[] {
  if (!isObj(x)) return ['document: not an object'];
  const p = new Problems();
  if (x.schema !== AIB_SCHEMA) p.add(`document.schema: expected '${AIB_SCHEMA}'`);
  if (!isNum(x.version)) p.add('document.version: not a number');
  else if (Math.floor(x.version) > AIB_VERSION) p.add(`document: version ${x.version} needs a newer app`);
  checkMeta(p, x.meta);
  const classes = isObj(x.meta) && Array.isArray(x.meta.classes) ? (x.meta.classes as unknown[]).filter(isStr) : [...AIB_CLASSES];
  const codes = indexCodesOf(x.meta);
  // countries: an object of names ([] when empty, as jsonlite writes an empty named list)
  if (Array.isArray(x.countries) ? x.countries.length > 0 : !isObj(x.countries)) p.add('countries: expected an object of ISO3 -> name');
  else if (isObj(x.countries)) for (const [k, v] of Object.entries(x.countries)) p.str(v, `countries.${k}`);
  const nodeIds = new Set<string>();
  if (p.arr(x.nodes, 'nodes')) {
    x.nodes.forEach((n, i) => {
      checkNode(p, n, `nodes[${i}]`, classes, codes);
      if (isObj(n) && isStr(n.id)) {
        if (nodeIds.has(n.id)) p.add(`nodes[${i}].id: duplicate '${n.id}'`);
        nodeIds.add(n.id);
      }
    });
  }
  const compactIds = new Set<string>();
  if (p.arr(x.compacts, 'compacts')) {
    x.compacts.forEach((c, i) => {
      checkCompact(p, c, `compacts[${i}]`, nodeIds, codes);
      if (isObj(c) && isStr(c.id)) {
        if (compactIds.has(c.id)) p.add(`compacts[${i}].id: duplicate '${c.id}'`);
        else if ((AIB_RESERVED as readonly string[]).includes(c.id.toLowerCase())) p.add(`compacts[${i}].id: '${c.id}' is a reserved path segment`);
        compactIds.add(c.id);
      }
    });
  }
  // a node's compact must be one of the document's compacts (when there is a list to check against)
  if (Array.isArray(x.nodes) && Array.isArray(x.compacts)) {
    x.nodes.forEach((n, i) => {
      if (isObj(n) && isStr(n.compact) && !compactIds.has(n.compact)) p.add(`nodes[${i}].compact: '${n.compact}' is not a compact of the document`);
    });
  }
  checkNetwork(p, x.network, nodeIds);
  checkInvestmentFlows(p, x.investmentFlows, nodeIds);
  return p.list;
}

// The additive sections (2026-10-17). The R mirror is R/dashboard/aib/aib_check.R (same paths, same bounds).

function checkVariant(p: Problems, v: unknown, at: string): void {
  if (!isObj(v)) return p.add(`${at}: not an object`);
  for (const k of ['shock', 'scenario']) p.str(v[k], `${at}.${k}`);
  for (const k of ['shockSemantics', 'supplyConstraint', 'strategicScope', 'demandScope', 'candidateUniverse', 'universe']) p.str(v[k], `${at}.${k}`, true);
}

function checkSide(p: Problems, s: unknown, at: string, trade: boolean): void {
  if (!isObj(s)) return p.add(`${at}: not an object`);
  for (const k of ['partners', 'selected', 'upstream', 'midstream', 'downstream']) p.num(s[k], `${at}.${k}`, { min: 0, int: true });
  for (const k of ['headline', 'cumulative']) p.num(s[k], `${at}.${k}`, { min: 0 });
  if (trade) for (const k of ['dividend', 'volumeUsd']) p.num(s[k], `${at}.${k}`, { min: 0 });
}

/** `network`: absent is fine; else the variant, unique economies with four sides, and edges over the document's nodes. */
function checkNetwork(p: Problems, n: unknown, nodeIds: ReadonlySet<string> | null): void {
  if (n === undefined) return;
  if (!isObj(n)) return p.add('network: not an object');
  checkVariant(p, n.variant, 'network.variant');
  const seen = new Set<string>();
  if (p.arr(n.countries, 'network.countries')) {
    n.countries.forEach((r, i) => {
      const at = `network.countries[${i}]`;
      if (!isObj(r)) return p.add(`${at}: not an object`);
      p.str(r.iso3, `${at}.iso3`);
      if (isStr(r.iso3)) {
        if (seen.has(r.iso3)) p.add(`${at}.iso3: duplicate '${r.iso3}'`);
        seen.add(r.iso3);
      }
      if (p.strArr(r.roles, `${at}.roles`)) r.roles.forEach((x) => p.oneOf(x, AIB_NETWORK_ROLES, `${at}.roles`));
      p.strArr(r.technologies, `${at}.technologies`);
      p.num(r.lostUsd, `${at}.lostUsd`, { nullable: true, min: 0 });
      p.num(r.exposedNodes, `${at}.exposedNodes`, { nullable: true, min: 0, int: true });
      checkSide(p, r.supplier, `${at}.supplier`, true);
      checkSide(p, r.buyer, `${at}.buyer`, true);
      checkSide(p, r.host, `${at}.host`, false);
      checkSide(p, r.investor, `${at}.investor`, false);
    });
  }
  if (p.arr(n.edges, 'network.edges')) {
    n.edges.forEach((e, i) => {
      const at = `network.edges[${i}]`;
      if (!isObj(e)) return p.add(`${at}: not an object`);
      for (const k of ['from', 'to']) p.str(e[k], `${at}.${k}`);
      if (isStr(e.from) && e.from === e.to) p.add(`${at}: supplier and buyer are the same economy`);
      for (const k of ['nTrade', 'nInvestment', 'nShocks', 'nHighSeverity']) p.num(e[k], `${at}.${k}`, { min: 0, int: true });
      if (p.strArr(e.nodes, `${at}.nodes`) && nodeIds) for (const id of e.nodes) if (!nodeIds.has(id)) p.add(`${at}.nodes: '${id}' is not a node of the document`);
      p.strArr(e.techs, `${at}.techs`);
      if (p.strArr(e.stages, `${at}.stages`)) e.stages.forEach((s) => p.oneOf(s, AIB_STAGES, `${at}.stages`));
      for (const k of ['tradeUsd', 'trade', 'investment', 'cumTrade', 'cumDividend', 'cumInvestment']) p.num(e[k], `${at}.${k}`, { min: 0 });
      for (const k of ['opportunity', 'friendshore']) p.num(e[k], `${at}.${k}`, SHARE);
    });
  }
}

/** `investmentFlows`: absent is fine; else the variant, pairs (investor != host, rank 1..maxRank) and ranked hosts over the document's nodes. */
function checkInvestmentFlows(p: Problems, f: unknown, nodeIds: ReadonlySet<string> | null): void {
  if (f === undefined) return;
  if (!isObj(f)) return p.add('investmentFlows: not an object');
  checkVariant(p, f.variant, 'investmentFlows.variant');
  const v = isObj(f.variant) ? f.variant : {};
  p.num(v.increment, 'investmentFlows.variant.increment', { min: 0, max: 1 });
  p.num(v.maxRank, 'investmentFlows.variant.maxRank', { min: 1, int: true });
  const maxRank = isNum(v.maxRank) ? v.maxRank : Infinity;
  if (p.arr(f.pairs, 'investmentFlows.pairs')) {
    f.pairs.forEach((e, i) => {
      const at = `investmentFlows.pairs[${i}]`;
      if (!isObj(e)) return p.add(`${at}: not an object`);
      for (const k of ['investor', 'host', 'node']) p.str(e[k], `${at}.${k}`);
      if (isStr(e.investor) && e.investor === e.host) p.add(`${at}: investor and host are the same economy`);
      if (isStr(e.node) && nodeIds && !nodeIds.has(e.node)) p.add(`${at}.node: '${e.node}' is not a node of the document`);
      p.num(e.rank, `${at}.rank`, { min: 1, max: maxRank, int: true });
      p.num(e.usd, `${at}.usd`, { min: 0 });
      for (const k of ['share', 'lostShare', 'friendshore', 'opportunity']) p.num(e[k], `${at}.${k}`, SHARE);
    });
  }
  if (p.arr(f.hosts, 'investmentFlows.hosts')) {
    f.hosts.forEach((h, i) => {
      const at = `investmentFlows.hosts[${i}]`;
      if (!isObj(h)) return p.add(`${at}: not an object`);
      for (const k of ['node', 'host']) p.str(h[k], `${at}.${k}`);
      if (isStr(h.node) && nodeIds && !nodeIds.has(h.node)) p.add(`${at}.node: '${h.node}' is not a node of the document`);
      p.num(h.rank, `${at}.rank`, { min: 1, max: maxRank, int: true });
      p.num(h.gain, `${at}.gain`, { nullable: true, min: -1, max: 1 });
      for (const k of ['before', 'after', 'newCapacityUsed']) p.num(h[k], `${at}.${k}`, SHARE);
      p.num(h.servedUsd, `${at}.servedUsd`, { nullable: true, min: 0 });
      p.num(h.capabilityTier, `${at}.capabilityTier`, { nullable: true, min: 0, int: true });
      p.str(h.capabilityBasis, `${at}.capabilityBasis`, true);
      p.optNum(h.hostIncrement, `${at}.hostIncrement`, { min: 0, max: 1 });
      p.optStr(h.rankBasis, `${at}.rankBasis`);
      p.optStr(h.type, `${at}.type`);
    });
  }
}

// ---- Scenario files (2026-10-19) ---------------------------------------------------------------------

/** What a scenario file is cross-checked against: the main document's nodes and compacts (by id). */
export interface ScenarioCheckRefs {
  nodes: readonly { id: string }[];
  compacts: readonly { id: string }[];
}

/** One compact of scenario-managed (tolerant: the arrays and metrics may be absent; normalizeScenarioDoc() fills them). */
function checkScenarioCompact(p: Problems, c: unknown, at: string): void {
  if (!isObj(c)) return p.add(`${at}: not an object`);
  p.str(c.id, `${at}.id`);
  p.num(c.nodesRun, `${at}.nodesRun`, { min: 0, int: true });
  if (c.coalition !== null) checkCoalition(p, c.coalition, `${at}.coalition`);
  p.optStr(c.coalitionReason, `${at}.coalitionReason`);
  if (c.coalition === null && !isStr(c.coalitionReason)) p.add(`${at}.coalitionReason: expected a string when coalition is null`);
  p.optOneOf(c.type, AIB_COMPACT_TYPES, `${at}.type`);
  p.optOneOf(c.archetype, AIB_ARCHETYPES, `${at}.archetype`);
  p.optStr(c.secondaryClass, `${at}.secondaryClass`);
  for (const k of ['anchors', 'suppliers', 'hosts', 'specialtyHosts', 'developmentHosts', 'technologyPartners', 'bridges', 'associates', 'policyNeeds']) p.optStrArr(c[k], `${at}.${k}`);
  if (c.roles !== undefined) checkRoles(p, c.roles, `${at}.roles`);
  if (c.metrics !== undefined && c.metrics !== null) checkMetrics(p, c.metrics, `${at}.metrics`);
  if (c.path !== undefined) checkPath(p, c.path, `${at}.path`);
  if (c.trade !== undefined) checkTradeEdges(p, c.trade, `${at}.trade`);
  if (c.investment !== undefined) checkInvestmentEdges(p, c.investment, `${at}.investment`);
}

/** An array of {id, suppliers: [ISO3, USD, share][]} (scenario-baseline nodes / compacts), ids unique and, with `known`, of the main document. */
function checkSupplierLists(p: Problems, x: unknown, at: string, known: ReadonlySet<string> | null, what: string): void {
  if (!p.arr(x, at)) return;
  const seen = new Set<string>();
  x.forEach((r, i) => {
    const ra = `${at}[${i}]`;
    if (!isObj(r)) return p.add(`${ra}: not an object`);
    p.str(r.id, `${ra}.id`);
    if (isStr(r.id)) {
      if (seen.has(r.id)) p.add(`${ra}.id: duplicate '${r.id}'`);
      seen.add(r.id);
      if (known && !known.has(r.id)) p.add(`${ra}.id: '${r.id}' is not a ${what} of the main document`);
    }
    checkSupplierTuples(p, r.suppliers, `${ra}.suppliers`, 3);
  });
}

/**
 * Problems with a scenario-<id>.<h8>.json file (empty when it is valid). `refs` (the main document) adds
 * the cross-checks: compact and node ids must be the main document's. Tolerant like the main checks: [] for
 * an empty named list, null numbers; network and investmentFlows may be absent; a managed compact's arrays,
 * metrics and coalitionReason may be absent (normalizeScenarioDoc() fills them), and its coalition may be
 * null (then coalitionReason says why). `expect`: the scenario the manifest names the file for.
 */
export function checkScenarioDoc(x: unknown, refs: ScenarioCheckRefs | null = null, expect: AibScenarioFileId | null = null): string[] {
  if (!isObj(x)) return ['document: not an object'];
  const p = new Problems();
  if (x.schema !== AIB_SCENARIO_SCHEMA) p.add(`document.schema: expected '${AIB_SCENARIO_SCHEMA}'`);
  if (!isNum(x.version)) p.add('document.version: not a number');
  else if (Math.floor(x.version) > AIB_SCENARIO_VERSION) p.add(`document: version ${x.version} needs a newer app`);
  p.oneOf(x.scenario, AIB_SCENARIO_FILE_IDS, 'document.scenario');
  if (expect && isStr(x.scenario) && x.scenario !== expect) p.add(`document.scenario: '${x.scenario}' where the manifest names a ${expect} file`);
  if (x.scenario === 'managed') p.str(x.shockId, 'document.shockId');
  else p.str(x.shockId, 'document.shockId', true);
  p.str(x.created, 'document.created');
  if (Array.isArray(x.countries) ? x.countries.length > 0 : !isObj(x.countries)) p.add('countries: expected an object of ISO3 -> name');
  else if (isObj(x.countries)) for (const [k, v] of Object.entries(x.countries)) p.str(v, `countries.${k}`);
  const nodeIds = refs ? new Set(refs.nodes.map((n) => n.id)) : null;
  const compactIds = refs ? new Set(refs.compacts.map((c) => c.id)) : null;
  if (x.scenario === 'managed') {
    if (p.arr(x.compacts, 'compacts')) {
      const seen = new Set<string>();
      x.compacts.forEach((c, i) => {
        checkScenarioCompact(p, c, `compacts[${i}]`);
        if (isObj(c) && isStr(c.id)) {
          if (seen.has(c.id)) p.add(`compacts[${i}].id: duplicate '${c.id}'`);
          seen.add(c.id);
          if (compactIds && !compactIds.has(c.id)) p.add(`compacts[${i}].id: '${c.id}' is not a compact of the main document`);
        }
      });
    }
    checkInvestmentFlows(p, x.investmentFlows, nodeIds);
  } else if (x.scenario === 'baseline') {
    checkSupplierLists(p, x.nodes, 'nodes', nodeIds, 'node');
    checkSupplierLists(p, x.compacts, 'compacts', compactIds, 'compact');
  }
  checkNetwork(p, x.network, nodeIds);
  return p.list;
}

/**
 * A checked scenario file with its tolerated gaps filled (a copy): a managed compact's absent arrays as [],
 * absent metrics and coalitionReason as null; countries [] as {}.
 */
export function normalizeScenarioDoc<D extends AibScenarioDoc>(d: D): D {
  const countries = Array.isArray(d.countries) ? {} : d.countries;
  if (d.scenario !== 'managed') return { ...d, countries };
  const arr = <T,>(v: T[] | undefined): T[] => (Array.isArray(v) ? v : []);
  const compacts = d.compacts.map((c): AibScenarioCompact => {
    const x = c as Partial<AibScenarioCompact> & { id: string; nodesRun: number };
    return {
      ...x,
      coalition: x.coalition ?? null,
      coalitionReason: x.coalitionReason ?? null,
      type: x.type ?? null,
      archetype: x.archetype ?? null,
      policyNeeds: arr(x.policyNeeds),
      secondaryClass: x.secondaryClass ?? null,
      anchors: arr(x.anchors),
      suppliers: arr(x.suppliers),
      hosts: arr(x.hosts),
      specialtyHosts: arr(x.specialtyHosts),
      developmentHosts: arr(x.developmentHosts),
      technologyPartners: arr(x.technologyPartners),
      bridges: arr(x.bridges),
      associates: arr(x.associates),
      roles: arr(x.roles),
      metrics: x.metrics ?? null,
      path: arr(x.path),
      trade: arr(x.trade),
      investment: arr(x.investment),
    };
  });
  return { ...d, countries, compacts };
}

/** The scenario files a manifest names (an entry only for a valid file name of that scenario). */
export function scenarioFilesOf(m: Pick<AibManifest, 'files'>): Partial<Record<AibScenarioFileId, string>> {
  const s = (m.files as { scenarios?: unknown }).scenarios;
  const out: Partial<Record<AibScenarioFileId, string>> = {};
  if (!isObj(s)) return out;
  for (const id of AIB_SCENARIO_FILE_IDS) {
    const f = s[id];
    const g = isStr(f) ? AIB_SCENARIO_FILE_RE.exec(f) : null;
    if (isStr(f) && g && g[1] === id) out[id] = f;
  }
  return out;
}

/** The supply scenarios of a bundle: meta.scenarios when it lists names; else meta.supplyScenarios; else the headline scenario alone. */
export function supplyScenariosOf(m: Pick<AibMeta, 'scenarios' | 'supplyScenarios' | 'headlineScenario'>): string[] {
  const s = m.scenarios as unknown;
  if (Array.isArray(s) && s.every(isStr)) return s;
  if (Array.isArray(m.supplyScenarios) && m.supplyScenarios.every(isStr)) return m.supplyScenarios;
  return [m.headlineScenario];
}

/** The scenario list of meta.scenarios ([] when it lists the supply scenarios: an older bundle), in AIB_SCENARIO_IDS order. */
export function scenarioDefsOf(m: Pick<AibMeta, 'scenarios'>): AibScenarioDef[] {
  const s = m.scenarios as unknown;
  if (!Array.isArray(s)) return [];
  const defs = s.filter((x): x is AibScenarioDef => isObj(x) && (AIB_SCENARIO_IDS as readonly unknown[]).includes(x.id) && isStr(x.label));
  return AIB_SCENARIO_IDS.flatMap((id) => defs.filter((d) => d.id === id).slice(0, 1));
}

/** A scenario file's country names as an object (the exporter writes [] for an empty one). */
export const scenarioNamesOf = (d: Pick<AibScenarioDoc, 'countries'>): Record<string, string> => (Array.isArray(d.countries) ? {} : d.countries);

// ---- Accessors ---------------------------------------------------------------------------------------

/** A path step's node coverages as an object (the exporter writes [] for an empty one). */
export const nodeCoverageOf = (s: AibPathStep): Record<string, number> => (Array.isArray(s.nodeCoverage) ? {} : s.nodeCoverage);

/** The country names as an object (the exporter writes [] for an empty one). */
export const countryNamesOf = (doc: AibDoc): Record<string, string> => (Array.isArray(doc.countries) ? {} : doc.countries);

/** A node's classes under the other variants as an object ({} when the export has none, or wrote []). */
export const variantsOf = (n: AibNode): Record<string, string | null> => (!n.variants || Array.isArray(n.variants) ? {} : n.variants);

/** A node's variant warnings as an object ({} when the export wrote []); null when the export predates them. */
export const variantWarningsOf = (n: AibNode): Record<string, boolean> | null =>
  n.variantWarnings === undefined ? null : Array.isArray(n.variantWarnings) ? {} : n.variantWarnings;

/**
 * The compact's coalition as the pages show it: coalition.members (the exact minimum at the middle target,
 * or the widest greedy set when that target is out of reach) when the export lists it; else (an earlier
 * export wrote members [] for an unreached target and kept the widest set as the roles with member = true)
 * those economies, alphabetical.
 */
export function coalitionMembersOf(c: AibCompact): string[] {
  if (c.coalition.members.length) return c.coalition.members;
  return [...new Set(c.roles.filter((r) => r.member).map((r) => r.iso3))].sort();
}

/** In the strategic scope (an older bundle without the field: every node is). */
export const inStrategicScope = (n: AibNode): boolean => n.inStrategicScope !== false;

/** The nodes come from the electro-industrial dictionary (any other value, or none, reads as the OSI basket). */
export const isDictionary = (m: Pick<AibMeta, 'taxonomy'>): boolean => m.taxonomy === 'dictionary';

/** The EIS indices of the bundle ([] for master or an older bundle). */
export const indicesOf = (m: Pick<AibMeta, 'indices'>): AibIndex[] => (Array.isArray(m.indices) ? m.indices : []);

/** The diversification scenario of the bundle (meta.diversify); null in an older bundle. */
export const diversifyMetaOf = (m: Pick<AibMeta, 'diversify'>): AibDiversifyMeta | null => (isObj(m.diversify) ? m.diversify : null);

/** The shock id config/aib.yml gives the diversification scenario (when meta.diversify does not name one). */
export const AIB_DIVERSIFY_SHOCK = 'diversify_benchmark';

/** The diversification scenario's id in meta.shocks and node.variants (meta.diversify.shockId, else AIB_DIVERSIFY_SHOCK). */
export const diversifyShockIdOf = (m: Pick<AibMeta, 'diversify'>): string => diversifyMetaOf(m)?.shockId ?? AIB_DIVERSIFY_SHOCK;
