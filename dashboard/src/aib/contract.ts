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
//                     criticalExternal, criticalExternalMarginal, external, associatesReach99;
//                     nodePolicyNeeds; physicalWarnings; and per role its coalition tier (AIB_TIERS, six),
//                     critical and leave-one-out (looCritical) flags, its SINGLE substitutes and
//                     substitutesNeeded. Externals hold roles with member = false, like associates.
//   node (later)      variantWarnings (variant -> the class under it carries the physical-capability
//                     warning) and growth[].warning: the warning of every class a node shows.

// ---- Schema ----------------------------------------------------------------------------------------

export const AIB_SCHEMA = 'osi-aib';
export const AIB_VERSION = 1;
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

/** Shock semantics: hold (an export shock; the shocked economy keeps buying) or release (decoupling). */
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
 * The coalition tiers of a compact's economies, from the most to the least involved:
 *   core coordinating member                 a member of the exact coalition that is not a critical supply
 *                                            partner (the demand anchors, and suppliers one other candidate
 *                                            could replace)
 *   critical supply partner                  a needed member no single other candidate can replace
 *   associate partner                        the fewest additions to the core (held in) that reach the
 *                                            highest target
 *   critical external dependency             a non-member whose loss pushes a node below the middle target
 *                                            with the coalition fixed, supplying at least
 *                                            critical_min_supply_share (1%) of the anchors' demand there
 *   critical external dependency (marginal)  the same, under that share: critical only because the
 *                                            coalition sits near the target
 *   external dependency                      another non-member supplying a material share (5%)
 */
export const AIB_TIERS = [
  'core coordinating member',
  'critical supply partner',
  'associate partner',
  'critical external dependency',
  'critical external dependency (marginal)',
  'external dependency',
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
  files: { compacts: string };
}

// ---- Document ----------------------------------------------------------------------------------------

export interface AibShock {
  id: string;
  label: string;
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
  scopeTechs?: string[];
  tradeYear: number;
  built: string;
  shocks: AibShock[];
  scenarios: string[];
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
   * leave-one-out with the coalition fixed: losing its supply pushes a node below the middle target (true
   * for nearly every non-anchor member by construction of a minimum coalition; additive)
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
  name: string;
  technologies: string[];
  stages: string[];
  archetype: AibArchetype;
  type: AibCompactType;
  /** node ids */
  nodes: string[];
  coalition: AibCoalition;
  anchors: string[];
  suppliers: string[];
  hosts: string[];
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
  techs: string[];
  stage: string;
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
  hosts: string[];
  bestGain: number | null;
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
  /** mean Export Opportunity (supplier -> buyer) and Friendshore (buyer -> supplier) scores, NEIS baseline */
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
 * that serves the investor's lost demand. Each rank is a SEPARATE simulation of the same increment: only
 * rank 1 is the selected host; flows of different ranks are alternatives and must not be added up.
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
  /** the investor's Friendshore score for the host, NEIS baseline */
  friendshore: number | null;
  /** the host's Export Opportunity score for the investor, NEIS baseline */
  opportunity: number | null;
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
}

export interface AibInvestmentFlows {
  /** with the simulated increment (share of node demand) and the deepest host rank kept */
  variant: AibVariant & { increment: number; maxRank: number };
  pairs: AibInvestmentPair[];
  hosts: AibInvestmentHost[];
}

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
  else if (!isStr(x.files.compacts) || !AIB_DOC_FILE_RE.test(x.files.compacts)) p.add('manifest.files.compacts: not a compacts.<8 hex>.json file name');
  return p.list;
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
  p.strArr(m.scenarios, 'meta.scenarios');
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
}

/** A class written beside the headline one (a variant, a growth bound): null, or one of meta.classes. */
function checkCls(p: Problems, x: unknown, path: string, classes: readonly string[]): void {
  if (x === null) return;
  if (!isStr(x)) p.add(`${path}: expected a string or null`);
  else if (!classes.includes(x)) p.add(`${path}: '${x}' is not one of meta.classes`);
}

function checkNode(p: Problems, n: unknown, at: string, classes: readonly string[]): void {
  if (!isObj(n)) return p.add(`${at}: not an object`);
  for (const k of ['id', 'label', 'stage']) p.str(n[k], `${at}.${k}`);
  if (!isStr(n.cls)) p.add(`${at}.cls: expected a string`);
  else if (!classes.includes(n.cls)) p.add(`${at}.cls: '${n.cls}' is not one of meta.classes`);
  for (const k of ['reason', 'quality', 'baseLargest', 'largest', 'partners', 'treatment', 'compact']) p.str(n[k], `${at}.${k}`, true);
  for (const k of ['techs', 'flags', 'hosts', 'yfCells', 'yfSubcomponents']) p.strArr(n[k], `${at}.${k}`);
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

function checkCompact(p: Problems, c: unknown, at: string, nodeIds: ReadonlySet<string>): void {
  if (!isObj(c)) return p.add(`${at}: not an object`);
  for (const k of ['id', 'name']) p.str(c[k], `${at}.${k}`);
  p.oneOf(c.archetype, AIB_ARCHETYPES, `${at}.archetype`);
  p.oneOf(c.type, AIB_COMPACT_TYPES, `${at}.type`);
  for (const k of ['technologies', 'stages', 'anchors', 'suppliers', 'hosts', 'technologyPartners', 'bridges', 'associates', 'policyNeeds']) p.strArr(c[k], `${at}.${k}`);
  if (p.strArr(c.nodes, `${at}.nodes`)) {
    if (!c.nodes.length) p.add(`${at}.nodes: a compact without nodes`);
    for (const id of c.nodes) if (!nodeIds.has(id)) p.add(`${at}.nodes: '${id}' is not a node of the document`);
  }
  const co = c.coalition;
  if (!isObj(co)) p.add(`${at}.coalition: not an object`);
  else {
    for (const k of ['members', 't90', 't95', 't99']) p.strArr(co[k], `${at}.coalition.${k}`);
    p.bool(co.reached99, `${at}.coalition.reached99`);
    p.num(co.maxCoverage, `${at}.coalition.maxCoverage`, SHARE);
    p.optStr(co.method, `${at}.coalition.method`);
    for (const k of ['greedy', 'criticalPartners', 'criticalExternal', 'criticalExternalMarginal', 'external']) p.optStrArr(co[k], `${at}.coalition.${k}`);
    for (const k of ['associatesReach99', 'robustTieBreakSolved']) p.optBool(co[k], `${at}.coalition.${k}`);
  }
  p.optStr(c.nodePolicyNeeds, `${at}.nodePolicyNeeds`);
  p.optStrArr(c.physicalWarnings, `${at}.physicalWarnings`);
  if (p.arr(c.roles, `${at}.roles`)) {
    c.roles.forEach((r, i) => {
      const ra = `${at}.roles[${i}]`;
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
  const m = c.metrics;
  if (!isObj(m)) p.add(`${at}.metrics: not an object`);
  else {
    for (const k of ['demandUsd', 'lostUsd']) p.num(m[k], `${at}.metrics.${k}`, { nullable: true, min: 0 });
    for (const k of ['exposure', 'static', 'existing', 'trade', 'investment', 'residual', 'residualAfterInvestment']) p.num(m[k], `${at}.metrics.${k}`, SHARE);
    p.num(m.dividend, `${at}.metrics.dividend`, { nullable: true, min: -1, max: 1 });
    for (const k of ['globalOptimum', 'anchorFragmented', 'anchorCompact']) p.optNum(m[k], `${at}.metrics.${k}`, { min: 0, max: 1 });
    p.optNum(m.compactGain, `${at}.metrics.compactGain`, { min: -1, max: 1 });
  }
  p.optStr(c.classMix, `${at}.classMix`);
  p.optStr(c.secondaryClass, `${at}.secondaryClass`);
  if (p.arr(c.path, `${at}.path`)) {
    c.path.forEach((s, i) => {
      const sa = `${at}.path[${i}]`;
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
  if (p.arr(c.trade, `${at}.trade`)) {
    c.trade.forEach((e, i) => {
      const ea = `${at}.trade[${i}]`;
      if (!isObj(e)) return p.add(`${ea}: not an object`);
      for (const k of ['from', 'to', 'node']) p.str(e[k], `${ea}.${k}`);
      p.num(e.usd, `${ea}.usd`, { nullable: true });
      p.num(e.share, `${ea}.share`, { nullable: true });
      p.oneOf(e.relationship, AIB_RELATIONSHIPS, `${ea}.relationship`);
      p.num(e.opportunity, `${ea}.opportunity`, { nullable: true });
    });
  }
  if (p.arr(c.investment, `${at}.investment`)) {
    c.investment.forEach((e, i) => {
      const ea = `${at}.investment[${i}]`;
      if (!isObj(e)) return p.add(`${ea}: not an object`);
      for (const k of ['investor', 'host', 'node']) p.str(e[k], `${ea}.${k}`);
      p.num(e.usd, `${ea}.usd`, { nullable: true });
      p.num(e.share, `${ea}.share`, { nullable: true });
      p.num(e.friendshore, `${ea}.friendshore`, { nullable: true });
    });
  }
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
  // countries: an object of names ([] when empty, as jsonlite writes an empty named list)
  if (Array.isArray(x.countries) ? x.countries.length > 0 : !isObj(x.countries)) p.add('countries: expected an object of ISO3 -> name');
  else if (isObj(x.countries)) for (const [k, v] of Object.entries(x.countries)) p.str(v, `countries.${k}`);
  const nodeIds = new Set<string>();
  if (p.arr(x.nodes, 'nodes')) {
    x.nodes.forEach((n, i) => {
      checkNode(p, n, `nodes[${i}]`, classes);
      if (isObj(n) && isStr(n.id)) {
        if (nodeIds.has(n.id)) p.add(`nodes[${i}].id: duplicate '${n.id}'`);
        nodeIds.add(n.id);
      }
    });
  }
  const compactIds = new Set<string>();
  if (p.arr(x.compacts, 'compacts')) {
    x.compacts.forEach((c, i) => {
      checkCompact(p, c, `compacts[${i}]`, nodeIds);
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
function checkNetwork(p: Problems, n: unknown, nodeIds: ReadonlySet<string>): void {
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
      if (p.strArr(e.nodes, `${at}.nodes`)) for (const id of e.nodes) if (!nodeIds.has(id)) p.add(`${at}.nodes: '${id}' is not a node of the document`);
      p.strArr(e.techs, `${at}.techs`);
      if (p.strArr(e.stages, `${at}.stages`)) e.stages.forEach((s) => p.oneOf(s, AIB_STAGES, `${at}.stages`));
      for (const k of ['tradeUsd', 'trade', 'investment', 'cumTrade', 'cumDividend', 'cumInvestment']) p.num(e[k], `${at}.${k}`, { min: 0 });
      for (const k of ['opportunity', 'friendshore']) p.num(e[k], `${at}.${k}`, SHARE);
    });
  }
}

/** `investmentFlows`: absent is fine; else the variant, pairs (investor != host, rank 1..maxRank) and ranked hosts over the document's nodes. */
function checkInvestmentFlows(p: Problems, f: unknown, nodeIds: ReadonlySet<string>): void {
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
      if (isStr(e.node) && !nodeIds.has(e.node)) p.add(`${at}.node: '${e.node}' is not a node of the document`);
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
      if (isStr(h.node) && !nodeIds.has(h.node)) p.add(`${at}.node: '${h.node}' is not a node of the document`);
      p.num(h.rank, `${at}.rank`, { min: 1, max: maxRank, int: true });
      p.num(h.gain, `${at}.gain`, { nullable: true, min: -1, max: 1 });
      for (const k of ['before', 'after', 'newCapacityUsed']) p.num(h[k], `${at}.${k}`, SHARE);
      p.num(h.servedUsd, `${at}.servedUsd`, { nullable: true, min: 0 });
      p.num(h.capabilityTier, `${at}.capabilityTier`, { nullable: true, min: 0, int: true });
      p.str(h.capabilityBasis, `${at}.capabilityBasis`, true);
    });
  }
}

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
