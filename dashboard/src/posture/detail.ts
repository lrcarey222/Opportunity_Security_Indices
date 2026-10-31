// One node's detail, normalised (design section 3, D11, D12, D14): what the drawer's five questions and the
// Country sections read beyond the core. The detail document (posture-detail-ISO3, scripts/91) writes one
// object per technology x six-stage node; nodeDetail() reads it into one typed shape, accepting the field
// spellings the exporter may use: flat families (dn_demand: 0.8 with dn_demand_basis / _scenario) or nested
// ones (dn_demand: {value, basis, scenario, note}); the R table names of the supply-chain families
// (sn_import_dependence, sn_supplier_concentration, sn_risky_supplier_exposure) or the short ones
// (import_dependence, import_hhi, china_bloc); a policy object or the v1 flat alignment fields; pathway_basis
// and watch_reasons as a list or a ";"-separated string. A value the document does not carry is null: missing
// evidence is shown as "not assessed", never as zero. Pure: no React, no IO.
// Yards & Fences (2026-10-30, R posture_export.R; each field optional, an older document reads without them):
// prize {value, measure, market, ...} (the value the choice read, its measure and the market prize; a flat
// industrial_opportunity is both the value and the market prize), the economy's own large-yard case where it
// differs from the node's (large_yard {case, n, s, u, floor, reason}; s and u index meta.large_yard_signals),
// the fence scope of its Secure layers (yard_fence_scope) and its child layers (yard_children [{layer_id, yard,
// choice, variant}]; their names and evidence are the core's meta.yf_nodes, resolved by yards.ts).

import { DEPLOYMENT_STAGE } from '../lib/stages.ts';
import type {
  Alignment, AlignmentConfidence, CompeteMode, DeployMode, NeedType, Pathway, PostureStage, PostureWord, SecurityRationale, Verb, Yard,
} from './contract.ts';

/** One measured family or component of a score: a 0-1 value (null = not assessed), where it comes from and its scenario. */
export interface Family {
  value: number | null;
  /** national | regional_proxy | observed_trend | global_context | not_assessed, or the exporter's word */
  basis: string | null;
  /** the scenario id (current_policy, national_commitments, climate_aligned) */
  scenario: string | null;
  note: string | null;
  /**
   * null for a 0-1 score; else the exporter's unit of a value that is not one. The climate-aligned alternative
   * (dn_climate_aligned) is a ratio to current policies ('ratio_vs_current_policies': 1.04 = 104% more deployment
   * in the climate-aligned scenario than under current policies, World 2035), never a score.
   */
  unit: string | null;
}

/** The unit of the climate-aligned alternative: a ratio to current policies, not a 0-1 score. */
export const RATIO_VS_CURRENT_POLICIES = 'ratio_vs_current_policies';

/**
 * A family value that is a ratio to current policies, in words ("+104% vs current policies", "-80% vs current
 * policies"); null for a 0-1 score (the callers show those as scores) or a missing value.
 */
export function ratioVsCurrentWords(f: Pick<Family, 'value' | 'unit'> | null | undefined): string | null {
  if (!f || f.value === null || !Number.isFinite(f.value) || f.unit !== RATIO_VS_CURRENT_POLICIES) return null;
  const p = Math.round(f.value * 100);
  return `${p > 0 ? '+' : ''}${p}% vs current policies`;
}

export interface SupplyChainNeed {
  /** the scored families (0-1): import dependence, supplier concentration (own import HHI), risky supplier exposure (China / opposite bloc) */
  importDependence: number | null;
  supplierConcentration: number | null;
  riskyExposure: number | null;
  /** trusted suppliers other than home (a scored family where observed widely, else a subcomponent; design D4) */
  trustedAlternatives: number | null;
  topSupplier: string | null;
  topSupplierShare: number | null;
  chinaShare: number | null;
  oppositeBlocShare: number | null;
  importsToGdp: number | null;
  trustedShare: number | null;
  coerciveTop: string | null;
  coerciveTopShare: number | null;
  coerciveImportShare: number | null;
  /** Secure-engine node attributes, in words */
  substitutability: string | null;
  reconstitution: string | null;
  inventories: string | null;
  essentiality: string | null;
  operationalControl: string | null;
}

export interface DeploymentNeed {
  /** A demand adequacy (current policy), B target / climate gap (national commitments), C system need */
  demand: Family;
  policyGap: Family;
  system: Family;
  /** a climate-aligned value, shown only as an explicitly labelled alternative (design D12); never used in the choice */
  alternative: Family | null;
  /** the current deployment trend, in words or as a growth rate */
  trend: string | number | null;
}

export interface Prize {
  /** the prize the choice read, 0-1: the Strategic Industrial Value (measure strategic_value), else the market prize */
  value: number | null;
  /** prize.measure: strategic_value or market (null: not written, an older document's market prize) */
  measure: string | null;
  /** the market prize (industrial_opportunity), 0-1: prize.market, or a flat industrial_opportunity */
  market: number | null;
  nodePrize: number | null;
  investmentMomentum: number | null;
  homeMarket: number | null;
  /** where the home-market growth comes from (national, not_assessed ...) */
  homeMarketBasis: string | null;
  /** technology maturity (TRL), context only */
  maturity: string | number | null;
  basis: string | null;
  outlook: string | null;
  note: string | null;
}

/** One Competitive Position family (0-1). */
export interface CpFamily {
  id: string;
  label: string;
  value: number;
}

/** A step of the compact decision trace (design D11). */
export interface TraceStep {
  step: string;
  test: string;
  value: string | number | null;
  threshold: string | number | null;
  result: string;
}

export interface PolicyEvidence {
  /** revealed policy priority, a within-country percentile: the observed policy intensity */
  priority: number | null;
  /** NIPO support, a within-country percentile rank (not a share of the node's policy attention) */
  support: number | null;
  /** NIPO protection or localisation (at Deployment & Use, local-content protection on the equipment), a within-country percentile rank */
  protection: number | null;
  /**
   * false when the records hold no protection measure for the node (its rank is then a tie among zeros and is
   * shown as "none recorded"); true when they hold some; null when the document does not say (the rank is shown)
   */
  protectionRecorded: boolean | null;
  /**
   * policy momentum where the NIPO time series supports it: a within-country percentile rank of recent policy
   * activity (never a change: it cannot go negative), or words as written; null: not available
   */
  momentum: string | number | null;
  alignment: Alignment | null;
  alignmentConfidence: AlignmentConfidence | null;
  flag: string | null;
  sources: string[];
  nSources: number | null;
}

export interface DetailPartner {
  iso3: string;
  capability?: number;
  cp?: number;
  relationship?: number;
  diversification?: number;
}

export interface TradeEvidence {
  benchmark_source?: string;
  benchmark_share?: number;
  benchmark_rca?: number;
  benchmark_exports?: number;
  momentum_quality?: string;
  momentum_raw?: number;
  momentum_specialization?: number;
  momentum_score?: number;
  months_observed?: number;
  exports_current?: number;
  exports_previous?: number;
}

/**
 * The economy's own large-yard case of a node, written only where it differs from the node's (the core's
 * meta.yf_nodes): s and u are 0-based indexes into meta.large_yard_signals (yards.ts resolves them).
 */
export interface LargeYardException {
  /** true holds, false does not, null undecided */
  case: boolean | null;
  /** how many of the non-required (commodity) signals hold */
  n: number | null;
  s: number[];
  u: number[];
  floor: boolean;
  /** the reason, written only when undecided */
  reason: string | null;
}

/** The fence scope of a node with Secure layers (R yard_rollup.R sp_yard_fence_scope), where it is not the layer alone. */
export type YardFenceScope = 'extends_to_hardware' | 'unclear';

/** One child layer of a node (R yard_children, compact): its own yard, choice and engine variant; its name and evidence are in the core's meta.yf_nodes. */
export interface YardChild {
  layerId: string;
  /** small, medium or large; null when the layer is not assessed */
  yard: Yard | null;
  /** the layer's own choice; "not_assessed" when it has none */
  choice: Verb | 'not_assessed' | null;
  variant: string | null;
}

export interface NodeDetail {
  tech: string;
  stage: PostureStage;
  /** "Tech|Stage" */
  key: string;
  posture: PostureWord;
  variant: string | null;
  mode: CompeteMode | null;
  deployMode: DeployMode | null;
  confidence: string | null;
  possible: string | null;
  pivotal: string | null;
  delivery: string | null;
  rationale: SecurityRationale | null;
  routes: string[];
  /** the recommended mitigation of a Secure node, in words, when the document writes one */
  mitigation: string | null;
  /** why the node is not assessed (need or prize missing), when it is not */
  notAssessedReason: string | null;
  needType: NeedType;
  /** the relevant need, 0-1 */
  need: number | null;
  /** at the producing stages only (null at Deployment & Use) */
  supplyChain: SupplyChainNeed | null;
  /** at Deployment & Use only */
  deployment: DeploymentNeed | null;
  prize: Prize;
  cp: number | null;
  homeShare: number | null;
  cpFamilies: CpFamily[];
  pathway: { credible: Pathway | null; basis: string[] };
  borderline: string[];
  flag: string | null;
  trace: TraceStep[];
  policy: PolicyEvidence;
  partners: DetailPartner[];
  partnerGap: boolean | null;
  /** the economy's own gap reasons for this node (R partner_gap_reasons: no_trusted_supplier, single_credible_supplier, weak_evidence, insufficient_allied_capacity, excessive_concentration) */
  partnerGapReasons: string[];
  /** the economy's AIB status for this node (R partner_aib_status): covered or gap; null when not written */
  aibStatus: 'covered' | 'gap' | null;
  partnerReason: string | null;
  partnerEvidence: string | null;
  partnerSource: string | null;
  trade: TradeEvidence | null;
  /**
   * Why a Watch node is Watch, as the engine decided it (R decision_engine.R sp_watch_reasons, written as
   * watch_reasons: a ";"-separated string or an array of low_need, low_prize, ample_diversified_supply,
   * weak_domestic_case, option_value, low_deployment_need); empty when the document writes none
   */
  watchReasons: string[];
  /** the deciding test's words */
  reason: string | null;
  /** the other choices the evidence allows ("catch_up|watch"), where there are any */
  choicePossible: string | null;
  /** the economy's own large-yard case, only where it differs from the node's (else null: read meta.yf_nodes) */
  largeYard: LargeYardException | null;
  /** where the high fence of a Secure layer reaches beyond the layer (null: the layer alone, or no Secure layer) */
  yardFenceScope: YardFenceScope | null;
  /** the node's child layers (R yard_children), in the document's order; empty when it has none */
  yardChildren: YardChild[];
}

type Raw = Record<string, unknown>;
const isObj = (v: unknown): v is Raw => typeof v === 'object' && v !== null && !Array.isArray(v);
const str = (v: unknown): string | null => (typeof v === 'string' && v !== '' ? v : null);
const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);
const first = <T,>(...xs: (T | null)[]): T | null => xs.find((x) => x !== null) ?? null;
const numOf = (r: Raw, ...keys: string[]): number | null => first(...keys.map((k) => num(r[k])));
const strOf = (r: Raw, ...keys: string[]): string | null => first(...keys.map((k) => str(r[k])));
/** a list as an array of words or a ";"-separated string */
const listOf = (v: unknown): string[] =>
  Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string' && x !== '') : typeof v === 'string' && v !== '' ? v.split(';').map((x) => x.trim()).filter(Boolean) : [];

const MODES = new Set(['preserve', 'catch_up', 'partner']);
const DEPLOY = new Set(['accelerate', 'watch']);
const RATIONALES = new Set(['operational_control', 'coercive_chokepoint', 'systemic_resilience']);
const PATHWAYS = new Set(['yes', 'no', 'unknown']);
const ALIGN = new Set(['aligned', 'under_prioritized', 'over_prioritized', 'misaligned', 'deployment_gap', 'mixed', 'insufficient_data']);

/** A Deployment Need family from either spelling: `name: {value, basis, scenario, note}` or `name: 0.8` with name_basis / name_scenario / name_note. */
function family(r: Raw, name: string): Family {
  const v = r[name];
  if (isObj(v)) return { value: numOf(v, 'value', 'score'), basis: strOf(v, 'basis'), scenario: strOf(v, 'scenario'), note: strOf(v, 'note', 'detail'), unit: strOf(v, 'unit') };
  return { value: num(v), basis: str(r[`${name}_basis`]), scenario: str(r[`${name}_scenario`]), note: str(r[`${name}_note`]), unit: str(r[`${name}_unit`]) };
}

/** The Competitive Position families the drawer lists, in its order (R strategic_position_tbl names, with or without the cp_ prefix). */
export const CP_FAMILIES: readonly { id: string; label: string }[] = [
  { id: 'export_position', label: 'Export position' },
  { id: 'export_position_structural', label: 'Structural trade share (benchmark)' },
  { id: 'rca', label: 'Revealed comparative advantage' },
  { id: 'trade_momentum', label: 'Current trade momentum' },
  { id: 'production_position', label: 'Production share' },
  { id: 'manufacturing_capacity', label: 'Operating manufacturing capacity' },
  { id: 'investment_position', label: 'Current investment' },
  { id: 'cost_position', label: 'Cost position' },
  { id: 'resource_endowment', label: 'Resource endowment' },
  { id: 'deployment_scale', label: 'Deployment scale' },
];

function cpFamilies(r: Raw): CpFamily[] {
  const nested = isObj(r.cp_families) ? r.cp_families : {};
  const out: CpFamily[] = [];
  for (const f of CP_FAMILIES) {
    const v = first(num(nested[f.id]), num(nested[`cp_${f.id}`]), num(r[`cp_${f.id}`]));
    if (v !== null) out.push({ id: f.id, label: f.label, value: v });
  }
  return out;
}

function trace(v: unknown): TraceStep[] {
  if (!Array.isArray(v)) return [];
  return v.filter(isObj).map((t, i) => ({
    step: str(t.step) ?? (num(t.step) !== null ? String(t.step) : String(i + 1)),
    test: strOf(t, 'test', 'id') ?? '',
    value: typeof t.value === 'number' || typeof t.value === 'string' ? t.value : null,
    threshold: typeof t.threshold === 'number' || typeof t.threshold === 'string' ? t.threshold : null,
    result: strOf(t, 'result', 'state') ?? '',
  }));
}

function policy(r: Raw): PolicyEvidence {
  const p = isObj(r.policy) ? r.policy : {};
  const al = first(strOf(p, 'alignment'), strOf(r, 'alignment', 'policy_alignment'));
  const alc = first(strOf(p, 'alignment_confidence'), strOf(r, 'alignment_confidence', 'policy_alignment_confidence'));
  const mom = p.momentum ?? r.policy_momentum;
  const nSources = first(numOf(p, 'n_sources'), numOf(r, 'pp_n_sources'));
  return {
    priority: first(numOf(p, 'priority', 'intensity'), numOf(r, 'policy_priority')),
    support: first(numOf(p, 'support'), numOf(r, 'policy_support', 'policy_support_priority')),
    protection: first(numOf(p, 'protection'), numOf(r, 'policy_protection', 'policy_protection_priority')),
    protectionRecorded: (() => {
      const v = p.protection_recorded ?? r.policy_protection_recorded;
      return typeof v === 'boolean' ? v : null;
    })(),
    momentum: typeof mom === 'number' && Number.isFinite(mom) ? mom : str(mom),
    alignment: al !== null && ALIGN.has(al) ? (al as Alignment) : null,
    alignmentConfidence: alc === 'high' ? 'high' : alc === 'medium' || alc === 'moderate' ? 'medium' : alc === 'low' ? 'low' : null,
    flag: first(strOf(p, 'flag'), strOf(r, 'alignment_flag', 'policy_alignment_flag')),
    sources: listOf(p.sources ?? r.policy_sources),
    nSources,
  };
}

/** Normalise one detail cell (its tech, stage and posture are checked by checkPostureDetail). */
export function nodeDetail(r: Raw): NodeDetail {
  const stage = r.stage as PostureStage;
  const nt = strOf(r, 'need_type');
  const needType: NeedType = nt === 'deployment' || nt === 'supply_chain' ? nt : stage === DEPLOYMENT_STAGE ? 'deployment' : 'supply_chain';
  const mode = strOf(r, 'mode', 'compete_mode');
  const dm = strOf(r, 'deploy_mode');
  const rat = strOf(r, 'security_rationale', 'rationale');
  const pw = strOf(r, 'pathway_credible', 'pathway');
  const supplyChain: SupplyChainNeed | null =
    needType === 'deployment'
      ? null
      : {
          importDependence: numOf(r, 'import_dependence', 'sn_import_dependence'),
          supplierConcentration: numOf(r, 'import_hhi', 'supplier_concentration', 'sn_supplier_concentration'),
          riskyExposure: numOf(r, 'china_bloc', 'risky_supplier_exposure', 'sn_risky_supplier_exposure'),
          trustedAlternatives: numOf(r, 'trusted_alternatives', 'sn_trusted_alternatives'),
          topSupplier: strOf(r, 'top_supplier', 'top_supplier_iso3'),
          topSupplierShare: numOf(r, 'top_supplier_share'),
          chinaShare: numOf(r, 'china_share'),
          oppositeBlocShare: numOf(r, 'opposite_bloc_share'),
          importsToGdp: numOf(r, 'imports_to_gdp'),
          trustedShare: numOf(r, 'trusted_share', 'trusted_world_share'),
          coerciveTop: strOf(r, 'coercive_top', 'coercive_top_iso3'),
          coerciveTopShare: numOf(r, 'coercive_top_share'),
          coerciveImportShare: numOf(r, 'coercive_import_share'),
          substitutability: strOf(r, 'substitutability'),
          reconstitution: strOf(r, 'reconstitution_time', 'reconstitution'),
          inventories: strOf(r, 'inventories', 'stockpile'),
          essentiality: strOf(r, 'systemic_essentiality', 'essentiality'),
          operationalControl: strOf(r, 'operational_control', 'digitally_active'),
        };
  const deployment: DeploymentNeed | null =
    needType === 'deployment'
      ? {
          demand: family(r, 'dn_demand'),
          policyGap: family(r, 'dn_policy_gap'),
          system: family(r, 'dn_system'),
          alternative: r.dn_climate_aligned !== undefined && r.dn_climate_aligned !== null ? { ...family(r, 'dn_climate_aligned'), scenario: family(r, 'dn_climate_aligned').scenario ?? 'climate_aligned' } : null,
          trend: typeof r.deployment_trend === 'number' && Number.isFinite(r.deployment_trend) ? r.deployment_trend : str(r.deployment_trend),
        }
      : null;
  const pr = isObj(r.prize) ? r.prize : {};
  const prize: Prize = {
    value: first(numOf(pr, 'value'), numOf(r, 'strategic_industrial_value', 'industrial_opportunity', 'opportunity')),
    measure: first(strOf(pr, 'measure'), strOf(r, 'prize_measure')),
    market: first(numOf(pr, 'market'), numOf(r, 'industrial_opportunity')),
    nodePrize: first(numOf(pr, 'node_prize'), numOf(r, 'io_node_prize')),
    investmentMomentum: first(numOf(pr, 'investment_momentum'), numOf(r, 'io_investment_momentum')),
    homeMarket: first(numOf(pr, 'home_market'), numOf(r, 'io_home_market')),
    homeMarketBasis: first(strOf(pr, 'home_market_basis'), strOf(r, 'io_home_market_basis')),
    maturity: (() => {
      const m = pr.maturity ?? r.io_maturity;
      return typeof m === 'number' && Number.isFinite(m) ? m : str(m);
    })(),
    basis: first(strOf(pr, 'basis'), strOf(r, 'io_basis', 'prize_basis', 'opportunity_basis')),
    outlook: first(strOf(pr, 'outlook'), strOf(r, 'io_outlook', 'market_outlook')),
    note: first(strOf(pr, 'note'), strOf(r, 'io_note')),
  };
  const partners = Array.isArray(r.partners) ? (r.partners.filter((x) => isObj(x) && typeof x.iso3 === 'string') as unknown as DetailPartner[]) : [];
  const gap = r.partner_gap;
  return {
    tech: r.tech as string,
    stage,
    key: `${r.tech as string}|${stage}`,
    posture: r.posture as PostureWord,
    variant: strOf(r, 'variant'),
    mode: mode !== null && MODES.has(mode) ? (mode as CompeteMode) : null,
    deployMode: dm !== null && DEPLOY.has(dm) ? (dm as DeployMode) : null,
    confidence: strOf(r, 'confidence'),
    possible: strOf(r, 'possible'),
    pivotal: strOf(r, 'pivotal'),
    delivery: strOf(r, 'delivery', 'delivery_mode'),
    rationale: rat !== null && RATIONALES.has(rat) ? (rat as SecurityRationale) : null,
    routes: listOf(r.security_routes),
    mitigation: strOf(r, 'mitigation'),
    notAssessedReason: strOf(r, 'not_assessed_reason'),
    needType,
    need: numOf(r, 'need', needType === 'deployment' ? 'deployment_need' : 'supply_chain_need'),
    supplyChain,
    deployment,
    prize,
    cp: numOf(r, 'cp'),
    homeShare: numOf(r, 'home_share'),
    cpFamilies: cpFamilies(r),
    pathway: { credible: pw !== null && PATHWAYS.has(pw) ? (pw as Pathway) : null, basis: listOf(r.pathway_basis) },
    borderline: listOf(r.borderline),
    flag: strOf(r, 'consistency_flag', 'flag'),
    trace: trace(r.trace),
    policy: policy(r),
    partners,
    partnerGap: typeof gap === 'boolean' ? gap : null,
    partnerGapReasons: listOf(r.partner_gap_reasons),
    aibStatus: (() => {
      const a = strOf(r, 'partner_aib_status', 'aib_status');
      return a === 'covered' || a === 'gap' ? a : null;
    })(),
    partnerReason: strOf(r, 'partner_reason'),
    partnerEvidence: strOf(r, 'partner_evidence'),
    partnerSource: strOf(r, 'partner_source'),
    trade: isObj(r.trade) ? (r.trade as TradeEvidence) : null,
    watchReasons: listOf(r.watch_reasons),
    reason: strOf(r, 'reason'),
    choicePossible: strOf(r, 'choice_possible'),
    largeYard: largeYard(r.large_yard),
    yardFenceScope: (() => {
      const s = strOf(r, 'yard_fence_scope');
      return s === 'extends_to_hardware' || s === 'unclear' ? s : null;
    })(),
    yardChildren: yardChildren(r.yard_children),
  };
}

const YARDS = new Set(['small', 'medium', 'large']);
const VERBS = new Set(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed']);
const indexes = (v: unknown): number[] => (Array.isArray(v) ? v.filter((x): x is number => typeof x === 'number' && Number.isInteger(x) && x >= 0) : []);

/** The economy's own large-yard case (detail large_yard {case, n, s, u, floor, reason}); null when the document writes none. */
function largeYard(v: unknown): LargeYardException | null {
  if (!isObj(v)) return null;
  return {
    case: typeof v.case === 'boolean' ? v.case : null,
    n: num(v.n),
    s: indexes(v.s),
    u: indexes(v.u),
    floor: v.floor === true,
    reason: str(v.reason),
  };
}

/** The compact child layers (detail yard_children [{layer_id, yard, choice, variant}]); a child without a layer id is dropped. */
function yardChildren(v: unknown): YardChild[] {
  if (!Array.isArray(v)) return [];
  return v.filter(isObj).flatMap((c) => {
    const id = strOf(c, 'layer_id');
    if (id === null) return [];
    const y = strOf(c, 'yard');
    const ch = strOf(c, 'choice');
    return [{ layerId: id, yard: y !== null && YARDS.has(y) ? (y as Yard) : null, choice: ch !== null && VERBS.has(ch) ? (ch as Verb | 'not_assessed') : null, variant: strOf(c, 'variant') }];
  });
}
