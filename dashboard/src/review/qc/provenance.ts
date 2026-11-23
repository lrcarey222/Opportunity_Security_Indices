// The "Why this result?" provenance chain of a posture cell (docs/QC_SYSTEM_DESIGN.md 5): choice -> decision
// trace steps (value, threshold, result) -> component scores (need, prize and its parts, Competitive Position
// and its families, deployment need) -> family values -> the raw values the detail ships (import shares,
// imports / GDP, the trade benchmark) -> the transformation (from config/strategic_position.yml and
// config/deployment_need.yml recipes, hand-mapped below in RECIPES and checked against the YAML by
// provenance.test.ts) -> source and vintage (config/sector_dictionary.yml sources and the bundle meta's
// benchmark_period / momentum_period / trade.benchmark_source). Every link says whether it is shipped in the
// release, known from config only, or not in the release: a gap is stated, never hidden. Pure: no IO (the
// YAML is parsed by the caller and passed in as a ProvenanceDictionary).

import { DEPLOYMENT_STAGE } from '../../lib/stages.ts';
import type { PostureCore } from '../../posture/contract.ts';
import type { NodeDetail } from '../../posture/detail.ts';
import { detailChoice } from './choiceRule.ts';

export type LinkStatus = 'shipped' | 'config_only' | 'not_in_release';
export type LinkLevel = 'choice' | 'trace' | 'component' | 'family' | 'raw' | 'transform' | 'source';

export interface ProvenanceLink {
  /** unique within the chain ("trace:supply_chain_need", "family:need:import_dependence", ...) */
  id: string;
  /** the link this one explains (null for the choice) */
  parent: string | null;
  level: LinkLevel;
  label: string;
  value: number | string | null;
  threshold?: number | string | null;
  result?: string | null;
  status: LinkStatus;
  /** transformation or recipe text */
  text?: string | null;
  /** "<file> <dotted.key>" of the recipe */
  config_ref?: string | null;
  /** source id (config/sector_dictionary.yml) and its short name */
  source?: string | null;
  vintage?: string | null;
  licence?: string | null;
}

export interface ProvenanceChain {
  iso3: string | null;
  tech: string;
  stage: string;
  choice: string | null;
  links: ProvenanceLink[];
  /** the links that are not shipped, in words, for the drawer's "not in this release" line */
  gaps: string[];
}

/** A source of config/sector_dictionary.yml (the fields the chain shows). */
export interface DictionarySource {
  id: string;
  short: string | null;
  edition: string | null;
  licence: string | null;
  accessed: string | null;
  transform: string | null;
}
export interface ProvenanceDictionary {
  sources: Record<string, DictionarySource>;
}

/** Build the dictionary from the parsed config/sector_dictionary.yml (its `sources` list). */
export function dictionaryFromYaml(doc: unknown): ProvenanceDictionary {
  const out: Record<string, DictionarySource> = {};
  const list = doc && typeof doc === 'object' ? (doc as { sources?: unknown }).sources : null;
  const s = (v: unknown): string | null => (typeof v === 'string' && v !== '' ? v : null);
  if (Array.isArray(list))
    for (const x of list) {
      if (!x || typeof x !== 'object') continue;
      const r = x as Record<string, unknown>;
      const id = s(r.id);
      if (id) out[id] = { id, short: s(r.short), edition: s(r.edition), licence: s(r.licence), accessed: s(r.accessed), transform: s(r.transform) };
    }
  return { sources: out };
}

// ---- the recipe map -------------------------------------------------------------------------------------------

export type RecipeFile = 'config/strategic_position.yml' | 'config/deployment_need.yml';
export interface Recipe {
  label: string;
  file: RecipeFile;
  /** dotted key in the file; every key here must exist (provenance.test.ts) */
  key: string;
  /** measures / variables named under the key (checked against the YAML) */
  measures?: readonly string[];
  /** what the pipeline does, in words (from the YAML and the R function it names) */
  transform: string;
  /** source ids in config/sector_dictionary.yml */
  sources?: readonly string[];
}

const SP: RecipeFile = 'config/strategic_position.yml';
const DN: RecipeFile = 'config/deployment_need.yml';

/** The recipes the chain quotes, keyed by the bundle's field ids. */
export const RECIPES = {
  // composite scores and the rule
  supply_chain_need: {
    label: 'Supply-chain vulnerability (Strategic Need)', file: SP, key: 'decision_framework.supply_chain_need',
    transform:
      'Mean of the observed need families, ranked within the cell as a mid-rank percentile (rank-0.5)/n across economies; composition families are zeroed at home share >= self_supply_share (0.10) and for immaterial importers (< USD 1m); thin cells and entrepots are not assessed; a score below score_floor (0.10) caps the percentile at 0.61 (supply_chain_need.R).',
  },
  need_threshold: { label: 'High need bar', file: SP, key: 'decision_framework.need_threshold', transform: 'High need = supply-chain need at or above the bar (inclusive).' },
  prize_threshold: { label: 'High prize bar', file: SP, key: 'decision_framework.prize_threshold', transform: 'High prize = prize at or above the bar and not capped as market-only.' },
  cp_preserve_pct: {
    label: 'Strong position bar', file: SP, key: 'decision_framework.cp_preserve_pct',
    transform: 'Strong position = Competitive Position among contenders at or above the bar, the economy a contender (>= contenders.min_share of world supply) and the capability lower bound at least moderate (decision_engine.R sp_choice_high_cp).',
  },
  contenders: { label: 'Contender line', file: SP, key: 'decision_framework.contenders', transform: 'Contender = measured world-supply share >= min_share; with fewer than min_n contenders the cross-country percentile stands in.' },
  deployment_threshold: { label: 'Accelerate bar', file: SP, key: 'decision_framework.deployment_threshold', transform: 'At Deployment & Use: deployment need at or above the bar -> Accelerate, else Watch.' },
  large_yard: {
    label: 'Large-yard case', file: SP, key: 'decision_framework.large_yard', measures: ['min_signals', 'required', 'position_first'],
    transform: 'mature and passive required; at least min_signals of the eight commodity signals; vetoed by a credible leap or learning points >= 2; with position_first a strong position is tested first (strategic_value.R sp_large_yard_case).',
  },
  pathway: { label: 'Credible pathway', file: SP, key: 'decision_framework.pathway', measures: ['rule', 'direct_share_min'], transform: 'Anchor rule: any anchor (measured share, export share, operating or committed capacity, adjacency, resource base) and at least two evidence families (pathway.R).' },
  score_floor: { label: 'Need score floor', file: SP, key: 'decision_framework.supply_chain_need.score_floor', transform: 'A need score below the floor caps the percentile under the need bar and its band (0.61).' },
  trusted_alternatives: {
    label: 'Trusted alternatives', file: SP, key: 'decision_framework.supply_chain_need.trusted_alternatives_min_coverage',
    transform: 'Within-cell percentile of 1 - (trusted partners\' world share - own share); a fourth need family only where observed for at least the coverage line of assessed cells.',
    sources: ['comtrade_matrix', 'atlas'],
  },
  // need families
  import_dependence: {
    label: 'Import dependence', file: SP, key: 'strategic_need.families.import_dependence', measures: ['deficit_gdp', 'Production surplus/deficit'],
    transform: 'Net imports of the stage basket / GDP and fossil-fuel production surplus/deficit, each inverted and ranked within the cell (strategic_need.R); the family is the mean of its measure percentiles.',
    sources: ['comtrade_matrix', 'wb_wdi', 'ei'],
  },
  supplier_concentration: {
    label: 'Supplier concentration', file: SP, key: 'strategic_need.families.supplier_concentration', measures: ['import_hhi'],
    transform: "HHI of the country's own import suppliers of the stage basket, ranked within the cell (as is).", sources: ['comtrade_matrix'],
  },
  risky_supplier_exposure: {
    label: 'Risky-supplier exposure', file: SP, key: 'strategic_need.families.risky_supplier_exposure', measures: ['china_share', 'opposite_bloc_share'],
    transform: 'Import shares from China (with HKG and MAC) and from opposite-bloc economies, each ranked within the cell; shares below share_floor (0.01) count as 0.',
    sources: ['comtrade_matrix', 'un_votes'],
  },
  // prize
  industrial_opportunity: {
    label: 'Market prize (industrial opportunity)', file: SP, key: 'decision_framework.industrial_opportunity', measures: ['base_year', 'horizon_year', 'scenario', 'caps', 'home_market_mode'],
    transform: 'Node prize = (1 - 0.25) x world demand growth percentile (base_year -> horizon_year, WEO Stated Policies; minerals: IEA Critical Minerals Outlook) + 0.25 x capex-momentum percentile where observed; declining markets capped at 0.33; lifted to mean(node prize, national home market) where the home market is higher.',
    sources: ['iea_weo_pathways', 'iea_cm', 'cim'],
  },
  investment_momentum: { label: 'Capex momentum', file: SP, key: 'decision_framework.industrial_opportunity.investment_momentum', measures: ['weight', 'latest_years', 'baseline_years'], transform: 'Log change of world capex, mean of latest_years over baseline_years, as a percentile among observed nodes; weight 0.25 in the node prize.', sources: ['cim'] },
  strategic_value: {
    label: 'Strategic Industrial Value (prize)', file: SP, key: 'decision_framework.strategic_value', measures: ['weights', 'gate', 'cap_margin', 'prize_measure'],
    transform: 'Weighted mean (1:1:1) of market prize, industrial commons and future option, ranked as a mid-rank percentile across scored rows; where neither commons nor option reaches the gate (0.5, Unknown = 0) the value is capped at prize_threshold - cap_margin (0.59) (strategic_value.R).',
  },
  // Competitive Position families
  export_position: { label: 'Export position', file: SP, key: 'competitive_position.families.export_position', measures: ['market_share', 'rca'], transform: 'World export share and RCA of the stage basket on the structural trade benchmark (85%) with current trade momentum (15%) where it exists.', sources: ['comtrade_reported', 'atlas'] },
  export_position_structural: { label: 'Structural trade benchmark', file: SP, key: 'competitive_position.trade_benchmark', measures: ['years', 'min_years'], transform: 'Comtrade annual exports to the World averaged over the benchmark years; Atlas share for non-reporters.', sources: ['comtrade_reported', 'atlas'] },
  trade_momentum: { label: 'Trade momentum', file: SP, key: 'competitive_position.trade_momentum', measures: ['momentum_weight', 'window_months'], transform: 'Latest 12 months of monthly exports vs the previous 12 (scripts/94 monthly mode), weight 0.15 of the export family.', sources: ['comtrade_reported'] },
  rca: { label: 'Revealed comparative advantage', file: SP, key: 'competitive_position.families.export_position', measures: ['rca'], transform: 'Balassa RCA of the stage basket (export family, four traded stages).', sources: ['comtrade_reported'] },
  production_position: { label: 'Production share', file: SP, key: 'competitive_position.families.production_position', measures: ['size'], transform: 'Share of world mined or refined supply / fuel production (structural zero where absent).', sources: ['usgs', 'ei', 'iea_cm'] },
  manufacturing_capacity: { label: 'Manufacturing capacity', file: SP, key: 'competitive_position.families.manufacturing_capacity', measures: ['Operating Capacity Index'], transform: 'Operating plants by product (GCIM / Rhodium), structural zero where absent.', sources: ['cim'] },
  investment_position: { label: 'Recent investment', file: SP, key: 'competitive_position.families.investment_position', measures: ['Annual Investment Index'], transform: 'Recent manufacturing or deployment capex (structural zero where absent).', sources: ['cim'] },
  innovation_position: { label: 'Innovation position', file: SP, key: 'competitive_position.families.innovation_position', measures: ['Patent Innovation Index', 'Public RD&D Index'], transform: 'US climate-tech patent grants (CPC Y02/Y04S) and IEA public RD&D, technology-level, at Components and Assembly only.' },
  cost_position: { label: 'Cost position', file: SP, key: 'competitive_position.families.cost_position', measures: ['LCOE Index'], transform: 'BNEF LCOE at Deployment only.' },
  resource_endowment: { label: 'Resource endowment', file: SP, key: 'competitive_position.families.resource_endowment', transform: 'Reserves at Extraction; solar, wind and geothermal resource at Deployment.', sources: ['usgs', 'gsa', 'gwa'] },
  deployment_scale: { label: 'Deployment scale', file: SP, key: 'competitive_position.families.deployment_scale', measures: ['size'], transform: 'Generation or use (Deployment only).', sources: ['ei'] },
  // deployment need
  dn_demand: { label: 'Demand adequacy', file: DN, key: 'families.dn_demand', measures: ['scenario'], transform: 'Percentile of the mean of its measure percentiles within the technology (deployment_need.R); WEO Current Policies.', sources: ['iea_weo_pathways', 'iea_gevo', 'ei'] },
  dn_policy_gap: { label: 'Climate / policy gap', file: DN, key: 'families.dn_policy_gap', measures: ['scenario'], transform: 'Projected 2030 shortfall against national targets at the recent build rate; percentile within the technology.' },
  dn_system: { label: 'System need', file: DN, key: 'families.dn_system', measures: ['scenario'], transform: 'Binding system constraints (fuel imports, flexibility, firm capacity); percentile within the technology.' },
} as const satisfies Record<string, Recipe>;

export type RecipeId = keyof typeof RECIPES;

/** The trace test -> the recipe of its threshold. */
const TRACE_RECIPE: Readonly<Record<string, RecipeId>> = {
  supply_chain_need: 'need_threshold',
  strategic_industrial_value: 'prize_threshold',
  industrial_opportunity: 'prize_threshold',
  cp_contender_pct: 'cp_preserve_pct',
  deployment_need: 'deployment_threshold',
  large_yard_case: 'large_yard',
  pathway_credible: 'pathway',
};

/** The trade-benchmark source word of the bundle -> a dictionary source id. */
const BENCHMARK_SOURCE: Readonly<Record<string, string>> = { comtrade: 'comtrade_reported', atlas: 'atlas' };

// ---- the chain ------------------------------------------------------------------------------------------------

const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

/** The provenance chain of one detail cell. iso3: the economy (the detail document's). */
export function provenanceChain(cell: NodeDetail, core: Pick<PostureCore, 'meta'>, dictionary: ProvenanceDictionary, iso3: string | null = null): ProvenanceChain {
  const links: ProvenanceLink[] = [];
  const meta = core.meta;
  const dep = cell.stage === DEPLOYMENT_STAGE;
  const src = (ids: readonly string[] | undefined): Pick<ProvenanceLink, 'source' | 'vintage' | 'licence'> => {
    const found = (ids ?? []).map((i) => dictionary.sources[i]).filter((x): x is DictionarySource => !!x);
    if (!found.length) return { source: null, vintage: null, licence: null };
    return {
      source: found.map((f) => `${f.id}${f.short ? ` (${f.short})` : ''}`).join('; '),
      vintage: found.map((f) => f.edition ?? f.accessed ?? 'edition not stated').join('; '),
      licence: [...new Set(found.map((f) => f.licence ?? 'licence not stated'))].join('; '),
    };
  };
  const recipe = (id: RecipeId): Pick<ProvenanceLink, 'text' | 'config_ref'> => ({ text: RECIPES[id].transform, config_ref: `${RECIPES[id].file} ${RECIPES[id].key}` });
  const add = (l: ProvenanceLink) => links.push(l);
  const component = (id: string, label: string, value: number | null, rid: RecipeId | null, parent: string, missingWhy: string) => {
    add({
      id, parent, level: 'component', label, value, status: value === null ? 'not_in_release' : 'shipped',
      ...(rid ? recipe(rid) : {}), ...(rid ? src((RECIPES[rid] as Recipe).sources) : {}),
      ...(value === null ? { text: `${missingWhy}${rid ? `. Recipe: ${RECIPES[rid].transform}` : ''}` } : {}),
    });
  };

  const choice = detailChoice(cell);
  add({ id: 'choice', parent: null, level: 'choice', label: 'Strategic choice', value: choice, status: choice ? 'shipped' : 'not_in_release', text: cell.reason ?? cell.notAssessedReason ?? null });

  // trace steps
  for (const t of cell.trace) {
    if (t.test === 'choice') continue;
    const rid = TRACE_RECIPE[t.test];
    add({
      id: `trace:${t.test}`, parent: 'choice', level: 'trace', label: t.test, value: t.value, threshold: t.threshold, result: t.result, status: 'shipped',
      ...(rid ? recipe(rid) : { text: t.test === 'secure_override' ? 'The posture engine\'s security override (critical consequence, coercion, delivery tests; config strategic_posture), decided before the triage and not re-runnable from the bundle.' : null }),
    });
  }
  if (!cell.trace.length) add({ id: 'trace', parent: 'choice', level: 'trace', label: 'decision trace', value: null, status: 'not_in_release', text: 'the detail cell carries no decision trace' });
  const parentOf = (test: string) => (links.some((l) => l.id === `trace:${test}`) ? `trace:${test}` : 'choice');

  // config-only settings the trace does not carry
  add({ id: 'config:score_floor', parent: parentOf('supply_chain_need'), level: 'transform', label: 'need score floor', value: null, status: 'config_only', ...recipe('score_floor') });
  add({ id: 'config:contenders', parent: parentOf('cp_contender_pct'), level: 'transform', label: 'contender line and capability floor', value: null, status: 'config_only', ...recipe('contenders') });

  if (dep) {
    const dn = cell.deployment;
    component('component:deployment_need', 'Deployment Need', cell.need, null, parentOf('deployment_need'), 'deployment need not measured for this economy');
    const fam: [RecipeId, string, number | null, string | null][] = [
      ['dn_demand', 'Demand adequacy', dn?.demand.value ?? null, dn?.demand.scenario ?? meta.family_scenarios.demand],
      ['dn_policy_gap', 'Climate / policy gap', dn?.policyGap.value ?? null, dn?.policyGap.scenario ?? meta.family_scenarios.policy_gap],
      ['dn_system', 'System need', dn?.system.value ?? null, dn?.system.scenario ?? meta.family_scenarios.system],
    ];
    for (const [rid, label, v, scen] of fam)
      add({ id: `family:dn:${rid}`, parent: 'component:deployment_need', level: 'family', label, value: v, status: v === null ? 'not_in_release' : 'shipped', ...recipe(rid), ...src((RECIPES[rid] as Recipe).sources), vintage: scen ? `scenario ${meta.scenario_labels[scen] ?? scen}` : null });
  } else {
    // need
    component('component:need', 'Supply-chain vulnerability', cell.need, 'supply_chain_need', parentOf('supply_chain_need'), 'supply-chain need not measured (not assessed)');
    const sc = cell.supplyChain;
    const needFam: [RecipeId, string, number | null][] = [
      ['import_dependence', 'Import dependence', sc?.importDependence ?? null],
      ['supplier_concentration', 'Supplier concentration', sc?.supplierConcentration ?? null],
      ['risky_supplier_exposure', 'Risky-supplier exposure', sc?.riskyExposure ?? null],
      ['trusted_alternatives', 'Trusted alternatives', sc?.trustedAlternatives ?? null],
    ];
    for (const [rid, label, v] of needFam) add({ id: `family:need:${rid}`, parent: 'component:need', level: 'family', label, value: v, status: v === null ? 'not_in_release' : 'shipped', ...recipe(rid) });
    const raw = (id: string, parent: string, label: string, v: number | string | null, sources: readonly string[], why = 'not shipped for this cell') =>
      add({ id: `raw:${id}`, parent, level: 'raw', label, value: v, status: v === null ? 'not_in_release' : 'shipped', ...src(sources), text: v === null ? why : null });
    raw('imports_to_gdp', 'family:need:import_dependence', 'Imports of the stage basket / GDP (gross; the measure is net imports / GDP)', sc?.importsToGdp ?? null, ['comtrade_matrix', 'wb_wdi']);
    add({ id: 'raw:deficit_gdp', parent: 'family:need:import_dependence', level: 'raw', label: 'Net imports / GDP and fuel surplus/deficit (the measures)', value: null, status: 'not_in_release', text: 'the measures behind the family are not shipped; only the family percentile and gross imports / GDP are', config_ref: `${SP} strategic_need.families.import_dependence` });
    add({ id: 'raw:import_hhi', parent: 'family:need:supplier_concentration', level: 'raw', label: 'Import HHI (the measure)', value: null, status: 'not_in_release', text: 'the raw HHI is not shipped; the top supplier and its share are', config_ref: `${SP} strategic_need.families.supplier_concentration` });
    raw('top_supplier', 'family:need:supplier_concentration', 'Largest import supplier', sc?.topSupplier ?? null, ['comtrade_matrix']);
    raw('top_supplier_share', 'family:need:supplier_concentration', "Largest supplier's share of imports", sc?.topSupplierShare ?? null, ['comtrade_matrix']);
    raw('china_share', 'family:need:risky_supplier_exposure', 'China (with HKG and MAC) share of imports', sc?.chinaShare ?? null, ['comtrade_matrix']);
    raw('opposite_bloc_share', 'family:need:risky_supplier_exposure', 'Opposite-bloc share of imports', sc?.oppositeBlocShare ?? null, ['comtrade_matrix', 'un_votes'], 'not applicable or not shipped (nonaligned importers: NA by rule)');
    raw('trusted_share', 'family:need:trusted_alternatives', "Trusted partners' share of world supply", sc?.trustedShare ?? null, ['comtrade_matrix']);
    raw('coercive_top_share', 'component:need', "Largest coercive supplier's share of world supply (Secure-engine input)", sc?.coerciveTopShare ?? null, ['comtrade_matrix']);
    if (sc?.currentTrade)
      add({ id: 'transform:current_trade', parent: 'family:need:risky_supplier_exposure', level: 'transform', label: 'Current-trade blend', value: sc.currentTrade.weight, status: 'shipped', text: `import shares blended with the latest 12 months of ${sc.currentTrade.source ?? 'monthly'} trade (window ending ${sc.currentTrade.windowEnd ?? 'n/a'})`, config_ref: `${SP} current_trade` });

    // prize
    const pz = cell.prize;
    component('component:prize', 'Strategic Industrial Value', pz.value, 'strategic_value', parentOf('strategic_industrial_value'), 'prize not measured');
    add({ id: 'part:prize:market', parent: 'component:prize', level: 'family', label: 'Market prize', value: pz.market, status: pz.market === null ? 'not_in_release' : 'shipped', ...recipe('industrial_opportunity'), ...src(RECIPES.industrial_opportunity.sources), vintage: pz.note ?? null });
    add({ id: 'part:prize:node_prize', parent: 'part:prize:market', level: 'family', label: 'Node prize (world demand growth + capex momentum)', value: pz.nodePrize, status: pz.nodePrize === null ? 'not_in_release' : 'shipped', ...recipe('industrial_opportunity') });
    add({ id: 'part:prize:investment_momentum', parent: 'part:prize:node_prize', level: 'family', label: 'Capex momentum', value: pz.investmentMomentum, status: pz.investmentMomentum === null ? 'not_in_release' : 'shipped', ...recipe('investment_momentum'), ...src(RECIPES.investment_momentum.sources) });
    add({ id: 'part:prize:home_market', parent: 'part:prize:market', level: 'family', label: `Home market (${pz.homeMarketBasis ?? 'basis not stated'})`, value: pz.homeMarket, status: pz.homeMarket === null ? 'not_in_release' : 'shipped', text: 'lift: the prize is raised to mean(node prize, home market) where the national home market is higher', config_ref: `${SP} decision_framework.industrial_opportunity.home_market_mode` });
    const yf = meta.yf_nodes[`${cell.tech}|${cell.stage}|stage`];
    add({ id: 'part:prize:commons', parent: 'component:prize', level: 'family', label: 'Industrial commons (node attribute)', value: yf?.commons ?? null, status: yf?.commons === null || yf?.commons === undefined ? 'not_in_release' : 'shipped', text: 'process knowledge, workforce & suppliers, adjacent innovation: yes 1 / partly 0.5 / no 0, Unknown 0, over three (data/reference/posture_node_attributes.csv)', config_ref: `${SP} decision_framework.strategic_value` });
    add({ id: 'part:prize:option', parent: 'component:prize', level: 'family', label: 'Future option (node attribute)', value: yf?.option ?? null, status: yf?.option === null || yf?.option === undefined ? 'not_in_release' : 'shipped', text: 'four leap tests over four for a leap candidate, else 0 (posture_node_attributes.csv)', config_ref: `${SP} decision_framework.strategic_value` });
    add({ id: 'part:prize:cap', parent: 'component:prize', level: 'transform', label: 'Market-only cap', value: yf?.cap ?? 'none', status: yf ? 'shipped' : 'not_in_release', text: yf?.cap === 'market_only' ? 'neither commons nor option reaches the gate: the value is capped at 0.59, never high, never borderline' : 'not capped', config_ref: `${SP} decision_framework.strategic_value.gate` });
  }

  // Competitive Position and its families
  component('component:cp', 'Competitive Position among contenders', cell.cp, 'cp_preserve_pct', parentOf('cp_contender_pct'), 'Competitive Position not measured for this node');
  for (const f of cell.cpFamilies) {
    const rid = (f.id in RECIPES ? f.id : null) as RecipeId | null;
    add({ id: `family:cp:${f.id}`, parent: 'component:cp', level: 'family', label: f.label, value: f.value, status: 'shipped', ...(rid ? recipe(rid) : {}), ...(rid ? src((RECIPES[rid] as Recipe).sources) : {}) });
  }
  const tr = cell.trade;
  const bsrc = tr?.benchmark_source ? (BENCHMARK_SOURCE[tr.benchmark_source] ?? tr.benchmark_source) : null;
  const bench = (id: string, label: string, v: number | null) =>
    add({
      id: `raw:${id}`, parent: cell.cpFamilies.some((f) => f.id === 'export_position') ? 'family:cp:export_position' : 'component:cp', level: 'raw', label, value: v,
      status: v === null ? 'not_in_release' : 'shipped', ...src(bsrc ? [bsrc] : []), vintage: `${meta.benchmark_period ?? 'benchmark period not stated'} (benchmark)${tr?.benchmark_source ? `, source word "${tr.benchmark_source}"` : ''}`,
      text: v === null ? 'no trade-benchmark value for this node' : null,
    });
  bench('benchmark_share', 'World export share (trade benchmark)', num(tr?.benchmark_share));
  bench('benchmark_rca', 'RCA (trade benchmark)', num(tr?.benchmark_rca));
  bench('benchmark_exports', 'Exports (trade benchmark, USD)', num(tr?.benchmark_exports));
  if (num(tr?.momentum_score) !== null)
    add({ id: 'raw:momentum_score', parent: 'component:cp', level: 'raw', label: 'Trade momentum score', value: num(tr?.momentum_score), status: 'shipped', ...recipe('trade_momentum'), vintage: meta.momentum_period });
  add({ id: 'raw:home_share', parent: 'component:cp', level: 'raw', label: "Measured share of world supply (contender test)", value: cell.homeShare, status: cell.homeShare === null ? 'not_in_release' : 'shipped', text: cell.homeShare === null ? 'no measured share for this node' : 'home_share_exposure, else home_share (sp_measured_share)' });
  add({ id: 'raw:contenders', parent: 'component:cp', level: 'raw', label: 'Contender set behind the percentile', value: null, status: 'not_in_release', text: 'the contender list and n behind cp are not shipped; the capability lower bound (t3_capability_lo) is not shipped either' });
  // the source and vintage the bundle states once
  add({ id: 'source:bundle_periods', parent: 'choice', level: 'source', label: 'Bundle-level vintages', value: null, status: 'shipped', vintage: `trade benchmark ${meta.benchmark_period ?? 'n/a'}; momentum ${meta.momentum_period ?? 'n/a'}`, text: 'indicator-level sources and data years are not shipped in the posture bundle; these are the only vintages it states' });

  const gaps = links.filter((l) => l.status !== 'shipped').map((l) => `${l.label}: ${l.status === 'config_only' ? 'known from config only' : 'not in this release'}${l.text ? ` (${l.text})` : ''}`);
  return { iso3, tech: cell.tech, stage: cell.stage, choice, links, gaps };
}

/** Every config key the recipes name, as [file, dotted key, measures]. For the config test. */
export function recipeKeys(): { id: string; file: RecipeFile; key: string; measures: readonly string[] }[] {
  return Object.entries(RECIPES).map(([id, r]) => ({ id, file: r.file, key: r.key, measures: (r as Recipe).measures ?? [] }));
}

/** Every source id the recipes name. */
export function recipeSourceIds(): string[] {
  return [...new Set(Object.values(RECIPES).flatMap((r) => (r as Recipe).sources ?? []).concat(Object.values(BENCHMARK_SOURCE)))].sort();
}
