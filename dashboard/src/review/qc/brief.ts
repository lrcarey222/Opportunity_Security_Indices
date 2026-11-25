// The neutral case brief (UX v2; NEIS brief sections 3-5 and 11; docs/qc_audit/QC_EVIDENCE_CATALOGUE.md sections
// 2-6): what a reviewer sees BEFORE the blind answer. Reviewers are blind to the Explorer's conclusion, not to the
// information needed to form one, so the brief carries the decision question, context and objective (authored in
// config/review/review_cases.yml `brief:`, with per-kind defaults), the true time horizon and scenario of the kind,
// the six-stage location, 5-8 neutral evidence facts (value, plain reading by generic conventions, source,
// vintage, inline warnings), the analytical assumptions (method choices, linked to method cards), data-quality
// warnings computed from the data, what the data cannot show, and definitions; plus the tailored pre-blind
// sections (partnership sides, the first three links of a compact's causal chain, a sector chain stage by stage).
//
// Never here: a conclusion, a model score (need / prize / position percentiles), a class, a purpose statement,
// coalition outputs, or the AIB physical-capability warning (it fires only for some classes). qc/briefLint.ts
// enforces it and the review data build fails on a leak. Pure: no IO.

import { SCENARIO_DEFINITION, SCENARIO_NAME } from '../../content/vocabulary.ts';
import { tradeWhen, type AibDoc, type AibNode, type AibStrategicCompactScenario } from '../../aib/contract.ts';
import { DEPLOYMENT_STAGE, isSupplyStage, STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import type { PostureCell } from '../../posture/contract.ts';
import { plainReason, type NodeDetail } from '../../posture/detail.ts';
import {
  REVIEW_PRODUCT_OF_KIND,
  type BriefAssumption,
  type BriefDefinition,
  type BriefFact,
  type BriefIdentityPart,
  type BriefWarning,
  type CaseBrief,
  type CaseDefinition,
  type CaseKind,
  type CaseSnapshot,
  type ChainStep,
  type PartnerSide,
  type StageLocation,
} from '../model.ts';
import type { DecodedBundles } from './bundles.ts';
import {
  annualRate,
  cleanNote,
  countryNames,
  flagWords,
  growthWords,
  HHI_SOURCE,
  hhiWords,
  nn,
  parsePrizeNote,
  pct,
  physicalDataWords,
  rcaWords,
  signedPct,
  supplierShareWords,
  TRANSIT_HUBS,
  trlWords,
  usd,
  windowEnd,
} from './evidence.ts';
import { chainAnalysis, type ChainAnalysis, type ChainNode } from './sectorChain.ts';

// ---- vocabularies -------------------------------------------------------------------------------------------------

/** The brief fact ids each kind can show (brief.evidence_ids in review_cases.yml names these). */
export const POSTURE_BRIEF_FACT_IDS = [
  'largest_supplier', 'china_import_share', 'rival_group_import_share', 'own_world_share', 'export_position', 'export_trend', 'major_supplier_world_share',
  'same_group_world_share', 'import_scale', 'demand_outlook', 'technology_readiness',
] as const;
export const DEPLOYMENT_BRIEF_FACT_IDS = ['installed_trend', 'deployment_demand', 'target_gap', 'system_need', 'climate_context'] as const;
export const AIB_NODE_BRIEF_FACT_IDS = [
  'allied_imports', 'china_share_allied', 'supply_at_risk', 'shift_share', 'concentration_benchmark', 'top_suppliers', 'concentration_today', 'top3_today', 'production_data',
  'data_quality',
] as const;
export const CHAIN_BRIEF_FACT_IDS = [
  'chain_steps', 'chain_imports', 'supply_at_risk', 'step_suppliers', 'existing_trade_cover', 'gap_after_existing', 'untested_steps', 'production_data_gaps',
  'production_divergence',
] as const;
export const PARTNERSHIP_BRIEF_FACT_IDS = ['a_strengths_b_needs', 'b_strengths_a_needs', 'shared_exposure', 'both_strong', 'shared_allied_problem', 'memo_status'] as const;

export const BRIEF_FACT_IDS_BY_KIND: Readonly<Record<CaseKind, readonly string[]>> = {
  posture_cell: [...POSTURE_BRIEF_FACT_IDS, ...DEPLOYMENT_BRIEF_FACT_IDS],
  sector: [...POSTURE_BRIEF_FACT_IDS, ...DEPLOYMENT_BRIEF_FACT_IDS],
  aib_node: AIB_NODE_BRIEF_FACT_IDS,
  compact: CHAIN_BRIEF_FACT_IDS,
  sector_chain: CHAIN_BRIEF_FACT_IDS,
  partnership: PARTNERSHIP_BRIEF_FACT_IDS,
};

/** The analytical assumptions (method choices, worded neutrally; never a conclusion label). */
export const ASSUMPTIONS: Readonly<Record<string, BriefAssumption>> = Object.fromEntries(
  (
    [
      ['need_rank', 'Vulnerability is a rank, not a level', 'Import vulnerability is ranked against about 200 economies for the same step, so it says how exposed an economy is relative to others, not how much dependence is acceptable.', 'MC_007'],
      ['supplier_groups', 'Who counts as a higher-risk or a same-group supplier', 'Economies on the other side of UN General Assembly votes (sessions to 2023), plus China (with Hong Kong and Macao) and Russia, count as higher-risk suppliers; other economies in the same voting group count as same-group suppliers. Several such definitions coexist in the Explorer.', 'MC_022'],
      ['self_supply', 'Large producers are not scored on their import mix', 'An economy that holds 10% or more of world supply of a step is not scored on where its imports come from.', 'MC_008'],
      ['transit_hubs', 'Only Hong Kong and Macao are treated as transit hubs', 'Trade passing through other ports (for example Rotterdam or Singapore) is read as the supply of the economy it passes through.', 'MC_014'],
      ['value_parts', 'What makes a step valuable', 'The value of a step weighs world market growth to 2035, industrial know-how and future technology options equally; market size alone cannot make the value high.', 'MC_011'],
      ['value_whole_step', 'Value is set for the step worldwide', 'The value of a step is the same for every economy, lifted where the home market grows fast and never lowered by a small home market.', 'MC_003'],
      ['position_contenders', 'What a strong position means', 'A strong position means ranking in the top quarter of the economies that hold at least 0.5% of world supply, with a minimum capability floor.', 'MC_004'],
      ['commodity_steps', 'Commodity steps default to open supply', 'Mature, standardised, diversified steps default to affordable, diversified supply unless the economy already ranks in the top quarter of producers.', 'MC_005'],
      ['build_base', 'Building up needs a real base', 'Building a capability up requires an existing anchor (supply, exports, operating capacity or committed projects) plus one corroborating signal.', 'MC_009'],
      ['security_routes', 'A separate security test comes first', 'Before value or position is weighed, a security test looks for an acute vulnerability through three curated routes (control or dual-use steps, a rival chokepoint, essential systems that take long to rebuild); concentration alone never triggers it.', 'MC_013'],
      ['current_trade_weight', 'Recent trade counts for half', 'Import shares weigh the latest 12 months of customs data at 50% beside 2024, where monthly data exist.', null],
      ['policy_not_input', 'Current policy is not an input', 'Observed government policy does not feed the result; it is compared with the result afterwards.', null],
      ['deployment_families', 'How deployment need is measured', 'Deployment need combines demand under current policies to 2035, the gap to 2030 national targets and power-system constraints; a result is reached even when only one of the three is measured.', 'MC_010'],
      ['deployment_only', 'Deployment is separate from manufacturing', 'The question is only about deploying the technology at home; where the equipment is made is assessed separately.', null],
      ['climate_context_only', 'A net-zero path is context only', 'A climate-aligned (net-zero) deployment path is shown for context and never used.', null],
      ['traded_only', 'Only traded supply counts', 'The stress test reads traded supply only; production consumed at home is not seen.', 'MC_015'],
      ['hold_semantics', 'The cut is one-way', 'In the disruption China stops selling to allied economies but keeps buying from them; a two-way cut is a sensitivity test.', 'MC_016'],
      ['growth_bound', 'Existing links can at most double', 'After a shock, an existing supplier-buyer link may grow to at most twice its current size.', 'MC_017'],
      ['reallocation', 'Suppliers may redirect exports', 'Suppliers may redirect exports away from buyers outside the allied group.', 'MC_018'],
      ['round_bars', 'Fixed bars turn results into a response type', 'Round-number bars on exposure, on the gap left after trade and on concentration decide which kind of response a product needs.', 'MC_019'],
      ['sibling_benchmark', 'Normal concentration is set by comparable products', 'Under Managed Interdependence, a product is compared with the other products of its customs heading; only concentration above that benchmark has to shift.', 'MC_020'],
      ['allied_scope', 'Who the allied economies are', 'Allied demand covers 59 economies (the coalition core and its wider allied tier); any economy may supply them.', 'MC_022'],
      ['physical_limits', 'Production data limit who can add supply', 'Where production data cover at least 80% of world production, economies that do not produce cannot add supply.', null],
      ['immediate', 'Re-sourcing is immediate', 'No build times, prices or firm capacity are modelled: supply is re-sourced at once.', null],
      ['materiality', 'When joint action on a chain is material', 'Joint action on a chain is judged material when it concerns a quarter of the chain supply at risk, or US$1bn.', 'MC_021'],
      ['pm_competition', 'Two strong producers compete', 'Where both economies are strong producers of a step they are treated as competitors there, not as complementary.', null],
      ['pm_supply_need', 'Complementarity needs a supplier and a buyer', 'Complementarity needs one side to be a strong (or named moderate) supplier and the other to be highly vulnerable on imports.', null],
      ['pm_chokepoint', 'What a chokepoint is', 'A chokepoint is a rival supplier holding at least half of world supply.', null],
      ['pm_short', 'When allied supply is short', 'Allied supply counts as short where the allied stress test leaves a gap that trade cannot fill.', null],
    ] as const
  ).map(([id, label, text, card]) => [id, { id, label, text, method_card_id: card }]),
);

const DEFAULT_ASSUMPTIONS: Readonly<Record<string, readonly string[]>> = {
  producing: ['need_rank', 'supplier_groups', 'self_supply', 'value_parts', 'position_contenders', 'commodity_steps', 'security_routes', 'current_trade_weight'],
  deployment: ['deployment_families', 'deployment_only', 'climate_context_only', 'policy_not_input'],
  aib_decoupling: ['traded_only', 'hold_semantics', 'growth_bound', 'reallocation', 'round_bars', 'allied_scope', 'physical_limits', 'immediate'],
  aib_managed: ['traded_only', 'sibling_benchmark', 'growth_bound', 'reallocation', 'round_bars', 'allied_scope', 'physical_limits', 'immediate'],
  chain_decoupling: ['traded_only', 'hold_semantics', 'growth_bound', 'reallocation', 'round_bars', 'materiality', 'allied_scope', 'immediate'],
  chain_managed: ['traded_only', 'sibling_benchmark', 'growth_bound', 'reallocation', 'round_bars', 'materiality', 'allied_scope', 'immediate'],
  partnership: ['pm_competition', 'pm_supply_need', 'pm_chokepoint', 'pm_short', 'supplier_groups'],
};

const DEFAULT_OBJECTIVE = 'Resilient and affordable supply, not maximum national self-sufficiency.';
const OBJECTIVE: Readonly<Record<string, string>> = {
  producing: DEFAULT_OBJECTIVE,
  deployment: 'An energy system that gets enough of this technology, affordably, by 2035; where the equipment is made is a separate question.',
  aib: 'Allied supply that survives the stress at reasonable cost, using trade before new capacity, not self-sufficiency for each country.',
  chain: 'Allied supply that survives the stress at reasonable cost, with joint action only where it changes the outcome.',
  partnership: 'Joint resilience and industrial value where one side’s strengths meet the other’s needs, not diplomatic goodwill.',
};

const SOURCE = {
  comtrade: 'UN Comtrade, reconciled trade matrix',
  comtradeReported: 'UN Comtrade, exporter-reported',
  comtradeMonthly: 'UN Comtrade monthly customs reports',
  worldSupply: 'World supply: production data (International Energy Agency, Energy Institute, US Geological Survey) or export shares (Harvard Atlas), by step',
  unVotes: 'UN General Assembly voting data (sessions to 2023)',
  gdp: 'UN Comtrade; World Bank World Development Indicators',
  aibTrade: 'UN Comtrade (each economy’s latest 12 months of customs reports; 2024 where none)',
  posture: 'Explorer country data (UN Comtrade trade; production and export shares)',
  memo: 'Partnership memo manifest',
} as const;

// ---- small builders -----------------------------------------------------------------------------------------------

type FactInit = Omit<BriefFact, 'warnings' | 'estimate'> & { warnings?: BriefWarning[]; estimate?: boolean };
const bf = (o: FactInit): BriefFact => ({ ...o, interpretation: o.interpretation ?? null, warnings: o.warnings ?? [], estimate: o.estimate ?? false });
const warn = (id: string, title: string, text: string, level: BriefWarning['level'] = 'warn'): BriefWarning => ({ id, level, title, text });

/** The six-stage strip with the given stages highlighted. */
export function stageStrip(highlight: readonly string[]): StageLocation {
  return { stages: SUPPLY_STAGES.map((s) => ({ key: s, label: STAGE_DISPLAY[s] })), highlight: SUPPLY_STAGES.filter((s) => highlight.includes(s)) };
}

/** Pick 5-8 facts: the authored ids in order, else the defaults, topped up from the rest (non-null values first). */
function pickFacts(all: Map<string, BriefFact>, wanted: readonly string[] | undefined, defaults: readonly string[], min = 5, max = 8): BriefFact[] {
  const ok = (id: string) => {
    const f = all.get(id);
    return f !== undefined && f.value !== null;
  };
  if (wanted && wanted.length) return wanted.map((id) => all.get(id)).filter((f): f is BriefFact => !!f);
  const out = defaults.filter(ok);
  for (const id of all.keys()) if (out.length < min && !out.includes(id) && ok(id)) out.push(id);
  return out.slice(0, max).map((id) => all.get(id)!);
}

const pickAssumptions = (wanted: readonly string[] | undefined, defaults: readonly string[]): BriefAssumption[] =>
  (wanted && wanted.length ? wanted : defaults).map((id) => ASSUMPTIONS[id]).filter((a): a is BriefAssumption => !!a);

const isoDate = (s: string | null | undefined): string | null => (typeof s === 'string' && /^\d{4}-\d{2}-\d{2}/.test(s) ? s.slice(0, 10) : null);

// ---- posture cells (Country review) -------------------------------------------------------------------------------

interface PostureCtx {
  def: Pick<CaseDefinition, 'kind' | 'key' | 'brief' | 'title'>;
  b: DecodedBundles;
  name: (iso: string | null | undefined) => string;
}

function postureBrief(o: PostureCtx): CaseBrief {
  const { def, b, name } = o;
  const p = b.posture;
  const iso = def.key.iso3 ?? '';
  const tech = def.key.tech ?? '';
  const stage = def.key.stage ?? '';
  const c: PostureCell | undefined = p?.core.cells.find((x) => x.iso3 === iso && x.tech === tech && x.stage === stage);
  const d: NodeDetail | undefined = p?.details[iso]?.cells.find((x) => x.tech === tech && x.stage === stage);
  const meta = p?.core.meta;
  const dep = stage === DEPLOYMENT_STAGE;
  const country = name(iso);
  const stageName = isSupplyStage(stage) ? STAGE_DISPLAY[stage] : stage;
  const phrase = meta?.phrases[`${tech}|${stage}`] ?? `${tech.toLowerCase()} ${stageName.toLowerCase()}`;
  const parts: BriefIdentityPart[] = [
    { role: 'country', label: country, code: iso },
    { role: 'technology', label: tech, code: tech },
    { role: 'stage', label: stageName, code: stage },
  ];
  const identity = { title: `${country} × ${tech} × ${stageName}`, parts };
  const sc = d?.supplyChain ?? null;
  const ct = sc?.currentTrade ?? null;
  const tr = d?.trade ?? null;
  const facts = new Map<string, BriefFact>();
  const warnings: BriefWarning[] = [];
  const missing: string[] = [];
  const definitions: BriefDefinition[] = [];
  const created = isoDate(p?.manifest && typeof p.manifest.created === 'string' ? p.manifest.created : p?.core.created);
  const importVintage = ct ? `2024 blended with the 12 months to ${windowEnd(ct.windowEnd) ?? 'the latest month'}` : '2024';
  let horizon: CaseBrief['time_horizon'];
  let scenario: CaseBrief['scenario'];
  let question: string;
  let context: string;
  let objective: string;
  let assumptionDefaults: readonly string[];
  let factDefaults: string[];

  if (!dep) {
    // ---- producing stages
    const top = sc?.topSupplier ?? null;
    const topShare = sc?.topSupplierShare ?? null;
    const hub = top !== null && TRANSIT_HUBS.includes(top) && (topShare ?? 0) >= 0.5;
    facts.set('largest_supplier', bf({
      id: 'largest_supplier', label: `Largest supplier of ${country}’s imports for ${phrase}`, value: topShare,
      display: top ? `${name(top)}, ${pct(topShare)} of imports` : 'not measured',
      interpretation: top ? `${supplierShareWords(topShare)}${hub ? '; this economy is a major transit hub' : ''}` : null,
      source: SOURCE.comtrade, vintage: importVintage,
      warnings: ct?.source === 'ember_fill' ? [warn('estimated_latest', 'Latest months estimated', 'The latest 12 months are estimated from partners’ reports and China’s export data, not from this economy’s own customs data.', 'info')] : [],
    }));
    const cs = sc?.chinaShare ?? null;
    facts.set('china_import_share', bf({
      id: 'china_import_share', label: `China’s share of ${country}’s imports (with Hong Kong and Macao)`, value: iso === 'CHN' ? null : cs,
      display: iso === 'CHN' ? 'not applicable' : `${pct(cs)}${ct && ct.chinaShareBase !== null && ct.chinaShareLatest !== null ? ` (2024: ${pct(ct.chinaShareBase)}; latest 12 months: ${pct(ct.chinaShareLatest)})` : ''}`,
      interpretation: iso === 'CHN' ? null : supplierShareWords(cs), source: SOURCE.comtrade, vintage: importVintage,
    }));
    const rs = sc?.coerciveImportShare ?? null;
    facts.set('rival_group_import_share', bf({
      id: 'rival_group_import_share', label: iso === 'CHN' ? 'Share of China’s imports from Russia and the other UN-voting group' : `Share of ${country}’s imports from China, Russia and the other UN-voting group`, value: rs,
      display: `${pct(rs)}${ct && ct.coerciveShareBase !== null && ct.coerciveShareLatest !== null ? ` (2024: ${pct(ct.coerciveShareBase)}; latest 12 months: ${pct(ct.coerciveShareLatest)})` : ''}`,
      interpretation: rs === null ? null : rs >= 0.5 ? 'more than half of imports come from this group' : rs >= 0.25 ? 'between a quarter and a half of imports come from this group' : 'under a quarter of imports come from this group',
      source: `${SOURCE.comtrade}; ${SOURCE.unVotes}`, vintage: `${importVintage}; voting to 2023`,
      warnings: [warn('definition', 'A definition, not an observation', 'Which economies count in this group is a modelling choice; see the assumptions.', 'info')],
    }));
    const home = d?.homeShare ?? c?.share ?? null;
    facts.set('own_world_share', bf({
      id: 'own_world_share', label: `${country}’s share of world supply in ${phrase}`, value: home, display: pct(home),
      interpretation: home === null ? null : 'measured share of world supply of this step (production where measured, otherwise exports)',
      source: SOURCE.worldSupply, vintage: 'latest year of each source (production data to 2025; export shares to 2024)',
      warnings: [warn('measure_not_stated', 'Measure not stated', 'This release does not say whether production or exports were used for this step.', 'info')],
    }));
    const bs = nn(tr?.benchmark_share);
    const br = nn(tr?.benchmark_rca);
    const be = nn(tr?.benchmark_exports);
    facts.set('export_position', bf({
      id: 'export_position', label: `${country}’s share of world exports and export specialisation`, value: bs,
      display: bs === null ? 'not measured' : `${pct(bs)} of world exports; revealed comparative advantage (RCA) ${br === null ? 'not measured' : br.toFixed(2)}; ${usd(be)} a year`,
      interpretation: rcaWords(br), source: tr?.benchmark_source === 'atlas' ? 'Harvard Atlas of Economic Complexity' : SOURCE.comtradeReported, vintage: meta?.benchmark_period ?? '2022-2024',
    }));
    const exC = nn(tr?.exports_current);
    const exP = nn(tr?.exports_previous);
    const ch = exC !== null && exP !== null && exP > 0 ? (exC / exP - 1) * 100 : null;
    facts.set('export_trend', bf({
      id: 'export_trend', label: `${country}’s exports, latest 12 months against the previous 12`, value: ch === null ? null : ch / 100,
      display: ch === null ? 'not measured' : `${usd(exC)} against ${usd(exP)} (${signedPct(ch)})`,
      interpretation: ch === null ? null : Math.abs(ch) < 5 ? 'roughly unchanged' : ch > 0 ? 'rising' : 'falling',
      source: SOURCE.comtradeMonthly, vintage: `${meta?.momentum_period ?? 'latest 12 months'} against ${meta?.momentum_previous_period ?? 'previous 12 months'}`,
      warnings: tr?.momentum_quality && tr.momentum_quality !== 'ok' ? [warn('momentum_quality', 'Short series', `The monthly export series is marked “${tr.momentum_quality}”.`, 'info')] : [],
    }));
    const ct2 = sc?.coerciveTop ?? null;
    const named = ct2 === 'CHN' || ct2 === 'RUS';
    const cts = sc?.coerciveTopShare ?? null;
    facts.set('major_supplier_world_share', bf({
      id: 'major_supplier_world_share', label: named ? `${name(ct2)}’s share of world supply in ${phrase}` : 'Largest world supplier outside the same group', value: named ? cts : null,
      display: named ? pct(cts) : 'not shown for this economy',
      interpretation: named ? (cts !== null && cts >= 0.5 ? 'a majority of world supply in one economy' : `${supplierShareWords(cts)} in world supply`) : null,
      source: SOURCE.worldSupply, vintage: 'latest year of each source',
    }));
    const ts = sc?.trustedShare ?? null;
    facts.set('same_group_world_share', bf({
      id: 'same_group_world_share', label: `World supply held by economies in ${country}’s UN-voting group (excluding ${country})`, value: ts, display: pct(ts),
      interpretation: ts === null ? null : ts >= 0.5 ? 'same-group economies hold more than half of world supply' : 'same-group economies hold less than half of world supply',
      source: `${SOURCE.worldSupply}; ${SOURCE.unVotes}`, vintage: 'latest year of each source; voting to 2023',
      warnings: ts === 0 ? [warn('zero_same_group', 'Implausible zero', 'This share is exactly 0%; it may reflect the measure used for this step rather than real supply. Treat it with caution.')] : [],
    }));
    const ig = sc?.importsToGdp ?? null;
    facts.set('import_scale', bf({
      id: 'import_scale', label: `Gross imports for ${phrase}, as a share of ${country}’s GDP`, value: ig, display: ig === null ? 'not measured' : `${(ig * 100).toFixed(3)}% of GDP`,
      interpretation: ig === null ? null : 'gross imports, shown as a number only (net imports are not in this release)', source: SOURCE.gdp, vintage: '2024',
    }));
    const out = parsePrizeNote(d?.prize.note);
    const mineral = out?.source.includes('Critical Minerals') ?? false;
    facts.set('demand_outlook', bf({
      id: 'demand_outlook', label: `World demand outlook for ${phrase}`, value: out?.totalPct === null || !out ? null : out.totalPct / 100,
      display: out && out.totalPct !== null ? `${signedPct(out.totalPct)} from ${out.fromYear} to ${out.toYear} (${growthWords(out.totalPct, out.toYear - out.fromYear)})` : 'not measured',
      interpretation: out && out.capexPct !== null ? `a projection; recent world clean-technology manufacturing investment changed ${signedPct(out.capexPct)} (${out.capexWindow})` : null,
      source: out ? `${out.source}${out.capexSource ? `; ${out.capexSource}` : ''}` : null, vintage: out ? `${out.fromYear}-${out.toYear}` : null,
      warnings: mineral ? [warn('minerals_base_year', 'Base year to check', 'The mineral outlook is labelled 2024 to 2035, but the configuration states a 2025 base year for the minerals data.', 'info')] : [],
    }));
    const trl = typeof d?.prize.maturity === 'number' ? d.prize.maturity : null;
    facts.set('technology_readiness', bf({
      id: 'technology_readiness', label: 'Technology readiness, International Energy Agency scale of 1 to 11', value: trl, display: trl === null ? 'not measured' : trl.toFixed(1),
      interpretation: trlWords(trl), source: 'International Energy Agency Clean Energy Technology Guide', vintage: 'edition not stated in the bundle',
    }));
    factDefaults = top === 'CHN' || iso === 'CHN'
      ? ['largest_supplier', 'rival_group_import_share', 'own_world_share', 'export_position', 'export_trend', 'major_supplier_world_share', 'same_group_world_share', 'demand_outlook']
      : ['largest_supplier', 'china_import_share', 'own_world_share', 'export_position', 'export_trend', 'major_supplier_world_share', 'same_group_world_share', 'demand_outlook'];

    // warnings computed from the data
    const fam = d?.cpFamilies ?? [];
    if (fam.length && fam.every((f) => f.id.startsWith('export_position')))
      warnings.push(warn('trade_only', 'Production evidence limited', `${country}’s current capability here is inferred from trade data only: no production, capacity, investment or innovation measure is available for this step.`));
    if (home !== null && bs !== null && Math.abs(home - bs) >= 0.05 && Math.max(home, bs) >= 0.05)
      warnings.push(warn('trade_production_divergence', 'Trade and production evidence disagree', `${country} holds ${pct(home)} of world supply but ${pct(bs)} of world exports for this step; the release does not say which measure the world-supply share uses.`));
    if (c?.evidenceRaw === 'none' || c?.evidenceRaw === 'low')
      warnings.push(warn('thin_capability_evidence', 'Thin capability evidence', 'Few of the capability measures (production, capacity, investment, innovation) are available for this step.'));
    if (hub) warnings.push(warn('transit_hub', 'Possible transit trade', `${name(top)} supplies ${pct(topShare)} of imports, but it is a major transit hub: much of this may be goods passing through rather than produced there.`));
    if (ct?.flag) warnings.push(warn('china_share_moved', 'China’s import share moved sharply', `China’s share of imports went from ${pct(ct.chinaShareBase)} in 2024 to ${pct(ct.chinaShareLatest)} in the latest 12 months.`));
    warnings.push(
      ct
        ? warn('import_periods', 'Import shares mix two periods', `Import shares combine 2024 with the 12 months to ${windowEnd(ct.windowEnd) ?? 'the latest month'} at equal weight${ct.source === 'ember_fill' ? ' (the latest months are estimated)' : ` (${country}’s own customs reports)`}.`, 'info')
        : warn('import_periods', 'Import shares are for 2024', `No monthly customs data for ${country}: import shares are for 2024 only.`, 'info'),
    );
    if (home !== null && home >= 0.1) warnings.push(warn('large_producer', 'Large producer', `${country} holds ${pct(home)} of world supply, so its imports are a small part of its own supply.`, 'info'));
    if (sc && sc.trustedAlternatives === null) warnings.push(warn('alternatives_not_measured', 'Alternative suppliers not measured', 'The measure of same-group alternative suppliers is missing for this step.', 'info'));
    if (d?.notAssessedReason) warnings.push(warn('too_little_data', 'Too little data', plainReason(d.notAssessedReason) ?? d.notAssessedReason));
    warnings.push(warn('older_groups', 'Older source', 'Supplier groups are based on UN General Assembly votes up to 2023.', 'info'));
    missing.push(
      'Import values in US dollars for this step are not in this release.',
      'Whether the world-supply share uses production or exports is not stated for this step.',
      'Net imports (the basis of the vulnerability measure) are not shipped; gross imports are shown.',
      'Firm-level capacity, qualification lead times, cost curves and project pipelines are not captured.',
    );
    definitions.push(
      { term: 'Share of world supply', text: 'The economy’s share of world production of this step where production is measured, otherwise of world exports.' },
      { term: 'Revealed comparative advantage (RCA)', text: 'The step’s share of the economy’s exports divided by its share of world exports: above 1 means the economy specialises in it.' },
      { term: 'UN-voting group', text: 'Economies grouped by how they vote in the UN General Assembly; the analysis treats the other group, plus China and Russia, as higher-risk suppliers.' },
      { term: 'Gross imports as a share of GDP', text: 'The value of imports of this step’s goods divided by gross domestic product.' },
    );
    horizon = {
      label: 'Through 2035',
      note: `Market outlook 2024 to 2035; import shares ${importVintage}; export benchmark ${meta?.benchmark_period ?? '2022-2024'}; export trend ${meta?.momentum_period ?? 'latest 12 months'}.`,
    };
    scenario = { key: 'baseline', label: SCENARIO_NAME.baseline, description: 'Current trade network and stated policies (International Energy Agency World Energy Outlook 2025, Stated Policies Scenario); no disruption is assumed.' };
    question = `Over the next decade (to 2035), how should ${country} handle ${phrase} (the ${stageName} step of ${tech}): protect it, keep and strengthen it, build it up, rely on trusted partners or diversified suppliers, or leave it to the open market?`;
    context = `This approximates a call a national economic-security or industrial-strategy team makes about one step of one supply chain. The objective is resilient, affordable supply and lasting industrial value, not self-sufficiency: relying on partners or leaving supply open can be the best outcome. The analysis does not assess individual firms, the cost of programmes or which policy instrument to use.`;
    objective = OBJECTIVE.producing;
    assumptionDefaults = DEFAULT_ASSUMPTIONS.producing;
  } else {
    // ---- Deployment & Use
    const dp = d?.deployment ?? null;
    const trend = dp?.trend === null || dp?.trend === undefined ? null : String(dp.trend);
    facts.set('installed_trend', bf({
      id: 'installed_trend', label: `${tech} installed in ${country}`, value: trend, display: trend ? trend.replace(/ -> /g, ' to ') : 'not measured',
      interpretation: null, source: 'Ember; Energy Institute Statistical Review', vintage: '2020-2025',
    }));
    const dn = cleanNote(dp?.demand.note);
    facts.set('deployment_demand', bf({
      id: 'deployment_demand', label: 'Demand outlook and observed growth', value: dn, display: dn ?? 'not measured',
      interpretation: 'current-policies outlook beside the growth actually observed', source: 'International Energy Agency World Energy Outlook 2025, Current Policies Scenario; Ember', vintage: '2024-2035 (observed 2020-2025)',
    }));
    const tg = dp?.policyGap.value === null || dp?.policyGap.value === undefined ? null : cleanNote(dp.policyGap.note);
    facts.set('target_gap', bf({
      id: 'target_gap', label: 'Distance to the 2030 national target', value: tg ?? 'not measured', display: tg ?? `Not measured for ${country}`,
      interpretation: tg ? null : 'no comparable 2030 target is recorded for this technology and economy', source: 'Ember 2030 target tracker and approved national plans', vintage: '2030 targets',
      warnings: tg ? [] : [warn('target_missing', 'Evidence missing', `The 2030-target comparison is not measured for ${country}, so this part of the evidence is absent.`)],
    }));
    const sy = cleanNote(dp?.system.note);
    facts.set('system_need', bf({
      id: 'system_need', label: 'Power-system constraints', value: sy, display: sy ?? 'not measured', interpretation: 'firm capacity and import displacement the system needs', source: 'Ember; Energy Institute; International Energy Agency', vintage: '2025',
    }));
    const alt = dp?.alternative ?? null;
    const altV = nn(alt?.value);
    facts.set('climate_context', bf({
      id: 'climate_context', label: 'World deployment under a net-zero path, against current policies', value: altV,
      display: altV === null ? 'not measured' : `${signedPct(altV * 100)} more deployment worldwide by 2035 than under current policies`,
      interpretation: 'context only: never used in the analysis', source: 'International Energy Agency Net Zero Emissions and Current Policies scenarios', vintage: '2035',
    }));
    factDefaults = ['installed_trend', 'deployment_demand', 'target_gap', 'system_need', 'climate_context'];
    const fams = [dp?.demand, dp?.policyGap, dp?.system].filter((f) => f && f.value !== null).length;
    if (fams < 3) warnings.push(warn('families_missing', 'Part of the evidence is missing', `${fams} of the 3 evidence families (demand, 2030 targets, system needs) are measured for ${country}.`));
    missing.push('Grid connection queues, permitting times and project pipelines are not captured.', 'Costs of deployment and financing conditions are not captured.');
    definitions.push(
      { term: 'Current policies', text: 'What existing policies imply, without new commitments (International Energy Agency Current Policies Scenario).' },
      { term: '2030 national target', text: 'The deployment level a government has committed to for 2030.' },
      { term: 'Firm capacity', text: 'Capacity that can run when needed, regardless of weather.' },
    );
    horizon = { label: 'Through 2035, against 2030 national targets', note: 'Demand 2024 to 2035 under current policies; 2030 national targets; system constraints as of 2025.' };
    scenario = { key: 'current_policies', label: 'Current policies', description: 'Demand under the International Energy Agency Current Policies Scenario to 2035, compared with 2030 national commitments; no disruption is assumed.' };
    question = `Does ${country} need a major push to deploy ${phrase} at home faster than current policies imply (to 2030-2035)? This asks only about deployment, not about where the equipment is made.`;
    context = `This approximates a call an energy ministry or system planner makes. The question is whether the domestic energy system needs much more of this technology; it says nothing about manufacturing it. The analysis does not cost programmes or compare instruments.`;
    objective = OBJECTIVE.deployment;
    assumptionDefaults = DEFAULT_ASSUMPTIONS.deployment;
  }
  if (!c) warnings.unshift(warn('not_in_release', 'Not in this release', `${country} ${tech} ${stageName} is not in the posture data.`));
  if (created) horizon.note = `${horizon.note ?? ''} Data rebuilt ${created}.`.trim();
  const a = def.brief;
  return {
    product: REVIEW_PRODUCT_OF_KIND[def.kind],
    identity,
    decision_question: a?.decision_question ?? question,
    decision_context: a?.decision_context ?? context,
    objective: a?.objective ?? objective,
    time_horizon: a?.time_horizon ? { label: a.time_horizon, note: horizon.note } : horizon,
    scenario,
    stage_location: stageStrip([stage]),
    evidence: pickFacts(facts, a?.evidence_ids, factDefaults),
    assumptions: pickAssumptions(a?.assumption_ids, assumptionDefaults),
    warnings,
    missing_evidence: missing,
    definitions,
    partnership: null,
    compact_chain: null,
    sector_chain: null,
  };
}

// ---- AIB words shared by nodes and chains --------------------------------------------------------------------------

function aibHorizon(doc: AibDoc): CaseBrief['time_horizon'] {
  return {
    label: `Immediate re-sourcing on ${tradeWhen(doc.meta)} trade`,
    note: `${doc.meta.tradeNote ?? `Trade year ${doc.meta.tradeYear}.`} Build times for new capacity are not modelled anywhere in the Explorer.`,
  };
}

function aibScenario(scenario: string): CaseBrief['scenario'] {
  if (scenario === 'managed') return { key: 'managed', label: SCENARIO_NAME.managed, description: SCENARIO_DEFINITION.managed };
  if (scenario === 'baseline') return { key: 'baseline', label: SCENARIO_NAME.baseline, description: SCENARIO_DEFINITION.baseline };
  return { key: 'decoupling', label: SCENARIO_NAME.decoupling, description: `${SCENARIO_DEFINITION.decoupling} China keeps buying from allied economies: the cut is one-way.` };
}

const suppliersLine = (sup: readonly [string, number | null][] | undefined, name: (i: string) => string, n = 5): string =>
  (sup ?? []).slice(0, n).map(([iso, s]) => `${name(iso)} ${pct(nn(s))}`).join('; ');

/** A node flag that is not about production, in plain words (customs codes spelled out). */
function dataFlagWords(f: string): string {
  return f
    .replace(/\bHS6\b/g, '6-digit customs')
    .replace(/\bHS(\d{4})\b/g, 'the $1 customs classification')
    .replace(/\bHS\b/g, 'customs')
    .replace(/^./, (x) => x.toUpperCase());
}

function nodeWarnings(n: AibNode, name: (i: string) => string, scenario: string): BriefWarning[] {
  const w: BriefWarning[] = [];
  if (!n.physicalData || n.physicalData === 'none') w.push(warn('no_production_data', 'Production evidence limited', 'Supply is read from trade only: there are no production data for this product.'));
  for (const f of n.flags ?? []) {
    if (/production more concentrated than trade/i.test(f)) w.push(warn('production_divergence', 'Trade and production evidence disagree', flagWords(f, name)));
    else w.push(warn('data_flag', 'Data note', dataFlagWords(f), 'info'));
  }
  if (n.quality === 'Usable with caution') w.push(warn('quality_caution', 'Data quality: usable with caution', 'The trade data for this product carry the caveats above.', 'info'));
  if (scenario === 'managed' && n.diversify?.status === 'already as diversified as its benchmark')
    w.push(warn('not_stressed', 'Already as diversified as comparable products', 'Under this scenario nothing has to shift for this product.', 'info'));
  return w;
}

// ---- AIB nodes (Sector review, one product) ------------------------------------------------------------------------

function aibNodeBrief(def: Pick<CaseDefinition, 'kind' | 'key' | 'brief'>, b: DecodedBundles, name: (i: string | null | undefined) => string): CaseBrief {
  const doc = b.aib?.doc;
  const scenario = def.key.scenario ?? 'decoupling';
  const n = doc?.nodes.find((x) => x.id === def.key.node);
  const label = n?.label ?? def.key.node ?? 'product';
  const stage = n && isSupplyStage(n.stage) ? n.stage : null;
  const scen = aibScenario(scenario);
  const parts: BriefIdentityPart[] = [
    { role: 'node', label, code: def.key.node ?? null },
    ...(stage ? [{ role: 'stage' as const, label: STAGE_DISPLAY[stage], code: stage }] : []),
    { role: 'scenario', label: scen!.label, code: scenario },
  ];
  const facts = new Map<string, BriefFact>();
  const tw = doc ? tradeWhen(doc.meta) : 'latest';
  const bl = n?.baseline ?? null;
  facts.set('allied_imports', bf({ id: 'allied_imports', label: `Allied economies’ imports of ${label.toLowerCase()}`, value: nn(n?.demandUsd), display: `${usd(nn(n?.demandUsd))} a year`, interpretation: '59 allied economies', source: SOURCE.aibTrade, vintage: tw }));
  if (scenario === 'decoupling') {
    const ex = nn(n?.exposure);
    facts.set('china_share_allied', bf({ id: 'china_share_allied', label: 'China’s share of supply to allied economies (with Hong Kong and Macao re-exports)', value: ex, display: pct(ex), interpretation: supplierShareWords(ex), source: SOURCE.aibTrade, vintage: tw }));
    facts.set('supply_at_risk', bf({ id: 'supply_at_risk', label: 'Supply lost if China stops supplying', value: nn(n?.lostUsd), display: `${usd(nn(n?.lostUsd))} a year`, interpretation: null, source: SOURCE.aibTrade, vintage: tw }));
  } else {
    const dv = n?.diversify;
    const ex = nn(dv?.excess);
    facts.set('shift_share', bf({
      id: 'shift_share', label: 'Share of allied demand that has to move to other suppliers under the scenario', value: ex,
      display: ex === null ? 'not measured' : `${pct(ex)}${dv?.cap !== null && dv?.cap !== undefined ? `: ${(dv.capped ?? []).map((i) => name(i)).join(', ') || 'no supplier'} above a cap of ${pct(dv.cap)} of allied demand` : ''}`,
      interpretation: 'the scenario setting, not an observation', source: `${SOURCE.aibTrade}; Harvard Atlas (comparable products)`, vintage: tw, estimate: true,
    }));
    facts.set('supply_at_risk', bf({ id: 'supply_at_risk', label: 'Supply that has to shift under the cap', value: nn(dv?.lostUsd), display: `${usd(nn(dv?.lostUsd))} a year`, interpretation: null, source: SOURCE.aibTrade, vintage: tw, estimate: true }));
    facts.set('concentration_benchmark', bf({
      id: 'concentration_benchmark', label: 'World export concentration (HHI), against comparable products', value: nn(dv?.currentHhi),
      display: `${nn(dv?.currentHhi)?.toFixed(3) ?? 'not measured'} against ${nn(dv?.benchmarkHhi)?.toFixed(3) ?? 'not measured'} (${dv?.basis ?? 'benchmark not stated'})`,
      interpretation: `this product: ${hhiWords(nn(dv?.currentHhi)) ?? 'not measured'}; comparable products: ${hhiWords(nn(dv?.benchmarkHhi)) ?? 'not measured'}`, source: `Harvard Atlas of Economic Complexity; ${HHI_SOURCE}`, vintage: 'latest Atlas year',
    }));
  }
  facts.set('top_suppliers', bf({ id: 'top_suppliers', label: 'Largest suppliers to allied economies today', value: suppliersLine(bl?.suppliers, name) || null, display: suppliersLine(bl?.suppliers, name) || 'not measured', interpretation: 'share of allied imports', source: SOURCE.aibTrade, vintage: tw }));
  facts.set('concentration_today', bf({ id: 'concentration_today', label: 'Concentration (HHI) of supply to allied economies today', value: nn(bl?.hhi), display: nn(bl?.hhi)?.toFixed(3) ?? 'not measured', interpretation: hhiWords(nn(bl?.hhi)), source: `${SOURCE.aibTrade}; bands: ${HHI_SOURCE}`, vintage: tw }));
  const t3 = nn(bl?.top3Share);
  facts.set('top3_today', bf({ id: 'top3_today', label: 'Share of the three largest suppliers today', value: t3, display: pct(t3), interpretation: t3 === null ? null : t3 >= 0.75 ? 'three suppliers hold three-quarters or more' : 'three suppliers hold less than three-quarters', source: SOURCE.aibTrade, vintage: tw }));
  // Coverage counts world production; exporters outside the production data are held at today's shipments (the
  // bundle's validation note says how much world trade that is), which the coverage word alone hides.
  const absent = /exporters with ([0-9.]+%) of world trade are absent from the physical data/.exec(n?.physicalValidation ?? '');
  facts.set('production_data', bf({ id: 'production_data', label: 'Production data for this product', value: n?.physicalData ?? 'none', display: physicalDataWords(n?.physicalData),
    interpretation: absent ? `exporters holding ${absent[1]} of world trade are not in the production data; the analysis holds them at their current shipments` : null, source: 'Production-capacity data used by the Explorer (International Energy Agency and capacity databases)', vintage: isoDate(doc?.meta.built) ?? 'as built' }));
  const q = n?.quality ?? null;
  facts.set('data_quality', bf({ id: 'data_quality', label: 'Trade data quality', value: q, display: q === 'Insufficient for optimization' ? 'Too thin for a full analysis' : (q ?? 'not graded'), interpretation: null, source: 'Explorer data checks', vintage: tw }));
  const defaults = scenario === 'decoupling'
    ? ['allied_imports', 'china_share_allied', 'supply_at_risk', 'top_suppliers', 'concentration_today', 'top3_today', 'production_data', 'data_quality']
    : ['allied_imports', 'shift_share', 'supply_at_risk', 'concentration_benchmark', 'top_suppliers', 'concentration_today', 'production_data', 'data_quality'];
  const warnings = n ? nodeWarnings(n, name, scenario) : [warn('not_in_release', 'Not in this release', `${def.key.node} is not in the AIB data.`)];
  if (doc?.meta.tradeNote) warnings.push(warn('trade_period', 'Trade period', doc.meta.tradeNote, 'info'));
  const a = def.brief;
  const question = scenario === 'managed'
    ? `If no supplier could hold more of the allied market for ${label.toLowerCase()} than is normal for comparable products, what kind of allied response would be needed: new production capacity, coordinated trade, managed reliance on a few suppliers with backup, or little intervention?`
    : `If China stopped supplying ${label.toLowerCase()} to the 59 allied economies (while still buying from them), what kind of allied response would be needed: new production capacity, coordinated trade, managed reliance on a few suppliers with backup, or little intervention?`;
  return {
    product: REVIEW_PRODUCT_OF_KIND[def.kind],
    identity: { title: [label, stage ? STAGE_DISPLAY[stage] : null, scen!.label].filter(Boolean).join(' × '), parts },
    decision_question: a?.decision_question ?? question,
    decision_context: a?.decision_context ?? 'This approximates a joint decision by allied economic-security and trade officials about one product group. It asks whether existing trade could absorb a supply shock, or whether coordination or new capacity is needed. It looks at traded supply only and is immediate: it does not model how long new plants take, prices or firm-level constraints.',
    objective: a?.objective ?? OBJECTIVE.aib,
    time_horizon: a?.time_horizon ? { label: a.time_horizon, note: doc ? aibHorizon(doc).note : null } : doc ? aibHorizon(doc) : { label: 'Immediate re-sourcing', note: null },
    scenario: scen,
    stage_location: stageStrip(stage ? [stage] : []),
    evidence: pickFacts(facts, a?.evidence_ids, defaults),
    assumptions: pickAssumptions(a?.assumption_ids, scenario === 'managed' ? DEFAULT_ASSUMPTIONS.aib_managed : DEFAULT_ASSUMPTIONS.aib_decoupling),
    warnings,
    missing_evidence: [
      'The customs codes behind this product group are not listed in this release.',
      'Build times for new capacity are not modelled anywhere in the Explorer.',
      'Firm-level capacity, qualification lead times and spare capacity are not captured.',
      ...(!n?.physicalData || n.physicalData === 'none' ? ['Production outside trade is not measured for this product.'] : []),
    ],
    definitions: [
      { term: 'Allied economies', text: 'The 59 economies of the coalition core and its wider allied tier whose demand the analysis protects.' },
      { term: 'Concentration (HHI)', text: 'Herfindahl-Hirschman Index: the sum of squared supplier shares, from near 0 (many small suppliers) to 1 (one supplier).' },
      ...(scenario === 'managed' ? [{ term: 'Comparable products', text: 'The other 6-digit products of the same 4-digit customs heading, used as the benchmark for normal concentration.' }] : []),
    ],
    partnership: null,
    compact_chain: null,
    sector_chain: null,
  };
}

// ---- compacts and sector chains -------------------------------------------------------------------------------------

function chainFacts(an: ChainAnalysis, doc: AibDoc, name: (i: string | null | undefined) => string): Map<string, BriefFact> {
  const rec: AibStrategicCompactScenario | null = an.rec;
  const tw = tradeWhen(doc.meta);
  const dec = an.scenario === 'decoupling';
  const f = new Map<string, BriefFact>();
  const steps = an.compact.steps.map((s) => `${s.label}${s.description ? ` (${s.description})` : ''}`).join('; ');
  f.set('chain_steps', bf({ id: 'chain_steps', label: 'What the chain makes, step by step', value: an.compact.steps.length, display: steps, interpretation: `${an.nodes.length} products in ${an.compact.steps.length} steps`, source: 'Explorer strategic compact definition (curated steps)', vintage: isoDate(doc.meta.built) ?? 'as built' }));
  f.set('chain_imports', bf({ id: 'chain_imports', label: 'Allied economies’ imports across the chain', value: nn(rec?.demandUsd), display: `${usd(nn(rec?.demandUsd))} a year`, interpretation: '59 allied economies', source: SOURCE.aibTrade, vintage: tw }));
  f.set('supply_at_risk', bf({
    id: 'supply_at_risk', label: dec ? 'Supply lost if China stops supplying' : 'Supply that has to shift under the cap', value: nn(rec?.lostUsd),
    display: `${usd(nn(rec?.lostUsd))} a year (${pct(nn(rec?.exposure))} of the chain’s allied imports)`, interpretation: null, source: SOURCE.aibTrade, vintage: tw, estimate: !dec,
  }));
  const bySteps = an.compact.steps.map((s) => {
    const ns = an.nodes.filter((n) => n.stepOrder === s.order);
    const big = [...ns].sort((x, y) => (y.demandUsd ?? 0) - (x.demandUsd ?? 0))[0];
    if (!big) return null;
    const share = dec ? big.chinaShare : big.largestShare;
    const who = dec ? 'China' : name(big.largest);
    return `${s.label}: ${who} ${pct(share)} of ${big.label.toLowerCase()}`;
  }).filter(Boolean).join('; ');
  f.set('step_suppliers', bf({ id: 'step_suppliers', label: dec ? 'China’s share of allied supply, largest product of each step' : 'Largest supplier’s share of allied supply, largest product of each step', value: bySteps || null, display: bySteps || 'not measured', interpretation: 'before the stress', source: SOURCE.aibTrade, vintage: tw }));
  const ex = nn(rec?.coverage.existing);
  f.set('existing_trade_cover', bf({ id: 'existing_trade_cover', label: 'Share of allied demand existing trade links could cover after the stress (estimate)', value: ex, display: pct(ex), interpretation: 'a model estimate: current suppliers expand their existing links, each at most doubling', source: 'Explorer allied stress test', vintage: tw, estimate: true }));
  f.set('gap_after_existing', bf({ id: 'gap_after_existing', label: 'Gap left after existing trade links (estimate)', value: ex === null ? null : 1 - ex, display: ex === null ? 'not measured' : `${pct(1 - ex)} of allied demand (${usd(nn(rec?.demandUsd) !== null ? (1 - ex) * (rec!.demandUsd as number) : null)} a year)`, interpretation: 'a model estimate', source: 'Explorer allied stress test', vintage: tw, estimate: true }));
  const k = rec?.counts;
  const untested = k ? k.insufficient + (k.notTested ?? 0) : null;
  f.set('untested_steps', bf({ id: 'untested_steps', label: 'Products the analysis could not test', value: untested, display: untested === null ? 'not measured' : `${untested} of ${an.nodes.length}`, interpretation: untested ? 'too little trade data for these products' : 'every product could be tested', source: 'Explorer data checks', vintage: tw }));
  const noPhys = an.nodes.filter((n) => !n.physicalData || n.physicalData === 'none');
  f.set('production_data_gaps', bf({ id: 'production_data_gaps', label: 'Products without production data (trade only)', value: noPhys.length, display: `${noPhys.length} of ${an.nodes.length}${noPhys.length ? `: ${noPhys.map((n) => n.label).join(', ')}` : ''}`, interpretation: null, source: 'Production-capacity data used by the Explorer', vintage: isoDate(doc.meta.built) ?? 'as built' }));
  const div = an.nodes.filter((n) => n.productionFlags.length);
  f.set('production_divergence', bf({ id: 'production_divergence', label: 'Products where production is more concentrated than trade', value: div.length, display: div.length ? div.map((n) => `${n.label}: ${flagWords(n.productionFlags[0], (i) => name(i))}`).join(' ') : 'none flagged', interpretation: null, source: 'Production-capacity data used by the Explorer', vintage: isoDate(doc.meta.built) ?? 'as built' }));
  return f;
}

function chainWarnings(an: ChainAnalysis, doc: AibDoc, name: (i: string) => string): BriefWarning[] {
  const w: BriefWarning[] = [];
  const noPhys = an.nodes.filter((n) => !n.physicalData || n.physicalData === 'none').length;
  if (noPhys) w.push(warn('no_production_data', 'Production evidence limited', `${noPhys} of ${an.nodes.length} products have no production data: their supply is read from trade only.`));
  for (const n of an.nodes) for (const f of n.productionFlags) w.push(warn('production_divergence', 'Trade and production evidence disagree', `${n.label}: ${flagWords(f, name)}`));
  const k = an.rec?.counts;
  if (k && k.notStressed && an.scenario === 'managed') w.push(warn('not_stressed', 'Some products are not stressed', `${k.notStressed} of ${an.nodes.length} products are already as diversified as comparable products, so nothing shifts there under this scenario.`, 'info'));
  const untested = k ? k.insufficient + (k.notTested ?? 0) : 0;
  if (untested) w.push(warn('untested', 'Untested products', `${untested} ${untested === 1 ? 'product has' : 'products have'} too little trade data to test.`));
  if (doc.meta.tradeNote) w.push(warn('trade_period', 'Trade period', doc.meta.tradeNote, 'info'));
  return w;
}

function chainSteps(an: ChainAnalysis, name: (i: string | null | undefined) => string): ChainStep[] {
  const rec = an.rec;
  const dec = an.scenario === 'decoupling';
  const ex = nn(rec?.coverage.existing);
  const dem = nn(rec?.demandUsd);
  const top = [...an.nodes].sort((x, y) => (y.lostUsd ?? 0) - (x.lostUsd ?? 0)).slice(0, 3);
  return [
    {
      id: 'problem_today', label: 'Problem today', estimate: !dec,
      display: `${usd(nn(rec?.lostUsd))} of ${usd(dem)} a year (${pct(nn(rec?.exposure))})`,
      text: `${dec ? 'If China stopped supplying, allied economies would lose this much of the chain’s imports.' : 'Under the cap, this much of the chain’s allied supply has to move to other suppliers.'} Largest amounts: ${top.map((n) => `${n.label} ${usd(n.lostUsd)}${dec && n.chinaShare !== null ? ` (China ${pct(n.chinaShare)} of allied supply)` : n.largest ? ` (${name(n.largest)} ${pct(n.largestShare)})` : ''}`).join('; ')}.`,
    },
    {
      id: 'existing_trade', label: 'What existing trade can cover', estimate: true, display: pct(ex),
      text: 'A model estimate: current suppliers expand their existing links to allied buyers, each at most doubling, before any new trade relationship or new capacity.',
    },
    {
      id: 'gap_after_existing', label: 'Remaining gap after existing links', estimate: true,
      display: ex === null ? 'not measured' : `${pct(1 - ex)} of allied demand (${usd(dem !== null ? (1 - ex) * dem : null)} a year)`,
      text: 'What existing links leave uncovered; new trade relationships or new capacity would have to close it.',
    },
  ];
}

function chainBrief(def: Pick<CaseDefinition, 'kind' | 'key' | 'brief'>, b: DecodedBundles, name: (i: string | null | undefined) => string): CaseBrief {
  const doc = b.aib?.doc;
  const scenario = def.key.scenario ?? 'decoupling';
  const an = doc && def.key.sid ? chainAnalysis(doc, def.key.sid, scenario) : null;
  const scen = aibScenario(scenario);
  const sectorChain = def.kind === 'sector_chain';
  const cname = an?.compact.name ?? def.key.sid ?? 'chain';
  const facts = an && doc ? chainFacts(an, doc, name) : new Map<string, BriefFact>();
  const stages = an ? SUPPLY_STAGES.filter((s) => an.nodes.some((n) => n.stage === s)) : [];
  const a = def.brief;
  const defaults = sectorChain
    ? ['chain_steps', 'chain_imports', 'supply_at_risk', 'existing_trade_cover', 'gap_after_existing', 'production_data_gaps', 'production_divergence']
    : ['chain_steps', 'chain_imports', 'supply_at_risk', 'step_suppliers', 'existing_trade_cover', 'gap_after_existing', 'untested_steps', 'production_data_gaps'];
  const dec = scenario === 'decoupling';
  const perStage = an
    ? SUPPLY_STAGES.map((s): NonNullable<CaseBrief['sector_chain']>[number] => {
        const ns = an.nodes.filter((n) => n.stage === s);
        const tw = doc ? tradeWhen(doc.meta) : 'latest';
        if (!ns.length) return { stage: s, label: STAGE_DISPLAY[s], facts: [] };
        const imp = ns.reduce((x, n) => x + (n.demandUsd ?? 0), 0);
        const lost = ns.reduce((x, n) => x + (n.lostUsd ?? 0), 0);
        const out: BriefFact[] = [
          bf({ id: `${s}_products`, label: 'Products of the chain at this stage', value: ns.length, display: ns.map((n) => n.label).join('; '), interpretation: null, source: 'Explorer strategic compact definition', vintage: isoDate(doc?.meta.built) ?? 'as built' }),
          bf({ id: `${s}_imports`, label: 'Allied imports', value: imp, display: `${usd(imp)} a year`, interpretation: null, source: SOURCE.aibTrade, vintage: tw }),
          bf({ id: `${s}_at_risk`, label: dec ? 'Supply lost if China stops supplying' : 'Supply that has to shift under the cap', value: lost, display: `${usd(lost)} a year (${pct(imp > 0 ? lost / imp : null)} of the stage’s allied imports)`, interpretation: null, source: SOURCE.aibTrade, vintage: tw, estimate: !dec }),
          bf({
            id: `${s}_suppliers`, label: dec ? 'China’s share of allied supply, by product' : 'Largest supplier’s share of allied supply, by product', value: ns.length,
            display: ns.map((n) => (dec ? `${n.label}: China ${pct(n.chinaShare)}` : `${n.label}: ${name(n.largest)} ${pct(n.largestShare)}`)).join('; '), interpretation: 'before the stress', source: SOURCE.aibTrade, vintage: tw,
          }),
          bf({
            id: `${s}_production`, label: 'Production data', value: ns.filter((n) => n.physicalData && n.physicalData !== 'none').length,
            display: ns.map((n) => `${n.label}: ${!n.physicalData || n.physicalData === 'none' ? 'trade only' : `production data ${n.physicalData}`}`).join('; '), interpretation: null, source: 'Production-capacity data used by the Explorer', vintage: isoDate(doc?.meta.built) ?? 'as built',
            warnings: ns.flatMap((n: ChainNode) => n.productionFlags.map((f) => warn('production_divergence', 'Trade and production evidence disagree', `${n.label}: ${flagWords(f, (i) => name(i))}`))),
          }),
        ];
        return { stage: s, label: STAGE_DISPLAY[s], facts: out };
      })
    : null;
  const question = sectorChain
    ? `Under ${scen!.label.toLowerCase() === 'managed interdependence' ? 'Managed Interdependence' : `a ${scen!.label.toLowerCase()}`}, at which stages of the ${cname.toLowerCase()} chain is allied intervention warranted? Select every stage that needs it, then the one you would act on first.`
    : `Under ${scenario === 'managed' ? 'Managed Interdependence' : `a ${scen!.label.toLowerCase()}`}, do the allied economies need a standing multi-country arrangement across the whole ${cname.toLowerCase()} chain, joint action on a few steps only, or no joint arrangement?`;
  const context = sectorChain
    ? 'This approximates a choice by allied economic-security officials about where in one supply chain to concentrate joint effort. Products are grouped into the chain’s six stages; the question is where existing trade cannot absorb the stress, not where the largest market is. The analysis looks at traded supply only and does not model build times.'
    : 'This approximates a decision by a group of allied governments on whether to set up a standing multi-country arrangement (coordinated investment, long-term offtake, stockpiles, common trade rules) for a whole supply chain. Such an arrangement is meant to be warranted only where joint action materially changes resilience compared with what countries would do alone. The analysis does not cost the arrangement or test its politics.';
  return {
    product: REVIEW_PRODUCT_OF_KIND[def.kind],
    identity: { title: `${cname} × ${scen!.label}`, parts: [{ role: sectorChain ? 'chain' : 'compact', label: cname, code: def.key.sid ?? null }, { role: 'scenario', label: scen!.label, code: scenario }] },
    decision_question: a?.decision_question ?? question,
    decision_context: a?.decision_context ?? context,
    objective: a?.objective ?? OBJECTIVE.chain,
    time_horizon: a?.time_horizon ? { label: a.time_horizon, note: doc ? aibHorizon(doc).note : null } : doc ? aibHorizon(doc) : { label: 'Immediate re-sourcing', note: null },
    scenario: scen,
    stage_location: stageStrip(stages),
    evidence: pickFacts(facts, a?.evidence_ids, defaults),
    assumptions: pickAssumptions(a?.assumption_ids, scenario === 'managed' ? DEFAULT_ASSUMPTIONS.chain_managed : DEFAULT_ASSUMPTIONS.chain_decoupling),
    warnings: an && doc ? chainWarnings(an, doc, name) : [warn('not_in_release', 'Not in this release', `${def.key.sid} is not in the AIB data.`)],
    missing_evidence: [
      'Build times for new capacity are not modelled anywhere in the Explorer.',
      'Concentration of the whole chain before and after joint action is not exported.',
      'Firm-level capacity, qualification lead times and project pipelines are not captured.',
      'The cost of a standing arrangement and its politics are not assessed.',
    ],
    definitions: [
      { term: 'Allied economies', text: 'The 59 economies of the coalition core and its wider allied tier whose demand the analysis protects.' },
      { term: 'Existing trade links', text: 'Supplier-buyer pairs that already trade the product; after a shock each may grow to at most twice its size.' },
      { term: 'Standing multi-country arrangement', text: 'A lasting agreement among several governments covering investment, offtake, stockpiles and trade rules for a supply chain.' },
    ],
    partnership: null,
    compact_chain: sectorChain || !an ? null : chainSteps(an, name),
    sector_chain: sectorChain ? perStage : null,
  };
}

// ---- partnerships -----------------------------------------------------------------------------------------------------

interface CellView {
  key: string;
  phrase: string;
  home: number | null;
  bench: number | null;
  rca: number | null;
  top: string | null;
  topShare: number | null;
  rival: number | null;
  /** gross imports of the step as a share of GDP: the scale that orders exposures */
  gdp: number | null;
}

function cellViews(b: DecodedBundles, iso: string): CellView[] {
  const p = b.posture;
  const cells = p?.details[iso]?.cells ?? [];
  return cells
    .filter((c) => c.stage !== DEPLOYMENT_STAGE)
    .map((c) => ({
      key: `${c.tech}|${c.stage}`,
      phrase: `${p?.core.meta.phrases[`${c.tech}|${c.stage}`] ?? `${c.tech} ${c.stage}`} (${isSupplyStage(c.stage) ? STAGE_DISPLAY[c.stage] : c.stage})`,
      home: c.homeShare,
      bench: nn(c.trade?.benchmark_share),
      rca: nn(c.trade?.benchmark_rca),
      top: c.supplyChain?.topSupplier ?? null,
      topShare: c.supplyChain?.topSupplierShare ?? null,
      rival: c.supplyChain?.coerciveImportShare ?? null,
      gdp: c.supplyChain?.importsToGdp ?? null,
    }));
}
const brings = (c: CellView) => (c.home !== null && c.home >= 0.05) || (c.bench !== null && c.bench >= 0.05 && (c.rca ?? 0) > 1);
const exposed = (c: CellView) => ((c.topShare ?? 0) >= 0.5 || (c.rival ?? 0) >= 0.5) && (c.home ?? 0) < 0.1;
const bringWords = (c: CellView) => `${c.phrase}: ${pct(c.home)} of world supply${c.bench !== null ? `, ${pct(c.bench)} of world exports` : ''}`;
const exposedWords = (c: CellView, name: (i: string | null) => string) =>
  `${c.phrase}: ${name(c.top)} supplies ${pct(c.topShare)} of imports${(c.rival ?? 0) >= 0.5 ? `; ${pct(c.rival)} from China, Russia and the other UN-voting group` : ''}`;

function partnershipBrief(def: Pick<CaseDefinition, 'kind' | 'key' | 'brief'>, b: DecodedBundles, name: (i: string | null | undefined) => string): CaseBrief {
  const pair = def.key.pair ?? '';
  const entry = b.partnerships?.manifest.pairs[pair] ?? null;
  const [isoA, isoB] = entry?.country_a && entry?.country_b ? [entry.country_a, entry.country_b] : pair.split('_');
  const A = name(isoA);
  const B = name(isoB);
  const ca = cellViews(b, isoA);
  const cb = cellViews(b, isoB);
  const byKey = (xs: CellView[]) => new Map(xs.map((x) => [x.key, x]));
  const ma = byKey(ca);
  const mb = byKey(cb);
  const sortHome = (xs: CellView[]) => [...xs].sort((x, y) => (y.home ?? 0) - (x.home ?? 0));
  // exposures ordered by import scale (a 100% share of a tiny import is not a material exposure)
  const sortTop = (xs: CellView[]) => [...xs].sort((x, y) => (y.gdp ?? 0) - (x.gdp ?? 0) || (y.topShare ?? 0) - (x.topShare ?? 0));
  const side = (iso: string, nm: string, xs: CellView[]): PartnerSide => ({
    iso3: iso,
    name: nm,
    brings: sortHome(xs.filter(brings)).slice(0, 5).map(bringWords),
    exposed_on: sortTop(xs.filter(exposed)).slice(0, 5).map((c) => exposedWords(c, name)),
  });
  const aToB = sortHome(ca.filter((c) => brings(c) && mb.get(c.key) && exposed(mb.get(c.key)!)));
  const bToA = sortHome(cb.filter((c) => brings(c) && ma.get(c.key) && exposed(ma.get(c.key)!)));
  const sharedExp = sortTop(ca.filter((c) => exposed(c) && mb.get(c.key) && exposed(mb.get(c.key)!)));
  const bothStrong = sortHome(ca.filter((c) => brings(c) && mb.get(c.key) && brings(mb.get(c.key)!)));
  // allied-market products where China holds half or more and both are sizeable buyers (division tuples: [iso, node, ws, ds, rank, flags])
  const doc = b.aib?.doc;
  const ds = new Map<string, number>();
  for (const sc of doc?.strategic?.compacts ?? []) for (const t of sc.division) ds.set(`${t[0]}|${t[1]}`, nn(t[3]) ?? 0);
  const shared: string[] = [];
  for (const n of doc?.nodes ?? []) {
    const chn = (n.baseline?.suppliers ?? []).find(([i]) => i === 'CHN');
    const s = chn ? nn(chn[1]) : null;
    const da = ds.get(`${isoA}|${n.id}`) ?? 0;
    const db = ds.get(`${isoB}|${n.id}`) ?? 0;
    if (s !== null && s >= 0.5 && da >= 0.05 && db >= 0.05) shared.push(`${n.label}: China ${pct(s)} of allied supply; ${A} ${pct(da)} and ${B} ${pct(db)} of allied demand`);
  }
  // what makes each side dependent: its largest supplier, plus the rival-group share when that is what crosses half
  const depOf = (c: CellView) => `${name(c.top)} ${pct(c.topShare)}${(c.topShare ?? 0) < 0.5 && (c.rival ?? 0) >= 0.5 ? `, China, Russia and the other UN-voting group ${pct(c.rival)}` : ''}`;
  const shareProblem = [...shared, ...sharedExp.slice(0, 3).map((c) => `${c.phrase}: both depend on one supplier, or on China, Russia and the other UN-voting group, for half or more of imports (${A}: ${depOf(c)}; ${B}: ${depOf(mb.get(c.key)!)})`)];
  const pv = b.posture?.core.meta;
  const postureVintage = `trade 2024 and latest 12 months; exports ${pv?.benchmark_period ?? '2022-2024'}`;
  const list = (xs: CellView[], words: (c: CellView) => string) => (xs.length ? xs.slice(0, 4).map(words).join('; ') : 'none');
  const facts = new Map<string, BriefFact>();
  const rule = 'Large supplier: 5% or more of world supply, or of world exports with export specialisation. Depends on one supplier: one supplier, or China, Russia and the other UN-voting group together, provide half or more of imports, and the economy holds under 10% of world supply.';
  facts.set('a_strengths_b_needs', bf({ id: 'a_strengths_b_needs', label: `Steps where ${A} is a large supplier and ${B} depends on one supplier`, value: aToB.length, display: `${aToB.length}: ${list(aToB, (c) => c.phrase)}`, interpretation: rule, source: SOURCE.posture, vintage: postureVintage }));
  facts.set('b_strengths_a_needs', bf({ id: 'b_strengths_a_needs', label: `Steps where ${B} is a large supplier and ${A} depends on one supplier`, value: bToA.length, display: `${bToA.length}: ${list(bToA, (c) => c.phrase)}`, interpretation: rule, source: SOURCE.posture, vintage: postureVintage }));
  facts.set('shared_exposure', bf({ id: 'shared_exposure', label: 'Steps where both depend on one supplier', value: sharedExp.length, display: `${sharedExp.length}: ${list(sharedExp, (c) => c.phrase)}`, interpretation: 'a shared vulnerability neither side can fill alone', source: SOURCE.posture, vintage: postureVintage }));
  facts.set('both_strong', bf({ id: 'both_strong', label: 'Steps where both are large suppliers', value: bothStrong.length, display: `${bothStrong.length}: ${list(bothStrong, (c) => c.phrase)}`, interpretation: 'two large suppliers of the same step usually compete there', source: SOURCE.posture, vintage: postureVintage }));
  facts.set('shared_allied_problem', bf({ id: 'shared_allied_problem', label: 'Allied-market products where China holds half or more of supply and both buy 5% or more of allied demand', value: shared.length, display: shared.length ? shared.join('; ') : 'none', interpretation: null, source: SOURCE.aibTrade, vintage: doc ? tradeWhen(doc.meta) : 'latest' }));
  facts.set('memo_status', bf({ id: 'memo_status', label: 'The memo behind this case', value: entry?.verified === true ? 'verified' : 'unverified', display: `${entry?.verified === true ? 'Independently verified' : 'Rule-generated, not independently verified'}; data as of ${entry?.as_of ?? 'not stated'}`, interpretation: null, source: SOURCE.memo, vintage: entry?.as_of ?? null }));
  const warnings: BriefWarning[] = [];
  if (!entry) warnings.push(warn('not_in_release', 'Not in this release', `The ${pair} memo is not in the partnership data.`));
  if (entry && entry.verified !== true) warnings.push(warn('memo_unverified', 'Rule-generated memo', 'The memo was generated by rules from the Explorer’s data and has not been independently verified.'));
  const created = isoDate(b.posture?.manifest && typeof b.posture.manifest.created === 'string' ? b.posture.manifest.created : b.posture?.core.created);
  if (entry?.as_of && created && entry.as_of < created) warnings.push(warn('memo_older', 'Memo older than the data', `The memo’s data are as of ${entry.as_of}; the Explorer’s data were rebuilt on ${created}, so figures in the memo can differ from those here.`));
  warnings.push(warn('older_groups', 'Older source', 'Supplier groups are based on UN General Assembly votes up to 2023.', 'info'));
  const a = def.brief;
  return {
    product: REVIEW_PRODUCT_OF_KIND[def.kind],
    identity: { title: `${A} × ${B}`, parts: [{ role: 'pair', label: `${A} × ${B}`, code: pair }, { role: 'country', label: A, code: isoA }, { role: 'country', label: B, code: isoB }] },
    decision_question: a?.decision_question ?? `What should lead cooperation between ${A} and ${B} on clean-energy supply chains over the next 12 to 36 months?`,
    decision_context: a?.decision_context ?? `This approximates the agenda of a bilateral economic-security dialogue. The aim is joint resilience and industrial value where each side’s strengths meet the other’s needs; diplomatic closeness alone does not count, and two strong competitors are not complementary.`,
    objective: a?.objective ?? OBJECTIVE.partnership,
    time_horizon: a?.time_horizon ? { label: a.time_horizon, note: null } : { label: '12 to 36 months (the memo’s deal horizon)', note: `Memo data as of ${entry?.as_of ?? 'not stated'}${created ? `; Explorer data rebuilt ${created}` : ''}.` },
    scenario: { key: 'pair', label: 'Bilateral cooperation', description: 'The memo reads the pair against today’s trade and the allied stress test in which China stops supplying allied economies.' },
    stage_location: null,
    evidence: pickFacts(facts, a?.evidence_ids, ['a_strengths_b_needs', 'b_strengths_a_needs', 'shared_exposure', 'both_strong', 'shared_allied_problem', 'memo_status']),
    assumptions: pickAssumptions(a?.assumption_ids, DEFAULT_ASSUMPTIONS.partnership),
    warnings,
    missing_evidence: [
      'Bilateral trade by product between the two economies is not in this release.',
      'The memo’s evidence register is not shipped, so its claims cannot be traced here.',
      'Firm-level commitments, investment pipelines and the incentives of each side are not captured.',
    ],
    definitions: [
      { term: 'Large supplier', text: 'An economy with 5% or more of world supply of a step, or of world exports with export specialisation.' },
      { term: 'Depends on one supplier', text: 'One supplier, or China, Russia and the other UN-voting group together, provide half or more of the economy’s imports of the step.' },
      { term: 'Allied demand', text: 'Imports of the 59 economies of the coalition core and its wider allied tier.' },
    ],
    partnership: { a: side(isoA, A, ca), b: side(isoB, B, cb), shared_problem: shareProblem },
    compact_chain: null,
    sector_chain: null,
  };
}

// ---- entry point ---------------------------------------------------------------------------------------------------

/**
 * The neutral case brief of a case (shown before the blind answer). `snapshot` is accepted for symmetry with the
 * reasoning builder; nothing of its conclusion is read.
 */
export function buildCaseBrief(def: Pick<CaseDefinition, 'kind' | 'key' | 'brief' | 'title'>, b: DecodedBundles, _snapshot?: CaseSnapshot | null): CaseBrief {
  const name = countryNames(b);
  switch (def.kind) {
    case 'posture_cell':
    case 'sector':
      return postureBrief({ def, b, name });
    case 'aib_node':
      return aibNodeBrief(def, b, name);
    case 'compact':
    case 'sector_chain':
      return chainBrief(def, b, name);
    case 'partnership':
      return partnershipBrief(def, b, name);
  }
}

/** Re-exported for tests: the annual-rate convention. */
export { annualRate };
