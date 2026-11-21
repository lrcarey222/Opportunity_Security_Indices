// The case snapshot builder (docs/QC_SYSTEM_DESIGN.md 4.2): the exact values a reviewer sees for one case,
// read from the decoded shipped bundles. Facts are keyed by the shared fact ids of the case kind (a fact the
// bundle does not carry is written with value null and a note, never dropped); the conclusion is in canonical
// keys with canonical labels (qc/answerOptions.ts); the posture decision trace, the AIB / compact threat metrics
// (qc/threat.ts), the bundle-layer robustness record (qc/sensitivity.ts) and data-quality notes are kept as
// separate fields, never merged into one score. Pure: no IO. canonicalJson() gives the stable text the
// snapshot hash is taken over.

import type { AibDoc, AibNode, AibStrategicCompactScenario } from '../../aib/contract.ts';
import { ALIGNMENT_DEPLOYMENT_GAP, ALIGNMENT_NAME, CHOICE_NAME, FENCE_NAME } from '../../content/vocabulary.ts';
import type { MemoDoc } from '../../country/contract.ts';
import { DEPLOYMENT_STAGE } from '../../lib/stages.ts';
import type { PostureCell } from '../../posture/contract.ts';
import type { NodeDetail } from '../../posture/detail.ts';
import { FENCE_OF_VERB, POSTURE_LABEL } from '../../posture/grammar.ts';
import type { AibScenarioKey, CaseConclusion, CaseDefinition, CaseFact, CaseKey, CaseKind, CaseSnapshot, RobustnessRecord } from '../model.ts';
import { conclusionLabel, isStrategicCompactId, primaryMission, strategicCompactDecision, STRATEGIC_COMPACT_DECISION_LABEL } from './answerOptions.ts';
import { canonicalJson as privacyCanonicalJson } from '../privacy.ts';
import { cellKey, type DecodedBundles } from './bundles.ts';
import { cappedNodes, extractChoiceInputs, shippedChoice, thresholdsFromCore } from './choiceRule.ts';
import { aibNodeRobustness, cellRobustness, postureSpecs } from './sensitivity.ts';
import { deriveCompactThreat, deriveNodeThreat } from './threat.ts';

// ---- the shared fact ids --------------------------------------------------------------------------------------

export const POSTURE_FACT_IDS = [
  'need', 'prize', 'market', 'cp', 'home_share', 'import_dependence', 'import_hhi', 'china_bloc', 'trusted_alternatives', 'top_supplier', 'top_supplier_share',
  'china_share', 'opposite_bloc_share', 'trusted_share', 'coercive_top', 'coercive_top_share', 'imports_to_gdp', 'benchmark_share', 'benchmark_rca',
  'benchmark_exports', 'outlook', 'prize_note', 'pathway', 'deployment_need', 'large_yard_signals', 'partner', 'policy_alignment', 'confidence', 'borderline',
  'reason', 'choice', 'fence', 'posture',
] as const;
export const AIB_NODE_FACT_IDS = [
  'class', 'reason', 'exposure', 'lost_usd', 'demand_usd', 'baseline_hhi', 'baseline_largest', 'baseline_largest_share', 'baseline_top3_share', 'static', 'existing',
  'optimized', 'dividend', 'residual', 'residual_usd', 'after_hhi', 'after_largest', 'after_largest_share', 'n1_reoptimized', 'diversion', 'physical_warning',
  'top_suppliers', 'hosts',
] as const;
export const COMPACT_FACT_IDS = [
  'decision', 'purpose', 'core', 'steps', 'coverage_static', 'coverage_existing', 'coverage_coordinated', 'coverage_post_investment', 'lost_usd', 'exposure',
  'class_counts', 'missions', 'non_allied_relied_on', 'critical_external', 'build_hosts',
] as const;
export const PARTNERSHIP_FACT_IDS = ['pair', 'choices_summary', 'memo_verified', 'memo_as_of', 'memo_generated_at', 'headline'] as const;

/** The fact ids of each case kind (sector cases reuse the posture ids). */
export const FACT_IDS: Readonly<Record<CaseKind, readonly string[]>> = {
  posture_cell: POSTURE_FACT_IDS,
  sector: POSTURE_FACT_IDS,
  aib_node: AIB_NODE_FACT_IDS,
  compact: COMPACT_FACT_IDS,
  partnership: PARTNERSHIP_FACT_IDS,
};

/** Fact ids that give the conclusion away: never in context_fields (the same list review/manifest.ts enforces). */
export const REVEALING_FACT_IDS: readonly string[] = [
  'choice', 'fence', 'posture', 'reason', 'confidence', 'borderline', 'partner', 'policy_alignment', 'class', 'decision', 'missions', 'choices_summary', 'headline',
];

const FACT_LABEL: Readonly<Record<string, string>> = {
  need: 'Supply-chain vulnerability (Strategic Need, 0-1 percentile)',
  prize: 'Strategic Industrial Value (prize, 0-1)',
  market: 'Market prize (industrial opportunity, 0-1)',
  cp: 'Competitive Position among contenders (0-1)',
  home_share: "The economy's share of world supply",
  import_dependence: 'Import dependence (family percentile)',
  import_hhi: 'Supplier concentration (family percentile)',
  china_bloc: 'Risky-supplier exposure (family percentile)',
  trusted_alternatives: 'Trusted alternatives (family percentile)',
  top_supplier: 'Largest import supplier',
  top_supplier_share: "Largest supplier's share of imports",
  china_share: 'China (with HKG and MAC) share of imports',
  opposite_bloc_share: 'Opposite-bloc share of imports',
  trusted_share: "Trusted partners' share of world supply (less own)",
  coercive_top: 'Largest coercive supplier (world supply)',
  coercive_top_share: "Largest coercive supplier's share of world supply",
  imports_to_gdp: 'Imports of the stage basket / GDP',
  benchmark_share: 'World export share (trade benchmark)',
  benchmark_rca: 'Revealed comparative advantage (trade benchmark)',
  benchmark_exports: 'Exports (trade benchmark, USD)',
  outlook: 'World market outlook',
  prize_note: 'Prize note (market growth and capex)',
  pathway: 'Credible pathway to build capability',
  deployment_need: 'Deployment Need (0-1)',
  large_yard_signals: 'Large-yard (commodity) signals that hold',
  partner: 'Leading partner',
  policy_alignment: 'Policy alignment',
  confidence: 'Confidence in the call',
  borderline: 'Inputs within the borderline band of their threshold',
  reason: 'Why (the deciding test, in words)',
  choice: 'Strategic choice',
  fence: 'Fence',
  posture: 'Posture',
  class: 'AIB treatment class',
  exposure: 'Share of allied traded demand lost to the shock',
  lost_usd: 'Lost supply (USD)',
  demand_usd: 'Allied traded demand (USD)',
  // worded as the Explorer's node table words it (aib/views/compact/NodeSection.tsx: "Today's concentration (HHI) of
  // supply to the allied market"); no bare acronym and no word of content/vocabulary.ts AVOID_RE in a reviewer label
  baseline_hhi: "Today's concentration (HHI) of supply to the allied market",
  baseline_largest: 'Largest supplier today',
  baseline_largest_share: "Largest supplier's share today",
  baseline_top3_share: 'Top-3 suppliers share today',
  static: 'Coverage right after the shock (no response)',
  existing: 'Coverage with existing relationships',
  optimized: 'Coverage after coordinated trade',
  dividend: 'Coordination dividend (coordinated trade minus existing relationships)',
  residual: 'Remaining gap after coordinated trade (new capacity needed)',
  residual_usd: 'Remaining gap (USD)',
  after_hhi: 'Concentration (HHI) of the supply after coordinated trade',
  after_largest: 'Largest supplier after',
  after_largest_share: "Largest supplier's share after",
  n1_reoptimized: 'Coverage if the largest remaining supplier is removed and the trade re-solved (N-1 test)',
  diversion: 'Share served by supply diverted from buyers outside the scope',
  physical_warning: 'Physical-capability warning',
  top_suppliers: 'Top suppliers today (share of scope supply)',
  hosts: 'Leading investment hosts',
  decision: 'Compact conclusion',
  purpose: 'Purpose',
  core: 'Core countries and roles',
  steps: 'Supply-chain steps',
  coverage_static: 'Coverage right after the shock',
  coverage_existing: 'Coverage with existing relationships',
  coverage_coordinated: 'Coverage with coordinated trade',
  coverage_post_investment: 'Coverage after simulated new capacity',
  class_counts: 'Steps by treatment',
  missions: 'Missions',
  non_allied_relied_on: 'Non-allied suppliers the coalition still relies on',
  critical_external: 'Critical non-allied partners',
  build_hosts: 'Hosts of new capacity',
  pair: 'Partnership pair',
  choices_summary: "The memo's box of strategic choices",
  memo_verified: 'Memo independently verified',
  memo_as_of: 'Memo data as of',
  memo_generated_at: 'Memo generated at',
  headline: "The memo's headline recommendation",
};

/** The note on a fact whose text is copied verbatim from the Explorer's bundle (shown with the fact). */
export const QUOTED_FROM_EXPLORER = "Quoted verbatim from the Explorer's AIB data";

// ---- formatting -------------------------------------------------------------------------------------------------

const nn = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);
export const fmtScore = (v: number | null): string => (v === null ? 'not in this release' : v.toFixed(3));
export const fmtShare = (v: number | null): string => (v === null ? 'not in this release' : `${(Math.round(v * 1000) / 10).toFixed(1)}%`);
export const fmtUsd = (v: number | null): string =>
  v === null ? 'not in this release' : Math.abs(v) >= 1e9 ? `US$${(v / 1e9).toFixed(1)}bn` : Math.abs(v) >= 1e6 ? `US$${(v / 1e6).toFixed(0)}m` : `US$${Math.round(v).toLocaleString('en-US')}`;

type Extra = Partial<Pick<CaseFact, 'unit' | 'source' | 'source_year' | 'note'>>;

function fact(id: string, value: CaseFact['value'], display: string, extra: Extra = {}): CaseFact {
  return { id, label: FACT_LABEL[id] ?? id, value, display, unit: extra.unit ?? null, source: extra.source ?? null, source_year: extra.source_year ?? null, note: extra.note ?? null };
}
const missing = (id: string, note: string): CaseFact => fact(id, null, 'not in this release', { note });
const score = (id: string, v: number | null, missNote: string, extra: Extra = {}): CaseFact => (v === null ? missing(id, missNote) : fact(id, v, fmtScore(v), { unit: 'score 0-1', ...extra }));
const share = (id: string, v: number | null, missNote: string, extra: Extra = {}): CaseFact => (v === null ? missing(id, missNote) : fact(id, v, fmtShare(v), { unit: 'share', ...extra }));
const usd = (id: string, v: number | null, missNote: string, extra: Extra = {}): CaseFact => (v === null ? missing(id, missNote) : fact(id, v, fmtUsd(v), { unit: 'USD', ...extra }));
const text = (id: string, v: string | null, missNote: string, extra: Extra = {}): CaseFact => (v === null ? missing(id, missNote) : fact(id, v, v, extra));
const list = (id: string, v: readonly string[] | null | undefined, missNote: string, extra: Extra = {}): CaseFact =>
  !v || !v.length ? missing(id, missNote) : fact(id, v.join('; '), v.join('; '), extra);

/** Every fact id of the kind present: a fact the builder did not write is added as null with a note. */
function complete(kind: CaseKind, facts: Record<string, CaseFact>, why: string): Record<string, CaseFact> {
  const out: Record<string, CaseFact> = {};
  for (const id of FACT_IDS[kind]) out[id] = facts[id] ?? missing(id, why);
  return out;
}

/** Stable JSON (object keys sorted at every level): the text the snapshot hash is taken over. One definition with
 * review/privacy.ts, which the app and the admin exports hash with, so a snapshot hash means the same everywhere. */
export const canonicalJson = privacyCanonicalJson;

// ---- context ----------------------------------------------------------------------------------------------------

export interface SnapshotContext {
  /** precomputed robustness by posture cell key "ISO|Tech|Stage" (else computed for the case) */
  postureRobustness?: ReadonlyMap<string, RobustnessRecord> | null;
  /** precomputed robustness by AIB node id (Decoupling; else computed) */
  aibRobustness?: ReadonlyMap<string, RobustnessRecord> | null;
}

export class SnapshotError extends Error {}

const missingConclusion = (why: string): CaseConclusion => ({ value: 'missing', label: 'Not in this release', detail: null, reason: why, confidence: null });

// ---- posture cells ----------------------------------------------------------------------------------------------

function postureSnapshot(key: CaseKey, b: DecodedBundles, ctx: SnapshotContext, kind: CaseKind): CaseSnapshot {
  const p = b.posture;
  const why = (s: string) => `${s} (posture bundle ${p?.manifest && typeof p.manifest.created === 'string' ? p.manifest.created : 'unstamped'})`;
  if (!p || !key.iso3 || !key.tech || !key.stage) {
    return { conclusion: missingConclusion('no posture bundle or incomplete key'), facts: complete(kind, {}, 'no posture bundle in this release'), trace: [], threat: null, robustness: null, data_quality: { grade: null, notes: ['no posture bundle'] } };
  }
  const c: PostureCell | undefined = p.core.cells.find((x) => x.iso3 === key.iso3 && x.tech === key.tech && x.stage === key.stage);
  const d: NodeDetail | undefined = p.details[key.iso3]?.cells.find((x) => x.tech === key.tech && x.stage === key.stage);
  if (!c) {
    const note = why(`${key.iso3} ${key.tech}/${key.stage} is not in the posture core`);
    return { conclusion: missingConclusion(note), facts: complete(kind, {}, note), trace: [], threat: null, robustness: null, data_quality: { grade: null, notes: [note] } };
  }
  const dep = c.stage === DEPLOYMENT_STAGE;
  const sc = d?.supplyChain ?? null;
  const noDetail = d ? null : 'no detail document cell for this node';
  const scMiss = dep ? 'not measured at Deployment & Use (deployment need is read instead)' : (noDetail ?? 'not in the detail cell (not measured for this node)');
  const meta = p.core.meta;
  const tr = d?.trade ?? null;
  const benchSrc = tr?.benchmark_source ? `${tr.benchmark_source} (trade benchmark)` : 'trade benchmark (source not stated)';
  const choice = shippedChoice(c);
  const facts: Record<string, CaseFact> = {};
  facts.need = dep ? missing('need', 'supply-chain need is not read at Deployment & Use; see deployment_need') : score('need', d?.need ?? c.need, 'supply-chain need is not measured for this node (not assessed)');
  facts.prize = dep ? missing('prize', 'no prize at Deployment & Use') : score('prize', d?.prize.value ?? c.prize, 'prize not measured', { note: d?.prize.measure ? `measure: ${d.prize.measure}` : null });
  facts.market = dep ? missing('market', 'no market prize at Deployment & Use') : score('market', d?.prize.market ?? c.market, 'market prize not measured');
  facts.cp = score('cp', d?.cp ?? c.cp, 'Competitive Position is not measured for this node');
  facts.home_share = share('home_share', d?.homeShare ?? c.share, 'no measured world-supply share');
  facts.import_dependence = score('import_dependence', sc?.importDependence ?? null, scMiss);
  facts.import_hhi = score('import_hhi', sc?.supplierConcentration ?? null, scMiss);
  facts.china_bloc = score('china_bloc', sc?.riskyExposure ?? null, scMiss);
  facts.trusted_alternatives = score('trusted_alternatives', sc?.trustedAlternatives ?? null, scMiss);
  facts.top_supplier = text('top_supplier', sc?.topSupplier ?? null, scMiss);
  facts.top_supplier_share = share('top_supplier_share', sc?.topSupplierShare ?? null, scMiss);
  const ct = sc?.currentTrade ?? null;
  facts.china_share = share('china_share', sc?.chinaShare ?? null, scMiss, {
    note: ct ? `blended with the latest 12 months of trade (weight ${ct.weight ?? 'n/a'}, ${ct.source ?? 'source not stated'}, window ending ${ct.windowEnd ?? 'n/a'})` : null,
  });
  facts.opposite_bloc_share = share('opposite_bloc_share', sc?.oppositeBlocShare ?? null, scMiss);
  facts.trusted_share = share('trusted_share', sc?.trustedShare ?? null, scMiss);
  facts.coercive_top = text('coercive_top', sc?.coerciveTop ?? null, scMiss);
  facts.coercive_top_share = share('coercive_top_share', sc?.coerciveTopShare ?? null, scMiss);
  facts.imports_to_gdp = sc?.importsToGdp !== null && sc?.importsToGdp !== undefined
    ? fact('imports_to_gdp', sc.importsToGdp, `${(sc.importsToGdp * 100).toFixed(3)}% of GDP`, { unit: 'ratio to GDP' })
    : missing('imports_to_gdp', scMiss);
  const tMiss = 'no trade-benchmark value for this node';
  facts.benchmark_share = share('benchmark_share', nn(tr?.benchmark_share), tMiss, { source: benchSrc, source_year: meta.benchmark_period });
  facts.benchmark_rca = nn(tr?.benchmark_rca) === null ? missing('benchmark_rca', tMiss) : fact('benchmark_rca', tr!.benchmark_rca!, tr!.benchmark_rca!.toFixed(2), { unit: 'RCA', source: benchSrc, source_year: meta.benchmark_period });
  facts.benchmark_exports = usd('benchmark_exports', nn(tr?.benchmark_exports), tMiss, { source: benchSrc, source_year: meta.benchmark_period });
  facts.outlook = text('outlook', d?.prize.outlook ?? null, dep ? 'no market outlook at Deployment & Use' : 'no outlook written');
  facts.prize_note = text('prize_note', d?.prize.note ?? null, dep ? 'no prize at Deployment & Use' : 'no prize note written');
  facts.pathway = d?.pathway.credible
    ? fact('pathway', d.pathway.credible, d.pathway.credible, { note: d.pathway.basis.length ? `basis: ${d.pathway.basis.join('; ')}` : null })
    : missing('pathway', dep ? 'no pathway at Deployment & Use' : 'no pathway written');
  facts.deployment_need = dep ? score('deployment_need', d?.need ?? c.need, 'deployment need is not measured for this economy') : missing('deployment_need', 'deployment need is read only at Deployment & Use');
  const ly = d?.trace.find((t) => t.test === 'large_yard_case');
  facts.large_yard_signals = ly
    ? fact('large_yard_signals', nn(Number(ly.value)) ?? null, `${ly.value ?? 'n/a'} of 8 commodity signals hold (case needs ${ly.threshold ?? 'n/a'}): ${ly.result}`, {
        note: (() => {
          const yf = meta.yf_nodes[`${c.tech}|${c.stage}|stage`];
          return yf ? `node signals that hold: ${yf.signals.join(', ') || 'none'}; not observed: ${yf.unknown.join(', ') || 'none'}` : null;
        })(),
      })
    : missing('large_yard_signals', dep ? 'no large-yard case at Deployment & Use' : 'the trace has no large-yard step (Secure, or the case is not read)');
  const partner = d?.partners[0]?.iso3 ?? c.partner1;
  facts.partner = text('partner', partner ?? null, 'no partner recommendation written for this cell');
  facts.policy_alignment = c.alignment ? fact('policy_alignment', c.alignment, c.alignment === 'deployment_gap' ? ALIGNMENT_DEPLOYMENT_GAP : ALIGNMENT_NAME[c.alignment]) : missing('policy_alignment', 'no policy alignment written');
  facts.confidence = text('confidence', c.confidence, 'no confidence written');
  const bl = d?.borderline.length ? d.borderline : c.borderline ? c.borderline.split(';') : [];
  facts.borderline = bl.length ? fact('borderline', bl.join('; '), bl.join('; ')) : fact('borderline', 'none', 'none within the band', { note: 'no input within the borderline band (0.05) of its threshold' });
  facts.reason = text('reason', d?.reason ?? d?.notAssessedReason ?? null, 'no reason written');
  facts.choice = choice ? fact('choice', choice, conclusionLabel('posture_cell', choice)) : missing('choice', 'no shipped choice');
  const fence = choice && choice !== 'not_assessed' ? FENCE_OF_VERB[choice] : null;
  facts.fence = fence ? fact('fence', fence, FENCE_NAME[fence]) : missing('fence', 'not assessed: no fence');
  facts.posture = fact('posture', c.posture, POSTURE_LABEL[c.posture]);

  // robustness (bundle layer)
  let robustness: RobustnessRecord | null = ctx.postureRobustness?.get(cellKey(c.iso3, c.tech, c.stage)) ?? null;
  if (!robustness && d) {
    const th = thresholdsFromCore(p.core);
    const inp = extractChoiceInputs(d, c.stage, { capped: cappedNodes(p.core).get(`${c.tech}|${c.stage}`) ?? null, cpThreshold: th.cp });
    robustness = cellRobustness(inp, { iso3: c.iso3, tech: c.tech, stage: c.stage }, choice, th, postureSpecs(th));
  }
  // data quality (kept apart from robustness and from expert validation)
  const notes: string[] = [];
  if (noDetail) notes.push(noDetail);
  if (c.evidenceRaw) notes.push(`stage evidence quality (Competitive Position inputs): ${c.evidenceRaw}`);
  if (c.flag) notes.push(`consistency flag: ${c.flag}`);
  if (ct) notes.push(`import shares blended with current trade (${ct.source ?? 'source not stated'})${ct.flag ? `, flag ${ct.flag}` : ''}`);
  if (facts.prize.value === 0.59) notes.push('prize sits exactly at the 0.59 market-only cap (a rule, not a measurement near the bar)');
  notes.push('indicator-level source and data year are not shipped in the posture bundle; the trade benchmark period and momentum period are bundle-level only');
  const conclusion: CaseConclusion = choice
    ? {
        value: choice,
        label: conclusionLabel('posture_cell', choice),
        detail: fence ? `${FENCE_NAME[fence]} · ${POSTURE_LABEL[c.posture]}` : POSTURE_LABEL[c.posture],
        reason: d?.reason ?? d?.notAssessedReason ?? null,
        confidence: c.confidence,
      }
    : missingConclusion('no shipped choice');
  const trace = (d?.trace ?? []).map((t) => ({
    step: t.step,
    test: t.test,
    value: typeof t.value === 'number' ? t.value : typeof t.value === 'string' && t.value !== '' && Number.isFinite(Number(t.value)) ? Number(t.value) : null,
    threshold: typeof t.threshold === 'number' ? t.threshold : typeof t.threshold === 'string' && Number.isFinite(Number(t.threshold)) ? Number(t.threshold) : null,
    result: t.result,
  }));
  return { conclusion, facts: complete(kind, facts, 'not written'), trace, threat: null, robustness, data_quality: { grade: c.evidenceRaw, notes } };
}

// ---- AIB nodes ------------------------------------------------------------------------------------------------------

const suppliersText = (s: readonly [string, number | null][] | undefined): string[] => (s ?? []).map(([iso, sh]) => `${iso} ${fmtShare(nn(sh))}`);

function aibNodeSnapshot(key: CaseKey, b: DecodedBundles, ctx: SnapshotContext): CaseSnapshot {
  const doc = b.aib?.doc ?? null;
  const scenario: AibScenarioKey = key.scenario ?? 'decoupling';
  const node: AibNode | undefined = doc?.nodes.find((n) => n.id === key.node);
  if (!doc || !node) {
    const note = !doc ? 'no AIB bundle in this release' : `node ${key.node} is not in the AIB bundle`;
    return { conclusion: missingConclusion(note), facts: complete('aib_node', {}, note), trace: [], threat: null, robustness: null, data_quality: { grade: null, notes: [note] } };
  }
  const f: Record<string, CaseFact> = {};
  const bl = node.baseline ?? null;
  f.baseline_hhi = score('baseline_hhi', nn(bl?.hhi), 'no baseline record for this node', { unit: 'HHI 0-1' });
  f.baseline_largest = text('baseline_largest', bl?.largest ?? node.baseLargest ?? null, 'no baseline record');
  f.baseline_largest_share = share('baseline_largest_share', nn(bl?.largestShare) ?? nn(node.baseLargestShare), 'no baseline record');
  f.baseline_top3_share = share('baseline_top3_share', nn(bl?.top3Share), 'no baseline record');
  f.top_suppliers = list('top_suppliers', suppliersText(bl?.suppliers), 'no baseline supplier list');
  f.physical_warning = fact('physical_warning', node.warning ?? 'none', node.warning ?? 'none', { note: node.physicalValidation ?? null });
  let cls: string | null = null;
  let reason: string | null = null;
  const notes: string[] = [];
  if (scenario === 'decoupling') {
    cls = node.cls;
    reason = node.reason;
    f.exposure = share('exposure', nn(node.exposure), 'not exported');
    f.lost_usd = usd('lost_usd', nn(node.lostUsd), 'not exported');
    f.demand_usd = usd('demand_usd', nn(node.demandUsd), 'not exported');
    f.static = share('static', nn(node.static), 'not exported');
    f.existing = share('existing', nn(node.existing), 'not exported');
    f.optimized = share('optimized', nn(node.optimized), 'not exported');
    f.dividend = share('dividend', nn(node.dividend), 'not exported');
    f.residual = share('residual', nn(node.residual), 'not exported');
    const r = nn(node.residual);
    const dm = nn(node.demandUsd);
    f.residual_usd = r !== null && dm !== null ? usd('residual_usd', r * dm, '', { note: 'residual x demandUsd (display arithmetic on exported fields; residualUsd is not exported for Decoupling)' }) : missing('residual_usd', 'residual or demand not exported');
    f.after_hhi = score('after_hhi', nn(node.hhi), 'not exported', { unit: 'HHI 0-1' });
    f.after_largest = text('after_largest', node.largest, 'not exported');
    f.after_largest_share = share('after_largest_share', nn(node.largestShare), 'not exported');
    f.n1_reoptimized = share('n1_reoptimized', nn(node.n1Reoptimized), 'not exported');
    f.diversion = share('diversion', nn(node.diversion), 'not exported');
    f.hosts = list('hosts', node.hosts, 'no leading host');
  } else if (scenario === 'managed') {
    const dv = node.diversify;
    if (!dv) notes.push('the Managed Interdependence scenario did not assess this node (node.diversify absent)');
    cls = dv?.cls ?? null;
    reason = dv ? `status: ${dv.status}` : null;
    const ex = nn(dv?.excess);
    f.exposure = share('exposure', ex, 'not exported for this node', { note: 'Managed Interdependence: the excess supply above the benchmark cap that must shift (diversify.excess), not a removed supplier' });
    f.lost_usd = usd('lost_usd', nn(dv?.lostUsd), 'not exported for this node');
    const dmd = nn(node.demandUsd);
    f.demand_usd = usd('demand_usd', dmd, 'not exported', { note: 'the node\'s allied traded demand (node.demandUsd, the same base as Decoupling)' });
    f.static = share('static', nn(dv?.static), 'not exported for this node');
    f.existing = share('existing', nn(dv?.existing), 'not exported for this node');
    f.optimized = share('optimized', nn(dv?.optimized), 'not exported for this node');
    f.dividend = share('dividend', nn(dv?.dividend), 'not exported for this node');
    f.residual = share('residual', nn(dv?.residual), 'not exported for this node');
    f.residual_usd = usd('residual_usd', nn(dv?.residualUsd), 'not exported for this node');
    f.after_hhi = score('after_hhi', nn(dv?.hhi), 'not exported for this node', { unit: 'HHI 0-1', note: `world-export HHI basis (diversify.currentHhi ${dv?.currentHhi ?? 'n/a'} vs benchmark ${dv?.benchmarkHhi ?? 'n/a'}), not the scope-supply basis of baseline_hhi` });
    f.after_largest = text('after_largest', dv?.largest ?? null, 'not exported for this node');
    f.after_largest_share = share('after_largest_share', nn(dv?.largestShare), 'not exported for this node');
    f.n1_reoptimized = missing('n1_reoptimized', 'no N-1 test is exported for the Managed Interdependence scenario');
    f.diversion = missing('diversion', 'diversion is exported for Decoupling only');
    f.hosts = list('hosts', dv?.hosts ?? null, 'no leading host');
  } else {
    notes.push("Today's network: no shock is applied, so the node has no class, coverage path or hosts under this scenario");
    for (const id of ['exposure', 'lost_usd', 'static', 'existing', 'optimized', 'dividend', 'residual', 'residual_usd', 'after_hhi', 'after_largest', 'after_largest_share', 'n1_reoptimized', 'diversion', 'hosts'])
      f[id] = missing(id, 'no shock in the baseline scenario');
    f.demand_usd = usd('demand_usd', nn(node.demandUsd), 'not exported');
  }
  f.class = cls ? fact('class', cls, conclusionLabel('aib_node', cls)) : missing('class', `no class under ${scenario}`);
  // the AIB export's own words, kept verbatim and marked as quoted (they may use terms the review pages avoid)
  f.reason = text('reason', reason, 'no reason exported', { note: QUOTED_FROM_EXPLORER });
  const robustness = scenario === 'decoupling' ? (ctx.aibRobustness?.get(node.id) ?? aibNodeRobustness(node, 'decoupling')) : aibNodeRobustness(node, scenario);
  const threat = deriveNodeThreat(doc, node.id, scenario, { managed: b.aib?.managed ?? null });
  if (node.warning) notes.push(`${node.warning}${node.physicalValidation ? `: ${node.physicalValidation}` : ''}`);
  if (node.flags?.length) notes.push(`flags: ${node.flags.join(', ')}`);
  notes.push('the AIB bundle carries no git commit');
  const conclusion: CaseConclusion = cls ? { value: cls, label: conclusionLabel('aib_node', cls), detail: null, reason, confidence: null } : missingConclusion(`no class under ${scenario}`);
  return { conclusion, facts: complete('aib_node', f, 'not exported'), trace: [], threat, robustness, data_quality: { grade: node.quality, notes } };
}

// ---- compacts ---------------------------------------------------------------------------------------------------------

function strategicCompactSnapshot(key: CaseKey, doc: AibDoc): CaseSnapshot | null {
  const sc = doc.strategic?.compacts.find((c) => c.id.toUpperCase() === String(key.sid).toUpperCase());
  if (!sc) return null;
  const scenario: AibScenarioKey = key.scenario ?? 'decoupling';
  const rec: AibStrategicCompactScenario | null = (sc.scenarios as Record<string, AibStrategicCompactScenario | undefined>)[scenario] ?? null;
  const f: Record<string, CaseFact> = {};
  const why = rec ? 'not exported' : `the compact has no ${scenario} record`;
  const dec = strategicCompactDecision(rec);
  f.decision = dec ? fact('decision', dec, STRATEGIC_COMPACT_DECISION_LABEL[dec], { note: 'read with the export\'s materiality rule (primary mission vs `targeted`), as main\'s compact page renders it' }) : missing('decision', why);
  f.purpose = text('purpose', sc.purpose, 'no purpose exported');
  f.core = list('core', sc.core.map((e) => `${e.iso3} (${e.role}${e.step ? `, ${e.step}` : ''})`), 'no core countries');
  f.steps = list('steps', sc.steps.map((s) => s.label), 'no steps');
  const cov = rec?.coverage ?? null;
  f.coverage_static = share('coverage_static', nn(cov?.static), why);
  f.coverage_existing = share('coverage_existing', nn(cov?.existing), why);
  f.coverage_coordinated = share('coverage_coordinated', nn(cov?.coordinated), why);
  f.coverage_post_investment = share('coverage_post_investment', nn(cov?.postInvestment), rec ? 'no investment simulation under this scenario' : why, {
    note: 'postInvestment = coordinated + min(increment, gap) by construction (docs/aib_methodology.md 16.3): arithmetic, not a finding',
  });
  f.lost_usd = usd('lost_usd', nn(rec?.lostUsd), why);
  f.exposure = share('exposure', nn(rec?.exposure), why);
  const k = rec?.counts;
  f.class_counts = k
    ? fact('class_counts', `BUILD ${k.build}; COORDINATE ${k.coordinate}; MANAGE SPECIALIZATION ${k.manage}; LOW INTERVENTION ${k.low}; INSUFFICIENT DATA ${k.insufficient}; not stressed ${k.notStressed}; not tested ${k.notTested ?? 0}; no shock ${k.noShock}`, `BUILD ${k.build} · COORDINATE ${k.coordinate} · MANAGE ${k.manage} · LOW ${k.low}`)
    : missing('class_counts', why);
  f.missions = rec?.missions.length
    ? list('missions', rec.missions.map((m) => `${m.targeted ? 'Targeted action' : m.slot}: ${m.cls}: ${m.text}${m.basis ? ` (${m.basis})` : ''}`), why)
    : rec
      ? fact('missions', 'none', rec.statement || 'none')
      : missing('missions', why);
  f.non_allied_relied_on = rec?.nonAlliedReliedOn === null || rec?.nonAlliedReliedOn === undefined ? missing('non_allied_relied_on', rec ? 'no coalition under this scenario' : why) : fact('non_allied_relied_on', rec.nonAlliedReliedOn.join('; ') || 'none', rec.nonAlliedReliedOn.join('; ') || 'none');
  f.critical_external = rec?.criticalExternal === null || rec?.criticalExternal === undefined ? missing('critical_external', rec ? 'no coalition under this scenario' : why) : fact('critical_external', rec.criticalExternal.join('; ') || 'none', rec.criticalExternal.join('; ') || 'none');
  const bh = (rec as { buildHosts?: string[] | null } | null)?.buildHosts;
  f.build_hosts = bh === null || bh === undefined ? missing('build_hosts', why) : fact('build_hosts', bh.join('; ') || 'none', bh.join('; ') || 'none');
  const pm = primaryMission(rec);
  const conclusion: CaseConclusion = dec
    ? { value: dec, label: STRATEGIC_COMPACT_DECISION_LABEL[dec], detail: pm ? `${pm.cls}: ${pm.text}` : (rec?.statement ?? null), reason: pm?.basis ?? null, confidence: null }
    : missingConclusion(why);
  return {
    conclusion,
    facts: complete('compact', f, 'not exported'),
    trace: [],
    threat: deriveCompactThreat(doc, sc.id, scenario),
    robustness: null,
    data_quality: { grade: null, notes: ['the AIB bundle carries no git commit', 'main has no compact-decision vocabulary (compact/decide.ts is redesign-only): the conclusion is read from the export\'s mission materiality flag'] },
  };
}

function operationalCompactSnapshot(key: CaseKey, doc: AibDoc): CaseSnapshot | null {
  const c = doc.compacts.find((x) => x.id.toUpperCase() === String(key.sid).toUpperCase());
  if (!c) return null;
  const scenario: AibScenarioKey = key.scenario ?? 'decoupling';
  const m = scenario === 'decoupling' ? c.metrics : null;
  const mg = scenario === 'managed' ? (c.managed ?? null) : null;
  const why = scenario === 'baseline' ? 'an operational compact has no baseline record' : 'not exported';
  const f: Record<string, CaseFact> = {};
  f.decision = fact('decision', c.type, conclusionLabel('compact', c.type, key), { note: 'the compact type is the Decoupling headline result in every scenario' });
  f.purpose = missing('purpose', 'operational compacts carry no purpose text');
  f.core = list('core', c.anchors, 'no anchors');
  f.steps = list('steps', c.stages, 'no stages');
  f.coverage_static = share('coverage_static', nn(m?.static ?? mg?.static), why);
  f.coverage_existing = share('coverage_existing', nn(m?.existing ?? mg?.existing), why);
  f.coverage_coordinated = share('coverage_coordinated', nn(m?.trade ?? mg?.optimized), why);
  f.coverage_post_investment = share('coverage_post_investment', nn(m?.investment), m ? 'not exported' : 'no investment simulation under this scenario');
  f.lost_usd = usd('lost_usd', nn(m?.lostUsd ?? mg?.lostUsd), why);
  f.exposure = share('exposure', nn(m?.exposure), m ? 'not exported' : 'exposure is exported for Decoupling only');
  f.class_counts = text('class_counts', mg?.classMix ?? c.classMix ?? null, 'no class mix exported');
  f.missions = missing('missions', 'operational compacts have no missions (strategic compacts do)');
  f.non_allied_relied_on = missing('non_allied_relied_on', 'not exported per operational compact');
  const ce = (c.coalition as { criticalExternal?: string[] } | undefined)?.criticalExternal;
  f.critical_external = ce ? fact('critical_external', ce.join('; ') || 'none', ce.join('; ') || 'none') : missing('critical_external', 'not exported');
  f.build_hosts = list('build_hosts', c.hosts, 'no scaling hosts');
  return {
    conclusion: { value: c.type, label: conclusionLabel('compact', c.type, key), detail: c.archetype, reason: null, confidence: null },
    facts: complete('compact', f, 'not exported'),
    trace: [],
    threat: deriveCompactThreat(doc, c.id, scenario),
    robustness: null,
    data_quality: { grade: null, notes: ['the AIB bundle carries no git commit'] },
  };
}

function compactSnapshot(key: CaseKey, b: DecodedBundles): CaseSnapshot {
  const doc = b.aib?.doc ?? null;
  const s = doc ? (isStrategicCompactId(key.sid) ? strategicCompactSnapshot(key, doc) : operationalCompactSnapshot(key, doc)) : null;
  if (s) return s;
  const note = doc ? `compact ${key.sid} is not in the AIB bundle` : 'no AIB bundle in this release';
  return { conclusion: missingConclusion(note), facts: complete('compact', {}, note), trace: [], threat: null, robustness: null, data_quality: { grade: null, notes: [note] } };
}

// ---- partnerships -----------------------------------------------------------------------------------------------

interface ChoiceItem {
  label: string;
  kind: string;
  text: string;
}

/** The memo's box of strategic choices (the summary section's `choices` block). */
export function memoChoices(m: MemoDoc | Record<string, unknown>): ChoiceItem[] {
  const secs = ((m as { sections?: unknown }).sections ?? []) as { id?: string; blocks?: unknown[] }[];
  const sum = (m as { summary?: unknown }).summary;
  const pools: unknown[][] = [];
  if (sum && typeof sum === 'object' && Array.isArray((sum as { blocks?: unknown[] }).blocks)) pools.push((sum as { blocks: unknown[] }).blocks);
  for (const s of secs) if (Array.isArray(s.blocks)) pools.push(s.blocks);
  for (const blocks of pools)
    for (const bl of blocks) {
      const blk = bl as { type?: string; items?: { label?: string; kind?: string; inlines?: { v?: string }[] }[] };
      if (blk.type === 'choices' && Array.isArray(blk.items))
        return blk.items.map((it) => ({ label: it.label ?? '', kind: it.kind ?? '', text: (it.inlines ?? []).map((x) => x.v ?? '').join('') }));
    }
  return [];
}

function partnershipSnapshot(key: CaseKey, b: DecodedBundles): CaseSnapshot {
  const pr = b.partnerships;
  const pair = key.pair ?? '';
  const entry = pr?.manifest.pairs[pair] ?? pr?.manifest.compacts[pair] ?? null;
  const memo = pr?.memos[pair] ?? null;
  const f: Record<string, CaseFact> = {};
  f.pair = text('pair', pair || null, 'no pair');
  if (!entry || !memo) {
    const note = !pr ? 'no partnerships bundle in this release' : `the ${pair} memo is not shipped in this release (main's partnerships manifest lists ${Object.keys(pr.manifest.pairs).join(', ') || 'no pair'})`;
    return { conclusion: missingConclusion(note), facts: complete('partnership', f, note), trace: [], threat: null, robustness: null, data_quality: { grade: null, notes: [note] } };
  }
  const items = memoChoices(memo);
  f.memo_verified = fact('memo_verified', entry.verified === true, entry.verified === true ? 'yes' : 'no: rule-generated, not independently verified');
  f.memo_as_of = text('memo_as_of', entry.as_of ?? null, 'no as-of date');
  f.memo_generated_at = text('memo_generated_at', entry.generated_at ?? null, 'no generation date');
  f.choices_summary = items.length
    ? fact('choices_summary', items.map((i) => `${i.label} (${CHOICE_NAME[i.kind as keyof typeof CHOICE_NAME] ?? i.kind}): ${i.text}`).join(' | '), items.map((i) => `${i.label}: ${i.text}`).join(' | '))
    : missing('choices_summary', 'the memo has no box of strategic choices');
  const head = items[0] ?? null;
  f.headline = head ? fact('headline', head.text, `${head.label}: ${head.text}`) : missing('headline', 'the memo has no box of strategic choices');
  const conclusion: CaseConclusion = head
    ? { value: head.kind, label: conclusionLabel('partnership', head.kind), detail: `${head.label}: ${head.text}`, reason: null, confidence: null }
    : missingConclusion('the memo has no box of strategic choices');
  return {
    conclusion,
    facts: complete('partnership', f, 'not written'),
    trace: [],
    threat: null,
    robustness: null,
    data_quality: {
      grade: entry.verified === true ? 'verified' : 'unverified',
      notes: [
        'memo is rule-generated and not independently verified (verified: false)',
        'no claim-level provenance: the evidence register the memo cites is not shipped',
        'the partnerships manifest carries no git commit',
      ],
    },
  };
}

// ---- entry point --------------------------------------------------------------------------------------------------

/** True when the snapshot's subject was found in the bundles (the conclusion is not "missing"). */
export const snapshotFound = (s: CaseSnapshot): boolean => s.conclusion.value !== 'missing';

/** Build one case's snapshot from the decoded bundles. */
export function buildCaseSnapshot(def: Pick<CaseDefinition, 'kind' | 'key'>, b: DecodedBundles, ctx: SnapshotContext = {}): CaseSnapshot {
  switch (def.kind) {
    case 'posture_cell':
      return postureSnapshot(def.key, b, ctx, 'posture_cell');
    case 'sector':
      return postureSnapshot(def.key, b, ctx, 'sector');
    case 'aib_node':
      return aibNodeSnapshot(def.key, b, ctx);
    case 'compact':
      return compactSnapshot(def.key, b);
    case 'partnership':
      return partnershipSnapshot(def.key, b);
  }
}

/** The context and reveal facts of a case, split; a revealing id in context_fields is an error (blind-first). */
export function splitFacts(def: Pick<CaseDefinition, 'case_id' | 'context_fields' | 'reveal_fields'>, s: CaseSnapshot): { context: CaseFact[]; reveal: CaseFact[]; errors: string[] } {
  const errors: string[] = [];
  for (const id of def.context_fields) if (REVEALING_FACT_IDS.includes(id)) errors.push(`${def.case_id}: context field ${id} reveals the conclusion`);
  const pick = (ids: readonly string[]) => ids.map((id) => s.facts[id] ?? missing(id, 'not a fact of this case kind'));
  return { context: pick(def.context_fields), reveal: pick(def.reveal_fields), errors };
}
