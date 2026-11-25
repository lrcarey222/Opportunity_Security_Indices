// A synthetic UX v2 campaign for the case-review UI tests (test-only; no real reviewer data): one case per Explorer
// product, each with a neutral brief (snapshot.brief) and a reasoning reveal (snapshot.reasoning) in the shapes
// qc/brief.ts and qc/reasoning.ts build, plus one v1 case without a brief. Every text that may only appear after
// the blind answer carries the marker "zqv", so a test can prove none of it is in the page before the answer is
// stored and all of it is after.

import { answerOptions } from '../../qc/answerOptions.ts';
import { SUPPLY_STAGES, STAGE_DISPLAY } from '../../../lib/stages.ts';
import type { BriefFact, CaseBrief, CaseFact, CaseReasoning, CasesFile, ReviewCase } from '../../model.ts';
import { CASES, CAMPAIGN_ID, manifest, RELEASE, SHA } from './reviewFixture.ts';

const fact = (id: string, label: string, value: CaseFact['value'], display: string, extra: Partial<CaseFact> = {}): CaseFact => ({
  id, label, value, display, unit: null, source: null, source_year: null, note: null, ...extra,
});
const bf = (id: string, label: string, value: BriefFact['value'], display: string, extra: Partial<BriefFact> = {}): BriefFact => ({
  id, label, value, display, interpretation: null, source: 'UN Comtrade', vintage: '2024', warnings: [], estimate: false, ...extra,
});
const strip = (highlight: string[]) => ({ stages: SUPPLY_STAGES.map((s) => ({ key: s as string, label: STAGE_DISPLAY[s] })), highlight });

/** Texts that may only appear after the blind answer (each long and marked). */
export const V2_SECRET = {
  summary: 'zqv-summary: keep and strengthen an existing strength',
  needSentence: 'zqv-need: ranks above 73% of economies',
  rule: 'zqv-rule: high value and a top-quarter position',
  whyNot: 'zqv-whynot: position is top-quarter',
  wouldNeed: 'zqv-wouldneed: ranked below the top quarter',
  why: 'zqv-why: this case sits on the threshold',
  reason: 'zqv-reason in the Explorer words',
  revealFact: 'zqv-fact-display 0.7634',
  chainRest: 'zqv-chain: 39 economies in the coalition',
  coordination: 'zqv-coordination: existing links restore 62.7%',
  memoItem: 'zqv-memo: build magnets together',
  chokepoint: 'zqv-chokepoint: production more concentrated than trade',
  sectorDriver: 'zqv-driver: modules carry US$20bn at risk',
};
export const ALL_V2_SECRETS = Object.values(V2_SECRET);

const baseBrief = (over: Partial<CaseBrief>): CaseBrief => ({
  product: 'country',
  identity: { title: 'South Korea × Batteries × Component Manufacturing', parts: [] },
  decision_question: 'How should South Korea manage battery component manufacturing through 2035?',
  decision_context: 'This approximates a call by industrial-strategy officials. Relying on partners can be the best outcome.',
  objective: 'Resilient and affordable supply, not maximum national self-sufficiency.',
  time_horizon: { label: 'Through 2035', note: 'Market outlook 2024 to 2035.' },
  scenario: { key: 'baseline', label: "Today's network", description: 'Current trade network; no disruption is assumed.' },
  stage_location: strip(['Components']),
  evidence: [],
  assumptions: [],
  warnings: [],
  missing_evidence: [],
  definitions: [],
  ...over,
});

const reasoning = (over: Partial<CaseReasoning>): CaseReasoning => ({
  conclusion: { value: 'preserve', label: 'Preserve', summary: V2_SECRET.summary },
  steps: [],
  rule: V2_SECRET.rule,
  alternatives: [],
  compact_chain: null,
  partnership: null,
  sector: null,
  technical_path: null,
  ...over,
});

const common = {
  sector_groups: ['batteries'],
  country_groups: [] as string[],
  stage: null,
  assignment_group: 'batteries',
  anchor: false,
  snapshot_sha256: SHA(101),
  bundle: [],
  data_release: RELEASE,
  app_commit: 'test',
  built_at: '2026-11-20T00:00:00Z',
};

/** Country: Korea batteries components (Preserve), decision-use questions, a fragile robustness class. */
export const COUNTRY: ReviewCase = {
  ...common,
  case_id: 'CASE_101',
  kind: 'posture_cell',
  key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' },
  title: 'Korea: battery component manufacturing',
  question: 'How should South Korea manage battery component manufacturing through 2035?',
  country_groups: ['KOR'],
  stage: 'Components',
  decision_use: true,
  why_selected: V2_SECRET.why,
  explorer_path: '/countries/KOR?node=Batteries|Components',
  context_fields: ['home_share'],
  reveal_fields: ['choice', 'cp'],
  snapshot_sha256: SHA(101),
  answer_options: answerOptions('posture_cell'),
  snapshot: {
    conclusion: { value: 'preserve', label: 'Preserve', detail: 'Medium fence · Compete', reason: V2_SECRET.reason, confidence: 'moderate' },
    facts: {
      home_share: fact('home_share', "The economy's share of world supply", 0.062, '6.2%'),
      choice: fact('choice', 'Strategic choice', 'preserve', 'Preserve'),
      cp: fact('cp', 'Competitive Position among contenders (0-1)', 0.7634, V2_SECRET.revealFact),
    },
    trace: [
      { step: 'need', test: 'supply_chain_need', value: 0.732, threshold: 0.67, result: 'high' },
      { step: 'position', test: 'cp_contender_pct', value: 0.763, threshold: 0.75, result: 'high' },
      { step: 'choice', test: 'choice', value: null, threshold: null, result: 'preserve' },
    ],
    threat: null,
    robustness: {
      layer: 'bundle', case_key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }, baseline_choice: 'preserve', specs_run: 13,
      local_flips: [{ spec_id: 'cp_plus', kind: 'local', label: 'Position bar 0.8 (+0.05)', choice: 'catch_up', changed: true, fence_changed: false }],
      structural_flips: [], fence_change: false, class: 'fragile', drivers: ['cp_contender_pct'], note: null,
    },
    data_quality: { grade: 'medium', notes: ['import shares blended with current trade'] },
    brief: baseBrief({
      evidence: [
        bf('largest_supplier', 'Largest supplier of South Korea’s imports for battery components', 0.766, 'China, 76.6% of imports', { interpretation: 'a majority from one supplier', vintage: '2024 blended with the 12 months to Dec 2025' }),
        bf('own_world_share', 'South Korea’s share of world supply in battery components', 0.0617, '6.2%', {
          interpretation: 'measured share of world supply',
          source: 'Production data or export shares',
          vintage: 'latest year of each source',
          warnings: [{ id: 'measure_not_stated', level: 'info', title: 'Measure not stated', text: 'This release does not say whether production or exports were used.' }],
        }),
        bf('import_scale', 'Imports of the step as a share of GDP', null, '', { source: null, vintage: null }),
        bf('demand_outlook', 'World demand outlook for battery components', 3.01, '+301% from 2024 to 2035', { source: 'International Energy Agency World Energy Outlook 2025', vintage: '2024-2035' }),
      ],
      assumptions: [{ id: 'need_rank', label: 'Vulnerability is a rank, not a level', text: 'Import vulnerability is ranked against about 200 economies.', method_card_id: 'MC_007' }],
      warnings: [{ id: 'no_production_data', level: 'warn', title: 'Production evidence limited', text: 'Current capability is inferred primarily from trade data.' }],
      missing_evidence: ['Firm-level capacity is not captured.'],
      definitions: [{ term: 'Share of world supply', text: 'The share of world production where measured, else of exports.' }],
    }),
    reasoning: reasoning({
      steps: [
        { id: 'need', label: 'Need: import vulnerability', level: 'High', value: 0.732, threshold: 0.67, sentence: V2_SECRET.needSentence, drivers: ['largest supplier China 76.6% of imports'] },
        { id: 'prize', label: 'Prize: value of the industry', level: 'High', value: 0.787, threshold: 0.6, sentence: 'World demand grows about 13.5% a year to 2035.', drivers: [] },
        { id: 'position', label: 'Position: competitive standing', level: 'High (top quarter)', value: 0.763, threshold: 0.75, sentence: 'South Korea ranks 0.763 among the main producers.', drivers: [] },
      ],
      alternatives: [{ value: 'catch_up', label: 'Catch Up', why_not: V2_SECRET.whyNot, would_need: V2_SECRET.wouldNeed, margin: 'Position 0.763 vs bar 0.75 (+0.013)' }],
      technical_path: '/countries/KOR?node=Batteries|Components&tech=1',
    }),
  },
};

/** Sector (one allied step): rare-earth magnets under a disruption (Build capacity), threat questions. */
export const SECTOR_NODE: ReviewCase = {
  ...common,
  case_id: 'CASE_102',
  kind: 'aib_node',
  key: { node: 'rare-earth-permanent-magnets', scenario: 'decoupling' },
  title: 'Rare-earth magnets under a major-supplier disruption',
  question: 'What kind of allied response would be needed?',
  sector_groups: ['critical-minerals'],
  assignment_group: 'critical-minerals',
  threat: true,
  why_selected: 'zqv-why-node: a step with a large gap',
  explorer_path: '/compacts/AIB-C05?scn=decoupling#nodes',
  context_fields: [],
  reveal_fields: ['class'],
  snapshot_sha256: SHA(102),
  answer_options: answerOptions('aib_node'),
  snapshot: {
    conclusion: { value: 'BUILD', label: 'Build capacity', detail: null, reason: null, confidence: null },
    facts: { class: fact('class', 'Treatment class', 'BUILD', 'BUILD') },
    trace: [],
    threat: CASES[4].snapshot.threat,
    robustness: null,
    data_quality: { grade: 'High confidence', notes: [] },
    brief: baseBrief({
      product: 'sector',
      identity: { title: 'Rare-earth permanent magnets × Component Manufacturing × Major-supplier disruption', parts: [] },
      decision_question: 'If China stopped supplying rare-earth magnets to allied economies, what kind of allied response would be needed?',
      scenario: { key: 'decoupling', label: 'Major-supplier disruption', description: 'A major supplier stops supplying allied markets; the cut is one-way.' },
      time_horizon: { label: 'Immediate re-sourcing on 2025–26 trade', note: null },
      evidence: [bf('china_share_allied', 'China’s share of supply to allied economies', 0.618, '61.8%', { interpretation: 'a majority from one supplier', vintage: '2025–26' })],
    }),
    reasoning: reasoning({
      conclusion: { value: 'BUILD', label: 'Build capacity', summary: 'zqv-node-summary: coordinated trade still leaves a gap' },
      steps: [{ id: 'gap', label: 'Gap left after coordinated trade', level: 'Material', value: 0.607, threshold: 0.1, sentence: 'zqv-gap: 60.7% of allied demand is still unmet', drivers: [] }],
      rule: 'zqv-node-rule: the first test that matches decides',
      alternatives: [{ value: 'COORDINATE', label: 'Coordinate trade', why_not: 'zqv-node-whynot: coordination adds nothing', would_need: 'zqv-node-would: new links closing the gap', margin: null }],
    }),
  },
};

/** Compact: the battery chain under a disruption (Potential Sectoral Compact), decision-use and threat questions. */
export const COMPACT: ReviewCase = {
  ...common,
  case_id: 'CASE_103',
  kind: 'compact',
  key: { sid: 'SC-BATTERIES', scenario: 'decoupling' },
  title: 'An allied battery compact under a major-supplier disruption',
  question: 'Do allied economies need a standing arrangement across the battery chain?',
  why_selected: 'zqv-why-compact: the largest chain',
  explorer_path: '/compacts/s/SC-BATTERIES?scn=decoupling',
  context_fields: [],
  reveal_fields: [],
  snapshot_sha256: SHA(103),
  answer_options: answerOptions('compact', { sid: 'SC-BATTERIES' }),
  snapshot: {
    conclusion: { value: 'sectoral', label: 'Potential Sectoral Compact', detail: null, reason: null, confidence: null },
    facts: {},
    trace: [],
    threat: null,
    robustness: null,
    data_quality: { grade: null, notes: [] },
    brief: baseBrief({
      product: 'compact',
      identity: { title: 'Battery Materials & Cells × Major-supplier disruption', parts: [] },
      decision_question: 'Do allied economies need a standing multi-country arrangement across the battery chain, joint action on a few steps, or none?',
      stage_location: strip(['Extraction', 'Refining', 'Components', 'Assembly']),
      compact_chain: [
        { id: 'problem_today', label: 'Problem today', display: 'US$108.3bn of US$182.4bn a year', text: 'Allied economies would lose this much.', estimate: false },
        { id: 'existing_trade', label: 'What existing trade can cover', display: '42.0%', text: 'A model estimate.', estimate: true },
        { id: 'gap_after_existing', label: 'Remaining gap after existing links', display: '58.0% of allied demand', text: 'What existing links leave uncovered.', estimate: true },
      ],
      evidence: [bf('chain_imports', 'Allied economies’ imports across the chain', 182.4e9, 'US$182.4bn a year')],
    }),
    reasoning: reasoning({
      conclusion: { value: 'sectoral', label: 'Potential Sectoral Compact', summary: 'zqv-compact-summary: the lead mission carries 99.8%' },
      steps: [{ id: 'materiality', label: 'Size of the lead mission', level: 'Material', value: 0.998, threshold: 0.25, sentence: 'zqv-materiality: material at 25% or US$1bn', drivers: [] }],
      rule: 'zqv-compact-rule: material means a potential sectoral compact',
      alternatives: [{ value: 'narrower', label: 'Targeted action', why_not: 'zqv-compact-whynot: the share clears 25%', would_need: 'zqv-compact-would: below both bars', margin: 'share 99.8% vs 25%' }],
      compact_chain: [
        { id: 'new_capacity', label: 'New capacity required', display: 'zqv-new: 58.0% needs new capacity', text: 'Products needing new capacity.', estimate: true },
        { id: 'coalition', label: 'Proposed coalition', display: V2_SECRET.chainRest, text: 'Non-allied suppliers still relied on.', estimate: true },
      ],
    }),
  },
};

/** Partnership: United States and Japan (Build together). */
export const PARTNERSHIP: ReviewCase = {
  ...common,
  case_id: 'CASE_104',
  kind: 'partnership',
  key: { pair: 'USA_JPN' },
  title: 'The United States and Japan',
  question: 'What should lead cooperation between the United States and Japan?',
  sector_groups: [],
  country_groups: ['USA', 'JPN'],
  assignment_group: 'partnerships',
  why_selected: 'zqv-why-pair: the pilot memo',
  explorer_path: '/aib/partnerships?country=USA&partner=JPN',
  context_fields: [],
  reveal_fields: [],
  snapshot_sha256: SHA(104),
  answer_options: answerOptions('partnership'),
  snapshot: {
    conclusion: { value: 'preserve', label: 'Build together', detail: null, reason: null, confidence: null },
    facts: {},
    trace: [],
    threat: null,
    robustness: null,
    data_quality: { grade: 'unverified', notes: [] },
    brief: baseBrief({
      product: 'partnership',
      identity: { title: 'United States × Japan', parts: [] },
      decision_question: 'Over the next 12 to 36 months, what should lead cooperation between the United States and Japan?',
      time_horizon: { label: '12 to 36 months', note: null },
      scenario: { key: 'pair', label: 'Bilateral cooperation', description: 'Today’s trade and the allied stress test.' },
      stage_location: null,
      partnership: {
        a: { iso3: 'USA', name: 'United States', brings: ['gas liquids processing: 37.5% of world supply'], exposed_on: ['battery cells and packs: China supplies 56.3% of imports'] },
        b: { iso3: 'JPN', name: 'Japan', brings: ['wafers and PV cells: 9.8% of world supply'], exposed_on: [] },
        shared_problem: ['Rare-earth permanent magnets: China 61.7% of allied supply'],
      },
      evidence: [bf('memo_status', 'The memo behind this case', null, 'Rule-generated, not independently verified')],
    }),
    reasoning: reasoning({
      conclusion: { value: 'preserve', label: 'Build together', summary: 'zqv-pair-summary: the memo leads with build together' },
      steps: [{ id: 'box_preserve', label: 'Build together', level: '4 items', value: null, threshold: null, sentence: 'zqv-box: rare-earth oxides (joint)', drivers: [] }],
      rule: 'zqv-pair-rule: the memo applies fixed rules',
      alternatives: [{ value: 'partner', label: 'Trade and standards', why_not: 'zqv-pair-whynot: six products for joint capacity', would_need: 'zqv-pair-would: allied capacity adequate', margin: null }],
      partnership: {
        coordination_changes: [V2_SECRET.coordination],
        memo_choices: [
          { kind: 'preserve', label: 'Build together', items: [V2_SECRET.memoItem] },
          { kind: 'watch', label: 'Stay out', items: [] },
        ],
      },
    }),
  },
};

/** Sector chain: the solar chain under a disruption (lead stage Final Assembly & Equipment). */
export const SECTOR_CHAIN: ReviewCase = {
  ...common,
  case_id: 'CASE_105',
  kind: 'sector_chain',
  key: { sid: 'SC-SOLAR', scenario: 'decoupling' },
  title: 'Where in the solar chain to intervene',
  question: 'At which stages is allied intervention warranted?',
  sector_groups: ['solar'],
  assignment_group: 'solar',
  why_selected: 'zqv-why-chain: trade and production diverge',
  explorer_path: '/compacts/s/SC-SOLAR?scn=decoupling',
  context_fields: [],
  reveal_fields: ['lead_stage', 'explorer_stages'],
  snapshot_sha256: SHA(105),
  answer_options: answerOptions('sector_chain'),
  snapshot: {
    conclusion: { value: 'Assembly', label: 'Final Assembly & Equipment', detail: 'zqv-chain-detail: Refining, Components, Assembly', reason: null, confidence: null },
    facts: {
      lead_stage: fact('lead_stage', 'Stage to act on first', 'Assembly', 'zqv-lead Final Assembly & Equipment'),
      explorer_stages: fact('explorer_stages', 'Stages needing action', 'Refining;Components;Assembly', 'zqv-stages Refining, Components, Assembly'),
    },
    trace: [],
    threat: null,
    robustness: null,
    data_quality: { grade: null, notes: [] },
    brief: baseBrief({
      product: 'sector',
      identity: { title: 'Solar Manufacturing × Major-supplier disruption', parts: [] },
      decision_question: 'If China stopped supplying the solar chain, at which stages is allied intervention warranted?',
      scenario: { key: 'decoupling', label: 'Major-supplier disruption', description: 'A major supplier stops supplying allied markets.' },
      stage_location: strip(['Refining', 'Components', 'Assembly']),
      sector_chain: [
        { stage: 'Extraction', label: 'Extraction', facts: [] },
        { stage: 'Refining', label: 'Refining & Processing', facts: [bf('Refining_imports', 'Allied imports', 18e9, 'US$18.0bn a year')] },
        { stage: 'Assembly', label: 'Final Assembly & Equipment', facts: [bf('Assembly_imports', 'Allied imports', 29e9, 'US$29.7bn a year')] },
      ],
      evidence: [bf('chain_imports', 'Allied economies’ imports across the chain', 61.8e9, 'US$61.8bn a year')],
    }),
    reasoning: reasoning({
      conclusion: { value: 'Assembly', label: 'Final Assembly & Equipment', summary: 'zqv-chain-summary: the largest dollar gap is at assembly' },
      steps: [{ id: 'stage_Assembly', label: 'Final Assembly & Equipment', level: 'Needs allied action', value: 2e10, threshold: null, sentence: 'zqv-stage: 1 of 1 product needs action', drivers: [V2_SECRET.sectorDriver] }],
      rule: 'zqv-chain-rule: the lead stage is where the largest amount sits',
      alternatives: [{ value: 'Refining', label: 'Refining & Processing', why_not: 'zqv-chain-whynot: US$5.5bn against US$20.3bn', would_need: 'zqv-chain-would: production capacity set the exposure', margin: null }],
      sector: { explorer_stages: ['Refining & Processing', 'Component Manufacturing', 'Final Assembly & Equipment'], lead_stage: 'Final Assembly & Equipment', chokepoint_note: V2_SECRET.chokepoint },
    }),
  },
};

/** A v1 case built without a brief or reasoning (the old cases file shape). */
export const LEGACY: ReviewCase = { ...CASES[1], case_id: 'CASE_106', answer_options: answerOptions('posture_cell') };

export const V2_CASES: ReviewCase[] = [COUNTRY, SECTOR_NODE, COMPACT, PARTNERSHIP, SECTOR_CHAIN, LEGACY];

export function v2Files(): Record<string, unknown> {
  const m = manifest();
  m.campaigns[0] = { ...m.campaigns[0], n_cases: V2_CASES.length, anchor_cases: [] };
  const cases: CasesFile = { schema: 'aib-review-cases', schema_version: 1, campaign_id: CAMPAIGN_ID, data_release: RELEASE, built_at: '2026-11-20T00:00:00Z', app_commit: 'test', cases: V2_CASES };
  return { 'manifest.json': m, [`cases.${CAMPAIGN_ID}.json`]: cases };
}

/** Every reveal-only text of a case: the conclusion's words, the reasoning, the alternatives, the remainders. */
export function revealTexts(c: ReviewCase): string[] {
  const r = c.snapshot.reasoning;
  const k = c.snapshot.conclusion;
  const out = [k.reason, k.detail, c.why_selected];
  if (r) {
    out.push(r.conclusion.summary, r.rule);
    for (const st of r.steps) out.push(st.sentence, ...st.drivers);
    for (const a of r.alternatives) out.push(a.why_not, a.would_need);
    for (const s of r.compact_chain ?? []) out.push(s.display, s.text);
    for (const x of r.partnership?.coordination_changes ?? []) out.push(x);
    for (const m of r.partnership?.memo_choices ?? []) out.push(...m.items);
    if (r.sector?.chokepoint_note) out.push(r.sector.chokepoint_note);
  }
  return out.filter((x): x is string => typeof x === 'string' && x.length > 12);
}
