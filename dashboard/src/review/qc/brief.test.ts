// Tests of the neutral case brief (qc/brief.ts) and its leak lint (qc/briefLint.ts), UX v2: per product the
// brief carries the true horizon and scenario, the six-stage strip, 5-8 facts each with a source and a vintage, and
// data warnings computed from the data; nothing in it names the conclusion. The lint catches every leak type and
// passes all 23 built cases of the shipped campaign. Pure functions over synthetic and shipped bundles.

import { describe, expect, it } from 'vitest';
import { existsSync, readFileSync } from 'node:fs';
import { CHOICE_NAME } from '../../content/vocabulary.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import { bareAcronymsInText } from '../../components/testing/acronyms.ts';
import type { CaseBrief, CaseDefinition, CasesFile } from '../model.ts';
import { answerOptions } from './answerOptions.ts';
import { ASSUMPTIONS, BRIEF_FACT_IDS_BY_KIND, buildCaseBrief } from './brief.ts';
import { briefTexts, lintAuthoredText, lintBriefNeutrality } from './briefLint.ts';
import { annualRate, cleanNote, flagWords, hhiWords, parsePrizeNote, rcaWords, supplierShareWords } from './evidence.ts';
import { buildCaseSnapshot } from './snapshot.ts';
import { HAVE_REAL, loadReal, mkDecoded, mkPosture, REAL_DATA_DIR, triageTrace } from './__tests__/fixtures.ts';

type Def = Pick<CaseDefinition, 'kind' | 'key' | 'brief' | 'title'>;
const def = (kind: CaseDefinition['kind'], key: CaseDefinition['key']): Def => ({ kind, key, title: 't' });

describe('generic interpretation conventions (never the Explorer bars)', () => {
  it('HHI bands, supplier shares, RCA classes and annual rates', () => {
    expect(hhiWords(0.05)).toMatch(/unconcentrated/);
    expect(hhiWords(0.15)).toMatch(/moderately/);
    expect(hhiWords(0.4)).toMatch(/highly/);
    expect(supplierShareWords(0.766)).toBe('a majority from one supplier');
    expect(supplierShareWords(0.3)).toBe('one large supplier');
    expect(rcaWords(3.48)).toMatch(/medium/);
    expect(annualRate(301, 11)).toBeCloseTo(13.5, 1);
  });
  it('parses the prize note into a source and a vintage, and cleans deployment notes', () => {
    const o = parsePrizeNote('World demand +301% 2024-35 (WEO 2025 STEPS); capex +29% 2021-22 to 2024-25 (CIM).')!;
    expect(o).toMatchObject({ totalPct: 301, fromYear: 2024, toYear: 2035, capexPct: 29 });
    expect(o.source).toMatch(/World Energy Outlook 2025, Stated Policies/);
    expect(parsePrizeNote('World demand +185% 2024-35 (IEA minerals STEPS).')!.source).toMatch(/Critical Minerals/);
    expect(cleanNote('demand_growth: WEO Current Policies 2024-2035: 1.68%/yr | output_growth: 815 -> 834 TWh')).toBe(
      'demand growth: International Energy Agency World Energy Outlook 2025 Current Policies 2024-2035: 1.68% a year; output growth: 815 to 834 TWh',
    );
    expect(flagWords('production more concentrated than trade: CHN holds 97% of GCIM operational capacity: Solar Polysilicon vs 10% of exports, so ...', () => 'China')).toBe(
      'Production is more concentrated than trade: China holds 97% of operational production capacity, against 10% of exports.',
    );
  });
});

describe('buildCaseBrief on synthetic bundles', () => {
  const p = mkPosture([
    {
      iso3: 'KOR', tech: 'Batteries', stage: 'Components', choice: 'preserve', need: 0.73, prize: 0.79, cp: 0.76, share: 0.06, pathway: 'yes',
      trace: triageTrace({ need: 0.73, prize: 0.79, cp: 0.76, pos: 'high', choice: 'preserve' }),
      extra: {
        top_supplier: 'CHN', top_supplier_share: 0.77, china_share: 0.77, coercive_import_share: 0.86, trusted_share: 0.63, coercive_top: 'CHN', coercive_top_share: 0.27,
        imports_to_gdp: 0.00015, prize: { value: 0.79, measure: 'strategic_value', market: 0.85, note: 'World demand +301% 2024-35 (WEO 2025 STEPS); capex +29% 2021-22 to 2024-25 (CIM).' },
        trade: { benchmark_share: 0.107, benchmark_rca: 3.48, benchmark_exports: 6.23e8, exports_current: 6.9e8, exports_previous: 4.7e8 },
        cp_families: { export_position: 0.87 },
      },
    },
  ]);
  const b = mkDecoded({ posture: p });
  const brief = buildCaseBrief(def('posture_cell', { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }), b);
  it('the country brief: identity, true horizon and scenario, the stage strip, 5-8 sourced facts', () => {
    expect(brief.product).toBe('country');
    expect(brief.identity.title).toBe(`KOR × Batteries × ${STAGE_DISPLAY.Components}`);
    expect(brief.time_horizon.label).toBe('Through 2035');
    expect(brief.time_horizon.note).toMatch(/2024 to 2035/);
    expect(brief.scenario).toMatchObject({ key: 'baseline' });
    expect(brief.scenario!.description).toMatch(/no disruption is assumed/);
    expect(brief.scenario!.description).toMatch(/Stated Policies/);
    expect(brief.stage_location!.stages.map((s) => s.key)).toEqual([...SUPPLY_STAGES]);
    expect(brief.stage_location!.highlight).toEqual(['Components']);
    expect(brief.evidence.length).toBeGreaterThanOrEqual(5);
    expect(brief.evidence.length).toBeLessThanOrEqual(8);
    for (const f of brief.evidence) {
      expect(f.source, f.id).toBeTruthy();
      expect(f.vintage, f.id).toBeTruthy();
    }
    // the largest supplier is China, so the China share gives way to the wider group share
    expect(brief.evidence.map((f) => f.id)).toContain('rival_group_import_share');
    expect(brief.evidence.map((f) => f.id)).not.toContain('china_import_share');
  });
  it('computes warnings from the data: trade-only position, import periods, older groups', () => {
    const ids = brief.warnings.map((w) => w.id);
    expect(ids).toContain('trade_only');
    expect(ids).toContain('import_periods');
    expect(ids).toContain('older_groups');
  });
  it('authored text and evidence ids override the defaults', () => {
    const a = buildCaseBrief(
      { ...def('posture_cell', { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }), brief: { decision_question: 'Q?', decision_context: 'C.', objective: 'O.', evidence_ids: ['own_world_share', 'export_position', 'export_trend', 'import_scale', 'demand_outlook'], assumption_ids: ['need_rank'] } },
      b,
    );
    expect(a).toMatchObject({ decision_question: 'Q?', decision_context: 'C.', objective: 'O.' });
    expect(a.evidence.map((f) => f.id)).toEqual(['own_world_share', 'export_position', 'export_trend', 'import_scale', 'demand_outlook']);
    expect(a.assumptions.map((x) => x.id)).toEqual(['need_rank']);
  });
  it('vocabularies: every assumption has text; the brief fact ids are per kind', () => {
    for (const a of Object.values(ASSUMPTIONS)) expect(a.text.length).toBeGreaterThan(20);
    expect(BRIEF_FACT_IDS_BY_KIND.sector_chain).toContain('production_divergence');
    expect(BRIEF_FACT_IDS_BY_KIND.partnership).toContain('memo_status');
  });
});

describe('lintBriefNeutrality catches each leak type', () => {
  const base = (): CaseBrief => ({
    product: 'country', identity: { title: 'Korea × Batteries', parts: [] }, decision_question: 'How should Korea manage it?', decision_context: 'Neutral.', objective: 'Resilient supply.',
    time_horizon: { label: 'Through 2035' }, scenario: null, stage_location: null,
    evidence: [{ id: 'own_world_share', label: 'World share', value: 0.06, display: '6.2%', interpretation: null, source: 's', vintage: 'v' }],
    assumptions: [], warnings: [], missing_evidence: [], definitions: [],
  });
  const opts = answerOptions('posture_cell');
  const concl = { value: 'catch_up', label: CHOICE_NAME.catch_up };
  it('a neutral brief passes', () => expect(lintBriefNeutrality(base(), concl, opts)).toEqual([]));
  const leak = (patch: (b: CaseBrief) => void, re: RegExp) => {
    const b = base();
    patch(b);
    const e = lintBriefNeutrality(b, concl, opts);
    expect(e.join('\n')).toMatch(re);
  };
  it('the conclusion label or key', () => {
    leak((b) => (b.decision_context = 'A case for Catch Up.'), /conclusion label "Catch Up"/);
    leak((b) => (b.evidence[0].interpretation = 'reads as catch_up'), /conclusion key "catch_up"/);
  });
  it('any canonical choice, class, compact decision or memo-box label', () => {
    leak((b) => (b.objective = 'Preserve the industry.'), /"Preserve"/);
    leak((b) => (b.warnings = [{ id: 'w', level: 'warn', title: 'Low intervention likely', text: 'x' }]), /"Low intervention"/);
    leak((b) => (b.definitions = [{ term: 'BUILD', text: 'x' }]), /"BUILD"/);
    leak((b) => (b.missing_evidence = ['A Targeted action is possible.']), /"Targeted action"/);
    leak((b) => (b.partnership = { a: { iso3: 'USA', name: 'US', brings: ['Build together on magnets'], exposed_on: [] }, b: { iso3: 'JPN', name: 'JP', brings: [], exposed_on: [] }, shared_problem: [] }), /"Build together"/);
    leak((b) => (b.compact_chain = [{ id: 'x', label: 'Keep open', display: 'x', text: 'x', estimate: false }]), /keep open/i);
  });
  it('model score ids and names, and outcome-giving model labels', () => {
    leak((b) => (b.evidence[0].id = 'cp'), /model score or decision field/);
    leak((b) => (b.evidence[0].label = 'cp_contender_pct of 0.76'), /model field "cp_contender_pct"/);
    leak((b) => (b.evidence[0].label = 'Competitive Position 0.76'), /model score "Competitive Position"/);
    leak((b) => (b.warnings = [{ id: 'w', level: 'warn', title: 'PHYSICAL CAPABILITY NOT VALIDATED', text: 'x' }]), /PHYSICAL CAPABILITY NOT VALIDATED/);
    leak((b) => (b.evidence[0].label = 'Largest coercive supplier'), /"coercive"/);
    leak((b) => (b.decision_question = 'Which posture fits?'), /"posture"/);
  });
  it('claims about the Explorer’s answer and banned words', () => {
    leak((b) => (b.decision_context = 'The Explorer recommends building.'), /states what the Explorer concludes/);
    leak((b) => (b.decision_context = 'A chance to leverage strengths.'), /banned word/);
  });
  it('stage names are exempt for a sector chain, whose options are the stages', () => {
    const b = base();
    b.sector_chain = [{ stage: 'Assembly', label: STAGE_DISPLAY.Assembly, facts: [] }];
    expect(lintBriefNeutrality(b, { value: 'Assembly', label: STAGE_DISPLAY.Assembly }, answerOptions('sector_chain'))).toEqual([]);
  });
  it('authored text is linted before any build', () => {
    expect(lintAuthoredText([['CASE_X.question', 'Which strategic posture is closest?']]).join()).toMatch(/CASE_X.question: uses the model label "posture"/);
    expect(lintAuthoredText([['CASE_X.question', 'How should Korea manage battery components through 2035?']])).toEqual([]);
  });
  it('briefTexts walks every reviewer-facing text', () => {
    const b = base();
    b.sector_chain = [{ stage: 'Refining', label: 'R', facts: [{ id: 'f', label: 'L', value: 1, display: 'D', interpretation: 'I', source: 'S', vintage: 'V', warnings: [{ id: 'w', level: 'warn', title: 'T', text: 'X' }] }] }];
    const where = briefTexts(b).map(([w]) => w);
    expect(where).toContain('sector_chain.Refining.f.warnings[0].text');
    expect(where).toContain('decision_question');
  });
});

const CASES_FILE = `${REAL_DATA_DIR}review/cases.aib_qc_2027_round1.json`;

describe.skipIf(!HAVE_REAL)('briefs of the shipped campaign', () => {
  const { decoded } = loadReal();
  const brief = (kind: CaseDefinition['kind'], key: CaseDefinition['key']) => buildCaseBrief(def(kind, key), decoded, buildCaseSnapshot({ kind, key }, decoded));
  it('country: KOR battery components, through 2035, today’s network, Component Manufacturing highlighted', () => {
    const b = brief('posture_cell', { iso3: 'KOR', tech: 'Batteries', stage: 'Components' });
    expect(b.identity.title).toBe('South Korea × Batteries × Component Manufacturing');
    expect(b.time_horizon.note).toMatch(/12 months to Dec 2025/);
    expect(b.scenario!.description).toMatch(/no disruption/);
    expect(b.evidence.find((f) => f.id === 'largest_supplier')!.display).toBe('China, 76.6% of imports');
  });
  it('deployment: USA nuclear names current policies and 2030 targets, and the missing target family', () => {
    const b = brief('posture_cell', { iso3: 'USA', tech: 'Nuclear', stage: 'Deployment' });
    expect(b.time_horizon.label).toBe('Through 2035, against 2030 national targets');
    expect(b.scenario!.key).toBe('current_policies');
    expect(b.evidence.map((f) => f.id)).toEqual(['installed_trend', 'deployment_demand', 'target_gap', 'system_need', 'climate_context']);
    expect(b.warnings.map((w) => w.id)).toContain('families_missing');
    expect(b.evidence.find((f) => f.id === 'climate_context')!.interpretation).toMatch(/never used/);
  });
  it('the entrepot and the zero same-group share are flagged', () => {
    expect(brief('posture_cell', { iso3: 'BEL', tech: 'Oil', stage: 'Extraction' }).warnings.map((w) => w.id)).toContain('transit_hub');
    const aus = brief('posture_cell', { iso3: 'AUS', tech: 'Batteries', stage: 'Extraction' });
    expect(aus.evidence.find((f) => f.id === 'same_group_world_share')!.warnings!.map((w) => w.id)).toContain('zero_same_group');
    expect(aus.warnings.map((w) => w.id)).toContain('trade_production_divergence');
    // the demand reading no longer presents the investment figure as the reading of demand
    expect(aus.evidence.find((f) => f.id === 'demand_outlook')!.interpretation ?? '').not.toMatch(/^world manufacturing/);
  });
  it('sector (AIB node): the bundle trade period, not 2024; China removed one-way; no physical warning', () => {
    const b = brief('aib_node', { node: 'polysilicon', scenario: 'decoupling' });
    expect(b.product).toBe('sector');
    expect(b.time_horizon.label).toContain(decoded.aib!.doc.meta.tradePeriod!);
    expect(b.scenario!.description).toMatch(/keeps buying/);
    expect(b.warnings.map((w) => w.id)).toContain('production_divergence');
    expect(JSON.stringify(b)).not.toMatch(/PHYSICAL CAPABILITY/);
    // walkthrough 2026-11-21: "cover the main producers" alone hid that exporters with 15% of world trade are missing
    expect(b.evidence.find((f) => f.id === 'production_data')!.interpretation).toMatch(/^exporters holding 15% of world trade are not in the production data/);
    const m = brief('aib_node', { node: 'li-ion-cells-and-packs', scenario: 'managed' });
    expect(m.scenario!.key).toBe('managed');
    expect(m.evidence.find((f) => f.id === 'shift_share')!.estimate).toBe(true);
  });
  it('compact: three pre-blind chain links, estimates labelled, no purpose or coalition', () => {
    const b = brief('compact', { sid: 'SC-BATTERIES', scenario: 'decoupling' });
    expect(b.product).toBe('compact');
    expect(b.compact_chain!.map((s) => s.id)).toEqual(['problem_today', 'existing_trade', 'gap_after_existing']);
    expect(b.compact_chain!.slice(1).every((s) => s.estimate)).toBe(true);
    const sc = decoded.aib!.doc.strategic!.compacts.find((c) => c.id === 'SC-BATTERIES')!;
    expect(JSON.stringify(b)).not.toContain(sc.purpose!);
    expect(b.warnings.find((w) => w.id === 'untested')!.text).toBe('1 product has too little trade data to test.');
  });
  it('sector chain: per-stage facts for every stage, stages of the chain highlighted', () => {
    const b = brief('sector_chain', { sid: 'SC-SOLAR', scenario: 'decoupling' });
    expect(b.sector_chain!.map((s) => s.stage)).toEqual([...SUPPLY_STAGES]);
    expect(b.stage_location!.highlight).toEqual(['Refining', 'Components', 'Assembly']);
    expect(b.sector_chain!.find((s) => s.stage === 'Refining')!.facts.length).toBeGreaterThan(3);
    expect(b.sector_chain!.find((s) => s.stage === 'Extraction')!.facts).toEqual([]);
  });
  it('partnership: both sides from posture data, the shared problem from the allied market, memo warnings', () => {
    const b = brief('partnership', { pair: 'USA_JPN' });
    expect(b.partnership!.a.name).toBe('United States');
    expect(b.partnership!.b.name).toBe('Japan');
    expect(b.partnership!.a.brings.length).toBeGreaterThan(0);
    expect(b.partnership!.b.exposed_on.length).toBeGreaterThan(0);
    expect(b.partnership!.shared_problem.join(' ')).toMatch(/Rare-earth permanent magnets/);
    // walkthrough 2026-11-21: Japan's rare-earth refining qualifies on the rival-group share, not on China's 42.6%
    const ree = b.partnership!.shared_problem.find((x) => x.startsWith('rare-earth and wind materials refining'))!;
    expect(ree).toMatch(/Japan: China 42\.6%, China, Russia and the other UN-voting group \d+\.\d%\)$/);
    expect(b.warnings.map((w) => w.id)).toEqual(expect.arrayContaining(['memo_unverified', 'memo_older']));
  });
});

describe.skipIf(!existsSync(CASES_FILE))('the built campaign', () => {
  const file = JSON.parse(readFileSync(CASES_FILE, 'utf8')) as CasesFile;
  it('has 23 cases, each with a brief that passes the lint, 5-8 sourced facts and a reasoning reveal', () => {
    expect(file.cases).toHaveLength(23);
    for (const c of file.cases) {
      const b = c.snapshot.brief!;
      expect(lintBriefNeutrality(b, c.snapshot.conclusion, c.answer_options), c.case_id).toEqual([]);
      expect(b.evidence.length, c.case_id).toBeGreaterThanOrEqual(5);
      expect(b.evidence.length, c.case_id).toBeLessThanOrEqual(8);
      for (const f of b.evidence) expect(!!f.source && !!f.vintage, `${c.case_id} ${f.id}`).toBe(true);
      expect(c.snapshot.reasoning!.alternatives.length, c.case_id).toBeGreaterThan(0);
      // no bare acronym in the short labels a heading or label would show
      for (const t of [b.identity.title, ...b.evidence.map((f) => f.label), ...b.warnings.map((w) => w.title)]) expect(bareAcronymsInText(t), `${c.case_id}: ${t}`).toEqual([]);
    }
    expect(file.cases.filter((c) => c.kind === 'sector_chain').map((c) => c.case_id)).toEqual(['CASE_022', 'CASE_023']);
  });
});
