// The posture bundle contract, schema 2 (design section 3): the columnar core decoded and checked (a v1 core
// refused, the framework's invariants, the vocabularies, evidence and confidence normalised once), the meta
// (thresholds, scenario labels, stage display names, node phrases), the allied division's extended gap
// vocabulary, and the detail document normalised from either spelling of its fields (detail.ts).

import { describe, expect, it } from 'vitest';
import { STAGE_DISPLAY } from '../../lib/stages.ts';
import { checkPostureDetail, decodeCore, normalizeAlignmentConfidence, normalizeConfidence, normalizeEvidence, PostureContractError } from '../contract.ts';
import { ratioVsCurrentWords } from '../detail.ts';
import { fixtureCore, fixtureDetail, fixtureStore } from './fixture.ts';

type Cols = Record<string, unknown[]>;
const cellsOf = (doc: Record<string, unknown>) => doc.cells as Cols;
/** The index of one fixture cell (economy, technology, stage indices) in the columnar document. */
const at = (doc: Record<string, unknown>, c: number, t: number, s: number) => {
  const k = cellsOf(doc);
  return k.c.findIndex((_, i) => k.c[i] === c && k.t[i] === t && k.s[i] === s);
};
const problemsOf = (f: () => unknown): string[] => {
  try {
    f();
  } catch (e) {
    if (e instanceof PostureContractError) return e.problems;
    throw e;
  }
  return [];
};

describe('the core, schema 2', () => {
  it('decodes the columnar core into six-stage cells with every choice of the framework', () => {
    const core = decodeCore(fixtureCore());
    expect(core.cells).toHaveLength(36);
    expect(new Set(core.cells.map((c) => c.stage)).size).toBe(6);
    const a = (tech: string, stage: string) => core.cells.find((c) => c.iso3 === 'AAA' && c.tech === tech && c.stage === stage)!;
    expect([a('Batteries', 'Refining').posture, a('Batteries', 'Refining').rationale]).toEqual(['S', 'coercive_chokepoint']);
    expect([a('Solar', 'Refining').rationale, a('Solar', 'Components').rationale]).toEqual(['systemic_resilience', 'operational_control']);
    expect([a('Batteries', 'Extraction').mode, a('Batteries', 'Components').mode, a('Batteries', 'Assembly').mode]).toEqual(['partner', 'catch_up', 'preserve']);
    expect([a('Batteries', 'Deployment').posture, a('Batteries', 'Deployment').deployMode, a('Batteries', 'Deployment').needType]).toEqual(['D', 'accelerate', 'deployment']);
    expect([a('Solar', 'Extraction').deployMode, a('Solar', 'Extraction').needType]).toEqual(['watch', 'supply_chain']);
    const na = a('Batteries', 'Recovery');
    expect([na.posture, na.mode, na.deployMode, na.need, na.prize]).toEqual(['N', null, null, null, 0.3]);
    expect(a('Solar', 'Assembly').flag).toBe('review_strong_position_low_prize');
    expect(a('Batteries', 'Components').pathway).toBe('yes');
    expect([a('Batteries', 'Extraction').partner1, a('Batteries', 'Extraction').partnerGap, a('Batteries', 'Extraction').momentum]).toEqual(['BBB', false, 'up']);
    expect(core.division.map((d) => `${d.tech}|${d.stage}:${d.gap}`)).toEqual([
      'Batteries|Extraction:none', 'Batteries|Refining:insufficient_redundancy', 'Batteries|Components:single_point_of_failure', 'Solar|Assembly:no_trusted_supplier',
    ]);
    expect(core.division[1].backups.map((b) => b.iso3)).toEqual(['AAA']);
  });

  it('normalises evidence once: high / medium / low / none -> strong / moderate / weak, the raw word kept', () => {
    const core = decodeCore(fixtureCore());
    const a = (tech: string, stage: string) => core.cells.find((c) => c.iso3 === 'AAA' && c.tech === tech && c.stage === stage)!;
    expect([a('Batteries', 'Extraction').evidence, a('Batteries', 'Extraction').evidenceRaw]).toEqual(['strong', 'high']);
    expect([a('Batteries', 'Assembly').evidence, a('Batteries', 'Assembly').evidenceRaw]).toEqual(['weak', 'low']);
    expect([a('Solar', 'Recovery').evidence, a('Solar', 'Recovery').evidenceRaw]).toEqual(['weak', 'none']);
    expect(a('Batteries', 'Recovery').evidence).toBeNull();
    // the medium / moderate mismatch of the v1 grammar is gone: both words read as moderate
    expect([normalizeEvidence('medium'), normalizeEvidence('moderate'), normalizeEvidence('Strong'), normalizeEvidence(null), normalizeEvidence('excellent')]).toEqual([
      'moderate', 'moderate', 'strong', null, undefined,
    ]);
    expect([normalizeConfidence('medium'), normalizeConfidence('moderate'), normalizeConfidence('x')]).toEqual(['moderate', 'moderate', undefined]);
    expect([normalizeAlignmentConfidence('moderate'), normalizeAlignmentConfidence('medium'), normalizeAlignmentConfidence('low')]).toEqual(['medium', 'medium', 'low']);
  });

  it('reads the meta: thresholds, scenario labels, stage display names, node phrases, provenance', () => {
    const core = decodeCore(fixtureCore());
    expect(core.meta.thresholds).toEqual({ need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.67, borderline_band: 0.05 });
    expect(core.meta.scenario_labels).toEqual({ current_policy: 'Current policy', national_commitments: 'National commitments', climate_aligned: 'Climate-aligned' });
    expect(core.meta.family_scenarios).toEqual({ demand: 'current_policy', policy_gap: 'national_commitments', system: null });
    expect(core.meta.stage_display).toEqual(STAGE_DISPLAY);
    expect(core.meta.phrases['Batteries|Refining']).toBe('battery materials refining');
    expect([core.created, core.gitCommit]).toEqual(['2026-10-29T00:00:00Z', 'abc1234']);
    // flat threshold keys are read too; a stage display name in the meta wins
    const flat = fixtureCore();
    const m = flat.meta as Record<string, unknown>;
    delete m.thresholds;
    Object.assign(m, { need_threshold: 0.7, opportunity_threshold: 0.55, cp_threshold: 0.8, deployment_threshold: 0.6, borderline_band: 0.04, stage_display: { Refining: 'Refining' } });
    const f = decodeCore(flat).meta;
    expect(f.thresholds).toEqual({ need: 0.7, prize: 0.55, cp: 0.8, deployment: 0.6, borderline_band: 0.04 });
    expect([f.stage_display.Refining, f.stage_display.Assembly]).toEqual(['Refining', 'Final Assembly & Equipment']);
  });

  it('refuses a v1 core, a newer one, three-stage data and missing thresholds', () => {
    expect(() => decodeCore({ ...fixtureCore(), schema_version: 1 })).toThrow(/retired Need x Opportunity posture/);
    expect(() => decodeCore({ ...fixtureCore(), schema_version: 9 })).toThrow(/newer app/);
    expect(() => decodeCore({ ...fixtureCore(), schema: 'nope' })).toThrow(PostureContractError);
    const three = fixtureCore();
    (three.meta as { stages: string[] }).stages = ['Upstream', 'Midstream', 'Downstream'];
    expect(() => decodeCore(three)).toThrow(/not the six stages/);
    const noT = fixtureCore();
    delete (noT.meta as Record<string, unknown>).thresholds;
    expect(problemsOf(() => decodeCore(noT))).toContain('core: meta.thresholds.need is missing');
  });

  it('refuses a core whose required columns are missing or of the wrong length; optional ones may be absent', () => {
    const noDm = fixtureCore();
    delete cellsOf(noDm).dm;
    expect(problemsOf(() => decodeCore(noDm))).toContain('core: cells.dm is missing');
    const short = fixtureCore();
    cellsOf(short).ev = cellsOf(short).ev.slice(1);
    expect(problemsOf(() => decodeCore(short))).toContain('core: cells.ev has 35 values, not 36');
    const bare = fixtureCore();
    for (const k of ['v', 'sec', 'share', 'pw', 'bd', 'conf', 'fl', 'ev', 'al', 'alc', 'alf', 'p1', 'gap', 'mom', 'mq']) delete cellsOf(bare)[k];
    const core = decodeCore(bare);
    const s = core.cells.find((c) => c.iso3 === 'AAA' && c.stage === 'Refining' && c.tech === 'Batteries')!;
    expect([s.posture, s.rationale, s.evidence, s.alignment, s.needType]).toEqual(['S', null, null, null, 'supply_chain']);
  });

  it('refuses the choices the framework cannot make (spec VIII) and unknown words', () => {
    const cases: [string, (d: Record<string, unknown>) => void][] = [
      ['a Compete node without its mode alone', (d) => (cellsOf(d).m[at(d, 0, 0, 0)] = null)],
      ['a Deploy node without its deploy mode alone', (d) => (cellsOf(d).m[at(d, 0, 1, 0)] = 'partner')],
      ['a Secure or not-assessed node with a mode', (d) => (cellsOf(d).dm[at(d, 0, 0, 5)] = 'watch')],
      ['a Compete node at Deployment', (d) => Object.assign(cellsOf(d), { p: cellsOf(d).p.map((x, i) => (i === at(d, 0, 0, 4) ? 'C' : x)), m: cellsOf(d).m.map((x, i) => (i === at(d, 0, 0, 4) ? 'partner' : x)), dm: cellsOf(d).dm.map((x, i) => (i === at(d, 0, 0, 4) ? null : x)) })],
      ['Accelerate outside Deployment', (d) => (cellsOf(d).dm[at(d, 0, 1, 0)] = 'accelerate')],
      ['an unknown posture or index', (d) => (cellsOf(d).p[0] = 'X')],
      ['an unknown compete mode', (d) => (cellsOf(d).m[at(d, 0, 0, 0)] = 'grow')],
      ['an unknown Secure rationale', (d) => (cellsOf(d).sec[at(d, 0, 0, 1)] = 'reshore')],
      ['an unknown pathway value', (d) => (cellsOf(d).pw[0] = 'maybe')],
      ['an unknown policy alignment', (d) => (cellsOf(d).al[0] = 'great')],
      ['an unknown evidence word', (d) => (cellsOf(d).ev[0] = 'excellent')],
      ['an unknown alignment confidence', (d) => (cellsOf(d).alc[0] = 'certain')],
    ];
    for (const [why, edit] of cases) {
      const doc = fixtureCore();
      edit(doc);
      expect(problemsOf(() => decodeCore(doc)).join(' | '), why).toContain(why);
    }
  });

  it('the allied division: the extended gap vocabulary is read, an unknown gap refused', () => {
    const doc = fixtureCore();
    const d = doc.division as Cols;
    d.gap[0] = 'excessive_concentration';
    d.gaps[0] = 'excessive_concentration;insufficient_allied_capacity';
    expect(decodeCore(doc).division[0].gaps).toEqual(['excessive_concentration', 'insufficient_allied_capacity']);
    d.gap[0] = 'too_few';
    expect(() => decodeCore(doc)).toThrow(/division gap 'too_few' is not known/);
  });
});

describe('the detail document, schema 2', () => {
  const det = checkPostureDetail(fixtureDetail());
  const node = (key: string) => det.cells.find((c) => c.key === key)!;

  it('checks the header and every cell (six-stage stage, a schema-2 posture word)', () => {
    expect(det.cells).toHaveLength(12);
    expect([det.iso3, det.country]).toEqual(['AAA', 'Aland']);
    const mid = fixtureDetail();
    (mid.cells as { stage: string }[])[0].stage = 'Midstream';
    expect(() => checkPostureDetail(mid)).toThrow(PostureContractError);
    const v1 = fixtureDetail();
    (v1.cells as { posture: string }[])[0].posture = 'grow';
    expect(() => checkPostureDetail(v1)).toThrow(/not_assessed/);
    expect(() => checkPostureDetail({ ...fixtureDetail(), schema_version: 1 })).toThrow(/retired/);
  });

  it('reads the supply-chain families in either spelling, and none at Deployment', () => {
    const ext = node('Batteries|Extraction').supplyChain!;
    expect([ext.importDependence, ext.supplierConcentration, ext.riskyExposure, ext.trustedAlternatives]).toEqual([0.9, 0.7, 0.6, 0.5]);
    const ref = node('Batteries|Refining').supplyChain!;
    expect([ref.importDependence, ref.supplierConcentration, ref.riskyExposure, ref.trustedAlternatives]).toEqual([0.95, 0.9, 0.92, null]);
    expect([ref.substitutability, ref.reconstitution, ref.inventories, ref.coerciveTop, ref.coerciveTopShare]).toEqual(['hard', 'y1to3', 'unknown', 'XXX', 0.7]);
    expect(node('Batteries|Deployment').supplyChain).toBeNull();
    expect(node('Batteries|Extraction').deployment).toBeNull();
  });

  it('reads the deployment families with basis and scenario, nested or flat; missing evidence stays null, never zero', () => {
    const dn = node('Batteries|Deployment').deployment!;
    expect(dn.demand).toEqual({ value: 0.9, basis: 'national', scenario: 'current_policy', note: null, unit: null });
    expect(dn.policyGap).toEqual({ value: 0.8, basis: 'national', scenario: 'national_commitments', note: null, unit: null });
    expect(dn.system).toEqual({ value: null, basis: 'not_assessed', scenario: null, note: null, unit: null });
    // the climate-aligned alternative is a ratio to current policies (R unit), shown as such, never as a score
    expect(dn.alternative).toEqual({ value: 1.04, basis: 'global_context', scenario: 'climate_aligned', note: 'World 2035, Net Zero vs Current Policies', unit: 'ratio_vs_current_policies' });
    expect(ratioVsCurrentWords(dn.alternative)).toBe('+104% vs current policies');
    expect(ratioVsCurrentWords({ value: -0.8, unit: 'ratio_vs_current_policies' })).toBe('-80% vs current policies');
    expect(ratioVsCurrentWords({ value: 0, unit: 'ratio_vs_current_policies' })).toBe('0% vs current policies');
    expect(ratioVsCurrentWords(dn.demand)).toBeNull();
    expect(ratioVsCurrentWords({ value: null, unit: 'ratio_vs_current_policies' })).toBeNull();
    expect(dn.trend).toBe('rising');
    const solar = node('Solar|Deployment').deployment!;
    expect([solar.demand.value, solar.demand.basis, solar.system.value, solar.alternative, solar.trend]).toEqual([0.4, 'observed_trend', 0.5, null, 0.08]);
  });

  it('reads the prize, position families, pathway, trace, policy and partners', () => {
    const com = node('Batteries|Components');
    expect(com.prize).toMatchObject({ value: 0.8, nodePrize: 0.85, investmentMomentum: 0.7, homeMarket: 0.75, basis: 'national' });
    expect(node('Batteries|Refining').prize).toMatchObject({ value: 0.4, nodePrize: 0.45, homeMarket: null });
    expect(node('Batteries|Extraction').prize.maturity).toBe(9);
    expect(com.cpFamilies.map((f) => f.id)).toEqual(['export_position', 'manufacturing_capacity', 'investment_position']);
    expect(com.pathway).toEqual({ credible: 'yes', basis: ['feasibility', 'pipeline'] });
    expect(com.trace.map((s) => s.result)).toEqual(['high', 'weak', 'yes', 'catch_up']);
    expect(com.policy).toMatchObject({ priority: 0.6, support: 0.6, protection: 0.3, protectionRecorded: false, momentum: 0.12, alignment: 'aligned', alignmentConfidence: 'medium', sources: ['NIPO'] });
    // protection_recorded is optional: true, false, or absent (null: the rank is shown)
    expect([node('Batteries|Extraction').policy.protectionRecorded, node('Batteries|Assembly').policy.protectionRecorded]).toEqual([true, null]);
    // the v1 flat alignment fields are read too
    expect(node('Batteries|Refining').policy).toMatchObject({ priority: 0.2, alignment: 'under_prioritized', alignmentConfidence: 'medium', momentum: null, sources: [] });
    const ext = node('Batteries|Extraction');
    expect(ext.partners.map((p) => p.iso3)).toEqual(['BBB', 'CCC']);
    expect([ext.partnerGap, ext.aibStatus, ext.partnerGapReasons, ext.delivery]).toEqual([false, 'covered', [], 'allied']);
    expect(node('Batteries|Recovery')).toMatchObject({ posture: 'not_assessed', notAssessedReason: 'Supply-chain need is not measured for this node.', need: null });
    expect(node('Solar|Assembly').flag).toBe('review_strong_position_low_prize');
  });

  it("reads the engine's Watch reasons as a \";\"-separated string or an array; none where the document writes none", () => {
    expect(node('Solar|Extraction').watchReasons).toEqual(['low_need', 'low_prize', 'ample_diversified_supply']);
    expect(node('Solar|Recovery').watchReasons).toEqual(['low_need', 'low_prize', 'option_value']);
    expect(node('Solar|Deployment').watchReasons).toEqual(['low_deployment_need']);
    expect(node('Batteries|Extraction').watchReasons).toEqual([]);
  });

  it('the store fetches, checks and caches both documents', async () => {
    const st = fixtureStore();
    const a = await st.core();
    expect(await st.core()).toBe(a);
    expect(st.peek('core')).toBe(a);
    expect((await st.detail('AAA')).country).toBe('Aland');
    await expect(st.detail('ZZZ')).rejects.toThrow(/No posture detail/);
  });
});
