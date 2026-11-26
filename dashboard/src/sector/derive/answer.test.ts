// The Sector pages' answers first (usability pass, 2026-11-25): chainAnswer / chainAnswerText (the
// technology page's "What matters in this supply chain?"), strongestSentence (the stage page's "Who is
// strongest today?") and affordabilityEvidence (which cost and deployment measures exist), on synthetic
// rows; missing evidence reads as insufficient, never as low concentration or zero.

import { describe, expect, it } from 'vitest';
import { AVOID_RE } from '../../content/vocabulary.ts';
import type { ItemInfo, SectorModel, VarInfo } from '../data/model.ts';
import { affordabilityEvidence, bandApplies, MIN_HOLDERS, chainAnswer, chainAnswerText, licensedWords, NO_STAGE_MEASURE, strongestSentence, yearWords, type ChainLine } from './answer.ts';

const line = (o: Partial<ChainLine> & Pick<ChainLine, 'stage' | 'words'>): ChainLine => ({ top1Name: 'China', top1Share: 0.5, top1Economy: true, year: 2024, family: 'exports', holders: 20, ...o });

describe('chainAnswer (pure)', () => {
  const lines = [
    line({ stage: 'extraction', words: 'lithium mining', top1Name: 'Australia', top1Share: 0.48, family: 'production', year: 2023 }),
    line({ stage: 'extraction', words: 'cobalt mining', top1Name: 'the Democratic Republic of the Congo', top1Share: 0.7, family: 'production', year: 2023 }),
    line({ stage: 'refining', words: 'graphite refining', top1Share: 0.91, family: 'production' }),
    line({ stage: 'refining', words: 'refining exports', top1Share: 0.6 }),
    line({ stage: 'assembly', words: 'assembly exports', top1Share: 0.75 }),
    line({ stage: 'deployment', words: 'deployment capacity', top1Name: 'Asia Pacific', top1Share: 0.95, top1Economy: false, family: 'capacity' }),
    line({ stage: 'recovery', words: 'recovery exports', top1Name: null, top1Share: null }),
  ];
  const a = chainAnswer(lines, ['components', 'recovery']);
  it('takes each stage’s most concentrated row, in chain order, and skips a row without a known holder', () => {
    expect(a.byStage.map((b) => [b.stage, b.line.words])).toEqual([
      ['extraction', 'cobalt mining'],
      ['refining', 'graphite refining'],
      ['assembly', 'assembly exports'],
      ['deployment', 'deployment capacity'],
    ]);
    expect(a.nLines).toBe(6);
  });
  it('counts economies only among the strongest positions; the tightest row is the largest single share', () => {
    expect(a.strongest).toEqual([
      { name: 'China', n: 3 },
      { name: 'Australia', n: 1 },
      { name: 'the Democratic Republic of the Congo', n: 1 },
    ]);
    expect(a.tightest?.words).toBe('deployment capacity');
    expect(a.exportsOnly).toEqual(['assembly']);
  });
  it('says it in plain words, with the year and the limits, without a model claim', () => {
    const t = chainAnswerText(a, 'batteries');
    expect(t.insufficient).toBeNull();
    expect(t.concentration?.[1].text).toBe('China holds 91% of graphite refining (2024).');
    expect(t.strongest).toBe('China is the largest holder in 3 of the 6 measures shown; Australia in 1 and the Democratic Republic of the Congo in 1.');
    expect(t.implication).toBe('The most concentrated measure: Asia Pacific holds 95% of deployment capacity (2024). Concentration shows who holds the most today, not whether it could be replaced.');
    expect(t.limits).toBe('No comparable country-level evidence for Components and Recovery. Assembly is measured by export value only, not production.');
    for (const s of [t.strongest, t.implication, t.limits, ...(t.concentration ?? []).map((c) => c.text)]) expect(s).not.toMatch(AVOID_RE);
  });
  it('capitalizes a sentence that starts with "the"', () => {
    const t = chainAnswerText(chainAnswer([line({ stage: 'assembly', words: 'assembly exports', top1Name: 'the United States' })], []), 'wind');
    expect(t.strongest).toBe('The United States is the largest holder of the one measure shown.');
    expect(t.concentration?.[0].text).toBe('The United States holds 50% of assembly exports (2024).');
    expect(t.limits).toBe('Assembly is measured by export value only, not production.');
  });
  it('no row: insufficient evidence, never a low-concentration or zero reading', () => {
    const t = chainAnswerText(chainAnswer([], ['extraction', 'refining', 'components', 'assembly', 'recovery']), 'geothermal');
    expect(t.insufficient).toBe('Insufficient evidence: no stage of Geothermal has a country-level measure with a world total, so where supply is concentrated cannot be said.');
    expect(t.concentration).toBeNull();
    expect(t.strongest).toBeNull();
    expect(t.implication).toBeNull();
    expect(t.limits).toMatch(/^No comparable country-level evidence for Extraction, Refining, Components, Assembly and Recovery\.$/);
    expect(JSON.stringify(t)).not.toMatch(/\b0(\.0)?%|not concentrated|low/i);
  });
  it('only regions lead: no economy is named as strongest', () => {
    const t = chainAnswerText(chainAnswer([line({ stage: 'deployment', words: 'deployment capacity', top1Name: 'Asia Pacific', top1Economy: false, family: 'capacity' })], []), 'solar');
    expect(t.strongest).toBe('No single economy is the largest holder of the one measure shown.');
  });
  it(`a row with fewer than ${MIN_HOLDERS} holders makes no claim and is named under the limits`, () => {
    const thin = line({ stage: 'deployment', words: 'deployment investment', top1Name: 'the United States', top1Share: 0.998, holders: 2, family: 'investment', year: 2025 });
    const pot = line({ stage: 'deployment', words: 'deployment enhanced geothermal potential', top1Name: 'Brazil', top1Share: 0.088, holders: 165, family: 'resources', year: 2023 });
    const a = chainAnswer([thin, pot], []);
    expect(a.byStage.map((b) => b.line.words)).toEqual(['deployment enhanced geothermal potential']);
    expect(a.thin).toEqual([thin]);
    const t = chainAnswerText(a, 'geothermal');
    expect(t.strongest).toBe('Brazil is the largest holder of the one measure shown.');
    expect(t.implication).toBeNull();
    expect(t.limits).toBe('Deployment investment has values for fewer than 3 economies and is left out of this summary.');
    expect(JSON.stringify(t)).not.toMatch(/100%|99\.8%|United States/);
    const only = chainAnswerText(chainAnswer([thin], []), 'geothermal');
    expect(only.insufficient).toBe('Insufficient evidence: the Geothermal measures have values for too few economies to say where supply is concentrated.');
    expect(only.concentration).toBeNull();
  });
});

describe('strongestSentence (pure)', () => {
  it('names up to three holders, the measure and the year', () => {
    expect(strongestSentence(['China', 'Vietnam', 'Malaysia', 'Thailand'], 'world exports of solar modules', 2024, true)).toBe('China, Vietnam and Malaysia hold the largest shares of world exports of solar modules (2024).');
    expect(strongestSentence(['the United States'], 'world capacity additions', 2025, true)).toBe('The United States holds the largest share of world capacity additions (2025).');
    expect(strongestSentence(['Chile', 'China'], 'self-sufficiency', 2025, false)).toBe('Chile and China have the highest self-sufficiency (2025).');
  });
  it('without a measure or a holder it says so, never a ranking', () => {
    expect(strongestSentence([], 'world exports', 2024, true)).toBe(NO_STAGE_MEASURE);
    expect(strongestSentence(['China'], '', 2024, true)).toBe(NO_STAGE_MEASURE);
    expect(strongestSentence(['China'], 'world exports', null, true)).toBe(NO_STAGE_MEASURE);
    expect(NO_STAGE_MEASURE).toBe('No comparable country-level measure for this stage.');
  });
  it('with fewer than three holders it says the ranking is not a comparison', () => {
    expect(strongestSentence(['the United States'], 'world investment in geothermal deployment', 2025, true, 1)).toBe('The United States holds the largest share of world investment in geothermal deployment (2025). Only one economy has a value, so this is not a comparison.');
    expect(strongestSentence(['China', 'Chile'], 'world exports', 2024, true, 2)).toMatch(/Only 2 economies have a value/);
    expect(strongestSentence(['China', 'Chile', 'Peru'], 'world exports', 2024, true, 3)).not.toMatch(/Only/);
  });
});

// ---- affordabilityEvidence on a hand-built model ----------------------------------------------------

const item = (idx: number, o: Partial<ItemInfo>): ItemInfo => ({ idx, axis: 'cell', slug: `i${idx}`, label: `Item ${idx}`, tech: null, stage: null, techSlug: null, stageSlug: null, headline: true, sort: idx, has_trade: false, ...o });
const vr = (idx: number, id: string, o: Partial<VarInfo>): VarInfo => ({ idx, id, theme: id.split('.')[0], family: 'prices', label: id, short: id, unit: { short: '', unit_kind: 'number' }, item_axis: 'cell', projection_from: null, licence: 'attribution', source: { short: 'Src' }, ...o }) as VarInfo;

function fakeModel(opts: { licensed?: boolean; noPrices?: boolean; noDeploy?: boolean }): SectorModel {
  const dep = item(0, { techSlug: 'geothermal', stageSlug: 'deployment', tech: 'Geothermal', stage: 'Deployment', slug: 'geothermal-deployment' });
  const ref = item(1, { techSlug: 'geothermal', stageSlug: 'refining', tech: 'Geothermal', stage: 'Refining', slug: 'geothermal-refining' });
  const cu = item(2, { axis: 'commodity', slug: 'copper', label: 'Copper' });
  const ni = item(3, { axis: 'commodity', slug: 'nickel', label: 'Nickel' });
  const vars = [
    vr(0, 'resources.geo_lcoe_500', { unit: { id: 'usd_mwh', long: 'US dollars per megawatt-hour', short: 'USD/MWh', unit_kind: 'lcoe' } as never, label: 'Enhanced geothermal cost' }),
    vr(1, 'prices.latest', { item_axis: 'commodity' }),
    vr(2, 'investment.power_additions', { family: 'investment', short: 'Capacity additions' }),
    vr(3, 'consumption.weo', { family: 'consumption', item_axis: 'fuel', projection_from: 2030 }),
  ];
  const avail: Record<string, { year_min: number; year_max: number }> = {
    '0|0': { year_min: 2023, year_max: 2023 },
    ...(opts.noPrices ? {} : { '1|2': { year_min: 2025, year_max: 2025 }, '1|3': { year_min: 2025, year_max: 2025 } }),
    ...(opts.noDeploy ? {} : { '2|0': { year_min: 2018, year_max: 2025 } }),
  };
  return {
    vars,
    varById: new Map(vars.map((v) => [v.id, v])),
    cell: (t: string, s: string) => (t === 'geothermal' ? (s === 'deployment' ? dep : s === 'refining' ? ref : null) : null),
    availOf: (v: VarInfo, it?: number) => {
      const a = avail[`${v.idx}|${it}`];
      return a ? [{ var: v.idx, item: it, chunk: 'facts', n_years: 1, n_geo: 1, ...a }] : [];
    },
    itemsOfCell: (c: number, axis?: string) => (c === 1 && axis === 'commodity' ? [{ item: ni, weight: 0.2 }, { item: cu, weight: 0.8 }] : []),
    coverageOf: (c: number) => (opts.licensed && c === 0 ? [{ item: 0, family: 'prices', status: 'licensed', n_geo: 0, reason: 'licensed', sources: ['BNEF LCOE'] }] : []),
  } as unknown as SectorModel;
}

describe('affordabilityEvidence (pure)', () => {
  it('lists technology cost, input prices by link weight and deployment, with their years', () => {
    const a = affordabilityEvidence(fakeModel({}), 'geothermal');
    expect(a.cost.map((r) => [r.v.id, r.stage, r.year])).toEqual([['resources.geo_lcoe_500', 'deployment', 2023]]);
    expect(a.inputs?.items.map((i) => i.slug)).toEqual(['copper', 'nickel']);
    expect(a.inputs?.ref.year).toBe(2025);
    expect(a.deployment.map((r) => [r.v.id, r.year])).toEqual([['investment.power_additions', 2025]]);
    expect(a.licensed).toEqual([]);
  });
  it('names a licensed source without showing it; no measure, nothing listed (never a grade)', () => {
    const a = affordabilityEvidence(fakeModel({ licensed: true, noPrices: true, noDeploy: true }), 'geothermal');
    expect(a.inputs).toBeNull();
    expect(a.deployment).toEqual([]);
    expect(a.licensed).toEqual(['BNEF LCOE']);
    expect(licensedWords(a.licensed)).toBe('BNEF LCOE is licensed and not shown in the public Explorer.');
    expect(affordabilityEvidence(fakeModel({}), 'solar')).toEqual({ cost: [], inputs: null, deployment: [], licensed: [] });
  });
  it('a projection is said as one', () => {
    expect(yearWords({ year: 2024, projectionTo: 2035 })).toBe('2024; projections to 2035');
    expect(yearWords({ year: 2023, projectionTo: null })).toBe('2023');
  });
});

describe('bandApplies (pure)', () => {
  it('the plain concentration line is said of supply measures with enough holders only', () => {
    expect(bandApplies({ family: 'exports' }, { n: 120 })).toBe(true);
    expect(bandApplies({ family: 'production' }, { n: 3 })).toBe(true);
    expect(bandApplies({ family: 'investment' }, { n: 2 })).toBe(false);
    expect(bandApplies({ family: 'exports' }, { n: 2 })).toBe(false);
    expect(bandApplies({ family: 'reserves' }, { n: 165 })).toBe(false);
  });
});

describe('holderKey', () => {
  it('counts a source-tagged holder as the economy itself', async () => {
    const { holderKey } = await import('./answer.ts');
    expect(holderKey('China (IEA)')).toBe('China');
    expect(holderKey('Korea, Rep.')).toBe('Korea, Rep.');
    expect(holderKey('the United States')).toBe('the United States');
  });
});
