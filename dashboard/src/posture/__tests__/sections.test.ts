// The pure rows of the Country page's sections (sections.ts; spec XII-XVI): the two Need rankings, the Prize &
// Position points and quadrant tendencies, the Strategic Choices groups with the Watch reasons and the
// manufacturing-versus-deployment split, the Secure fields, the Policy Alignment rows, and the Allied Strategy
// rows, their gap logic and the opening sentence. Missing inputs stay null; nothing is invented.

import { describe, expect, it } from 'vitest';
import { checkPostureDetail, decodeCore, type PostureCell } from '../contract.ts';
import { countryCells, countryNames } from '../derive.ts';
import {
  alliedRows, alliedSentence, catchUpPositionWords, choiceGroups, countryRankWords, deploymentNeedRanking, detailByKey, diversificationWords, domesticCase, intensityWord, manufacturingPriority, manufacturingPriorityWord,
  ordinal, partnerWhy, policyMomentumWords, policyRows, positionGap, prizePositionPoints, prizeQuadrant, prizeTendency, protectionWords, secureRisk, secureTreatment, supplyNeedRanking,
  TOP_QUARTER_NOT_PRESERVE, watchReasons,
} from '../sections.ts';
import { fixtureCore, fixtureDetail } from './fixture.ts';

const core = decodeCore(fixtureCore());
const t = core.meta.thresholds;
const aaa = countryCells(core, 'AAA');
const detail = checkPostureDetail(fixtureDetail());
const dk = detailByKey(detail);
const names = countryNames(core);
const cell = (key: string, cells: PostureCell[] = aaa) => cells.find((c) => `${c.tech}|${c.stage}` === key)!;

describe('Need (spec XII)', () => {
  it('A: producing-stage nodes, Secure first, then by supply-chain need, with reason tags; Deployment and unmeasured nodes left out', () => {
    const rows = supplyNeedRanking(aaa, t);
    expect(rows.map((r) => r.key)).toEqual([
      'Batteries|Refining', 'Solar|Refining', 'Solar|Components', 'Batteries|Extraction', 'Solar|Assembly', 'Batteries|Components', 'Batteries|Assembly', 'Solar|Extraction', 'Solar|Recovery',
    ]);
    expect(rows.slice(0, 5).map((r) => r.tags)).toEqual([
      ['CHOKEPOINT', 'HIGH DEPENDENCE'], ['SYSTEMIC RESILIENCE', 'HIGH DEPENDENCE'], ['CONTROL LAYER', 'HIGH DEPENDENCE'], ['HIGH DEPENDENCE'], ['HIGH DEPENDENCE'],
    ]);
    expect(rows[5].tags).toEqual([]);
  });

  it('B: technologies by deployment need, with the three families, their basis and scenario', () => {
    const rows = deploymentNeedRanking(aaa, detail);
    expect(rows.map((r) => [r.tech, r.need, r.verb])).toEqual([['Batteries', 0.85, 'accelerate'], ['Solar', 0.4, 'watch']]);
    expect(rows[0].families?.demand).toMatchObject({ value: 0.9, scenario: 'current_policy', basis: 'national' });
    expect(rows[0].families?.policyGap.scenario).toBe('national_commitments');
    expect(rows[0].families?.system.value).toBeNull();
    // while the detail loads: the totals alone
    expect(deploymentNeedRanking(aaa, null)[0].families).toBeNull();
  });
});

describe('Prize & Position (spec XIII)', () => {
  it('plots producing-stage nodes with a prize and a position, Secure at its true coordinates; the rest are listed', () => {
    const { plotted, notPlotted } = prizePositionPoints(aaa, 'all');
    expect(plotted).toHaveLength(9);
    expect(notPlotted.map((c) => `${c.tech}|${c.stage}`)).toEqual(['Batteries|Recovery']);
    const sec = plotted.find((p) => p.key === 'Batteries|Refining')!;
    expect([sec.verb, sec.prize, sec.cp]).toEqual(['secure', 0.4, 0.1]);
    expect(plotted.some((p) => p.stage === 'Deployment')).toBe(false);
    expect(prizePositionPoints(aaa, 'Assembly').plotted.map((p) => [p.key, p.verb])).toEqual([['Batteries|Assembly', 'preserve'], ['Solar|Assembly', 'preserve']]);
  });

  it('the quadrant of a point, a tendency at the prize and position thresholds', () => {
    expect(prizeQuadrant({ prize: 0.9, cp: 0.95 }, t)).toBe('strong_strong');
    expect(prizeQuadrant({ prize: 0.8, cp: 0.4 }, t)).toBe('weak_strong_prize');
    expect(prizeQuadrant({ prize: 0.3, cp: 0.2 }, t)).toBe('weak_weak');
    expect(prizeQuadrant({ prize: 0.3, cp: 0.9 }, t)).toBe('strong_weak_prize');
  });

  it('a high-prize node with a top-quarter score that is not Preserve did not pass the Preserve test: never "typically Preserve"', () => {
    expect(prizeTendency({ prize: 0.9, cp: 0.95, verb: 'preserve' }, t)).toEqual({ words: 'Strong position + strong prize: typically Preserve', tendency: true });
    expect(prizeTendency({ prize: 0.9, cp: 0.95, verb: 'secure' }, t).tendency).toBe(true);
    const failed = { words: 'A top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor)', tendency: false };
    for (const verb of ['catch_up', 'partner', 'watch'] as const) expect(prizeTendency({ prize: 0.9, cp: 0.875, verb }, t), verb).toEqual(failed);
    // below the bar, or with a low prize, the quadrant's tendency stays
    expect(prizeTendency({ prize: 0.8, cp: 0.4, verb: 'catch_up' }, t)).toEqual({ words: 'Weak position + strong prize: Catch Up, given a real base to build from', tendency: true });
    expect(prizeTendency({ prize: 0.3, cp: 0.9, verb: 'watch' }, t).words).toMatch(/^Strong position \+ weak prize: mature or specialized/);
  });
});

describe('Strategic Choices (spec XIV)', () => {
  it('six groups, each in its reading order, the review list and the nodes not assessed', () => {
    const g = choiceGroups(aaa);
    const keys = (xs: PostureCell[]) => xs.map((c) => `${c.tech}|${c.stage}`);
    expect(keys(g.secure)).toEqual(['Batteries|Refining', 'Solar|Refining', 'Solar|Components']);
    expect(keys(g.preserve)).toEqual(['Batteries|Assembly', 'Solar|Assembly']);
    expect(keys(g.catch_up)).toEqual(['Batteries|Components']);
    expect(keys(g.partner)).toEqual(['Batteries|Extraction']);
    expect(keys(g.accelerate)).toEqual(['Batteries|Deployment']);
    expect(keys(g.watch)).toEqual(['Solar|Extraction', 'Solar|Deployment', 'Solar|Recovery']);
    expect(keys(g.review)).toEqual(['Solar|Assembly']);
    expect(keys(g.notAssessed)).toEqual(['Batteries|Recovery']);
  });

  it("Watch says why domestic industrial priority is low: the engine's reasons from the detail, in its order", () => {
    // the engine's codes (R sp_watch_reasons), never re-derived: a weak domestic case needs a high prize, so these low-prize nodes have none
    expect(watchReasons(cell('Solar|Extraction'), t, dk.get('Solar|Extraction')!)).toEqual(['Low need', 'Low prize', 'Ample diversified supply']);
    expect(watchReasons(cell('Solar|Recovery'), t, dk.get('Solar|Recovery')!)).toEqual(['Low need', 'Low prize', 'Emerging technology, research and option value']);
    expect(watchReasons(cell('Solar|Deployment'), t, dk.get('Solar|Deployment')!)).toEqual(['Deployment need below the acceleration bar']);
    // a high-prize Watch: the engine's weak domestic case (no top-quarter position, no credible pathway)
    const highPrize = { ...cell('Solar|Extraction'), prize: 0.8 };
    expect(watchReasons(highPrize, t, { ...dk.get('Solar|Extraction')!, watchReasons: ['low_need', 'weak_domestic_case'] })).toEqual(['Low need', 'Weak domestic case (no strong position or credible pathway)']);
    // the detail's lack of trusted alternatives is a need score (high = few alternatives): it never turns into "ample diversified supply"
    const scarce = { ...dk.get('Solar|Extraction')!, watchReasons: ['low_need', 'low_prize'], supplyChain: { ...dk.get('Solar|Extraction')!.supplyChain!, trustedAlternatives: 0.74, supplierConcentration: 0.1 } };
    expect(watchReasons(cell('Solar|Extraction'), t, scarce)).toEqual(['Low need', 'Low prize']);
  });

  it('while the detail loads, Watch gives only what the core says for sure: never ample supply or a weak domestic case', () => {
    expect(watchReasons(cell('Solar|Extraction'), t, null)).toEqual(['Low need', 'Low prize']);
    expect(watchReasons(cell('Solar|Deployment'), t, null)).toEqual(['Deployment need below the acceleration bar']);
    // a high prize without a strong position or a pathway: the engine's weak domestic case, but the core alone does not say it
    expect(watchReasons({ ...cell('Solar|Extraction'), prize: 0.8, cp: 0.3, pathway: 'no' }, t, null)).toEqual(['Low need']);
    // a loaded detail that writes no reasons falls back to the same core facts
    expect(watchReasons(cell('Solar|Extraction'), t, { ...dk.get('Solar|Extraction')!, watchReasons: [] })).toEqual(['Low need', 'Low prize']);
  });

  it('deployment priority is separate from domestic manufacturing priority', () => {
    expect(manufacturingPriority('Batteries', aaa)).toEqual({
      low: false, assessed: true, words: 'Partner at Extraction; Secure at Refining & Processing; Catch Up at Component Manufacturing; Preserve at Final Assembly & Equipment; Not assessed at Recovery & Recycling',
    });
    expect(manufacturingPriorityWord(manufacturingPriority('Batteries', aaa))).toBe('set at the producing stages');
    const bland = countryCells(core, 'BBB');
    expect(manufacturingPriority('Solar', bland).low).toBe(true);
    expect(manufacturingPriorityWord(manufacturingPriority('Solar', bland))).toBe('low');
    // a technology whose producing stages are all unassessed is "not assessed", never "low"
    const unassessed = aaa.map((c) => (c.tech === 'Batteries' && c.stage !== 'Deployment' ? { ...c, posture: 'N' as const, mode: null, deployMode: null } : c));
    expect(manufacturingPriority('Batteries', unassessed)).toMatchObject({ low: true, assessed: false });
    expect(manufacturingPriorityWord(manufacturingPriority('Batteries', unassessed))).toBe('not assessed');
    // Bland accelerates battery deployment while catching up in battery cells; a technology can be low manufacturing priority and still Accelerate
    const lowMfg = bland.map((c) => (c.tech === 'Batteries' && c.stage === 'Components' ? { ...c, posture: 'D' as const, mode: null, deployMode: 'watch' as const } : c));
    expect(manufacturingPriority('Batteries', lowMfg).low).toBe(true);
    expect(cell('Batteries|Deployment', lowMfg).deployMode).toBe('accelerate');
  });

  it('the Secure fields: main risk and recommended treatment from the detail, never invented', () => {
    expect(secureRisk(cell('Batteries|Refining'), dk.get('Batteries|Refining')!, names)).toBe('XXX holds 70% of world supply; 80% of imports come from potentially coercive suppliers');
    expect(secureRisk(cell('Solar|Components'), dk.get('Solar|Components')!, names)).toBe('digitally active; remote access');
    expect(secureRisk(cell('Solar|Refining'), null, names)).toMatch(/^it carries an essential function/);
    expect(secureTreatment(dk.get('Batteries|Refining')!, core.design, 'secure_input')).toBe('Diversify refining through allied offtake and hold a strategic stock.');
    expect(secureTreatment(dk.get('Solar|Refining')!, core.design, 'secure_resilience')).toBeNull();
    expect(secureTreatment(null, core.design, 'secure_input')).toBeNull();
  });

  it('the Catch Up position gap and the Partner domestic case', () => {
    expect(positionGap(cell('Batteries|Components'), t)).toBe(35);
    expect(positionGap(cell('Batteries|Assembly'), t)).toBeNull();
    // the Catch Up position from the cell: below the bar, a top-quarter score that did not pass the Preserve test, or not measured
    expect(catchUpPositionWords(cell('Batteries|Components'), t, 'gap')).toBe('35 points below the Preserve bar (position 40)');
    expect(catchUpPositionWords(cell('Batteries|Components'), t, 'sentence')).toBe('a position below the top quarter');
    for (const style of ['gap', 'sentence'] as const) {
      expect(catchUpPositionWords({ cp: 0.875 }, t, style)).toBe(TOP_QUARTER_NOT_PRESERVE);
      expect(catchUpPositionWords({ cp: 0.75 }, t, style)).toBe(TOP_QUARTER_NOT_PRESERVE);
      expect(catchUpPositionWords({ cp: null }, t, style)).toBe('position not measured');
    }
    expect(TOP_QUARTER_NOT_PRESERVE).toBe('a top-quarter score that does not pass the Preserve test (not a main producer, or below the capability floor)');
    expect(domesticCase(cell('Batteries|Extraction'), t)).toBe('position 20 (below the Preserve bar of 75); prize 30 (below 60); no credible pathway');
    // a Partner node with a top-quarter score failed the Preserve test; it is never called strong
    expect(domesticCase({ ...cell('Batteries|Extraction'), cp: 0.88 }, t)).toBe(
      'position 88 (top-quarter score, but not a main producer or below the capability floor); prize 30 (below 60); no credible pathway',
    );
    expect(diversificationWords(dk.get('Batteries|Extraction')!.partners)).toBe('moving 10% of imports to BBB would cut import concentration (HHI) by 0.050');
    expect(diversificationWords([])).toBeNull();
  });
});

describe('Policy Alignment (spec XV)', () => {
  it('every assessed node, the ones needing attention first, with the observed policy from the detail; no overall score', () => {
    const rows = policyRows(aaa, detail);
    expect(rows).toHaveLength(11);
    expect(rows.map((r) => r.alignment)).toEqual([
      'misaligned', 'under_prioritized', 'deployment_gap', 'over_prioritized', 'mixed', 'aligned', 'aligned', 'aligned', 'aligned', 'aligned', 'insufficient_data',
    ]);
    expect(rows[0]).toMatchObject({ key: 'Batteries|Extraction', recommendation: 'Partner', intensity: 0.85, support: 0.2, protection: 0.9, momentum: 'rising', sources: ['NIPO', 'IEA PAMS'] });
    expect(rows[2]).toMatchObject({ key: 'Batteries|Deployment', recommendation: 'Accelerate', alignmentConfidence: 'low' });
    expect(rows[3]).toMatchObject({ key: 'Solar|Deployment', recommendation: 'Watch', protection: 0.85 });
    // policy momentum only where the time series supports it
    expect(rows.find((r) => r.key === 'Solar|Extraction')?.momentum).toBeNull();
    expect(policyRows(aaa, null)[0]).toMatchObject({ intensity: null, support: null, sources: [] });
    expect([intensityWord(0.9), intensityWord(0.5), intensityWord(0.1), intensityWord(null)]).toEqual(['high', 'moderate', 'low', 'not available']);
    expect(rows.find((r) => r.key === 'Batteries|Components')).toMatchObject({ protection: 0.3, protectionRecorded: false, momentum: 0.12 });
    expect(rows[0].protectionRecorded).toBe(true);
    expect(rows.find((r) => r.key === 'Batteries|Assembly')?.protectionRecorded).toBeNull();
  });

  it('support, protection and momentum are within-country percentile ranks: never shares, never a signed change', () => {
    expect([ordinal(1), ordinal(2), ordinal(3), ordinal(4), ordinal(11), ordinal(12), ordinal(13), ordinal(21), ordinal(45), ordinal(92), ordinal(100), ordinal(111)]).toEqual([
      '1st', '2nd', '3rd', '4th', '11th', '12th', '13th', '21st', '45th', '92nd', '100th', '111th',
    ]);
    expect(countryRankWords(0.917)).toBe('high, 92nd percentile in the country');
    expect(countryRankWords(0.447, true)).toBe('moderate, 45th percentile');
    expect(countryRankWords(0.0076)).toBe('low, 1st percentile in the country');
    expect(countryRankWords(null)).toBe('not available');
    // momentum is a rank of recent activity (0.0076-0.992 in the real table): it never reads as "+92%", and a low rank is not a decline
    expect(policyMomentumWords(0.917)).toBe('high, 92nd percentile in the country');
    expect(policyMomentumWords(0.12, true)).toBe('low, 12th percentile');
    expect(policyMomentumWords(0.12)).not.toMatch(/[+%]/);
    expect([policyMomentumWords('rising'), policyMomentumWords(null)]).toEqual(['rising', 'not available']);
    // protection: "none recorded" where the records hold no protection measure (a tie among zeros), the rank where they do or do not say
    expect(protectionWords(0.447, false)).toBe('none recorded');
    expect(protectionWords(0.9, true)).toBe('high, 90th percentile in the country');
    expect(protectionWords(0.447, null, true)).toBe('moderate, 45th percentile');
    expect(protectionWords(null, null)).toBe('not available');
  });
});

describe('Allied Strategy (spec XVI)', () => {
  it('rows: every Partner node and every Secure node delivered with allies; partners, why, AIB status and gap logic', () => {
    const rows = alliedRows(aaa, detail, core.division, names);
    expect(rows.map((r) => [r.key, r.verb, r.status])).toEqual([
      ['Batteries|Extraction', 'partner', 'covered'], ['Batteries|Refining', 'secure', 'gap'], ['Solar|Refining', 'secure', 'not_mapped'],
    ]);
    expect(rows[0].partners.map((p) => [p.name, p.why])).toEqual([
      ['Bland', ['strong supply position (20% of world supply)', 'strong competitive position (top quarter)', 'trusted relationship', 'diversification contribution']],
      ['Cland', ['strong supply position (6% of world supply)', 'trusted relationship']],
    ]);
    expect(rows[1].gaps).toEqual(['allied capacity is insufficient', 'capability evidence is weak', 'concentration remains excessive (50% held by potentially coercive suppliers)']);
    expect(partnerWhy({ iso3: 'X' })).toEqual([]);
  });

  it('a Partner node without a trusted supplier says "no credible allied solution yet"; the economy gap reasons win over the division', () => {
    const cland = countryCells(core, 'CCC');
    const [row] = alliedRows(cland, null, core.division, names);
    expect([row.key, row.status, row.noSolution, row.partners]).toEqual(['Batteries|Components', 'gap', true, []]);
    expect(row.gaps).toEqual(['only one credible supplier exists', 'allied capacity is insufficient', 'concentration remains excessive (70% held by potentially coercive suppliers)']);
    const doc = fixtureDetail();
    Object.assign((doc.cells as Record<string, unknown>[])[0], { partner_gap_reasons: 'weak_evidence;single_credible_supplier', partner_aib_status: 'gap' });
    const [own] = alliedRows(aaa, checkPostureDetail(doc), core.division, names);
    expect([own.status, own.gaps]).toEqual(['gap', ['only one credible supplier exists', 'capability evidence is weak']]);
  });

  it('the opening sentence is computed from the rows', () => {
    expect(alliedSentence('Aland', aaa, alliedRows(aaa, detail, core.division, names))).toBe(
      "1 of Aland's 7 priority nodes is better solved through allies; 2 Secure nodes also depend on trusted foreign capacity.",
    );
    const cland = countryCells(core, 'CCC');
    expect(alliedSentence('Cland', cland, alliedRows(cland, null, core.division, names))).toBe(
      "1 of Cland's 1 priority node is better solved through allies; 1 has no credible allied solution yet.",
    );
    expect(alliedSentence('Wales', [], [])).toBe("None of Wales' 0 priority nodes are a Partner choice.");
  });
});
