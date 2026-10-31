// The Sector strategy card's pure derivations (design section 5): the points of the Competitive Position x
// need chart, the node's prize in words, the decision rule, the region tendencies and the six-verb takeaway
// with "led by" wording, on the synthetic schema-2 posture bundle.

import { describe, expect, it } from 'vitest';
import { decodeCore, type PostureCell } from '../../posture/contract.ts';
import { countryNames } from '../../posture/derive.ts';
import { fixtureCore } from '../../posture/__tests__/fixture.ts';
import {
  alliedSides, decisionWords, fenceCountsText, ledBy, mixedLayersNote, nodePrize, notPlottedReason, prizeWords, regionLabels, sectorStrategyTakeaway, sectorStrategyTitle,
  strategyHref, strategyPoints, verbRank, verbsOfStage,
} from './derive.ts';

const core = decodeCore(fixtureCore());
const names = countryNames(core);
const node = (tech: string, stage: string) => core.cells.filter((c) => c.tech === tech && c.stage === stage);
const T = core.meta.thresholds;

describe('the Sector strategy chart: Competitive Position against the relevant need', () => {
  it('places every assessed economy with a measured need, Secure last; a missing position is kept (the left strip)', () => {
    const pts = strategyPoints(node('Batteries', 'Refining'), names);
    expect(pts.map((p) => [p.iso3, p.verb])).toEqual([['BBB', 'watch'], ['CCC', 'watch'], ['AAA', 'secure']]);
    // Recovery: Aland is not assessed, so it is not plotted
    expect(strategyPoints(node('Batteries', 'Recovery'), names).map((p) => p.iso3)).toEqual(['BBB', 'CCC']);
    const noCp = node('Batteries', 'Components').map((c) => (c.iso3 === 'CCC' ? { ...c, cp: null } : c));
    expect(strategyPoints(noCp, names).find((p) => p.iso3 === 'CCC')?.cp).toBeNull();
    // the reasons the table gives for an economy off the chart
    const aaaRec = node('Batteries', 'Recovery').find((c) => c.iso3 === 'AAA') as PostureCell;
    expect(notPlottedReason(aaaRec)).toBe('not assessed (need or prize not measured)');
    expect(notPlottedReason(null)).toBe('no strategic choice in the data');
    expect(notPlottedReason(node('Batteries', 'Refining')[0])).toBeNull();
  });

  it('Deployment & Use is plotted on deployment need, with Accelerate and Watch (never Partner)', () => {
    const pts = strategyPoints(node('Batteries', 'Deployment'), names);
    expect(new Set(pts.map((p) => p.verb))).toEqual(new Set(['accelerate', 'watch']));
    expect(pts.every((p) => p.cell.needType === 'deployment')).toBe(true);
    expect(verbsOfStage('Deployment')).toEqual(['secure', 'accelerate', 'watch']);
    expect(verbsOfStage('Refining')).toEqual(['secure', 'preserve', 'catch_up', 'partner', 'watch']);
  });

  it('region labels are tendencies by stage and prize: Accelerate / Watch at Deployment; Preserve right of the bar when the prize is high; Partner / Watch when it is low; none when mixed', () => {
    expect(regionLabels('Deployment', null, T).map((r) => r.label)).toEqual(['ACCELERATE', 'WATCH']);
    expect(regionLabels('Assembly', 'high', T).map((r) => r.label)).toEqual(['PRESERVE', 'CATCH UP OR PARTNER', 'CATCH UP OR WATCH']);
    expect(regionLabels('Assembly', 'high', T)[0].x).toBeGreaterThan(T.cp);
    expect(regionLabels('Extraction', 'low', T).map((r) => r.label)).toEqual(['PARTNER', 'PRESERVE · REVIEW', 'WATCH']);
    expect(regionLabels('Extraction', 'mixed', T)).toEqual([]);
  });
});

describe("the node's prize and the decision rule, in words", () => {
  it('the prize is one number when every economy shares it, a median and range when a home market lifts some; none at Deployment', () => {
    const same = node('Batteries', 'Refining').map((c) => ({ ...c, prize: 0.85 }));
    const p = nodePrize(same, T.prize);
    expect(p).toMatchObject({ n: 3, high: 3, kind: 'high' });
    // the prize is the Strategic Industrial Value (2026-10-30); an older core names its market prize
    expect(prizeWords('Refining', p, T.prize)).toBe('Prize (Strategic Industrial Value) at this step: 85 for every economy, at or above the bar of 60.');
    expect(prizeWords('Refining', p, T.prize, 'Industrial Opportunity')).toBe('Prize (Industrial Opportunity) at this step: 85 for every economy, at or above the bar of 60.');
    const lifted = same.map((c, i) => ({ ...c, prize: i === 0 ? 0.91 : 0.85 }));
    expect(prizeWords('Refining', nodePrize(lifted, T.prize), T.prize)).toBe(
      'Prize (Strategic Industrial Value) at this step: 85 for most economies (85–91 where a national home market lifts it), at or above the bar of 60 for all.',
    );
    const mixed = nodePrize(node('Batteries', 'Components'), T.prize);
    expect(mixed).toMatchObject({ n: 3, high: 2, kind: 'mixed' });
    expect(prizeWords('Components', mixed, T.prize)).toContain('2 of 3 at or above the bar of 60');
    expect(nodePrize(node('Batteries', 'Deployment'), T.prize)).toBeNull();
    expect(prizeWords('Deployment', null, T.prize)).toMatch(/^Deployment & Use has no prize of its own/);
    expect(prizeWords('Recovery', null, T.prize)).toBe('The prize (Strategic Industrial Value) is not measured at this step.');
  });

  it('the rule follows the stage and the prize, at the core thresholds', () => {
    expect(decisionWords('Deployment', null, T)).toMatch(/^At Deployment & Use the choice reads deployment need only: at or above 67, Accelerate; below it, Watch\./);
    expect(decisionWords('Deployment', null, T)).toContain('not a call to manufacture the equipment at home');
    const high = decisionWords('Assembly', { n: 3, min: 0.8, median: 0.8, max: 0.9, high: 3, kind: 'high' }, T);
    expect(high).toMatch(/^Where the prize is high, Competitive Position decides: the top quarter of this step's main producers \(at or above 75\) Preserve; below it, a credible pathway \(a capability anchor and a corroborating signal\) means Catch Up/);
    expect(high).not.toContain('where the prize is low');
    expect(high).toContain('Secure overrides all of these');
    expect(high).not.toMatch(/triage|contenders|node/);
    const low = decisionWords('Extraction', { n: 3, min: 0.1, median: 0.1, max: 0.1, high: 0, kind: 'low' }, T);
    expect(low).toMatch(/^Where the prize is low, need decides: high need means Partner \(a strong position is kept as Preserve and flagged for review\), low need Watch\./);
    expect(decisionWords('Components', nodePrize(node('Batteries', 'Components'), T.prize), T)).toContain('; where the prize is low, need decides');
  });
});

describe('the takeaway: the six verbs, named economies, "led by" for a longer group', () => {
  it('names each group (not Watch) in verb order, then the economies not assessed, with the node phrase', () => {
    expect(sectorStrategyTakeaway('Batteries', 'Components', node('Batteries', 'Components'), names, core.meta)).toBe(
      'In battery cell manufacturing, 2 economies should catch up from a credible base (Bland and Aland); and 1 economy should rely on trusted partners for it (Cland).',
    );
    expect(sectorStrategyTakeaway('Batteries', 'Refining', node('Batteries', 'Refining'), names, core.meta)).toBe(
      'In battery materials refining, 1 economy should secure it (Aland); and 2 economies should leave it open (low domestic priority).',
    );
    expect(sectorStrategyTakeaway('Batteries', 'Deployment', node('Batteries', 'Deployment'), names, core.meta)).toBe(
      'In battery storage, 2 economies should deploy faster (Bland and Aland); and 1 economy should watch rather than accelerate deployment (need below the bar for acceleration).',
    );
    // a node without a phrase falls back to "<technology> <stage word>"; not assessed is counted last
    expect(sectorStrategyTakeaway('Batteries', 'Recovery', node('Batteries', 'Recovery'), names, core.meta)).toBe(
      'In batteries recycling, 2 economies should leave it open (low domestic priority); and 1 economy is not assessed.',
    );
    expect(sectorStrategyTakeaway('Solar', 'Extraction', [], names, core.meta)).toBe('No economy has a strategic choice in solar extraction.');
  });

  it('a group of more than three is "led by" its first three (Preserve by Competitive Position)', () => {
    const base = node('Batteries', 'Assembly')[0];
    const many: PostureCell[] = ['AAA', 'BBB', 'CCC', 'DDD', 'EEE'].map((iso3, i) => ({ ...base, iso3, posture: 'C', mode: 'preserve', deployMode: null, cp: 0.8 + i * 0.03 }));
    const nm = { ...names, DDD: 'Dland', EEE: 'Eland' };
    expect(sectorStrategyTakeaway('Batteries', 'Assembly', many, nm, core.meta)).toBe('In battery pack assembly, 5 economies should preserve a strong position (led by Eland, Dland and Cland).');
    expect(ledBy(['A', 'B'], 2)).toBe('A and B');
    expect(ledBy(['A', 'B', 'C', 'D'], 4)).toBe('led by A, B and C');
    expect(ledBy(['A', 'B', 'C'], 9)).toBe('led by A, B and C');
  });

  it('the counts are grouped by fence, as the legend is (not the Secure / Compete / Deploy families)', () => {
    expect(fenceCountsText(node('Batteries', 'Components'))).toBe('0 High fence · 3 Medium fence · 0 Low fence');
    expect(fenceCountsText(node('Batteries', 'Refining'))).toBe('1 High fence · 0 Medium fence · 2 Low fence');
    // Deployment & Use: Accelerate is Medium fence, Watch Low fence
    expect(fenceCountsText(node('Batteries', 'Deployment'))).toBe('0 High fence · 2 Medium fence · 1 Low fence');
    // not assessed counts in no fence (the card adds "N not assessed")
    expect(fenceCountsText(node('Batteries', 'Recovery'))).toBe('0 High fence · 0 Medium fence · 2 Low fence');
  });

  it('a step whose layers sit in another yard is named, so a mark never implies the whole step is in one yard', () => {
    const cells = node('Batteries', 'Components');
    expect(mixedLayersNote(cells.map((c) => ({ ...c, yards: null })), names)).toBeNull();
    const mixed = cells.map((c) => (c.iso3 === 'CCC' ? { ...c, yards: 'mixed_high_medium' as const } : { ...c, yards: 'medium' as const }));
    const note = mixedLayersNote(mixed, names) ?? '';
    expect(note).toMatch(/^Layers in another yard: for 1 economy \(Cland\), a layer of this step, such as control software or a critical input, sits in a different yard from the step as a whole\./);
    expect(note).toContain("A mark's colour is the step's own choice; the economy's Country Strategy Map shows each layer.");
    expect(note).toContain('A control layer is in the Small yard only where the data show coercive foreign control for that economy.');
    // more than three are counted past the first three, alphabetically
    const base = cells[0];
    const many = ['AAA', 'BBB', 'CCC', 'DDD', 'EEE'].map((iso3) => ({ ...base, iso3, yards: 'mixed_high_medium' as const }));
    expect(mixedLayersNote(many, { ...names, DDD: 'Dland', EEE: 'Eland' })).toContain('for 5 economies (Aland, Bland, Cland and 2 more)');
  });

  it('the two sides of the allied division of labor: the strongest positions, and the economies whose choice is Partner', () => {
    const sides = alliedSides('Components', node('Batteries', 'Components'), names) ?? '';
    expect(sides).toMatch(/^Who has the strongest capability\? .+ \(the highest Competitive Positions\)\. Which countries need a partner\? Cland \(their choice is Partner\)\.$/);
    const noPartner = node('Batteries', 'Components').map((c) => (c.iso3 === 'CCC' ? { ...c, mode: 'catch_up' as const } : c));
    expect(alliedSides('Components', noPartner, names)).toMatch(/Which countries need a partner\? None here: no economy has Partner as its choice\.$/);
    // Deployment & Use has no Partner and reads Competitive Position as context only: no sides
    expect(alliedSides('Deployment', node('Batteries', 'Deployment'), names)).toBeNull();
  });

  it('the title uses the stage display name; a mark opens the Country Strategy Map on the node; rows sort by verb', () => {
    expect(sectorStrategyTitle('Batteries', 'Refining')).toBe("Every economy's choice in Batteries — Refining & Processing");
    expect(sectorStrategyTitle('Solar', 'Deployment')).toBe("Every economy's choice in Solar — Deployment & Use");
    const href = strategyHref('AAA', 'Electric Grid', 'Assembly');
    const u = new URL(href, 'http://x');
    expect([u.pathname, u.searchParams.get('node'), u.hash]).toEqual(['/countries/AAA', 'Electric Grid|Assembly', '#strategy']);
    const cells = node('Batteries', 'Components');
    expect(cells.map(verbRank)).toEqual([2, 2, 3]);
    expect(verbRank(null)).toBe(6);
  });
});
