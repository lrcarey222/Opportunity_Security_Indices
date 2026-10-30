// The Sector strategy card's pure derivations (design section 5): the points of the Competitive Position x
// need chart, the node's prize in words, the decision rule, the region tendencies and the six-verb takeaway
// with "led by" wording, on the synthetic schema-2 posture bundle.

import { describe, expect, it } from 'vitest';
import { decodeCore, type PostureCell } from '../../posture/contract.ts';
import { countryNames } from '../../posture/derive.ts';
import { fixtureCore } from '../../posture/__tests__/fixture.ts';
import {
  decisionWords, ledBy, nodePrize, notPlottedReason, prizeWords, regionLabels, sectorStrategyTakeaway, sectorStrategyTitle, strategyHref, strategyPoints, verbRank,
  verbsOfStage,
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
    expect(notPlottedReason(null)).toBe('not classified');
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
    expect(prizeWords('Refining', p, T.prize)).toBe('Prize (Industrial Opportunity) in this node: 85 for every economy, at or above the bar of 60.');
    const lifted = same.map((c, i) => ({ ...c, prize: i === 0 ? 0.91 : 0.85 }));
    expect(prizeWords('Refining', nodePrize(lifted, T.prize), T.prize)).toBe(
      'Prize (Industrial Opportunity) in this node: 85 for most economies (85–91 where a national home market lifts it), at or above the bar of 60 for all.',
    );
    const mixed = nodePrize(node('Batteries', 'Components'), T.prize);
    expect(mixed).toMatchObject({ n: 3, high: 2, kind: 'mixed' });
    expect(prizeWords('Components', mixed, T.prize)).toContain('2 of 3 at or above the bar of 60');
    expect(nodePrize(node('Batteries', 'Deployment'), T.prize)).toBeNull();
    expect(prizeWords('Deployment', null, T.prize)).toMatch(/^Deployment & Use has no prize of its own/);
    expect(prizeWords('Recovery', null, T.prize)).toBe('The prize (Industrial Opportunity) is not measured in this node.');
  });

  it('the rule follows the stage and the prize, at the core thresholds', () => {
    expect(decisionWords('Deployment', null, T)).toMatch(/^At Deployment & Use the choice reads deployment need only: at or above 67, Accelerate; below it, Watch\./);
    expect(decisionWords('Deployment', null, T)).toContain('not a call to manufacture the equipment at home');
    const high = decisionWords('Assembly', { n: 3, min: 0.8, median: 0.8, max: 0.9, high: 3, kind: 'high' }, T);
    expect(high).toMatch(/^Where the prize is high, Competitive Position decides: the top quarter of the node's contenders \(at or above 75\) Preserve/);
    expect(high).not.toContain('where the prize is low');
    expect(high).toContain('Secure overrides the triage');
    const low = decisionWords('Extraction', { n: 3, min: 0.1, median: 0.1, max: 0.1, high: 0, kind: 'low' }, T);
    expect(low).toMatch(/^Where the prize is low, need decides: high need means Partner \(a strong position is kept as Preserve and flagged for review\), low need Watch\./);
    expect(decisionWords('Components', nodePrize(node('Batteries', 'Components'), T.prize), T)).toContain('; where the prize is low, need decides');
  });
});

describe('the takeaway: the six verbs, named economies, "led by" for a longer group', () => {
  it('names each group (not Watch) in verb order, then the economies not assessed, with the node phrase', () => {
    expect(sectorStrategyTakeaway('Batteries', 'Components', node('Batteries', 'Components'), names, core.meta)).toBe(
      'In battery cell manufacturing, 2 economies should catch up, with the prize and a credible pathway (Bland and Aland); and 1 economy should partner, needing it without a domestic case (Cland).',
    );
    expect(sectorStrategyTakeaway('Batteries', 'Refining', node('Batteries', 'Refining'), names, core.meta)).toBe(
      'In battery materials refining, 1 economy must secure it (Aland); and 2 economies should watch (domestic industrial priority low).',
    );
    expect(sectorStrategyTakeaway('Batteries', 'Deployment', node('Batteries', 'Deployment'), names, core.meta)).toBe(
      'In battery storage, 2 economies should accelerate deployment (Bland and Aland); and 1 economy should watch (no acceleration needed).',
    );
    // a node without a phrase falls back to "<technology> <stage word>"; not assessed is counted last
    expect(sectorStrategyTakeaway('Batteries', 'Recovery', node('Batteries', 'Recovery'), names, core.meta)).toBe(
      'In batteries recycling, 2 economies should watch (domestic industrial priority low); and 1 economy is not assessed.',
    );
    expect(sectorStrategyTakeaway('Solar', 'Extraction', [], names, core.meta)).toBe('No economy is classified in solar extraction.');
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
