// The Country strategy sentence (spec XI.2, XI.7; design section 4): a pure function over the node choices.
// Every clause is backed by at least one node of its verb; a verb with no node has no clause; nothing is
// invented (the words are the phrase table's, else the technology and stage).

import { describe, expect, it } from 'vitest';
import type { PostureCell } from '../contract.ts';
import { decodeCore } from '../contract.ts';
import { countryCells } from '../derive.ts';
import { verbOf } from '../grammar.ts';
import { listPhrases, nodePhrase, strategyClauses, strategySentence } from '../strategy.ts';
import { fixtureCore } from './fixture.ts';

const core = decodeCore(fixtureCore());
const meta = core.meta;
const cellsOf = (iso3: string) => countryCells(core, iso3);

describe('the strategy sentence', () => {
  it("names Aland's pattern from its choices, in the order preserve, secure, catch up, partner, accelerate", () => {
    expect(strategySentence('Aland', cellsOf('AAA'), meta)).toBe(
      'Aland should preserve its strongest positions in battery pack assembly and solar module assembly; secure battery materials refining (chokepoint), polysilicon refining (systemic resilience) and solar inverters (operational / control); catch up in battery cell manufacturing; rely on partners for lithium and nickel mining; and accelerate deployment of battery storage.',
    );
  });

  it('drops every clause without a node behind it', () => {
    expect(strategySentence('Bland', cellsOf('BBB'), meta)).toBe('Bland should catch up in battery cell manufacturing and accelerate deployment of battery storage.');
    expect(strategySentence('Cland', cellsOf('CCC'), meta)).toBe('Cland should rely on partners for battery cell manufacturing.');
    const watchOnly = cellsOf('BBB').filter((c) => c.deployMode === 'watch');
    expect(strategySentence('Bland', watchOnly, meta)).toBe(
      `Bland has no node that calls for securing, preserving, catching up, partnering or accelerating deployment: domestic industrial priority is low in all ${watchOnly.length} assessed nodes.`,
    );
    const notAssessed = cellsOf('CCC').filter((c) => c.posture === 'N');
    expect(strategySentence('Cland', notAssessed, meta)).toBe('Cland has no assessed node yet: need or prize is not measured in any of its 1 nodes.');
    expect(strategySentence('Zland', [], meta)).toBe('Zland has no classified supply-chain node.');
  });

  it('every clause is backed by the nodes it names, each of its verb', () => {
    for (const iso of ['AAA', 'BBB', 'CCC']) {
      const cells = cellsOf(iso);
      for (const cl of strategyClauses(cells, meta)) {
        expect(cl.keys.length).toBeGreaterThan(0);
        expect(cl.total).toBe(cells.filter((c) => verbOf(c) === cl.verb).length);
        for (const k of cl.keys) expect(verbOf(cells.find((c) => `${c.tech}|${c.stage}` === k)!)).toBe(cl.verb);
      }
    }
  });

  it('ranks each group by its own measure and counts a group longer than three, "led by" the first three', () => {
    const base = cellsOf('AAA').find((c) => c.mode === 'preserve')!;
    const pres = (tech: string, stage: PostureCell['stage'], cp: number): PostureCell => ({ ...base, tech, stage, cp });
    const cells = [pres('Wind', 'Components', 0.8), pres('Solar', 'Components', 0.99), pres('Nuclear', 'Assembly', 0.9), pres('Grid', 'Assembly', 0.85)];
    const [cl] = strategyClauses(cells, meta);
    expect(cl.text).toBe('preserve 4 strong positions, led by solar inverters, nuclear assembly and grid assembly');
    expect(cl.keys).toEqual(['Solar|Components', 'Nuclear|Assembly', 'Grid|Assembly']);
    expect(cl.total).toBe(4);
  });

  it('a list whose phrases carry their own "and" writes those as "&", so the list has one "and"; a single phrase keeps its words', () => {
    expect(listPhrases(['battery recycling', 'wafers and PV cells', 'PV modules and inverters'])).toBe('battery recycling, wafers & PV cells and PV modules & inverters');
    expect(listPhrases(['blades, towers and nacelle parts', 'wind turbines'])).toBe('blades, towers & nacelle parts and wind turbines');
    expect(listPhrases(['solar inverters', 'nuclear assembly', 'grid assembly'])).toBe('solar inverters, nuclear assembly and grid assembly');
    expect(listPhrases(['copper and bauxite mining'])).toBe('copper and bauxite mining');
    const base = cellsOf('AAA').find((c) => c.mode === 'catch_up')!;
    const meta2 = { phrases: { 'Solar|Refining': 'wafers and PV cells', 'Solar|Assembly': 'PV modules and inverters', 'Batteries|Recovery': 'battery recycling' } };
    const cu = (tech: string, stage: PostureCell['stage'], prize: number): PostureCell => ({ ...base, tech, stage, prize });
    const [cl] = strategyClauses([cu('Batteries', 'Recovery', 0.9), cu('Solar', 'Refining', 0.8), cu('Solar', 'Assembly', 0.7), cu('Wind', 'Components', 0.6)], meta2);
    expect(cl.text).toBe('catch up in 4 nodes, led by battery recycling, wafers & PV cells and PV modules & inverters');
  });

  it('the node phrase: the table, else the technology and stage word; Deployment names the technology', () => {
    expect(nodePhrase(meta, 'Batteries', 'Refining')).toBe('battery materials refining');
    expect(nodePhrase(meta, 'Solar', 'Extraction')).toBe('solar extraction');
    expect(nodePhrase(meta, 'Solar', 'Recovery')).toBe('solar recycling');
    expect(nodePhrase({ phrases: {} }, 'Wind', 'Deployment')).toBe('wind');
  });
});
