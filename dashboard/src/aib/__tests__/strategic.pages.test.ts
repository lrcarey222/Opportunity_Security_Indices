// The helpers of the Strategic Sectoral Compact pages (views/strategic/, 2026-10-20) on the synthetic
// fixture and, where the export is present, the real bundle: the words (lowerFirst keeps acronyms and
// chemical symbols, the headline without its scenario prefix, the intervention and workstream lines), the
// landing's families (every compact once, in the export's order) and the CSV of every compact under every
// scenario (one row each; the persistent fields the same in every scenario, the status counts adding up to
// the compact's nodes).

import { describe, expect, it } from 'vitest';
import type { AibStrategic } from '../contract.ts';
import { compactNodeIds, compactScenarioOf, strategicCompacts, strategicScenarios } from '../derive/strategic.ts';
import { compactFamilies } from '../views/StrategicCompactsView.tsx';
import { strategicCsvRows } from '../views/strategic/ScenarioMatrix.tsx';
import { activeWords, countsWords, headlineBody, interventionWords, lowerFirst, upperFirst } from '../views/strategic/shared.tsx';
import { haveFixture, readDoc, REAL_DIR, SYNTHETIC_DIR } from './fixture.ts';

const name = (iso3: string) => `name:${iso3}`;

describe('the words of the strategic pages', () => {
  it('lowerFirst lowers an ordinary first word and keeps acronyms, symbols and mixed case', () => {
    expect(lowerFirst('Demand anchor')).toBe('demand anchor');
    expect(lowerFirst('Rare-earth ore')).toBe('rare-earth ore');
    expect(lowerFirst('Magnets, magnet parts & assemblies')).toBe('magnets, magnet parts & assemblies');
    expect(lowerFirst('Modules')).toBe('modules');
    for (const kept of ['AC generators', 'PGMs (iridium, ruthenium)', 'PV modules & generators', 'Li-ion cells & packs', 'NdPr metal', 'EV', '']) expect(lowerFirst(kept)).toBe(kept);
    expect(upperFirst('build missing capacity')).toBe('Build missing capacity');
  });

  it('headlineBody drops the scenario prefix only when it is there', () => {
    expect(headlineBody('Decoupling: 24% of the demand is lost.', 'Decoupling')).toBe('24% of the demand is lost.');
    expect(headlineBody('24% of the demand is lost.', 'Decoupling')).toBe('24% of the demand is lost.');
    expect(headlineBody(null, 'Decoupling')).toBe('–');
  });
});

function checks(label: string, dir: string) {
  describe.skipIf(!haveFixture(dir) || !readDoc(dir).strategic)(`the landing's helpers on the ${label} bundle`, () => {
    const s = (haveFixture(dir) ? readDoc(dir).strategic : null) as AibStrategic;

    it('the families hold every compact once, in the export’s order', () => {
      const fams = compactFamilies(strategicCompacts(s));
      expect(fams.flatMap((f) => f.compacts.map((c) => c.id))).toEqual(expect.arrayContaining(strategicCompacts(s).map((c) => c.id)));
      expect(fams.flatMap((f) => f.compacts).length).toBe(strategicCompacts(s).length);
      expect(new Set(fams.map((f) => f.family)).size).toBe(fams.length);
      for (const f of fams) {
        const order = f.compacts.map((c) => strategicCompacts(s).indexOf(c));
        expect(order).toEqual([...order].sort((a, b) => a - b));
      }
    });

    it('the CSV: one row per compact and scenario; the persistent fields the same in every scenario; the counts add up to the nodes', () => {
      const rows = strategicCsvRows(s, name);
      const scenarios = strategicScenarios(s);
      expect(rows).toHaveLength(strategicCompacts(s).length * scenarios.length);
      for (const c of strategicCompacts(s)) {
        const mine = rows.filter((r) => r.strategic_compact_id === c.id);
        expect(mine.map((r) => r.scenario)).toEqual(scenarios.map((d) => d.label));
        const persistent = (r: Record<string, unknown>) => [r.strategic_compact_name, r.compact_family, r.value_chain, r.nodes, r.workstreams, r.core_countries];
        for (const r of mine) {
          expect(persistent(r)).toEqual(persistent(mine[0]));
          const counts = ['build_nodes', 'coordinate_nodes', 'manage_specialization_nodes', 'low_intervention_nodes', 'insufficient_data_nodes', 'not_stressed_nodes', 'not_tested_nodes', 'no_shock_nodes'];
          if (r.build_nodes !== null) expect(counts.reduce((a, k) => a + (r[k] as number), 0)).toBe(compactNodeIds(c).length);
        }
      }
    });

    it('the intervention, status and workstream lines', () => {
      for (const c of strategicCompacts(s)) {
        for (const d of strategicScenarios(s)) {
          const sc = compactScenarioOf(c, d.id);
          const words = interventionWords(c, sc);
          if (d.id === 'baseline') expect(words).toBe('no disruption applied');
          else expect(words).toMatch(/^(no step of \d+ needs action|\d+ of \d+ supply-chain steps? (needs|need) action)$/);
          // the statuses in plain words (Build capacity, Unaffected, ...), never the data's upper-case codes
          expect(countsWords(sc)).toMatch(/^\d+ [A-Z][a-z]+( [a-z]+)*(, \d+ [A-Z][a-z]+( [a-z]+)*)*$/);
        }
        expect(activeWords(c, [])).toBe(c.workstreams.length ? `0 of ${c.workstreams.length} workstream${c.workstreams.length === 1 ? '' : 's'} active` : 'No workstream: none of the analysis’s clusters falls mainly in this compact');
      }
    });
  });
}

checks('synthetic', SYNTHETIC_DIR);
checks('real', REAL_DIR);
