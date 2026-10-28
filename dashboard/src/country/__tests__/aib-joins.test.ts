// The Country profile's joins with the six-stage AIB document (2026-10-28; derive/framing.ts
// compactsOfCell() and dependencies()). The fact sheet's cells are the paper's three stages, the AIB
// nodes six-stage: a three-stage cell is joined through each node's three-stage cells (yfCellsLegacy),
// else through the approximate roll-up of its six-stage cells. The fixture's anode node is Refining
// (yfCells batteries-refining, which rolls up to batteries-upstream) while the paper filed its codes under
// Batteries Midstream (yfCellsLegacy batteries-midstream); it belongs to compact AIB-C02, whose demand
// anchor is AAA (renamed USA here, a fact-sheet economy, as in profile.test.tsx).

import { describe, expect, it } from 'vitest';
import type { AibCompact, AibDoc } from '../../aib/contract.ts';
import { compactTitle } from '../../aib/derive/format.ts';
import { haveFixture as haveAib, readDoc, stripLegacyCells, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { legacyCellsOf, rollUpCellSlug } from '../../lib/stages.ts';
import type { FactSheet, Quadrant } from '../contract.ts';
import { compactsOfCell, dependencies } from '../derive/framing.ts';
import { haveFixture, readSheet } from './fixture.ts';

/** The AIB fixture with its economy AAA renamed USA. */
function renamed(d: AibDoc): AibDoc {
  return JSON.parse(JSON.stringify(d).replaceAll('"AAA"', '"USA"')) as AibDoc;
}

const ids = (cs: readonly AibCompact[]) => cs.map((c) => c.id);

/** A document with one compact holding the given nodes only (the joins read compacts[].nodes and nodes). */
const oneCompact = (doc: AibDoc, nodes: string[]): Pick<AibDoc, 'compacts' | 'nodes'> => ({
  nodes: doc.nodes,
  compacts: [{ ...doc.compacts[0], id: 'K', nodes }],
});

describe.skipIf(!haveAib(SYNTHETIC_DIR))('Country joins with the six-stage AIB document: compactsOfCell', () => {
  const doc = renamed(readDoc(SYNTHETIC_DIR));
  const old = stripLegacyCells(renamed(readDoc(SYNTHETIC_DIR)));

  it('a Refining node filed under Midstream covers the Midstream cell, not the Upstream cell its stage rolls up to', () => {
    const anode = doc.nodes.find((n) => n.id === 'bat-mid-anode')!;
    expect(anode).toMatchObject({ stage: 'Refining', yfCells: ['batteries-refining'], yfCellsLegacy: ['batteries-midstream'], compact: 'AIB-C02' });
    expect(rollUpCellSlug(anode.yfCells[0])).toBe('batteries-upstream');
    const k = oneCompact(doc, ['bat-mid-anode']);
    expect(ids(compactsOfCell(k, 'batteries-midstream'))).toEqual(['K']);
    expect(compactsOfCell(k, 'batteries-upstream')).toEqual([]);
    // the whole fixture: AIB-C02 covers Batteries Midstream (the anode node) and Upstream (natural graphite)
    expect(ids(compactsOfCell(doc, 'batteries-midstream'))).toEqual(['AIB-C02']);
    expect(ids(compactsOfCell(doc, 'batteries-upstream'))).toEqual(['AIB-C02']);
  });

  it('without yfCellsLegacy the join falls back to the roll-up of the six-stage cells (approximate)', () => {
    expect(old.nodes.every((n) => n.yfCellsLegacy === undefined)).toBe(true);
    const k = oneCompact(old, ['bat-mid-anode']);
    expect(compactsOfCell(k, 'batteries-midstream')).toEqual([]);
    expect(ids(compactsOfCell(k, 'batteries-upstream'))).toEqual(['K']);
    // where a stage maps cleanly the roll-up agrees with the paper: Components -> Midstream, Refining (rare earths) -> Upstream
    expect(ids(compactsOfCell(old, 'wind-midstream'))).toEqual(['AIB-C01', 'AIB-C03']);
    expect(ids(compactsOfCell(old, 'wind-upstream'))).toEqual(['AIB-C01']);
    expect(ids(compactsOfCell(old, 'electric-vehicles-midstream'))).toEqual(['AIB-C01']);
    for (const n of doc.nodes) if (n.id !== 'bat-mid-anode') expect(legacyCellsOf({ yfCells: n.yfCells }), n.id).toEqual(n.yfCellsLegacy);
  });

  it('a six-stage key joins through the six-stage cells; unknown keys and node sectors find nothing', () => {
    expect(ids(compactsOfCell(doc, 'batteries-refining'))).toEqual(['AIB-C02']);
    expect(ids(compactsOfCell(doc, 'wind-components'))).toEqual(['AIB-C01', 'AIB-C03']);
    expect(compactsOfCell(doc, 'batteries-components')).toEqual([]);
    expect(compactsOfCell(doc, 'wind-downstream')).toEqual([]);
    expect(compactsOfCell(doc, 'magnets-midstream')).toEqual([]);
    expect(compactsOfCell(doc, 'no-such-cell')).toEqual([]);
  });

  it('no duplicates: a compact with several nodes on the cell is listed once, in the document order', () => {
    // under the roll-up both AIB-C02 nodes (natural graphite, the anode node) sit in Batteries Upstream
    expect(ids(compactsOfCell(old, 'batteries-upstream'))).toEqual(['AIB-C02']);
    const both = oneCompact(doc, ['ev-wind-mid-magnets', 'wind-down-towers', 'ev-wind-mid-magnets']);
    expect(ids(compactsOfCell(both, 'wind-midstream'))).toEqual(['K']);
    for (const d of [doc, old]) {
      for (const key of ['batteries-upstream', 'batteries-midstream', 'wind-midstream', 'wind-upstream', 'electric-vehicles-midstream', 'solar-midstream']) {
        const got = ids(compactsOfCell(d, key));
        expect(new Set(got).size, key).toBe(got.length);
        expect(got, key).toEqual(d.compacts.map((c) => c.id).filter((id) => got.includes(id)));
      }
    }
  });
});

describe.skipIf(!haveFixture() || !haveAib(SYNTHETIC_DIR))('Country joins with the six-stage AIB document: dependencies', () => {
  const doc = renamed(readDoc(SYNTHETIC_DIR));
  const old = stripLegacyCells(renamed(readDoc(SYNTHETIC_DIR)));
  /** The USA fixture sheet with Batteries Upstream in the Upgrade corner and Midstream in the Exposed one (key secure). */
  const sheetWith = (): FactSheet => {
    const s = structuredClone(readSheet('USA'));
    const set = (key: string, q: Quadrant) => Object.assign(s.cells.find((c) => c.key === key)!, { placed: true, quadrant: q });
    set('batteries-upstream', 'upgrade');
    set('batteries-midstream', 'secure');
    return s;
  };

  it('an Exposed Batteries Midstream sector lists the compact of its Refining node, through the node’s three-stage cells', () => {
    const sheet = sheetWith();
    const deps = dependencies(sheet, doc, 99);
    const mid = deps.find((d) => d.cell.key === 'batteries-midstream')!;
    expect(mid).toBeTruthy();
    const c02 = doc.compacts.find((c) => c.id === 'AIB-C02')!;
    expect(c02.roles.some((r) => r.iso3 === 'USA' && r.role === 'Demand anchor')).toBe(true);
    expect(mid.compacts).toEqual([{ id: 'AIB-C02', name: compactTitle(c02, doc.nodes) }]);
    // Upstream through natural graphite; listed once
    expect(deps.find((d) => d.cell.key === 'batteries-upstream')!.compacts.map((c) => c.id)).toEqual(['AIB-C02']);
  });

  it('without yfCellsLegacy the Midstream sector loses the link (the roll-up files the Refining node under Upstream), Upstream keeps it once', () => {
    const deps = dependencies(sheetWith(), old, 99);
    expect(deps.find((d) => d.cell.key === 'batteries-midstream')!.compacts).toEqual([]);
    expect(deps.find((d) => d.cell.key === 'batteries-upstream')!.compacts.map((c) => c.id)).toEqual(['AIB-C02']);
  });
});
