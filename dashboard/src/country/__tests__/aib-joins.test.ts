// The Country profile's joins with the AIB document (derive/framing.ts compactsOfCell() and
// dependencies()). Since the six-stage fact sheets (schema 2, design A2, 2026-10-29) both sides are keyed on
// the six supply-chain stages, so a sheet cell ("batteries-refining") is joined through each node's six-stage
// cells (node.yfCells) directly; the nodes' three-stage cells (yfCellsLegacy, still in the bundle so older
// documents validate) are never read, and a three-stage key is not a sector and finds nothing. The fixture's
// anode node is Refining (yfCells batteries-refining) and its natural graphite node Extraction; both belong
// to compact AIB-C02, whose demand anchor is AAA (renamed USA here, a fact-sheet economy, as in
// profile.test.tsx).

import { describe, expect, it } from 'vitest';
import type { AibCompact, AibDoc } from '../../aib/contract.ts';
import { compactTitle } from '../../aib/derive/format.ts';
import { haveFixture as haveAib, readDoc, stripLegacyCells, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { COUNTRY_CELL_KEYS, type FactSheet, type Quadrant } from '../contract.ts';
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

describe.skipIf(!haveAib(SYNTHETIC_DIR))('Country joins with the AIB document at six stages: compactsOfCell', () => {
  const doc = renamed(readDoc(SYNTHETIC_DIR));
  const noLegacy = stripLegacyCells(renamed(readDoc(SYNTHETIC_DIR)));

  it("a six-stage sector joins through the nodes' six-stage cells: the Refining anode node covers Batteries — Refining", () => {
    const anode = doc.nodes.find((n) => n.id === 'bat-mid-anode')!;
    expect(anode).toMatchObject({ stage: 'Refining', yfCells: ['batteries-refining'], compact: 'AIB-C02' });
    const k = oneCompact(doc, ['bat-mid-anode']);
    expect(ids(compactsOfCell(k, 'batteries-refining'))).toEqual(['K']);
    for (const other of ['batteries-extraction', 'batteries-components', 'batteries-assembly']) expect(compactsOfCell(k, other), other).toEqual([]);
    // the whole fixture: AIB-C02 covers Batteries — Refining (the anode node) and Batteries — Extraction (natural graphite)
    expect(ids(compactsOfCell(doc, 'batteries-refining'))).toEqual(['AIB-C02']);
    expect(ids(compactsOfCell(doc, 'batteries-extraction'))).toEqual(['AIB-C02']);
    expect(compactsOfCell(doc, 'batteries-components')).toEqual([]);
  });

  it('many-to-many through the OSI crosswalk: a node in two cells, a cell with two compacts; a node in no compact adds none', () => {
    // the magnets node sits in Electric Vehicles and Wind Components, the rare-earth compounds node in their Refining cells: one compact, four cells
    for (const key of ['electric-vehicles-components', 'wind-components', 'electric-vehicles-refining', 'wind-refining']) expect(ids(compactsOfCell(doc, key)), key).toContain('AIB-C01');
    // Wind — Components also holds the towers node of AIB-C03
    expect(ids(compactsOfCell(doc, 'wind-components'))).toEqual(['AIB-C01', 'AIB-C03']);
    expect(ids(compactsOfCell(doc, 'wind-refining'))).toEqual(['AIB-C01']);
    // the solar modules and transformer nodes belong to no compact
    expect(compactsOfCell(doc, 'solar-assembly')).toEqual([]);
    expect(compactsOfCell(doc, 'electric-grid-assembly')).toEqual([]);
  });

  it("the nodes' three-stage cells are not read: the joins are the same without them, and a three-stage key finds nothing", () => {
    expect(doc.nodes.some((n) => n.yfCellsLegacy !== undefined)).toBe(true);
    expect(noLegacy.nodes.every((n) => n.yfCellsLegacy === undefined)).toBe(true);
    for (const key of COUNTRY_CELL_KEYS) expect(ids(compactsOfCell(noLegacy, key)), key).toEqual(ids(compactsOfCell(doc, key)));
    for (const key of ['batteries-midstream', 'batteries-upstream', 'wind-midstream', 'wind-upstream', 'electric-vehicles-midstream', 'wind-downstream', 'magnets-midstream', 'no-such-cell']) {
      expect(compactsOfCell(doc, key), key).toEqual([]);
    }
  });

  it('no duplicates: a compact with several nodes on the cell is listed once, in the document order', () => {
    const both = oneCompact(doc, ['ev-wind-mid-magnets', 'wind-down-towers', 'ev-wind-mid-magnets']);
    expect(ids(compactsOfCell(both, 'wind-components'))).toEqual(['K']);
    for (const key of COUNTRY_CELL_KEYS) {
      const got = ids(compactsOfCell(doc, key));
      expect(new Set(got).size, key).toBe(got.length);
      expect(got, key).toEqual(doc.compacts.map((c) => c.id).filter((id) => got.includes(id)));
    }
  });
});

describe.skipIf(!haveFixture() || !haveAib(SYNTHETIC_DIR))('Country joins with the AIB document at six stages: dependencies', () => {
  const doc = renamed(readDoc(SYNTHETIC_DIR));
  const noLegacy = stripLegacyCells(renamed(readDoc(SYNTHETIC_DIR)));
  /** The USA fixture sheet with Batteries — Extraction in the Upgrade corner and Batteries — Refining in the Exposed one (key secure). */
  const sheetWith = (): FactSheet => {
    const s = structuredClone(readSheet('USA'));
    const set = (key: string, q: Quadrant) => Object.assign(s.cells.find((c) => c.key === key)!, { placed: true, quadrant: q });
    set('batteries-extraction', 'upgrade');
    set('batteries-refining', 'secure');
    return s;
  };

  it('an Exposed Batteries — Refining sector lists the compact of its Refining node, where the economy is a demand anchor', () => {
    const deps = dependencies(sheetWith(), doc, 99);
    const ref = deps.find((d) => d.cell.key === 'batteries-refining')!;
    expect(ref).toBeTruthy();
    const c02 = doc.compacts.find((c) => c.id === 'AIB-C02')!;
    expect(c02.roles.some((r) => r.iso3 === 'USA' && r.role === 'Demand anchor')).toBe(true);
    // the display title is the AIB module's (imported, never copied): OSI technologies, then the node labels
    expect(ref.compacts).toEqual([{ id: 'AIB-C02', name: compactTitle(c02, doc.nodes) }]);
    expect(ref.compacts[0].name).toMatch(/^Batteries: /);
    // Batteries — Extraction through natural graphite; listed once
    expect(deps.find((d) => d.cell.key === 'batteries-extraction')!.compacts.map((c) => c.id)).toEqual(['AIB-C02']);
  });

  it("the same without the nodes' three-stage cells", () => {
    const deps = dependencies(sheetWith(), noLegacy, 99);
    expect(deps.find((d) => d.cell.key === 'batteries-refining')!.compacts.map((c) => c.id)).toEqual(['AIB-C02']);
    expect(deps.find((d) => d.cell.key === 'batteries-extraction')!.compacts.map((c) => c.id)).toEqual(['AIB-C02']);
  });
});
