import { describe, expect, it } from 'vitest';
import type { AibNode } from '../../aib/contract.ts';
import { readDoc, stripLegacyCells, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { legacyCellsOf, rollUpCellSlug } from '../../lib/stages.ts';
import { matchNodes, nodeCellsFor, nodeNamedApart, nodeNames, nodesOfCell, nodeSubcomponentsFor, ownSubcomponents, subParts, subsOfNodes } from './chain.ts';

const node = (id: string, label: string, yfCells: string[], yfSubcomponents: string[] = []): AibNode =>
  ({ id, label, yfCells, yfSubcomponents, cls: 'BUILD', exposure: 0.3 }) as unknown as AibNode;

// the real bundle's Solar Midstream: one trade sub-component "Polysilicon & Wafers", two AIB nodes
const poly = node('solar-mid-poly', 'Polysilicon', ['solar-midstream']);
const wafers = node('solar-mid-wafers', 'Wafers', ['solar-midstream']);
const cells = node('solar-mid-cells', 'Solar Cells', ['solar-midstream'], ['solar-midstream::Cells']);
const glass = node('solar-mid-glass', 'Solar Glass', ['solar-midstream']);
const other = node('wind-mid-x', 'Wafers', ['wind-midstream']);

describe('supply-chain navigator: sub-components and AIB nodes', () => {
  it('splits a sub-component name on "&" and ","', () => {
    expect(subParts('Racking, Mounting & Trackers')).toEqual(['racking', 'mounting', 'trackers']);
    expect(subParts('Polysilicon & Wafers')).toEqual(['polysilicon', 'wafers']);
  });

  it('keeps the nodes of one cell and names them by their own sub-component when they have one', () => {
    expect(nodesOfCell([poly, wafers, other], 'solar-midstream').map((n) => n.id)).toEqual(['solar-mid-poly', 'solar-mid-wafers']);
    expect(nodeNames(cells, 'solar-midstream')).toEqual(['cells']);
    expect(nodeNames(wafers, 'solar-midstream')).toEqual(['wafers']);
  });

  it('"Polysilicon & Wafers" holds the nodes Polysilicon and Wafers; exact names win; the rest are unmatched', () => {
    const m = matchNodes(['Polysilicon & Wafers', 'Cells'], [poly, wafers, cells, glass, other], 'solar-midstream');
    expect(m.bySub.get('Polysilicon & Wafers')?.map((n) => n.label)).toEqual(['Polysilicon', 'Wafers']);
    expect(m.bySub.get('Cells')?.map((n) => n.label)).toEqual(['Solar Cells']);
    expect(m.unmatched.map((n) => n.label)).toEqual(['Solar Glass']);
    expect(nodeNamedApart(wafers, 'Polysilicon & Wafers')).toBe(true);
    expect(nodeNamedApart(node('x', 'cells', []), 'Cells')).toBe(false);
  });
});

// The AIB synthetic fixture is an electro-industrial dictionary bundle: its nodes reach the OSI cells and
// sub-components through the crosswalk ids only (many-to-many), never through their own sector or stage.
describe('supply-chain navigator: dictionary nodes through the OSI crosswalk', () => {
  const doc = readDoc(SYNTHETIC_DIR);
  const byId = (id: string) => doc.nodes.find((n) => n.id === id)!;

  it('a node sits in every OSI cell its codes sit in, whatever its own sector and stage', () => {
    // the magnets node (sector Magnets) sits in Electric Vehicles and Wind Midstream
    expect(nodesOfCell(doc.nodes, 'electric-vehicles-midstream').map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    expect(nodesOfCell(doc.nodes, 'wind-midstream').map((n) => n.id).sort()).toEqual(['ev-wind-mid-magnets', 'wind-down-towers']);
    // a rare-earth node (dictionary Midstream; stage Refining) whose codes sit in the OSI Upstream cells
    expect(nodesOfCell(doc.nodes, 'wind-upstream').map((n) => n.id)).toEqual(['ev-wind-mid-ree-compounds']);
    expect(nodesOfCell(doc.nodes, 'wind-midstream').map((n) => n.id)).not.toContain('ev-wind-mid-ree-compounds');
    // the Solar sector's dictionary-Downstream node (stage Assembly) is in OSI Solar Midstream, not Solar Downstream (a name join would put it there)
    expect(nodesOfCell(doc.nodes, 'solar-downstream')).toEqual([]);
    expect(nodesOfCell(doc.nodes, 'solar-midstream').map((n) => n.id)).toEqual(['solar-down-modules']);
    // a node with no basket code sits in no cell
    const unmapped = doc.nodes.filter((n) => !n.yfCells.length);
    expect(unmapped.map((n) => n.id)).toEqual(['h2-down-electrolysers']);
    for (const cell of ['batteries-midstream', 'solar-midstream', 'wind-midstream', 'green-hydrogen-downstream']) expect(nodesOfCell(unmapped, cell)).toEqual([]);
  });

  it('one node under several sub-components of a cell, one sub-component holding several nodes', () => {
    const magnets = byId('ev-wind-mid-magnets');
    expect(ownSubcomponents(magnets, 'wind-midstream')).toEqual(['permanent magnets & magnet parts', 'generator parts']);
    expect(nodeNames(magnets, 'electric-vehicles-midstream')).toEqual(['permanent magnets & magnet parts']);
    const m = matchNodes(['Permanent Magnets & Magnet Parts', 'Generator Parts', 'Towers & Nacelles', 'Blades'], doc.nodes, 'wind-midstream');
    expect(m.bySub.get('Permanent Magnets & Magnet Parts')!.map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    expect(m.bySub.get('Generator Parts')!.map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    expect(m.bySub.get('Towers & Nacelles')!.map((n) => n.id)).toEqual(['wind-down-towers']);
    expect(m.bySub.get('Blades')).toEqual([]);
    expect(m.unmatched).toEqual([]);
    expect(subsOfNodes(m).get('ev-wind-mid-magnets')).toEqual(['Permanent Magnets & Magnet Parts', 'Generator Parts']);
    // a dictionary label is not the OSI sub-component's name: the view names the node apart
    expect(nodeNamedApart(magnets, 'Generator Parts')).toBe(true);
    // the cell's sub-components the node does not name: listed under the stage
    const m2 = matchNodes(['Blades'], doc.nodes, 'wind-midstream');
    expect(m2.unmatched.map((n) => n.id).sort()).toEqual(['ev-wind-mid-magnets', 'wind-down-towers']);
  });

  it('several dictionary nodes in one sub-component', () => {
    const two = [
      node('poly-d', 'Polysilicon', ['solar-midstream'], ['solar-midstream::Polysilicon & Wafers']),
      node('wafers-d', 'Wafers', ['solar-midstream', 'semiconductors-midstream'], ['solar-midstream::Polysilicon & Wafers', 'semiconductors-midstream::Semiconductor Materials']),
    ];
    expect(matchNodes(['Polysilicon & Wafers', 'Cells'], two, 'solar-midstream').bySub.get('Polysilicon & Wafers')!.map((n) => n.id)).toEqual(['poly-d', 'wafers-d']);
    expect(matchNodes(['Semiconductor Materials'], two, 'semiconductors-midstream').bySub.get('Semiconductor Materials')!.map((n) => n.id)).toEqual(['wafers-d']);
  });
});

// The six-stage AIB bundle (2026-10-28) on the three-stage Sector cells: the join goes through each node's
// three-stage cells (yfCellsLegacy / yfSubcomponentsLegacy), else through the roll-up of its six-stage
// cells. The fixture's anode node is Refining (yfCells batteries-refining, which rolls up to
// batteries-upstream) but the paper filed its codes under Batteries Midstream (yfCellsLegacy).
describe('supply-chain navigator: six-stage nodes on three-stage cells', () => {
  const doc = readDoc(SYNTHETIC_DIR);
  const ids = (ns: readonly AibNode[]) => ns.map((n) => n.id);
  const node6 = (id: string, label: string, yfCells: string[], yfSubcomponents: string[], legacy?: { cells: string[]; subs: string[] }): AibNode =>
    ({ id, label, yfCells, yfSubcomponents, ...(legacy ? { yfCellsLegacy: legacy.cells, yfSubcomponentsLegacy: legacy.subs } : {}), cls: 'BUILD', exposure: 0.3 }) as unknown as AibNode;

  it('a Refining node filed under Midstream sits on the Midstream cell, not on the Upstream cell its stage rolls up to', () => {
    const anode = doc.nodes.find((n) => n.id === 'bat-mid-anode')!;
    expect(anode).toMatchObject({ stage: 'Refining', yfCells: ['batteries-refining'], yfCellsLegacy: ['batteries-midstream'] });
    expect(rollUpCellSlug('batteries-refining')).toBe('batteries-upstream');
    expect(ids(nodesOfCell(doc.nodes, 'batteries-midstream'))).toEqual(['bat-mid-anode']);
    expect(ids(nodesOfCell(doc.nodes, 'batteries-upstream'))).toEqual(['bat-up-graphite']);
    expect(ownSubcomponents(anode, 'batteries-midstream')).toEqual(['anode materials']);
    expect(ownSubcomponents(anode, 'batteries-upstream')).toEqual([]);
    const m = matchNodes(['Anode Materials', 'Cathode Materials'], doc.nodes, 'batteries-midstream');
    expect(ids(m.bySub.get('Anode Materials')!)).toEqual(['bat-mid-anode']);
    expect(m.bySub.get('Cathode Materials')).toEqual([]);
    expect(m.unmatched).toEqual([]);
  });

  it('a six-stage cell slug joins through the six-stage cells (yfCells)', () => {
    expect(ids(nodesOfCell(doc.nodes, 'batteries-refining'))).toEqual(['bat-mid-anode']);
    expect(ids(nodesOfCell(doc.nodes, 'wind-components')).sort()).toEqual(['ev-wind-mid-magnets', 'wind-down-towers']);
    const anode = doc.nodes.find((n) => n.id === 'bat-mid-anode')!;
    expect(nodeCellsFor(anode, 'batteries-refining')).toEqual(['batteries-refining']);
    expect(nodeCellsFor(anode, 'batteries-midstream')).toEqual(['batteries-midstream']);
    expect(nodeSubcomponentsFor(anode, 'batteries-refining')).toEqual(['batteries-refining::Anode Materials']);
    expect(ownSubcomponents(anode, 'batteries-refining')).toEqual(['anode materials']);
    // a six-stage slug never matches a node's three-stage cells
    expect(nodesOfCell(doc.nodes, 'batteries-components')).toEqual([]);
  });

  it('without yfCellsLegacy the join falls back to the roll-up (approximate: the anode node moves to Upstream)', () => {
    const old = stripLegacyCells(readDoc(SYNTHETIC_DIR));
    expect(old.nodes.every((n) => n.yfCellsLegacy === undefined && n.yfSubcomponentsLegacy === undefined)).toBe(true);
    // the roll-up agrees with the paper wherever the stage maps cleanly (Components -> Midstream, Extraction -> Upstream)
    expect(ids(nodesOfCell(old.nodes, 'wind-midstream')).sort()).toEqual(['ev-wind-mid-magnets', 'wind-down-towers']);
    expect(ids(nodesOfCell(old.nodes, 'wind-upstream'))).toEqual(['ev-wind-mid-ree-compounds']);
    expect(ids(nodesOfCell(old.nodes, 'solar-midstream'))).toEqual(['solar-down-modules']);
    const magnets = old.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!;
    expect(ownSubcomponents(magnets, 'wind-midstream')).toEqual(['permanent magnets & magnet parts', 'generator parts']);
    // ... and not for the Refining anode node, which the paper filed under Midstream
    expect(ids(nodesOfCell(old.nodes, 'batteries-midstream'))).toEqual([]);
    expect(ids(nodesOfCell(old.nodes, 'batteries-upstream'))).toEqual(['bat-mid-anode', 'bat-up-graphite']);
    const anode = old.nodes.find((n) => n.id === 'bat-mid-anode')!;
    expect(ownSubcomponents(anode, 'batteries-upstream')).toEqual(['anode materials']);
    // a hand-made node without the field rolls up the same way
    const n = node6('x', 'Anode', ['batteries-refining'], ['batteries-refining::Anode Materials']);
    expect(nodeCellsFor(n, 'batteries-upstream')).toEqual(['batteries-upstream']);
    expect(nodeSubcomponentsFor(n, 'batteries-upstream')).toEqual(['batteries-upstream::Anode Materials']);
  });

  it('an empty yfCellsLegacy is an answer: the node sits in no three-stage cell, whatever its six-stage cells', () => {
    const n = node6('x', 'Anode', ['batteries-refining'], ['batteries-refining::Anode Materials'], { cells: [], subs: [] });
    expect(nodesOfCell([n], 'batteries-upstream')).toEqual([]);
    expect(nodesOfCell([n], 'batteries-midstream')).toEqual([]);
    expect(ids(nodesOfCell([n], 'batteries-refining'))).toEqual(['x']);
  });

  it('no duplicates: two six-stage cells rolling up to one, or a repeated id, give the node once', () => {
    const n = node6('x', 'Graphite', ['batteries-extraction', 'batteries-refining'], ['batteries-extraction::Anode Materials', 'batteries-refining::Anode Materials', 'batteries-refining::anode materials']);
    expect(legacyCellsOf(n)).toEqual(['batteries-upstream']);
    expect(ids(nodesOfCell([n], 'batteries-upstream'))).toEqual(['x']);
    expect(ownSubcomponents(n, 'batteries-upstream')).toEqual(['anode materials']);
    const m = matchNodes(['Anode Materials', 'Natural Graphite'], [n], 'batteries-upstream');
    expect(ids(m.bySub.get('Anode Materials')!)).toEqual(['x']);
    expect(m.bySub.get('Natural Graphite')).toEqual([]);
    expect(subsOfNodes(m).get('x')).toEqual(['Anode Materials']);
    // repeated legacy ids
    const r = node6('r', 'Magnets', ['wind-components'], ['wind-components::Generator Parts'], { cells: ['wind-midstream', 'wind-midstream'], subs: ['wind-midstream::Generator Parts', 'wind-midstream::Generator Parts'] });
    expect(ids(nodesOfCell([r], 'wind-midstream'))).toEqual(['r']);
    expect(ids(matchNodes(['Generator Parts'], [r], 'wind-midstream').bySub.get('Generator Parts')!)).toEqual(['r']);
  });

  it('every node of the fixture sits once on each of its three-stage cells and on no other', () => {
    const cells = [...new Set(doc.nodes.flatMap((n) => n.yfCellsLegacy ?? []))];
    expect(cells.length).toBeGreaterThan(0);
    for (const cell of cells) {
      const got = ids(nodesOfCell(doc.nodes, cell));
      expect(new Set(got).size, cell).toBe(got.length);
      expect(got, cell).toEqual(doc.nodes.filter((n) => n.yfCellsLegacy!.includes(cell)).map((n) => n.id));
    }
  });
});
