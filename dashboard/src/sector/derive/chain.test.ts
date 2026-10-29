import { describe, expect, it } from 'vitest';
import type { AibNode } from '../../aib/contract.ts';
import { readDoc, stripLegacyCells, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { matchNodes, nodeNamedApart, nodeNames, nodesOfCell, ownSubcomponents, subParts, subsOfNodes } from './chain.ts';

const node = (id: string, label: string, yfCells: string[], yfSubcomponents: string[] = []): AibNode =>
  ({ id, label, yfCells, yfSubcomponents, cls: 'BUILD', exposure: 0.3 }) as unknown as AibNode;

// a Solar Assembly-like cell: one trade sub-component "Polysilicon & Wafers", two AIB nodes
const poly = node('solar-poly', 'Polysilicon', ['solar-refining']);
const wafers = node('solar-wafers', 'Wafers', ['solar-refining']);
const cells = node('solar-cells', 'Solar Cells', ['solar-refining'], ['solar-refining::Cells']);
const glass = node('solar-glass', 'Solar Glass', ['solar-refining']);
const other = node('wind-x', 'Wafers', ['wind-refining']);

describe('supply-chain navigator: sub-components and AIB nodes', () => {
  it('splits a sub-component name on "&" and ","', () => {
    expect(subParts('Racking, Mounting & Trackers')).toEqual(['racking', 'mounting', 'trackers']);
    expect(subParts('Polysilicon & Wafers')).toEqual(['polysilicon', 'wafers']);
  });

  it('keeps the nodes of one cell and names them by their own sub-component when they have one', () => {
    expect(nodesOfCell([poly, wafers, other], 'solar-refining').map((n) => n.id)).toEqual(['solar-poly', 'solar-wafers']);
    expect(nodeNames(cells, 'solar-refining')).toEqual(['cells']);
    expect(nodeNames(wafers, 'solar-refining')).toEqual(['wafers']);
  });

  it('"Polysilicon & Wafers" holds the nodes Polysilicon and Wafers; exact names win; the rest are unmatched', () => {
    const m = matchNodes(['Polysilicon & Wafers', 'Cells'], [poly, wafers, cells, glass, other], 'solar-refining');
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

  it('a node sits in every OSI cell its codes sit in, whatever its own sector', () => {
    // the magnets node (sector Magnets) sits in Electric Vehicles and Wind Components
    expect(nodesOfCell(doc.nodes, 'electric-vehicles-components').map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    expect(nodesOfCell(doc.nodes, 'wind-components').map((n) => n.id).sort()).toEqual(['ev-wind-mid-magnets', 'wind-down-towers']);
    // a rare-earth node (stage Refining) whose codes sit in the OSI Refining cells, not in Components
    expect(nodesOfCell(doc.nodes, 'wind-refining').map((n) => n.id)).toEqual(['ev-wind-mid-ree-compounds']);
    expect(nodesOfCell(doc.nodes, 'wind-components').map((n) => n.id)).not.toContain('ev-wind-mid-ree-compounds');
    // the Solar modules node (stage Assembly) is in OSI Solar Assembly, not Solar Deployment (a name join would put it there)
    expect(nodesOfCell(doc.nodes, 'solar-deployment')).toEqual([]);
    expect(nodesOfCell(doc.nodes, 'solar-assembly').map((n) => n.id)).toEqual(['solar-down-modules']);
    // a node with no basket code sits in no cell
    const unmapped = doc.nodes.filter((n) => !n.yfCells.length);
    expect(unmapped.map((n) => n.id)).toEqual(['h2-down-electrolysers']);
    for (const cell of ['batteries-assembly', 'solar-assembly', 'wind-components', 'green-hydrogen-assembly', 'green-hydrogen-deployment']) expect(nodesOfCell(unmapped, cell)).toEqual([]);
  });

  it('one node under several sub-components of a cell, one sub-component holding several nodes', () => {
    const magnets = byId('ev-wind-mid-magnets');
    expect(ownSubcomponents(magnets, 'wind-components')).toEqual(['permanent magnets & magnet parts', 'generator parts']);
    expect(nodeNames(magnets, 'electric-vehicles-components')).toEqual(['permanent magnets & magnet parts']);
    const m = matchNodes(['Permanent Magnets & Magnet Parts', 'Generator Parts', 'Towers & Nacelles', 'Blades'], doc.nodes, 'wind-components');
    expect(m.bySub.get('Permanent Magnets & Magnet Parts')!.map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    expect(m.bySub.get('Generator Parts')!.map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    expect(m.bySub.get('Towers & Nacelles')!.map((n) => n.id)).toEqual(['wind-down-towers']);
    expect(m.bySub.get('Blades')).toEqual([]);
    expect(m.unmatched).toEqual([]);
    expect(subsOfNodes(m).get('ev-wind-mid-magnets')).toEqual(['Permanent Magnets & Magnet Parts', 'Generator Parts']);
    // a dictionary label is not the OSI sub-component's name: the view names the node apart
    expect(nodeNamedApart(magnets, 'Generator Parts')).toBe(true);
    // the cell's sub-components the node does not name: listed under the stage
    const m2 = matchNodes(['Blades'], doc.nodes, 'wind-components');
    expect(m2.unmatched.map((n) => n.id).sort()).toEqual(['ev-wind-mid-magnets', 'wind-down-towers']);
  });

  it('several dictionary nodes in one sub-component', () => {
    const two = [
      node('poly-d', 'Polysilicon', ['solar-refining'], ['solar-refining::Polysilicon & Wafers']),
      node('wafers-d', 'Wafers', ['solar-refining', 'semiconductors-components'], ['solar-refining::Polysilicon & Wafers', 'semiconductors-components::Semiconductor Materials']),
    ];
    expect(matchNodes(['Polysilicon & Wafers', 'Cells'], two, 'solar-refining').bySub.get('Polysilicon & Wafers')!.map((n) => n.id)).toEqual(['poly-d', 'wafers-d']);
    expect(matchNodes(['Semiconductor Materials'], two, 'semiconductors-components').bySub.get('Semiconductor Materials')!.map((n) => n.id)).toEqual(['wafers-d']);
  });
});

// Six-stage Sector cells and six-stage AIB nodes (2026-10-28, design A2): the join reads the nodes' own
// six-stage cells (yfCells / yfSubcomponents) directly; their three-stage cells (yfCellsLegacy, optional in
// the contract for older bundles) are never read.
describe('supply-chain navigator: six-stage nodes on six-stage cells', () => {
  const doc = readDoc(SYNTHETIC_DIR);
  const ids = (ns: readonly AibNode[]) => ns.map((n) => n.id);

  it('a Refining node sits on the Refining cell its codes sit in', () => {
    const anode = doc.nodes.find((n) => n.id === 'bat-mid-anode')!;
    expect(anode).toMatchObject({ stage: 'Refining', yfCells: ['batteries-refining'] });
    expect(ids(nodesOfCell(doc.nodes, 'batteries-refining'))).toEqual(['bat-mid-anode']);
    expect(ids(nodesOfCell(doc.nodes, 'batteries-extraction'))).toEqual(['bat-up-graphite']);
    expect(ownSubcomponents(anode, 'batteries-refining')).toEqual(['anode materials']);
    expect(ownSubcomponents(anode, 'batteries-extraction')).toEqual([]);
    const m = matchNodes(['Anode Materials', 'Cathode Materials'], doc.nodes, 'batteries-refining');
    expect(ids(m.bySub.get('Anode Materials')!)).toEqual(['bat-mid-anode']);
    expect(m.bySub.get('Cathode Materials')).toEqual([]);
    expect(m.unmatched).toEqual([]);
    expect(nodesOfCell(doc.nodes, 'batteries-components')).toEqual([]);
  });

  it('the three-stage cells are not read: no node sits on a legacy slug, and a bundle without them joins the same', () => {
    expect(doc.nodes.some((n) => (n.yfCellsLegacy ?? []).length > 0)).toBe(true);
    for (const legacy of ['batteries-midstream', 'batteries-upstream', 'wind-midstream', 'solar-midstream']) expect(nodesOfCell(doc.nodes, legacy), legacy).toEqual([]);
    const old = stripLegacyCells(readDoc(SYNTHETIC_DIR));
    expect(old.nodes.every((n) => n.yfCellsLegacy === undefined && n.yfSubcomponentsLegacy === undefined)).toBe(true);
    for (const cell of ['batteries-refining', 'batteries-extraction', 'wind-components', 'wind-refining', 'solar-assembly', 'electric-grid-assembly']) {
      expect(ids(nodesOfCell(old.nodes, cell)), cell).toEqual(ids(nodesOfCell(doc.nodes, cell)));
    }
  });

  it("a node whose own stage differs from the cell's sits on the cell its codes sit in", () => {
    const cellsAndPacks = node('li', 'Li-ion cells & packs', ['electric-vehicles-components', 'batteries-assembly'], ['electric-vehicles-components::Battery Packs', 'batteries-assembly::Battery Cells']);
    (cellsAndPacks as { stage: string }).stage = 'Assembly';
    expect(ids(nodesOfCell([cellsAndPacks], 'electric-vehicles-components'))).toEqual(['li']);
    expect(ids(nodesOfCell([cellsAndPacks], 'batteries-assembly'))).toEqual(['li']);
    expect(nodesOfCell([cellsAndPacks], 'electric-vehicles-assembly')).toEqual([]);
    expect(ownSubcomponents(cellsAndPacks, 'electric-vehicles-components')).toEqual(['battery packs']);
  });

  it('no duplicates: a repeated cell or sub-component id gives the node once', () => {
    const r = node('r', 'Magnets', ['wind-components', 'wind-components'], ['wind-components::Generator Parts', 'wind-components::generator parts']);
    expect(ids(nodesOfCell([r], 'wind-components'))).toEqual(['r']);
    expect(ownSubcomponents(r, 'wind-components')).toEqual(['generator parts']);
    const m = matchNodes(['Generator Parts', 'Blades'], [r], 'wind-components');
    expect(ids(m.bySub.get('Generator Parts')!)).toEqual(['r']);
    expect(m.bySub.get('Blades')).toEqual([]);
    expect(subsOfNodes(m).get('r')).toEqual(['Generator Parts']);
  });

  it('every node of the fixture sits once on each of its six-stage cells and on no other', () => {
    const cells = [...new Set(doc.nodes.flatMap((n) => n.yfCells))];
    expect(cells.length).toBeGreaterThan(0);
    for (const cell of cells) {
      const got = ids(nodesOfCell(doc.nodes, cell));
      expect(new Set(got).size, cell).toBe(got.length);
      expect(got, cell).toEqual(doc.nodes.filter((n) => n.yfCells.includes(cell)).map((n) => n.id));
    }
  });
});
