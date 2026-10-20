import { describe, expect, it } from 'vitest';
import type { AibNode } from '../../aib/contract.ts';
import { readDoc, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { matchNodes, nodeNamedApart, nodeNames, nodesOfCell, ownSubcomponents, subParts, subsOfNodes } from './chain.ts';

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
    // a Midstream rare-earth node whose codes sit in the OSI Upstream cells
    expect(nodesOfCell(doc.nodes, 'wind-upstream').map((n) => n.id)).toEqual(['ev-wind-mid-ree-compounds']);
    expect(nodesOfCell(doc.nodes, 'wind-midstream').map((n) => n.id)).not.toContain('ev-wind-mid-ree-compounds');
    // the Solar sector's Downstream node is in OSI Solar Midstream, not Solar Downstream (a name join would put it there)
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
