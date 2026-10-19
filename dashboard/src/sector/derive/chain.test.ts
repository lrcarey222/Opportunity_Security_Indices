import { describe, expect, it } from 'vitest';
import type { AibNode } from '../../aib/contract.ts';
import { matchNodes, nodeNamedApart, nodeNames, nodesOfCell, subParts } from './chain.ts';

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
