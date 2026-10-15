// One sector select for the Strategic Partners view and the Country partners section (repair
// 2026-10-15): grouped by stage, technologies alphabetical, labelled with cellLabel(); and the route
// from Development Potential to the raw development data in Sector Insights.

import { describe, expect, it } from 'vitest';
import { devDataLink } from '../country/derive/links.ts';
import { cellLabel, cellOptions } from './format.ts';

describe('cellOptions', () => {
  it('orders by stage, then technology, with one label form', () => {
    const cells = [
      { tech: 'Solar', stage: 'Midstream' },
      { tech: 'Electric Vehicles', stage: 'Downstream' },
      { tech: 'Nuclear', stage: 'Upstream' },
      { tech: 'Batteries', stage: 'Midstream' },
      { tech: 'Coal', stage: 'Upstream' },
    ];
    const o = cellOptions(cells, (x) => x.stage, (x) => `${x.tech}|${x.stage}`);
    expect(o.map((x) => x.label)).toEqual([
      cellLabel('Coal', 'Upstream'),
      cellLabel('Nuclear', 'Upstream'),
      cellLabel('Batteries', 'Midstream'),
      cellLabel('Solar', 'Midstream'),
      cellLabel('Electric Vehicles', 'Downstream'),
    ]);
    expect(o[0]).toEqual({ value: 'Coal|Upstream', label: 'Coal – Upstream', group: 'Upstream' });
  });
});

describe('devDataLink', () => {
  it('opens the partner page in a sector, else the World Bank indicators', () => {
    expect(devDataLink('IND', { tech: 'Electric Vehicles', stage: 'Midstream' })).toBe('/sector/electric-vehicles/midstream/IND#aid-investment');
    expect(devDataLink('IND', null)).toBe('/sector/explore/development?m=development.wdi&c=IND');
    expect(devDataLink(null, { tech: 'Solar', stage: 'Midstream' })).toBe('/sector/explore/development?m=development.wdi');
  });
});
