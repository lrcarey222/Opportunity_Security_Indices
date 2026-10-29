// One sector select for the Strategic Partners view and the Country partners section (repair
// 2026-10-15): grouped by stage, technologies alphabetical, labelled with cellLabel(); and the route
// from Development Potential to the raw development data in Sector Insights.

import { describe, expect, it } from 'vitest';
import { devDataLink } from '../country/derive/links.ts';
import { cellLabel, cellOptions } from './format.ts';

describe('cellOptions', () => {
  it('orders by stage in chain order, then technology, with one label form', () => {
    const cells = [
      { tech: 'Solar', stage: 'Assembly' },
      { tech: 'Electric Vehicles', stage: 'Deployment' },
      { tech: 'Nuclear', stage: 'Extraction' },
      { tech: 'Batteries', stage: 'Recovery' },
      { tech: 'Batteries', stage: 'Refining' },
      { tech: 'Wind', stage: 'Components' },
      { tech: 'Batteries', stage: 'Assembly' },
      { tech: 'Coal', stage: 'Extraction' },
    ];
    const o = cellOptions(cells, (x) => x.stage, (x) => `${x.tech}|${x.stage}`);
    expect(o.map((x) => x.label)).toEqual([
      cellLabel('Coal', 'Extraction'),
      cellLabel('Nuclear', 'Extraction'),
      cellLabel('Batteries', 'Refining'),
      cellLabel('Wind', 'Components'),
      cellLabel('Batteries', 'Assembly'),
      cellLabel('Solar', 'Assembly'),
      cellLabel('Electric Vehicles', 'Deployment'),
      cellLabel('Batteries', 'Recovery'),
    ]);
    expect(o.map((x) => x.group)).toEqual(['Extraction', 'Extraction', 'Refining', 'Components', 'Assembly', 'Assembly', 'Deployment', 'Recovery']);
    expect(o[0]).toEqual({ value: 'Coal|Extraction', label: 'Coal – Extraction', group: 'Extraction' });
  });
});

describe('devDataLink', () => {
  it('opens the partner page in a sector, else the World Bank indicators', () => {
    expect(devDataLink('IND', { tech: 'Electric Vehicles', stage: 'Assembly' })).toBe('/sector/electric-vehicles/assembly/IND#aid-investment');
    expect(devDataLink('IND', null)).toBe('/sector/explore/development?m=development.wdi&c=IND');
    expect(devDataLink(null, { tech: 'Solar', stage: 'Assembly' })).toBe('/sector/explore/development?m=development.wdi');
  });
});
