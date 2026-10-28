// The opportunity-security space's corner words (src/views/Space/derive.ts): since 2026-10-28 the corners
// keyed secure and watch are shown as Exposed and Stable everywhere, the scatter's own labels included.

import { describe, expect, it } from 'vitest';
import { CORNERS, cornerAnnotation, regionOf, REGIONS } from './derive.ts';
import { QUADRANTS } from '../../country/contract.ts';

describe('Space corners', () => {
  it('the scatter labels, the table regions and the Country corners use the same four names', () => {
    const labels = ([0, 1, 2, 3] as const).map((i) => cornerAnnotation(i, false));
    expect(labels).toEqual([
      'UPGRADE: high opportunity, low security',
      'GROW: high opportunity, high security',
      'EXPOSED: low opportunity, low security',
      'STABLE: low opportunity, high security',
    ]);
    expect(([0, 1, 2, 3] as const).map((i) => cornerAnnotation(i, true))).toEqual(['UPGRADE', 'GROW', 'EXPOSED', 'STABLE']);
    for (const l of labels) expect(l).not.toMatch(/SECURE|WATCH/);
    expect(REGIONS.map((r) => r.split(':')[0])).toEqual([...CORNERS]);
    expect([...CORNERS].sort()).toEqual(QUADRANTS.map((q) => q.label).sort());
  });
  it('regionOf: the bottom left is Exposed and the top left Stable (high = at or above the median)', () => {
    expect(regionOf(0.2, 0.2, 0.5, 0.5)).toBe(REGIONS[2]);
    expect(regionOf(0.5, 0.2, 0.5, 0.5)).toBe(REGIONS[3]);
    expect(regionOf(0.2, 0.5, 0.5, 0.5)).toBe(REGIONS[0]);
    expect(regionOf(0.5, 0.5, 0.5, 0.5)).toBe(REGIONS[1]);
  });
});
