import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { TREAT_AS_ALLIED, trustedNetwork } from '../trustedNetwork.ts';

describe('trusted network', () => {
  it('mirrors config/aib_strategic_layer.yml treat_as_allied', () => {
    const yml = readFileSync(resolve(__dirname, '../../../../config/aib_strategic_layer.yml'), 'utf8');
    const m = yml.match(/^\s*treat_as_allied:\s*\[([^\]]*)\]/m);
    expect(m).not.toBeNull();
    const listed = m![1].split(',').map((s) => s.trim()).filter(Boolean);
    expect([...listed].sort()).toEqual([...TREAT_AS_ALLIED].sort());
  });

  it('takes core and partner tiers plus the treated-as-allied economies, nothing else', () => {
    const net = trustedNetwork([
      { iso3: 'USA', name: 'United States', tier: 'core' },
      { iso3: 'IDN', name: 'Indonesia', tier: 'partner' },
      { iso3: 'TWN', name: 'Taiwan', tier: 'none' },
      { iso3: 'CHN', name: 'China', tier: 'none' },
      { iso3: 'RUS', name: 'Russia', tier: 'none' },
      { iso3: 'LKA', name: 'Sri Lanka', tier: 'none' },
    ]);
    expect(net.size).toBe(3);
    expect(net.count).toEqual({ core: 1, partner: 1, allied: 1 });
    expect(net.has('CHN')).toBe(false);
    expect(net.has('RUS')).toBe(false);
    expect(net.tierOf('TWN')).toBe('allied');
    expect(net.tierOf('LKA')).toBe('outside');
    expect(net.has(null)).toBe(false);
  });
});
