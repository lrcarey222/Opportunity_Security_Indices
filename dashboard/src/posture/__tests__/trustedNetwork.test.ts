import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { ALLIANCE_TAG, allianceGroup, allianceTier, isTrusted, TREAT_AS_ALLIED, trustedNetwork } from '../trustedNetwork.ts';

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

  it('allianceTier / allianceGroup: one allied set for every view', () => {
    expect(allianceTier('USA', 'core')).toBe('core');
    expect(allianceTier('IDN', 'partner')).toBe('partner');
    expect(allianceTier('TWN', 'none')).toBe('allied');
    expect(allianceTier('ARE', null)).toBe('allied');
    expect(allianceTier('CHN', 'none')).toBe('none');
    expect(allianceTier(null, 'none')).toBe('none');
    expect(isTrusted('CHE', 'none')).toBe(true);
    expect(isTrusted('RUS', 'none')).toBe(false);
    expect(allianceGroup('TWN', 'rest')).toBe('partner');
    expect(allianceGroup('SVN', 'core')).toBe('core');
    expect(allianceGroup('LKA', 'rest')).toBe('rest');
    expect(allianceGroup('CHN', 'china')).toBe('china');
    expect(ALLIANCE_TAG[allianceTier('ISL', 'none')]).toBe('TREATED AS ALLIED');
  });

  it('config/aib.yml: the AIB demand scope takes treat_as_allied (the stress tests protect the same set)', () => {
    const yml = readFileSync(resolve(__dirname, '../../../../config/aib.yml'), 'utf8');
    const block = yml.split(/^demand_scope:/m)[1]?.split(/^\S/m)[0] ?? '';
    expect(block).toMatch(/mode:\s*coalition/);
    expect(block).toMatch(/include_treat_as_allied:\s*true/);
  });
});
