import { afterEach, describe, expect, it } from 'vitest';
import { moduleItems } from '../shell/Header.tsx';
import { lastLocations, moduleResumeHref, rememberLocation, resetLastLocations, yardsTabHref } from '../shell/lastLocation.ts';
import { MODULES, placeOf, YARDS_NAV, yardsNavKeyOf } from '../shell/modules.ts';

afterEach(() => resetLastLocations());

describe('Draw the Fence in the shared shell', () => {
  it('is a module with its own paths and views', () => {
    expect(MODULES.map((m) => m.id)).toContain('yards');
    expect(placeOf('/yards-fences')).toBe('yards');
    expect(placeOf('/yards-fences/result')).toBe('yards');
    expect(yardsNavKeyOf('/yards-fences')).toBe('home');
    expect(yardsNavKeyOf('/yards-fences/scenario')).toBe('scenario');
    expect(yardsNavKeyOf('/sector')).toBeNull();
  });

  it('its views carry the assessment, and the current one is marked', () => {
    const items = moduleItems('yards', { module: null, items: [] }, '/yards-fences/fence', '?a=abc&x=1');
    expect(items.map((i) => i.key)).toEqual(YARDS_NAV.map((n) => n.key));
    expect(items.every((i) => i.to.endsWith('?a=abc'))).toBe(true);
    expect(items.find((i) => i.active)?.key).toBe('fence');
  });

  it('the tab and Home resume the last assessment', () => {
    expect(moduleResumeHref('yards')).toBe('/yards-fences');
    expect(yardsTabHref('/sector', '')).toBe('/yards-fences');
    rememberLocation('/yards-fences/result', '?a=xyz');
    expect(lastLocations().yards).toBe('/yards-fences/result?a=xyz');
    expect(moduleResumeHref('yards')).toBe('/yards-fences/result?a=xyz');
    expect(yardsTabHref('/overview', '')).toBe('/yards-fences/result?a=xyz');
    // inside the module the tab goes to the overview, keeping the assessment
    expect(yardsTabHref('/yards-fences/assess', '?a=xyz')).toBe('/yards-fences?a=xyz');
  });
});
