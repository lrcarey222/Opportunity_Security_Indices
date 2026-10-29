// URL state (spec_react.md 8.2): canonical round trip, defaults omitted, invalid values dropped and
// reported, navigation keeps only the scenario and the shared view keys.

import { describe, expect, it } from 'vitest';
import {
  advancedChanged, defaultScenario, navSearch, parseSearch, scenarioKey, toSpec, VIEW_DEFAULTS, writeSearch, type UrlCtx,
} from './url.ts';

const ES = ['Trade', 'Foreign Dependency', 'Investment'];
const EO = ['Investment', 'Trade'];

const ctx: UrlCtx = {
  esCategories: ES,
  eoCategories: EO,
  defaults: {
    es_weights: { Trade: 1, 'Foreign Dependency': 2, Investment: 1 },
    eo_weights: { Investment: 1, Trade: 1 },
    tau: null,
    fossil: 'auto',
    require_resource_base: true,
    psi_view: 'world',
  },
  techs: ['Batteries', 'Solar', 'Electric Vehicles'],
  stages: ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'],
};

const roundTrip = (q: string) => {
  const p = parseSearch(q, ctx);
  return writeSearch(p.scenario, p.view, ctx);
};

describe('parseSearch / writeSearch', () => {
  it('the NEIS baseline is the bare URL', () => {
    const p = parseSearch('', ctx);
    expect(p.problems).toEqual([]);
    expect(p.scenario.dials).toEqual({ hawkishness: 50, bullishness: 50, populism: 50, greenness: 50 });
    expect(writeSearch(p.scenario, p.view, ctx)).toBe('');
    expect(scenarioKey(p.scenario, ctx)).toBe('');
  });

  it('round-trips every key in the canonical order', () => {
    const q =
      '?h=80&b=25&p=55&g=95&es=1,0.5,3&eo=2,0&tau=0.25&fossil=never&screen=0&psi=coalition' +
      '&show=change&tech=Solar&stage=Refining&metric=si_rank&rankable=0&partner=CAN';
    expect(roundTrip(q)).toBe(q.replace(/,/g, '%2C'));
    // any order in, canonical order out
    const shuffled = '?metric=si_rank&g=95&h=80&show=change';
    expect(roundTrip(shuffled)).toBe('?h=80&g=95&show=change&metric=si_rank');
  });

  it('omits values equal to the defaults', () => {
    expect(roundTrip('?h=50&fossil=auto&screen=1&psi=world&show=scenario&rankable=1&es=1,2,1')).toBe('');
  });

  it('is idempotent: writeSearch(parseSearch(x)) is a fixed point', () => {
    for (const q of ['?h=80', '?g=100&fossil=always', '?tau=off&psi=coalition', '?es=0,0,1&eo=1,3']) {
      const once = roundTrip(q);
      expect(roundTrip(once)).toBe(once);
    }
  });

  it('drops invalid values and lists them', () => {
    const p = parseSearch('?h=140&b=abc&tech=Coal&es=1,2&tau=2&zzz=1&g=72.4', ctx);
    expect(p.scenario.dials.hawkishness).toBe(100);
    expect(p.scenario.dials.bullishness).toBe(50);
    expect(p.scenario.dials.greenness).toBe(72);
    expect(p.view.tech).toBeNull();
    expect(p.scenario.advanced.es_weights).toEqual(ctx.defaults.es_weights);
    expect(p.scenario.advanced.tau).toBeNull();
    const text = p.problems.join(' | ');
    for (const k of ['h=140', 'b=abc', 'tech=Coal', 'es=1,2', 'tau=2', 'zzz', 'g=72.4']) expect(text).toContain(k);
  });

  it('an old link with the retired network design settings still opens; the keys are dropped and named', () => {
    const p = parseSearch('?h=80&lambda=1.5&maxshare=0.3&minprod=4&caps=0&cmp=paper', ctx);
    expect(p.scenario.dials.hawkishness).toBe(80);
    expect('network' in p.scenario.advanced).toBe(false);
    expect(p.problems).toHaveLength(5);
    for (const k of ['lambda', 'maxshare', 'minprod', 'caps', 'cmp']) expect(p.problems.join(' | ')).toContain(`${k} (a setting of the original allied network design, which is retired)`);
    expect(writeSearch(p.scenario, p.view, ctx)).toBe('?h=80');
  });

  it('an old three-stage stage: Downstream opens Deployment, Upstream and Midstream are dropped; each is named', () => {
    const down = parseSearch('?h=80&tech=Solar&stage=Downstream', ctx);
    expect(down.view.stage).toBe('Deployment');
    expect(down.problems).toEqual(['stage=Downstream (Downstream is now Deployment; showing Deployment)']);
    expect(writeSearch(down.scenario, down.view, ctx)).toBe('?h=80&tech=Solar&stage=Deployment');
    const mid = parseSearch('?tech=Batteries&stage=Midstream', ctx);
    expect(mid.view.stage).toBeNull();
    expect(mid.view.tech).toBe('Batteries');
    expect(mid.problems).toHaveLength(1);
    expect(mid.problems[0]).toMatch(/^stage=Midstream \(Midstream is no longer a stage: it is now mostly Components and Assembly .*; the stage was dropped\)$/);
    const up = parseSearch('?stage=Upstream', ctx);
    expect(up.view.stage).toBeNull();
    expect(up.problems[0]).toContain('Extraction, Refining and Recovery');
    expect(writeSearch(up.scenario, up.view, ctx)).toBe('');
    // anything else is still just not a stage
    expect(parseSearch('?stage=Sidestream', ctx).problems).toEqual(['stage=Sidestream (not a supply-chain stage)']);
    // a build without Deployment drops Downstream too
    const noDep = parseSearch('?stage=Downstream', { ...ctx, stages: ['Extraction', 'Refining'] });
    expect(noDep.view.stage).toBeNull();
    expect(noDep.problems[0]).toContain('the stage was dropped');
  });

  it('rejects all-zero category weights and repeated keys', () => {
    const p = parseSearch('?es=0,0,0&h=60&h=70', ctx);
    expect(p.scenario.advanced.es_weights).toEqual(ctx.defaults.es_weights);
    expect(p.scenario.dials.hawkishness).toBe(60);
    expect(p.problems.length).toBe(2);
  });

  it('navigation keeps the scenario, Show, technology and stage only', () => {
    const p = parseSearch('?h=80&show=baseline&tech=Solar&metric=psi&partner=CAN', ctx);
    expect(navSearch(p.scenario, p.view, ctx)).toBe('?h=80&show=baseline&tech=Solar');
    expect(scenarioKey(p.scenario, ctx)).toBe('?h=80');
  });

  it('toSpec hands the engine every dial and an independent copy of the advanced settings', () => {
    const s = defaultScenario(ctx);
    const spec = toSpec(s);
    expect(spec.dials).toEqual(s.dials);
    spec.advanced!.es_weights!.Trade = 9;
    expect(s.advanced.es_weights.Trade).toBe(1);
    expect(ctx.defaults.es_weights.Trade).toBe(1);
  });

  it('advancedChanged names the groups that differ', () => {
    const p = parseSearch('?eo=1,3', ctx);
    const c = advancedChanged(p.scenario.advanced, ctx);
    expect(c).toMatchObject({ es: false, eo: true, sample: false, fossil: false, screen: false, psi: false, any: true });
    expect(advancedChanged(defaultScenario(ctx).advanced, ctx).any).toBe(false);
    expect(VIEW_DEFAULTS.show).toBe('scenario');
  });

  it('carries the endowment screen of the allied picks as screen=0 / screen=1 against the default', () => {
    const off = parseSearch('?screen=0', ctx);
    expect(off.problems).toEqual([]);
    expect(off.scenario.advanced.require_resource_base).toBe(false);
    expect(advancedChanged(off.scenario.advanced, ctx)).toMatchObject({ screen: true, any: true });
    expect(scenarioKey(off.scenario, ctx)).toBe('?screen=0');
    expect(navSearch(off.scenario, off.view, ctx)).toBe('?screen=0');
    expect(toSpec(off.scenario).advanced?.require_resource_base).toBe(false);
    expect(roundTrip('?screen=1')).toBe('');
    const bad = parseSearch('?screen=no', ctx);
    expect(bad.scenario.advanced.require_resource_base).toBe(true);
    expect(bad.problems.join(' ')).toContain('screen=no');
  });
});
