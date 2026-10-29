// Each dial moves exactly the parameters spec.md 2.2-2.5 gives it, in the documented direction, and
// nothing else (reviewer test). Runs on the committed synthetic manifest and, when present, on the
// shipped data (public/data/manifest.json). The retired allied network design has no parameters left
// (2026-10-17), so no dial moves a network term.

import { describe, expect, it } from 'vitest';
import { existsSync, readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { parseBaselineInputs } from '../bundle.ts';
import { dialReadout, makeParams } from '../params.ts';
import type { Bundle, Dials, Params } from '../types.ts';

type Ctx = Pick<Bundle, 'baselineInputs' | 'manifest'>;

function ctxFrom(rel: string): Ctx | null {
  const path = fileURLToPath(new URL(rel, import.meta.url));
  if (!existsSync(path)) return null;
  const manifest = JSON.parse(readFileSync(path, 'utf8')) as Bundle['manifest'];
  return { manifest, baselineInputs: parseBaselineInputs(manifest.baseline_inputs as unknown as Record<string, unknown>) };
}

/** Leaf paths whose value differs between two parameter objects, with both values. */
function changed(a: unknown, b: unknown, path = '', out: Map<string, [unknown, unknown]> = new Map()) {
  if (a !== null && b !== null && typeof a === 'object' && typeof b === 'object' && !Array.isArray(a) && !Array.isArray(b)) {
    const keys = new Set([...Object.keys(a as object), ...Object.keys(b as object)]);
    for (const k of keys) changed((a as Record<string, unknown>)[k], (b as Record<string, unknown>)[k], path ? `${path}.${k}` : k, out);
    return out;
  }
  if (JSON.stringify(a) !== JSON.stringify(b)) out.set(path, [a, b]);
  return out;
}

const SI = ['si.weights.policy', 'si.weights.economic_opportunity', 'si.weights.energy_security_need', 'si.weights.supply_chain', 'si.weights.technology'];

/** The parameters each dial owns (spec.md 2.2-2.5) with the sign of their change as the dial rises. */
function owned(ctx: Ctx, dial: keyof Dials, d: number): Record<string, 1 | -1 | 0> {
  const bi = ctx.baselineInputs;
  const iea = `policy.weights.${bi.policy_iea_var}`;
  const cat = `policy.weights.${bi.policy_cat_var}`;
  // SI weights are renormalised: the dial's own weight moves with the dial, the others against it
  const si = (own: string) => Object.fromEntries(SI.map((k) => [k, k === own ? 1 : -1])) as Record<string, 1 | -1>;
  switch (dial) {
    case 'hawkishness':
      return {
        'dials.hawkishness': 1, 'm.H': 1, ...si('si.weights.energy_security_need'),
        'partner.psi_weights.friendshore': 1,
        ...(bi.fs_weights.geo_alignment != null && bi.fs_weights.geo_alignment > 0 ? { 'partner.fs_weights.geo_alignment': 1 as const } : {}),
        // the CM-FDI ownership component of Friendshore scales with hawkishness like alignment (spec_country.md C4.1)
        ...(bi.fs_weights.chinese_parent_exposure != null && bi.fs_weights.chinese_parent_exposure > 0 ? { 'partner.fs_weights.chinese_parent_exposure': 1 as const } : {}),
      };
    case 'bullishness':
      return {
        'dials.bullishness': 1, 'm.B': 1, ...si('si.weights.economic_opportunity'),
        'partner.psi_weights.opportunity': 1,
      };
    case 'populism': {
      const o: Record<string, 1 | -1 | 0> = {
        'dials.populism': 1, 'm.P': 1, ...si('si.weights.policy'), [iea]: 1,
        // the contrast exponent keeps Components and Assembly (the largest stage weight) and pushes the
        // other four stages down as P rises
        'si.stage_weights.Extraction': -1, 'si.stage_weights.Refining': -1,
        'si.stage_weights.Deployment': -1, 'si.stage_weights.Recovery': -1,
      };
      return o;
    }
    case 'greenness': {
      const o: Record<string, 1 | -1 | 0> = {
        'dials.greenness': 1, 'm.G': 1, ...si('si.weights.technology'), [cat]: 1, 'partner.fs_penalty': 1, 'partner.po_penalty': 1,
      };
      if (d >= bi.fossil_threshold) {
        o['si.fossil_excluded'] = 0;
        o['partner.exclude_techs'] = 0;
      }
      return o;
    }
  }
}

const sets: [string, Ctx | null][] = [
  ['synthetic s1', ctxFrom('../__fixtures__/synthetic/s1/data/manifest.json')],
  ['shipped data', ctxFrom('../../../public/data/manifest.json')],
];

describe.each(sets)('dials on the %s manifest', (_name, ctx) => {
  it.skipIf(!ctx).each(['hawkishness', 'bullishness', 'populism', 'greenness'] as const)('%s moves exactly its parameters, in the documented direction', (dial) => {
    const c = ctx!;
    const base = makeParams(null, c);
    for (const d of [0, 25, 75, 90, 100]) {
      const p: Params = makeParams({ dials: { [dial]: d } as Partial<Dials> as Dials }, c);
      const diff = changed(base, p);
      const expected = owned(c, dial, d);
      expect([...diff.keys()].sort(), `dial ${dial} = ${d}`).toEqual(Object.keys(expected).sort());
      const sign = d > 50 ? 1 : -1;
      for (const [k, dir] of Object.entries(expected)) {
        if (dir === 0) continue;
        const [b0, b1] = diff.get(k)! as [number, number];
        expect(Math.sign(b1 - b0), `${k} at ${dial} = ${d}`).toBe(dir * sign);
      }
      expect('network' in p).toBe(false);
      if (dial === 'greenness' && d >= c.baselineInputs.fossil_threshold) {
        expect(p.si.fossil_excluded).toBe(true);
        expect(p.partner.exclude_techs).toEqual(c.baselineInputs.fossil_techs);
      }
      // the six stages, and Components and Assembly (the largest stage weight) held fixed by the
      // populism contrast exponent
      expect(Object.keys(p.si.stage_weights)).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery']);
      expect(p.si.stage_weights.Components).toBe(base.si.stage_weights.Components);
      expect(p.si.stage_weights.Assembly).toBe(base.si.stage_weights.Assembly);
      // the Strategic Index weights keep their sum (renormalisation)
      const s0 = Object.values(base.si.weights).reduce((a, x) => a + x, 0);
      const s1 = Object.values(p.si.weights).reduce((a, x) => a + x, 0);
      expect(Math.abs(s1 - s0)).toBeLessThan(1e-15);
    }
  });

  it.skipIf(!ctx)('m(d) is the documented 4^((d - 50) / 50): x0.25, x0.5, x1, x2, x4', () => {
    const c = ctx!;
    const m = (d: number) => makeParams({ dials: { hawkishness: d } as Dials }, c).m.H;
    expect([0, 25, 50, 75, 100].map(m)).toEqual([0.25, 0.5, 1, 2, 4]);
  });

  it.skipIf(!ctx)('every dial at 50 returns the baseline params bit for bit', () => {
    const c = ctx!;
    const p = makeParams({ dials: { hawkishness: 50, bullishness: 50, populism: 50, greenness: 50 } }, c);
    expect(changed(makeParams(null, c), p).size).toBe(0);
  });

  it.skipIf(!ctx)('the populism readout gives the six-stage weights over the manufacturing weight', () => {
    const c = ctx!;
    const bl = { baselineInputs: c.baselineInputs, baselineParams: makeParams(null, c) };
    const row = (P: number, id: string) => {
      const t = dialReadout(makeParams({ dials: { populism: P } as Partial<Dials> as Dials }, c), bl);
      const k = (t.cols.parameter as string[]).indexOf(id);
      return { label: (t.cols.label as string[])[k], current: (t.cols.current as Float64Array)[k], text: (t.cols.text as string[])[k] };
    };
    const sw = c.baselineInputs.stage_weights;
    // the pipeline's six-stage weights (SUPPLY_CHAIN_STAGE_SI_WEIGHTS: an equal share of the
    // roll-up weights 0.50 / 0.75 / 0.25)
    expect(sw).toEqual({ Extraction: 0.5 / 3, Refining: 0.5 / 3, Components: 0.375, Assembly: 0.375, Deployment: 0.25, Recovery: 0.5 / 3 });
    expect(row(50, 'stage_up_ratio').label).toBe('Extraction, Refining, Recovery / Components, Assembly stage weight');
    expect(row(50, 'stage_down_ratio').label).toBe('Deployment / Components, Assembly stage weight');
    // lambda_s = lambda_max (lambda0_s / lambda_max)^mP with lambda_max the Components / Assembly weight
    for (const P of [0, 50, 100]) {
      const mP = Math.pow(4, (P - 50) / 50);
      expect(row(P, 'stage_up_ratio').current).toBeCloseTo(Math.pow(sw.Extraction / sw.Components, mP), 12);
      expect(row(P, 'stage_down_ratio').current).toBeCloseTo(Math.pow(sw.Deployment / sw.Components, mP), 12);
    }
    expect(row(50, 'stage_up_ratio').text).toMatch(/\(baseline\)$/);
  });
});
