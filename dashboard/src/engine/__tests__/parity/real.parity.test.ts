// Parity on the real data: (1) the engine at the baseline against the baseline tables shipped in
// public/data (needs no fixtures: the worker's self-check, run here in Node), (2) the real fixture
// set written by `Rscript scripts/86_export_dashboard_fixtures.R --set real` (gitignored), and (3)
// timings of every layer. Skipped with a message when public/data or the fixtures are absent.
import { afterEach, describe, expect, it } from 'vitest';
import { existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';
import { loadIndex, loadWebBundle } from './harness.ts';
import { paritySuite, turnEventLoop } from './suite.ts';
import { selfCheckCore, selfCheckPartner } from '../../selfcheck.ts';
import { makeParams, isBaseline } from '../../params.ts';
import { run, runPartner } from '../../engine.ts';
import type { Bundle } from '../../types.ts';

const dataDir = fileURLToPath(new URL('../../../../public/data/', import.meta.url));
const fixtureDir = fileURLToPath(new URL('../../__fixtures__/real/', import.meta.url));
const hasData = existsSync(join(dataDir, 'manifest.json'));
let cached: Bundle | null = null;
const bundle = () => (cached ??= loadWebBundle(dataDir));

describe.skipIf(!hasData)('real data: engine at the baseline vs the shipped tables', () => {
  afterEach(turnEventLoop);
  it('baseline params equal manifest.baseline_params', () => {
    const b = bundle();
    expect(isBaseline(b.baselineParams, b)).toBe(true);
  });
  it('core layers (L1-L3)', () => {
    const r = selfCheckCore(bundle());
    if (!r.ok) throw new Error(r.detail);
    expect(r.ok).toBe(true);
  });
  it('partner layer (best-five, PSI)', () => {
    const r = selfCheckPartner(bundle());
    if (!r.ok) throw new Error(r.detail);
    expect(r.ok).toBe(true);
  });
  it('the shipped bundle has no network chunk (the original allied network design is retired)', () => {
    const b = bundle();
    expect(b.manifest.chunks.map((c) => c.name)).not.toContain('network');
    expect('network' in b.baselineParams).toBe(false);
  });
  it('performance (timings per layer)', () => {
    const b = bundle();
    const t = (f: () => unknown) => {
      const t0 = performance.now();
      const v = f();
      return [performance.now() - t0, v] as const;
    };
    const p = makeParams({ dials: { hawkishness: 80, bullishness: 30, populism: 70, greenness: 80 } }, b);
    const [tCore, res] = t(() => run(b, p));
    const [tPartner, part] = t(() => runPartner(b, p));
    console.log(`[performance] L1-L3 ${tCore.toFixed(0)} ms; L4 ${tPartner.toFixed(0)} ms`);
    expect(res).toBeTruthy();
    expect(part).toBeTruthy();
  });
});

const index = loadIndex(fixtureDir);
if (hasData && index) {
  paritySuite({ name: 'real', fixtureDir, bundle });
} else {
  describe('real fixtures', () => {
    it.skip(`absent: run Rscript scripts/86_export_dashboard_fixtures.R --set real (${fixtureDir})`, () => {});
  });
}
