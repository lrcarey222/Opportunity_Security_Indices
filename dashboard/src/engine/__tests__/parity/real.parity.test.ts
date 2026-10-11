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
import { selfCheckCore, selfCheckNetwork, selfCheckPartner } from '../../selfcheck.ts';
import { makeParams, isBaseline } from '../../params.ts';
import { run, runPartner } from '../../engine.ts';
import { networkInputs, networkScore, networkSolve } from '../../network/index.ts';
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
  it('greedy network', async () => {
    const r = await selfCheckNetwork(bundle());
    if (!r.ok) throw new Error(r.detail);
    expect(r.ok).toBe(true);
  });
  it('performance (timings per layer)', async () => {
    const b = bundle();
    const t = (f: () => unknown) => {
      const t0 = performance.now();
      const v = f();
      return [performance.now() - t0, v] as const;
    };
    const p = makeParams({ dials: { hawkishness: 80, bullishness: 30, populism: 70, greenness: 80 } }, b);
    const [tCore, res] = t(() => run(b, p));
    const [tPartner, part] = t(() => runPartner(b, p));
    const [tInputs, inp] = t(() => networkInputs(b, p, (part as ReturnType<typeof runPartner>).cells, (res as ReturnType<typeof run>).policy));
    const [tScore, sc] = t(() => networkScore(inp as ReturnType<typeof networkInputs>, p));
    const t0 = performance.now();
    const sol = await networkSolve(sc as ReturnType<typeof networkScore>, p, b, { yieldBetweenStages: false });
    const tSolve = performance.now() - t0;
    console.log(
      `[performance] L1-L3 ${tCore.toFixed(0)} ms; L4 ${tPartner.toFixed(0)} ms; L5 inputs ${tInputs.toFixed(0)} ms; ` +
        `score ${tScore.toFixed(0)} ms; greedy ${tSolve.toFixed(0)} ms (${sol?.stages.length} stages, ${sol?.portfolio?.iters} cap iterations)`,
    );
    expect(sol).not.toBeNull();
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
